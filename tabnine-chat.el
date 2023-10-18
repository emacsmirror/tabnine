;;; tabnine-chat.el --- TabNine Chat -*- lexical-binding: t -*-

;; Copyright (C) 2023  Aaron Ji

;; Author: Aaron Ji
;; Keywords: convenience

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;
;;; Commentary:
;;

;;; Code:

;;
;; Dependencies
;;

(require 'tabnine-core)
(require 'tabnine-util)

(eval-when-compile
  (require 'cl-lib))

(require 'url)
(require 'url-http)
(require 's)

;; (declare-function tabnine-chat-menu "tabnine-chat-transient")
(declare-function tabnine-util--path-to-uri "tabnine-util")
(declare-function tabnine-util--language-id-buffer "tabnine-util")
(declare-function tabnine-chat-curl-get-response "tabnine-chat-curl")
(declare-function tabnine--access-token "tabnine-core")

(defconst tabnine-chat--debug-buffer-name "*tabnine-chat-debug*")
(defconst tabnine-chat--curl-buffer-name "*tabnine-chat-curl*")

(defvar tabnine-chat-default-session "*tabnine-chat*"
  "TabNine Chat Default session name.")

(defvar tabnine-chat--cached-contexts nil
  "TabNine Chat Cached contexts in plist.

Should at least include following keys:
contexts: cached contexts in array(vector)
size: size of cached context in bytes
last-context-hash: last success context hash.")

(defcustom tabnine-chat-default-mode (if (featurep 'markdown-mode)
					 'markdown-mode
				       'text-mode)
  "The default major mode for dedicated chat buffers.

If `markdown-mode' is available, it is used. Otherwise `tabnine-chat'
defaults to `text-mode'."
  :group 'tabnine
  :type 'symbol)

;; TODO: Handle `prog-mode' using the `comment-start' variable
(defcustom tabnine-chat-prompt-prefix-alist
  '((markdown-mode . "### ")
    (org-mode . "*** ")
    (text-mode . "### "))
  "String inserted after the response from TabNine Chat.

This is an alist mapping major modes to the prefix strings. This
is only inserted in dedicated tabnine chat buffers."
  :group 'tabnine
  :type '(alist :key-type symbol :value-type string))

(defun tabnine-chat-prompt-prefix-string ()
  "TabNine chat prompt prefix string."
  (or (alist-get major-mode tabnine-chat-prompt-prefix-alist) ""))

(defvar-local tabnine-chat--num-messages-to-send nil)
(defvar-local tabnine-chat--old-header-line nil)
(defvar-local tabnine-chat--intent nil)

(defvar tabnine-chat--debug nil
  "TabNine chat debugging.")

(define-minor-mode tabnine-chat-mode
  "Minor mode for interacting with TabNine Chat."
  :lighter " TabNine Chat"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'tabnine-chat-send)
    map)
  (if tabnine-chat-mode
      (setq tabnine-chat--old-header-line header-line-format
            header-line-format
            (list (concat (propertize " " 'display '(space :align-to 0))
                          (format "%s" (buffer-name)))
                  (propertize " Ready" 'face 'success)))
    (setq header-line-format tabnine-chat--old-header-line)))

(defun tabnine-chat--update-header-line (msg face)
  "Update header line with status MSG in FACE."
  (and tabnine-chat-mode (consp header-line-format)
       (setf (nth 1 header-line-format)
             (propertize msg 'face face))
       (force-mode-line-update)))

(defcustom tabnine-chat-stream t
  "Whether responses from TabNine Chat be played back as they are received.

This option is ignored unless Curl is in use (see `tabnine-chat-use-curl' ).

When set to nil, Emacs waits for the full response and inserts it
all at once. This wait is asynchronous.

tis a bit silly."
  :group 'tabnine
  :type 'boolean)

(defcustom tabnine-chat-use-curl (and (executable-find "curl") t)
  "Whether TabNine Chat should prefer Curl when available."
  :group 'tabnine
  :type 'boolean)

(defcustom tabnine-chat-response-filter-functions
  '(tabnine-chat--convert-org)
  "Abnormal hook for transforming the response from TabNine Chat.

This is useful if you want to format the response in some way,
such as filling paragraphs, adding annotations or recording
information in the response like links.

Each function in this hook receives two arguments, the response
string to transform and the TabNine Chat interaction buffer. It should
return the transformed string."
  :group 'tabnine
  :type 'hook)

(defcustom tabnine-chat-post-response-hook nil
  "Hook run after inserting TabNine Chat's response into the current buffer.

This hook is called in the buffer from which the prompt was sent
to TabNine Chat. Note: this hook runs even if the request fails."
  :group 'tabnine
  :type 'hook)

(defcustom tabnine-chat-default-language "elisp"
  "Default language for TabNine Chat.

Used while failed get the last context language buffer."
  :group 'tabnine
  :type 'string)

(defcustom tabnine-chat-max-context-length 400
  "Maximum number of contexts send to TabNine Chat."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-chat-max-cached-context-size 1048576
  "Maximum size of cached messages in bytes send to TabNine Chat.

Default 1MB."
  :group 'tabnine
  :type 'float)

(defcustom tabnine-chat-prompt-alist '((explain-code . "Explain the selected code")
				       (generate-test-for-code . "Write tests for the selected code")
				       (document-code . "Add documentation for the selected code")
				       (fix-code . "Find errors in the selected code and fix them"))
  "Alist of cons cells that map method to TabNine Chat prompt string.
Valid match type keys are:
- explain-code
- generate-test-for-code
- document-code
- fix-code

The other element of each cons pair in this list is the prompt string to
use for TabNine Chat."
  :group 'tabnine
  :type '(repeat (cons (choice :tag "Key"
			       (const explain-code)
			       (const generate-test-for-code)
			       (const document-code)
			       (const fix-code))
		       (string :tag "Prompt String"))))

;;
;; Variables
;;

(defvar tabnine-chat--conversation-id nil
  "The TabNine chat conversation id.")

(defun tabnine-chat--conversion-id()
  "Get conversion ID."
  (unless tabnine-chat--conversation-id
    (setq tabnine-chat--conversation-id (tabnine-util--random-uuid)))
  tabnine-chat--conversation-id)

(defun tabnine-chat--error-no-chat-feature ()
  "Signal user error while TabNine Chat feature not available."
  (user-error "TabNine Chat feature is NOT available yet, please send Tabnine Pro email to support@tabnine.com to join BETA"))

(defun tabnine-chat--context-info(user-context selected-code)
  "Get context's hash with USER-CONTEXT and SELECTED-CODE."
  (let* ((current-line-index
	  (when-let* ((enriching-context (plist-get user-context :enrichingContextData))
		      (first-el (elt enriching-context 0)))
	    (plist-get first-el :currentLineIndex)))
	 (txt (format "%s%s%s" (or (plist-get user-context :fileCode) "")
		      (or (when selected-code
			    (plist-get selected-code :code)) "")
		      (if current-line-index
			  (number-to-string current-line-index) ""))))
    (list :hash (md5 txt) :size  (length txt))))

(defun tabnine-chat--user-context()
  "Return the context for the current state."
  (let ((file-content (decode-coding-string
		       (buffer-substring-no-properties (point-min) (point-max))
		       'utf-8))
	(file-uri (tabnine-util--path-to-uri (buffer-name)))
	(language (tabnine-util--language-id-buffer)))
    (list
     :basicContext (list
		    :fileUri file-uri
		    :language (or language tabnine-chat-default-language))
     :enrichingContextData (vector
			    (list
			     :type "Editor"
			     :fileCode file-content
			     :currentLineIndex (1- (line-number-at-pos)))))))

(defun tabnine-chat--make-request (info)
  "TabNine api make request with INFO.
Method can be explain-code, document-code, generate-test-for-code or fix-code."
  (with-current-buffer (or (plist-get info :context-buffer) (tabnine-chat--context-buffer) (current-buffer))
    (let* ((user-context (tabnine-chat--user-context))
	   (intent (when (and tabnine-chat--intent (symbolp tabnine-chat--intent))
		     (symbol-name tabnine-chat--intent)))
	   (selected-code (when (region-active-p)
			    (list
			     :code (buffer-substring-no-properties
				    (region-beginning)
				    (region-end))
			     :startLine (1- (line-number-at-pos (region-beginning)))
			     :endLine (1- (line-number-at-pos (region-end))))))
	   (context (list
		     :id (tabnine-util--random-uuid)
		     :conversationId (tabnine-chat--conversion-id)
		     :text (plist-get info :prompt)
		     :isBot :json-false
		     :intent intent
		     ;; current unix timestamp in milliseconds
		     :timestamp (number-to-string (car (time-convert (current-time) 1000)))
		     :by "user"))
	   (contexts (tabnine-chat--cached-contexts
		      context user-context selected-code)))
      (list
       :modelParams
       (list :temperature 0.3
	     :top_p 0.95)
       :conversationId (tabnine-chat--conversion-id)
       :messageId (tabnine-util--random-uuid)
       :input contexts
       :isTelemetryEnabled :json-false))))

(defun tabnine-chat--cached-contexts(context &optional user-context selected-code)
  "Cache TabNine Chat CONTEXT with USER-CONTEXT and SELECTED-CODE.

Return contexts result, user-context nil means result from assist."
  (let* ((by (plist-get context :by))
	 (context-size (+ (length (plist-get context :id))
			  (length (plist-get context :text))
			  (length by)))
	 (context-info)
	 (contexts)
	 (size)
	 (last-context-hash)
	 (hash))
    (when user-context
      (setq context-info (tabnine-chat--context-info user-context selected-code))
      (setq hash (plist-get context-info :hash)))

    (when tabnine-chat--cached-contexts
      (setq contexts (plist-get tabnine-chat--cached-contexts :contexts))
      (setq size (plist-get tabnine-chat--cached-contexts :size))
      (setq last-context-hash (plist-get tabnine-chat--cached-contexts :last-context-hash)))

    (setq size (+ (or size 0) context-size))
    (when (and (equal "user" by) (not (and last-context-hash (equal last-context-hash hash))))
      (plist-put context :userContext user-context)
      (plist-put context :selectedCode selected-code)
      (setq size (+ size (plist-get context-info :size)))
      (setq last-context-hash hash))

    (if contexts
	(setq contexts (vconcat contexts (vector context)))
      (setq contexts (vector context)))

    (if tabnine-chat--cached-contexts
	(progn
	  (plist-put tabnine-chat--cached-contexts :size size)
	  (plist-put tabnine-chat--cached-contexts :last-context-hash last-context-hash)
	  (plist-put tabnine-chat--cached-contexts :contexts contexts))
      (setq tabnine-chat--cached-contexts (list :size size
						:contexts contexts
						:last-context-hash last-context-hash)))
    (tabnine-chat--context-cleanup)
    contexts))

(defun tabnine-chat--context-cleanup()
  "Cleanup contexts from cache."
  (let ((contexts (plist-get tabnine-chat--cached-contexts :contexts))
	(size (plist-get tabnine-chat--cached-contexts :size))
	(drop-num 0)
	(drop-num-2 0)
	(drop-size 0))
    (while (> (- size drop-size)  tabnine-chat-max-cached-context-size)
      (let* ((ctx (elt contexts drop-num))
	     (ctx-size (+ (length (plist-get ctx :id))
			  (length (plist-get ctx :text))
			  (length (plist-get ctx :by))))
	     (user-ctx (plist-get ctx :userContext))
	     (selectedCode (plist-get ctx :selectedCode))
	     (ctx-info (and user-ctx (tabnine-chat--context-info user-ctx selectedCode)))
	     (user-ctx-size (or (and ctx-info (plist-get ctx-info :size)) 0)))
	(setq drop-size (+ drop-size ctx-size user-ctx-size))
	(setq drop-num (1+ drop-num))))
    (setq drop-num-2 (- (length contexts) tabnine-chat-max-context-length))
    (while (and (> drop-num-2 drop-num))
      (let* ((ctx (elt contexts drop-num))
	     (ctx-size (+ (length (plist-get ctx :id))
			  (length (plist-get ctx :text))
			  (length (plist-get ctx :by))))
	     (user-ctx (plist-get ctx :userContext))
	     (selectedCode (plist-get ctx :selectedCode))
	     (ctx-info (and user-ctx (tabnine-chat--context-info user-ctx selectedCode)))
	     (user-ctx-size (or (and ctx-info (plist-get ctx-info :size)) 0)))
	(setq drop-size (+ drop-size ctx-size user-ctx-size))
	(setq drop-num (1+ drop-num))))
    (when (> drop-num 0)
      (plist-put tabnine-chat--cached-contexts :contexts (seq-drop contexts drop-num))
      (plist-put tabnine-chat--cached-contexts :size
		 (- size drop-size)))))

(defun tabnine-chat--url-get-response (info &optional callback)
  "Fetch response to prompt in INFO from TabNine Chat.

INFO is a plist with the following keys:
- :prompt (the prompt being sent)
- :buffer (the tabnine chat buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (let* ((inhibit-message t)
         (message-log-max nil)
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json; charset=utf-8")
            ("Authorization" . ,(concat "Bearer " (tabnine--access-token)))))
	 (request (tabnine-chat--make-request info))
	 (url-request-data (tabnine-util--json-serialize request)))
    (url-retrieve (format "%s/chat/v3/generate_chat_response" tabnine-api-server)
                  (lambda (_)
                    (pcase-let ((`(,response, _ ,http-msg ,error)
                                 (tabnine-chat--parse-http-response (current-buffer) t)))
		      (plist-put info :status http-msg)
		      (when error (plist-put info :error error))
		      (funcall (or callback #'tabnine-chat--insert-response)
			       response info)
		      (kill-buffer)))
                  nil t nil)))

(defun tabnine-chat--context-buffer()
  "Get TabNine Chat Context buffer."
  (let* ((buffer-list (buffer-list))
	 (last-prog-buffer)
	 (current-buffer (current-buffer))
	 (current-buffer-major-mode (with-current-buffer current-buffer major-mode))
	 buffer)
    (dolist (buf buffer-list)
      (with-current-buffer buf
	(when (and (tabnine-util--language-id-buffer) (not last-prog-buffer))
	  (setq last-prog-buffer buf))))
    (cond
     ((eq current-buffer-major-mode tabnine-chat-default-mode)
      (setq buffer last-prog-buffer))
     (t (setq buffer last-prog-buffer)))
    buffer))

(cl-defun tabnine-chat--request
    (&optional prompt &key callback
	       (buffer (current-buffer))
	       (position (with-current-buffer buffer
			   (save-excursion
			     (goto-char (point-max))
			     (point-marker))))
	       context
	       context-buffer
	       (stream nil)
	       (in-place nil))
  "Request a response from TabNine Chat for PROMPT.

If PROMPT is
- a string, it is used to create a full prompt suitable for
  sending to TabNine Chat.
- nil, the current buffer's contents up to (point) are used.
  Previous responses from TabNine Chat are identified as responses.

Keyword arguments:

CALLBACK, if supplied, is a function of two arguments, called
with the RESPONSE (a string) and INFO (a plist):

 (callback RESPONSE INFO)

RESPONSE is nil if there was no response or an error.

The INFO plist has (at least) the following keys:
:prompt       - The full prompt that was sent with the request
:position     - marker at the point the request was sent.
:buffer       - The buffer current when the request was sent.
:status       - Short string describing the result of the request

Example of a callback that messages the user with the response
and info:

 (lambda (response info)
   (if response
       (let ((posn (marker-position (plist-get info :position)))
             (buf  (buffer-name (plist-get info :buffer))))
         (message \"Response for request from %S at %d: %s\"
                  buf posn response))
     (message \"gptel-request failed with message: %s\"
              (plist-get info :status))))

Or, for just the response:

 (lambda (response _)
   ;; Do something with response
   (message (rot13-string response)))

If CALLBACK is omitted, the response is inserted at the point the
request was sent.

BUFFER is the buffer the request belongs to. If omitted the
current buffer is recorded.

POSITION is a buffer position (integer or marker). If omitted,
the value of (point) or (region-end) is recorded, depending on
whether the region is active.

CONTEXT is any additional data needed for the callback to run. It
is included in the INFO argument to the callback.

CONTEXT-BUFFER is the editor context the request belongs to.

The following keywords are mainly for internal use:

IN-PLACE is a boolean used by the default callback when inserting
the response to determine if delimiters are needed between the
prompt and the response.

STREAM is a boolean that determines if the response should be
streamed, as in `tabnine-chat-stream'. Do not set this if you are
specifying a custom CALLBACK!"
  (unless tabnine--chat-enabled
    (tabnine-capabilities))
  (unless tabnine--chat-enabled
    (tabnine-chat--error-no-chat-feature))
  (let* ((tabnine-chat-stream stream)
	 (start-marker
          (cond
           ((null position)
            (if (use-region-p)
                (set-marker (make-marker) (region-end))
	      (point-marker)))
           ((markerp position) position)
           ((integerp position)
            (set-marker (make-marker) position buffer))))
	 (info (list :prompt (if (stringp prompt) prompt
			       (tabnine-chat--create-prompt start-marker))
		     :buffer buffer
		     :position start-marker)))
    (when context-buffer (plist-put info :context-buffer context-buffer))
    (when context (plist-put info :context context))
    (when in-place (plist-put info :in-place in-place))
    (funcall
     (if tabnine-chat-use-curl
	 #'tabnine-chat-curl-get-response #'tabnine-chat--url-get-response)
     info callback)
    (unless (eq buffer (current-buffer))
      (switch-to-buffer buffer))))

(defun tabnine-chat--request-by-method (method)
  "TabNine Chat request by METHOD."
  (let* ((stream tabnine-chat-stream)
         (output-to-other-buffer-p t)
	 (tabnine-chat--intent method)
	 (prompt (alist-get method tabnine-chat-prompt-alist))
	 (tabnine-chat-buffer-name tabnine-chat-default-session)
         (buffer (tabnine-chat tabnine-chat-buffer-name))
	 (position))
    (with-current-buffer buffer
      (unless (or buffer-read-only
		  (get-char-property (point) 'read-only))
	(insert prompt)
	(setq position (point-max))))

    (tabnine-chat--request
     prompt
     :buffer (or buffer (current-buffer))
     :position position
     :in-place nil
     :stream stream
     :callback nil)
    (when output-to-other-buffer-p
      (message (concat "Prompt sent to buffer: "
		       (propertize tabnine-chat-buffer-name 'face 'help-key-binding)))
      (display-buffer
       buffer '((display-buffer-reuse-window
                 display-buffer-pop-up-window)
                (reusable-frames . visible))))))

(defun tabnine-chat--diagnostics-text()
  "Get diagnostic text with flycheck."
  (let ((errors (tabnine-util--get-list-errors)))
    (string-join errors "\n")))

(defun tabnine-chat--parse-http-response (response-buffer decode &optional token)
  "Parse response in RESPONSE-BUFFER with DECODE and TOKEN.

Decode result from JSON to text while DECODE not nil, otherwise
return the original HTTP body directly.
TOKEN is used to disambiguate multiple requests in a single buffer.
Return body, http-status, http-msg and error in list."
  (when (buffer-live-p response-buffer)
    (with-current-buffer response-buffer
      (let* ((http-msg (save-excursion
			 (goto-char (point-min))
			 (while (looking-at "^HTTP/[.0-9]+ +[0-9]+ Connection established")
			   (forward-line 2))
			 (re-search-forward "HTTP/[.0-9]+ +[0-9]+" nil t) ;; jump to HTTP status line
			 (string-trim
			  (buffer-substring
			   (line-beginning-position)
			   (line-end-position)))))
	     (start (save-excursion
		      (goto-char (point-min))
		      (if (re-search-forward "\{" nil t)
			  (line-beginning-position)
			;; (1- (point))
			(forward-paragraph)
			(point))))
	     (end (save-excursion
		    (if token
			(progn
			  (goto-char (point-max))
			  (search-backward token nil t)
			  (backward-char)
			  (point)) (point-max))))
             (body (decode-coding-string
		    (buffer-substring-no-properties start end)
		    'utf-8))
	     (http-status (save-match-data
			    (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
				 (match-string 1 http-msg)))))
	(cond
	 ((equal http-status "200")
	  (if decode
	      (let* ((trim-body (s-trim body))
		     (ss (tabnine-chat--transform-body trim-body))
		     (ss (cl-remove-if (lambda(x) (not (s-present? x))) ss))
		     (json-ss (mapcar (lambda(x) (tabnine-util--read-json x)) ss))
		     (first-el (car json-ss))
		     (text (tabnine-chat--results-to-text json-ss)))
		(cond
		 ((and first-el (plist-get first-el :isError))
		  (list nil http-status http-msg (concat "Server Error: " (plist-get first-el :text))))
		 ((s-present? text)
		  (list text http-status http-msg))
		 (t (list nil http-status http-msg (concat "Unknown error: " trim-body)))))
	    (list body http-status http-msg)))
	 ((equal http-status "500")
	  (let* ((trim-body (s-trim body))
		 (error-response (tabnine-util--read-json trim-body)))
	    (cond
	     ((plist-get error-response :message)
	      (let* ((error-msg (plist-get error-response :message))
		     (error-stack (plist-get error-response :stack)))
		(list nil http-status http-msg
		      (format "(%s-%s)" http-msg (string-trim (or error-msg error-stack))))))
	     (t (list nil http-status http-msg (concat "Unknown error: " trim-body))))))
         ((equal http-status "404");; token expired
	  (unless tabnine--chat-enabled
	    (tabnine-chat--error-no-chat-feature))
	  (setq tabnine--access-token nil)
	  (list nil http-status http-msg
		(if tabnine--chat-enabled "TabNine token expired" "TabNine Chat feature is not available")))
	 (t (unless (progn (goto-char (point-min))
			   (when (looking-at "^HTTP/[.0-9]+ +[0-9]+ Connection established")
			     (string-trim
			      (buffer-substring
			       (line-beginning-position)
			       (line-end-position)))))
	      (message "Unknow error: %s, buffer text: %s" http-msg (buffer-string)))
	    (list nil http-status http-msg (concat "Unknow error: " http-msg))))))))

(defun tabnine-chat--transform-body(trim-body)
  "TabNine transform TRIM-BODY to result arrary."
  (when trim-body
    (let* ((ss (s-split "\n" trim-body))
	   (ss (mapcar
		(lambda(x)
		  (when x
		    (with-temp-buffer
		      (insert x)
		      (goto-char (point-min))
		      (if (re-search-forward "\{" nil t)
			  (let ((txt (buffer-substring
				      (1- (point))
				      (save-excursion
					(end-of-line) (point)))))
			    (when txt
			      (setq txt (s-trim txt)))
			    txt))))) ss)))
      ss)))

(defun tabnine-chat--results-to-text(results)
  "TabNine RESULTS in sequence to text."
  (when results
    (let ((text-arr (mapcar (lambda(x) (plist-get x :text)) results)))
      (string-join text-arr))))


;; TODO: Handle multiple requests(#15). (Only one request from one buffer at a time?)
;;;###autoload
(defun tabnine-chat-send ()
  "Submit this prompt to TabNine Chat."
  (interactive)
  (message "Querying TabNine Chat ...")
  (tabnine-chat--request nil
			 :stream tabnine-chat-stream))

(defun tabnine-chat--insert-response (response info)
  "Insert RESPONSE from TabNine Chat into the TabNine Chat buffer.

INFO is a plist containing information relevant to this buffer.
See `tabnine-chat--url-get-response' for details."
  (let* ((status-str  (plist-get info :status))
         (tabnine-chat-buffer (plist-get info :buffer))
         (start-marker (plist-get info :position)))
    ;; Handle read-only buffers
    (when (with-current-buffer tabnine-chat-buffer
            (or buffer-read-only
                (get-char-property start-marker 'read-only)))
      (message "Buffer is read only, displaying reply in buffer \"*TabNine Chat response*\"")
      (display-buffer
       (with-current-buffer (get-buffer-create "*TabNine Chat response*")
         (goto-char (point-max))
         (move-marker start-marker (point) (current-buffer))
         (current-buffer))
       '((display-buffer-reuse-window
          display-buffer-pop-up-window)
         (reusable-frames . visible))))
    ;; Insert response and status message/error message
    (with-current-buffer tabnine-chat-buffer
      (if response
          (progn
            (setq response (tabnine-chat--transform-response
                            response tabnine-chat-buffer))
            (save-excursion
	      (put-text-property 0 (length response) 'tabnine-chat 'response response)
	      (with-current-buffer (marker-buffer start-marker)
                (goto-char start-marker)
                (unless (or (bobp) (plist-get info :in-place))
                  (insert "\n\n"))
                (let ((p (point)))
                  (insert response)
                  (pulse-momentary-highlight-region p (point)))
                (when tabnine-chat-mode (insert "\n\n" (tabnine-chat-prompt-prefix-string))))
	      (when tabnine-chat-mode (tabnine-chat--update-header-line " Ready" 'success))))
        (tabnine-chat--update-header-line
         (format " Response Error: %s" status-str) 'error)
        (message "TabNine Chat response error: (%s) %s"
                 status-str (plist-get info :error)))
      (run-hooks 'tabnine-chat-post-response-hook))))

(defun tabnine-chat--create-prompt (&optional prompt-end)
  "Return a prompt from the contents of this buffer.

If PROMPT-END (a marker) is provided, end the prompt contents
there."
  (save-excursion
    (save-restriction
      (if (use-region-p)
          (progn (narrow-to-region (region-beginning) (region-end))
                 (goto-char (point-max)))
        (goto-char (or prompt-end (point-max))))
      (let ((start)
	    (end (point-max)))
	(setq start (let ((p (point-min)))
		      (text-property-search-backward
		       'tabnine-chat 'response)
		      (setq p (point)) p))
	(string-trim
	 (decode-coding-string
	  (buffer-substring-no-properties start
					  end)
	  'utf-8)
	 "[*# \t\n\r]+")))))


;; TODO: Use `run-hook-wrapped' with an accumulator instead to handle
;; buffer-local hooks, etc.
(defun tabnine-chat--transform-response (content-str buffer)
  "Filter CONTENT-STR through `tabnine-chat-response-filter-functions`.

BUFFER is passed along with CONTENT-STR to each function in this
hook."
  (let ((filtered-str content-str))
    (dolist (filter-func tabnine-chat-response-filter-functions filtered-str)
      (condition-case nil
          (when (functionp filter-func)
            (setq filtered-str
                  (funcall filter-func filtered-str buffer)))
        (error
         (display-warning '(tabnine-chat filter-functions)
                          (format "Function %S returned an error"
                                  filter-func)))))))


;;;###autoload
(defun tabnine-chat (name &optional initial)
  "Switch to or start TabNine Chat session with NAME.

With a prefix arg, query for a (new) session name.

If region is active, use it as the INITIAL prompt. Returns the
buffer created or switched to."
  (interactive (list (if current-prefix-arg
			 (read-string "Session name: " (generate-new-buffer-name tabnine-chat-default-session))
		       tabnine-chat-default-session)
                     (and (use-region-p)
                          (buffer-substring (region-beginning)
                                            (region-end)))))
  (with-current-buffer (get-buffer-create name)
    (cond ;Set major mode
     ((eq major-mode tabnine-chat-default-mode))
     ((eq tabnine-chat-default-mode 'text-mode)
      (text-mode)
      (visual-line-mode 1))
     (t (funcall tabnine-chat-default-mode)))
    (unless tabnine-chat-mode (tabnine-chat-mode 1))
    (if (bobp) (insert (or initial (tabnine-chat-prompt-prefix-string))))
    (goto-char (point-max))
    (skip-chars-backward "\t\r\n")
    (when (called-interactively-p 'tabnine-chat)
      (pop-to-buffer (current-buffer))
      (message "Send your query with %s!"
	       (substitute-command-keys "\\[tabnine-chat-send]")))
    (current-buffer)))

(defun tabnine-chat--convert-org (content buffer)
  "Transform CONTENT according to required major-mode.

Currently only `org-mode' is handled.

BUFFER is the interaction buffer for TabNine Chat."
  (pcase (buffer-local-value 'major-mode buffer)
    ('org-mode (tabnine-chat--convert-markdown->org content))
    (_ content)))

(defun tabnine-chat--convert-markdown->org (str)
  "Convert string STR from markdown to org markup.

This is a very basic converter that handles only a few markup
elements."
  (interactive)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "`\\|\\*\\{1,2\\}\\|_" nil t)
      (pcase (match-string 0)
        ("`" (if (looking-at "``")
                 (progn (backward-char)
                        (delete-char 3)
                        (insert "#+begin_src ")
                        (when (re-search-forward "^```" nil t)
                          (replace-match "#+end_src")))
	       (replace-match "=")))
        ("**" (cond
	       ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)")
                (delete-char 1))
	       ((looking-back "\\(?:[[:word:]]\\|\s\\)\\*\\{2\\}"
			      (max (- (point) 3) (point-min)))
                (delete-char -1))))
        ((or "_" "*")
         (if (save-match-data
	       (and (looking-back "\\(?:[[:space:]]\\|\s\\)\\(?:_\\|\\*\\)"
                                  (max (- (point) 2) (point-min)))
                    (not (looking-at "[[:space:]]\\|\s"))))
             ;; Possible beginning of italics
             (and
	      (save-excursion
                (when (and (re-search-forward (regexp-quote (match-string 0)) nil t)
                           (looking-at "[[:space]]\\|\s")
                           (not (looking-back "\\(?:[[:space]]\\|\s\\)\\(?:_\\|\\*\\)"
					      (max (- (point) 2) (point-min)))))
                  (delete-char -1)
                  (insert "/") t))
	      (progn (delete-char -1)
                     (insert "/")))))))
    (buffer-string)))

(defun tabnine-chat--stream-convert-markdown->org ()
  "Return a Markdown to Org converter.

This function parses a stream of Markdown text to Org
continuously when it is called with successive chunks of the
text stream."
  (letrec ((in-src-block)
           (temp-buf (generate-new-buffer-name "*tabnine-chat-temp*"))
           (start-pt (make-marker))
           (cleanup-fn
            (lambda ()
	      (when (buffer-live-p (get-buffer temp-buf))
                (set-marker start-pt nil)
                (kill-buffer temp-buf))
	      (remove-hook 'tabnine-chat-post-response-hook cleanup-fn))))
    (add-hook 'tabnine-chat-post-response-hook cleanup-fn)
    (lambda (str)
      (let ((noop-p))
        (with-current-buffer (get-buffer-create temp-buf)
          (save-excursion (goto-char (point-max))
                          (insert str))
          (when (marker-position start-pt) (goto-char start-pt))
          (save-excursion
            (while (re-search-forward "`\\|\\*\\{1,2\\}\\|_" nil t)
	      (pcase (match-string 0)
                ("`"
                 (cond
                  ((looking-at "``")
                   (backward-char 1)
                   (delete-char 3)
                   (if in-src-block
		       (progn (insert "#+end_src")
			      (setq in-src-block nil))
                     (insert "#+begin_src ")
                     (setq in-src-block t)))
                  ((looking-at "`\\|$")
                   (setq noop-p t)
                   (set-marker start-pt (1- (point)))
                   (unless (eobp) (forward-char 1)))
                  ((not in-src-block) (replace-match "="))))
                ((and "**" (guard (not in-src-block)))
                 (cond
                  ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)")
                   (delete-char 1))
                  ((looking-back "\\(?:[[:word:]]\\|\s\\)\\*\\{2\\}"
                                 (max (- (point) 3) (point-min)))
                   (delete-char -1))))
                ((and (or "_" "*") (guard (not in-src-block)))
                 (when (save-match-data
                         (save-excursion
                           (backward-char 2)
                           (or
                            (looking-at
                             "[^[:space:][:punct:]\n]\\(?:_\\|\\*\\)\\(?:[[:space:][:punct:]]\\|$\\)")
                            (looking-at
                             "\\(?:[[:space:][:punct:]]\\)\\(?:_\\|\\*\\)\\([^[:space:][:punct:]]\\|$\\)"))))
                   (delete-char -1)
                   (insert "/"))))))
          (if noop-p
	      (buffer-substring (point) start-pt)
            (prog1 (buffer-substring (point) (point-max))
	      (set-marker start-pt (point-max)))))))))

;;
;; TabNine Chat Operations
;;

(defun tabnine-chat-explain-code()
  "Explain the selected code."
  (interactive)
  (tabnine-chat--request-by-method 'explain-code))

(defun tabnine-chat-generate-test-for-code()
  "Write test for the selected code."
  (interactive)
  (tabnine-chat--request-by-method 'generate-test-for-code))

(defun tabnine-chat-document-code()
  "Add documentation for the selected code."
  (interactive)
  (tabnine-chat--request-by-method 'document-code))

(defun tabnine-chat-fix-code()
  "Find errors in the selected code and fix them."
  (interactive)
  (tabnine-chat--request-by-method 'fix-code))


(provide 'tabnine-chat)

;;; tabnine-chat.el ends here
