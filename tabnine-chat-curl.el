;;; tabnine-chat-curl.el --- Curl Support for TabNine Chat -*- lexical-binding: t -*-

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
(require 'tabnine-chat)
(require 'tabnine-util)

(declare-function tabnine--access-token "tabnine-core")
(declare-function tabnine-chat--parse-http-response "tabnine-chat")

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'map)
(require 's)

(defvar tabnine-chat-curl--process-alist nil
  "Alist of active TabNine Chat curl requests.")

(defun tabnine-chat-curl--get-args (info token)
  "Produce list of arguments for calling Curl.

INFO is the chat info to send, TOKEN is a unique identifier.."
  (let* ((request (tabnine-chat--make-request info))
	 (data (tabnine-util--json-serialize request))
	 (url (format "%s/chat/v3/generate_chat_response" tabnine-api-server))
         (headers
          `(("Content-Type" . "application/json; charset=utf-8")
            ("Authorization" . ,(concat "Bearer " (tabnine--access-token))))))
    (append
     (list "--location" "--silent" "--compressed" "--disable"
           (format "-X%s" "POST")
           (format "-w(%s . %%{size_header})" token)
           (format "-m%s" 60)
           "-D-"
           (format "-d%s" data))
     (when (and tabnine-network-proxy (stringp tabnine-network-proxy)
		(not (string-empty-p tabnine-network-proxy)))
       (list "--proxy" tabnine-network-proxy))
     (cl-loop for (key . val) in headers
              collect (format "-H%s: %s" key val))
     (list url))))

;;TODO: The :transformer argument here is an alternate implementation of
;;`tabnine-chat-response-filter-functions'. The two need to be unified.
;;;###autoload
(defun tabnine-chat-curl-get-response (info &optional callback)
  "Retrieve response to prompt in INFO.

INFO is a plist with the following keys:
- :prompt (the prompt being sent)
- :buffer (the TabNine Chat buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (let* ((token (md5 (format "%s%s%s%s"
                             (random) (emacs-pid) (user-full-name)
                             (recent-keys))))
	 (args (tabnine-chat-curl--get-args info token))
         (process (apply #'start-process "tabnine-chat-curl"
                         (generate-new-buffer tabnine-chat--curl-buffer-name) "curl" args)))
    (with-current-buffer (process-buffer process)
      (set-process-query-on-exit-flag process nil)
      (setf (alist-get process tabnine-chat-curl--process-alist)
            (nconc (list :token token
			 :callback (or callback
                                       (if tabnine-chat-stream
                                           #'tabnine-chat-curl--stream-insert-response
                                         #'tabnine-chat--insert-response))
                         :transformer (when (or (eq tabnine-chat-default-mode 'org-mode)
                                                (eq (buffer-local-value
                                                     'major-mode
                                                     (plist-get info :buffer))
                                                    'org-mode))
                                        (tabnine-chat--stream-convert-markdown->org)))
                   info))
      (if tabnine-chat-stream
          (progn (set-process-sentinel process #'tabnine-chat-curl--stream-cleanup)
                 (set-process-filter process #'tabnine-chat-curl--stream-filter))
        (set-process-sentinel process #'tabnine-chat-curl--sentinel)))))

(defun tabnine-chat-abort (buffer)
  "Stop any active `tabnine-chat' process associated with the current BUFFER."
  (interactive (list (current-buffer)))
  (unless tabnine-chat-use-curl
    (user-error "Cannot stop a `url-retrieve' request!"))
  (if-let* ((proc-attrs
             (cl-find-if
              (lambda (proc-list)
		(eq (plist-get (cdr proc-list) :buffer) buffer))
              tabnine-chat-curl--process-alist))
            (proc (car proc-attrs)))
      (progn
        (setf (alist-get proc tabnine-chat-curl--process-alist nil 'remove) nil)
        (set-process-sentinel proc #'ignore)
        (delete-process proc)
        (kill-buffer (process-buffer proc))
        (with-current-buffer buffer
          (when tabnine-chat-mode (tabnine-chat--update-header-line  " Ready" 'success)))
        (message "Stopped TabNine Chat request in buffer %S" (buffer-name buffer)))
    (message "No TabNine Chat request associated with buffer %S" (buffer-name buffer))))

;; TODO: Separate user-messaging from this function
(defun tabnine-chat-curl--stream-cleanup (process _status)
  "Process sentinel for TabNine Chat curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when tabnine-chat--debug
      (with-current-buffer proc-buf
        (clone-buffer tabnine-chat--debug-buffer-name 'show)))
    (let* ((info (alist-get process tabnine-chat-curl--process-alist))
	   (proc-token (plist-get info :token))
	   (tabnine-chat-buffer (plist-get info :buffer))
	   (tracking-marker (plist-get info :tracking-marker))
	   (start-marker (plist-get info :position)))
      (pcase-let ((`(,response ,http-status ,http-msg ,error)
		   (tabnine-chat--parse-http-response proc-buf t proc-token)))
	(if (and response (equal http-status "200"))       ;; Finish handling response
	    (progn
	      (let ((context (list :id (tabnine-util--random-uuid) :by "chat" :text response)))
		(tabnine-chat--cached-contexts context))
              (with-current-buffer (marker-buffer start-marker)
		(pulse-momentary-highlight-region (+ start-marker 2) tracking-marker)
		(when tabnine-chat-mode (save-excursion (goto-char tracking-marker)
							(insert "\n\n" (tabnine-chat-prompt-prefix-string)))))
              (with-current-buffer tabnine-chat-buffer
		(when tabnine-chat-mode (tabnine-chat--update-header-line  " Ready" 'success))))

	  (with-current-buffer tabnine-chat-buffer
            (when tabnine-chat-mode
              (tabnine-chat--update-header-line
               (format " Response Error: %s" (or error http-msg)) 'error))))
	(with-current-buffer tabnine-chat-buffer
          (run-hooks 'tabnine-chat-post-response-hook))))
    (setf (alist-get process tabnine-chat-curl--process-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun tabnine-chat-curl--stream-insert-response (response info)
  "Insert streaming RESPONSE from TabNine Chat into the TabNine Chat buffer.

INFO is a mutable plist containing information relevant to this buffer.
See `tabnine-chat--url-get-response' for details."
  (let ((start-marker (plist-get info :position))
        (tracking-marker (plist-get info :tracking-marker))
        (transformer (plist-get info :transformer)))
    (when response
      (with-current-buffer (marker-buffer start-marker)
        (save-excursion
          (unless tracking-marker
            (tabnine-chat--update-header-line " Typing..." 'success)
            (goto-char start-marker)
            (unless (or (bobp) (plist-get info :in-place))
              (insert "\n\n"))
            (setq tracking-marker (set-marker (make-marker) (point)))
            (set-marker-insertion-type tracking-marker t)
            (plist-put info :tracking-marker tracking-marker))

          (when transformer
            (setq response (funcall transformer response)))

          (put-text-property 0 (length response) 'tabnine-chat 'response response)
          (goto-char tracking-marker)
          (insert response))))))

(defun tabnine-chat-curl--stream-filter (process output)
  "Filter for TabNine Chat curl process.
PROCESS is the process under watch, OUTPUT is the output received."
  (let* ((proc-info (alist-get process tabnine-chat-curl--process-alist))
	 (proc-token (plist-get proc-info :token)))
    (with-current-buffer (process-buffer process)
      ;; Insert output
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point)))

      ;; Find HTTP status
      (unless (plist-get proc-info :http-status)
	(pcase-let ((`(,_ ,http-status ,http-msg ,_ )
                     (tabnine-chat--parse-http-response (process-buffer process) nil proc-token)))
	  (when http-status
	    (plist-put proc-info :http-status http-status)
	    (plist-put proc-info :status (string-trim http-msg))))

        ;; Handle read-only TabNine Chat buffer
        (when (with-current-buffer (plist-get proc-info :buffer)
                (or buffer-read-only
                    (get-char-property (plist-get proc-info :position) 'read-only)))
          (message "Buffer is read only, displaying reply in buffer \"*TabNine Chat response*\"")
          (display-buffer
           (with-current-buffer (get-buffer-create "*TabNine Chat response*")
             (goto-char (point-max))
             (move-marker (plist-get proc-info :position) (point) (current-buffer))
             (current-buffer))
           '((display-buffer-reuse-window
              display-buffer-pop-up-window)
             (reusable-frames . visible)))))

      (when-let ((http-msg (plist-get proc-info :status))
                 (http-status (plist-get proc-info :http-status)))
        ;; Find data chunk(s) and run callback
        (when (equal http-status "200")
          (funcall (or (plist-get proc-info :callback)
                       #'tabnine-chat-curl--stream-insert-response)
                   (let* ((content-strs))
		     (condition-case nil
			 (while (re-search-forward "\{" nil t)
			   (when-let* ((line-content (decode-coding-string
						      (buffer-substring-no-properties
						       (1- (point))
						       (save-excursion
							 (end-of-line) (point)))
						      'utf-8))
				       (response (tabnine-util--read-json (s-trim line-content)))
				       (content (plist-get response :text)))
			     (push content content-strs))
			   (forward-line))
		       (error
			(forward-line)))
                     (apply #'concat (nreverse content-strs)))
                   proc-info))))))

(defun tabnine-chat-curl--sentinel (process _status)
  "Process sentinel for TabNine Chat curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when tabnine-chat--debug
      (with-current-buffer proc-buf
        (clone-buffer tabnine-chat--debug-buffer-name 'show)))
    (when-let* (((eq (process-status process) 'exit))
                (proc-info (alist-get process tabnine-chat-curl--process-alist))
                (proc-token (plist-get proc-info :token))
                (proc-callback (plist-get proc-info :callback)))
      (pcase-let ((`(,response,_ ,http-msg ,error)
                   (tabnine-chat--parse-http-response proc-buf t proc-token)))
        (plist-put proc-info :status http-msg)
        (when error (plist-put proc-info :error error))
        (funcall proc-callback response proc-info)))
    (setf (alist-get process tabnine-chat-curl--process-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(provide 'tabnine-chat-curl)

;;; tabnine-chat-curl.el ends here
