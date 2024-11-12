;;; tabnine-core.el --- TabNine Core(complete)-*- lexical-binding: t -*-

;; Copyright (C) 2024  Aaron Ji

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

(require 'json)
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'url)
(require 'tabnine-util)


;;
;; Externals
;;

(declare-function vterm-delete-region "ext:vterm.el")
(declare-function vterm-insert "ext:vterm.el")

;;
;; Constants
;;

(defconst tabnine--process-name "tabnine")
(defconst tabnine--buffer-name "*tabnine-log*")
(defconst tabnine--hooks-alist nil)
(defconst tabnine--api-version "4.4.223"
  "TabNine api version.")

(defconst tabnine-version "0.0.1"
  "TabNine version.")

;; tmp file put in tabnine-binaries-folder directory
(defconst tabnine--version-tempfile "version")

;;
;; Customization
;;

(defgroup tabnine nil
  "Options for tabnine."
  :link '(url-link :tag "Github" "https://github.com/shuxiao9058/tabnine")
  :prefix "tabnine-"
  :group 'completion)

(defcustom tabnine-idle-delay 0.1
  "Time in seconds to wait before starting completion.
Complete immediately if set to 0."
  :group 'tabnine
  :type 'float)

(defcustom tabnine-max-num-results 5
  "Maximum number of results to show."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-max-wait-count-while-nil 5
  "Maximum number to wait while no response got."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-wait-disabled-method '(prefetch configuration)
  "List of wait disabled method."
  :group 'tabnine
  :type '(repeat symbol))

(defcustom tabnine-wait-interval-while-nil 0.1
  "Time in seconds to wait before retry while no response got."
  :group 'tabnine
  :type 'float)

(defcustom tabnine-context-char-limit-before 100000
  "The number of chars before point to send for completion."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-context-char-limit-after 100000
  "The number of chars after point to send for completion."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-minimum-prefix-length 3
  "The minimum prefix length for idle completion."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-clear-overlay-ignore-commands nil
  "List of commands that should not clear the overlay when called."
  :group 'tabnine
  :type '(repeat function))

(defcustom tabnine-max-consecutive-restart-count 10
  "Maximum number of times TabNine can consecutively restart.
This may be due to errors in or automatic server updates.
Any successful completion will reset the consecutive count."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-wait 1.0
  "Number of seconds to wait for TabNine to respond."
  :group 'tabnine
  :type 'float)

(defcustom tabnine-binaries-folder "~/.TabNine"
  "Path to TabNine binaries folder.
`tabnine-install-binary' will use this directory."
  :group 'tabnine
  :type 'string)

(defcustom tabnine-install-static-binary (file-exists-p "/etc/nixos/hardware-configuration.nix")
  "Whether install the musl-linked static binary.
Only useful on GNU/Linux.  Automatically set if NixOS is detected."
  :group 'tabnine
  :type 'boolean)

(defcustom tabnine-log-file-path nil
  "If non-nil, next TabNine restart will write debug log to this path."
  :group 'tabnine
  :type 'string)

(defcustom tabnine-debug-file-path nil
  "If non-nil, will log debug messages to TABNINE-DEBUG-FILE-PATH named file."
  :group 'tabnine
  :type 'string)

(defcustom tabnine-auto-balance t
  "Whether TabNine should insert balanced parentheses upon completion."
  :group 'tabnine
  :type 'boolean)

(defcustom tabnine-network-proxy nil
  "Network proxy to use for TabNine. Nil means no proxy.
e.g.: http://user:password@127.0.0.1:7890"
  :group 'tabnine
  :type 'string)

(defcustom tabnine-api-server "https://api.tabnine.com"
  "TabNine api server address."
  :group 'tabnine
  :type 'string)

(defcustom tabnine-access-token-expires-in 2700
  "TabNine access token expire in seconds."
  :group 'tabnine
  :type 'integer)

;;
;; Variables
;;


(defvar tabnine-executable-args nil
  "Arguments passed to TabNine.")

(defvar tabnine--process nil
  "TabNine server process.")

(defvar tabnine--consecutive-restart-count 0
  "Number of times TabNine server has restarted abnormally.
Resets every time successful completion is returned.")

(defvar tabnine--response nil
  "Temporarily stored TabNine server responses.")

(defvar tabnine--response-chunks nil
  "The string to store response chunks from TabNine server.")

(defvar-local tabnine--last-correlation-id 0
  "The last correlation id send to TabNine server for completion.")

(defvar tabnine--correlation-id 0
  "Correlation id send to TabNine server for completion.")


(defvar tabnine--chat-enabled nil
  "Indicate TabNine chat enabled.")

(defvar tabnine--user nil
  "TabNine user info with plist.

Keys at least contains:
access-token: access token for TabNine API
username: username in TabNine Hub
avatar-url: avatar url in TabNine Hub.")

(defvar tabnine--auth-token nil
  "TabNine auth token, used for login.")

(defvar tabnine--access-token nil
  "TabNine access token with plist.

Keys at least contains:
token: token value
expires-at: token expire at
at: the token at time.")

(defvar-local tabnine--overlay nil
  "Overlay for TabNine completion.")

(defvar-local tabnine--completion-cache nil)
(defvar-local tabnine--completion-idx 0)

(defvar tabnine--completion-cache-result nil
  "Temporarily TabNine completion cache responses.")

(defvar-local tabnine--trigger-with-capf nil
  "Completion trigger with capf for TabNine.")

(defvar-local tabnine--trigger-point nil
  "Completion trigger point position for TabNine.")

(defvar tabnine--post-command-timer nil)

(defvar tabnine--mutex (make-mutex "tabnine")
  "Global mutex for TabNine completion.")

(defun tabnine--buffer-changed ()
  "Return non-nil if buffer has changed since \"tabnine-complete\" been invoked."
  (not (= tabnine--last-correlation-id tabnine--correlation-id)))

(defun tabnine--log-to-debug-file(prefix text)
  "Log to TabNine debug buffer, PREFIX is log prefix and TEXT is the log body."
  (when tabnine-debug-file-path
    (let ((str (format "====== %s START %s ====== \n%s ====== END ======\n" (format-time-string "%F %T" (current-time)) prefix text))
	  (message-log-max nil))
      (with-temp-buffer
	(insert str)
	(write-region (point-min) (point-max) tabnine-debug-file-path t 'silent)))))

;;
;; Agent process
;;

(defsubst tabnine--process-alivep ()
  "Non-nil if the `tabnine--process' is alive."
  (and tabnine--process
       (process-live-p tabnine--process)))

(defmacro tabnine--send-request (request)
  "Send a REQUEST to the TabNine process.
REQUEST should be JSON-serializable object."
  `(progn
     (unless (tabnine--process-alivep)
       (tabnine-start-process))

     (when tabnine--process
       (let ((encoded (concat
		       (tabnine-util--json-serialize ,request) "\n"))
	     (wait (not (memq tabnine--request-method tabnine-wait-disabled-method))))
	 (setq tabnine--response nil)
	 ;; (setq tabnine--completion-cache-result nil)
	 (tabnine--log-to-debug-file "Write to TabNine process" encoded)
	 (process-send-string tabnine--process encoded)
	 (when wait
	   (accept-process-output tabnine--process tabnine-wait))))))

(defun tabnine--make-request (method)
  "Create request body for method METHOD and parameters PARAMS."
  (cond
   ((eq method 'autocomplete)
    (let* ((pt (point))
	   (buffer-min 1)
           (buffer-max (1+ (buffer-size)))
           (before-point
            (max (point-min) (- pt tabnine-context-char-limit-before)))
           (after-point
            (min (point-max) (+ pt tabnine-context-char-limit-after)))
	   (offset (max 0 (1- pt)))
	   (line (- (line-number-at-pos) 1))
	   (character (max 0 (- pt (line-beginning-position))))
	   (indentation_size (tabnine-util--infer-indentation-offset)))
      (setq tabnine--trigger-point pt)
      (list
       :version tabnine--api-version
       :request
       (list :Autocomplete
             (list
	      :filename (or (buffer-file-name) nil)
	      :before (buffer-substring-no-properties before-point pt)
	      :after (buffer-substring-no-properties pt after-point)
	      :region_includes_beginning (if (= before-point buffer-min)
                                             t json-false)
	      :region_includes_end (if (= after-point buffer-max)
				       t json-false)
	      :max_num_results tabnine-max-num-results
	      :offset offset
	      :line line
	      :character character
	      :indentation_size indentation_size
	      :correlation_id tabnine--correlation-id
	      :cached_only json-false)))))
   ((eq method 'prefetch)
    (list
     :version tabnine--api-version
     :request
     (list :Prefetch
           (list
            :filename (or (buffer-file-name) nil)))))
   ((eq method 'getidentifierregex)
    (list
     :version tabnine--api-version
     :request
     (list :GetIdentifierRegex
           (list
            :filename (or (buffer-file-name) nil)))))
   ;; Query Capabilities
   ((eq method 'capabilities)
    (list
     :version tabnine--api-version
     :request
     (list :Features '(:__ t))))
   ((eq method 'state)
    (list
     :version tabnine--api-version
     :request
     (list :State '(:__ t))))
   ((eq method 'configuration)
    (list
     :version tabnine--api-version
     :request
     (list :Configuration '(:__ t))))
   ((eq method 'login-with-custom-token-url)
    (list
     :version tabnine--api-version
     :request
     (list :LoginWithCustomTokenUrl '(:__ t))))
   ((eq method 'login-with-custom-token)
    (list
     :version tabnine--api-version
     :request
     (list :LoginWithCustomToken `(:custom_token ,tabnine--auth-token))))
   ((eq method 'logout)
    (list
     :version tabnine--api-version
     :request
     (list :Logout '(:__ t))))
   ;; NotifyWorkspaceChanged
   ;; setState
   ;; StartupActions
   ;; HoverAction
   ))


(defun tabnine--error-no-binaries ()
  "Signal error for when TabNine binary is not found."
  (error "No TabNine binaries found.  Run M-x tabnine-install-binary to download binaries"))

(defun tabnine--get-target ()
  "Return TabNine's system configuration.  Used for finding the correct binary."
  (let* ((system-architecture (car (s-split "-" system-configuration)))
         (tabnine-architecture
          (cond
           ((or (string= system-architecture "aarch64")
                (and (eq system-type 'darwin)
                     (string= system-architecture "x86_64")
                     ;; Detect AArch64 running x86_64 Emacs
                     (string= (shell-command-to-string "arch -arm64 uname -m") "arm64\n")))
            "aarch64")
           ((or (string= system-architecture "arm")
                (and (eq system-type 'darwin)
                     (string= system-architecture "x86_64")
                     ;; Detect AArch64 running x86_64 Emacs
                     (string= (shell-command-to-string "arch -arm64 uname -m") "arm64\n")))
            "aarch64")
           ((string= system-architecture "x86_64")
            "x86_64")
           ((string-match system-architecture "i.86")
            "i686")
           (t
            (error "Unknown or unsupported architecture %s" system-architecture))))

         (os
          (cond
           ((or (eq system-type 'ms-dos)
                (eq system-type 'windows-nt)
                (eq system-type 'cygwin))
            "pc-windows-gnu")
           ((or (eq system-type 'darwin))
            "apple-darwin")
           (tabnine-install-static-binary
            "unknown-linux-musl")
           (t
            "unknown-linux-gnu"))))

    (concat tabnine-architecture "-" os)))

(defun tabnine--get-exe ()
  "Return TabNine's binary file name.  Used for finding the correct binary."
  (if (or (eq system-type 'ms-dos)
          (eq system-type 'windows-nt)
          (eq system-type 'cygwin))
      "TabNine.exe"
    "TabNine"))

(defun tabnine--executable-path ()
  "Find and return the path of the latest TabNine binary for the current system."
  (let ((parent tabnine-binaries-folder))
    (if (file-directory-p parent)
        (let* ((children (->> (directory-files parent)
			      (--remove (member it '("." "..")))
			      (--filter (file-directory-p
                                         (expand-file-name
                                          it
                                          (file-name-as-directory
                                           parent))))
			      (--filter (ignore-errors (version-to-list it)))
			      (-non-nil)))
	       (sorted (nreverse (sort children #'version<)))
	       (target (tabnine--get-target))
	       (filename (tabnine--get-exe)))
          (cl-loop
           for ver in sorted
           for fullpath = (expand-file-name (format "%s/%s/%s"
                                                    ver target filename)
                                            parent)
           if (and (file-exists-p fullpath)
                   (file-regular-p fullpath))
           return fullpath
           finally do (tabnine--error-no-binaries)))
      (tabnine--error-no-binaries))))

(defun tabnine-start-process ()
  "Start TabNine process."
  (tabnine-kill-process)
  (let ((process-environment (cl-copy-list process-environment))
	(process-connection-type nil))
    (when (and tabnine-network-proxy (or (s-starts-with? "http://" tabnine-network-proxy  t)
					 (s-starts-with? "https://" tabnine-network-proxy  t)))
      (setq process-environment
	    (cl-remove-if (lambda(env) (or (s-starts-with? "HTTP_PROXY=" env t) (s-starts-with? "HTTPS_PROXY=" env t)))
			  process-environment))
      (add-to-list 'process-environment (format "HTTPS_PROXY=%s" tabnine-network-proxy))
      (add-to-list 'process-environment (format "https_proxy=%s" tabnine-network-proxy))
      (add-to-list 'process-environment (format "HTTP_PROXY=%s" tabnine-network-proxy))
      (add-to-list 'process-environment (format "http_proxy=%s" tabnine-network-proxy)))
    (setq tabnine--process
          (make-process
           :name tabnine--process-name
           :buffer tabnine--buffer-name
	   :command (append
                     (cons (tabnine--executable-path)
			   (list "--client" "emacs"))
		     tabnine-executable-args
		     (when tabnine-log-file-path
		       (list
			"--log-file-path"
			(expand-file-name
			 (format "%s-%d" tabnine-log-file-path (emacs-pid)))))
		     (list "--client-metadata"
			   (format "clientVersion=%s" emacs-version)
			   (format "pluginVersion=%s" tabnine-version)
			   (format "ide-restart-counter=%d" tabnine--consecutive-restart-count)))
	   :coding 'utf-8-emacs-unix
	   :connection-type 'pipe
	   :filter #'tabnine--process-filter
	   :sentinel #'tabnine--process-sentinel
	   :noquery t
	   :stderr (get-buffer-create
                    (format "*%s stderr*" tabnine--process-name)))))
  ;; hook setup
  (message "TabNine server started.")

  (unless tabnine--chat-enabled
    (tabnine-capabilities)
    (when tabnine--chat-enabled
      (tabnine-state)))

  (dolist (hook tabnine--hooks-alist)
    (add-hook (car hook) (cdr hook))))

(defun tabnine-kill-process ()
  "Kill TabNine process."
  (interactive)
  (when tabnine--process
    (let ((process tabnine--process))
      (setq tabnine--process nil) ; this happens first so sentinel don't catch the kill
      (delete-process process)))
  ;; hook remove
  (dolist (hook tabnine--hooks-alist)
    (remove-hook (car hook) (cdr hook))))

;;
;; TabNine all operations
;;

(defmacro tabnine--request (method)
  "Send TabNine `METHOD` request and get response in json."
  (declare (indent 2))
  `(with-mutex tabnine--mutex
     (let ((num 0)
	   (max-retry (or tabnine-max-wait-count-while-nil 5))
	   (request (tabnine--make-request ,method))
	   (tabnine--request-method ,method)
	   (wait (not (memq ,method tabnine-wait-disabled-method))))
       (tabnine--send-request request)
       (while (and wait (< num max-retry) (not tabnine--response))
	 (setq num (1+ num))
	 (sleep-for (or tabnine-wait-interval-while-nil 0.2)))
       tabnine--response)))

(defun tabnine-autocomplete ()
  "Query TabNine server for auto-complete."
  (let ((num 0)
	(result))
    (while (and (< num 3) (not result))
      (setq result
	    (or (let ((cached-result (or tabnine--completion-cache-result tabnine--response)))
		  (when (tabnine--response-latest-completion-p cached-result)
		    cached-result))
		(tabnine--request 'autocomplete)))
      (setq num (1+ num))
      (sleep-for 0.05))
    result))

(defun tabnine-prefetch ()
  "Prefetch buffer."
  (tabnine--request 'prefetch))

(defun tabnine-capabilities ()
  "Query TabNine server Capabilities."
  (interactive)
  (when-let* ((response (tabnine--request 'capabilities))
	      (enabled-features (plist-get response :enabled_features)))
    (seq-doseq (feature enabled-features)
      (when (equal feature "plugin.feature.tabnine_chat")
	(setq tabnine--chat-enabled t)))
    (if (called-interactively-p 'interactive)
	(if tabnine--chat-enabled
	    (message (tabnine-util--json-serialize response))
	  (user-error "TabNine Chat feature is not available"))
      response)))

(defun tabnine-state ()
  "Query TabNine Service State."
  (when-let* ((response (tabnine--request 'state))
	      (username (plist-get response :user_name))
	      (access-token (plist-get response :access_token))
	      (avatar-url (plist-get response :user_avatar_url))
	      (curr-time (current-time)))
    (setq tabnine--access-token
	  (list :token access-token :at curr-time :expires-at (time-add curr-time tabnine-access-token-expires-in)))
    (setq tabnine--user (list :username username :access-token tabnine--access-token :avatar-url avatar-url))
    response))

(defun tabnine-getidentifierregex ()
  "Get identifier regex from TabNine server."
  (tabnine--request 'getidentifierregex))

(defun tabnine-configuration ()
  "Open TabNine Hub page."
  (interactive)
  (tabnine--request 'configuration))

(defun tabnine--login-with-custom-token-url()
  "Login with custom token URL."
  (tabnine--request 'login-with-custom-token-url))

(defun tabnine-logout()
  "Logout from TabNine."
  (interactive)
  (if (tabnine-state)
      (tabnine--request 'logout)
    (user-error "Already logout")))

(defun tabnine-login()
  "Login to TabNine."
  (interactive)
  (when (tabnine-state)
    (user-error "Already signed in as %s!" (plist-get tabnine--user :username)))
  (let ((token-url (tabnine--login-with-custom-token-url)))
    (if token-url
	(progn
	  (read-from-minibuffer (format "Press ENTER to open TabNine Hub in your browser."))
	  (browse-url token-url)
	  (setq tabnine--auth-token (read-from-minibuffer (format "Paste auth token copied from page:")))
	  (when (> (length tabnine--auth-token) 10)
	    (tabnine--request 'login-with-custom-token))
	  (if (tabnine-state)
	      (message "Authenticated as TabNine user %s." (plist-get tabnine--user :username))
	    (user-error "Login failed!")))
      (user-error "Failed to get the token url from TabNine process!"))))

;;
;; Other helper functions
;;

(defun tabnine--access-token()
  "Get API Token.

Refresh token before expire."
  (unless (and tabnine--access-token
	       (s-present? (plist-get tabnine--access-token :token))
	       (> tabnine-access-token-expires-in (float-time (time-since (plist-get tabnine--access-token :at)))))
    (tabnine-state))
  (plist-get tabnine--access-token :token))

;;
;; Auto completion
;;

(defun tabnine--process-sentinel (process event)
  "Sentinel for TabNine server process.
PROCESS is the process under watch, EVENT is the event occurred."
  (when (and tabnine--process
             (memq (process-status process) '(exit signal)))

    (message "TabNine process %s received event %s."
             (prin1-to-string process)
             (prin1-to-string event))

    (if (>= tabnine--consecutive-restart-count
            tabnine-max-consecutive-restart-count)
        (progn
          (message "TabNine process restart limit reached.")
          (setq tabnine--process nil))

      (message "Restarting TabNine process.")
      (tabnine-start-process)
      (setq tabnine--consecutive-restart-count
            (1+ tabnine--consecutive-restart-count)))))

(defun tabnine--valid-response-p(response)
  "Check whether the RESPONSE is valid completion result."
  (when response
    (let* ((results (plist-get response :results))
           (first-completion (if (seq-empty-p results) nil (seq-elt results 0))))
      (and first-completion (s-present? (plist-get first-completion :new_prefix))))))

(defun tabnine--invalid-completion-p(completion)
  "Check if the COMPLETION is valid, some completions is stupid."
  (when completion
    (let ((new_prefix (plist-get completion :new_prefix))
	  (old_suffix (plist-get completion :old_suffix)))
      (s-equals? new_prefix old_suffix))))

(defun tabnine--completion-capf-p(completion)
  "Check COMPLETION is capf result."
  (tabnine-util--dbind (:new_prefix) completion
    (let ((line (length (s-split "\n" (s-trim new_prefix)))))
      (<= line 2))))

(defun tabnine--response-display-with-capf-p(response)
  "Test RESPONSE whether display with capf."
  (when response
    (let* ((display-with-overlay)
	   (results (plist-get response :results)))
      (mapc (lambda(x)
	      (unless (tabnine--completion-capf-p x)
		(setq display-with-overlay t))) results)
      (when (not tabnine--trigger-with-capf)
	(setq display-with-overlay t))
      (or (not tabnine-mode) (not display-with-overlay)))))

(defun tabnine--response-latest-completion-p(response)
  "Test RESPONSE if the latest completion."
  (when response
    (equal tabnine--correlation-id (plist-get response :correlation_id))))

(defun tabnine--filter-completions(completions)
  "Filter duplicates and bad COMPLETIONS result."
  (when completions
    (when-let* ((completions (cl-remove-duplicates completions
						   :key (lambda (x) (plist-get x :new_prefix))
						   :test #'s-equals-p))
		(completions (cl-remove-duplicates completions
						   :key (lambda (x) (let ((new_prefix (plist-get x :new_prefix))
								     (new_posfix (plist-get x :new_posfix)))
								 (s-trim (concat new_prefix (or new_posfix "")))))
						   :test #'s-equals-p))
		(completions (cl-remove-if #'tabnine--invalid-completion-p
					   completions)))
      completions)))

(defun tabnine--process-filter (_ output)
  "Filter for TabNine server process.
PROCESS is the process under watch, OUTPUT is the output received."
  (tabnine--log-to-debug-file "Read from TabNine process" output)
  (push output tabnine--response-chunks)

  (let* ((response
	  (mapconcat #'identity
		     (nreverse tabnine--response-chunks) nil))
         (ss (nreverse (s-split "\n" (s-trim response))))
         str
	 result)

    (while (and ss (> (length ss) 0)
		(or (not result) (equal result 'json-error)))
      (setq tabnine--response-chunks nil)
      (setq str (car ss))
      (when (s-present? str)
        (setq result (tabnine-util--read-json str))
	(unless (equal result 'json-error)
	  (when (and result (tabnine--valid-response-p result))
	    (when-let*  ((completions (tabnine--filter-completions (plist-get result :results))))
	      (setq completions (cl-sort completions
					 (lambda(a b)
					   (let* ((get-detail-number-fn
						   (lambda(x)
						     (when-let* ((md (plist-get x :completion_metadata))
								 (detail (plist-get md :detail)))
						       (string-to-number (s-trim detail)))))
						  (detail-a (funcall get-detail-number-fn a))
						  (detail-b (funcall get-detail-number-fn b)))
					     (> detail-a detail-b)))))
	      (plist-put result :results completions))
	    (setq tabnine--completion-cache-result result)
	    (when (and tabnine-mode (equal (point) tabnine--trigger-point))
	      (tabnine--show-completion-1 result)))
	  (setq tabnine--response result)))
      (setq ss (cdr ss)))))

;;
;; Interactive functions
;;

(defun tabnine-restart-server ()
  "Start/Restart TabNine server."
  (interactive)
  (tabnine-start-process))

(defun tabnine-install-binary ()
  "Install TabNine binary into `tabnine-binaries-folder'."
  (interactive)
  (let ((version-tempfile (concat
                           (file-name-as-directory
                            tabnine-binaries-folder)
                           tabnine--version-tempfile))
        (target (tabnine--get-target))
        (exe (tabnine--get-exe))
        (binaries-dir tabnine-binaries-folder))
    (message version-tempfile)
    (message "Getting current version...")
    (make-directory (file-name-directory version-tempfile) t)
    (url-copy-file "https://update.tabnine.com/bundles/version" version-tempfile t)
    (let ((version (s-trim (with-temp-buffer (insert-file-contents version-tempfile) (buffer-string)))))
      (when (= (length version) 0)
        (error "TabNine installation failed.  Please try again"))
      (message "Current version is %s" version)
      (let* ((url (concat "https://update.tabnine.com/bundles/" version "/" target "/TabNine.zip"))
             (version-directory (file-name-as-directory
                                 (concat
                                  (file-name-as-directory
                                   (concat (file-name-as-directory binaries-dir) version)))))
             (target-directory (file-name-as-directory (concat version-directory target) ))
             (bundle-path (concat version-directory (format "%s.zip" target)))
             (target-path (concat target-directory exe)))
        (message "Installing at %s. Downloading %s ..." target-path url)
        (make-directory target-directory t)
        (url-copy-file url bundle-path t)
        (condition-case nil
            (let ((default-directory target-directory))
	      (if (or (eq system-type 'ms-dos)
		      (eq system-type 'windows-nt)
		      (eq system-type 'cygwin))
                  (shell-command (format "tar -xf %s" (shell-quote-argument (expand-file-name bundle-path))))
                (shell-command (format "unzip -o %s -d %s"
				       (shell-quote-argument (expand-file-name bundle-path))
				       (shell-quote-argument (expand-file-name target-directory))))))
          (error
           (error "Unable to unzip automatically. Please go to [%s] and unzip the content of [%s] into [%s/]"
                  (expand-file-name version-directory)
                  (file-name-nondirectory bundle-path)
                  (file-name-sans-extension (file-name-nondirectory bundle-path)))))
        (mapc (lambda (filename)
                (set-file-modes (concat target-directory filename) (string-to-number "744" 8)))
	      (--remove (member it '("." "..")) (directory-files target-directory)))
        (delete-file bundle-path)
        (delete-file version-tempfile)
        (message "TabNine installation complete.")))))

(defun tabnine--get-completions-cycling (callback)
  "Get completion cycling options with CALLBACK."
  (when tabnine--completion-cache
    (funcall callback tabnine--completion-cache)))

(defun tabnine--cycle-completion (direction)
  "Cycle completion with DIRECTION."
  (lambda (result)
    (unless tabnine--completion-cache
      (setq tabnine--completion-cache result))
    (let* ((cache tabnine--completion-cache)
	   (correlation_id (plist-get result :correlation_id))
	   (old_prefix (plist-get cache :old_prefix))
	   (completions (plist-get cache :results)))
      (cond ((seq-empty-p completions)
	     (message "No completion is available."))
	    ((= (length completions) 1)
	     (message "Only one completion is available."))
	    (t (let ((idx (mod (+ tabnine--completion-idx direction)
			       (length completions))))
		 (setq tabnine--completion-idx idx)
		 (let ((completion (elt completions idx)))
		   (tabnine--show-completion correlation_id old_prefix completion))))))))

(defsubst tabnine--overlay-visible-p ()
  "Return whether the `tabnine--overlay' is avaiable."
  (and (overlayp tabnine--overlay)
       (overlay-buffer tabnine--overlay)))

(defun tabnine--overlay-displaying-result (result)
  "Return t while `tabnine--overlay' is displaying the RESULT."
  (let* ((ov (tabnine--get-overlay))
	 (ov-correlation-id (overlay-get ov 'correlation_id))
	 (correlation_id (plist-get result :correlation_id)))
    (and (equal ov-correlation-id correlation_id)
	 (tabnine--overlay-visible-p))))

(defun tabnine-next-completion ()
  "Cycle to next completion."
  (interactive)
  (when (tabnine--overlay-visible-p)
    (tabnine--get-completions-cycling (tabnine--cycle-completion 1))))

(defun tabnine-previous-completion ()
  "Cycle to previous completion."
  (interactive)
  (when (tabnine--overlay-visible-p)
    (tabnine--get-completions-cycling (tabnine--cycle-completion -1))))

;;
;; UI
;;

(defun tabnine-current-completion ()
  "Get current completion."
  (and (tabnine--overlay-visible-p)
       (overlay-get tabnine--overlay 'completion)))

(defface tabnine-overlay-face
  '((t :inherit shadow))
  "Face for TabNine overlay.")

(defvar-local tabnine--real-posn nil
  "Posn information without overlay.
To work around posn problems with after-string property.")

(defvar tabnine-completion-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "TAB") #'tabnine-accept-completion)
    (define-key keymap (kbd "<tab>") #'tabnine-accept-completion)
    (define-key keymap (kbd "C-g")  #'tabnine-clear-overlay)
    (define-key keymap (kbd "M-f")  #'tabnine-accept-completion-by-word)
    (define-key keymap (kbd "M-<return>")  #'tabnine-accept-completion-by-line)
    (define-key keymap (kbd "M-[")  #'tabnine-previous-completion)
    (define-key keymap (kbd "M-]")  #'tabnine-next-completion)
    keymap)
  "Keymap for TabNine completion overlay.")

(defun tabnine--get-overlay ()
  "Create or get overlay for TabNine."
  (unless (overlayp tabnine--overlay)
    (setq tabnine--overlay (make-overlay 1 1 nil nil t))
    (overlay-put tabnine--overlay 'keymap tabnine-completion-map)
    (overlay-put tabnine--overlay 'priority 100))
  tabnine--overlay)

(defun tabnine--overlay-end (ov)
  "Return the end position of overlay OV."
  (- (line-end-position) (overlay-get ov 'tail-length)))

(defun tabnine--set-overlay-text (ov completion)
  "Set overlay OV with COMPLETION."
  (move-overlay ov (point) (line-end-position))
  (let* ((p-completion (propertize completion 'face 'tabnine-overlay-face)))
    (if (eolp)
        (progn
	  (overlay-put ov 'after-string "") ; make sure posn is correct
	  (setq tabnine--real-posn (cons (point) (posn-at-point)))
	  (put-text-property 0 1 'cursor t p-completion)
	  (overlay-put ov 'display "")
	  (overlay-put ov 'after-string p-completion))
      (overlay-put ov 'display (substring p-completion 0 1))
      (overlay-put ov 'after-string (substring p-completion 1)))
    (overlay-put ov 'completion completion)
    (overlay-put ov 'start (point))))

(defun tabnine-clear-overlay ()
  "Clear TabNine overlay."
  (interactive)
  (when (tabnine--overlay-visible-p)
    (delete-overlay tabnine--overlay)
    (setq tabnine--real-posn nil)))

(defun tabnine-accept-completion (&optional transform-fn)
  "Accept completion. Return t if there is a completion.
Use TRANSFORM-FN to transform completion if provided."
  (interactive)
  (when (tabnine--overlay-visible-p)
    (let* ((ov tabnine--overlay)
	   (completion (overlay-get ov 'completion))
	   (start (overlay-get ov 'start))
	   (end (tabnine--overlay-end ov))
	   (new_prefix (overlay-get ov 'new_prefix))
	   (new_suffix (overlay-get ov 'new_suffix))
	   (old_suffix (overlay-get ov 'old_suffix))
	   (t-completion (funcall (or transform-fn #'identity) completion))
	   (full-completion (s-equals-p t-completion completion))
	   (partial-completion (and (s-prefix-p t-completion completion)
				    (not full-completion))))
      (tabnine-clear-overlay)
      (when (and tabnine-auto-balance full-completion
		 (s-present? old_suffix) (s-present? (s-trim old_suffix)))
	(delete-region (point)
		       (min (+ (point) (length old_suffix))
			    (point-max))))
      (if (derived-mode-p 'vterm-mode)
	  (progn
	    (vterm-delete-region start end)
	    (vterm-insert t-completion)))
      ;; maybe should not delete this line
      ;; (delete-region start (line-end-position))
      (insert t-completion)

      ;; if it is a partial completion
      (if partial-completion
	  (tabnine--set-overlay-text (tabnine--get-overlay) (s-chop-prefix t-completion completion))
	(when (and (s-present? new_suffix) (s-present? new_prefix))
	  (when-let* ((left (s-chop-prefix new_prefix completion))
		      (len (length left)))
	    (backward-char len))))
      t)))

(defmacro tabnine--define-accept-completion-by-action (func-name action)
  "Define function FUNC-NAME to accept completion by ACTION."
  `(defun ,func-name (&optional n)
     (interactive "p")
     (setq n (or n 1))
     (tabnine-accept-completion (lambda (completion)
				  (with-temp-buffer
				    (insert completion)
				    (goto-char (point-min))
				    (funcall ,action n)
				    (decode-coding-string
				     (buffer-substring-no-properties (point-min) (point))
				     'utf-8))))))

(tabnine--define-accept-completion-by-action tabnine-accept-completion-by-word #'forward-word)
(tabnine--define-accept-completion-by-action tabnine-accept-completion-by-line #'forward-line)
(tabnine--define-accept-completion-by-action tabnine-accept-completion-by-paragraph #'forward-paragraph)

(defun tabnine--show-completion-1 (response)
  "Show completion result after first get RESPONSE."
  (when-let* ((result response)
	      (completions (plist-get result :results))
	      (completion (if (seq-empty-p completions) nil (seq-elt completions 0)))
	      (old_prefix (plist-get result :old_prefix))
	      (new_prefix (plist-get completion :new_prefix))
	      (correlation_id (plist-get result :correlation_id)))
    (unless (tabnine--overlay-displaying-result result)
      (when (s-present? new_prefix)
	(setq tabnine--completion-cache result)
	(setq tabnine--completion-idx 0)
	(when (s-starts-with? old_prefix new_prefix)
	  (setq new_prefix (s-chop-prefix old_prefix new_prefix)))
	(tabnine--show-completion correlation_id old_prefix completion) ;; alway show completion result, while display with capf, then clear the overlay
	;; (when (not (tabnine--response-display-with-capf-p completion))
	;;   (tabnine--show-completion correlation_id old_prefix completion))
	))))

(defun tabnine--show-completion (correlation_id old_prefix completion)
  "Show OLD_PREFIX's COMPLETION with validate completion's CORRELATION_ID."
  (when (and (= correlation_id tabnine--correlation-id)
	     (tabnine--satisfy-display-predicates))
    (tabnine-util--dbind (:new_prefix
			  :old_suffix :new_suffix  :completion_metadata
			  (:snippet_context (:completion_index))) completion
      (when (s-starts-with? old_prefix  new_prefix)
	(setq new_prefix (s-chop-prefix old_prefix new_prefix)))
      (save-restriction
	(widen)
	(let* ((completion-str (if (s-present? new_suffix)
				   (concat new_prefix new_suffix)
				 new_prefix))
	       (completion-data
		(list  :completion completion-str
		       :new_prefix new_prefix
		       :new_suffix new_suffix
		       :old_suffix old_suffix
		       :correlation_id correlation_id
		       :completion_index completion_index
		       :pos (point))))
	  (tabnine--display-overlay-completion completion-data))))))

(defun tabnine--display-overlay-completion (completion)
  "Show COMPLETION with overlay ."
  (tabnine-clear-overlay)
  (let ((completion-str (plist-get completion :completion))
	(old_suffix (plist-get completion :old_suffix))
	(new_suffix (plist-get completion :new_suffix))
	(new_prefix (plist-get completion :new_prefix))
	(correlation_id (plist-get completion :correlation_id))
	(completion_index (plist-get completion :completion_index))
	(pos (plist-get  completion :pos)))
    (when (s-present-p completion-str)
      (save-excursion
	(goto-char pos) ; removing indentation
	(let* ((ov (tabnine--get-overlay)))
	  (overlay-put ov 'tail-length (- (line-end-position) pos))
	  (overlay-put ov 'correlation_id correlation_id)
	  (overlay-put ov 'completion_index completion_index)
	  (overlay-put ov 'old_suffix old_suffix)
	  (overlay-put ov 'new_suffix new_suffix)
	  (overlay-put ov 'new_prefix new_prefix)
	  (tabnine--set-overlay-text ov completion-str))))))

;;;###autoload
(defun tabnine-complete (arg)
  "Complete at the current point with ARG, if arg is set, call from capf."
  (interactive "P")

  (setq tabnine--last-correlation-id tabnine--correlation-id)
  (setq tabnine--completion-cache nil)
  (setq tabnine--completion-idx 0)
  (setq tabnine--trigger-with-capf (if arg t))

  (let ((called-interactively (called-interactively-p 'interactive)))
    (let ((result (if tabnine--trigger-with-capf
		      (tabnine-autocomplete)
		    (with-timeout (0.2)
		      (tabnine-autocomplete)))))
      (unless (tabnine--valid-response-p result)
	(when called-interactively
	  (message "No overlay completion is available."))))))

;;
;; minor mode
;;

(defcustom tabnine-disable-predicates '(window-minibuffer-p)
  "A list of predicate functions with no argument to disable TabNine.
TabNine will not be triggered if any predicate returns t."
  :type '(repeat function)
  :group 'tabnine)

(defcustom tabnine-enable-predicates '(evil-insert-state-p tabnine--buffer-changed
							   tabnine--completion-triggers-p)
  "A list of predicate functions with no argument to enable TabNine.
TabNine will be triggered only if all predicates return t."
  :type '(repeat function)
  :group 'tabnine)

(defcustom tabnine-disable-display-predicates '(window-minibuffer-p)
  "A list of predicate functions with no argument to disable TabNine.
TabNine will not show completions if any predicate returns t."
  :type '(repeat function)
  :group 'tabnine)

(defcustom tabnine-enable-display-predicates nil
  "A list of predicate functions with no argument to enable TabNine.
TabNine will show completions only if all predicates return t."
  :type '(repeat function)
  :group 'tabnine)

(defcustom tabnine-completion-triggers " .(){}[],:'\",=<>/\\+-|&*%=$#@!\n\r\t\v"
  "Completion triggers."
  :type 'string
  :group 'tabnine)

(defmacro tabnine--satisfy-predicates (enable disable)
  "Return t if satisfy all predicates in ENABLE and none in DISABLE."
  `(and (cl-every (lambda (pred)
		    (if (functionp pred) (funcall pred) t))
		  ,enable)
        (cl-notany (lambda (pred)
		     (if (functionp pred) (funcall pred) nil))
		   ,disable)))

(defun tabnine--satisfy-trigger-predicates ()
  "Return t if all trigger predicates are satisfied."
  (tabnine--satisfy-predicates tabnine-enable-predicates tabnine-disable-predicates))

(defun tabnine--satisfy-display-predicates ()
  "Return t if all display predicates are satisfied."
  (tabnine--satisfy-predicates tabnine-enable-display-predicates tabnine-disable-display-predicates))

(defvar tabnine-mode-map
  (make-sparse-keymap)
  "Keymap for TabNine minor mode.
Use this for custom bindings in `tabnine-mode'.")

;;;###autoload
(define-minor-mode tabnine-mode
  "Minor mode for TabNine."
  :init-value nil
  :lighter " ⌬"
  (tabnine-clear-overlay)
  (advice-add 'posn-at-point :before-until #'tabnine--posn-advice)
  (if tabnine-mode
      (progn
        (add-hook 'post-command-hook #'tabnine--post-command nil 'local)
        (add-hook 'before-change-functions #'tabnine--on-change nil 'local)
	(advice-add 'switch-to-buffer :after #'tabnine--prefetch-advice))
    (remove-hook 'post-command-hook #'tabnine--post-command 'local)
    (remove-hook 'before-change-functions #'tabnine--on-change 'local)
    (advice-remove 'switch-to-buffer #'tabnine--prefetch-advice)))

(defun tabnine--posn-advice (&rest args)
  "Remap posn if in \"tabnine-mode\". Car of ARGS is point invoked."
  (when tabnine-mode
    (let ((pos (or (car-safe args) (point))))
      (when (and tabnine--real-posn
		 (eq pos (car tabnine--real-posn)))
        (cdr tabnine--real-posn)))))


;;;###autoload
(define-global-minor-mode global-tabnine-mode
  tabnine-mode tabnine-mode)

(defun tabnine--prefetch-advice (_ &rest _)
  "Invoke prefetch operation while switch buffer."
  (when (buffer-file-name)
    (tabnine-prefetch)))

(defun tabnine--on-change (&rest _args)
  "Do while buffer changed, _ARGS is not used."
  (cl-incf tabnine--correlation-id))

(defun tabnine--post-command ()
  "Complete in `post-command-hook' hook."
  (when (and this-command
	     (not (and (symbolp this-command)
		       (or
                        (s-starts-with-p "tabnine-" (symbol-name this-command))
                        (member this-command tabnine-clear-overlay-ignore-commands)
                        (tabnine--self-insert this-command)))))
    (tabnine-clear-overlay)
    (when tabnine--post-command-timer
      (progn
	(cancel-timer tabnine--post-command-timer)
	(setq tabnine--trigger-with-capf nil)))
    (setq tabnine--post-command-timer
	  (run-with-idle-timer tabnine-idle-delay
			       nil
			       #'tabnine--post-command-debounce
			       (current-buffer)))))

(defun tabnine--self-insert (command)
  "Handle the case where the char just inserted is the start of the completion.
If so, update the overlays and continue. COMMAND is the
command that triggered `post-command-hook'."
  (when (and (eq command 'self-insert-command)
	     (tabnine--overlay-visible-p)
	     (tabnine--satisfy-display-predicates))
    (let* ((ov tabnine--overlay)
	   (completion (overlay-get ov 'completion)))
      ;; The char just inserted is the next char of completion
      (when (and (> (length completion) 1)
		 (eq last-command-event (elt completion 0)))
        (tabnine--set-overlay-text ov (substring completion 1))))))

(defun tabnine--post-command-debounce (buffer)
  "Complete in BUFFER."
  (when (and (buffer-live-p buffer)
	     (equal (current-buffer) buffer)
	     tabnine-mode
	     (tabnine--satisfy-trigger-predicates))
    (tabnine-complete nil)))


(defun tabnine--completion-triggers-p()
  "Check whether completion triggers."
  (let ((before (char-before))
	(symbol (thing-at-point 'symbol)))
    (or (and tabnine-completion-triggers before (s-contains? (char-to-string before) tabnine-completion-triggers))
	(and tabnine-minimum-prefix-length symbol (>= (length symbol) tabnine-minimum-prefix-length))
	(not tabnine-minimum-prefix-length))))

;;
;; capf
;;

(defun tabnine--kind-to-type (kind)
  "Convert KIND to lsp type."
  (pcase kind
    (1 "Text")
    (2 "Method")
    (3 "Function")
    (4 "Constructor")
    (5 "Field")
    (6 "Variable")
    (7 "Class")
    (8 "Interface")
    (9 "Module")
    (10 "Property" )
    (11 "Unit" )
    (12 "Value" )
    (13 "Enum")
    (14 "Keyword" )
    (15 "Snippet")
    (16 "Color")
    (17 "File")
    (18 "Reference")
    (19 "Folder")
    (20 "EnumMember")
    (21 "Constant")
    (22 "Struct")
    (23 "Event")
    (24 "Operator")
    (25 "TypeParameter")))

(defun tabnine--construct-candidate-generic (candidate)
  "Generic function to construct completion string from a CANDIDATE."
  (tabnine-util--dbind (:new_prefix
			:old_suffix :new_suffix :completion_metadata
			(:kind :detail)) candidate
    (let ((type (tabnine--kind-to-type kind)))
      (propertize
       new_prefix
       'old_suffix old_suffix
       'new_suffix new_suffix
       'kind kind
       'type type
       'detail detail
       'annotation
       (concat (or detail "") " " (or type ""))))))

(defun tabnine--construct-candidates (results construct-candidate-fn)
  "Use CONSTRUCT-CANDIDATE-FN to construct a list of candidates from RESULTS."
  (let ((completions (mapcar construct-candidate-fn results)))
    completions))

(defun tabnine--candidates (response)
  "Get candidates for RESPONSE."
  (when (tabnine--response-display-with-capf-p response)
    (let* ((result response)
	   (completions (plist-get result :results)))
      (tabnine--construct-candidates
			completions
			#'tabnine--construct-candidate-generic))))

(defun tabnine--post-completion (candidate)
  "Replace old suffix with new suffix for CANDIDATE."
  (when tabnine-auto-balance
    (let ((old_suffix (get-text-property 0 'old_suffix candidate))
	  (new_suffix (get-text-property 0 'new_suffix candidate)))
      (delete-region (point)
		     (min (+ (point) (length old_suffix))
			  (point-max)))
      (when (stringp new_suffix)
        (save-excursion
	  (insert new_suffix))))))

;;;###autoload
(defun tabnine-completion-at-point ()
  "TabNine Completion at point function."
  (unless tabnine-mode
    (tabnine--on-change))
  (tabnine-complete t)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
	 (pt (point))
	 (start        (or (car bounds) pt))
	 (end          (or (cdr bounds) pt))
	 (response tabnine--completion-cache-result)
	 (candidates  (let* ((candidates (tabnine--candidates response)))
			(when (tabnine--response-display-with-capf-p response)
			  candidates)))
	 (get-candidates (lambda () candidates)))
    (list
     start
     end
     candidates
     :exclusive 'no
     :company-kind (lambda (_) (intern "tabnine"))
     :annotation-function
     (lambda (candidate)
       "Extract integer from company-tabnine's CANDIDATE."
       (tabnine-clear-overlay)
       (concat "  "(get-text-property 0 'annotation candidate)))
     :exit-function
     (lambda (candidate _)
       "Post-completion function for tabnine."
       (let ((item (cl-find candidate (funcall get-candidates) :test #'string=)))
	 (tabnine--post-completion item))))))

(provide 'tabnine-core)


;;; tabnine-core.el ends here
