;;; tabnine-chat-transient.el --- TabNine Chat Transient -*- lexical-binding: t -*-

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

;;
;;; Commentary:
;;

;;; Code:

;;
;; Dependencies
;;

(eval-when-compile (require 'cl-lib))
(require 'tabnine-chat)
(require 'tabnine-util)
(require 'transient)

(declare-function ediff-regions-internal "ediff")
(declare-function ediff-make-cloned-buffer "ediff-utils")

;; * Helper functions
(defun tabnine-chat--refactor-or-rewrite ()
  "Rewrite should be refactored into refactor.

Or is it the other way around?"
  (if (derived-mode-p 'prog-mode)
      "Refactor" "Rewrite"))

(defvar-local tabnine-chat--rewrite-message nil)
(defun tabnine-chat--rewrite-message ()
  "Set a generic refactor/rewrite message for the buffer."
  (if (derived-mode-p 'prog-mode)
      (format "You are a %s programmer. Refactor the selected code. Generate code only, no explanation."
              (tabnine-util--language-id-buffer))
    (format "You are a prose editor. Rewrite the following text to be more professional.")))


;; BUG: The `:incompatible' spec doesn't work if there's a `:description' below it.
;;;###autoload (autoload 'tabnine-chat-menu "tabnine-chat-transient" nil t)
(transient-define-prefix tabnine-chat-menu ()
  "Change parameters of prompt to send TabNine Chat."
  :incompatible '(("-m" "-n" "-k" "-e"))
  [["Prompt:"
    ("-r" "From minibuffer instead" "-r")
    ("-i" "Replace/Delete prompt" "-i")
    "Response to:"
    ("-m" "Minibuffer instead" "-m")
    ("-n" "New session" "-n"
     :class transient-option
     :prompt "Name for new session: "
     :reader
     (lambda (prompt _ history)
       (read-string
        prompt (generate-new-buffer-name tabnine-chat-default-session) history)))
    ("-e" "Existing session" "-e"
     :class transient-option
     :prompt "Existing session: "
     :reader
     (lambda (prompt _ history)
       (completing-read
        prompt (mapcar #'buffer-name (buffer-list))
        (lambda (buf) (and (buffer-local-value 'tabnine-chat-mode (get-buffer buf))
                      (not (equal (current-buffer) buf))))
        t nil history)))
    ("-k" "Kill-ring" "-k")]
   [:description tabnine-chat--refactor-or-rewrite
    :if use-region-p
    ("r"
     ;;FIXME: Transient complains if I use `tabnine-chat--refactor-or-rewrite' here. It
     ;;reads this function as a suffix instead of a function that returns the
     ;;description.
     (lambda () (if (derived-mode-p 'prog-mode)
               "Refactor" "Rewrite"))
     tabnine-chat-rewrite-menu)]
   ["Send" (tabnine-chat--suffix-send)]])

(transient-define-infix tabnine-chat--infix-rewrite-prompt ()
  "Chat directive (system message) to use for rewriting or refactoring."
  :description (lambda () (if (derived-mode-p 'prog-mode)
                         "Set directives for refactor"
                       "Set directives for rewrite"))
  :format "%k %d"
  :class 'transient-lisp-variable
  :variable 'tabnine-chat--rewrite-message
  :key "h"
  :prompt "Set directive for rewrite: "
  :reader (lambda (prompt _ history)
            (read-string
             prompt (tabnine-chat--rewrite-message) history)))

(transient-define-suffix tabnine-chat--suffix-send (args)
  "Send ARGS."
  :key "RET"
  :description "Send prompt"
  (interactive (list (transient-args transient-current-command)))
  (let ((stream tabnine-chat-stream)
        (in-place (and (member "-i" args) t))
        (output-to-other-buffer-p)
        (buffer)
	(position)
        (callback (and (member "-k" args)
		       (lambda (resp info)
			 (if (not resp)
			     (message "Chat response error: %s" (plist-get info :status))
			   (kill-new resp)
			   (message "Chat response: copied to kill-ring.")))))
	(tabnine-chat-buffer-name)
        (prompt
         (and (member "-r" args)
              (read-string
               "Ask Chat: "))))
    (cond
     ((member "-m" args)
      (setq stream nil)
      (setq callback
            (lambda (resp info)
              (if resp
                  (message "Chat response: %s" resp)
                (message "Chat response error: %s" (plist-get info :status))))))
     ((member "-k" args)
      (setq stream nil))
     ((setq tabnine-chat-buffer-name
            (cl-some (lambda (s) (and (string-prefix-p "-n" s)
                                 (substring s 2)))
                     args))
      (setq buffer
            (tabnine-chat tabnine-chat-buffer-name))
      (with-current-buffer buffer
	(when prompt
	  (insert prompt))
        (tabnine-chat--update-header-line " Waiting..." 'warning)
	(setq position (point)))
      (setq output-to-other-buffer-p t))
     ((setq tabnine-chat-buffer-name
            (cl-some (lambda (s) (and (string-prefix-p "-e" s)
                                 (substring s 2)))
                     args))
      (setq buffer (get-buffer tabnine-chat-buffer-name))
      (setq output-to-other-buffer-p t)
      (let ((reduced-prompt
             (or prompt
                 (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                   (buffer-substring-no-properties
                    (save-excursion
                      (text-property-search-backward
                       'tabnine-chat 'response
                       (when (get-char-property (max (point-min) (1- (point)))
                                                'tabnine-chat)
                         t))
                      (point))
                    (point))))))
        (with-current-buffer buffer
          (goto-char (point-max))
          (if (or buffer-read-only
                  (get-char-property (point) 'read-only))
              (setq prompt reduced-prompt)
            (insert reduced-prompt))
	  (setq position (point))
          (when tabnine-chat-mode
            (tabnine-chat--update-header-line " Waiting..." 'warning))))))

    (when in-place
      (setq prompt (tabnine-chat--create-prompt (point)))
      (let ((beg
             (if (use-region-p)
                 (region-beginning)
               (save-excursion
                 (text-property-search-backward
                  'tabnine-chat 'response
                  (when (get-char-property (max (point-min) (1- (point)))
                                           'tabnine-chat)
                    t))
                 (point))))
            (end (if (use-region-p) (region-end) (point))))
        (kill-region beg end)))

    (tabnine-chat--request
     prompt
     :buffer (or buffer (current-buffer))
     :position position
     :in-place (and in-place (not output-to-other-buffer-p))
     :stream stream
     :callback callback)
    (when output-to-other-buffer-p
      (message (concat "Prompt sent to buffer: "
                       (propertize tabnine-chat-buffer-name 'face 'help-key-binding)))
      (display-buffer
       buffer '((display-buffer-reuse-window
                 display-buffer-pop-up-window)
                (reusable-frames . visible))))))

(transient-define-prefix tabnine-chat-rewrite-menu ()
  "Rewrite or refactor text region using TabNine."
  [:description
   (lambda ()
     (format "Directive:  %s"
             (truncate-string-to-width
              (or tabnine-chat--rewrite-message (tabnine-chat--rewrite-message))
              (max (- (window-width) 14) 20) nil nil t)))
   (tabnine-chat--infix-rewrite-prompt)]
  [[:description "Diff Options"
   ("-w" "Wordwise diff" "-w")]
   [:description
    (lambda () (if (derived-mode-p 'prog-mode)
              "Refactor" "Rewrite"))
    (tabnine-chat--suffix-rewrite)
    (tabnine-chat--suffix-rewrite-and-replace)
    (tabnine-chat--suffix-rewrite-and-ediff)]
   ]
  (interactive)
  (unless tabnine-chat--rewrite-message
    (setq tabnine-chat--rewrite-message (tabnine-chat--rewrite-message)))
  (transient-setup 'tabnine-chat-rewrite-menu))


(transient-define-suffix tabnine-chat--suffix-rewrite ()
  "Rewrite or refactor region contents."
  :key "r"
  :description #'tabnine-chat--refactor-or-rewrite
  (interactive)
  (let* ((prompt (or tabnine-chat--rewrite-message
		     (tabnine-chat--rewrite-message)))
         (stream tabnine-chat-stream))
    (tabnine-chat--request prompt :stream stream)))

(transient-define-suffix tabnine-chat--suffix-rewrite-and-replace ()
  "Refactor region contents and replace it."
  :key "R"
  :description (lambda () (concat (tabnine-chat--refactor-or-rewrite) " in place"))
  (interactive)
  (let* ((prompt (or tabnine-chat--rewrite-message (tabnine-chat--rewrite-message)))
         (stream tabnine-chat-stream))
    (kill-region (region-beginning) (region-end))
    (message "Original text saved to kill-ring.")
    (tabnine-chat--request prompt :stream stream :in-place t)))

(transient-define-suffix tabnine-chat--suffix-rewrite-and-ediff (args)
  "Refactoring or rewrite region contents and run Ediff."
  :key "E"
  :description (lambda () (concat (tabnine-chat--refactor-or-rewrite) " and Ediff"))
  (interactive (list (transient-args transient-current-command)))
  (let* ((prompt (or tabnine-chat--rewrite-message (tabnine-chat--rewrite-message))))
    (message "Waiting for response... ")
    (tabnine-chat--request
     prompt
     :context (cons (region-beginning) (region-end))
     :callback
     (lambda (response info)
       (if (not response)
	   (message "TabNine Chat response error: %s" (plist-get info :status))
         (let* ((tabnine-chat-buffer (plist-get info :buffer))
                (tabnine-chat-bounds (plist-get info :context))
		(codeblocks (tabnine-util--markdown-codeblocks response))
		(codeblocks-text (mapcar (lambda(x) (plist-get x :code)) codeblocks))
		(code-text (string-join codeblocks-text "\n"))
                (buffer-mode
                 (buffer-local-value 'major-mode tabnine-chat-buffer)))
           (pcase-let ((`(,new-buf ,new-beg ,new-end)
                        (with-current-buffer (get-buffer-create "*tabnine-chat-rewrite-Region.B-*")
                          (erase-buffer)
                          (funcall buffer-mode)
                          (insert code-text)
                          (goto-char (point-min))
                          (list (current-buffer) (point-min) (point-max)))))
             (require 'ediff)
             (apply
              #'ediff-regions-internal
              (get-buffer (ediff-make-cloned-buffer tabnine-chat-buffer "-Region.A-"))
              (car tabnine-chat-bounds) (cdr tabnine-chat-bounds)
              new-buf new-beg new-end
              nil
              (if (transient-arg-value "-w" args)
                  (list 'ediff-regions-wordwise 'word-wise nil)
                (list 'ediff-regions-linewise nil nil))))))))))

(provide 'tabnine-chat-transient)

;;; tabnine-chat-transient.el ends here
