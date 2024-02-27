;;; tabnine-util.el --- TabNine Utilities functions -*- lexical-binding: t -*-

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

(require 'json)
(require 'editorconfig)
(require 'language-id)

;; flycheck
(defvar flycheck-current-errors)
(defvar flycheck-last-status-change)
(declare-function flycheck-error-message "ext:flycheck")
(declare-function flycheck-error-filename "ext:flycheck")
(declare-function flycheck-error-line "ext:flycheck")

;;
;; Macros
;;

(eval-and-compile
  (defun tabnine-util--transform-pattern (pattern)
    "Transform PATTERN to (&plist PATTERN) recursively."
    (cons '&plist
          (mapcar (lambda (p)
                    (if (listp p)
                        (tabnine-util--transform-pattern p)
                      p))
                  pattern))))

(defmacro tabnine-util--dbind (pattern source &rest body)
  "Destructure SOURCE against plist PATTERN and eval BODY."
  (declare (indent 2))
  `(-let ((,(tabnine-util--transform-pattern pattern) ,source))
     ,@body))

(defconst tabnine-util--indentation-alist
  (append '((latex-mode tex-indent-basic)
            (nxml-mode nxml-child-indent)
            (python-mode python-indent py-indent-offset python-indent-offset)
	    (python-ts-mode python-indent py-indent-offset python-indent-offset)
            (web-mode web-mode-markup-indent-offset web-mode-html-offset))
          editorconfig-indentation-alist)
  "Alist of `major-mode' to indentation map with optional fallbacks.")

(defun tabnine-util--infer-indentation-offset ()
  "Infer indentation offset."
  (or (let ((mode major-mode))
        (while (and (not (assq mode tabnine-util--indentation-alist))
                    (setq mode (get mode 'derived-mode-parent))))
        (when mode
          (cl-some (lambda (s)
                     (when (boundp s)
		       (symbol-value s)))
                   (alist-get mode tabnine-util--indentation-alist))))
      tab-width))

;; code from eglot
(defconst tabnine-util--uri-path-allowed-chars
  (let ((vec (copy-sequence url-path-allowed-chars)))
    (aset vec ?: nil) ;; see github#639
    vec)
  "Like `url-path-allows-chars' but more restrictive.")

(defun tabnine-util--path-to-uri (path)
  "URIfy PATH."
  (let ((truepath (file-truename path)))
    (if (and (url-type (url-generic-parse-url path))
             ;; It might be MS Windows path which includes a drive
             ;; letter that looks like a URL scheme (bug#59338)
             (not (and (eq system-type 'windows-nt)
                       (file-name-absolute-p truepath))))
        ;; Path is already a URI, so forward it to the LSP server
        ;; untouched.  The server should be able to handle it, since
        ;; it provided this URI to clients in the first place.
        path
      (concat "file://"
              ;; Add a leading "/" for local MS Windows-style paths.
              (if (and (eq system-type 'windows-nt)
                       (not (file-remote-p truepath)))
                  "/")
              (url-hexify-string
               ;; Again watch out for trampy paths.
               (directory-file-name (file-local-name truepath))
               tabnine-util--uri-path-allowed-chars)))))

(defun tabnine-util--language-id-buffer ()
  "Return the language used in the current buffer, or NIL.

Prefer getting the ID from the language-id library. Some
languages do not yet have official GitHub Linguist identifiers,
yet format-all needs to know about them anyway. That's why we
have this custom language-id function in format-all. The
unofficial languages IDs are prefixed with \"_\"."
  (or (language-id-buffer)
      (and (or (equal major-mode 'angular-html-mode)
               (and (equal major-mode 'web-mode)
                    (equal (symbol-value 'web-mode-content-type) "html")
                    (equal (symbol-value 'web-mode-engine) "angular")))
           "_Angular")
      (and (member major-mode '(js-mode js2-mode js3-mode))
           (boundp 'flow-minor-mode)
	   (symbol-value 'flow-minor-mode)
           "_Flow")
      (and (equal major-mode 'f90-mode) "_Fortran 90")
      (and (equal major-mode 'ledger-mode) "_Ledger")))


(defun tabnine-util--random-uuid ()
  "Random a UUID. This use a simple hashing of variable data.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d."
  (let ((str (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                          (user-uid)
                          (emacs-pid)
                          (system-name)
                          (user-full-name)
                          (current-time)
                          (emacs-uptime)
                          (garbage-collect)
                          (buffer-string)
                          (random)
                          (recent-keys)))))

    (format "%s-%s-4%s-%s%s-%s"
            (substring str 0 8)
            (substring str 8 12)
            (substring str 13 16)
            (format "%x" (+ 8 (random 4)))
            (substring str 17 20)
            (substring str 20 32))))

(defmacro tabnine-util--json-serialize (params)
  "Return PARAMS JSON serialized result."
  (declare (indent 2))
  (if (progn
	(require 'json)
	(fboundp 'json-serialize))
      `(json-serialize ,params
		       :null-object nil
		       :false-object :json-false)
    `(let ((json-false :json-false))
       (json-encode ,params 'utf-8))))

(defmacro tabnine-util--read-json (str)
  "Read JSON string STR.  and return the decoded object."
  (declare (indent 2))
  (if (progn
        (require 'json)
        (fboundp 'json-parse-string))
      `(json-parse-string ,str
                          :array-type 'array
                          :object-type 'plist
                          :null-object nil
                          :false-object :json-false)
    `(let ((json-array-type 'vector)
           (json-object-type 'plist)
           (json-false nil))
       (condition-case nil (json-read-from-string ,str)
	 (json-readtable-error 'json-error)
	 (json-error 'json-error)))))

(defun tabnine-util--get-list-errors()
  "Get current buffer error list."
  (when (and (fboundp 'flycheck-mode)
	     (boundp 'flycheck-mode)
	     flycheck-mode
	     (equal flycheck-last-status-change 'finished)
	     (> (length flycheck-current-errors) 0))
    (mapcar (lambda(x)
	      (let ((line (flycheck-error-line  x))
		    (filename (flycheck-error-filename x))
		    (buffer-name (buffer-file-name))
		    (message (flycheck-error-message x))
		    (region-begin-line (when (region-active-p) (save-excursion (goto-char (region-beginning)) (line-number-at-pos))) )
		    (region-end-line (when (region-active-p) (save-excursion  (goto-char (region-end)) (line-number-at-pos)))))
		(if buffer-name
		    (when (equal buffer-name filename)
		      (if (region-active-p)
			  (when (and (>= line region-begin-line) (<= line region-end-line))
			    message)
			message))
		  message))) flycheck-current-errors)))

(defun tabnine-util--markdown-codeblocks (text)
  "Get codeblocks from markdown TEXT.

Return codeblocks in sequence.
CodeBlock contains at least these keys:
code: code content in string
lang: language of code."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((codeblocks)
	  (code)
	  (lang))
      (while (re-search-forward
	      "```\\(.+\\)?\\([. \n\r\s\t[:word:][:space:][:multibyte:][:digit:][:punct:]_]+?\\)```" nil t)
	(setq lang (match-string 1))
	(setq code (list :code (match-string 2)
			 :lang lang))
	(setq codeblocks (vconcat codeblocks (vector code))))
      codeblocks)))

(provide 'tabnine-util)

;;; tabnine-util.el ends here
