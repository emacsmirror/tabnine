;;; tabnine.el --- An unofficial TabNine package with TabNine Chat supported -*- lexical-binding: t -*-
;;
;; Copyright (c) 2023  Aaron Ji
;;
;; Author: Aaron Ji <shuxiao9058@gmail.com>
;;         Tommy Xiang <tommyx058@gmail.com>
;;         John Gong <gjtzone@hotmail.com>
;;
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (dash "2.16.0") (s "1.12.0") (editorconfig "0.9.1") (vterm "0.0.2") (language-id "0.5.1") (transient "0.4.0"))
;; Keywords: convenience
;; URL: https://github.com/shuxiao9058/tabnine/

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

;; This file is NOT part of GNU Emacs.

;;
;;; Commentary:
;;
;; An unofficial TabNine package with TabNine Chat supported.
;;
;; Installation:
;;
;; 1. Install `tabnine` with your favorite package manager.
;; 2. Enable `tabnine-mode` in `prog-mode`(if you want to use `capf` only, skip this step).
;; (add-to-list 'prog-mode-hook #'tabnine-mode)
;; 3. Run M-x tabnine-install-binary to install the TabNine binary for your system.
;; 4. Run M-x tabnine-configuration to login your TabNine account.
;;
;; Usage:
;;
;; See M-x customize-group RET tabnine RET for customizations.
;;
;;

;;; Code:

;;
;; Dependencies
;;

(require 'tabnine-core)
(require 'tabnine-chat)
(require 'tabnine-chat-transient)

(provide 'tabnine)

;;; tabnine.el ends here
