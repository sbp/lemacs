;; interactive turn on and off of the font-lock-mode
;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;; This file is preloaded, but font-lock.el is not.
;; Make the font-lock faces exist at startup time, so that people can
;; mess with them in their .emacs file.
;;
;; They will be initialized either: from the resource db when the first
;; screen is created; or by font-lock.el when it is first loaded.

(make-face 'font-lock-comment-face)
(make-face 'font-lock-doc-string-face)
(make-face 'font-lock-string-face)
(make-face 'font-lock-function-name-face)
(make-face 'font-lock-keyword-face)
(make-face 'font-lock-type-face)


;; Fontify Energize Error Log
(defconst energize-log-font-lock-keywords 
  (purecopy
   '(("^ *Note:.*$"			. font-lock-string-face)
     ("^ *Warning:.*$"			. font-lock-keyword-face)
     ("^ *Error:.*$"			. font-lock-function-name-face)
     ("^/.* file system is full\r?$"	. font-lock-function-name-face)
     ))
  "Expressions to highlight in the Energize Error Log.")
(add-hook 'energize-log-mode-hook 'turn-on-font-lock)


;; this thing is such a kludge...
(defun energize-configure-font-lock-mode (on-p font-p less-p)
  (cond (on-p
	 (add-hook 'c++-mode-hook 'turn-on-font-lock)
	 (add-hook 'c-mode-hook 'turn-on-font-lock)
	 (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
	 (add-hook 'lisp-mode-hook 'turn-on-font-lock))
	(t
	 (remove-hook 'c++-mode-hook 'turn-on-font-lock)
	 (remove-hook 'c-mode-hook 'turn-on-font-lock)
	 (remove-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
	 (remove-hook 'lisp-mode-hook 'turn-on-font-lock)))
  (cond (font-p
	 (remove-hook 'font-lock-mode-hook 'font-lock-use-default-colors)
	 (add-hook 'font-lock-mode-hook 'font-lock-use-default-fonts))
	(t
	 (remove-hook 'font-lock-mode-hook 'font-lock-use-default-fonts)
	 (add-hook 'font-lock-mode-hook 'font-lock-use-default-colors)))
  (cond (less-p
	 (remove-hook 'font-lock-mode-hook
		      'font-lock-use-default-maximal-decoration)
	 (add-hook 'font-lock-mode-hook
		   'font-lock-use-default-minimal-decoration))
	(t
	 (remove-hook 'font-lock-mode-hook
		      'font-lock-use-default-minimal-decoration)
	 (add-hook 'font-lock-mode-hook
		   'font-lock-use-default-maximal-decoration))))

;; turn the default set-up on
(energize-configure-font-lock-mode t t t)
