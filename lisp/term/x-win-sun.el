;;; x-win-sun.el --- runtime initialization for Sun X servers and keyboards
;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author: jwz@lucid.com
;; Keywords: terminals

;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file is loaded by x-win.el at run-time when we are sure that emacs
;; is running on the display of a Sun.

;; The Sun X server (both the MIT and OpenWindows varieties) have extremely
;; stupid names for their keypad and function keys.  For example, the key
;; labeled 3 / PgDn, with R15 written on the front, is actually called F35.

;; Since we don't yet have a key-translation-map, we do this kludge to repair
;; things.  We do bindings of the keys which are actually generated by the
;; keyboard, to give them sensible names.  The user can then bind either
;; (for example) `pgdn' or `f35' and get the expected behavior.

;; Once key-translation-map exists, this should be done differently.
;; The current implementation has the bug that `C-x f35' does not get
;; translated to `C-x pgdn'.  (But we do translate `C-f35' to `C-pgdn'.)

;;; Code:

(let ((mapping '((f11 . stop)		; the type4 keyboard Sun/MIT name
		 (f36 . stop)		; the type5 keyboard Sun name
		 (f12 . again)		; the type4 keyboard Sun/MIT name
		 (f37 . again)		; the type5 keyboard Sun name
		 (f13 . props)
		 (f14 . undo)
		 (f15 . front)
		 (f16 . copy)
		 (f17 . open)
		 (f18 . paste)
		 (f19 . find)
		 (f20 . cut)
		 ;; help is ok
		 (f21 . pause)
		 (f22 . prsc)
		 (f23 . scroll)
		 ;; num_lock is ok
		 ;;(f24 . kp_equal)	; type4 only!
		 (f25 . kp_divide)
		 (f26 . kp_multiply)
		 (f24 . kp_subtract)	; type5 only!
		 (f27 . home)		; note: not kp_7
		 ;; up is ok
		 (f29 . pgup)
		 ;; left is ok
		 (f31 . kp_5)
		 ;; right is ok
		 ;; kp_add is ok
		 (f33 . end)		; the Sun name
		 (r13 . end)		; the MIT name
		 ;; down is ok
		 (f35 . pgdn)
		 ;; insert is ok
		 ;; delete is ok
		 ;; kp_enter is ok
		 )))
  ;; for each element in the left column of the above table, alias it to
  ;; the thing the right column.  Then do the same with the shift, control,
  ;; meta, and meta-control versions of the keys.
  (while mapping
    (let ((mods '(() (shift) (control) (meta) (meta control))))
      (while mods
	(let ((k1 (vector (append (car mods) (list (car (car mapping))))))
	      (k2 (vector (append (car mods) (list (cdr (car mapping)))))))
	  (define-key global-map k1 k2))
	(setq mods (cdr mods))))
    (setq mapping (cdr mapping))))



;;; OpenWindows-like "find" processing.
;;; As far as I know, the `find' key is a Sunism, so we do that binding
;;; here.  This is the only Sun-specific keybinding.  (The functions 
;;; themselves are in x-win.el in case someone wants to use them when
;;; not running on a Sun display.)

(define-key global-map 'find		'ow-find)
(define-key global-map '(shift find)	'ow-find-backward)

;;; x-win-sun.el ends here
