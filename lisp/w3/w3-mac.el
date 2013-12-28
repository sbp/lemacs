;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)
;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macintosh emacs specifics... these could kill your emacs
;;; Resistance is futile, you will be quickdrawn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun w3-install-menu-lucid-style (menudesc)
  "Install a lucid style menu on the mac version of emacs 18"
  (let ((mnu (NewMenu (get-unique-menu-ID) (car menudesc)))
	tmp)
    (setq menudesc (cdr menudesc))
    (while menudesc
      (setq tmp (car menudesc)
	    menudesc (cdr menudesc))
      (cond
       ((vectorp tmp)			; Menu description
	(AppendMenu mnu (aref tmp 0) (list
				      'lambda (list 'x 'y)
				      (list (aref tmp 1)))))
       ((stringp tmp)			; Separator
	(AppendMenu mnu "(-" nil))
       ((listp tmp)			; Submenu
	nil)				; Ignore for now
       ((null tmp)			; Null??
	(AppendMenu mnu "(-" nil))
       (t (message "Bad menu descriptor %S" tmp))))
    (InsertMenu mnu nil)
    (DrawMenuBar)))

(defun w3-install-mac-menus ()
  "Install menus on a macintosh"
  (mapcar 'w3-install-menu-lucid-style
	  '(w3-popup-menu w3-help-menu w3-annotation-menu w3-navigate-menu
			  w3-options-menu)))
