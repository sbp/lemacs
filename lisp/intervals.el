;; Management of intervals as logical units.
;; Copyright (C) 1991 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'intervals)

;; Run the hook associated with class 'point-entered
(defun run-point-entered-hooks (i)
  (interval-class-hook i 'point-entered))
(defun run-point-left-hooks (i)
  (interval-class-hook i 'point-left))

;; Install as the interval hook the function which scans class hooks.
(defun set-point-entered-hook (i)
  (interval-put i 'point-entered 'run-point-entered-hooks))
(defun set-point-left-hook (i)
  (interval-put i 'point-left 'run-point-left-hooks))

;; Hilite all intervals of class 'point-test
(defun hilite-class (i)
  (with-interval-class
   'point-test '(lambda (i)
		  (set-interval-attributes i '((foreground . "blue")
					       (background . "yellow"))))))
(defun unhilite-class (i)
  (with-interval-class
   'point-test '(lambda (i)
		  (set-interval-attributes i '((foreground . "cyan")
					       (background . "darkgreen"))))))


;; Click someplace to make an interval class between point and
;; mouse.  This also sets up point-entered and point-left hooks.

(defun make-point-class (event)
  (interactive "@e")
  (save-excursion
      (let ((point-save (point)))
	  (mouse-set-point event)
	  (if (> (point) point-save)
	      (make-interval-class point-save (point) 'point-test)
	    (make-interval-class (point) point-save 'point-test))
	  (set-class-hook 'point-test '((point-entered . hilite-class)
					(point-left . unhilite-class)))
	  (with-interval-class 'point-test 'set-point-entered-hook)
	  (with-interval-class 'point-test 'set-point-left-hook))))
