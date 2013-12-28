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
;;; Multi-Lingual Emacs (MULE) Specific Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-create-faces ()
  "Create faces, the no-quite-so-dumb MULE way"
  (setq w3-delimit-links nil))

(defvar attributed-region nil
  "Bogus definition to get rid of compile-time warnings.")

(defun w3-mule-attribute-zones (zones attr)
  (save-excursion
    (let ((c 0) l z beg end)
      (while (setq z (nth c zones))
	(setq beg (nth 0 z))
	(setq end (nth 1 z))
	(setq l (cons (cons beg attr) l))
	(goto-char (marker-position beg))
	(end-of-line)
	(while (< (point) (marker-position end))
	  (setq l (cons (cons (copy-marker (point)) 0) l))
	  (forward-line 1)
	  (skip-chars-forward " \t")
	  (if (= (point) (marker-position end)) t
	    (setq l (cons (cons (copy-marker (point)) attr) l)))
	  (end-of-line))
	(setq l (cons (cons end 0) l))
	(setq c (1+ c)))
      (setq attributed-region (cons '(0 . 0) (reverse l))))))

(provide 'w3-mule)
