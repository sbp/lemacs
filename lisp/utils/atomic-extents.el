;;; atomic-extents.el --- treat regions of text as a single object
;;; Copyright (C) 1993 Free Software Foundation, Inc.
;;; Created: 21-Dec-93, Chuck Thompson <cthomp@cs.uiuc.edu>

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Point is not allowed to fall inside of an atomic extent.  This has
;;; the effect of making all text covered by an atomic extent be
;;; treated as a single object.  Normally point will be adjusted to an
;;; end of an atomic extent in the direction of motion.  If point
;;; appears inside of an atomic extent (via goto-char for example),
;;; point will be adjusted to the side closest to the entry point.

;;; To make an extent atomic use the command:
;;;	(set-extent-property #<extent obj> 'atomic t)

;;; Known bug: the atomic property is not detected when sweeping
;;; regions with the mouse until after the mouse button is released.
;;; The release point will then be treated as if it had been reached
;;; using 'goto-char.

;;; atomic-extent-goto-char-p is defined in editfns.c

(provide 'atomic-extents)

(defvar atomic-extent-old-point nil
  "The value of point when pre-command-hook is called.
Used to determine the direction of motion.")

(defun atomic-extent-pre-hook ()
  (setq atomic-extent-old-point (point))
  (setq atomic-extent-goto-char-p nil))

(defun atomic-extent-post-hook ()
  (let ((extent (extent-at (point) nil 'atomic)))
    (if extent
	(let ((begin (extent-start-position extent))
	      (end (extent-end-position extent))
	      (pos (point))
	      (region-set (and (point) (mark))))
	  (if (/= pos begin)
	      (if atomic-extent-goto-char-p
		  (progn
		    (if (> (- pos begin) (- end pos))
			(goto-char end)
		      (goto-char begin)))
		(if (> pos atomic-extent-old-point)
		    (goto-char end)
		  (goto-char begin))))
	  (if (and region-set (/= pos begin))
	      (run-hooks 'zmacs-update-region-hook))))))

(add-hook 'pre-command-hook 'atomic-extent-pre-hook)
(add-hook 'post-command-hook 'atomic-extent-post-hook)
