;;; Pending delete selection
;;; Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.
;;; Created: 14 Jul 92, Matthieu Devin <devin@lucid.com>
;;; Last change  9-Sep-92. devin

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

;;; This files makes the active region be pending delete, meaning that
;;; text inserted while the region is active will replace the region contents.
;;; This is a popular behavior of personal computers text editors.

(defun delete-active-region (&optional killp)
  (if (and (not buffer-read-only)
	   (extentp primary-selection-extent)
	   (eq (current-buffer) (extent-buffer primary-selection-extent))
	   (< 0 (extent-start-position primary-selection-extent))
	   (< 0 (extent-end-position primary-selection-extent)))
      (progn
	(if killp
	    (kill-region (extent-start-position primary-selection-extent)
			 (extent-end-position primary-selection-extent))
	  (delete-region (extent-start-position primary-selection-extent)
			 (extent-end-position primary-selection-extent)))
	t)))

(defun pending-delete-pre-hook ()
  (let ((type (and (symbolp this-command)
		   (get this-command 'pending-delete))))
    (cond ((eq type 'kill)
	   (delete-active-region t))
	  ((eq type 'supersede)
	   (if (delete-active-region ())
	       (setq this-command '(lambda () (interactive)))))
	  (type
	   (delete-active-region ())))))

(put 'self-insert-command 'pending-delete t)

(put 'yank 'pending-delete t)
(put 'x-yank-clipboard-selection 'pending-delete t)

(put 'delete-backward-char 'pending-delete 'supersede)
(put 'backward-delete-char-untabify 'pending-delete 'supersede)
(put 'delete-char 'pending-delete 'supersede)

(put 'newline-and-indent 'pending-delete 't)
(put 'newline 'pending-delete t)
(put 'open-line 'pending-delete t)

(add-hook 'pre-command-hook 'pending-delete-pre-hook)

(setq zmacs-regions t)
