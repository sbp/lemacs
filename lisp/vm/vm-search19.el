;;; Incremental search through a mail folder
;;; For Lucid Emacs and FSF Emacs 19
;;; Copyright (C) 1989, 1993 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Version 18 code first adapted to v19 isearch-mode
;;; by Tim Bradshaw, March 1993

(defun vm-isearch-forward ()
  "Incrementally search forward through the current folder's messages.
Usage is identical to the standard Emacs incremental search.
When the search terminates the message containing point will be selected.

If the variable vm-search-using-regexps is non-nil, regular expressions
are understood; nil means the search will be for the input string taken
literally."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-error-if-virtual-folder)
  (vm-set-window-configuration 'searching-folder)
  (if (null (get-buffer-window (current-buffer)))
      (vm-display-current-message-buffer))
  (let ((clip-head (point-min))
	(clip-tail (point-max))
	(old-w (selected-window)))
    (unwind-protect
	(progn (select-window (get-buffer-window (current-buffer)))
	       (widen)
	       (isearch-mode t vm-search-using-regexps nil t)
	       (vm-update-search-position)
	       ;; vm-show-current-message only adjusts (point-max),
	       ;; it doesn't change (point-min).
	       (narrow-to-region
		(if (< (point) (vm-vheaders-of (car vm-message-pointer)))
		    (vm-start-of (car vm-message-pointer))
		  (vm-vheaders-of (car vm-message-pointer)))
		(point-max))
	       (vm-show-current-message)
	       (setq vm-system-state 'reading)
	       ;; turn the clipping unwind into a noop
	       (setq clip-head (point-min))
	       (setq clip-tail (point-max)))
      (narrow-to-region clip-head clip-tail)
      (select-window old-w))))


(defun vm-update-search-position (&optional record-change)
  (if (and (>= (point) (vm-start-of (car vm-message-pointer)))
	   (<= (point) (vm-end-of (car vm-message-pointer))))
      nil
    (let ((mp vm-message-list)
	  (point (point)))
      (while mp
	(if (and (>= point (vm-start-of (car mp)))
		 (<= point (vm-end-of (car mp))))
	    (if record-change
		(progn
		  (vm-record-and-change-message-pointer vm-message-pointer mp)
		  (setq mp nil))
	      (setq vm-message-pointer mp mp nil))
	  (setq mp (cdr mp))))
      (setq vm-need-summary-pointer-update t)
      (vm-update-summary-and-mode-line))))
