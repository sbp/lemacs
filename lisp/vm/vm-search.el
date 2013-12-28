;; Incremental search through a mail folder
;; Copyright (C) 1985, 1986, 1993 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Adapted for the VM mail reader, Kyle Jones, May 1989
;; And for isearch-mode, Tim Bradshaw, March 1993

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
	       (vm-highlight-headers (car vm-message-pointer)
				     (get-buffer-window (current-buffer)))
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
