;; GNU Emacs window commands aside from those written in C.
;; Copyright (C) 1985-1993 Free Software Foundation, Inc.

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


(defun count-windows (&optional minibuf)
   "Returns the number of visible windows.
Optional arg NO-MINI non-nil means don't count the minibuffer
even if it is active."
   (let ((count 0))
     (walk-windows (function (lambda ()
			       (setq count (+ count 1))))
		   minibuf)
     count))

(defun balance-windows ()
  "Makes all visible windows the same size (approximately)."
  (interactive)
  (let ((count 0))
    (walk-windows (function (lambda (w)
			      (setq count (+ count 1))))
		  'nomini)
    (let ((size (/ (screen-height) count)))
      (walk-windows (function (lambda (w)
				(select-window w)
				(enlarge-window (- size (window-height)))))
		    'nomini))))

(defun split-window-vertically (&optional arg hack-display)
  "Split current window into two windows, one above the other.
The top window gets ARG lines.  No arg means split equally.

The two windows will be displaying the same text as before: both windows will
be displaying the current buffer, but the second window will be scrolled such
that little redisplay will happen - the lines that were on the screen before
the split will still be on the screen, in the same places.  

An effort is made to keep the cursor in the same place relative to the text on
the screen as well.  If the cursor is below the split-point before the split,
then the bottom window will be selected; otherwise the top window will be
selected."
  (interactive (list current-prefix-arg t))
  (let ((old-w (selected-window))
	new-w)
    (setq new-w (split-window nil (and arg (prefix-numeric-value arg))))
    (if hack-display
	(let (bottom)
	  (save-excursion
	    (set-buffer (window-buffer))
	    (goto-char (window-start))
	    (vertical-motion (window-height))
	    (set-window-start new-w (point))
	    (if (>= (point) (window-point new-w))
		(set-window-point new-w (point)))
	    (vertical-motion -1)
	    (if (pos-visible-in-window-p (point) new-w)
		(set-window-start new-w (point)))
	    (setq bottom (point)))
	  (if (<= bottom (point))
	      (progn
		(set-window-point old-w (1- bottom))
		(select-window new-w)))))
    new-w))

(defun split-window-horizontally (&optional arg)
  "Split current window into two windows side by side.
This window becomes the leftmost of the two, and gets
ARG columns.  No arg means split equally."
  (interactive "P")
  (split-window nil (and arg (prefix-numeric-value arg)) t))

(defun enlarge-window-horizontally (arg)
  "Make current window ARG columns wider."
  (interactive "p")
  (enlarge-window arg t))

(defun shrink-window-horizontally (arg)
  "Make current window ARG columns narrower."
  (interactive "p")
  (shrink-window arg t))

(defun window-config-to-register (name)
  "Save the current window configuration in register REG (a letter).
It can be later retrieved using \\[M-x register-to-window-config]."
  (interactive "cSave window configuration in register: ")
  (set-register name (current-window-configuration)))

(defun register-to-window-config (name)
  "Restore (make current) the window configuration in register REG (a letter).
Use with a register previously set with \\[window-config-to-register]."
  (interactive "cRestore window configuration from register: ")
  (set-window-configuration (get-register name)))
