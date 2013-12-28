;;; Commands for handling messages marks
;;; Copyright (C) 1990 Kyle E. Jones
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

(defun vm-clear-all-marks ()
  "Removes all message marks in the current folder."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((mp vm-message-list))
    (while mp
      (vm-set-mark-of (car mp) nil)
      (vm-mark-for-display-update (car mp))
      (setq mp (cdr mp))))
  (vm-update-summary-and-mode-line))

(defun vm-mark-all-messages ()
  "Mark all messages in the current folder."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((mp vm-message-list))
    (while mp
      (vm-set-mark-of (car mp) t)
      (vm-mark-for-display-update (car mp))
      (setq mp (cdr mp))))
  (vm-update-summary-and-mode-line))

(defun vm-mark-message (count)
  "Mark the current message.
Numeric prefix argument N means mark the current message and the next
N-1 messages.  A negative N means mark the current message and the
previous N-1 messages."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if (not (eq vm-circular-folders t))
      (vm-check-count count))
  (let ((direction (if (< count 0) 'backward 'forward))
	(count (vm-abs count))
	(oldmp vm-message-pointer)
	(vm-message-pointer vm-message-pointer))
    (while (not (zerop count))
      (if (not (vm-mark-of (car vm-message-pointer)))
	  (progn
	    (vm-set-mark-of (car vm-message-pointer) t)
	    (vm-mark-for-display-update (car vm-message-pointer))))
      (vm-decrement count)
      (if (not (zerop count))
	  (vm-move-message-pointer direction))))
  (vm-update-summary-and-mode-line))

(defun vm-unmark-message (count)
  "Remove the mark from the current message.
Numeric prefix argument N means unmark the current message and the next
N-1 messages.  A negative N means unmark the current message and the
previous N-1 messages."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if (not (eq vm-circular-folders t))
      (vm-check-count count))
  (let ((direction (if (< count 0) 'backward 'forward))
	(count (vm-abs count))
	(oldmp vm-message-pointer)
	(vm-message-pointer vm-message-pointer))
    (while (not (zerop count))
      (if (vm-mark-of (car vm-message-pointer))
	  (progn
	    (vm-set-mark-of (car vm-message-pointer) nil)
	    (vm-mark-for-display-update (car vm-message-pointer))))
      (vm-decrement count)
      (if (not (zerop count))
	  (vm-move-message-pointer direction))))
  (vm-update-summary-and-mode-line))

(defun vm-next-command-uses-marks ()
  "Does nothing except insure that the next VM command will operate only
on the marked messages in the current folder."
  (interactive)
  (message "Next command uses marks...")
  (if (fboundp 'next-command-event)
      (setq unread-command-event (next-command-event (allocate-event)))
    (setq unread-command-char (read-char))))

(defun vm-marked-messages ()
  (let (list (mp vm-message-list))
    (while mp
      (if (vm-mark-of (car mp))
	  (setq list (cons (car mp) list)))
      (setq mp (cdr mp)))
    (nreverse list)))

(defun vm-mark-help ()
  (interactive)
  (message "MM = mark, MU = unmark, Mm = mark all, Mu = unmark all, MN - use marks"))
