;; pending-del.el --- Making insertions replace any selected text.

;; Copyright (C) 1992, 1994 Free Software Foundation, Inc.

;; Author: Matthieu Devin <devin@lucid.com>, 14 Jul 92.

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

;;; Code:

(defvar pending-delete-verbose
  1
  "*nil disables on/off messages for pending-del mode
1 suppresses messages on loading
t enables all messages")


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
	(zmacs-deactivate-region)
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

;;;###autoload
(defun pending-delete-on (verbose)
  "Turn on pending delete.
When it is ON typed text replaces the selection if the selection is active.
When it is OFF typed text is just inserted at point."
  (interactive "P")
  (add-hook 'pre-command-hook 'pending-delete-pre-hook)
  (and verbose
    (message "Pending delete is ON, use M-x pending-delete to turn it OFF")))

;;;###autoload
(defun pending-delete-off (verbose)
  "Turn on pending delete.
When it is ON typed text replaces the selection if the selection is active.
When it is OFF typed text is just inserted at point."
  (interactive "P")
  (remove-hook 'pre-command-hook 'pending-delete-pre-hook)
  (and verbose (message "pending delete is OFF")))

;;;###autoload
(defun pending-delete (&optional arg)
  "Toggle automatic deletion of the selected region.
With a positive argument, turns it on.
With a non-positive argument, turns it off.
When active, typed text replaces the selection."
  (interactive "P")
  (let* ((was-on (not (not (memq 'pending-delete-pre-hook pre-command-hook))))
	 (on-p (if (null arg)
		   (not was-on)
		(> (prefix-numeric-value arg) 0))))
    (cond ((eq on-p was-on)
	   nil)
	  (on-p
	   (pending-delete-on pending-delete-verbose))
	  (t
	   (pending-delete-off pending-delete-verbose)))))
  
;; Add pending-del mode.  Assume that if we load it then we obviously wanted
;; it on, even if it is already on.
(pending-delete-on (eq pending-delete-verbose t))

(provide 'pending-del)

  
;; This new definition of control-G makes the first control-G disown the 
;; selection and the second one signal a QUIT.
;; This is very useful for cancelling a selection in the minibuffer without 
;; aborting the minibuffer.
;; It has actually nothing to do with pending-delete but its more necessary
;; with pending delete because pending delete users use the selection more.
(defun keyboard-quit ()
  "Signal a `quit' condition.
If this character is typed while lisp code is executing, it will be treated
 as an interrupt.
If this character is typed at top-level, this simply beeps.
If `zmacs-regions' is true, and the zmacs region is active, then this
 key deactivates the region without beeping or signalling."
  (interactive)
  (if (and zmacs-regions (zmacs-deactivate-region))
      ;; pseudo-zmacs compatibility: don't beep if this ^G is simply
      ;; deactivating the region.  If it is inactive, beep.
      nil
    (signal 'quit nil)))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit
If `zmacs-regions' is true, and the zmacs region is active, then this
 key deactivates the region without beeping."
  (interactive)
  (if (and zmacs-regions (zmacs-deactivate-region))
      ;; pseudo-zmacs compatibility: don't beep if this ^G is simply
      ;; deactivating the region.  If it is inactive, beep.
      nil
    (abort-recursive-edit)))

(define-key minibuffer-local-map '(control g) 'minibuffer-keyboard-quit) 

;;; pending-del.el ends here
