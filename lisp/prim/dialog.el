;; Dialog-box support.
;; Copyright (C) 1991, 1992, 1993, 1994 Free Software Foundation, Inc.

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

(defun yes-or-no-p-dialog-box (prompt)
  "Ask user a \"y or n\" question with a popup dialog box.
Returns t if answer is \"yes\".
Takes one argument, which is the string to display to ask the question."
  (let ((echo-keystrokes 0)
	event)	 
    (popup-dialog-box
     ;; "Non-violent language please!" says Robin.
     (cons prompt '(["Yes" yes t] ["No" no t] nil ["Cancel" abort t])))
;     (cons prompt '(["Yes" yes t] ["No" no t] nil ["Abort" abort t])))
    (catch 'ynp-done
      (while t
	(setq event (next-command-event event))
	(cond ((and (menu-event-p event) (eq (event-object event) 'yes))
	       (throw 'ynp-done t))
	      ((and (menu-event-p event) (eq (event-object event) 'no))
	       (throw 'ynp-done nil))
	      ((and (menu-event-p event)
		    (or (eq (event-object event) 'abort)
			(eq (event-object event) 'menu-no-selection-hook)))
	       (signal 'quit nil))
	      ((button-release-event-p event) ;; don't beep twice
	       nil)
	      (t
	       (beep)
	       (message "please answer the dialog box")))))))

(defun yes-or-no-p-maybe-dialog-box (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes.
The question is asked with a dialog box or the minibuffer, as appropriate.
Takes one argument, which is the string to display to ask the question.
It should end in a space; `yes-or-no-p' adds `(yes or no) ' to it.
The user must confirm the answer with RET,
and can edit it until it as been confirmed."
  (if (or (button-press-event-p last-command-event)
	  (button-release-event-p last-command-event)
	  (menu-event-p last-command-event))
      (yes-or-no-p-dialog-box prompt)
    (yes-or-no-p-minibuf prompt)))

(defun y-or-n-p-maybe-dialog-box (prompt)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".
Takes one argument, which is the string to display to ask the question.
The question is asked with a dialog box or the minibuffer, as appropriate.
It should end in a space; `y-or-n-p' adds `(y or n) ' to it.
No confirmation of the answer is requested; a single character is enough.
Also accepts Space to mean yes, or Delete to mean no."
  (if (or (button-press-event-p last-command-event)
	  (button-release-event-p last-command-event)
	  (menu-event-p last-command-event))
      (yes-or-no-p-dialog-box prompt)
    (y-or-n-p-minibuf prompt)))

(if (fboundp 'popup-dialog-box)
    (progn
      (fset 'yes-or-no-p 'yes-or-no-p-maybe-dialog-box)
      (fset 'y-or-n-p 'y-or-n-p-maybe-dialog-box)))


(provide 'dialog)
