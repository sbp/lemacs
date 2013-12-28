;;; map-ynp.el --- General-purpose boolean question-asker.

;;; Copyright (C) 1991, 1992 Free Software Foundation, Inc.
;;; Written by Roland McGrath.
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
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to roland@ai.mit.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; map-y-or-n-p is a general-purpose question-asking function.
;;; It asks a series of y/n questions (a la y-or-n-p), and decides to
;;; applies an action to each element of a list based on the answer.
;;; The nice thing is that you also get some other possible answers
;;; to use, reminiscent of query-replace: ! to answer y to all remaining
;;; questions; ESC or q to answer n to all remaining questions; . to answer
;;; y once and then n for the remainder; and you can get help with C-h.

(defun map-y-or-n-p-help (object objects action)
  (format "Type SPC or `y' to %s the current %s;
DEL or `n' to skip the current %s;
! to %s all remaining %s;
ESC or `q' to exit;
or . (period) to %s the current %s and exit."
	  action object object action objects action object))

;;;###autoload
(defun map-y-or-n-p (prompter actor list &optional help)
  "Ask a series of boolean questions.
Takes args PROMPTER ACTOR LIST, and optional arg HELP.

LIST is a list of objects, or a function of no arguments to return the next
object or nil.

If PROMPTER is a string, the prompt is \(format PROMPTER OBJECT\).  If not
a string, PROMPTER is a function of one arg (an object from LIST), which
returns a string to be used as the prompt for that object.  If the return
value is not a string, it is eval'd to get the answer; it may be nil to
ignore the object, t to act on the object without asking the user, or a
form to do a more complex prompt.


ACTOR is a function of one arg (an object from LIST),
which gets called with each object that the user answers `yes' for.

If HELP is given, it is a list (OBJECT OBJECTS ACTION),
where OBJECT is a string giving the singular noun for an elt of LIST;
OBJECTS is the plural noun for elts of LIST, and ACTION is a transitive
verb describing ACTOR.  The default is \(\"object\" \"objects\" \"act on\"\).

At the prompts, the user may enter y, Y, or SPC to act on that object;
n, N, or DEL to skip that object; ! to act on all following objects;
ESC or q to exit (skip all following objects); . (period) to act on the
current object and then exit; or \\[help-command] to get help.

Returns the number of actions taken."
  (let* ((old-help-form help-form)
	 (help-form (cons 'map-y-or-n-p-help
			  (or help '("object" "objects" "act on"))))
	 (actions 0)
	 prompt
	 char
	 elt
	 (next (if (or (symbolp list)
		       (subrp list)
		       (compiled-function-p list)
		       (and (consp list)
			    (eq (car list) 'lambda)))
		   (function (lambda ()
			       (setq elt (funcall list))))
		 (function (lambda ()
			     (if list
				 (progn
				   (setq elt (car list)
					 list (cdr list))
				   t)
			       nil))))))
    (if (stringp prompter)
	(setq prompter (` (lambda (object)
			    (format (, prompter) object)))))
    (while (funcall next)
      (setq prompt (funcall prompter elt))
      (if (stringp prompt)
	  (progn
	    ;; Prompt the user about this object.
	    (let ((cursor-in-echo-area t))
	      (message "%s(y, n, ! ., q, or %s)"
		       prompt (key-description (char-to-string help-char)))
	      (setq char (read-char)))
	    (cond ((or (= ?q char)
		       (= ?\e char))
		   (setq next (function (lambda () nil))))
		  ((or (= ?y char)
		       (= ?Y char)
		       (= ?  char))
		   ;; Act on the object.
		   (let ((help-form old-help-form))
		     (funcall actor elt))
		   (setq actions (1+ actions)))
		  ((or (= ?n char)
		       (= ?N char)
		       (= ?\^? char))
		   ;; Skip the object.
		   )
		  ((= ?. char)
		   ;; Act on the object and then exit.
		   (funcall actor elt)
		   (setq actions (1+ actions)
			 next (function (lambda () nil))))
		  ((= ?! char)
		   ;; Act on this and all following objects.
		   (if (eval (funcall prompter elt))
		       (progn
			 (funcall actor elt)
			 (setq actions (1+ actions))))
		   (while (funcall next)
		     (if (eval (funcall prompter elt))
			 (progn
			   (funcall actor elt)
			   (setq actions (1+ actions))))))
		  ((= ?? char)
		   (setq unread-command-char help-char)
		   (setq next (` (lambda ()
				   (setq next '(, next))
				   '(, elt)))))
		  (t
		   ;; Random char.
		   (message "Type %s for help."
			    (key-description (char-to-string help-char)))
		   (beep)
		   (sit-for 1)
		   (setq next (` (lambda ()
				   (setq next '(, next))
				   '(, elt)))))))
	(if (eval prompt)
	    (progn
	      (funcall actor elt)
	      (setq actions (1+ actions))))))
    ;; Clear the last prompt from the minibuffer.
    (message "")
    ;; Return the number of actions that were taken.
    actions))

;;; map-ynp.el ends here
