;; Timeout hackery.
;; Copyright (C) 1992 Free Software Foundation, Inc.

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

(defun with-timeout-timer (tag)
  ;; I'm pretty sure the condition-case isn't really necessary here,
  ;; but it doesn't hurt.
  (condition-case () (throw tag nil) (no-catch nil)))

;;;###autoload
(defmacro with-timeout (seconds-and-timeout-forms &rest body)
  "Usage: (with-timeout (seconds &rest timeout-forms) &rest body)
This is just like progn, but if the given number of seconds expires before
the body returns, then timeout-forms are evaluated and returned instead.
The body won't be interrupted in the middle of a computation: the check for 
the timer expiration only occurs when body does a redisplay, or prompts the
user for input, or calls accept-process-output."
  (let ((seconds (car seconds-and-timeout-forms))
	(timeout-forms (cdr seconds-and-timeout-forms)))
    (` (let* ((with-timeout-tag (make-symbol "_with_timeout_"))
	      (with-timeout-timeout
	       (add-timeout (, seconds) 'with-timeout-timer with-timeout-tag)))
	 (unwind-protect
	     (let ((value (catch with-timeout-tag
			    (prog1 (progn (,@ body))
			      (setq with-timeout-tag nil)))))
	       (if with-timeout-tag
		   (progn (,@ timeout-forms))
		 value))
	   (disable-timeout with-timeout-timeout))))))

(put 'with-timeout 'lisp-indent-function 1)

;;;###autoload
(defun yes-or-no-p-with-timeout (timeout prompt &optional default-value)
  "Just like yes-or-no-p, but will time out after TIMEOUT seconds
if the user has not yes answered, returning DEFAULT-VALUE."
  (with-timeout (timeout
		 (message (concat prompt "(yes or no) Timeout to "
				  (if default-value "Yes" "No")))
		 default-value)
    (yes-or-no-p prompt)))

;;;###autoload
(defun y-or-n-p-with-timeout (timeout prompt &optional default-value)
  "Just like y-or-n-p, but will time out after TIMEOUT seconds
if the user has not yes answered, returning DEFAULT-VALUE."
  (with-timeout (timeout
		 (message (concat prompt "(yes or no) Timeout to "
				  (if default-value "Yes" "No")))
		 default-value)
    (y-or-n-p prompt)))
