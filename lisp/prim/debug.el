;; Debuggers and related commands for Emacs
;; Copyright (C) 1985, 1986, 1992 Free Software Foundation, Inc.

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


(defvar debug-function-list nil
  "List of functions currently set for debug on entry.")

(setq debugger 'debug)

(defun debug (&rest debugger-args)
  "Enter debugger.  Returns if user says \"continue\".
Arguments are mainly for use when this is called
 from the internals of the evaluator.
You may call with no args, or you may
 pass nil as the first arg and any other args you like.
 In that case, the list of args after the first will 
 be printed into the backtrace buffer."
  (message "Entering debugger...")
  (let (debugger-value
	(debugger-match-data (match-data))
	(debug-on-error nil)
	(debug-on-quit nil)
	(debugger-buffer (let ((default-major-mode 'fundamental-mode))
			   (generate-new-buffer "*Backtrace*")))
	(debugger-old-buffer (current-buffer))
	(debugger-step-after-exit nil)
	;; Don't keep reading from an executing kbd macro!
	(executing-macro nil)
	(cursor-in-echo-area nil))
    (unwind-protect
	(save-excursion
	  (save-window-excursion
	    (pop-to-buffer debugger-buffer)
	    (erase-buffer)
	    (let ((standard-output (current-buffer))
		  (print-escape-newlines t)
		  (print-length 50))
	      (backtrace))
	    (goto-char (point-min))
	    (debugger-mode)
	    (delete-region (point)
			   (progn
			     (re-search-forward "\n[* ] debug(")
			     (forward-line 1)
			     (point)))
	    (debugger-reenable)
	    (cond ((memq (car debugger-args) '(lambda debug))
		   (insert "Entering:\n")
		   (if (eq (car debugger-args) 'debug)
		       (progn
			 (backtrace-debug 4 t)
			 (delete-char 1)
			 (insert ?*)
			 (beginning-of-line))))
		  ((eq (car debugger-args) 'exit)
		   (insert "Return value: ")
		   (setq debugger-value (nth 1 debugger-args))
		   (prin1 debugger-value (current-buffer))
		   (insert ?\n)
		   (delete-char 1)
		   (insert ? )
		   (beginning-of-line))
		  ((eq (car debugger-args) 'error)
		   (insert "Signalling: ")
		   (prin1 (nth 1 debugger-args) (current-buffer))
		   (insert ?\n))
		  ((eq (car debugger-args) t)
		   (insert "Beginning evaluation of function call form:\n"))
		  (t
		   (prin1 (if (eq (car debugger-args) 'nil)
			      (cdr debugger-args) debugger-args)
			  (current-buffer))
		   (insert ?\n)))
	    (message "")
	    (let ((inhibit-trace t)
		  (standard-output nil)
		  (buffer-read-only t))
	      (message "")
	      (recursive-edit))))
      ;; So that users do not try to execute debugger commands
      ;;  in an invalid context
      (kill-buffer debugger-buffer)
      (store-match-data debugger-match-data))
    (setq debug-on-next-call debugger-step-after-exit)
    debugger-value))

(defun debugger-step-through ()
  "Proceed, stepping through subexpressions of this expression.
Enter another debugger on next entry to eval, apply or funcall."
  (interactive)
  (setq debugger-step-after-exit t)
  (message "Proceding, will debug on next eval or call.")
  (exit-recursive-edit))

(defun debugger-continue ()
  "Continue, evaluating this expression without stopping."
  (interactive)
  (message "Continuing.")
  (exit-recursive-edit))

(defun debugger-return-value (val)
  "Continue, specifying value to return.
This is only useful when the value returned from the debugger
will be used, such as in a debug on exit from a frame."
  (interactive "XReturn value (evaluated): ")
  (setq debugger-value val)
  (princ "Returning " t)
  (prin1 debugger-value)
  (exit-recursive-edit))

(defun debugger-jump ()
  "Continue to exit from this frame, with all debug-on-entry suspended."
  (interactive)
  ;; Compensate for the two extra stack frames for debugger-jump.
  (let ((debugger-frame-offset (+ debugger-frame-offset 2)))
    (debugger-frame))
  ;; Turn off all debug-on-entry functions
  ;; but leave them in the list.
  (let ((list debug-function-list))
    (while list
      (fset (car list)
	    (debug-on-entry-1 (car list) (symbol-function (car list)) nil))
      (setq list (cdr list))))
  (message "Continuing through this frame")
  (exit-recursive-edit))

(defun debugger-reenable ()
  ;; Turn all debug-on-entry functions back on.
  (let ((list debug-function-list))
    (while list
      (or (consp (symbol-function (car list)))
	  (debug-convert-byte-code (car list)))
      (fset (car list)
	    (debug-on-entry-1 (car list) (symbol-function (car list)) t))
      (setq list (cdr list)))))

(defun debugger-frame-number ()
  "Return number of frames in backtrace before the one point points at."
  (save-excursion
    (beginning-of-line)
    (let ((opoint (point))
	  (count 0))
      (goto-char (point-min))
      (if (or (equal (buffer-substring (point) (+ (point) 6))
		     "Signal")
	      (equal (buffer-substring (point) (+ (point) 6))
		     "Return"))
	  (progn
	    (search-forward ":")
	    (forward-sexp 1)))
      (forward-line 1)
      (while (progn
	       (forward-char 2)
	       (if (= (following-char) ?\()
		   (forward-sexp 1)
		 (forward-sexp 2))
	       (forward-line 1)
	       (<= (point) opoint))
	(setq count (1+ count)))
      count)))

;; Chosen empirically to account for all the frames
;; that will exist when debugger-frame is called
;; within the first one that appears in the backtrace buffer.
;; Assumes debugger-frame is called from a key;
;; will be wrong if it is called with Meta-x.
(defconst debugger-frame-offset 8 "")

(defun debugger-frame ()
  "Request entry to debugger when this frame exits.
Applies to the frame whose line point is on in the backtrace."
  (interactive)
  (beginning-of-line)
  (let ((level (debugger-frame-number)))
    (backtrace-debug (+ level debugger-frame-offset) t))
  (if (= (following-char) ? )
      (let ((buffer-read-only nil))
	(delete-char 1)
	(insert ?*)))
  (beginning-of-line))

(defun debugger-frame-clear ()
  "Do not enter to debugger when this frame exits.
Applies to the frame whose line point is on in the backtrace."
  (interactive)
  (beginning-of-line)
  (let ((level (debugger-frame-number)))
    (backtrace-debug (+ level debugger-frame-offset) nil))
  (if (= (following-char) ?*)
      (let ((buffer-read-only nil))
	(delete-char 1)
	(insert ? )))
  (beginning-of-line))

(defun debugger-eval-expression (exp)
  (interactive "xEval: ")
  (save-excursion
    (if (null (buffer-name debugger-old-buffer))
	;; old buffer deleted
	(setq debugger-old-buffer (current-buffer)))
    (set-buffer debugger-old-buffer)
    (eval-expression exp)))

(defvar debugger-mode-map nil)
(if debugger-mode-map
    nil
  (let ((loop ? ))
    (setq debugger-mode-map (make-keymap))
    (suppress-keymap debugger-mode-map)
    (define-key debugger-mode-map "-" 'negative-argument)
    (define-key debugger-mode-map "b" 'debugger-frame)
    (define-key debugger-mode-map "c" 'debugger-continue)
    (define-key debugger-mode-map "j" 'debugger-jump)
    (define-key debugger-mode-map "r" 'debugger-return-value)
    (define-key debugger-mode-map "u" 'debugger-frame-clear)
    (define-key debugger-mode-map "d" 'debugger-step-through)
    (define-key debugger-mode-map "l" 'debugger-list-functions)
    (define-key debugger-mode-map "h" 'describe-mode)
    (define-key debugger-mode-map "q" 'top-level)
    (define-key debugger-mode-map "e" 'debugger-eval-expression)
    (define-key debugger-mode-map " " 'next-line)))

(put 'debugger-mode 'mode-class 'special)

(defun debugger-mode ()
  "Mode for backtrace buffers, selected in debugger.
\\<debugger-mode-map>
A line starts with `*' if exiting that frame will call the debugger.
Type \\[debugger-frame] or \\[debugger-frame-clear] to set or remove the `*'.

When in debugger due to frame being exited,
use the \\[debugger-return-value] command to override the value
being returned from that frame.

Use \\[debug-on-entry] and \\[cancel-debug-on-entry] to control
which functions will enter the debugger when called.

Complete list of commands:
\\{debugger-mode-map}"
  (kill-all-local-variables)    
  (setq major-mode 'debugger-mode)
  (setq mode-name "Debugger")
  (setq truncate-lines t)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (use-local-map debugger-mode-map))

(defun debug-on-entry (function)
  "Request FUNCTION to invoke debugger each time it is called.
If the user continues, FUNCTION's execution proceeds.
Works by modifying the definition of FUNCTION,
which must be written in Lisp, not predefined.
Use \\[cancel-debug-on-entry] to cancel the effect of this command.
Redefining FUNCTION also does that."
  (interactive "aDebug on entry (to function): ")
  (debugger-reenable)
  (if (subrp (symbol-function function))
      (error "Function %s is a primitive" function))
  (or (consp (symbol-function function))
      (debug-convert-byte-code function))
  (or (consp (symbol-function function))
      (error "Definition of %s is not a list" function))
  (fset function (debug-on-entry-1 function (symbol-function function) t))
  (or (memq function debug-function-list)
      (setq debug-function-list (cons function debug-function-list)))
  function)

(defun cancel-debug-on-entry (&optional function)
  "Undo effect of \\[debug-on-entry] on FUNCTION.
If argument is nil or an empty string, cancel for all functions."
  (interactive "aCancel debug on entry (to function): ")
  (debugger-reenable)
  (if (and function (not (string= function "")))
      (progn
	(fset function
	      (debug-on-entry-1 function (symbol-function function) nil))
	(setq debug-function-list (delq function debug-function-list))
	function)
    (message "Cancelling debug-on-entry for all functions")
    (mapcar 'cancel-debug-on-entry debug-function-list)))

(defun debug-convert-byte-code (function)
  (let ((defn (symbol-function function)))
    (if (not (consp defn))
	;; Assume a compiled code object.
	(let* ((contents (append defn nil))
	       (body
		(list (list 'byte-code (nth 1 contents)
			    (nth 2 contents) (nth 3 contents)))))
	  (if (nthcdr 5 contents)
	      (setq body (cons (list 'interactive (nth 5 contents)) body)))
	  (if (nth 4 contents)
	      (setq body (cons (nth 4 contents) body)))
	  (fset function (cons 'lambda (cons (car contents) body)))))))

(defun debug-on-entry-1 (function defn flag)
  (if (subrp defn)
      (error "%s is a built-in function" function)
    (if (eq (car defn) 'macro)
	(debug-on-entry-1 function (cdr defn) flag)
      (or (eq (car defn) 'lambda)
	  (error "%s not user-defined Lisp function" function))
      (let (tail prec)
	(if (stringp (car (nthcdr 2 defn)))
	    (setq tail (nthcdr 3 defn)
		  prec (list (car defn) (car (cdr defn)) (car (cdr (cdr defn)))))
	  (setq tail (nthcdr 2 defn)
		prec (list (car defn) (car (cdr defn)))))
	(if (eq flag (equal (car tail) '(debug 'debug)))
	    defn
	  (if flag
	      (nconc prec (cons '(debug 'debug) tail))
	    (nconc prec (cdr tail))))))))

(defun debugger-list-functions ()
  "Display a list of all the functions now set to debug on entry."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (if (null debug-function-list)
	(princ "No debug-on-entry functions now\n")
      (princ "Functions set to debug on entry:\n\n")
      (let ((list debug-function-list))
	(while list
	  (prin1 (car list))
	  (terpri)
	  (setq list (cdr list))))
      (princ "Note: if you have redefined a function, then it may no longer\n")
      (princ "be set to debug on entry, even if it is in the list."))))
