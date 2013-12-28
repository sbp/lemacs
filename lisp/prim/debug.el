;;; debug.el --- debuggers and related commands for Emacs

;; Copyright (C) 1985, 1986, 1993, 1994 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keyword: lisp, tools

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

(defvar debug-function-list nil
  "List of functions currently set for debug on entry.")

;;>>> This is terminally random an nigh-unmaintainable.
;;>>>  (need progv in elisp...)
(defvar debugger-outer-last-command)
(defvar debugger-outer-this-command)
(defvar debugger-outer-unread-command-event)
(defvar debugger-outer-last-input-event)
(defvar debugger-outer-last-input-char)
(defvar debugger-outer-last-input-time)
(defvar debugger-outer-last-command-event)
(defvar debugger-outer-last-command-char)
(defvar debugger-outer-standard-input)
(defvar debugger-outer-standard-output)
(defvar debugger-outer-cursor-in-echo-area)

;;;don't ###autoload, loadup.el does something smarter.
(setq debugger 'debug)

(defvar debugger-step-after-exit)
(defvar debugger-old-buffer)
(defvar debugger-value)

;;;###autoload
(defun debug (&rest debugger-args)
  "Enter debugger.  To return, type \\<debugger-mode-map>`\\[debugger-continue]'.
Arguments are mainly for use when this is called
 from the internals of the evaluator.
You may call with no args, or you may
 pass nil as the first arg and any other args you like.
 In that case, the list of args after the first will 
 be printed into the backtrace buffer."
  (message (gettext "Entering debugger..."))
  (let (debugger-value
	(debugger-match-data (match-data))
	(debug-on-error nil)
	(debug-on-quit nil)
	(debugger-buffer (let ((default-major-mode 'fundamental-mode))
			   (generate-new-buffer (gettext "*Backtrace*"))))
	(debugger-old-buffer (current-buffer))
	(debugger-step-after-exit nil)
        ;; Don't keep reading from an executing kbd macro!
        (executing-macro nil)
        ;; Save the outer values of these vars for the `e' command
	;; before we replace the values.
        (debugger-outer-last-command last-command)
        (debugger-outer-this-command this-command)
        (debugger-outer-unread-command-event unread-command-event)
        (debugger-outer-last-input-event last-input-event)
        (debugger-outer-last-input-char last-input-char)
        (debugger-outer-last-input-time last-input-time)
        (debugger-outer-last-command-event last-command-event)
        (debugger-outer-last-command-char last-command-char)
        (debugger-outer-standard-input standard-input)
        (debugger-outer-standard-output standard-output)
        (debugger-outer-cursor-in-echo-area cursor-in-echo-area)
        )
    ;; Don't let these magic variables affect the debugger itself.
    (unwind-protect                     ;lemacs change
         (let ((last-command nil)
               (this-command nil)
               (unread-command-event nil)
               (last-input-event (allocate-event))
               (last-input-char -1)
               (last-input-time nil)
               (last-command-event (allocate-event))
               (last-command-char -1)
               (standard-input t)
               (standard-output t)
               (cursor-in-echo-area nil))
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
                      (insert (gettext "Entering:\n"))
                      (if (eq (car debugger-args) 'debug)
                          (progn
                            (backtrace-debug 4 t)
                            (delete-char 1)
                            (insert ?*)
                            (beginning-of-line))))
                     ((eq (car debugger-args) 'exit)
                      (insert (gettext "Return value: "))
                      (setq debugger-value (nth 1 debugger-args))
                      (prin1 debugger-value (current-buffer))
                      (insert ?\n)
                      (delete-char 1)
                      (insert ? )
                      (beginning-of-line))
                     ((eq (car debugger-args) 'error)
                      (insert (gettext "Signalling: "))
                      (prin1 (nth 1 debugger-args) (current-buffer))
                      (insert ?\n))
                     ((eq (car debugger-args) t)
                      (insert
                       (gettext "Beginning evaluation of function call form:\n")))
                     (t
                      (prin1 (if (eq (car debugger-args) 'nil)
                                 (cdr debugger-args) debugger-args)
                             (current-buffer))
                      (insert ?\n)))
               (message "")
               (let ((inhibit-trace t)
                     (standard-output nil)
                     (buffer-read-only t))
                 (message nil)
                 (recursive-edit)))
             debugger-value))
      ;; So that users do not try to execute debugger commands
      ;;  in an invalid context
      (kill-buffer debugger-buffer)
      (store-match-data debugger-match-data)
      ;; Put into effect the modified values of these variables
      ;; in case the user set them with the `e' command.
      (setq last-command debugger-outer-last-command)
      (setq this-command debugger-outer-this-command)
      (setq unread-command-event debugger-outer-unread-command-event)
      (setq last-input-event debugger-outer-last-input-event)
      (setq last-input-char debugger-outer-last-input-char)
      (setq last-input-time debugger-outer-last-input-time)
      (setq last-command-event debugger-outer-last-command-event)
      (setq last-command-char debugger-outer-last-command-char)
      (setq standard-input debugger-outer-standard-input)
      (setq standard-output debugger-outer-standard-output)
      (setq cursor-in-echo-area debugger-outer-cursor-in-echo-area)
      (setq debug-on-next-call debugger-step-after-exit) ;do this last!
      )))


(defun debugger-exit ()
  (condition-case nil
      (let ((debug-on-error nil))
        ;; Tell signal to keep searching for handlers
        (throw 'debugger t))
    ;; Called from an old version of Emacs, perhaps?
    (no-catch (exit-recursive-edit))))


(defun debugger-step-through ()
  "Proceed, stepping through subexpressions of this expression.
Enter another debugger on next entry to eval, apply or funcall."
  (interactive)
  (setq debugger-step-after-exit t)
  (message (gettext "Proceeding, will debug on next eval or call."))
  (debugger-exit))

(defun debugger-continue ()
  "Continue, evaluating this expression without stopping."
  (interactive)
  (message (gettext "Continuing."))
  (debugger-exit))

(defun debugger-return-value (val)
  "Continue, specifying value to return.
This is only useful when the value returned from the debugger
will be used, such as in a debug on exit from a frame."
  (interactive "XReturn value (evaluated): ")
  (setq debugger-value val)
  (princ (gettext "Returning ") t)
  (prin1 debugger-value)
  (exit-recursive-edit))

;; Chosen empirically to account for all the frames
;; that will exist when debugger-frame is called
;; within the first one that appears in the backtrace buffer.
;; Assumes debugger-frame is called from a key;
;; will be wrong if it is called with Meta-x.
(defconst debugger-frame-offset 8 "")

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
  (message (gettext "Continuing through this frame"))
  (debugger-exit))

(defun debugger-reenable ()
  "Turn all debug-on-entry functions back on."
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
		     (gettext "Signal"))
	      (equal (buffer-substring (point) (+ (point) 6))
		     (gettext "Return")))
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

(defun debugger-eval-expression (debugger-exp)
  (interactive "xEval: ")
  (save-excursion
    (if (null (buffer-name debugger-old-buffer))
	;; old buffer deleted
	(setq debugger-old-buffer (current-buffer)))
    (set-buffer debugger-old-buffer)
    (let ((last-command debugger-outer-last-command)
          (this-command debugger-outer-this-command)
          (unread-command-event debugger-outer-unread-command-event)
          (last-input-event debugger-outer-last-input-event)
          (last-input-char debugger-outer-last-input-char)
          (last-input-time debugger-outer-last-input-time)
          (last-command-event debugger-outer-last-command-event)
          (last-command-char debugger-outer-last-command-char)
          (standard-input debugger-outer-standard-input)
          (standard-output debugger-outer-standard-output)
          (cursor-in-echo-area debugger-outer-cursor-in-echo-area))
      (prog1 (eval-expression debugger-exp)
        (setq debugger-outer-last-command last-command
              debugger-outer-this-command this-command
              debugger-outer-unread-command-event unread-command-event
              debugger-outer-last-input-event last-input-event
              debugger-outer-last-input-char last-input-char
              debugger-outer-last-input-time last-input-time
              debugger-outer-last-command-event last-command-event
              debugger-outer-last-command-char last-command-char
              debugger-outer-standard-input standard-input
              debugger-outer-standard-output standard-output
              debugger-outer-cursor-in-echo-area cursor-in-echo-area)))))

(defvar debugger-mode-map
  (let ((map (make-keymap)))
    (set-keymap-name map 'debugger-mode-map)
    (suppress-keymap map)
    (define-key map "-" 'negative-argument)
    (define-key map "b" 'debugger-frame)
    (define-key map "c" 'debugger-continue)
    (define-key map "j" 'debugger-jump)
    (define-key map "r" 'debugger-return-value)
    (define-key map "u" 'debugger-frame-clear)
    (define-key map "d" 'debugger-step-through)
    (define-key map "l" 'debugger-list-functions)
    (define-key map "h" 'describe-mode)
    (define-key map "q" 'top-level)
    (define-key map "e" 'debugger-eval-expression)
    (define-key map " " 'next-line)
    map))

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
  (setq mode-name (gettext "Debugger"))
  (setq truncate-lines t)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (use-local-map debugger-mode-map))

;;;###autoload
(defun debug-on-entry (function)
  "Request FUNCTION to invoke debugger each time it is called.
If you tell the debugger to continue, FUNCTION's execution proceeds.
This works by modifying the definition of FUNCTION,
which must be written in Lisp, not predefined.
Use \\[cancel-debug-on-entry] to cancel the effect of this command.
Redefining FUNCTION also cancels it."
  (interactive "aDebug on entry (to function): ")
  (debugger-reenable)
  (if (subrp (symbol-function function))
      (error (gettext "Function %s is a primitive") function))
  (or (consp (symbol-function function))
      (debug-convert-byte-code function))
  (or (consp (symbol-function function))
      (error (gettext "Definition of %s is not a list") function))
  (fset function (debug-on-entry-1 function (symbol-function function) t))
  (or (memq function debug-function-list)
      (setq debug-function-list (cons function debug-function-list)))
  function)

;;;###autoload
(defun cancel-debug-on-entry (&optional function)
  "Undo effect of \\[debug-on-entry] on FUNCTION.
If argument is nil or an empty string, cancel for all functions."
  (interactive
   (list (let ((name
                (completing-read (gettext "Cancel debug on entry (to function): ")
                                 ;; Make an "alist" of the functions
				 ;; that now have debug on entry.
				 (mapcar 'list (mapcar 'symbol-name
                                                       debug-function-list))
				 nil t nil)))
	   (if name (intern name)))))
  (debugger-reenable)
  (if (and function (not (string= function "")))
      (progn
	(fset function
	      (debug-on-entry-1 function (symbol-function function) nil))
	(setq debug-function-list (delq function debug-function-list))
	function)
    (message (gettext "Cancelling debug-on-entry for all functions"))
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
      (error (gettext "%s is a built-in function") function)
    (if (eq (car defn) 'macro)
	(debug-on-entry-1 function (cdr defn) flag)
      (or (eq (car defn) 'lambda)
	  (error (gettext "%s not user-defined Lisp function") function))
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
  (with-output-to-temp-buffer (gettext "*Help*")
    (if (null debug-function-list)
	(princ (gettext "No debug-on-entry functions now\n"))
      (princ (gettext "Functions set to debug on entry:\n\n"))
      (let ((list debug-function-list))
	(while list
	  (prin1 (car list))
	  (terpri)
	  (setq list (cdr list))))
      (princ (gettext "Note: if you have redefined a function, then it may no longer\n"))
      (princ (gettext "be set to debug on entry, even if it is in the list.")))))

;;; debug.el ends here
