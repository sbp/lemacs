;; Basic lisp subroutines for Emacs
;; Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

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



;;;; Lisp language features.

;; I'm sorry, this is just bogus as hell.
;(defmacro lambda (&rest cdr)
;  "Return a lambda expression.
;A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
;self-quoting; the result of evaluating the lambda expression is the
;expression itself.  The lambda expression may then be treated as a
;function, i. e. stored as the function value of a symbol, passed to
;funcall or mapcar, etcetera.
;ARGS should take the same form as an argument list for a `defun'.
;DOCSTRING should be a string, as described for `defun'.  It may be omitted.
;INTERACTIVE should be a call to the function `interactive', which see.
;It may also be omitted.
;BODY should be a list of lisp expressions."
;  ;; Note that this definition should not use backquotes; subr.el should not
;  ;; depend on backquote.el.
;  (list 'function (cons 'lambda cdr)))


;; called by Fkill_buffer()
(defvar kill-buffer-hook nil
  "Function or functions to be called when a buffer is killed.
The value of this variable may be buffer-local.
The buffer about to be killed is current when this hook is run.")

(defvar kill-emacs-hook nil
  "Function or functions to be called when `kill-emacs' is called,
just before emacs is actually killed.")

(defun generate-new-buffer (name)
  "Create and return a buffer with a name based on NAME.
Choose the buffer's name using `generate-new-buffer-name'."
  (get-buffer-create (generate-new-buffer-name name)))


;;;; Window tree functions.

(defun one-window-p (&optional nomini)
  "Returns non-nil if there is only one window.
Optional arg NOMINI non-nil means don't count the minibuffer
even if it is active."
  (let ((base-window (selected-window)))
    (if (and nomini (eq base-window (minibuffer-window)))
	(setq base-window (next-window base-window)))
    (eq base-window
	(next-window base-window (if nomini 'nomini)))))

(defun walk-windows (proc &optional minibuf all-screens)
  "Cycle through all visible windows, calling PROC for each one.
PROC is called with a window as argument.
Optional second arg MINIBUF t means count the minibuffer window
even if not active.  If MINIBUF is neither t nor nil it means
not to count the minibuffer even if it is active.

Optional third arg ALL-SCREENS t means include all windows in all screens;
otherwise cycle within the selected screen."
  ;; Note that, like next-window & previous-window, this behaves a little 
  ;; strangely if the selected window is on an invisible screen: it hits
  ;; some of the windows on that screen, and all windows on visible screens.
  (let* ((walk-windows-history nil)
	 (walk-windows-current (selected-window)))
    (while (progn
	     (setq walk-windows-current
		   (next-window walk-windows-current minibuf all-screens))
	     (not (memq walk-windows-current walk-windows-history)))
      (setq walk-windows-history (cons walk-windows-current
				       walk-windows-history))
      (funcall proc walk-windows-current))))


; old names
(define-function 'make-syntax-table 'copy-syntax-table)
(define-function 'dot 'point)
(define-function 'dot-marker 'point-marker)
(define-function 'dot-min 'point-min)
(define-function 'dot-max 'point-max)
(define-function 'window-dot 'window-point)
(define-function 'set-window-dot 'set-window-point)
(define-function 'read-input 'read-string)
(define-function 'send-string 'process-send-string)
(define-function 'send-region 'process-send-region)
(define-function 'show-buffer 'set-window-buffer)
(define-function 'buffer-flush-undo 'buffer-disable-undo)
(define-function 'eval-current-buffer 'eval-buffer)
(define-function 'byte-code-function-p 'compiled-function-p) ;RMSmacs
(define-function 'truename 'file-truename)

; alternate names
(define-function 'not 'null)
(define-function 'string= 'string-equal)
(define-function 'string< 'string-lessp)
(if (not (fboundp 'mod)) (define-function 'mod '%))
(define-function 'move-marker 'set-marker)
(define-function 'eql 'eq)
(if (not (fboundp 'numberp))
    (define-function 'numberp 'integerp)) ; different when floats
(define-function 'rplaca 'setcar)
(define-function 'rplacd 'setcdr)
(define-function 'beep 'ding) ;preserve lingual purtity
(define-function 'indent-to-column 'indent-to)
(define-function 'backward-delete-char 'delete-backward-char)
(define-function 'search-forward-regexp (symbol-function 're-search-forward))
(define-function 'search-backward-regexp (symbol-function 're-search-backward))
(define-function 'int-to-string 'number-to-string)
(define-function 'string-to-int 'string-to-number)
(define-function 'remove-directory 'delete-directory)

;; FSF19 emits calls to defalias instead of fset in .elc files.
(define-function 'defalias 'define-function)

;; Some programs still use this as a function.
(defun baud-rate ()
  "Obsolete function returning the value of the `baud-rate' variable."
  baud-rate)

;;; This should be only in mlsupport.el, but lots of otherwise-non-Mucklisp
;;; third party code still uses it.
(defun insert-string (&rest args)
  "Mocklisp-compatibility insert function.  Do not use this.
Like the function `insert' except that any argument that is a number
is converted into a string by expressing it in decimal.  (Yuck!!)"
  (while args
    (let ((arg (car args)))
      (if (integerp arg)
          (insert (number-to-string arg))
	(insert arg))
      (setq args (cdr args)))))

;;;; Hook manipulation functions.

(defun run-hooks (&rest hooklist)
  "Takes hook names and runs each one in turn.  Major mode functions use this.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments."
  (while hooklist
    (let ((sym (car hooklist)))
      (and (boundp sym)
	   (symbol-value sym)
	   (let ((value (symbol-value sym)))
	     (if (and (listp value) (not (eq (car value) 'lambda)))
		 (mapcar 'funcall value)
	       (funcall value)))))
    (setq hooklist (cdr hooklist))))

;; Tell C code how to call this function.
(setq run-hooks 'run-hooks)

(defun run-hook-with-args (hook &rest args)
  "Runs hook with the specified arguments.
HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
value, that value may be a function or a list of functions to be
called to run the hook.  If the value is a function, it is called with
the given arguments and its return value is returned.  If it is a
list, the elements are called, in order, with the given arguments,
and a list of the each function's return value is returned."
  (and (boundp hook)
       (symbol-value hook)
       (let ((value (symbol-value hook)))
	 (if (and (listp value) (not (eq (car value) 'lambda)))
	     (mapcar #'(lambda (foo) (apply foo args))
		     value)
	   (apply value args)))))


(defun add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  ;(interactive "SAdd to hook-var (symbol): \naAdd which function to %s? ")
  (if (not (boundp hook)) (set hook nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(setq old (list old)))
    (if (member function old)
	nil
      (set hook (if append
		    (append old (list function)) ; don't nconc
		  (cons function old))))))

(defun remove-hook (hook function)
  "Remove a function from a hook, if it is present.
First argument HOOK (a symbol) is the name of a hook, second
 argument FUNCTION is the function to remove (compared with `eq')."
  (let (val)
    (cond ((not (boundp hook))
	   nil)
	  ((eq function (setq val (symbol-value hook)))
	   (setq hook nil))
	  ((consp val)
	   ;; don't side-effect the list
	   (set hook (delq function (copy-sequence val)))))))

;;; Temporary support for old the interface to extents.

(make-obsolete 'extent-data 'extent-property)
(make-obsolete 'set-extent-data 'set-extent-property)
(make-obsolete 'set-extent-attribute 'set-extent-property)
(make-obsolete 'extent-attributes 'extent-property)
(make-obsolete 'extent-glyph
	       "use extent-begin-glyph or extent-end-glyph instead")

(defun extent-data (extent)
  "Obsolete.  Returns the `data' property of the given extent."
  (extent-property extent 'data))

(defun set-extent-data (extent data)
  "Obsolete.  Sets the `data' property of the given extent."
  (set-extent-property extent 'data data))

(defun set-extent-attribute (extent attr &optional clearp)
  "Obsolete; use set-extent-property instead."
  (cond ((eq attr 'write-protected)
	 (set-extent-property extent 'read-only t))
	((eq attr 'unhighlight)
	 (set-extent-property extent 'highlight nil))
	((eq attr 'writable)
	 (set-extent-property extent 'read-only nil))
	((eq attr 'visible)
	 (set-extent-property extent 'invisible nil))
	(t
	 (set-extent-property extent attr t))))

(defun extent-glyph (extent)
  "Obsolete.  Use extent-begin-glyph or extent-end-glyph instead."
  (or (extent-begin-glyph extent)
      (extent-end-glyph extent)))


;;;; Miscellanea.

(defun ignore (&rest ignore) 
  "Do nothing.
Accept any number of arguments, but ignore them."
  nil)

(defun error (&rest args)
  "Signal an error, making error message by passing all args to `format'."
  (while t
    (signal 'error (list (apply 'format args)))))

(defun user-original-login-name ()
  "Return user's login name from original login.
This tries to remain unaffected by `su', by looking in environment variables."
  (or (getenv "LOGNAME") (getenv "USER") (user-login-name)))

(defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (list 'let '((_match_data_ (match-data)))
	(list 'unwind-protect
	      (cons 'progn body)
	      '(store-match-data _match_data_))))

(defun invocation-directory ()
  "Returns the directory name in which the Emacs executable was located."
  (file-name-directory execution-path))


;;; The real defn is in abbrev.el but some early callers
;;;  (eg lisp-mode-abbrev-table) want this before abbrev.el is loaded...

(if (not (fboundp 'define-abbrev-table))
    (progn
      (setq abbrev-table-name-list '())
      (fset 'define-abbrev-table (function (lambda (name defs)
                                   ;; These are fixed-up when abbrev.el loads.
                                   (setq abbrev-table-name-list
                                         (cons (cons name defs)
                                               abbrev-table-name-list)))))))
