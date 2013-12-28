;;; -*- mode: LISP; package: LISP -*-

;;;
;;; Todd Kaufmann    May 1990
;;;
;;; Make CMU CL run better within GNU inferior-lisp (by ccm).
;;;
;;; This program is freely distributable under
;;;  the terms of the GNU Public license.


(in-package "USER")

;;;% CMU CL does not define defun as a macro
(defun ilisp-compile (form package filename)
  "Compile FORM in PACKAGE recording FILENAME as the source file."
  (ilisp-errors
   ;; This makes sure that function forms are compiled
   (labels ((compiler (form env)
	      (if (consp form)
		  (if (and (eq (first form) 'function)
			   (consp (second form)))
		      (evalhook `(compile nil ,form) nil nil env)
		      (if (eq (first form) 'defun)
			  (prog1
			      (evalhook form nil nil env)
			    (compile (second form)))
			  (evalhook form #'compiler nil env)))
		  (evalhook form #'compiler nil env))))
     (let ((*evalhook* #'compiler))
       (ilisp-eval form package filename)))))

;;;% Stream settings, when running connected to pipes.
;;;
;;; This fixes a problem when running piped: When CMU is running as a piped
;;; process, *terminal-io* really is a terminal; ie, /dev/tty.  This means an
;;; error will cause lisp to stop and wait for input from /dev/tty, which it
;;; won't be able to grab, and you'll have to restart your lisp.  But we want
;;; it to use the same input that the user is typing in, ie, the pipe (stdin).
;;; This fixes that problem, which only occurs in the CMU cores of this year.
;;;

(defvar *Fix-pipe-streams* T
  "Set to Nil if you want them left alone.  And tell me you don't get stuck.")

(when (and *Fix-pipe-streams*
	   (lisp::synonym-stream-p *terminal-io*)
	   (eq (lisp::synonym-stream-symbol *terminal-io*)
	       'SYSTEM::*TTY*))
  (setf *terminal-io* (make-two-way-stream system::*stdin* system::*stdout*))
  ;; *query-io* and *debug-io* are synonym streams to this, so this fixes
  ;; everything.
  )

;;;% Debugger extensions

;;;%% Implementation of a :pop command for CMU CL debugger

;;;
;;; Normally, errors which occur while in the debugger are just ignored, unless
;;; the user issues the "flush" command, which toggles this behavior.
;;;
(setq debug:*flush-debug-errors* nil)  ;; allow multiple error levels.

;;; This implementation of "POP" simply looks for the first restart that says
;;; "Return to debug level n" or "Return to top level." and executes it.
;;;
(debug::def-debug-command "POP"
    ;; find the first "Return to ..." restart
    (if (not (boundp 'debug::*debug-restarts*))
	(error "You're not in the debugger; how can you call this!?")
	(labels ((find-return-to (restart-list num)
		 (let ((first
			(member-if
			 #'(lambda (restart)
			     (string= (funcall
				       (conditions::restart-report-function restart)
				       nil)
				      "Return to " :end1 10))
			  restart-list)))
		   (cond ((zerop num) (car first))
			 ((cdr first) (find-return-to (cdr first) (1- num)))))))
	(let* ((level (debug::read-if-available 1))
	       (first-return-to (find-return-to 
				 debug::*debug-restarts* (1- level))))
	  (if (null first-return-to)
	      (format *debug-io* "pop: ~d is too far" level)
	      (debug::invoke-restart-interactively first-return-to)
	      ))))
    )

;;;% Extensions to describe.

(in-package "LISP")

;;; Put these in the EXT package, but to define them we need access to
;;; symbols in lisp's guts. 

(import '(arglist source-file) (find-package "EXTENSIONS"))
(export '(arglist source-file) (find-package "EXTENSIONS"))


;;;%% ext:arglist - return arglist of function

(defun arglist (symbol package)
  (user:ilisp-errors
   (let* ((x (user:ilisp-find-symbol symbol package))
	  (fun (symbol-function x)))
     (values
      (read-from-string
       (cond ((compiled-function-p fun)
	      (%primitive header-ref fun %function-arg-names-slot)
	      )
	     ((desc-lambdap fun)	; (lambda (arglist) ..)  form
	      (cadr fun))

	     ;; this never happens.
	     ;;((eq (car fun) '%compiled-closure%)
	     ;;(describe-function-compiled (third x)))

	     ((desc-lexical-closure-p fun)
	      (cadadr fun))
	     (t (error "Unknown type of function"))))))))

;;;%% ext:source-file
;;;
;;; For compiled functions only, since the compiler adds this information.

(defun source-file (symbol package type)
  (declare (ignore type))
  (user:ilisp-errors
   (let ((fun (user:ilisp-find-symbol symbol package)))
     (and (fboundp fun)
	  (compiled-function-p (symbol-function fun))
	  (let* ((compiler-string
		  (%primitive header-ref (symbol-function fun)
			      %function-defined-from-slot))
		 (def-string
		     (subseq
		      compiler-string 0 (position #\space compiler-string))))
	    (if (string= def-string "Lisp") nil
		(progn (print def-string)
		       t)
		))))))

