;;; -*- mode: LISP; package: LISP -*-

;;;
;;; Todd Kaufmann    May 1990
;;;
;;; Make CMU CL run better within GNU inferior-lisp (by ccm).
;;;
;;; This program is freely distributable under
;;;  the terms of the GNU Public license.
;;;
;;; Rcs_Info: cmulisp.lisp,v 1.29 1993/09/03 02:05:07 ivan Rel $
;;;
;;;


(in-package "ILISP")

;;;% CMU CL does not define defun as a macro
(defun ilisp-compile (form package filename)
  "Compile FORM in PACKAGE recording FILENAME as the source file."
  (ilisp-errors
   (ilisp-eval
    (format nil "(funcall (compile nil '(lambda () ~A)))" form)
    package filename)))

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
(debug::def-debug-command "POP" #+:new-compiler ()
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


;;;%% arglist/source-file utils.

(defun get-correct-fn-object (sym)
  "Deduce how to get the \"right\" function object and return it."
  (let ((fun (or (macro-function sym)
		 (and (fboundp sym) (symbol-function sym)))))
    (cond (fun
	   (when (and (= (lisp::get-type fun) #.vm:closure-header-type)
		      (not (eval:interpreted-function-p fun)))
	     (setq fun (lisp::%closure-function fun)))
	   fun)
	  (t
	   (error "Unknown function ~a.  Check package." sym)
	   nil))))



(export '(arglist source-file cmulisp-trace))

;;;%% arglist - return arglist of function

(defun arglist (symbol package)
  (ilisp-errors
   (let* ((x (ilisp-find-symbol symbol package))
	  (fun (get-correct-fn-object x)))
     (values
      (cond ((eval:interpreted-function-p fun) 
	     (eval:interpreted-function-arglist fun))
	    ((= (lisp::get-type fun)
		#.vm:funcallable-instance-header-type) 
	     ;; generic function / method
	     (pcl::generic-function-pretty-arglist fun))
	    ((compiled-function-p fun)
	     (let ((string-or-nil
		    (#+CMU17 lisp::%function-arglist
		     #-CMU17 lisp::%function-header-arglist
		     fun)))
	       (if string-or-nil
		   (read-from-string string-or-nil)
		   "No argument info.")))
	    (t (error "Unknown type of function")))))))

;;;%% source-file

(defun source-file (symbol package type)
  (declare (ignore type))
  (ilisp-errors
   (let* ((x (ilisp-find-symbol symbol package))
	  (fun (get-correct-fn-object x)))
     (when fun
       (cond ((= (lisp::get-type fun)
		 #.vm:funcallable-instance-header-type)
	      ;; A PCL method! Uh boy!
	      (dolist (method (pcl::generic-function-methods fun))
		(print-simple-source-info
		 (lisp::%closure-function (pcl::method-function method))))
	      t)
	     (t (print-simple-source-info fun)))))))


(defun print-simple-source-info (fun)
  (let ((info (#+CMU17 kernel:%code-debug-info
	       #-CMU17 kernel:code-debug-info       
	       (kernel:function-code-header fun))))
    (when info
      (let ((sources (c::compiled-debug-info-source info)))
	(when sources
	  (dolist (source sources)
	    (let ((name (c::debug-source-name source)))
	      (when (eq (c::debug-source-from source) :file)
		(print (namestring name)))))
	  t)))))

(defun cmulisp-trace (symbol package breakp)
  "Trace SYMBOL in PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package)))
     (setq breakp (read-from-string breakp))
     (when real-symbol (eval `(trace ,real-symbol :break ,breakp))))))
