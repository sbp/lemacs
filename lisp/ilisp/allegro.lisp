;;;
;;; Allegro initializations
;;; Author: Chris McConnell, ccm@cs.cmu.edu
;;;
(in-package "ILISP")

;;;
(defun ilisp-callers (symbol package)
  "Print a list of all of the functions that call FUNCTION and return
T if successful." 
  (ilisp-errors
   (let ((function (ilisp-find-symbol symbol package))
	 (callers nil)
	 (*print-level* nil)
	 (*print-length* nil)
	 (*package* (find-package 'lisp)))
     (when (and function (fboundp function))
       (labels ((in-expression (function expression)
		  (cond ((null expression) nil)
			((listp expression)
			 (let ((header (first expression)))
			   (if (or (eq header function)
				   (and (eq header 'function)
					(eq (second expression) function)))
			       t
			       (dolist (subexp expression)
				 (when (in-expression function subexp)
				   (return t)))))))))
	 (excl::who-references
	  function
	  #'(lambda (function)
	      (push (excl::fn_symdef function) callers)))
	 (do-all-symbols (symbol)
	   (when (and (fboundp symbol)
		      (not (compiled-function-p (symbol-function symbol)))
		      (in-expression function (symbol-function symbol)))
	     (push symbol callers)))
	 (dolist (caller callers)
	   (print caller))
	 t)))))

;;;
(defun ilisp-source-files (symbol package type)
  "Print each file for PACKAGE:SYMBOL's TYPE definition on a line and
return T if successful."
  (ilisp-errors
   (let* ((symbol (ilisp-find-symbol symbol package))
	  (type (if (equal type "any") t (ilisp-find-symbol type "keyword")))
	  (paths (when symbol (excl:source-file symbol type))))
     (if paths
	 (progn
	   (if (eq type t)
	       (dolist (path (remove-duplicates paths
						:key #'cdr :test #'equal))
		 (print (namestring (cdr path))))
	       (print (namestring paths)))
	   t)
	 nil))))

;;;
(dolist (symbol '(ilisp-callers ilisp-source-files))
  (export symbol))
(unless (compiled-function-p #'ilisp-callers)
  (format t "\"ILISP: File is not compiled, use M-x ilisp-compile-inits\""))

