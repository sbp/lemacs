;;;
;;; Lucid initializations 
;;; Author: Chris McConnell, ccm@cs.cmu.edu
;;;
(in-package 'user)

;;;
(defun ilisp-callers (symbol package &aux (list-of-callers nil))
  "Print the callers of PACKAGE::SYMBOL.  Only compiled functions
currently.  Return T if successful."
  (ilisp-errors
   (let ((function-name (ilisp-find-symbol symbol package))
	 (*print-level* nil)
	 (*print-length* nil)
	 (*package* (find-package 'lisp)))
     (when (and function-name (fboundp function-name))
       (flet
	   ((check-symbol (symbol)
	      (labels
		  ((check-function (function &optional exclusions)
		     (do ((i 4 (1+ i)))
			 ((>= i (lucid::procedure-length function)))
		       (let ((element (lucid::procedure-ref function i)))
			 (cond ((eq element function-name)
				(pushnew symbol list-of-callers))
			       ((and (compiled-function-p element)
				     (not (find element exclusions)))
				(check-function
				 element
				 (cons element exclusions))))))))
		(check-function (symbol-function symbol)))))
	 (do-all-symbols (symbol)
	   (when (fboundp symbol)
	     (check-symbol symbol)))
	 (dolist (caller list-of-callers)
	   (print caller))
	 t)))))

;;;
(defun ilisp-source-files (symbol package type)
  "Print each file for PACKAGE:SYMBOL's TYPE definition on a line and
return T if successful."
  (ilisp-errors
   (let* ((symbol (ilisp-find-symbol symbol package))
	  (all (equal type "any"))
	  (type (unless all (ilisp-find-symbol type package)))
	  (paths (when symbol
		   (lucid::get-source-file symbol type all))))
     (if paths
	 (progn
	   (if all
	       (dolist (file (remove-duplicates paths
						:key #'cdr :test #'equal))
		 (print (namestring (cdr file))))
	       (print (namestring paths)))
	   t)
	 nil))))

;;;
(dolist (symbol '(ilisp-callers ilisp-source-files))
  (export symbol)
  (unless (compiled-function-p #'ilisp-callers)
    (compile symbol)))