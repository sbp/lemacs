;;;
;;; Common Lisp initializations 
;;; Edit-History:
;;;
;;; Author: Chris McConnell, ccm@cs.cmu.edu
;;; 30-Oct-91 mm: LCL3.0 special case does not apply to LCL4.0
;;;               send sexp (not value of sexp) to DESCRIBE in LCL4.0
;;;
;;; End-of-Edit-History
;;;
(in-package 'user)

;;;
(defvar *ilisp-old-result* nil)

;;;
(eval-when (load compile eval)
  (unless (find-package 'pcl)
    (make-package 'pcl)))

;;;
(unless (macro-function 'handler-case)
  (defmacro handler-case (expression &rest handlers)
    "Evaluate EXPRESSION using HANDLERS to handle errors."
    handlers
    #+allegro `(excl::handler-case ,expression ,@handlers)
    #+lucid `(lucid::handler-case ,expression ,@handlers)
    #-(or allegro lucid) expression))

;;;
(unless (fboundp 'readtable-case)
  #+allegro (defun readtable-case (readtable)
	      readtable
	      (case *current-case-mode*
		(:case-insensitive-upper :upcase)
		(:case-insensitive-lower :downcase)
		(otherwise :preserve)))
  #-allegro (defun readtable-case (readtable) readtable :upcase))

;;;
(defmacro ilisp-errors (form)
  "Handle errors when evaluating FORM."
  `(progn
     (princ " ")			;Make sure we have output
     (handler-case
      ,form	
      (error (error)
	     error
	     #+allegro (excl::condition-report error t)
	     #+lucid (lucid::condition-report error t)
	     #+cmu (conditions::condition-report error t)
	     nil))))
     
;;;
(defun ilisp-save ()
  "Save the current state of the result history."
  (unless *ilisp-old-result*
    (setq *ilisp-old-result* (list /// // /))))

;;;
(defun ilisp-restore ()
  "Restore the old result history."
  (setq // (pop *ilisp-old-result*)
	** (first //)
	/  (pop *ilisp-old-result*)
	*  (first /))
  (values-list (pop *ilisp-old-result*)))
  
;;;
(defun ilisp-symbol-name (symbol-name)
  "Return SYMBOL-NAME with the appropriate case as a symbol."
  (case (readtable-case *readtable*)
    (:upcase (string-upcase symbol-name))
    (:downcase (string-downcase symbol-name))
    (:preserve symbol-name)))
  
;;;
(defun ilisp-find-package (package-name)
  "Return package PACKAGE-NAME or the current package."
  (if (string-equal package-name "nil")
      *package*
      (or (find-package (ilisp-symbol-name package-name))
	  (error "Package not found"))))

;;;
(defun ilisp-find-symbol (symbol-name package-name)
  "Return the symbol associated with SYMBOL-NAME in PACKAGE-NAME trying to
handle case issues intelligently."
  (find-symbol (ilisp-symbol-name symbol-name)
	       (ilisp-find-package package-name)))

;;;
(defun ilisp-eval (form package filename)
  "Evaluate FORM in PACKAGE recording FILENAME as the source file."
  (princ " ")
  (let* ((*package* (ilisp-find-package package))
	 #+allegro (excl::*source-pathname* filename)
	 #+allegro (excl::*redefinition-warnings* nil)
	 #+lucid (lucid::*source-pathname* (truename filename))
	 )
    filename
    (eval (read-from-string form))))

;;;
(defun ilisp-compile (form package filename)
  "Compile FORM in PACKAGE recording FILENAME as the source file."
  (princ " ")
  ;; This makes sure that function forms are compiled
  (labels ((compiler (form env)
	     (if (and (consp form)
		      (eq (first form) 'function)
		      (consp (second form)))
		 #+(or (not LCL3.0) lcl4.0)
		 (evalhook `(compile nil ,form) nil nil env)
		 #+(and LCL3.0 (not lcl4.0))
		 ;; If we have just compiled a named-lambda, and the
		 ;; name didn't make it in to the procedure object,
		 ;; then stuff the appropriate symbol in to the
		 ;; procedure object.
		 (let* ((proc (evalhook `(compile nil ,form) nil nil env))
			(lambda (second form))
			(name (and (eq (first lambda) 'lucid::named-lambda)
				   (second lambda))))
		   (when (and proc (null (lucid::procedure-name proc)))
		     (setf (lucid::procedure-name proc) name))
		   proc)
		 (evalhook form #'compiler nil env))))
    (let ((*evalhook* #'compiler))
      (ilisp-eval form package filename))))

;;;
(defun ilisp-describe (sexp package)
  "Describe SEXP in PACKAGE."
  (ilisp-errors
   (let ((*package* (ilisp-find-package package)))
     #-LCL4.0(describe (eval (read-from-string sexp)))
     #+LCL4.0(describe (read-from-string sexp))
	)))

;;;
(defun ilisp-arglist (symbol package)
  "Return the argument list of SYMBOL from PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package))
	 (*print-length* nil)
	 (*print-level* nil))
     (if (and real-symbol (fboundp real-symbol))
	 (pprint (let ((function (symbol-function real-symbol)))
		   (if (and (fboundp 'pcl::generic-function-p)
			    (pcl::generic-function-p function))
		       (pcl::generic-function-pretty-arglist function)
		       #+allegro (excl::arglist real-symbol)
		       #+lucid (lucid::arglist real-symbol)
		       #+(or ibcl kcl) (help real-symbol)
		       #-(or allegro lucid kcl ibcl)
		       (documentation real-symbol 'function))))
	 (format t "~A is not a function" symbol)))))

;;;
(defun ilisp-documentation (symbol package type)
  "Return the TYPE documentation for SYMBOL in PACKAGE.  If TYPE is
\(qualifiers* (class ...)), the appropriate method will be found."
  (ilisp-errors
   (let* ((real-symbol (ilisp-find-symbol symbol package))
	  (type (if (and (not (zerop (length type)))
			 (eq (elt type 0) #\())
		    (let ((*package* (ilisp-find-package package)))
		      (read-from-string type))
		    (ilisp-find-symbol type package))))
     (when (listp type)
       (setq real-symbol
	     (find-method (symbol-function real-symbol)
			  (reverse
			   (let ((quals nil))
			     (dolist (entry type quals)
			       (if (listp entry)
				   (return quals)
				   (setq quals (cons entry quals))))))
			  (reverse
			   (let ((types nil))
			     (dolist (class (first (last type)) types)
			       (setq types
				     (cons (find-class class) types))))))))
     (if real-symbol
	 (if (symbolp real-symbol)
	     (documentation real-symbol type)
	     ;; Prevent compiler complaints
	     (eval `(documentation ,real-symbol)))
	 (format nil "~A has no ~A documentation" symbol type)))))

;;;
(defun ilisp-macroexpand (expression package)
  "Macroexpand EXPRESSION as long as the top level function is still a
macro." 
  (ilisp-errors
   (let ((*print-length* nil)
	 (*print-level* nil)
	 (*package* (ilisp-find-package package)))
     (pprint (macroexpand (read-from-string expression))))))

;;;
(defun ilisp-macroexpand-1 (expression package)
  "Macroexpand EXPRESSION once."
  (ilisp-errors
   (let ((*print-length* nil)
	 (*print-level* nil)
	 (*package* (ilisp-find-package package)))
     (pprint (macroexpand-1 (read-from-string expression))))))

;;;
(defun ilisp-trace (symbol package)
  "Trace SYMBOL in PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package)))
     (when real-symbol (eval `(trace ,real-symbol))))))
(defun ilisp-untrace (symbol package)
  "Untrace SYMBOL in PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package)))
     (when real-symbol (eval `(untrace ,real-symbol))))))
   
;;;
(defun ilisp-compile-file (file extension)
  "Compile FILE putting the result in FILE+EXTENSION."
  (ilisp-errors
   (compile-file file
		 :output-file 
		 (merge-pathnames (make-pathname :type extension) file))))

;;;
(defun ilisp-casify (pattern string lower-p upper-p)
  "Return STRING with its characters converted to the case of PATTERN,
continuing with the last case beyond the end."
  (cond (lower-p (string-downcase string))
	(upper-p (string-upcase string))
	(t
	 (let (case)
	   (concatenate
	    'string
	    (map 'string
		 #'(lambda (p s)
		     (setq case (if (upper-case-p p)
				    #'char-upcase
				    #'char-downcase))
		     (funcall case s))
		 pattern string)
	    (map 'string case (subseq string (length pattern))))))))

;;;
(defun ilisp-matching-symbols (string package &optional (function-p nil)
				      (external-p nil)
				      (prefix-p nil))
  "Return a list of the symbols that have STRING as a prefix in
PACKAGE. FUNCTION-P indicates that only symbols with a function value
should be considered.  EXTERNAL-P indicates that only external symbols
should be considered.  PREFIX-P means that partial matches should not
be considered.  The returned strings have the same case as the
original string."
  (ilisp-errors
   (let* ((lower-p (notany #'upper-case-p string))
	  (upper-p (notany #'lower-case-p string))
	  (no-casify (eq (readtable-case *readtable*) :preserve))
	  (symbol-string (ilisp-symbol-name string))
	  (length (length string))
	  (results nil)
	  (*print-length* nil)
	  (*package* (ilisp-find-package package)))
     (labels
	 (
	  ;; Check SYMBOL against PATTERN
	  (check-symbol (symbol pattern)
	    (let ((name (symbol-name symbol)))
	      (when (and (or (not function-p) (fboundp symbol))
			 (>= (length name) length)
			 (string= pattern name :end2 length))
		(push (list (if no-casify
				name
				(ilisp-casify pattern name lower-p upper-p)))
		      results))))
	  ;; Break up STRING by non-alphanumerics
	  (words (string)
	    (do* ((length (length string))
		  (start 0)
		  (end t)
		  (words nil))
		 ((null end) (nreverse words))
	      (if (setq end (position-if-not #'alphanumericp string
					     :start start))
		  (setq words (cons (list end (1+ end) t)
				    (if (= start end)
					words
					(cons (list start end nil) words)))
			start (1+ end))
		  (setq words (cons (list start length nil) words)))))
	  ;; Match STRING to PATTERN using WORDS
	  (match-words (string pattern words)
	    (do* ((strlen (length string))
		  (words words (cdr words))
		  (word (first words) (first words))
		  (start1 (first word) (first word))
		  (end1 (second word) (second word))
		  (delimiter (third word) (third word))
		  (len (- end1 start1) (and word (- end1 start1)))
		  (start2 0)
		  (end2 len))
		 ((or (null word) (null start2)) start2)
	      (setq end2 (+ start2 len)
		    start2
		    (if delimiter
			(position (elt pattern start1) string :start start2)
			(when (and (< end2 strlen)
				   (string= pattern string
					    :start1 start1 :end1 end1
					    :start2 start2 :end2 end2))
			  (1- end2))))
	      (when start2 (incf start2))))
	  ;; Check SYMBOL against PATTERN using WORDS 
	  (check-symbol2 (symbol pattern words)
	    (let ((name (symbol-name symbol)))
	      (when (and (or (not function-p) (fboundp symbol))
			 (match-words name pattern words))
		(push (list (if no-casify
				name
				(ilisp-casify pattern name lower-p upper-p)))
		      results)))))
       (if external-p
	   (do-external-symbols (symbol *package*)
	     (check-symbol symbol symbol-string))
	   ;; KCL does not go over used symbols.
	   #+(or kcl ibcl)
	   (dolist (used-package (package-use-list *package*))
	     (do-external-symbols (symbol used-package)
	       (check-symbol symbol symbol-string)))
	   #-(or kcl ibcl)
	   (do-symbols (symbol *package*)
	     (check-symbol symbol symbol-string)))
       (unless (or results prefix-p)
	 (let ((words (words symbol-string)))
	   (if external-p
	       (do-external-symbols (symbol *package*)
		 (check-symbol2 symbol symbol-string words))
	       ;; KCL does not go over used symbols.
	       #+(or kcl ibcl)
	       (dolist (used-package (package-use-list *package*))
		 (do-external-symbols (symbol used-package)
		   (check-symbol2 symbol symbol-string words)))
	       #-(or kcl ibcl)
	       (do-symbols (symbol *package*)
		 (check-symbol2 symbol symbol-string words)))))
       (prin1 results)
       nil))))

;;; Make sure that functions are compiled
(dolist (symbol '(ilisp-save ilisp-restore
		  ilisp-symbol-name ilisp-find-symbol ilisp-find-package
		  ilisp-eval ilisp-compile
		  ilisp-describe ilisp-arglist ilisp-documentation
		  ilisp-macroexpand ilisp-macroexpand-1
		  ilisp-trace ilisp-untrace
		  ilisp-compile-file ilisp-casify ilisp-matching-symbols))
  (export symbol)
  (unless (compiled-function-p (symbol-function symbol))
    (compile symbol)))
(export 'ilisp-errors)
