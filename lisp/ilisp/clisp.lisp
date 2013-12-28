;;;
;;; Common Lisp initializations 
;;; Author: Chris McConnell, ccm@cs.cmu.edu
;;;
;;; ange-ftp hack added by ivan Wed Mar 10 12:30:15 1993
;;; ilisp-errors *gc-verbose* addition ivan Tue Mar 16 03:21:51 1993
;;;
;;; Rcs_Info: clisp.lisp,v 1.26 1993/09/03 02:05:07 ivan Rel $
;;;
;;; Rcs_Info: clisp.lisp,v $
;;; Revision 1.26  1993/09/03  02:05:07  ivan
;;; "Deposit for Release 5.5"
;;;
;;; Revision 1.25  1993/08/31  09:45:22  ivan
;;; "Deposit for Release 5.5"
;;;
;;; Revision 1.24  1993/08/31  09:07:17  ivan
;;; "Deposit for Release 5.5"
;;;
;;; Revision 1.23  1993/08/31  01:50:03  ivan
;;; "Deposit for Release 5.5"
;;;
;;; Revision 1.22  1993/08/31  01:37:44  ivan
;;; "Deposit for Release 5.5"
;;;
;;; Revision 1.21  1993/08/29  09:35:48  ivan
;;; "Deposit for Release 5.5"
;;;
;;; Revision 1.20  1993/08/25  20:37:50  ivan
;;; "Deposit for release ILISP_FIVE_O_FOUR"
;;;
;;; Revision 1.19  1993/08/24  22:01:52  ivan
;;; Use defpackage instead of just IN-PACKAGE.
;;; Renamed FUNCTION to FUN in ilisp-arglist to get around CMUCL 17b bug.
;;;
;;; Revision 1.18  1993/06/29  06:13:12  ivan
;;; "Deposit for release ILISP_FIVE_O_THREE"
;;;
;;; Revision 1.17  1993/06/29  06:12:03  ivan
;;; "Deposit for release ILISP_FIVE_O_2"
;;;
;;; Revision 1.16  1993/06/29  05:51:35  ivan
;;; Added Ed Gamble's #'readtable-case fix and Hans Chalupsky's
;;; allegro-4.1 addition.
;;;
;;; Revision 1.15  1993/06/28  20:30:00  ivan
;;; "Deposit for release ILISP_FIVE_O_2"
;;;
;;; Revision 1.14  1993/06/28  19:35:05  ivan
;;; "Deposit for release ILISP_FIVE_O_2"
;;;
;;; Revision 1.13  1993/06/28  16:44:02  ivan
;;; "Deposit for release ILISP_FIVE_O_1"
;;;
;;; Revision 1.12  1993/06/28  03:26:07  ivan
;;; "Deposit for release ILISP_FIVE_O"
;;;
;;; Revision 1.11  1993/06/28  03:12:44  ivan
;;; "Deposit for release ILISP_FIVE_O"
;;;
;;; Revision 1.10  1993/06/28  01:00:24  ivan
;;; "Deposit for release ILISP_FIVE_O"
;;;
;;; Revision 1.9  1993/06/28  00:59:02  ivan
;;; Deposit
;;;
;;; Revision 1.8  1993/06/28  00:57:42  ivan
;;; Stopped using 'COMPILED-FUNCTION-P for compiled check.
;;;
;;; Revision 1.7  1993/06/28  00:41:52  ivan
;;; Deposit for release ILISP_FIVE_O
;;;
;;; Revision 1.6  1993/06/28  00:39:28  ivan
;;; Deposit for release
;;;
;;; Revision 1.5  1993/06/28  00:36:20  ivan
;;; Deposit for release
;;;
;;; Revision 1.4  1993/06/11  19:03:47  ivan
;;; *** empty log message ***
;;;
;;; Revision 1.3  1993/03/16  23:22:10  ivan
;;; Added breakp arg to ilisp-trace.
;;;
;;; Revision 1.2  1993/03/16  09:45:56  ivan
;;; *** empty log message ***
;;;
;;; Revision 1.1  1993/03/16  08:21:17  ivan
;;; Initial revision
;;;
;;;


#+(or allegro-v4.0 allegro-v4.1)
(eval-when (compile load eval)
  (setq excl:*cltl1-in-package-compatibility-p* t))

(defpackage "ILISP" (:use "LISP"))
(in-package "ILISP")
;;;
(defvar *ilisp-old-result* nil "Used for save/restore of top level values.")

;;;
(defmacro ilisp-handler-case (expression &rest handlers)
  "Evaluate EXPRESSION using HANDLERS to handle errors."
  handlers
  (if (macro-function 'handler-case)
      `(handler-case ,expression ,@handlers)
      #+allegro `(excl::handler-case ,expression ,@handlers)
      #+lucid `(lucid::handler-case ,expression ,@handlers)
      #-(or allegro lucid) expression))

;;;
(defun ilisp-readtable-case (readtable)
  (if (fboundp 'readtable-case)
      (funcall #'readtable-case readtable)
      #+allegro (case excl:*current-case-mode*
		  (:case-insensitive-upper :upcase)
		  (:case-insensitive-lower :downcase)
		  (otherwise :preserve))
      #-allegro :upcase))

;;;
(defmacro ilisp-errors (form)
  "Handle errors when evaluating FORM."
  `(let ((*standard-output* *terminal-io*)
	 (*error-output* *terminal-io*)
	 #+cmu
	 (ext:*gc-verbose* nil) ; cmulisp outputs "[GC ...]" which
				; doesn't read well...
	 )
     (princ " ")			;Make sure we have output
     (ilisp-handler-case
      ,form	
      (error (error)
       (with-output-to-string (string)
	 (format string "ILISP: ~A" error))))))


;;;
(defun ilisp-save ()
  "Save the current state of the result history."
  (declare (special / // /// + ++ +++))
  (unless *ilisp-old-result*
    (setq *ilisp-old-result* (list /// // +++ ++ + /))))

;;;
(defun ilisp-restore ()
  "Restore the old result history."
  (declare (special / // /// + ++ +++ * ** -))
  (setq // (pop *ilisp-old-result*)
	** (first //)
	/  (pop *ilisp-old-result*)
	*  (first /)
	++  (pop *ilisp-old-result*)
	+   (pop *ilisp-old-result*)
	-   (pop *ilisp-old-result*))
  (values-list (pop *ilisp-old-result*)))
  
;;;
(defun ilisp-symbol-name (symbol-name)
  "Return SYMBOL-NAME with the appropriate case as a symbol."
  (case (ilisp-readtable-case *readtable*)
    (:upcase (string-upcase symbol-name))
    (:downcase (string-downcase symbol-name))
    (:preserve symbol-name)))
  
;;;
(defun ilisp-find-package (package-name)
  "Return package PACKAGE-NAME or the current package."
  (if (string-equal package-name "nil")
      *package*
      (or (find-package (ilisp-symbol-name package-name))
	  (error "Package ~A not found" package-name))))

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
  ;; Ivan's hack for getting away with dumb /ivan@bu-conx:/foo/bar/baz
  ;; filenames...
  (let* ((at-location (position #\@ filename))
	 (colon-location (position #\: filename))
	 (filename
	  (if (and at-location colon-location)
	      (subseq filename (1+ colon-location))
	      filename))
	 (*package* (ilisp-find-package package))
	 #+allegro (excl::*source-pathname* filename)
	 #+allegro (excl::*redefinition-warnings* nil)
	 #+lucid (lucid::*source-pathname*
		  (if (probe-file filename)
		      (truename filename)
		      (merge-pathnames filename)))
	 #+lucid (lucid::*redefinition-action* nil)
	 )
    filename
    (eval (read-from-string form))))

;;;
(defun ilisp-compile (form package filename)
  "Compile FORM in PACKAGE recording FILENAME as the source file."
  (princ " ")
  ;; This makes sure that function forms are compiled
  #-lucid
  (ilisp-eval
    (format nil "(funcall (compile nil '(lisp:lambda () ~A)))"
	    form)
    package
    filename)
  #+lucid
  (labels ((compiler (form env)
	     (if (and (consp form)
		      (eq (first form) 'function)
		      (consp (second form)))
		 #-LCL3.0
		 (evalhook `(compile nil ,form) nil nil env)
		 #+LCL3.0
		 ;; If we have just compiled a named-lambda, and the
		 ;; name didn't make it in to the procedure object,
		 ;; then stuff the appropriate symbol in to the
		 ;; procedure object.
		 (let* ((proc (evalhook `(compile nil ,form) nil nil env))
			(old-name (and proc (sys:procedure-ref proc 1)))
			(lambda (second form))
			(name (and (eq (first lambda) 'lucid::named-lambda)
				   (second lambda))))
		   (when (or (null old-name)
			     (and (listp old-name) (eq :internal (car old-name))))
		     (setf (sys:procedure-ref proc 1) name))
		   proc)
		 (evalhook form #'compiler nil env))))
	  (let ((*evalhook* #'compiler))
	    (ilisp-eval form package filename))))

;;;
(defun ilisp-describe (sexp package)
  "Describe SEXP in PACKAGE."
  (ilisp-errors
   (let ((*package* (ilisp-find-package package)))
     (describe (eval (read-from-string sexp))))))

;;;
(defun ilisp-inspect (sexp package)
  "Inspect SEXP in PACKAGE."
  (ilisp-errors
   (let ((*package* (ilisp-find-package package)))
     (inspect (eval (read-from-string sexp))))))

;;;
(defun ilisp-arglist (symbol package)
  "Return the argument list of SYMBOL from PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package))
	 (*print-length* nil)
	 (*print-level* nil)
	 (*package* (ilisp-find-package package)))
     (if (and real-symbol (fboundp real-symbol))
	 (pprint (let* ((fun (symbol-function real-symbol))
			(generic-p
			 (find-symbol "GENERIC-FUNCTION-P"
				      (or (find-package "PCL")
					  *package*))))
		   (if (and (fboundp generic-p) (funcall generic-p fun))
		       (funcall
			(find-symbol "GENERIC-FUNCTION-PRETTY-ARGLIST"
				     (or (find-package "PCL") *package*))
			fun)
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
	     (funcall
	      (find-symbol "FIND-METHOD" (or (find-package "CLOS")
					     (find-package "PCL")
					     *package*))
	      (symbol-function real-symbol)
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
			 (cons (funcall
				(find-symbol "FIND-CLASS"
					     (or (find-package "CLOS")
						 (find-package "PCL")
						 *package*))
				class) types))))))))
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
     (pprint (#-allegro macroexpand #+allegro excl::walk
			(read-from-string expression))))))

;;;
(defun ilisp-macroexpand-1 (expression package)
  "Macroexpand EXPRESSION once."
  (ilisp-errors
   (let ((*print-length* nil)
	 (*print-level* nil)
	 (*package* (ilisp-find-package package)))
     (pprint (macroexpand-1 (read-from-string expression))))))

;;;
(defun ilisp-trace (symbol package breakp)
  "Trace SYMBOL in PACKAGE."
  (declare (ignore breakp)) ; No way to do this in CL.
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
(defun ilisp-words (string)
  "Return STRING broken up into words.  Each word is (start end
delimiter)."
  (do* ((length (length string))
	(start 0)
	(end t)
	(words nil))
       ((null end) (nreverse words))
    (if (setq end (position-if-not #'alphanumericp string :start start))
	(setq words (cons (list end (1+ end) t)
			  (if (= start end)
			      words
			      (cons (list start end nil) words)))
	      start (1+ end))
	(setq words (cons (list start length nil) words)))))

;;;
(defun ilisp-match-words (string pattern words)
  "Match STRING to PATTERN using WORDS."
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
	      (when (and (<= end2 strlen)
			 (string= pattern string
				  :start1 start1 :end1 end1
				  :start2 start2 :end2 end2))
		(1- end2))))
    (when start2 (incf start2))))

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
	  (no-casify (eq (ilisp-readtable-case *readtable*) :preserve))
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
	  ;; Check SYMBOL against PATTERN using WORDS 
	  (check-symbol2 (symbol pattern words)
	    (let ((name (symbol-name symbol)))
	      (when (and (or (not function-p) (fboundp symbol))
			 (ilisp-match-words name pattern words))
		(push (list (if no-casify
				name
				(ilisp-casify pattern name lower-p upper-p)))
		      results)))))
       (if external-p
	   (do-external-symbols (symbol *package*)
	     (check-symbol symbol symbol-string))
	   (progn
	     ;; KCL does not go over used symbols.
	     #+(or kcl ibcl)
	     (dolist (used-package (package-use-list *package*))
	       (do-external-symbols (symbol used-package)
		 (check-symbol symbol symbol-string)))
	     (do-symbols (symbol *package*)
	       (check-symbol symbol symbol-string))))
       (unless (or results prefix-p)
	 (let ((words (ilisp-words symbol-string)))
	   (if external-p
	       (do-external-symbols (symbol *package*)
		 (check-symbol2 symbol symbol-string words))
	       (progn
		 ;; KCL does not go over used symbols.
		 #+(or kcl ibcl)
		 (dolist (used-package (package-use-list *package*))
		   (do-external-symbols (symbol used-package)
		     (check-symbol2 symbol symbol-string words)))
		 (do-symbols (symbol *package*)
		   (check-symbol2 symbol symbol-string words))))))
       (prin1 results)
       nil))))

;;; Make sure that functions are exported
(dolist (symbol '(ilisp-errors ilisp-save ilisp-restore
		  ilisp-symbol-name ilisp-find-symbol ilisp-find-package
		  ilisp-eval ilisp-compile
		  ilisp-describe ilisp-inspect
		  ilisp-arglist ilisp-documentation
		  ilisp-macroexpand ilisp-macroexpand-1
		  ilisp-trace ilisp-untrace
		  ilisp-compile-file ilisp-casify
		  ilisp-matching-symbols))
  (export symbol))
(when
    #+cmu (eval:interpreted-function-p #'ilisp-matching-symbols)
    #-cmu (not (compiled-function-p #'ilisp-matching-symbols))
    (format t "\"ILISP: File is not compiled, use M-x ilisp-compile-inits\""))
