;; edebug-cl-read.el  - Edebug reader macros for use with cl-read.
;; If you use cl-read.el and want to use edebug with any code
;; in a file written with CL syntax, then you need to use this
;; package.

;; To install, add the following to your .emacs file:
;; (add-hook 
;;   'cl-load-hook
;;   (function 
;;    (lambda () 
;;     (add-hook 'edebug-setup-hook 
;;	       (function (lambda () (load-library "edebug-cl-read")))))))

;; To Do:
;; Handle shared structures, but this is not normally used in executable code.

;; Read-time evaluation shouldn't be used in a form argument since
;; there is no way to instrument the result of the evaluation.  
;; Need to mangle all local variable names that might be visible to
;; eval, e.g. stream, char

(require 'cl)  ;; dg version
(require 'cl-read)

(provide 'edebug-cl-read)

(defconst edebug-readtable (copy-readtable)
  "The modified readtable in use while reading and instrumenting code.")

;; We need to call offset routines before and after processing several
;; macro chars.  So the next two utilities do that given macro char args.
;; Only wrap those macro char handlers that dont need to be replaced.

(defun edebug-wrap-macro-handler (char)
  ;; Assumes char already handled by function.
  (let ((func (get-macro-character char)))
    (set-macro-character 
     char 
     (byte-compile
      (` (lambda (stream char)
	   (edebug-storing-offsets (1- (point))
	     (funcall (function (, func)) stream char)))))
     edebug-readtable)))

;; Not used, but it could be.
'(defun edebug-wrap-dispatch-macro-handler (disp-char sub-char)
  ;; Assumes chars already handled by function
  (let ((func (get-dispatch-macro-character disp-char sub-char)))
    (set-dispatch-macro-character 
     disp-char sub-char
     (byte-compile
      (` (lambda (stream char n)
	   (edebug-storing-offsets
	       ;; good up to 999
	       (- (point) 2 (if (> n 9) (if (> n 99) 2 1) 0))
	     (funcall (function (, func)) stream char n)))))
     edebug-readtable)))

;; Install the changes to the edebug-readtable now.
(progn
  (edebug-wrap-macro-handler ?\?)
  (edebug-wrap-macro-handler ?\")
  (edebug-wrap-macro-handler ?\[)
  )

;;To recopy from *readtable*
;;(set-syntax-from-character ?\' ?\' edebug-readtable *readtable*)

;;============================================================
;; The rest are replacements for the handlers in cl-read.

;; To read symbols and numbers (constituents), save the internal
;; constituent reader function, define a new one which will be used only
;; while reading for instrumenting.
(if (not (fboundp 'edebug-reader:read-constituent))
    (fset 'edebug-reader:read-constituent
	  (symbol-function 'reader:read-constituent)))

(defun edebug-read-constituent (stream)
  ;; Store point before and after reading constituent.
  (edebug-storing-offsets (point)
    (edebug-reader:read-constituent stream)))


(defvar edebug-read-context)
(defvar edebug-read-stack)

;; Lists and dotted pairs
;; For \(, we must replace the handler because the behavior is 
;; changed in the middle.

(set-macro-character ?\( 
  (function 
   (lambda (stream char)
     (let (edebug-read-dotted-list)
       (edebug-storing-offsets (1- (point))
	 (catch 'read-list
	   (let ((edebug-read-context 'list) 
		 edebug-read-stack)
	     ;; read list elements up to a `.'
	     (catch 'dotted-pair
	       (while t
		 (push (reader:read-from-buffer stream 't)
		       edebug-read-stack)))
	     ;; In dotted pair. Read one more element
	     (push (reader:read-from-buffer stream 't) edebug-read-stack)
	     ;; signal it to the closing paren
	     (setq edebug-read-context 'dotted-pair)
	     ;; If the dotted form is a list, signal to offset routines.
	     (setq edebug-read-dotted-list (listp (car edebug-read-stack)))
	     ;; Next char *must* be closing paren that throws read-list
	     (reader:read-from-buffer stream 't)
	     ;; otherwise an error is signalled
	     (error "CL read error: illegal dotted pair read syntax")))))))
  edebug-readtable)

;; ?\) and ?\. are almost identical but included for completeness.

(set-macro-character ?\) 
  (function 
   (lambda (stream char)
     (cond ((eq edebug-read-context 'list)
	    (throw 'read-list (nreverse edebug-read-stack)))
	   ((eq edebug-read-context 'dotted-pair)
	    (throw 'read-list (nconc (nreverse (cdr edebug-read-stack)) 
				     (car edebug-read-stack))))
	   (t 
	    (error "CL read error: `)' doesn't end a list")))))
  edebug-readtable)
	
(set-macro-character ?\.
  (function 
   (lambda (stream char)
     (and (eq edebug-read-context 'dotted-pair) 
	  (error "CL read error: no more than one `.' allowed in list"))
     (throw 'dotted-pair nil)))
  edebug-readtable)

;;-----------------------------
;; Quoting and backquoting

(set-macro-character ?\'
  (function
   (lambda (stream char)
     (edebug-storing-offsets (1- (point))
       (list 
	(edebug-storing-offsets (point) 'quote)
	(reader:read-from-buffer stream 't)))))
  edebug-readtable)

(set-macro-character ?\`
  (function
   (lambda (stream char)
     (if (= (following-char) ?\ )
	 (edebug-storing-offsets (point) '\`)
       (edebug-storing-offsets (1- (point))
	 (list
	  (edebug-storing-offsets (point) '\`)
	  (reader:read-from-buffer stream 't))))))
  edebug-readtable)

(set-macro-character ?\,
  (function
   (lambda (stream char)
     (cond ((eq (following-char) ?\ )
	    ;; old syntax
	    (edebug-storing-offsets (point) '\,))
	   ((eq (following-char) ?\@)
	    (forward-char 1)
	    (cond ((eq (following-char) ?\ )
		   (edebug-storing-offsets (point) '\,\@))
		  (t
		   (edebug-storing-offsets (- (point) 2)
		     (list
		      (edebug-storing-offsets (point) '\,\@)
		      (reader:read-from-buffer stream 't))))))
	   (t
	    (edebug-storing-offsets (1- (point))
	      (list 	    
	       (edebug-storing-offsets (point) '\,)
	       (reader:read-from-buffer stream 't)))))))
  edebug-readtable)


(defun edebug-ensure-n=0 (n)
  (or (= n 0) 
      (error "Cl reader error: numeric infix argument not allowed %d" n)))

(set-dispatch-macro-character ?\# ?\'
  (function
   (lambda (stream char n)
     (edebug-ensure-n=0 n)
     (edebug-storing-offsets (- (point) 2)
       (list 
	(edebug-storing-offsets (point) 
	  (if (featurep 'cl)  'function* 'function))
	(reader:read-from-buffer stream 't)))))
  edebug-readtable)

;; Read time evaluation:  #.<form>
;; See comments at top.

(set-dispatch-macro-character ?\# ?\.
  (function 
   (lambda (stream char n)
     (edebug-ensure-n=0 n)
     ;; If this handler is called, assume we are instrumenting,
     ;; so first instrument code to evaluate here.   ** check this out more
     (eval (let ((edebug-all-forms t))
	     (edebug-storing-offsets (point)
	       (edebug-read-and-maybe-wrap-form t))))))
  edebug-readtable)


(defun edebug-read-feature (stream char n flag)
  (edebug-ensure-n=0 n)
  (let ((feature (reader:original-read stream))  ;; assume there is space after
	;; This is not exactly correct without *read-suppress*.
	;; But read goes one too far in emacs 18.
	;; And we can't use edebug-read-sexp because it uses read,
	;; which is just replaced by reader:read.
	(object (reader:read-from-buffer stream 't)))
    (if (eq (featurep feature) flag)
	object
      ;; Ignore it.
      (edebug-ignore-offset)
      (throw 'reader-ignore nil))))

(set-dispatch-macro-character ?\# ?\+
  (function 
   (lambda (stream char n)
     (edebug-read-feature stream char n t)))
  edebug-readtable)

(set-dispatch-macro-character ?\# ?\-
  (function 
   (lambda (stream char n)
     (edebug-read-feature stream char n nil)))
  edebug-readtable)

;;=========================================================================
;; Redefine the edebug-read routine to check whether CL syntax is active.

(defun edebug-read (&optional stream)
  "Read a sexp from STREAM.
STREAM is limited to the current buffer.
Create a parallel offset structure as described in doc for edebug-offsets.

This version, from edebug-cl-read, uses cl-read."
  (unwind-protect
      (if (not cl-read-active)
	  ;; Use the reader for standard Emacs Lisp.
	  (edebug-read1 stream)
    
	;; Use cl-read with edebug-readtable.
	(unwind-protect
	    ;; If *readtable* is buffer-local, this wont work.
	    (let ((*readtable* edebug-readtable))
	      (fset 'reader:read-constituent 'edebug-read-constituent)
	      (read stream);; Uses reader:read.
	      )
	  (fset 'reader:read-constituent 'edebug-reader:read-constituent)
	  ))

    ;; Just make sure it is reset for the next time, even if there is an error.
    (setq edebug-current-offset nil)))

