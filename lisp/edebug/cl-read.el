;;   -*- Mode: emacs-lisp -*-
;; 
;; Customizable, CL-like reader for version 19 Emacs Lisp. 
;; 
;; Copyright (C) 1993 by Guido Bosch <Guido.Bosch@loria.fr>

;; This file is written in GNU Emacs Lisp, but not (yet) part of GNU Emacs.

;; The software contained in this file is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License

;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;; 
;; Please send bugs and comments to the author.
;;
;; <DISCLAIMER>
;; This program is still under development.  Neither the author nor
;; his employer accepts responsibility to anyone for the consequences of
;; using it or for whether it serves any particular purpose or works
;; at all.


;; Introduction
;; ------------
;;
;; This package replaces the standard Emacs Lisp reader (implemented
;; in C) by a flexible and customizable Common Lisp like one
;; (implemented entirely in Emacs Lisp). During reading of elisp
;; source files, it is about 50% slower than the built-in reader, but
;; there is no difference for byte compiled files - they dont contain
;; any syntactic sugar and are loaded with the built in subroutine
;; `load'.

;; The user level functions for defining read tables, character and
;; dispatch macros are implemented according to the Commom Lisp
;; specification by Steels (2nd edition), but the macro functions
;; itself are implemented in a slightly different way, because the
;; character reading is done in an Emacs buffer, and not by using the
;; primitive funtions `read-char' and `unread-char', as a real CL
;; does.  To get 100% compatibility with CL, the above functions (or
;; their equivalents) have to be implemented as subroutines. 

;; Another difference to real CL reading is that basic token (symbols
;; numbers, strings, and a few more) are still read by the
;; original elisp reader. This is necessary to get a reasonable
;; performance. As a consquence, the syntax of basic tokens can't be
;; customized.

;; Most of the built-in character syntax has been replaced by lisp
;; character macros: parentheses and brackets, simple and double
;; quotes, semicolon comments and the dot. New syntax features are:

;; Backquote-Comma-Atsign Macro: `(,el ,@list) 
;;
;; (the clumsy elisp syntax (` ((, el) (,@ list))) is also supported,
;; but with one restriction: the blank behind the quote characters is
;; mandatory for the old syntax. The cl reader needs it as a landmark
;; to distinguish between old and new syntax. An example:
;;
;; With blanks, both readers read the same:
;; (` (, (head)) (,@ (tail))) -std-read->  (` (, (head)) (,@ (tail)))
;; (` (, (head)) (,@ (tail))) -cl-read->   (` (, (head)) (,@ (tail)))
;;
;; Without blanks, the form is interpreted differently by the two readers:
;; (`(,(head)) (,@(tail))) -std-read-> (` (, (head)) (,@ (tail)))
;; (`(,(head)) (,@(tail))) -cl-read->  ((` ((, ((head)))) ((,@ ((tail)))))
;;
;; 
;; Dispatch Character Macro" `#'
;;
;; #'<function>			function quoting
;; #\<charcter>			character syntax
;; #.<form>    			read time evaluation
;; #p<path>, #P<path> 		paths
;; #+<feature>, #-<feature> 	conditional reading
;; #<n>=, #<n># 		tags for shared structure reading
;;
;; Other read macros can be added easyly (see the definition of the
;; above ones in this file using the function `set-macro-character')

;; The Cl reader is mostly downward compatile, (exception: backquote
;; comma macro, see above). E.g., this file, which is written entirely
;; in the old Emacs Lisp dialect, can be read and compiled with the
;; cl-reader being activated. 

;; Installation: 
;; -------------
;;
;; The package is built on top of Dave Gillespie's cl.el package
;; (version 2.02 or later).  The old one (from Ceazar Quiroz, still
;; shiped with the Emacs 19 disributions) will not do.
;;
;; To use the cl-read package automatically when reading in a buffer, 
;; it has to be installed using the emacs-lisp-mode-hook:
;;
;; (add-hook 'emacs-lisp-mode-hook 'cl-read-install)
;;
;; (defun cl-read-install ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((case-fold-search t))
;;       (cond ((re-search-forward 
;;               "read-syntax: *common-lisp" 
;;               (save-excursion 
;;                 (end-of-line)
;;                 (point))
;;               t)
;;              (require 'cl-read)
;;              (setq cl-read-active t))))))
;;
;; As most of the Emacs Lisp files are written
;; using the standard syntax, the cl reader is only loaded and
;; activated on elisp files with the "Read-Syntax" property set to
;; "Common-Lisp" (in the property line):
;;
;; -*- Read-Syntax: Common-Lisp -*-
;;
;; Note that both property name ("Read-Syntax") and value
;; ("Common-Lisp") are not case sensitive. There can also be other
;; properties in this line: 
;;
;; -*- Mode: Emacs-Lisp; Read-Syntax: Common-Lisp -*-
;;
;; The `cl-read-install' hook function tests for the presence of the
;; correct Read-Syntax property and loads the cl-read package if
;; necessary. This replaces the follwing standard elisp
;; functions:
;;
;; 	- read
;; 	- read-from-string
;; 	- eval-current-buffer
;; 	- eval-buffer
;; 	- eval-region
;;
;; There may be other built-in functions that need to be replaced
;; (load, e.g).  The behavior of the new reader function depends on
;; the value of the buffer local variable `cl-read-active': if it is
;; nil, they just call the original functions, otherwise they call the
;; cl reader. If the cl reader is active in a buffer, the string "CL"
;; appears behind the mode name in the buffer's mode line.
;; 
;;
;; TO DO List: 
;; -----------
;; - Provide a replacement for load so that uncompiled cl syntax
;;   source file can be loaded, too.   - some have written load in elisp.
;; - Do we really need the (require 'cl) dependency? 
;; - More read macros.
;; - Refine the error signaling mechanism. 


; Change History
; 
; !Log: cl-read.el,v !
; Revision 1.8  1993/08/10  13:43:34  bosch
; Hook function `cl-read-install' for automatic installation added.
; Buffer local variable `cl-read-active' added: together with the above
; hook it allows the file specific activation of the cl reader.
;
; Revision 1.7  1993/08/10  10:35:21  bosch
; Functions `read*' and `read-from-string*' renamed into `reader:read'
; and `reader:read-from-string'. Whitespace character skipping after
; recursive reader calls removed (Emacs 19 should not need this).
; Functions `cl-reader-install'  and `cl-reader-uninstall' updated.
; Introduction text and  function comments added.
;
; Revision 1.6 1993/08/09 15:36:05 bosch Function `read*' now nearly
; elisp compatible (no functions as streams, yet -- I don't think I
; will ever implement this, it would be far too slow).  Elisp
; compatible function `read-from-string*' added.  Replacements for
; `eval-current-buffer', `eval-buffer' and `eval-region' added.
; Renamed feature `cl-dg' in `cl', as Dave Gillespie's cl.el package
; is rather stable now.  Function `cl-reader-install' and
; `cl-reader-uninstall' modified.
;
; Revision 1.5  1993/08/09  10:23:35  bosch
; Functions `copy-readtable' and `set-syntax-from-character' added.
; Variable `reader:internal-standard-readtable' added.  Standard
; readtable initialization modified. Whitespace skipping placed back
; inside the read loop.
;
; Revision 1.4  1993/05/14  13:00:48  bosch
; Included patches from Daniel LaLiberte.
;
; Revision 1.3  1993/05/11  09:57:39  bosch
; `read*' renamed in `reader:read-from-buffer'. `read*' now can read
; from strings.
;
; Revision 1.2  1993/05/09  16:30:50  bosch
; (require 'cl-read) added.
; Calling of `{before,after}-read-hook' modified.
;
; Revision 1.1  1993/03/29  19:37:21  bosch
; Initial revision
;
;

(require 'cl)
(provide 'cl-read)
;; load before compiling
;(require 'cl-read)

(autoload 'compiled-function-p "bytecomp")

(defvar cl-read-active nil
  "Buffer local variable that enables Common Lisp style syntax reading.")
(make-variable-buffer-local 'cl-read-active)
(setq-default cl-read-active nil)

(or (assq 'cl-read-active minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(cl-read-active " CL") minor-mode-alist)))


;; The readtable

(defvar reader:readtable-size 256
  "The size of a readtable."
  ;; Actually, the readtable is a vector of size (1+
  ;; reader:readtable-size), because the last element contains the
  ;; symbol `readtable', used for defining `readtablep.
  )

;; An entry of the readtable must have one of the following forms:
;;
;; 1. A symbol, one of {illegal, constituent, whitespace}.  It means 
;;    the character's reader class.
;;
;; 2. A function (i.e., a symbol with a function definition, a byte
;;    compiled function or an uncompiled lambda expression).  It means the
;;    character is a macro character.
;;
;; 3. A vector of length `reader:readtable-size'. Elements of this vector
;;    may be `nil' or a function (see 2.). It means the charater is a
;;    dispatch character, and the vector its dispatch fucntion table.

(defun* copy-readtable 
    (&optional (from-readtable *readtable*) 
	       (to-readtable 
		(make-vector (1+ reader:readtable-size) 'illegal)))
  "Return a copy of FROM-READTABLE \(default: *readtable*\). If the
FROM-READTABLE argument is provided as `nil', make a copy of a
standard \(CL-like\) readtable. If TO-READTABLE is provided, modify and
return it, otherwise create a new readtable object."

  (if (null from-readtable)
      (setq from-readtable reader:internal-standard-readtable))

  (loop for i to reader:readtable-size
	as from-syntax = (aref from-readtable i)
	do (setf (aref to-readtable i)
		 (if (vectorp from-syntax)
		     (copy-sequence from-syntax)
		   from-syntax))
	finally return to-readtable))


(defmacro reader:get-readtable-entry (char readtable)
  (` (aref (, readtable) (, char))))
   
(defun set-macro-character 
  (char function &optional readtable)
    "Makes CHAR to be a macro character with FUNCTION as handler.
When CHAR is seen by reader:read-from-buffer, it calls FUNCTION.
Returns always t. Optional argument READTABLE is the readtable to set
the macro character in (default: *readtable*)."
  (or readtable (setq readtable *readtable*))
  (or (reader:functionp function) 
      (error "Not valid character macro function: %s" function)) 
  (setf (reader:get-readtable-entry char readtable) function)
  t)


(put 'set-macro-character 'edebug-form-spec 
     '(&define sexp function-form &optional sexp))
(put 'set-macro-character 'lisp-indent-function 1)

(defun get-macro-character (char &optional readtable)
   "Return the function associated with the character CHAR in READTABLE
\(default: *readtable*.\). If char isn't a macro charater in
READTABLE, return nil."
   (or readtable (setq readtable *readtable*))
   (let ((entry (reader:get-readtable-entry char readtable)))
     (if (reader:functionp entry) 
	 entry)))

(defun set-syntax-from-character 
  (to-char from-char &optional to-readtable from-readtable)   
  "Make the syntax of TO-CHAR be the same as the syntax of FROM-CHAR.
Optional TO-READTABLE and FROM-READTABLE are the corresponding tables
to use. TO-READTABLE defaults to the current readtable
\(*readtable*\), and FROM-READTABLE to nil, meaning to use the
syntaxes from the standard Lisp Readtable."
  (or to-readtable (setq to-readtable *readtable*))
  (or from-readtable 
      (setq from-readtable reader:internal-standard-readtable))
  (let ((from-syntax
	 (reader:get-readtable-entry from-char from-readtable)))
    (if (vectorp from-syntax)
	;; dispatch macro character table
	(setq from-syntax (copy-sequence from-syntax)))
    (setf (reader:get-readtable-entry to-char to-readtable)
	  from-syntax))
  t)


;; Dispatch macro character
(defun make-dispatch-macro-character (char &optional readtable)
  "Let CHAR be a dispatch macro character in READTABLE (default: *readtable*)."
  (or readtable (setq readtable *readtable*))
  (setf (reader:get-readtable-entry char readtable)
	;; create a dispatch character table 
	(make-vector reader:readtable-size nil)))


(defun set-dispatch-macro-character 
  (disp-char sub-char function &optional readtable)
  "Make reading CHAR1 followed by CHAR2 be handled by FUNCTION.
Optional argument READTABLE (default: *readtable*).  CHAR1 must first be 
made a dispatch char with `make-dispatch-macro-character'."
  (or readtable (setq readtable *readtable*))
  (let ((disp-table (reader:get-readtable-entry disp-char readtable)))
    ;; check whether disp-char is a valid dispatch character
    (or (vectorp disp-table)
	(error "`%c' not a dispatch macro character." disp-char))
    ;; check whether function is a valid function 
    (or (reader:functionp function) 
	(error "Not valid dispatch character macro function: %s" function))
    (setf (aref disp-table sub-char) function)))


(put 'set-dispatch-macro-character 'edebug-form-spec
     '(&define sexp sexp function-form &optional def-form))
(put 'set-dispatch-macro-character 'lisp-indent-function 2)


(defun get-dispatch-macro-character (disp-char sub-char &optional readtable)
  "Return the macro character function for SUB-CHAR unser DISP-CHAR in
READTABLE (default: *readtable*), or nil if there is no such
function."
  (or readtable (setq readtable *readtable*))
  (let ((disp-table (reader:get-readtable-entry disp-char readtable)))
    (and (vectorp disp-table)
	 (reader:functionp (aref disp-table sub-char))
	 (aref disp-table sub-char))))


(defun reader:functionp (function)
  "Check whether FUNCTION is a valid function object to be used 
as (dispatch) macro character function."
  (or (and (symbolp function) (fboundp function))
      (compiled-function-p function)
      (and (consp function) (eq (first function) 'lambda))))
	   

;; The basic reader loop 

;; shared and circular structure reading
(defvar reader:shared-structure-references nil)
(defvar reader:shared-structure-labels nil)


(defconst before-read-hook nil)
(defconst after-read-hook nil)

;; Set the hooks to `read-char' in order to step through the reader:
;; (add-hook 'before-read-hook '(lambda () (message "before") (read-char)))
;; (add-hook 'after-read-hook '(lambda () (message "after") (read-char)))
   
;; *** Documenting internal things is fine, but you should probably leave
;; them as comments to save space.

(defmacro reader:encapsulate-recursive-call (reader-call)
  "Encapsulate READER-CALL, a form that contains a recursive call to the
reader, for usage inside the main reader loop.  The macro wraps two
hooks around READER-CALL: `before-read-hook' and `after-read-hook'. 

If READER-CALL returns normally, the macro exits immediately from the
surrounding loop with the value of READER-CALL as result.  If it exits
non-locally (with tag `reader-ignore'), it just returns the value of
READER-CALL, in which case the surrounding reader loop continues its
execution.

In both cases, `before-read-hook' and `after-read-hook' are called
before and after executing READER-CALL."

  (` (prog2
	 (run-hooks 'before-read-hook)
	 ;; this catch allows to ignore the return, in the case that reader:read-from-buffer
	 ;; should continue looping (e.g. skipping over comments)
	 (catch 'reader-ignore
	   ;; this only works inside a block (e.g., in a loop): 
	   ;; go outside 
	   (return 
	    (prog1 
		(, reader-call)
	      ;; this occurence of the after hook fires if the 
	      ;; reader-call returns normally ...
	      (run-hooks 'after-read-hook))))
       ;; ... and that one if  it was thrown to the tag 'reader-ignore
       (run-hooks 'after-read-hook))))

(defvar reader:tmp-buffer
  (get-buffer-create " *CL Read*"))

;; save a pointer to the original `read-from-string' function
(or (fboundp 'reader:original-read-from-string)
    (fset 'reader:original-read-from-string
	  (symbol-function 'read-from-string)))

(defun reader:read-from-string (string &optional start end)
  "Read one Lisp expression which is represented as text by STRING.
Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).
START and END optionally delimit a substring of STRING from which to read;
 they default to 0 and (length STRING) respectively.

This is the cl-read replacement of the standard elisp function
`read-from-string'."

  ;; It doesnt really make sense to have read-from-string depend on
  ;; what the current buffer happens to be.
  (if nil ;; (not cl-read-active)
      (reader:original-read-from-string string start end)
    (or start (setq start 0))
    (or end (setq end (length string)))
    (save-excursion
      (set-buffer reader:tmp-buffer)
      (auto-save-mode -1)
      (erase-buffer)
      (insert (substring string 0 end))
      (goto-char (1+ start))
      (cons 
       (reader:read-from-buffer reader:tmp-buffer nil)
       (1- (point))))))

;; (read-from-string "abc (car 'a) bc" 4)
;; (reader:read-from-string "abc (car 'a) bc" 4)
;; (read-from-string "abc (car 'a) bc" 2 11)
;; (reader:read-from-string "abc (car 'a) bc" 2 11)
;; (reader:read-from-string "`(car ,first ,@rest)")
;; (read-from-string ";`(car ,first ,@rest)")
;; (reader:read-from-string ";`(car ,first ,@rest)")


;; save a pointer to the original read function
(or (fboundp 'reader:original-read)
    (fset 'reader:original-read  (symbol-function 'read)))

(defun reader:read (&optional stream recursive-p)
  "Read one Lisp expression as text from STREAM, return as Lisp object.
If STREAM is nil, use the value of `standard-input' \(which see\).
STREAM or the value of `standard-input' may be:
 a buffer \(read from point and advance it\)
 a marker \(read from where it points and advance it\)
 a string \(takes text from string, starting at the beginning\)
 t \(read text line using minibuffer and use it\).

This is the cl-read replacement of the standard elisp function
`read'. The only incompatibility is that functions as stream arguments
are not supported."
  (if (not cl-read-active)
      (reader:original-read stream)
    (if (null stream)			; read from standard-input
	(setq stream standard-input))

    (if (eq stream 't)			; read from minibuffer
	(setq stream (read-from-minibuffer "Common Lisp Expression: ")))

    (cond 

     ((bufferp stream)			; read from buffer
      (reader:read-from-buffer stream recursive-p))

     ((markerp stream)			; read from marker
      (save-excursion 
	(set-buffer (marker-buffer stream))
	(goto-char (marker-position stream))
	(reader:read-from-buffer (current-buffer) recursive-p)))

     ((stringp stream)			; read from string
      (save-excursion
	(set-buffer reader:tmp-buffer)
	(auto-save-mode -1)
	(erase-buffer)
	(insert stream)
	(goto-char (point-min))
	(reader:read-from-buffer reader:tmp-buffer recursive-p)))
     (t 
      (error "CL reader error: Not a valid stream: %s"
	     stream)))))

;; (reader:read "#'car")
;; (read)
;; (reader:read)
;; (let ((standard-input nil)) (reader:read))
;; (reader:read (current-buffer))  'hello
;; (reader:read (save-excursion (backward-sexp 1) (point-marker)))

(defun reader:read-from-buffer (&optional stream recursive-p)
  (or (bufferp stream)
      (error "Sorry, can only read on buffers"))
  (if (not recursive-p)
      (let (reader:shared-structure-references
	    reader:shared-structure-labels)
	(reader:restore-shared-structure
	 (reader:read-from-buffer stream 't)))

    (loop for char = (following-char)
	  for entry = (reader:get-readtable-entry  char *readtable*)
	  if (eobp) do (error "CL read error: End of file during reading")
	  do 
	  (cond 

	   ((eq entry 'illegal)
	    (error "CL read error: `%c' has illegal character syntax" char))

	   ;; skipping whitespace characters must be done inside this
	   ;; loop as character macro subroutines may return without
	   ;; leaving the loop using (throw 'reader-ignore ...)
	   ((eq entry 'whitespace)
	    (forward-char 1)  
	    ;; skip all whitespace
	    (while (eq 'whitespace 
		       (reader:get-readtable-entry  
			(following-char) *readtable*))
	      (forward-char 1)))

	   ;; for every token starting with a constituent character
	   ;; call the built-in reader (symbols, numbers, strings,
	   ;; characters with ?<char> syntax)
	   ((eq entry 'constituent)    
	    (reader:encapsulate-recursive-call
	     (reader:read-constituent stream)))

	   ((vectorp entry)
	    ;; Dispatch macro character. The dispatch macro character
	    ;; function is contained in the vector `entry', at the
	    ;; place indicated by <sub-char>, the first non-digit
	    ;; character following the <disp-char>:
	    ;; 	<disp-char><digit>*<sub-char>
	    (reader:encapsulate-recursive-call
	      (loop initially do (forward-char 1)
		    for sub-char = (prog1 (following-char) 
				     (forward-char 1))
		    while (memq sub-char 
				'(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		    collect sub-char into digit-args
		    finally 
		    (return 
		     (funcall 
		      ;; no test is done here whether a non-nil
		      ;; contents is a correct dispatch character
		      ;; function to apply.
		      (or (aref entry sub-char)
			  (error 
			   "Cl reader error: undefined subsequent dispatch \
character `%c'" sub-char))
		      stream
		      sub-char 
		      (string-to-int
		       (apply 'concat 
			      (mapcar 
			       'char-to-string digit-args))))))))
	    
	   (t
	    ;; must be a macro character. In this case, `entry' is
	    ;; the function to be called
	    (reader:encapsulate-recursive-call
	      (progn 
		(forward-char 1)
		(funcall entry stream char))))))))
;; ?\"   
;; '[aaaa  (a . b)  bbbb]  `(a ,b ,@l) (` a (, b) (,@ l))


(put 'reader:encapsulate-recursive-call 'edebug-form-spec '(form))
(put 'reader:encapsulate-recursive-call 'lisp-indent-function 0)


'(defun reader:read-constituent (stream)
  ;; For Emacs 19, just read it.
  (reader:original-read stream))

(defun reader:read-constituent (stream)
  (prog1 (reader:original-read stream)
;;; For Emacs 18, backing up is necessary because the `read'
;;; function reads one character too far after reading a symbol or number.
;;; This doesnt apply to reading chars (e.g. ?n).
    ;; This still loses for escaped chars.
    (if (not (eq (reader:get-readtable-entry
		  (preceding-char) *readtable*) 'constituent))
	(forward-char -1))))



;; Creation and initialization of an internal standard readtable. 

(defconst  reader:internal-standard-readtable
  (loop with raw-readtable = 
	(make-vector (1+ reader:readtable-size) 'illegal)
	initially do (setf (aref raw-readtable reader:readtable-size)
			   'readtable)
	for entry in 
	'((constituent ?! ?@ ?$ ?% ?& ?* ?_ ?- ?+ ?= ?/ ?\\ ?0 ?1 ?2
		       ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?: ?~ ?> ?< ?a ?b
		       ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p
		       ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z ?A ?B ?C ?D
		       ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R
		       ?S ?T ?U ?V ?W ?X ?Y ?Z)
	  (whitespace ?  ?\t ?\n ?\r ?\f)

	  ;; The following CL character classes are only useful for
	  ;; token parsing.  We don't need them, as token parsing is
	  ;; left to the built-in reader.
	  ;; (single-escape ?\\)
	  ;; (multiple-escape ?|)
	  )
	do 
	(loop for char in (rest entry)
	      do (setf (reader:get-readtable-entry  char raw-readtable)
		       (first entry)))
	finally return raw-readtable)
  "The original (CL-like) standard readtable. If you ever modify this
readtable, you won't be able to recover a standard readtable using
\(copy-readtable nil\)")


;; Variables used non-locally in the standard readmacros
(defvar reader:context)
(defvar reader:stack)
(defvar recursive-p)  ; need reader: prefix.

;; Changing the setting for a char here needs to be done
;; for both the internal standard readtable and the *readtable*.
;; Is (set-syntax-from-character char char *readtable*) sufficient?
;; Could we instead set-macro-character for *readtable* in the source below,
;; and then copy *readtable* to reader:internal-standard-readtable?

;; Chars and strings

;; This is defined to distinguish chars from constituents 
;; since chars read without reading too far.
(set-macro-character ?\?
  (function
   (lambda (stream char)
     (forward-char -1)
     (reader:original-read stream)))
  reader:internal-standard-readtable)

;; This is defined to distinguish strings from constituents
;; since backing up after reading a string is simpler.
(set-macro-character ?\"
  (function
   (lambda (stream char)
     (forward-char -1)
     (prog1 (reader:original-read stream)
       ;; This is not needed with Emacs 19.  See above.
       (if (/= (preceding-char) ?\")
	   (forward-char -1)))))
  reader:internal-standard-readtable)

;; Lists and dotted pairs
(set-macro-character ?\( 
  (function 
   (lambda (stream char)
     (catch 'read-list
       (let ((reader:context 'list) reader:stack )
	 ;; read list elements up to a `.'
	 (catch 'dotted-pair
	   (while t
	     (setq reader:stack (cons (reader:read-from-buffer stream 't) 
				      reader:stack))))
	 ;; In dotted pair. Read one more element
	 (setq reader:stack (cons (reader:read-from-buffer stream 't) 
				  reader:stack)
	       ;; signal it to the closing paren
	       reader:context 'dotted-pair)
	 ;; this *must* be the closing paren that throws away from here
	 (reader:read-from-buffer stream 't)
	 ;; otherwise an error is signalled
	 (error "illegal dotted pair read syntax")))))
  reader:internal-standard-readtable)

(set-macro-character ?\) 
  (function 
   (lambda (stream char)
     (cond ((eq reader:context 'list)
	    (throw 'read-list (nreverse reader:stack)))
	   ((eq reader:context 'dotted-pair)
	    ;(throw 'read-list (apply 'list* (nreverse reader:stack)))
	    (throw 'read-list (nconc (nreverse (cdr reader:stack)) 
				     (car reader:stack)))
	    )
	   (t 
	    (error "CL read error: `)' doesn't end a list")))))
  reader:internal-standard-readtable)
	
(set-macro-character ?\.
  (function 
   (lambda (stream char)
     (and (eq reader:context 'dotted-pair) 
	  (error "CL read error: no more than one `.' allowed in list"))
     (throw 'dotted-pair nil)))
  reader:internal-standard-readtable)

;; '(#\a . #\b)
;; '(a . (b . c))

;; Vectors: [a b]
(set-macro-character ?\[
  (function
   (lambda (stream char)
     (let ((reader:context 'vector))
       (catch 'read-vector
	 (let ((reader:context 'vector)
	       reader:stack)
	   (while t (push (reader:read-from-buffer stream 't)
			  reader:stack)))))))
  reader:internal-standard-readtable) 

(set-macro-character ?\] 
  (function 
   (lambda (stream char)
     (if (eq reader:context 'vector)
	 (throw 'read-vector (apply 'vector (nreverse reader:stack)))
       (error "CL read error: `]' doesn't end a vector"))))
  reader:internal-standard-readtable) 


;; Quote and backquote comma macro
(set-macro-character ?\'
  (function
   (lambda (stream char)
     (list 'quote (reader:read-from-buffer stream 't))))
  reader:internal-standard-readtable)

(set-macro-character ?\`
  (function
   (lambda (stream char)
     (if (= (following-char) ?\ )
	 ;; old backquote syntax. This is ambigous, because 
	 ;; (`(sexp)) is a valid form in both syntaxes, but 
	 ;; unfortunately not the same. 
	 ;; old syntax: read -> (` (sexp))
	 ;; new syntax: read -> ((` (sexp)))
	 '\`
       (list '\` (reader:read-from-buffer stream 't)))))
  reader:internal-standard-readtable)

(set-macro-character ?\,
  (function
   (lambda (stream char)
     (cond ((eq (following-char) ?\ )
	    ;; old syntax
	    '\,)
	   ((eq (following-char) ?\@)
	    (forward-char 1)
	    (cond ((eq (following-char) ?\ )
		   '\,\@)
		  ((list '\,\@ (reader:read-from-buffer stream 't)))))
	   ((list '\, (reader:read-from-buffer stream 't))))))
  reader:internal-standard-readtable)
;; 'a '(a b c)
;; `(,a ,@b c)  
;; the old syntax is also supported:
;; (` ((, a) (,@ b) c))    

;; Single character comment:  ; 
(set-macro-character ?\;
  (function
   (lambda (stream char)
     (skip-chars-forward "^\n\r")
     (throw 'reader-ignore nil)))
  reader:internal-standard-readtable)

;; Standard CL dispatch character #
(make-dispatch-macro-character ?\# reader:internal-standard-readtable)

;; Function quoting: #'<function>
(set-dispatch-macro-character ?\# ?\'
  (function
   (lambda (stream char n)
     (or (= n 0) 
	 (error
	  "Cl reader error: numeric infix argument not allowed %d" n))
     (list (if (featurep 'cl)  'function* 'function)
	   (reader:read-from-buffer stream 't))))
  reader:internal-standard-readtable)

;; Character syntax: #\<char> 
;; Not yet implemented: #\Control-a #\M-C-a etc. 
(set-dispatch-macro-character ?# ?\\
  (function 
   (lambda (stream char n)
     (or (= n 0) 
	 (error 
	  "Cl reader error: numeric infix argument not allowed %d" n))
     (let ((next (following-char))
	   name)
       (if (not (and (<= ?a next) (<= next ?z)))
	   (progn (forward-char 1) next)
	 (setq next (reader:read-from-buffer stream t))
	 (cond ((symbolp next) (setq name (symbol-name next)))
	       ((integerp next) (setq name (int-to-string next))))
	 (if (= 1 (length name))
	     (string-to-char name)
	   (case next
	     (linefeed	?\n)
	     (newline	?\r)
	     (space	?\ )
	     (rubout	?\b)
	     (page	?\f)
	     (tab       ?\t)
	     (return	?\C-m)
	     (t
	      (error
	       "CL read error: unknown character specification `%s'"
	       next))))))))
  reader:internal-standard-readtable)
;; '(#\# #\> #\< #\a #\A #\tab #\return #\space)


;; Read and load time evaluation:  #.<form>
;; Not yet implemented: #,<form>
(set-dispatch-macro-character ?\# ?\.
  (function 
   (lambda (stream char n)
     (or (= n 0) 
	 (error 
	  "Cl reader error: numeric infix argument not allowed %d" n))
     ;; This eval will see all internal vars of reader, 
     ;; e.g. stream, recursive-p.  Anything that might be bound.
     (eval (reader:read-from-buffer stream t))))
  reader:internal-standard-readtable)
;; '(#.(current-buffer) #.(get-buffer "*scratch*"))

;; Path names (kind of):  #p<string>, #P<string>,
(set-dispatch-macro-character ?\# ?\P
  (function 
   (lambda (stream char n)
     (or (= n 0) 
	 (error 
	  "Cl reader error: numeric infix argument not allowed %d" n))
     (let ((string (reader:read-from-buffer stream 't)))
       (or (stringp string) 
	   (error "Cl reader error: Pathname must be a string: %s" string))
       (expand-file-name string))))
  reader:internal-standard-readtable)

(set-dispatch-macro-character ?\# ?\p
  (get-dispatch-macro-character ?\# ?\P reader:internal-standard-readtable)
  reader:internal-standard-readtable)

;; #P"~/.emacs"
;; #p"~root/home" 

;; Feature reading:  #+<feature>,  #-<feature>
;; Not yet implemented: #+<boolean expression>, #-<boolean expression>

(set-dispatch-macro-character ?\# ?\+
  (function 
   (lambda (stream char n)
     (or (= n 0) 
	 (error 
	  "Cl reader error: numeric infix argument not allowed %d" n))
     (let ((feature (reader:read-from-buffer stream 't))
	   (object (reader:read-from-buffer stream 't)))
       (if (featurep feature)
	   object
	 (throw 'reader-ignore nil)))))
  reader:internal-standard-readtable)

(set-dispatch-macro-character ?\# ?\-
  (function 
   (lambda (stream char n)
     (or (= n 0) 
	 (error 
	  "Cl reader error: numeric infix argument not allowed %d" n))
     (let ((feature (reader:read-from-buffer stream 't))
	   (object (reader:read-from-buffer stream 't)))
       (if (featurep feature)
	   (throw 'reader-ignore nil)
	 object))))
  reader:internal-standard-readtable)

;; (#+cl loop #+cl do #-cl while #-cl t (body))


;; Circular and shared structure reading: #<n>=, #<n>#
(set-dispatch-macro-character ?\# ?\=
  (function 
   (lambda (stream char n)
     (if (memq n reader:shared-structure-labels)
	 (error "Cl reader error: label defined twice")
       (push n reader:shared-structure-labels))
     (let* ((string (int-to-string n))
	    (ref (or (find string reader:shared-structure-references
			   :test 'string=)
		     (first 
		      (push (make-symbol string) 
			    reader:shared-structure-references)))))
	 
       (setf (symbol-value ref) 
	     ;; this is also the return value 
	     (reader:read-from-buffer stream 't)))))
  reader:internal-standard-readtable)


(set-dispatch-macro-character ?\# ?\#
  (function
   (lambda (stream char n)
     ;; using the non-local variable `recursive-p' (from the reader
     ;; main loop) doesn't seems very clever. Should do this
     ;; differently ...
     (if (not recursive-p)
	 (error "Cl reader error: references at top level not allowed"))
     (let* ((string (int-to-string n))
	    (ref (or (find string reader:shared-structure-references
			   :test 'string=)
		     (first
		      (push (make-symbol string) 
			    reader:shared-structure-references)))))
       ;; the value of reading a #n# form is a reference symbol
       ;; whose symbol value will be the shared structure
       ref)))
  reader:internal-standard-readtable)

(defun reader:restore-shared-structure (obj)
  (cond 
   ((consp obj)
    (if (memq (car obj) reader:shared-structure-references)
	(setf (car obj) (symbol-value (car obj)))
      (reader:restore-shared-structure (car obj)))
    (if (memq (cdr obj) reader:shared-structure-references)
	(setf (cdr obj) (symbol-value (cdr obj)))
      (reader:restore-shared-structure (cdr obj))))
	
   ((vectorp obj)
    (loop for i below (length obj)
	  do
	  (if;; substructure  is a reference
	      (memq (aref obj i) reader:shared-structure-references)
	      ;; replace it by the pointer in the cdr of the ref
	      (setf (aref obj i) (symbol-value (aref obj i)))
	    (reader:restore-shared-structure (aref obj i))))))
  obj)


;; #1=(a b #3=[#2=c])
;; (#1=[#\return #\a] #1# #1#)
;; (#1=[a b c] #1# #1#)
;; #1=(a b . #1#)

;; Now make the current readtable
(defvar *readtable* (copy-readtable nil)
  "The current readtable.")


;; Replace built-in functions that call the (built-in) reader: 

(or (fboundp 'reader:original-eval-current-buffer)
    (fset 'reader:original-eval-current-buffer 
	  (symbol-function 'eval-current-buffer)))
    
(defun reader:eval-current-buffer (&optional printflag)
  "Evaluate the current buffer as Lisp code.
Programs can pass argument PRINTFLAG which controls printing of output:
nil means discard it\; anything else is stream for print.

This is the cl-read replacement of the standard elisp function
`eval-current-buffer'."

  ;; The standard eval-current-buffer doesn't use eval-region.
  (interactive)
  (if (not cl-read-active)
      (reader:original-eval-current-buffer printflag)
    (reader:eval-buffer (current-buffer))))

(or (fboundp 'reader:original-eval-buffer)
    (fset 'reader:original-eval-buffer 
	  (if (fboundp 'eval-buffer)  ;; only in Emacs 19.
	      (symbol-function 'eval-buffer)
	    'eval-buffer)))

(defun reader:eval-buffer (bufname &optional printflag)
  "Execute BUFFER as Lisp code.  Programs can pass argument PRINTFLAG
which controls printing of output: nil means discard it; anything else
is stream for print.

This is the cl-read replacement of the standard elisp function
`eval-buffer'."
  (interactive "bBuffer: ")
  (if (not cl-read-active)
      (reader:original-eval-buffer bufname printflag)
    (save-excursion
      (set-buffer (or (get-buffer bufname) 
		      (error "No such buffer: %s" bufname)))
      (reader:eval-region (point-min) (point-max) printflag))))

(or (fboundp 'reader:original-eval-region)
    (fset 'reader:original-eval-region 
	  (symbol-function 'eval-region)))

;; (borrowed from Daniel LaLiberte's edebug)
(defun reader:eval-region (start end &optional output)
  "Execute the region as Lisp code.
When called from programs, expects two arguments,
giving starting and ending indices in the current buffer
of the text to be executed.
Programs can pass third argument PRINTFLAG which controls output:
nil means discard it; anything else is stream for printing it.

If there is no error, point does not move.  If there is an error,
point remains at the end of the last character read from the buffer.

arguments: (b e &optional printflag)

This is the cl-read replacement of the standard elisp function
`eval-region'."

  ;; One other difference concerns inserting whitespace after the expression.
  (interactive "r")
  (if (not cl-read-active)
      (reader:original-eval-region start end output)
    (let ((pnt (point))
	  (buf (current-buffer))
	  (inside-buf (current-buffer))
	  ;; Mark the end because it may move.
	  (end-marker (set-marker (make-marker) end))
	  form
	  val)
      (goto-char start)
      (reader:skip-whitespace)
      (while (< (point) end-marker)
	(setq form (reader:read-from-buffer inside-buf))

	;; Evaluate normally - after restoring the current-buffer.
	(let ((current-buffer (current-buffer)))
	  (set-buffer inside-buf)
	  (setq val (eval form))  ;; *** All the local vars above are visible.
	  ;; Remember current buffer for next time.
	  (setq inside-buf (current-buffer))
	  (set-buffer current-buffer))

	(if output
	    (let ((standard-output (or output t)))
	      (setq values (cons val values))
	      (if (eq standard-output t)
		  (prin1 val)
		(princ "\n")
		(prin1 val)
		(princ "\n")
		)))

	(goto-char 
	 (min (max end-marker (point)) 
	      (progn (reader:skip-whitespace) (point)))))
      (if (null output)
	  ;; like save-excursion recovery, but only if no error
	  (progn
	    ;; but mark is not restored
	    (set-buffer buf)
	    (goto-char pnt)))
      ;; return always nil
      nil)))

(defun reader:skip-whitespace ()
  ;; Leave point before the next token, skipping white space and comments.
  (skip-chars-forward " \t\r\n\f")
  (while (= (following-char) ?\;)
    (skip-chars-forward "^\n\r")  ; skip the comment
    (skip-chars-forward " \t\r\n\f")))


;; installing/uninstalling the cl reader
(defun cl-reader-install ()
  (interactive)
  (fset 'read 			'reader:read)
  (fset 'read-from-string 	'reader:read-from-string)
  (fset 'eval-current-buffer 	'reader:eval-current-buffer)
  (fset 'eval-buffer 		'reader:eval-buffer)
  (fset 'eval-region 		'reader:eval-region))

(defun cl-reader-uninstall ()
  (interactive)
  (fset 'read 		       
	(symbol-function 'reader:original-read))
  (fset 'read-from-string	
	(symbol-function 'reader:original-read-from-string))
  (fset 'eval-current-buffer 	
	(symbol-function 'reader:original-eval-current-buffer))
  (fset 'eval-buffer 		
	(symbol-function 'reader:original-eval-buffer))
  (fset 'eval-region 		
	(symbol-function 'reader:original-eval-region)))

;; now install the replacement functions:
(cl-reader-install)

; Example:
;
; After having called `cl-reader-install', this function can be compiled
; (M-x elisp-compile-defun) with the syntax used here, but
; `eval-last-sexp' and `eval-defun' don't work always (???)
;
; 
;
;(defun test (el list)
;  (mapcar #'list
;          `(,el  ,@list #\a #\0 #\return )))
;
; You also can call the reader explicitly as follows: 
;
;(read (current-buffer)) ;; <- set point after "))", then type C-x C-e   
;(defun test (el list)
;  (mapcar #'list
;          `(,el  ,@list #\a #\0 #\return )))

;;; also, the `eval-print-last-sexp' function (LF in *scratch*) seems to work 
;;; partially: 
;
;#'car  <LF>
;car
;'(#\a #\b) <LF>
;(97 98)
;
;;; but there is an error on that form:
;(setq l '(a b c)) <LF>
;(a b c)
;`(,l ,@l)  <LF>
;((a b c) a b c)
;)

;; end cl-read.el
