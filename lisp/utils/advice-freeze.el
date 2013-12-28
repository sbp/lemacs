;;; advice-freeze.el --- freezing advice for permanent redefinition

;; Copyright (C) 1993 Hans Chalupsky

;; Author: Hans Chalupsky <hans@cs.buffalo.edu>
;; Created: 12 Jul 93
;; Version: 0.1
;; Keywords: advice


;;; Commentary:

;; New version of `defadvice' suggested by Jamie Zawinski that has a `freeze'
;; option. `freeze' is like `preactivate' with the difference that the
;; `defadvice' expands into a `defun/defmacro' that has the preactivated
;; advised definition as its body. Frozen advices behave like hard
;; redefinitions, i.e., they cannot be undone, they can be preloaded with
;; their advised documentation written to the DOC file, and they do not
;; need any advice runtime support. Proper load-sequencing of the original
;; and advised definitions has to be taken care of by the advice programmer.
;; The original definition of a freeze-advised function has to be available
;; before it gets redefined by the advice.

;; `freeze' only makes sense if used in conjunction with jwz's byte-compiler.

;; Usage:

;; `freeze' implies `activate' and `preactivate', hence, a freezing advice can
;; be defined as
;;
;;    (defadvice foo (before test freeze)
;;       "foo doc"
;;       <do stuff>)


;;; Code:

(require 'advice)

(setq ad-defadvice-flags (cons '("freeze") ad-defadvice-flags))


(defmacro defadvice (function args &rest body)
  "Defines a piece of advice for FUNCTION (a symbol).

  (defadvice <function> (<class> <name> [<position>] [<arglist>] {<flags>}*)
    [ [<documentation-string>] [<interactive-form>] ]
    {<body-form>}* )

<function> ::= name of the function to be advised
<class> ::= before | around | after | activation | deactivation
<name> ::= non-NIL symbol that names this piece of advice
<position> ::= first | last | <number> (optional, defaults to `first',
    see also `ad-add-advice')
<arglist> ::= an optional argument list to be used for the advised function
    instead of the argument list of the original. The first one found in
    before/around/after advices will be used.
<flags> ::= protect | disable | activate | compile | preactivate | freeze
    All flags can be specified with unambiguous initial substrings.
<documentation-string> ::= optional documentation for this piece of advice
<interactive-form> ::= optional interactive form to be used for the advised
    function. The first one found in before/around/after advices will be used.
<body-form> ::= any s-expression

Semantics of the various flags:
`protect': The piece of advice will be protected against non-local exits in
any code that precedes it. If any around advice of a function is protected
then automatically all around advices will be protected (the complete onion).

`activate': All advice of FUNCTION will be activated immediately if
FUNCTION has been properly defined prior to the defadvice.

`compile': In conjunction with `activate' specifies that the resulting
advised function should be compiled.

`disable': The defined advice will be disabled, hence it will not be used 
during activation until somebody enables it.

`preactivate': Preactivates the advised FUNCTION at macro expansion/compile
time. This generates a compiled advised definition according to the current
advice state that will be used during activation if appropriate. Only use
this if the defadvice gets actually compiled (with a v18 byte-compiler put
the defadvice into the body of a defun).

`freeze': Expands the defadvice into a redefining defun/defmacro according
to the current advice state. No other advice information will be saved.
Frozen advices cannot be undone, they behave like a hard redefinition of
the advised function (this should only be used with jwz's byte-compiler).
`freeze' implies `activate' and `preactivate'.

Look at the file advice.el for comprehensive documentation."
  (if (not (ad-name-p function))
      (error "defadvice: Illegal function name: %s" function))
  (let* ((class (car args))
	 (name (if (not (ad-class-p class))
		   (error "defadvice: Illegal advice class: %s" class)
		 (nth 1 args)))
	 (position (if (not (ad-name-p name))
		       (error "defadvice: Illegal advice name: %s" name)
		     (setq args (nthcdr 2 args))
		     (if (ad-position-p (car args))
			 (prog1 (car args)
			   (setq args (cdr args))))))
	 (arglist (if (listp (car args))
		      (prog1 (car args)
			(setq args (cdr args)))))
	 (flags
	  (mapcar
	   (function
	    (lambda (flag)
	      (let ((completion
		     (try-completion (symbol-name flag) ad-defadvice-flags)))
		(cond ((eq completion t) flag)
		      ((assoc completion ad-defadvice-flags)
		       (intern completion))
		      (t (error "defadvice: Illegal or ambiguous flag: %s"
				flag))))))
	   args))
	 (advice (ad-make-advice
		  name (memq 'protect flags)
		  (not (memq 'disable flags))
		  (` (advice lambda (, arglist) (,@ body)))))
	 (preactivation (if (memq 'preactivate flags)
			    (ad-preactivate-advice
			     function advice class position)))
	 (redefinition
	  (if (memq 'freeze flags)
	      (ad-with-originals (ad-make-advised-definition-docstring)
		(fset 'ad-make-advised-definition-docstring
		      'ad-make-advised-docstring)
		(if (not (ad-has-proper-definition function))
		    (error
		     "defadvice: `freeze' needs proper definition of `%s'"
		     function))
		(ad-preactivate-advice function advice class position)))))
    ;; Now for the things to be done at evaluation time:
    (if redefinition
	(let* ((origname (ad-make-origname function))
	       (macro-p (ad-macro-p (car redefinition)))
	       (body (cdr (if macro-p
			      (ad-lambdafy (car redefinition))
			    (car redefinition)))))
	  (` (progn
	       (if (not (fboundp '(, origname)))
		   (fset '(, origname) (symbol-function '(, function))))
	       ((, (if macro-p 'defmacro 'defun))
		(, function)
		(,@ body)))))
      ;; the normal case:	 
      (` (progn
	   (ad-add-advice '(, function) '(, advice) '(, class) '(, position))
	   (,@ (if preactivation
		   (` ((ad-set-cache
			'(, function)
			;; the function will get compiled:
			(, (cond ((ad-macro-p (car preactivation))
				  (` (ad-macrofy
				      (function
				       (, (ad-lambdafy
					   (car preactivation)))))))
				 (t (` (function
					(, (car preactivation)))))))
			'(, (car (cdr preactivation))))))))
	   (,@ (if (memq 'activate flags)
		   (` ((ad-activate '(, function)
				    (, (if (memq 'compile flags) t)))))))
	   '(, function))))))

;;; advice-freeze.el ends here
