;; Edebug specs for cl.el, version 1.0
;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Written by Daniel LaLiberte <liberte@cs.uiuc.edu>.
;; To be used with edebug.el version 3.1 or later.
;; These are for the cl.el, version 2.02 or later,
;; by Dave Gillespie <daveg@synaptics.com>.

;; This file need not be byte-compiled, but it shouldnt hurt.
;; The following causes cl-specs to be loaded when you use Edebug with cl.el.
;; (add-hook 
;;   'cl-load-hook
;;   (function 
;;    (lambda () 
;;      (add-hook 'edebug-setup-hook 
;; 	       (function (lambda () (load-library "cl-specs")))))))

(require 'edebug)

;; Blocks

(def-edebug-spec block (symbolp body))
(def-edebug-spec return (&optional form))
(def-edebug-spec return-from (symbolp &optional form))

;; Loops

(def-edebug-spec when t)
(def-edebug-spec unless t)
(def-edebug-spec case (form &rest (sexp body)))
(def-edebug-spec ecase case)
(def-edebug-spec do
  ((&rest &or symbolp (symbolp &optional form form))
   (form body) 
   cl-declarations body))
(def-edebug-spec do* do)
(def-edebug-spec dolist 
  ((symbolp form &optional form) cl-declarations body))
(def-edebug-spec dotimes dolist)
(def-edebug-spec dosymbols 
  ((symbolp &optional form form) cl-declarations body))
(def-edebug-spec do-all-symbols 
  ((symbolp &optional form) cl-declarations body))

;; Multiple values

(def-edebug-spec multiple-value-list (form))
(def-edebug-spec multiple-value-call (function-form body))
(def-edebug-spec multiple-value-bind 
  ((&rest symbolp) form cl-declarations body))
(def-edebug-spec multiple-value-setq ((&rest symbolp) form))
(def-edebug-spec multiple-value-prog1 (form body))

;; Bindings

(def-edebug-spec lexical-let let)
(def-edebug-spec lexical-let* let)

(def-edebug-spec psetq setq)
(def-edebug-spec progv (form form body))

(def-edebug-spec flet ((&rest (defun*)) cl-declarations body))
(def-edebug-spec labels flet)

(def-edebug-spec macrolet 
  ((&rest (&define name (&rest arg) cl-declarations-or-string def-body)) 
   cl-declarations body))

(def-edebug-spec symbol-macrolet 
  ((&rest (symbol sexp)) cl-declarations body))

(def-edebug-spec destructuring-bind
  (&define cl-macro-list form cl-declarations def-body))

;; Setf

(def-edebug-spec setf (&rest [place form])) ;; sexp is not specific enough
(def-edebug-spec psetf setf)

(def-edebug-spec letf  ;; *not* available in Common Lisp
  ((&rest (fence place &optional form))
   body))
(def-edebug-spec letf* letf)


(def-edebug-spec defsetf 
  (&define name 
	   [&or [symbolp &optional stringp]
		[cl-lambda-list (symbolp)]]
	   cl-declarations-or-string def-body))

(def-edebug-spec define-setf-method 
  (&define name cl-lambda-list cl-declarations-or-string def-body))

(def-edebug-spec define-modify-macro
  (&define name cl-lambda-list ;; should exclude &key
	   symbolp &optional stringp))

(def-edebug-spec callf
  ([&or symbolp lambda-expr] place &rest form))

(def-edebug-spec callf2
  ([&or symbolp lambda-expr] form place &rest form))

;; Other operations on places

(def-edebug-spec remf (place form))

(def-edebug-spec incf (place &optional form))
(def-edebug-spec decf incf)
(def-edebug-spec push (form place))
(def-edebug-spec pushnew 
  (form place &rest [[&or ":test" ":test-not" ":key"] function-form]))
(def-edebug-spec pop (place))

(def-edebug-spec shiftf (&rest place))  ;; really [&rest place] form
(def-edebug-spec rotatef (&rest place))


;; Functions with function args.  These are only useful if the
;; function arg is quoted with ' instead of function.

(def-edebug-spec some (function-form form &rest form))
(def-edebug-spec every some)
(def-edebug-spec notany some)
(def-edebug-spec notevery some)

;; Mapping

(def-edebug-spec map (form function-form form &rest form))
(def-edebug-spec maplist (function-form form &rest form))
(def-edebug-spec mapc maplist)
(def-edebug-spec mapl maplist)
(def-edebug-spec mapcan maplist)
(def-edebug-spec mapcon maplist)

;; Sequences

(def-edebug-spec reduce (function-form form &rest form))

;; Types and assertions

(def-edebug-spec cl-type-spec (sexp)) ;; not worth the trouble to specify, yet.

(def-edebug-spec deftype defmacro*)
(def-edebug-spec check-type (place cl-type-spec &optional stringp))
;; (def-edebug-spec assert (form &optional form stringp &rest form))
(def-edebug-spec assert (form &rest form))
(def-edebug-spec typecase (form &rest ([&or cl-type-spec "otherwise"] body)))
(def-edebug-spec etypecase typecase)

(def-edebug-spec ignore-errors t)

;; Time of Evaluation

(def-edebug-spec eval-when
  ((&rest &or "compile" "load" "eval") body))
(def-edebug-spec eval-when-compile t)
(def-edebug-spec load-time-value (form &optional &or "t" "nil"))

;; Declarations

(def-edebug-spec cl-decl-spec 
  ((symbolp &rest sexp)))

(def-edebug-spec cl-declarations
  (&rest ("declare" &rest cl-decl-spec)))

(def-edebug-spec cl-declarations-or-string
  (&or stringp cl-declarations))

(def-edebug-spec declaim (&rest cl-decl-spec))
(def-edebug-spec declare (&rest cl-decl-spec))  ;; probably not needed.
(def-edebug-spec locally (cl-declarations &rest form))
(def-edebug-spec the (cl-type-spec form))

;;======================================================
;; Lambda things

(def-edebug-spec cl-lambda-list
  (([&rest arg]
    [&optional ["&optional" cl-&optional-arg &rest cl-&optional-arg]]
    [&optional ["&rest" arg]]
    [&optional ["&key" [cl-&key-arg &rest cl-&key-arg]
		&optional "&allow-other-keywords"]]
    [&optional ["&aux" &rest
		&or (symbolp &optional def-form) symbolp]]
    )))

(def-edebug-spec cl-&optional-arg
  (&or (arg &optional def-form arg) arg))

(def-edebug-spec cl-&key-arg
  (&or ([&or (symbolp arg) arg] &optional def-form arg) arg))

(def-edebug-spec cl-macro-list
  (([&optional "&environment" arg]  ;; only allowed first and last.
    [&rest cl-macro-arg]
    [&optional ["&optional" &rest 
		&or (cl-macro-arg &optional def-form cl-macro-arg) 
		arg]]
    [&optional [[&or "&rest" "&body"] cl-macro-arg]]
    [&optional ["&key" [&rest 
			[&or ([&or (symbolp cl-macro-arg) arg] 
			      &optional def-form cl-macro-arg)
			     arg]]
		&optional "&allow-other-keywords"]]
    [&optional ["&aux" &rest
		&or (symbolp &optional def-form) symbolp]]
    [&optional "&environment" arg]  ;; only allowed first and last.
    )))

(def-edebug-spec cl-macro-arg
  (&or arg cl-macro-list1))

(def-edebug-spec cl-macro-list1
  (([&optional "&whole" arg]  ;; only allowed at lower levels
    [&rest cl-macro-arg]
    [&optional ["&optional" &rest 
		&or (cl-macro-arg &optional def-form cl-macro-arg) 
		arg]]
    [&optional [[&or "&rest" "&body"] cl-macro-arg]]
    [&optional ["&key" [&rest 
			[&or ([&or (symbolp cl-macro-arg) arg] 
			      &optional def-form cl-macro-arg)
			     arg]]
		&optional "&allow-other-keywords"]]
    [&optional ["&aux" &rest
		&or (symbolp &optional def-form) symbolp]]
    . [&or arg nil])))


(def-edebug-spec defun*
  ;; Same as defun but use cl-lambda-list.
  (&define name cl-lambda-list
	   cl-declarations-or-string
	   [&optional ("interactive" interactive)]
	   def-body))
(def-edebug-spec defsubst* defun*)

(def-edebug-spec defmacro* 
  (&define name cl-macro-list cl-declarations-or-string def-body))
(def-edebug-spec define-compiler-macro defmacro*)


(def-edebug-spec function*
  (&or symbolp ("lambda" cl-lambda-expr)))

(def-edebug-spec cl-lambda-expr
  (&define cl-lambda-list
	   ;cl-declarations-or-string
	   ;[&optional ("interactive" interactive)]
	   def-body))


;;======================================================
;; Structures
;; (def-edebug-spec defstruct (&rest sexp)) would be sufficient, but...

;; defstruct may contain forms that are evaluated when a structure is created.
(def-edebug-spec defstruct
  (&define  ; makes top-level form not be wrapped
   [&or symbolp
	(fence
	 symbolp &rest 
		 (&or [":conc-name" &or stringp "nil"]
		      [":constructor" symbolp &optional cl-lambda-list]
		      [":copier" symbolp]
		      [":predicate" symbolp]
		      [":include" symbolp &rest sexp];; not finished
		      ;; The following are not supported.
		      ;; [":print-function" ...]
		      ;; [":type" ...]
		      ;; [":initial-offset" ...]
		      ))]
   [&optional stringp]
   &rest &or symbolp (symbolp def-form &optional ":read-only" sexp)))

;;======================================================
;; Loop macro is in several parts.

(def-edebug-spec loop
  ([&optional ["named" symbolp]]
   [&rest 
    &or
    ["repeat" form]
    loop-for-as
    loop-with
    loop-initial-final]
   [&rest loop-clause]
   ))

(def-edebug-spec loop-with
  ("with" loop-var
   loop-type-spec
   [&optional ["=" form]]
   &rest ["and" loop-var
	  loop-type-spec
	  [&optional ["=" form]]]))

(def-edebug-spec loop-for-as
  ([&or "for" "as"] loop-for-as-subclause
   &rest ["and" loop-for-as-subclause]))

(def-edebug-spec loop-for-as-subclause
  (loop-var 
   loop-type-spec
   &or 
   [[&or "in" "on" "in-ref" "across-ref"]
    form &optional ["by" function-form]]

   ["=" form &optional ["then" form]]
   ["across" form]
   ["being" 
    [&or "the" "each"]
    &or 
    [[&or "element" "elements"] 
     [&or "of" "in" "of-ref"] form
     &optional "using" ["index" symbolp]];; is this right?
    [[&or "hash-key" "hash-keys"
	  "hash-value" "hash-values"]
     [&or "of" "in"]
     hash-table-p &optional ["using" ([&or "hash-value" "hash-values" 
					   "hash-key" "hash-keys"] sexp)]]
					
    [[&or "symbol" "present-symbol" "external-symbol"
	  "symbols" "present-symbols" "external-symbols"]
     [&or "in" "of"] package-p]
     
    ;; Extensions for Emacs Lisp, including Lucid Emacs.
    ;; (Need a general extension mechanism.)
    [[&or "frame" "frames"
	  "screen" "screens"
	  "buffer" "buffers"]]

    [[&or "window" "windows"] 
     [&or "of" "in"] form]

    [[&or "overlay" "overlays"
	  "extent" "extents"]
     [&or "of" "in"] form
     &optional [[&or "from" "to"] form]]

    [[&or "interval" "intervals"] 
     [&or "in" "of"] form
     &optional [[&or "from" "to"] form]
     ["property" form]]
     
    [[&or "key-code" "key-codes"
	  "key-seq" "key-seqs"
	  "key-binding" "key-bindings"] 
     [&or "in" "of"] form
     &optional ["using" ([&or "key-code" "key-codes"
			      "key-seq" "key-seqs"
			      "key-binding" "key-bindings"] 
			 sexp)]]
    ;; For arbitrary extensions, recognize anything else.
    [symbolp &rest [&or symbolp form]]
    ]
   
   ;; arithmetic - must be last since all parts are optional.
   [[&optional [[&or "from" "downfrom" "upfrom"] form]]
    [&optional [[&or "to" "downto" "upto" "below" "above"] form]]
    [&optional ["by" form]]
    ]))

(def-edebug-spec loop-initial-final
  (&or ["initially" 
	;; [&optional &or "do" "doing"]  ;; CLtL2 doesnt allow this.
	&rest loop-non-atomic-expr]
       ["finally" &or 
	[[&optional &or "do" "doing"] &rest loop-non-atomic-expr]
	["return" form]]))

(def-edebug-spec loop-and-clause
  (loop-clause &rest ["and" loop-clause]))

(def-edebug-spec loop-clause
  (&or
   [[&or "while" "until" "always" "never" "thereis"] form]
     
   [[&or "collect" "collecting"
	 "append" "appending"
	 "nconc" "nconcing"
	 "concat" "vconcat"] form 
	 [&optional ["into" loop-var]]]

   [[&or "count" "counting"
	 "sum" "summing"
	 "maximize" "maximizing"
	 "minimize" "minimizing"] form
	 [&optional ["into" loop-var]]
	 loop-type-spec]

   [[&or "if" "when" "unless"]
    form loop-and-clause
    [&optional ["else" loop-and-clause]]
    [&optional "end"]]

   [[&or "do" "doing"] &rest loop-non-atomic-expr]

   ["return" form]
   loop-initial-final
   ))

(def-edebug-spec loop-non-atomic-expr
  ([&not atom] form))

(def-edebug-spec loop-var
  ;; The symbolp must come last to recognize e.g. (a b . c)
  ;; loop-var => 
  ;; (loop-var . [&or nil loop-var])
  ;; (symbolp . [&or nil loop-var])
  ;; (symbolp . loop-var)
  ;; (symbolp . (symbolp . [&or nil loop-var]))
  ;; (symbolp . (symbolp . loop-var))
  ;; (symbolp . (symbolp . symbolp))
  (&or (loop-var . [&or nil loop-var]) [fence symbolp]))

(def-edebug-spec loop-type-spec
  (&optional ["of-type" loop-d-type-spec]))

(def-edebug-spec loop-d-type-spec
  (&or (loop-d-type-spec . [&or nil loop-d-type-spec]) cl-type-spec))


