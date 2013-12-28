;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Date:	Wed Dec 16 17:40:58 1992
;;; File:	tooltalk-macros.el
;;; Title:	Useful macros for ToolTalk/elisp interface
;;; SCCS:	@(#)tooltalk-macros.el	1.5 21 Jan 1993 19:09:24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro destructuring-bind-tooltalk-message (variables
					       args-count
					       message
					       &rest body)
  "
arglist: (variables args-count message &rest body)

Binds VARIABLES to the ARG_VALs and ARG_IVALs of MESSAGE, 
starting from N = 0, and executes BODY in that context.
Binds actual number of message args to ARGS-COUNT.  

VARIABLES is a list of local variables to bind.  
Each item in VARIABLES is either nil, a symbol, or a list of the form:

	(symbol type)

If the item is nil, the nth ARG_VAL or ARG_IVAL of MESSAGE is skipped.
If the item is a symbol, the nth ARG_VAL of MESSAGE is bound.
If the item is a list
	If type =  \"int\" the nth ARG_IVAL of MESSAGE is bound,
	otherwise the nth ARG_VAL of MESSAGE is bound.

If there are more items than actual arguments in MESSAGE, the extra
items are bound to nil.

For example,

(destructuring-bind-tooltalk-message (a (b \"int\") nil d) foo msg
  x y z)

expands to

(let* ((foo (get-tooltalk-message-attribute msg 'args_count))
       (a (if (< 0 foo)
	      (get-tooltalk-message-attribute msg 'arg_val 0)))
       (b (if (< 1 foo) 
	      (get-tooltalk-message-attribute msg 'arg_val 1)))
       (d (if (< 3 foo)
	      (get-tooltalk-message-attribute msg 'arg_val 3))))
  x y z)

See GET-TOOLTALK-MESSAGE-ATTRIBUTE for more information.
"
  (let* ((var-list variables)
	 (nargs args-count)
	 (msg message)
	 (length (length var-list))
	 (n -1)
	 var-item
	 var
	 type
	 request
	 bindings)
    (setq bindings (cons
		    (list nargs
			  (list
			   'get-tooltalk-message-attribute
			   msg
			   ''args_count))
		    bindings))
    (while var-list
      (setq var-item (car var-list)
	    var-list (cdr var-list))
      (if (eq 'nil var-item)
	  (setq n (1+ n))
	(progn
	  (if (listp var-item)
	      (setq var (car var-item)
		    type (car (cdr var-item)))
	    (setq var var-item
		  type "string"))
	  (setq n (1+ n))
	  (setq request (list
			 'get-tooltalk-message-attribute
			 msg
			 (if (equal "int" type)
			     ''arg_ival
			   ''arg_val)
			 n))
	  (setq bindings (cons
			  (list var
				(list 'if
				      (list '< n nargs)
				      request))
			  bindings)))))
    (nconc (list 'let* (nreverse bindings)) body)))
