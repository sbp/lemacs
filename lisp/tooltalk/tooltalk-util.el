;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; Emacs Tooltalk Utility Functions
;;;
;;; @(#)tooltalk-util.el 1.6 93/02/10


(defun initialize-tooltalk-message-arg (msg n mode value vtype)
  "Initialize the nth tooltalk message argument, creating a new argument
if neccessary.  No attempt to distinguish between strings that contain
binary data and ordinary strings is made, all non integer argument
values are converted to a string (if not a string already) and
loaded with tt_message_arg_val_set.  Applications that need to 
put binary data into a ToolTalk message argument should initialize
the argument with:

   (set-tooltalk-message-attribute bin-string msg 'arg_bval arg-n)
"
  (let ((n-args-needed
	 (- (1+ n) (get-tooltalk-message-attribute msg 'args_count))))
    (while (> n-args-needed 0)
      (add-tooltalk-message-arg msg mode vtype)
      (setq n-args-needed (1- n-args-needed))))

  (cond
   ((integerp value) 
    (set-tooltalk-message-attribute value msg 'arg_ival n))
   ((stringp value)
    (set-tooltalk-message-attribute value msg 'arg_val n))
   (t
    (error "The value specified for msg %s argument %d, %s, must be a string or an integer"
	   (prin1-to-string msg)
	   n
	   (prin1-to-string value)))))



(defconst tooltalk-arg-mode-ids 
  (list 'TT_IN 'TT_OUT 'TT_INOUT TT_IN TT_OUT TT_INOUT))

(defun initialize-tooltalk-message/pattern-args (initfn msg args)
  "Apply initfn to each the position mode value and type of
each argument in the list.  The value of initfn should be either
'initialize-tooltalk-message-arg or 'initialize-tooltalk-pattern-arg.
See make-tooltalk-message for a description of how arguments are specified.
We distinguish the short form for arguments, e.g. \"just-a-value\", 
from the long form by checking to see if the argument is a list whose
car is one of the ToolTalk mode values like TT_INOUT."
  (let ((n 0))
    (while args
      (let* ((arg (car args))
	     (long-form 
	      (and (consp arg) 
		   (member (car arg) tooltalk-arg-mode-ids)))
	     (mode 
	      (if long-form (car arg) TT_IN))
	     (value 
	      (cond
	       ((not long-form) arg)
	       ((cdr arg) (car (cdr arg)))
	       (t "")))
	     (type
	      (cond
	       ((and long-form
		     (cdr (cdr arg)) 
		     (stringp (car (cdr (cdr arg)))))
		(car (cdr (cdr arg))))
	       ((integerp value) "int")
	       (t "string"))))
	(funcall initfn msg n mode value type))
      (setq args (cdr args))
      (setq n (1+ n)))))


(defun initialize-tooltalk-message-attributes (msg attributes)
  "Initialize the tooltalk message attributes.  The value of 
attributes must be a property list in the same form as for 
make-tooltalk-message.  This function can be used to reset
an existing message or to initailize a new one.  See 
initialize-tooltalk-message-args for a description of how
arguments are initialized.
"
  (let ((args attributes)
	(initfn 'initialize-tooltalk-message-arg))
    (while (and args (cdr args))
      (let ((indicator (car args))
	    (value (car (cdr args))))
	(if (eq indicator 'args)
	    (initialize-tooltalk-message/pattern-args initfn msg value)
	  (set-tooltalk-message-attribute value msg indicator)))
      (setq args (cdr (cdr args))))))


(defun make-tooltalk-message (attributes &optional no-callback)
  "Create a tooltalk message and initialize its attributes.
The value of attributes must be a list of alternating keyword/values, 
where keywords are symbols that name valid message attributes.  
For example:

  (make-tooltalk-message 
    '(class TT_NOTICE
      scope TT_SESSION
      address TT_PROCEDURE
      op \"do-something\"
      args (\"arg1\" 12345 (TT_INOUT \"arg3\" \"string\"))))

Values must always be strings, integers, or symbols that
represent Tooltalk constants.  Attribute names are the same as 
those supported by set-tooltalk-message-attribute, plus 'args.

The value of args should be a list of message arguments where
each message argument has the following form:

   (mode [value [type]]) or just value

Where mode is one of TT_IN, TT_OUT, TT_INOUT and type is a string.  
If type isn't specified then \"int\" is used if the value is a 
number otherwise \"string\" is used.  If only a value is specified 
then mode defaults to TT_IN.  If mode is TT_OUT then value and 
type don't need to be specified.  You can find out more about the 
semantics and uses of ToolTalk message arguments in chapter 4 of the 
Tooltalk Programmers Guide.

The no-callback arg is a hack to prevent the registration of the
C-level callback.  This hack is needed by the current SPARCworks
tool startup mechanism.  Yuchho.
"
  (let ((msg (create-tooltalk-message no-callback)))
    (initialize-tooltalk-message-attributes msg attributes)
    msg))


(defun describe-tooltalk-message (msg &optional stream)
  "Print the messages attributes and arguments to stream.  This is often
useful for debugging."
  (let ((attrs
	 '(address
	   class
	   disposition
	   file
	   gid
	   handler
	   handler_ptype
	   object
	   op
	   opnum
	   otype
	   scope
	   sender
	   sender_ptype
	   session
	   state
	   status
	   status_string
	   uid 
	   callback)))
    (terpri stream)
    (while attrs
      (princ (car attrs) stream)
      (princ "  " stream)
      (prin1 (get-tooltalk-message-attribute msg (car attrs)) stream)
      (terpri stream)
      (setq attrs (cdr attrs))))

  (let ((n (get-tooltalk-message-attribute msg 'args_count))
	(i 0))
    (while (< i n)
      (princ "Argument " stream)
      (princ i stream)
      (princ "  " stream)
      (let ((type (get-tooltalk-message-attribute msg 'arg_type i)))
	(princ
	 (prin1-to-string
	  (list 
	   (get-tooltalk-message-attribute msg 'arg_mode i)
	   (if (equal type "int")
	       (get-tooltalk-message-attribute msg 'arg_ival i)	      
	       (get-tooltalk-message-attribute msg 'arg_val i))
	   type))
	 stream))
      (terpri stream)
      (setq i (1+ i)))))


(defun initialize-tooltalk-pattern-arg (pat n mode value vtype)
  "Add one tooltalk pattern argument.  No support for specifying
pattern arguments whose value is a vector of binary data is provided.
"
  (let ((converted-value   
	 (if (or (integerp value) (stringp value))
	     value
	   (prin1-to-string value))))
    (add-tooltalk-pattern-arg pat mode vtype converted-value)))


(defun initialize-tooltalk-pattern-attributes (pat attributes)
  "Initialize the tooltalk patterns attributes.  The value of
attributes must be a property list in the same form as for
make-tooltalk-pattern.  The value of each attribute (except category)
can either be a single value or a list of values.  If a list of
values is provided then the pattern will match messages with
a corresponding attribute that matches any member of the list.

This function can be used to add attribute values to an existing
pattern or to initailize a new one.  See 
initialize-tooltalk-message/pattern-args for a description of how arguments
are initialized.
"
  (let ((args attributes)
	(initfn 'initialize-tooltalk-pattern-arg))
    (while (and args (cdr args))
      (let ((indicator (car args))
	    (value (car (cdr args))))
	(cond
	 ((eq indicator 'args)
	  (initialize-tooltalk-message/pattern-args initfn pat value))
	 ((consp value)
	  (let ((values value))
	    (while values
	      (add-tooltalk-pattern-attribute (car values) pat indicator)
	      (setq values (cdr values)))))
	 (t
	  (add-tooltalk-pattern-attribute value pat indicator))))
      (setq args (cdr (cdr args))))))



(defun make-tooltalk-pattern (attributes)
  "Create a tooltalk pattern and initialize its attributes.
The value of attributes must be a list of alternating keyword/values, 
where keywords are symbols that name valid pattern attributes
or lists of valid attributes.  For example:

  (make-tooltalk-pattern 
    '(category TT_OBSERVE
      scope TT_SESSION
      op (\"operation1\" \"operation2\")
      args (\"arg1\" 12345 (TT_INOUT \"arg3\" \"string\"))))


Values must always be strings, integers, or symbols that
represent Tooltalk constants or lists of same.  When a list 
of values is provided all of the list elements are added to 
the attribute.  In the example above, messages whose op
attribute is \"operation1\" or \"operation2\" would match the pattern.

The value of args should be a list of pattern arguments where 
each pattern argument has the following form:

   (mode [value [type]]) or just value

Where mode is one of TT_IN, TT_OUT, TT_INOUT and type is a string.  
If type isn't specified then \"int\" is used if the value is a 
number otherwise \"string\" is used.  If only a value is specified 
then mode defaults to TT_IN.  If mode is TT_OUT then value and type 
don't need to be specified.  You can find out more about the semantics 
and uses of ToolTalk pattern arguments in chapter 3 of the Tooltalk
Programmers Guide.
"
  (let ((pat (create-tooltalk-pattern)))
    (initialize-tooltalk-pattern-attributes pat attributes)
    pat))



