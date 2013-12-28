;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-style.el --- sets c-style control variables.
;; 
;; Author          : Daniel LaLiberte (liberte@a.cs.uiuc.edu)
;; Created On      : Wed Aug 12 08:00:20 1987
;; Last Modified By: Daniel LaLiberte
;; Last Modified On: Wed Nov 16 22:37:43 1988
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Definitions for buffer local c-mode indentation style.
;;; Put either (autoload 'set-c-style "c-style" nil t)
;;;         or (load "c-style") in your .emacs.

(defvar default-c-style 'GNU
  "*The default value of c-style.  Set this in your .emacs.")

;;; =====================================================
;;; There are several ways to call set-c-style
;;;
;;; - Just add set-c-style to your c-mode-hook.
;;;    Without style argument, default-c-style will be used.
;;;    With style argument, this will set the style for every 
;;;    c-mode buffer the same.
;;;
;;; - Call set-c-style from the Local Variables list.
;;;    e.g. "eval:(set-c-style 'C++)"
;;;
;;; - Call set-c-style interactively.  It prompts for the style name
;;;    with completion using default-c-style.
;;; =====================================================

;; Predefined styles
(defvar c-style-alist '(
			(GNU (c-indent-level . 2)
			     (c-continued-statement-offset . 2)
			     (c-brace-offset . 0)
			     (c-argdecl-indent . 5)
			     (c-label-offset . -2))
			(BSD (c-indent-level . 8)
			     (c-continued-statement-offset . 8)
			     (c-brace-offset . -8)
			     (c-argdecl-indent . 8)
			     (c-label-offset . -8))
			(K&R (c-indent-level . 5)
			     (c-continued-statement-offset . 5)
			     (c-brace-offset . -5)
			     (c-argdecl-indent . 0)
			     (c-label-offset . -5))
			(C++ (c-indent-level . 4)
			     (c-continued-statement-offset . 4)
			     (c-brace-offset . -4)
			     (c-argdecl-indent . 4)
			     (c-label-offset . -4))
			;; From Lynn Slater
			(LRS (c-indent-level . 4)
			     (c-continued-statement-offset . 4)
			     (c-brace-offset . 0)
			     (c-argdecl-indent . 4)
			     (c-label-offset . -2)
			     (c-auto-newline . nil)
			     )
			))

(defvar c-style nil
  "The buffer local C mode indentation style.")

(defvar default-c-style 'GNU
  "The default C mode indentation style.")

(defun set-c-style (&optional style)
  "Specify a style of indentation for C code for the current buffer.
The argument STYLE should be a standard style name defined in
`c-style-alist', such as GNU, BSD, K&R, etc.  (These are Lisp symbols.)
An omitted arg, or nil, means to use the value of `default-c-style'.

Setting the style sets various C-mode customization parameters accordingly,
all local to the current buffer."
  (interactive
   (let ((style-string			; get style name with completion
	  (completing-read
	   (format "Set c-mode indentation style to (default %s): "
		   default-c-style)
	   c-style-alist
	   t)))
     (if (string= "" style-string)
	 default-c-style
       (intern style-string))))

  ;; if style is nil, use default-c-style.
  (setq style (or style default-c-style))

  ;; Reject invalid styles.
  (or (assq style c-style-alist)
      (error (message "Undefined c style: %s" style)))

  ;; Set the style.
  (make-local-variable 'c-style)
  (setq c-style style)
  (message "c-style: %s" c-style)
    
  ;; Finally, set the indentation style variables making each one local
  (mapcar (function (lambda (c-style-pair)
		      (make-local-variable (car c-style-pair))
		      (set (car c-style-pair)
			   (cdr c-style-pair))))
	  (cdr (assq c-style c-style-alist)))

  c-style)

