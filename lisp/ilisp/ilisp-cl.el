;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-cl.el,v 1.18 1993/09/03 02:05:07 ivan Rel $
;;;%Header
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell, ccm@cs.cmu.edu.
;;;
;;; Send mail to ilisp-bug@darwin.bu.edu if you have problems.
;;;
;;; Send mail to ilisp-request@darwin.bu.edu if you want to be on the
;;; ilisp mailing list.
;;;
;;;


;;;
;;; ILISP Common Lisp dialect definition
;;;


;;;%%Common LISP
(defdialect clisp "Common LISP"
  ilisp
  (setq ilisp-load-or-send-command 
	"(or (and (load \"%s\" :if-does-not-exist nil) t)
             (and (load \"%s\" :if-does-not-exist nil) t))")
  (ilisp-load-init 'clisp "clisp.lisp")
  (setq ilisp-package-regexp "^[ \t]*(in-package[ \t\n]*"
	ilisp-package-command "(let ((*package* *package*)) %s (package-name *package*))"
	ilisp-package-name-command "(package-name *package*)"
	ilisp-in-package-command "(in-package \"%s\")"
	ilisp-last-command "*"
	ilisp-save-command "(progn (ILISP:ilisp-save) %s\n)"
	ilisp-restore-command "(ILISP:ilisp-restore)"
	ilisp-block-command "(progn %s\n)"
	ilisp-eval-command "(ILISP:ilisp-eval \"%s\" \"%s\" \"%s\")"
	ilisp-defvar-regexp "(defvar[ \t\n]")
  (setq ilisp-defvar-command 
	"(ILISP:ilisp-eval \"(let ((form '%s)) (progn (makunbound (second form)) (eval form)))\" \"%s\" \"%s\")")
  (setq ilisp-compile-command "(ILISP:ilisp-compile \"%s\" \"%s\" \"%s\")"
	ilisp-describe-command "(ILISP:ilisp-describe \"%s\" \"%s\")"
	ilisp-inspect-command "(ILISP:ilisp-inspect \"%s\" \"%s\")"
	ilisp-arglist-command "(ILISP:ilisp-arglist \"%s\" \"%s\")")
  (setq ilisp-documentation-types
	'(("function") ("variable")
	  ("structure") ("type")
	  ("setf") ("class")
	  ("(qualifiers* (class ...))")))
  (setq ilisp-documentation-command
	"(ILISP:ilisp-documentation \"%s\" \"%s\" \"%s\")")
  (setq ilisp-macroexpand-1-command 
	"(ILISP:ilisp-macroexpand-1 \"%s\" \"%s\")")
  (setq ilisp-macroexpand-command "(ILISP:ilisp-macroexpand \"%s\" \"%s\")")
  (setq ilisp-complete-command 
	"(ILISP:ilisp-matching-symbols \"%s\" \"%s\" %s %s %s)")
  (setq ilisp-locator 'lisp-locate-clisp)
  (setq ilisp-source-types 
	'(("function") ("macro") ("variable")
	  ("structure") ("type")
	  ("setf") ("class")
	  ("(qualifiers* (class ...))")))
  (setq ilisp-callers-command "(ILISP:ilisp-callers \"%s\" \"%s\")"
	ilisp-trace-command "(ILISP:ilisp-trace \"%s\" \"%s\" \"%s\")"
	ilisp-untrace-command "(ILISP:ilisp-untrace \"%s\" \"%s\")")
  (setq ilisp-directory-command "(namestring *default-pathname-defaults*)"
	ilisp-set-directory-command
	"(setq *default-pathname-defaults* (parse-namestring \"%s\"))")
  (setq ilisp-load-command "(load \"%s\")")
  (setq ilisp-compile-file-command 
	"(ILISP:ilisp-compile-file \"%s\" \"%s\")"))

(provide 'ilisp-cl )
