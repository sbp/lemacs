;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-sch.el,v 1.20 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP Scheme dialect definition
;;;
;;;%%Scheme
(defdialect scheme "Scheme" ilisp
  (setq ilisp-block-command "(begin \n%s)")
  (setq ilisp-load-command "(load \"%s\")")
  )
(if (not scheme-program) (setq scheme-program "scheme"))

;;;Cscheme
;;; This has a problem since interrupts cause things to crash
;(defdialect cscheme "C Scheme"
;  scheme
;  (setq comint-prompt-regexp
;   "^[0-9]+ \\([\\]=]=>\\|Error->\\|Bkpt->\\|Debug->\\|Where->\\) ")
;  (setq ilisp-program "cscheme")
;  (setq ilisp-binary-extension "bin")
;  )


;;;Oaklisp
(defdialect oaklisp "Oaklisp Scheme"
  scheme
  (setq comint-prompt-regexp ">+ ")
  (setq comint-fix-error "(ret 0)")
  (setq ilisp-last-command "*")
  (setq ilisp-describe-command "(describe %s)")
  )
(provide 'ilisp-sch )
