;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-luc.el,v 1.20 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP Lucid Common Lisp dialect definition
;;;



;;;%%%Lucid
(defun lucid-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((old-level (if (and old (eq 0 (string-match "\\(->\\)+" old)))
 			(- (match-end 0) (match-beginning 0))
 			0))
	 (new-level (if (eq 0 (string-match "\\(->\\)+" new))
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect lucid "Lucid Common LISP"
  clisp
  (ilisp-load-init 'lucid "lucid.lisp")
  (setq comint-prompt-regexp "^\\(->\\)+ \\|^[^> ]*> "
	comint-fix-error ":a"
	ilisp-reset ":a :t"
	comint-continue ":c"
	comint-interrupt-regexp ">>Break: Keyboard interrupt"
	comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'lucid-check-prompt))))
  (setq ilisp-error-regexp "ILISP:[^\"]*\\|>>[^\n]*")
  (setq ilisp-source-types (append ilisp-source-types '(("any"))))
  (setq ilisp-find-source-command 
	"(ILISP:ilisp-source-files \"%s\" \"%s\" \"%s\")")
  (setq ilisp-binary-command 
	"(first (last lucid::*load-binary-pathname-types*))"))
(if (not lucid-program) (setq lucid-program "lisp"))

(provide 'ilisp-luc )
