;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-kcl.el,v 1.20 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP Kyoto Common Lisp dialect definition
;;;

;;;%%%KCL--these dialects by Tom Emerson
;;; kcl-check-prompt doesn't after the first break because the
;;; number of ">" characters doesn't increase.

(defun kcl-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in-break (and old (string-match ">+" old)))
 	 (old-level (if was-in-break
 			(- (match-end 0) (match-beginning 0))
 			0))
 	 (is-in-break (string-match ">+" new))
 	 (new-level (if is-in-break
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect kcl "Kyoto Common LISP" clisp
  (setq comint-prompt-regexp "^>+"
        ilisp-error-regexp "Error: "
        ilisp-binary-extension "o"
        comint-fix-error ":q"
        comint-continue ":r"
	comint-prompt-status
	(function
	 (lambda (old line)
	   (comint-prompt-status old line 'kcl-check-prompt)))))
(if (not kcl-program) (setq kcl-program "kcl"))

;;;%%%AKCL
(defdialect akcl "Austin Kyoto Common LISP" kcl)
(if (not akcl-program) (setq akcl-program "akcl"))


;;;%%%IBCL
(defdialect ibcl "Ibuki Common LISP" kcl
  (setq comint-prompt-regexp "^[-A-Z]*>+\\|^[-A-Z]* ->"
        comint-interrupt-regexp ">>Condition: Terminal Interrupt"
        comint-continue ":q"
        ilisp-reset ":q!"
        ilisp-error-regexp ">>Error:"))
(if (not ibcl-program) (setq ibcl-program "ibcl"))
(provide 'ilisp-kcl )
