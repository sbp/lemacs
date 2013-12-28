;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-acl.el,v 1.20 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP Allegro Common Lisp dialect definition
;;;

;;;%%%Allegro
(defun allegro-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((old-level (if (and old (eq 1 (string-match "[0-9]+" old)))
 			(string-to-int (substring old 1))
 			0))
 	 (new-level (if (eq 1 (string-match "[0-9]+" new))
 			(string-to-int (substring new 1))
 			0)))
    (<= new-level old-level)))
 
;;;
(defdialect allegro "Allegro Common LISP"
  clisp
  (ilisp-load-init 'allegro "allegro.lisp")
  (setq comint-fix-error ":pop"
	ilisp-reset ":reset"
	comint-continue ":cont"
	comint-interrupt-regexp  "Error: [^\n]* interrupt\)")
  (setq comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'allegro-check-prompt))))
  ;; <cl> or package> at top-level
  ;; [0-9c] <cl> or package> in error
  ;; (setq comint-prompt-regexp "^\\(\\[[0-9]*c*\\] \\|\\)\\(<\\|\\)[^>]*> ")
  (setq comint-prompt-regexp "^\\(\\[[0-9]+i?c?\\] \\|\\[step\\] \\)?\\(<?[-A-Za-z]* ?[0-9]*?>\\|[-A-Za-z0-9]+([0-9]+):\\) ")
  (setq ilisp-error-regexp
	"\\(ILISP:[^\"]*\\)\\|\\(Error:[^\n]*\\)\\|\\(Break:[^\n]*\\)")

  (setq ilisp-binary-command "excl:*fasl-default-type*")
  (setq ilisp-source-types (append ilisp-source-types '(("any"))))
  (setq ilisp-find-source-command 
	"(ILISP:ilisp-source-files \"%s\" \"%s\" \"%s\")")
  (setq ilisp-init-binary-command 
	"(let ((ext (or #+m68k \"68fasl\"
		        #+sparc \"sfasl\"
		        #+iris4d \"ifasl\"
                        #+dec3100 \"pfasl\"
                        excl:*fasl-default-type*)))
           #+allegro-v4.0 (setq ext (concatenate 'string ext \"4\"))
           ext)"))
(if (not allegro-program) (setq allegro-program "cl"))

(provide 'ilisp-acl )
