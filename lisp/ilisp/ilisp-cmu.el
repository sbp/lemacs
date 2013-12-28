;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-cmu.el,v 1.21 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP CMU Common Lisp dialect definition
;;;
;;;%%%CMULisp

(defvar cmulisp-source-directory-regexp 
  "\\/afs\\/cs\\.cmu\\.edu\\/project\\/clisp\\/src\\/[0-9]*\\/"
  "*Regexp to match cmulisp source code directory.")

(defvar cmulisp-local-source-directory
  nil
  "*Where the cmulisp sources really are.")

(defun cmulisp-source-directory-fixup-function ()
  (if cmulisp-local-source-directory
      (replace-match cmulisp-local-source-directory)))

(defun cmulisp-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in-break (and old (string-match "]+" old)))
 	 (old-level (if was-in-break
 			(- (match-end 0) (match-beginning 0))
 			0))
 	 (is-in-break (string-match "]+" new))
 	 (new-level (if is-in-break
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect cmulisp "CMU Common LISP"
  clisp
  (ilisp-load-init 'cmu "cmulisp.lisp")
  (if cmulisp-local-source-directory
      (setq ilisp-source-directory-fixup-alist
	    (list 
	     (cons cmulisp-source-directory-regexp
		   cmulisp-local-source-directory)))
    (message "cmulisp-local-source-directory not set."))
  (setq comint-prompt-regexp "^\\([0-9]+\\]+\\|\\*\\) "
	ilisp-trace-command "(ILISP:cmulisp-trace \"%s\" \"%s\" \"%s\")"
	comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'cmulisp-check-prompt)))
	ilisp-error-regexp "ILISP:[^\"]*\\|Error [^\n]*"
	ilisp-arglist-command "(ILISP:arglist \"%s\" \"%s\")"
	ilisp-find-source-command "(ILISP:source-file \"%s\" \"%s\" \"%s\")"
	comint-fix-error ":pop"
	comint-continue ":go"
	ilisp-reset ":q"
	comint-interrupt-regexp "Interrupted at"
	ilisp-binary-extension "sparcf"))


(provide 'ilisp-cmu )
