;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-ind.el,v 1.19 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP indentation
;;;


;;;%Indentation
(defun indent-line-ilisp (&optional whole-exp)
  "Indent current line as Lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one.  This is restricted to the current buffer input."
  (interactive "P")
  (save-restriction
    (if (memq major-mode ilisp-modes)
	(narrow-to-region (save-excursion (lisp-input-start)) (point-max)))
    (lisp-indent-line whole-exp)))

;;;
(defun indent-sexp-ilisp ()
  "Indent each line of the list starting just after point."
  (interactive)
  (save-restriction
    (if (memq major-mode ilisp-modes)
	(narrow-to-region (save-excursion (lisp-input-start)) (point-max)))
    (indent-sexp)))


(provide 'ilisp-ind)
