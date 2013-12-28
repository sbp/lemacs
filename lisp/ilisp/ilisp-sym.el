;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-sym.el,v 1.19 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP Lisp symbol utils.
;;;

;;;%%Symbol
(defun lisp-symbol (package delimiter name)
  "Create a LISP symbol."
  (list package (if package (or delimiter "::")) name))
(defun lisp-symbol-name (symbol)
  "Return the name of SYMBOL."
  (car (cdr (cdr symbol))))
(defun lisp-symbol-package (symbol)
  "Return the package of SYMBOL."
  (car symbol))
(defun lisp-symbol-delimiter (symbol)
  "Return the qualifier of SYMBOL."
  (car (cdr symbol)))

;;;
(defun lisp-symbol= (symbol1 symbol2)
  "Return T is SYMBOL1 is equal to SYMBOL2."
  (and (string= (lisp-symbol-name symbol1) (lisp-symbol-name symbol2))
       (string= (lisp-symbol-package symbol1) (lisp-symbol-package symbol2))
       (string= (lisp-symbol-delimiter symbol1)
		(lisp-symbol-delimiter symbol2))))

(provide 'ilisp-sym)
