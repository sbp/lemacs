;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-el.el,v 1.19 1993/09/03 02:05:07 ivan Rel $
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
;;;
;;; 
;;; ILISP extensions to emacs lisp
;;;
;;;


;;;%Utils
;;; This should be in emacs, but it isn't.
(defun lisp-mem (item list &optional elt=)
  "Test to see if ITEM is equal to an item in LIST.
Option comparison function ELT= defaults to equal."
  (let ((elt= (or elt= (function equal)))
	(done nil))
    (while (and list (not done))
      (if (funcall elt= item (car list))
	  (setq done list)
	  (setq list (cdr list))))
    done))



;;;%%Misc
(defun lisp-memk (item list key)
  "Test to see if ITEM is in LIST using KEY on each item in LIST
before comparing it to ITEM."
  (lisp-mem item list (function (lambda (x y)
			(equal x (funcall key y))))))

;;; This should be in emacs, but it isn't.
(defun lisp-del (item list &optional test)
  "Delete ITEM from LIST using TEST comparison and return the result.
Default test is equal."
  (let ((test (or test (function equal)))
	(element list)
	(prev nil)
	(done nil))
    (while (and element (not done))
      (if (funcall test item (car element))
	  (progn
	    (setq done t)
	    (if prev
		(rplacd prev (cdr element))
		(setq list (cdr list))))
	  (setq prev element
		element (cdr element))))
    list))

;;;
(defun lisp-last (list)
  "Return the last element of LIST."
  (while (cdr list)
    (setq list (cdr list)))
  (car list))

(provide 'ilisp-el)
