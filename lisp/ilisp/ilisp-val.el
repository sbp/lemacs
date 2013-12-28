;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-val.el,v 1.18 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP buffer value interface
;;;
;;;

;;;
(defun ilisp-value (variable &optional no-error-p)
  "Return the value of VARIABLE in the ILISP buffer.
If NO-ERROR-P is NIL, then an error will be signalled if VARIABLE is nil."
  (save-excursion
    (set-buffer (ilisp-buffer))
    (let ((value (eval variable)))
      (if value
	  value
	  (if no-error-p
	      nil
	      (error "%s is not defined." variable))))))

;;;
(defun set-ilisp-value (variable value)
  "Set the value of VARIABLE in the ILISP buffer."
  (save-excursion
    (set-buffer (ilisp-buffer))
    (set variable value)))

(provide 'ilisp-val )
