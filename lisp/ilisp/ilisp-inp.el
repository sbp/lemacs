;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-inp.el,v 1.19 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP input functions
;;;

;;;%%Input 
(defun lisp-at-start ()
  "Return the point if you are at the start of an input expression in
an inferior Lisp."
  (save-excursion
    (let ((point (point)))
      (beginning-of-line)
      (comint-skip-prompt)
      (if (= point (point))
	  point))))

;;;
(defun lisp-input-start ()
  "Go to the start of the input region."
  (let* ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (if (>= (point) pmark)
	(goto-char pmark)
	(progn 
	  (end-of-line)
	  (if (re-search-backward comint-prompt-regexp (point-min) 'stay)
	      (comint-skip-prompt)
	      (point))))))

(provide 'ilisp-inp)
