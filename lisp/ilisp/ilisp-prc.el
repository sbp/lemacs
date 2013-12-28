;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-prc.el,v 1.18 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP process handling
;;;
;;;
(defun ilisp-process ()
  "Return the current ILISP process."
  (get-buffer-process (ilisp-buffer)))


;;;%Buffer and process selection
(defun ilisp-buffer ()
  "Return the current ILISP buffer."
  (if (memq major-mode ilisp-modes)
      (current-buffer)
    (let ((buffer 
	   (if ilisp-buffer 
	       (or (get-buffer ilisp-buffer)
		   (get-buffer
		    (setq ilisp-buffers
			  (lisp-del (substring ilisp-buffer 1 
					       (1- (length ilisp-buffer)))
				    ilisp-buffers 
				    (function (lambda (s1 s2)
						(string= s1 (car s2)))))
			  ilisp-buffer 
			  (format "*%s*" (car (car ilisp-buffers)))))))))
      (or buffer
	  (error "You must start an inferior LISP with run-ilisp.")))))

;;;
(defun select-ilisp ()
  "Select the current ILISP buffer."
  (interactive)
  (let ((new (completing-read
	      (if ilisp-buffer
		  (format "Buffer [%s]: "
			  (substring ilisp-buffer 1
				     (1- (length ilisp-buffer))))
		  "Buffer: ")
	      ilisp-buffers nil t)))
    (if (not (zerop (length new)))
	(setq ilisp-buffer (format "*%s*" new)))))

(provide 'ilisp-prc )
