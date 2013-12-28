;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-hnd.el,v 1.18 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP Error handler
;;;


;;;
(defun ilisp-handler (error-p wait-p message output prompt)
  "Given ERROR-P, WAIT-P, MESSAGE, OUTPUT and PROMPT, show the message
and output if there is an error or the output is multiple lines and
let the user decide what to do."
  (if lisp-no-popper
      (progn
	(if message
	    (progn
	      (setq ilisp-last-message message
		    ilisp-last-prompt prompt)
	      (if (not wait-p) (lisp-display-output output))))
	nil)
    (if (and (not wait-p)
	     (setq output (comint-remove-whitespace output))
	     (or error-p (string-match "\n" output)))
	(let* ((buffer (popper-output-buffer))
	       (out (if error-p 
			(funcall ilisp-error-filter output)
		      output))
	       (key
		(if (and error-p (not (comint-interrupted)))
		    (comint-handle-error
		     out
     "SPC-scroll, I-ignore, K-keep, A-abort sends and keep or B-break: "
		     '(?i ?k ?a ?b))
		  (comint-handle-error 
		   out 
	   "SPC-scroll, I-ignore, K-keep or A-abort sends and keep: "
		   '(?i ?k ?a))))
	       (clear comint-queue-emptied))
	  (if (= key ?i)
	      (progn
		(message "Ignore message")
		(if buffer 
		    (funcall
		     (ilisp-temp-buffer-show-function)
		     buffer)
		  (popper-bury-output))
		t)
	    (save-excursion
	      (set-buffer (get-buffer-create "*Errors*"))
	      (if clear (delete-region (point-min) (point-max)))
	      (goto-char (point-max))
	      (insert message)
	      (insert ?\n)
	      (insert out) 
	      (insert "\n\n"))
	    (if clear (setq comint-queue-emptied nil))
	    (if (= key ?a)
		(progn 
		  (message "Abort pending commands and keep in *Errors*")
		  (comint-abort-sends)
		  t)
	      (if (= key ?b)
		  (progn 
		    (comint-insert
		     (concat comment-start comment-start comment-start
			     message "\n"
			     output "\n" prompt))
		    (message "Preserve break") nil)
		(message "Keep error in *Errors* and continue")
		t))))
      t)))

;;;
(defun ilisp-abort-handler ()
  "Handle when the user aborts commands."
  (setq ilisp-initializing nil
	ilisp-load-files nil)
  (let ((add nil))
    (while ilisp-pending-changes
      (if (not (memq (car ilisp-pending-changes) lisp-changes))
	  (setq add (cons (car ilisp-pending-changes) add)))
      (setq ilisp-pending-changes (cdr ilisp-pending-changes)))
    (setq lisp-changes (nconc lisp-changes add))))

(provide 'ilisp-hnd )
