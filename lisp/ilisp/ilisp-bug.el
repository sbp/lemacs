;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-bug.el,v 1.19 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP bug stuff.
;;;

;;;
;;;%Bugs
(defun ilisp-bug ()
  "Generate an ilisp bug report."
  (interactive)
  (let ((buffer 
	 (if (y-or-n-p 
	      (format "Is %s the buffer where the error occurred? " 
		      (buffer-name (current-buffer))))
	     (current-buffer))))
    (if (or (not buffer) (not (mail)))
	(progn
	  (message 
	   (if buffer 
	       "Can't send bug report until mail buffer is empty."
	       "Switch to the buffer where the error occurred."))
	  (beep))
      (insert ilisp-bugs-to)
      (search-forward (concat "\n" mail-header-separator "\n"))
      (insert "\nYour problem: \n\n")
      (insert "Type C-c C-c to send\n")
      (insert "======= Emacs state below: for office use only =======\n")
      (forward-line 1)
      (insert (emacs-version))
      (insert 
       (format "\nWindow System: %s %s" window-system window-system-version))
      (let ((mode (save-excursion (set-buffer buffer) major-mode))
	    (match "popper-\\|completer-")
	    (val-buffer buffer)
	    string)
	(if (or (memq mode lisp-source-modes) (memq mode ilisp-modes))
	    (progn
	      (setq match (concat "ilisp-\\|comint-\\|lisp-" match)
		    val-buffer (save-excursion (set-buffer buffer)
					       (or (ilisp-buffer) buffer)))
	      (mapcar (function (lambda (dialect)
				  (setq match (concat (format "%s-\\|" (car dialect))
						      match))))
		      ilisp-dialects)
	      (save-excursion
		(set-buffer buffer)
		(let ((point (point))
		      (start (lisp-defun-begin))
		      (end (lisp-end-defun-text t)))
		  (setq string
			(format "
Mode: %s
Start: %s
End: %s
Point: %s
Point-max: %s
Code: %s"
				major-mode start end point (point-max)
				(buffer-substring start end)))))
	      (insert string)))
	(mapatoms
	 (function (lambda (symbol)
		     (if (and (boundp symbol)
			      (string-match match (format "%s" symbol))
			      (not (eq symbol 'ilisp-documentation)))
			 (let ((val (save-excursion
				      (set-buffer val-buffer) 
				      (symbol-value symbol))))
			   (if val
			       (insert (format "\n%s: %s" symbol val))))))))
	(insert (format "\nLossage: %s" (key-description (recent-keys))))
	(if (and (or (memq mode lisp-source-modes)
		     (memq mode ilisp-modes))
		 (ilisp-buffer) 
		 (memq 'clisp (ilisp-value 'ilisp-dialect t))
		 (not (cdr (ilisp-value 'comint-send-queue))))
	    (progn
	      (insert (format "\nLISP: %s"
			      (comint-remove-whitespace
			       (car (comint-send
				     (save-excursion
				       (set-buffer buffer)
				       (ilisp-process))
				     "(lisp-implementation-version)"
				     t t 'version)))))
	      (insert (format "\n*FEATURES*: %s"
			      (comint-remove-whitespace
			       (car (comint-send
				     (save-excursion
				       (set-buffer buffer)
				       (ilisp-process))
				     "(let ((*print-length* nil)
				       (*print-level* nil))
				   (print *features*)
				   nil)"
				     t t 'version)))))))
	(insert ?\n)
	(goto-char (point-min))
	(re-search-forward "^Subject")
	(end-of-line)
	(message "Send with sendmail or your favorite mail program.")))))

(provide 'ilisp-bug)
