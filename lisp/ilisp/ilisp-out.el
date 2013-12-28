;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-out.el,v 1.19 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP output
;;;
;;;
;;;
(defun lisp-pop-to-buffer (buffer)
  "Like pop-to-buffer, but select a screen that buffer was shown in."
  (let ((ilisp-window (if ilisp-epoch-running
			  (epoch::get-buffer-window buffer)
			  (get-buffer-window buffer))))
    (if ilisp-window
	(select-window ilisp-window)
	;; It is not currently displayed, so find some place to display
	;; it.
	(if ilisp-epoch-running
	    ;; Select a screen that the buffer has been displayed in before
	    ;; or the current screen otherwise.
	    (epoch::select-screen
	     ;; allowed-screens in epoch 3.2, was called screens before that
	     (or (car (save-excursion
			(set-buffer buffer)
			(symbol-value 'allowed-screens)))
		 (epoch::current-screen))))
	(pop-to-buffer buffer)))
  (set-buffer buffer))

(defun lisp-display-output (output)
  "Display OUTPUT in the appropriate place:
If lisp-no-popper is T then display in lisp window.
If lisp-no-popper is 'message then display in message window if output
is one line or less.
if lisp-no-popper is Nil then the blasted popper will be used."
  (if output
      (progn
	(if (ilisp-value 'comint-errorp t)
	    (setq output (funcall (ilisp-value 'ilisp-error-filter)
				  output)))
	(cond ((null lisp-no-popper)
	       (comint-display-output output))
	      ((or (eq lisp-no-popper t)
		   (and (eq lisp-no-popper 'message)
			(string-match "\n" output)))
	       (let ((buffer (current-buffer))
		     (window (selected-window)))
		 (unwind-protect
		     (progn
		       (lisp-pop-to-buffer (ilisp-buffer))
		       (if (not (eq (current-buffer) buffer))
			   (setq ilisp-last-buffer buffer))
		       (comint-insert 
			(concat 
			 (if ilisp-last-message
			     (concat ";;; " ilisp-last-message "\n"))
			 (comint-remove-whitespace output)
			 "\n"
			 ilisp-last-prompt))
		       (setq ilisp-last-message nil))
		   (if (window-point window)
		       (progn (select-window window)
			      (set-buffer buffer))))))
	      ((eq lisp-no-popper 'message)
	       (message "%s" output))
	      (t
	       (error "Unknown value of lisp-no-popper: '%s" 
		      lisp-no-popper))))))

;;;
(defun switch-to-lisp (eob-p &optional ilisp-only)
  "If in an ILISP buffer, switch to the buffer that last switched to
an ILISP otherwise, switch to the current ILISP buffer.  With
argument, positions cursor at end of buffer.  If you don't want to
split windows, set pop-up-windows to NIL."
  (interactive "P")
  (if (and (not ilisp-only) ilisp-last-buffer 
	   (memq major-mode ilisp-modes))
      (lisp-pop-to-buffer ilisp-last-buffer)
      (if (not (memq major-mode ilisp-modes))
	  (setq ilisp-last-buffer (current-buffer)))
      (lisp-pop-to-buffer (ilisp-buffer))
      (cond (eob-p (goto-char (point-max))))))

(provide 'ilisp-out )
