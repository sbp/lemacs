;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-kil.el,v 1.18 1993/09/03 02:05:07 ivan Rel $
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
;;; ILISP Panic/Reset/Status commands
;;;

;;;%% Panic/Reset/Status commands 
;;;
(defun status-lisp (showp)
  "Show the message of the current command being executed in the
inferior LISP.  With a prefix show pending sends as well."  
  (interactive "P")
  (save-excursion
    (set-buffer (ilisp-buffer))
    (comint-current-send showp)))


;;;
(defun reset-ilisp ()
  "Reset the inferior LISP top level."
  (interactive)
  (message "Reset LISP to top level")
  (comint-simple-send (ilisp-process) (ilisp-value 'ilisp-reset)))

;;;
(defun abort-commands-lisp (&optional message)
  "Abort the commands sent to the current ilisp."
  (interactive)
  (if (ilisp-value comint-aborting t)
      (message "Already aborted commands")
      (beep)
      (message (or message "Aborted commands"))
      (comint-abort-sends (ilisp-process))))

;;;
(defun panic-lisp ()
  "Panic reset for the inferior LISP."
  (interactive)
  (save-excursion
    (if (y-or-n-p "Panic reset LISP? ")
	(save-excursion
	  (set-buffer (ilisp-buffer))
	  (comint-setup-ipc t)
	  (message "LISP is reset, state is unknown"))
	(message ""))))

;;;
(defun interrupt-subjob-ilisp ()
  "Interrupt the current top level command in the inferior LISP."
  (interactive)
  (if (not (eq comint-send-queue comint-end-queue))
      (if (y-or-n-p "Abort commands before interrupting top level? ")
	  (abort-commands-lisp)
	  (message "Waiting for commands to finish")
	  (while (not (eq comint-send-queue comint-end-queue))
	    (accept-process-output)
	    (sit-for 0))))
  (message "Interrupted top level")
  (comint-interrupt-subjob))



(provide 'ilisp-kil)
