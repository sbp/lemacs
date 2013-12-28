;;; Commands to move around in a VM folder
;;; Copyright (C) 1989, 1990 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(defun vm-record-and-change-message-pointer (old new)
  (sets-set-insert vm-buffers-needing-display-update (current-buffer))
  (setq vm-last-message-pointer old
	vm-message-pointer new
	vm-need-summary-pointer-update t))

(defun vm-goto-message (n)
  "Go to the message numbered N.
Interactively N is the prefix argument.  If no prefix arg is provided
N is prompted for in the minibuffer.

If vm-follow-summary-cursor is non-nil this command first tries
to follow the summary cursor to a new message.  If a new message
is selected in this way, no further action is taken.  I.e. you can move
the cursor in the summary buffer, press RETURN and select a new
message without typing in a message number."
  (interactive
   (list
    (cond ((vm-follow-summary-cursor) nil)
	  (current-prefix-arg (prefix-numeric-value current-prefix-arg))
	  (t (vm-read-number "Go to message: ")))))
  (if (null n)
      ()				; nil means work has been done already
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (let ((cons (nthcdr (1- n) vm-message-list)))
      (if (null cons)
	  (error "No such message."))
      (if (eq vm-message-pointer cons)
	  (vm-preview-current-message)
	(vm-record-and-change-message-pointer vm-message-pointer cons)
	(setq vm-need-summary-pointer-update t)
	(vm-preview-current-message)))))

(defun vm-goto-message-last-seen ()
  "Go to the message last previewed."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if vm-last-message-pointer
      (progn
	(vm-record-and-change-message-pointer vm-message-pointer
					      vm-last-message-pointer)
	(setq vm-need-summary-pointer-update t)
	(vm-preview-current-message))))

(put 'beginning-of-folder 'error-conditions '(beginning-of-folder error))
(put 'beginning-of-folder 'error-message "Beginning of folder")
(put 'end-of-folder 'error-conditions '(end-of-folder error))
(put 'end-of-folder 'error-message "End of folder")

(defun vm-check-count (count)
  (if (>= count 0)
      (if (< (length vm-message-pointer) count)
	  (signal 'end-of-folder nil))
    (if (< (1+ (- (length vm-message-list) (length vm-message-pointer)))
	   (vm-abs count))
	(signal 'beginning-of-folder nil))))

(defun vm-move-message-pointer (direction)
  (let ((mp vm-message-pointer))
    (if (eq direction 'forward)
	(progn
	  (setq mp (cdr mp))
	  (if (null mp)
	      (if vm-circular-folders
		  (setq mp vm-message-list)
		(signal 'end-of-folder nil))))
      (setq mp (vm-reverse-link-of (car mp)))
      (if (null mp)
	  (if vm-circular-folders
	      (setq mp (vm-last vm-message-list))
	    (signal 'beginning-of-folder nil))))
    (setq vm-message-pointer mp)))

(defun vm-should-skip-message (mp &optional skip-dogmatically)
  (if skip-dogmatically
      (or (and vm-skip-deleted-messages
	       (vm-deleted-flag (car mp)))
	  (and vm-skip-read-messages
	       (or (vm-deleted-flag (car mp))
		   (not (or (vm-new-flag (car mp))
			    (vm-unread-flag (car mp))))))
	  (and (eq last-command 'vm-next-command-uses-marks)
	       (null (vm-mark-of (car mp)))))
    (or (and (eq vm-skip-deleted-messages t)
	     (vm-deleted-flag (car mp)))
	(and (eq vm-skip-read-messages t)
	     (or (vm-deleted-flag (car mp))
		 (not (or (vm-new-flag (car mp))
			  (vm-unread-flag (car mp))))))
	(and (eq last-command 'vm-next-command-uses-marks)
	     (null (vm-mark-of (car mp)))))))

(defun vm-next-message (&optional count retry signal-errors)
  "Go forward one message and preview it.
With prefix arg (optional first argument) COUNT, go forward COUNT
messages.  A negative COUNT means go backward.  If the absolute
value of COUNT is greater than 1, then the values of the variables
vm-skip-deleted-messages and vm-skip-read-messages are ignored.

When invoked on marked messages (via vm-next-command-uses-marks)
this command 'sees' marked messages as it moves."
  ;; second arg RETRY non-nil means retry a failed move, giving
  ;; not nil-or-t values of the vm-skip variables a chance to
  ;; work.
  ;;
  ;; third arg SIGNAL-ERRORS non-nil means that if after
  ;; everything we still have bashed into the end or beginning of
  ;; folder before completing the move, signal
  ;; beginning-of-folder or end-of-folder.  Otherwise no error
  ;; will be signaled.
  ;;
  ;; Note that interactively all args are 1, so error signaling
  ;; and retries apply to all interactive moves.
  (interactive "p\np\np")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (and signal-errors (vm-error-if-folder-empty))
  (or count (setq count 1))
  (let ((oldmp vm-message-pointer)
	(use-marks (eq last-command 'vm-next-command-uses-marks))
	(error)
	(direction (if (> count 0) 'forward 'backward))
	(count (vm-abs count)))
    (cond
     ((null vm-message-pointer)
      (setq vm-message-pointer vm-message-list))
     ((/= count 1)
      (condition-case ()
	  (let ((oldmp oldmp))
	    (while (not (zerop count))
	      (vm-move-message-pointer direction)
	      (if (and use-marks (null (vm-mark-of (car vm-message-pointer))))
		  (progn
		    (while (and (not (eq vm-message-pointer oldmp))
				(null (vm-mark-of (car vm-message-pointer))))
		      (vm-move-message-pointer direction))
		    (if (eq vm-message-pointer oldmp)
			;; terminate the loop
			(setq count 1)
		      ;; reset for next pass
		      (setq oldmp vm-message-pointer))))
	      (vm-decrement count)))
	(beginning-of-folder (setq error 'beginning-of-folder))
	(end-of-folder (setq error 'end-of-folder))))
     (t
      (condition-case ()
	  (progn
	    (vm-move-message-pointer direction)
	    (while (and (not (eq oldmp vm-message-pointer))
			(vm-should-skip-message vm-message-pointer t))
	      (vm-move-message-pointer direction))
	    ;; Retry the move if we've gone a complete circle and
	    ;; retries are allowed and there are other messages
	    ;; besides this one.
	    (and (eq vm-message-pointer oldmp) retry (cdr vm-message-list)
		 (progn
		   (while (and (not (eq oldmp vm-message-pointer))
			       (vm-should-skip-message vm-message-pointer))
		     (vm-move-message-pointer direction)))))
	(beginning-of-folder
	 ;; we bumped into the beginning of the folder without finding
	 ;; a suitable stopping point; retry the move if we're allowed.
	 (setq vm-message-pointer oldmp)
	 ;; if the retry fails, we make sure the message pointer
	 ;; is restored to its old value.
	 (if retry
	     (setq vm-message-pointer
		   (condition-case ()
		       (let ((vm-message-pointer vm-message-pointer))
			 (vm-move-message-pointer direction)
			 (while (vm-should-skip-message vm-message-pointer)
			   (vm-move-message-pointer direction))
			 vm-message-pointer )
		     (beginning-of-folder
		      (setq error 'beginning-of-folder)
		      oldmp )))
	   (setq error 'beginning-of-folder)))
	(end-of-folder
	 ;; we bumped into the end of the folder without finding
	 ;; a suitable stopping point; retry the move if we're allowed.
	 (setq vm-message-pointer oldmp)
	 ;; if the retry fails, we make sure the message pointer
	 ;; is restored to its old value.
	 (if retry
	     (setq vm-message-pointer
		   (condition-case ()
		       (let ((vm-message-pointer vm-message-pointer))
			 (vm-move-message-pointer direction)
			 (while (vm-should-skip-message vm-message-pointer)
			   (vm-move-message-pointer direction))
			 vm-message-pointer )
		     (end-of-folder
		      (setq error 'end-of-folder)
		      oldmp )))
	   (setq error 'end-of-folder))))))
    (if (not (eq vm-message-pointer oldmp))
	(progn
	  (vm-record-and-change-message-pointer oldmp vm-message-pointer)
	  (setq vm-need-summary-pointer-update t)
	  (vm-preview-current-message)))
    (and error signal-errors
	 (signal error nil))))

(defun vm-previous-message (&optional count retry signal-errors)
  "Go back one message and preview it.
With prefix arg COUNT, go backward COUNT messages.  A negative COUNT
means go forward.  If the absolute value of COUNT > 1 the values of the
variables vm-skip-deleted-messages and vm-skip-read-messages are
ignored."
  (interactive "p\np\np")
  (or count (setq count 1))
  (vm-select-folder-buffer)
  (vm-next-message (- count) retry signal-errors))

(defun vm-Next-message (&optional count)
  "Like vm-next-message but will not skip deleted or read messages."
  (interactive "p")
  (vm-select-folder-buffer)
  (let ((vm-skip-deleted-messages nil)
	(vm-skip-read-messages nil))
    (vm-next-message count nil t)))

(defun vm-Previous-message (&optional count)
  "Like vm-previous-message but will not skip deleted or read messages."
  (interactive "p")
  (vm-select-folder-buffer)
  (let ((vm-skip-deleted-messages nil)
	(vm-skip-read-messages nil))
    (vm-previous-message count)))

(defun vm-next-unread-message ()
  "Move forward to the nearest new or unread message, if there is one."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (condition-case ()
      (let ((vm-skip-read-messages t)
	    (oldmp vm-message-pointer))
	(vm-next-message 1 nil t)
	;; in case vm-circular-folder is non-nil
	(and (eq vm-message-pointer oldmp) (signal 'end-of-folder nil)))
    (end-of-folder (message "No next unread message"))))

(defun vm-previous-unread-message ()
  "Move backward to the nearest new or unread message, if there is one."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (condition-case ()
      (let ((vm-skip-read-messages t)
	    (oldmp vm-message-pointer))
	(vm-previous-message)
	;; in case vm-circular-folder is non-nil
	(and (eq vm-message-pointer oldmp) (signal 'beginning-of-folder nil)))
    (beginning-of-folder (message "No previous unread message"))))

(defun vm-find-first-unread-message (new)
  (let (mp unread-mp)
    (setq mp vm-message-list)
    (if new
	(while mp
	  (if (and (vm-new-flag (car mp)) (not (vm-deleted-flag (car mp))))
	      (setq unread-mp mp mp nil)
	    (setq mp (cdr mp))))
      (while mp
	(if (and (or (vm-new-flag (car mp)) (vm-unread-flag (car mp)))
		 (not (vm-deleted-flag (car mp))))
	    (setq unread-mp mp mp nil)
	  (setq mp (cdr mp)))))
    unread-mp ))

(defun vm-thoughtfully-select-message ()
  (let ((new (and vm-jump-to-new-messages (vm-find-first-unread-message t)))
	(unread (and vm-jump-to-unread-messages
		     (vm-find-first-unread-message nil)))
	fix mp)
    (if (null vm-message-pointer)
	(setq fix vm-message-list))
    (setq mp (or new unread fix))
    (if (and mp (not (eq mp vm-message-pointer)))
	(progn
	  (vm-record-and-change-message-pointer vm-message-pointer mp)
	  mp )
      nil )))

