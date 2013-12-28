;;; Delete and expunge commands for VM.
;;; Copyright (C) 1989, 1990, 1991, 1993 Kyle E. Jones
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

(defun vm-delete-message (count)
  "Add the `deleted' attribute to the current message.

The message will be physically deleted from the current folder the next
time the current folder is expunged.

With a prefix argument COUNT, the current message and the next
COUNT - 1 messages are deleted.  A negative argument means the
the current message and the previous COUNT - 1 messages are
deleted.

When invoked on marked messages (via vm-next-command-uses-marks),
only marked messages are deleted, other messages are ignored."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
	(mlist (vm-select-marked-or-prefixed-messages count)))
    (while mlist
      (if (not (vm-deleted-flag (car mlist)))
	  (vm-set-deleted-flag (car mlist) t))
      (setq mlist (cdr mlist)))
    (vm-update-summary-and-mode-line)
    (if (and vm-move-after-deleting (not used-marks))
	(let ((vm-circular-folders (and vm-circular-folders
					(eq vm-move-after-deleting t))))
	  (vm-next-message count t executing-kbd-macro)))))

(defun vm-delete-message-backward (count)
  "Like vm-delete-message, except that the deletion direction is reversed."
  (interactive "p")
  (let ((prefix-arg (- count)))
    (command-execute 'vm-delete-message)))

(defun vm-undelete-message (count)
  "Remove the `deleted' attribute from the current message.

With a prefix argument COUNT, the current message and the next
COUNT - 1 messages are undeleted.  A negative argument means the
the current message and the previous COUNT - 1 messages are
deleted.

When invoked on marked messages (via vm-next-command-uses-marks),
only marked messages are undeleted, other messages are ignored."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
	(mlist (vm-select-marked-or-prefixed-messages count)))
    (while mlist
      (if (vm-deleted-flag (car mlist))
	  (vm-set-deleted-flag (car mlist) nil))
      (setq mlist (cdr mlist)))
    (vm-update-summary-and-mode-line)
    (if (and vm-move-after-undeleting (not used-marks))
	(let ((vm-circular-folders (and vm-circular-folders
					(eq vm-move-after-undeleting t))))
	  (vm-next-message count t executing-kbd-macro)))))

(defun vm-kill-subject ()
  "Delete all messages with the same subject as the current message
\(ignoring re:'s)."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((subject (vm-su-subject (car vm-message-pointer)))
	(mp vm-message-list)
	(n 0)
	(case-fold-search t))
    ;; strip off re:'s
    ;; trying to avoid "\\(re: *\\)+" here since juxtaposing *
    ;; and + seems to make regex matching slow down under v19
    ;; Emacs.
    (while (string-match "^re: *" subject)
      (setq subject (substring subject (match-end 0))))
    (setq subject (concat "^\\(re: *\\)*" (regexp-quote subject) " *$"))
    (while mp
      (if (and (not (vm-deleted-flag (car mp)))
	       (string-match subject (vm-su-subject (car mp))))
	  (progn
	    (vm-set-deleted-flag (car mp) t)
	    (vm-increment n)))
      (setq mp (cdr mp)))
    (and (interactive-p)
       (if (zerop n)
	   (message "No messages deleted.")
	 (message "%d message%s deleted" n (if (= n 1) "" "s")))))
  (vm-update-summary-and-mode-line))

(defun vm-expunge-folder (&optional quitting shaddap)
  "Expunge messages that are flagged for deletion.
For normal folders this means that the deleted messages are
removed from the message list and the message contents are
removed from the folder buffer.

For virtual folders, messages are removed from the virtual
message list.  If virtual mirroring is in effect for the virtual
folder, the corresponding real messages are removed from real
message lists and their contents are removed from real folders.

When invoked on marked messages (via vm-next-command-uses-marks),
only marked and deleted messages are expunged, other messages are
ignored."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (if (not shaddap)
      (message "Expunging..."))
  (let ((mp (or (vm-select-marked-or-prefixed-messages 0) vm-message-list))
	(virtual (eq major-mode 'vm-virtual-mode))
	(buffers-altered (sets-make-set))
	buffer-read-only prev redo-start-point)
    (while mp
      (cond
       ((vm-deleted-flag (car mp))
	;; expunge from the virtual side first, removing all
	;; references to this message before actually removing
	;; the message itself.
	(cond
	 ((vm-virtual-messages-of (car mp))
	  (let (redo-start-point vms prev curr)
	    (setq vms (vm-virtual-messages-of (car mp)))
	    (while vms
	      (save-excursion
		(set-buffer (marker-buffer (vm-start-of (car vms))))
		(setq curr vm-message-list
		      prev nil)
		(while curr
		  (if (not (eq (car curr) (car vms)))
		      (setq prev curr
			    curr (cdr curr))
		    (sets-insert buffers-altered (current-buffer))
		    (or redo-start-point (setq redo-start-point (or prev t)))
		    (if (eq vm-message-pointer curr)
			(setq vm-system-state nil
			      vm-message-pointer (or prev (cdr curr))))
		    (if (eq vm-last-message-pointer curr)
			(setq vm-last-message-pointer nil))
		    ;; vm-clear-expunge-invalidated-undos uses
		    ;; this to recognize expunged messages.
		    ;; Since this stuff is mirrored we'll be
		    ;; setting this value multiple times if there
		    ;; are multiple virtual mesages referenced
		    ;; the underlying real message.  Harmless.
		    (vm-set-deleted-flag (car curr) 'expunged t)
		    (if (null prev)
			(progn
			  (setq vm-message-list (cdr vm-message-list))
			  (and (cdr curr)
			       (vm-set-reverse-link-of (cdr curr) nil)))
		      (setcdr prev (cdr curr))
		      (and (cdr curr)
			   (vm-set-reverse-link-of (cdr curr) prev))
		      (setq curr nil))))
		(if redo-start-point
		    (progn (vm-set-numbering-redo-start-point redo-start-point)
			   (vm-set-summary-redo-start-point redo-start-point)
			   (vm-set-buffer-modified-p t))))
	      (setq vms (cdr vms)))
	    (vm-set-virtual-messages-of (car mp) nil))))
	(if (eq vm-message-pointer mp)
	    (setq vm-system-state nil
		  vm-message-pointer (or prev (cdr mp))))
	(if (eq vm-last-message-pointer mp)
	    (setq vm-last-message-pointer nil))
	(sets-set-insert buffers-altered (current-buffer))
	(or redo-start-point (setq redo-start-point (or prev t)))
	(if (null prev)
	    (progn (setq vm-message-list (cdr vm-message-list))
		   (and (cdr mp) (vm-set-reverse-link-of (car mp) nil)))
	  (setcdr prev (cdr mp))
	  (and (cdr mp) (vm-set-reverse-link-of (car (cdr mp)) prev)))
	;; vm-clear-expunge-invalidated-undos uses this to recognize
	;; expunged messages.
	(vm-set-deleted-flag (car mp) 'expunged t)
	(or virtual
	    (vm-save-restriction
	     (widen)
	     (delete-region (vm-start-of (car mp)) (vm-end-of (car mp)))
	     (vm-increment vm-modification-counter)))
	(if redo-start-point
	    (progn (vm-set-numbering-redo-start-point redo-start-point)
		   (vm-set-summary-redo-start-point redo-start-point)
		   (vm-set-buffer-modified-p t))))
       (t (setq prev mp)))
      (setq mp (cdr mp)))
    (cond
     ((and (not quitting) buffers-altered)
      (save-excursion
	(sets-mapset
	 (function
	  (lambda (buffer)
	    (set-buffer buffer)
	    (if (null vm-system-state)
		(if (null vm-message-pointer)
		    ;; folder is now empty
		    (progn (setq vm-folder-type nil)
			   (vm-update-summary-and-mode-line))
		  (vm-preview-current-message))
	      (vm-update-summary-and-mode-line))
	    (if (not (eq major-mode 'vm-virtual-mode))
		(setq vm-message-order-changed vm-message-order-stuffed))
	    (vm-clear-expunge-invalidated-undos)))
	 buffers-altered))
      (if (not shaddap)
	  (message "Deleted messages expunged.")))
     (t (error "No messages are flagged for deletion.")))))
