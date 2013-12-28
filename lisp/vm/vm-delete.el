;;; Delete and expunge commands for VM.
;;; Copyright (C) 1989, 1990, 1991 Kyle E. Jones
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

(require 'vm)

(defun vm-delete-message (count)
  "Add the `deleted' attribute to the current message.

The message will be physically deleted from the current folder the next
time the current folder is expunged.

With a prefix argument, the next COUNT messages are deleted.  A negative
argument means the previous COUNT messages are deleted.

When invoked on marked messages (via vm-next-command-uses-marks),
all marked messages are deleted, other messages are ignored."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let* ((used-marks (eq last-command 'vm-next-command-uses-marks))
	 (mlist (vm-select-marked-or-prefixed-messages count)))
    (while mlist
      (if (not (vm-deleted-flag (car mlist)))
	  (vm-set-deleted-flag (car mlist) t))
      (setq mlist (cdr mlist)))
    (vm-update-summary-and-mode-line)
    (if (and vm-move-after-deleting (not used-marks))
	(vm-next-message count t executing-kbd-macro))))

(defun vm-delete-message-backward (count)
  "Like vm-delete-message, except that the deletion direction is reversed."
  (interactive "p")
  (let ((prefix-arg (- count)))
    (command-execute 'vm-delete-message)))

(defun vm-undelete-message (count)
  "Remove the `deleted' attribute from the current message.

With a prefix argument, the next COUNT messages are undeleted.  A
negative argument means the previous COUNT messages are undeleted.

When invoked on marked messages (via vm-next-command-uses-marks),
all marked messages are undeleted, other messages are ignored."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let* ((used-marks (eq last-command 'vm-next-command-uses-marks))
	 (mlist (vm-select-marked-or-prefixed-messages count)))
    (while mlist
      (if (vm-deleted-flag (car mlist))
	  (vm-set-deleted-flag (car mlist) nil))
      (setq mlist (cdr mlist)))
    (vm-update-summary-and-mode-line)
    (if (and vm-move-after-undeleting (not used-marks))
	(vm-next-message count t executing-kbd-macro))))

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
	(n 0))
    (if (string-match "^\\(re: *\\)+" subject)
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
  "Expunge deleted messages, but don't save folder to disk or exit VM."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-referenced-virtually)
  (if (not shaddap)
      (message "Expunging..."))
  (let ((inhibit-quit t) did-gobble)
    (if (setq did-gobble (vm-gobble-deleted-messages))
	(progn
	  (setq vm-numbering-redo-start-point did-gobble
		vm-summary-redo-start-point did-gobble
		vm-totals nil)
	  (if (not quitting)
	      (progn
		(if (not shaddap)
		    (vm-deferred-message "Deleted messages expunged."))
		(if (null vm-message-pointer)
		    (if (null vm-message-list)
			(vm-update-summary-and-mode-line)
		      (vm-next-message))
		  (if (null vm-system-state)
		      (vm-preview-current-message)
		    (vm-update-summary-and-mode-line))))))
      (error "No messages are flagged for deletion."))))

;; Remove any message marked for deletion from the buffer and the
;; message list.
(defun vm-gobble-deleted-messages ()
  (save-excursion
    (vm-save-restriction
     (widen)
     (let ((mp vm-message-list)
	   (old-message-list vm-message-list)
	   virtual prev buffer-read-only tail-cons did-gobble)
       (setq virtual
	     (and mp (not (eq (car mp) (vm-real-message-of (car mp))))))
       (while mp
	 (if (not (vm-deleted-flag (car mp)))
	     (setq prev mp)
	   (or did-gobble (setq did-gobble (or prev vm-message-list)))
	   (or virtual (delete-region (vm-start-of (car mp))
				      (vm-end-of (car mp))))
	   (if (null prev)
	       (progn (setq vm-message-list (cdr vm-message-list))
		      (vm-set-reverse-link-of (car mp) nil))
	     (setcdr prev (cdr mp))
	     (and (cdr mp) (vm-set-reverse-link-of (car (cdr mp)) prev))))
	 (setq mp (cdr mp)))
       (if (and did-gobble (eq did-gobble old-message-list))
	   (setq did-gobble t))
       (if did-gobble
	   (progn
	     (vm-clear-expunge-invalidated-undos)
	     (vm-set-buffer-modified-p t)
	     (setq vm-message-order-changed vm-message-order-stuffed)
	     (cond ((and vm-last-message-pointer
			 (vm-deleted-flag (car vm-last-message-pointer)))
		    (setq vm-last-message-pointer nil)))
	     (cond ((and vm-message-pointer
			 (vm-deleted-flag (car vm-message-pointer)))
		    (setq vm-system-state nil)
		    (setq mp (cdr vm-message-pointer))
		    (while (and mp (vm-deleted-flag (car mp)))
		      (setq mp (cdr mp)))
		    (setq vm-message-pointer
			  (or mp (vm-last vm-message-list)))))
	     did-gobble ))))))
