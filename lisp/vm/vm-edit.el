;;; Editing VM messages
;;; Copyright (C) 1990, 1991, 1993 Kyle E. Jones
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

(defun vm-edit-message (&optional prefix-argument)
  "Edit the current message.  Prefix arg means mark as unedited instead.
If editing, the current message is copied into a temporary buffer, and
this buffer is selected for editing.  The major mode of this buffer is
controlled by the variable `vm-edit-message-mode'.

Use C-c ESC when you have finished editing the message.  The message
will be inserted into its folder replacing the old version of the
message.  If you don't want your edited version of the message to
replace the original, use C-c C-] and the edit will be aborted."
  (interactive "P")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (if (and (vm-virtual-message-p (car vm-message-pointer))
	   (null (vm-virtual-messages-of (car vm-message-pointer))))
      (error "Can't edit unmirrored virtual messages."))
  (if prefix-argument
      (if (vm-edited-flag (car vm-message-pointer))
	  (progn
	    (vm-set-edited-flag (car vm-message-pointer) nil)
	    (vm-mark-for-summary-update (car vm-message-pointer))
	    (if (eq vm-flush-interval t)
		(vm-stuff-virtual-attributes (car vm-message-pointer))
	      (vm-set-modflag-of (car vm-message-pointer) t))
	    (vm-update-summary-and-mode-line))
	(message "Message has not been edited."))
    (let ((mp vm-message-pointer)
	  (edit-buf (vm-edit-buffer-of (car vm-message-pointer)))
	  (folder-buffer (current-buffer)))
      (if (not (and edit-buf (buffer-name edit-buf)))
	  (progn
	    (vm-save-restriction
	      (widen)
	      (setq edit-buf
		    (generate-new-buffer
		     (format "edit of %s subj %s"
			     (vm-su-message-id (car vm-message-pointer))
			     (vm-su-subject (car vm-message-pointer)))))
	      (vm-set-edit-buffer-of (car mp) edit-buf)
	      (copy-to-buffer edit-buf
			      (vm-headers-of (car mp))
			      (vm-text-end-of (car mp))))
	    (if (get-buffer-window edit-buf)
		(select-window (get-buffer-window edit-buf))
	      (switch-to-buffer edit-buf))
	    (set-buffer-modified-p nil)
	    (goto-char (point-min))
	    (search-forward "\n\n" (point-max) t)
	    (funcall (or vm-edit-message-mode 'text-mode))
	    (use-local-map vm-edit-message-map)
	    ;; (list (car mp)) because a different message may
	    ;; stuffed into a cons linked into the folder's
	    ;; message list.
	    (setq vm-message-pointer (list (car mp))
		  vm-mail-buffer folder-buffer)
	    (run-hooks 'vm-edit-message-hook)
	    (message 
	     (substitute-command-keys
	      "Type \\[vm-edit-message-end] to end edit, \\[vm-edit-message-abort] to abort with no change."))))
      (or (vm-set-window-configuration 'editing-message)
	  (switch-to-buffer edit-buf)))))

(defun vm-discard-cached-data (&optional count)
  "Discard cached information about the current message.
When VM gathers information from the headers of a message, it stores it
internally for future reference.  This command causes VM to forget this
information, and VM will be forced to search the headers of the message
again for these data.  VM will also have to decide again which headers
should be displayed and which should not.  Therefore this command is
useful if you change the value of vm-visible-headers or
vm-invisible-headers in the midst of a VM session.

Numeric prefix argument N means to discard data from the current message
plus the next N-1 messages.  A negative N means discard data from the
current message and the previous N-1 messages.

When invoked on marked messages (via vm-next-command-uses-marks),
data is discarded only from the marked messages in the current folder."
  (interactive "p")
  (or count (setq count 1))
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((mlist (vm-select-marked-or-prefixed-messages count)) m)
    (while mlist
      (setq m (vm-real-message-of (car mlist)))
      (vm-set-cache-of m
		       (make-vector
			(length (vm-cache-of m))
			nil ))
      (vm-set-vheaders-of m nil)
      (vm-set-vheaders-regexp-of m nil)
      (vm-set-text-of m nil)
      (vm-mark-for-summary-update m)
      (setq mlist (cdr mlist))))
  (vm-update-summary-and-mode-line))

(defun vm-edit-message-end ()
  "End the edit of a VM mail message and copy the new version
to the message's folder."
  (interactive)
  (if (null vm-message-pointer)
      (error "This is not a VM message edit buffer."))
  (if (null (buffer-name (marker-buffer (vm-end-of (car vm-message-pointer)))))
      (error "The folder buffer for this message has been killed."))
  (let ((edit-buf (current-buffer))
	(mp vm-message-pointer))
    (if (buffer-modified-p)
	(progn
	  (widen)
	  (save-excursion
	    (set-buffer (marker-buffer (vm-start-of (vm-real-message-of (car mp)))))
	    (if (not (memq (vm-real-message-of (car mp)) vm-message-list))
		(error "The original copy of this message has been expunged."))
	    (vm-save-restriction
	     (widen)
	     (goto-char (vm-headers-of (vm-real-message-of (car mp))))
	     (let ((vm-message-pointer mp)
		   opoint
		   (buffer-read-only nil))
	       (setq opoint (point))
	       (insert-buffer-substring edit-buf)
	       (and (/= (preceding-char) ?\n) (insert ?\n))
	       (delete-region (point) (vm-text-end-of (vm-real-message-of (car mp))))
	       ;; vm-message-pointer is set above so that the
	       ;; correct message gets its cache flushed
	       (vm-discard-cached-data))
	     (vm-set-edited-flag (car mp) t)
	     (vm-mark-for-summary-update (car mp))
	     (if (eq vm-flush-interval t)
		 (vm-stuff-virtual-attributes (car mp))
	       (vm-set-modflag-of (car mp) t))
	     (vm-set-buffer-modified-p t)
	     (vm-clear-modification-flag-undos)
	     (vm-set-edit-buffer-of (car mp) nil))
	    (if (eq (car mp) (car vm-message-pointer))
		(vm-preview-current-message)
	      (vm-update-summary-and-mode-line))))
      (message "No change."))
    (set-buffer-modified-p nil)
    (kill-buffer edit-buf)))

(defun vm-edit-message-abort ()
  "Abort editing of a VM message, without updating the message's folder."
  (interactive)
  (if (null vm-message-pointer)
      (error "This is not a VM message edit buffer."))
  (if (null (buffer-name (marker-buffer (vm-start-of (vm-real-message-of (car vm-message-pointer))))))
      (error "The folder buffer for this message has been killed."))
  (vm-set-edit-buffer-of (car vm-message-pointer) nil)
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer))
  (message "Aborted, no change."))
