;;; Editing VM messages
;;; Copyright (C) 1990, 1991 Kyle E. Jones
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

(defun vm-edit-message (&optional prefix-argument)
  "Edit the current message.  Prefix arg means mark as unedited instead.
If editing, the current message is copied into a temporary buffer, and
this buffer is selected for editing.  The major mode of this buffer is
controlled by the variable `vm-edit-message-mode'.

Use C-c ESC when you have finished editing the message.  The message
will be inserted into its folder replacing the old version of the
message.  If you don't want your edited version of the message to
replace the original, use C-c C-]."
  (interactive "P")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (if prefix-argument
      (if (vm-edited-flag (car vm-message-pointer))
	  (progn
	    (vm-set-edited-flag (car vm-message-pointer) nil)
	    (vm-mark-for-display-update (car vm-message-pointer))
	    (if (eq vm-flush-interval t)
		(vm-stuff-virtual-attributes (car vm-message-pointer))
	      (vm-set-modflag-of (car vm-message-pointer) t))
	    (vm-set-buffer-modified-p t (vm-current-message-buffer))
	    (vm-update-summary-and-mode-line))
	(message "Message has not been edited."))
    (let ((mp vm-message-pointer)
	  (edit-buf (vm-edit-buffer-of (car vm-message-pointer)))
	  (folder-buffer (current-buffer))
	  (inhibit-quit t))
      (if (not (and edit-buf (buffer-name edit-buf)))
	  (progn
	    (vm-save-restriction
	      (widen)
	      (setq edit-buf (generate-new-buffer "*VM message edit*"))
	      (vm-set-edit-buffer-of (car mp) edit-buf)
	      (copy-to-buffer edit-buf
			      (save-excursion
				(goto-char (vm-start-of (car mp)))
				(forward-line 1)
				(point))
			      (vm-text-end-of (car mp))))
	    (if (get-buffer-window edit-buf)
		(select-window (get-buffer-window edit-buf))
	      (switch-to-buffer edit-buf))
	    (set-buffer-modified-p nil)
	    (goto-char (point-min))
	    (search-forward "\n\n" (point-max) t)
	    (funcall (or vm-edit-message-mode 'text-mode))
	    (setq vm-message-pointer mp
		  vm-mail-buffer folder-buffer)
	    (use-local-map (copy-keymap (or (current-local-map)
					    (make-sparse-keymap))))
	    (vm-overlay-keymap vm-edit-message-mode-map (current-local-map))
	    (message "Type C-c ESC to end edit, C-c C-] to abort with no change."))
	(switch-to-buffer edit-buf)))))

(defun vm-overlay-keymap (src-map dest-map)
  (let ((old-local-map (current-local-map)))
    (unwind-protect
	(progn
	  (use-local-map dest-map)
	  (cond
	   ((vectorp src-map)
	    (let ((i (1- (length src-map))) src-b dest-b)
	      (while (>= i 0)
		(setq src-b (aref src-map i))
		(cond
		 ((null src-b))
		 ((keymapp src-b)
		  (setq dest-b (local-key-binding (char-to-string i)))
		  (if (not (keymapp dest-b))
		      (define-key dest-map (char-to-string i)
			(setq dest-b (make-sparse-keymap))))
		  (vm-overlay-keymap src-b dest-b))
		 (t
		  (define-key dest-map (char-to-string i) src-b)))
		(vm-decrement i))))
	   ((consp src-map)
	    (let (src-b dest-b)
	      (setq src-map (cdr src-map))
	      (while src-map
		(setq src-b (cdr (car src-map)))
		(cond
		 ((null src-b))
		 ((keymapp src-b)
		  (setq dest-b (local-key-binding
				(char-to-string (car (car src-map)))))
		  (if (not (keymapp dest-b))
		      (define-key dest-map (char-to-string (car (car src-map)))
			(setq dest-b (make-sparse-keymap))))
		  (vm-overlay-keymap src-b dest-b))
		 (t
		  (define-key dest-map (char-to-string (car (car src-map)))
		    src-b)))
		(setq src-map (cdr src-map)))))
	   ((fboundp 'map-keymap)
	    (map-keymap
	     (function
	      (lambda (key src-b)
		(cond ((keymapp src-b)
		       (setq dest-b (local-key-binding (vector key)))
		       (if (not (keymapp dest-b))
			   (define-key dest-map key
			     (setq dest-b (make-sparse-keymap))))
		       (vm-overlay-keymap src-b dest-b))
		      (t
		       (define-key dest-map key src-b)))))
	     src-map))))
      (use-local-map old-local-map))))
				
(defun vm-discard-cached-data (&optional count)
  "Discard cached information about the current message.
When VM digs information from the headers of a message, it stores it
iunternally for future reference.  This command causes VM to forget this
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
  ;; Do this in case the user is using this command because the
  ;; variables that control visible headers have been altered.
  (vm-build-visible-header-alist)
  (let ((mlist (vm-select-marked-or-prefixed-messages count)))
    (while mlist
      (vm-set-cache-of (car mlist)
		       (make-vector
			(length (vm-cache-of (car mlist)))
			nil ))
      (vm-set-vheaders-of (car mlist) nil)
      (vm-set-vheaders-regexp-of (car mlist) nil)
      (vm-set-text-of (car mlist) nil)
      (vm-mark-for-display-update (car mlist))
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
	(let ((inhibit-quit t))
	  (save-excursion
	    (set-buffer (marker-buffer (vm-start-of (car mp))))
	    (if (not (memq (car mp) vm-message-list))
		(error "The original copy of this message has been expunged."))
	    (vm-save-restriction
	     (widen)
	     (goto-char (vm-start-of (car mp)))
	     (forward-line 1)
	     (let ((vm-message-pointer mp)
		   vm-next-command-uses-marks
		   buffer-read-only)
	       (insert-buffer-substring edit-buf)
	       (and (/= (preceding-char) ?\n) (insert ?\n))
	       (delete-region (point) (vm-text-end-of (car mp)))
	       (vm-discard-cached-data))
	     (vm-set-edited-flag (car mp) t)
	     (vm-mark-for-display-update (car mp))
	     (if (eq vm-flush-interval t)
		 (vm-stuff-virtual-attributes (car mp))
	       (vm-set-modflag-of (car mp) t))
	     (vm-set-buffer-modified-p t)
	     (vm-clear-modification-flag-undos)
	     (vm-set-edit-buffer-of (car mp) nil))
	    (if (eq mp vm-message-pointer)
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
  (if (null (buffer-name (marker-buffer (vm-end-of (car vm-message-pointer)))))
      (error "The folder buffer for this message has been killed."))
  (vm-set-edit-buffer-of (car vm-message-pointer) nil)
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer))
  (message "Aborted, no change."))
