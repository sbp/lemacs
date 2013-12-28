;;; Virtual folders
;;; Copyright (C) 1990 Kyle E. Jones
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

(defun vm-visit-virtual-folder (folder-name &optional read-only)
  (interactive
   (progn
     (vm-session-initialization)
     (list
      (completing-read "Visit virtual folder: " vm-virtual-folder-alist nil t)
      current-prefix-arg)))
  (vm-session-initialization)
  (if (not (assoc folder-name vm-virtual-folder-alist))
      (error "No such virtual folder, %s" folder-name))
  (let ((buffer-name (concat folder-name " virtual"))
	first-time)
    (set-buffer (get-buffer-create buffer-name))
    (setq first-time (not (eq major-mode 'vm-virtual-mode)))
    (if first-time
	(progn
	  (buffer-disable-undo (current-buffer))
	  (abbrev-mode 0)
	  (auto-fill-mode 0)
	  (setq mode-name "VM Virtual"
		mode-line-format vm-mode-line-format
		buffer-read-only t
		vm-folder-read-only read-only
		truncate-lines t
		vm-current-grouping vm-group-by
		vm-numbering-redo-start-point t
		vm-summary-redo-start-point t)
	  (vm-build-virtual-message-list
	   (assoc folder-name vm-virtual-folder-alist))
	  (use-local-map vm-mode-map)
	  ;; save this for last in case the user interrupts.
	  ;; an interrupt anywhere before this point will cause
	  ;; everything to be redone next revisit.
	  (setq major-mode 'vm-virtual-mode)
	  (vm-thoughtfully-select-message)
	  (vm-emit-totals-blurb)))
    (switch-to-buffer (current-buffer))
    (and (not vm-inhibit-startup-message) (not vm-startup-message-displayed)
	 (vm-display-startup-message))))

(defun vm-build-virtual-message-list (def)
  (let ((clauses (cdr def))
	;; letter bomb protection
	;; set enable-local-variables to nil for newer Emacses
	(inhibit-local-variables t)
	(enable-local-variables nil)
	(vbuffer (current-buffer))
	message-list mp folders folder selector-list selector arg
	real-buffers-used)
    (save-excursion
      (while clauses
	(setq folders (car (car clauses))
	      selector-list (cdr (car clauses)))
	(while selector-list
	  (setq selector
		(intern
		 (concat "vm-vs-" (symbol-name (car (car selector-list))))))
	  (if (cdr (car selector-list))
	      (setq arg (car (cdr (car selector-list))))
	    (setq arg nil))
	  (while folders
	    (setq folder (car folders))
	    (while
		(not
		 (equal folder
			(setq folder
			      (expand-file-name folder vm-folder-directory)))))
	    (if (file-directory-p folder)
		(setq folders (nconc (cdr folders)
				     (vm-delete-directories
				      (directory-files folder t nil))))
	      (set-buffer (or (get-file-buffer folder)
			      (find-file-noselect folder)))
	      (if (not (memq vbuffer vm-virtual-buffers))
		  (setq vm-virtual-buffers (cons vbuffer vm-virtual-buffers)
			real-buffers-used (cons (current-buffer) real-buffers-used)))
	      (if (not (eq major-mode 'vm-mode))
		  (vm-mode))
	      (setq mp vm-message-list)
	      (while mp
		(if (if arg
			(funcall selector (car mp) arg)
		      (funcall selector (car mp)))
		    (progn
		      (setq message-list (cons (copy-sequence (car mp))
					       message-list))
		      (if vm-virtual-mirror
			  ()
			(vm-set-mirror-data-of
			 (car message-list)
			 (make-vector nil vm-mirror-data-vector-length))
			(vm-set-attributes-of
			 (car message-list)
			 (make-vector vm-attributes-vector-length nil)))
		      (vm-set-softdata-of
		       (car message-list)
		       (copy-sequence (vm-softdata-of
				       (car message-list))))
		      (vm-set-mark-of (car message-list) nil)
		      (vm-set-modflag-of (car message-list) nil)
		      (vm-set-reverse-link-of (car message-list) nil)))
		(setq mp (cdr mp)))
	      (setq folders (cdr folders))))
	  (setq selector-list (cdr selector-list)))
	(setq clauses (cdr clauses))))
    ;; Until this point the user doesn't really have a virtual
    ;; folder, as the virtual message haven't been linked to the
    ;; real messages, virtual buffers to the real buffers, and no
    ;; message list has been installed.
    ;;
    ;; Now we tie it all together, with this section of code being
    ;; uninterruptible.
    (let ((inhibit-quit))
      (setq vm-real-buffers real-buffers-used)
      (while real-buffers-used
	(set-buffer (car real-buffers-used))
	(if (not (memq vbuffer vm-virtual-buffers))
	    (setq vm-virtual-buffers (cons vbuffer vm-virtual-buffers)))
	(setq real-buffers-used (cdr real-buffers-used)))
      (if vm-virtual-mirror
	  (progn
	    (setq mp message-list)
	    (while mp
	      (vm-set-virtual-messages-of
	       (vm-real-message-of (car mp))
	       (cons (car map) (vm-virtual-messages-of (car mp))))
	      (setq mp (cdr mp)))))
      (setq vm-message-list (nreverse message-list)))))

(defun vm-delete-directories (list)
  (vm-delete 'file-directory-p list))

(defun vm-vs-any (m) t)

(defun vm-vs-author (m arg)
  (or (string-match arg (vm-su-full-name m))
      (string-match arg (vm-su-from m))))

(defun vm-vs-recipient (m arg)
  (or (string-match arg (vm-su-to m))
      (string-match arg (vm-su-to-names m))))

(defun vm-vs-subject (m arg)
  (string-match arg (vm-su-subject m)))

(defun vm-vs-header (m arg)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (vm-start-of m))
      (forward-line)
      (re-search-forward arg (vm-text-of m) t))))

(defun vm-vs-text (m arg)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (vm-text-of m))
      (re-search-forward arg (vm-text-end-of m) t))))

(defun vm-virtual-quit ()
  (if (and (eq vm-confirm-quit t)
	   (not (y-or-n-p "Do you really want to quit? ")))
      (error "Aborted")
    (message ""))
  (let ((bp vm-real-buffers)
	(mp vm-message-list)
	(b (current-buffer))
	(inhibit-quit t))
    (save-excursion
      (while bp
	(set-buffer (car bp))
	(setq vm-virtual-buffers (delq b vm-virtual-buffers)
	      bp (cdr bp))))
    (while mp
      (vm-set-virtual-messages-of
       (vm-real-message-of (car mp))
       (delq (car mp) (vm-virtual-messages-of (vm-real-message-of (car mp)))))
      (setq mp (cdr mp)))
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))
