;;; Commands to undo message attribute changes in VM
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

(require 'vm)

(defun vm-set-buffer-modified-p (flag &optional clear-modflags buffer)
  (save-excursion
    (and buffer (set-buffer buffer))
    (if (eq (setq flag (not (not flag))) vm-buffer-modified-p)
	()
      (set-buffer-modified-p flag)
      (vm-increment vm-modification-counter)
      (if (null flag)
	  (let ((mp vm-message-list))
	    (setq vm-messages-not-on-disk 0)
	    (if clear-modflags
		(while mp
		  (vm-set-modflag-of (car mp) nil)
		  (setq mp (cdr mp))))))
      (setq vm-buffer-modified-p (if flag "--**-")))))

(defun vm-sanity-check-modification-flag ()
  ;; this is possible if the user used a normal buffer save command instead of
  ;; vm-save-folder...
  (if (not (eq (not (not vm-buffer-modified-p)) (buffer-modified-p)))
      (vm-set-buffer-modified-p (buffer-modified-p))))

(defun vm-undo-boundary ()
  (if (car vm-undo-record-list)
      (setq vm-undo-record-list (cons nil vm-undo-record-list))))

(defun vm-clear-expunge-invalidated-undos (&optional real-folder)
  (let ((udp vm-undo-record-list) udp-prev)
    (while udp
      (cond ((null (car udp))
	     (setq udp-prev udp))
	    ((and (not (eq (car (car udp)) 'vm-set-buffer-modified-p))
		  (vm-deleted-flag (car (cdr (car udp))))
		  (or (null real-folder)
		      (eq real-folder (marker-buffer
				       (vm-start-of (car (cdr (car udp))))))))
	     (cond (udp-prev (setcdr udp-prev (cdr udp)))
		   (t (setq vm-undo-record-list (cdr udp)))))
	    (t (setq udp-prev udp)))
      (setq udp (cdr udp)))
    (and (null real-folder) vm-virtual-buffers
	 (let ((b-list vm-virtual-buffers)
	       (curbuf (current-buffer)))
	   (save-excursion
	     (while b-list
	       (set-buffer (car b-list))
	       (vm-clear-expunge-invalidated-undos curbuf)
	       (setq b-list (cdr b-list))))))
    (vm-clear-modification-flag-undos real-folder)))
	    
(defun vm-clear-modification-flag-undos (&optional real-folder)
  (let ((udp vm-undo-record-list) udp-prev)
    (while udp
      (cond ((null (car udp))
	     (setq udp-prev udp))
	    ((and (eq (car (car udp)) 'vm-set-buffer-modified-p)
		  (or (null real-folder)
		      (eq real-folder (vm-last (car udp)))))
	     (cond (udp-prev (setcdr udp-prev (cdr udp)))
		   (t (setq vm-undo-record-list (cdr udp)))))
	    (t (setq udp-prev udp)))
      (setq udp (cdr udp)))
    (vm-squeeze-consecutive-undo-boundaries)
    (and (null real-folder) vm-virtual-buffers
	 (let ((b-list vm-virtual-buffers)
	       (curbuf (current-buffer)))
	   (save-excursion
	     (while b-list
	       (set-buffer (car b-list))
	       (vm-clear-modification-flag-undos curbuf)
	       (setq b-list (cdr b-list))))))))

;; squeeze out consecutive record separators left by record deletions
(defun vm-squeeze-consecutive-undo-boundaries ()
  (let ((udp vm-undo-record-list) udp-prev)
    (while udp
      (cond ((and (null (car udp)) udp-prev (null (car udp-prev)))
	     (setcdr udp-prev (cdr udp)))
	    (t (setq udp-prev udp)))
      (setq udp (cdr udp)))
    (if (equal '(nil) vm-undo-record-list)
	(setq vm-undo-record-list nil))))
	    
(defun vm-undo-record (sexp)
  (setq vm-undo-record-list (cons sexp vm-undo-record-list)))

(defun vm-undo ()
  "Undo last change to message attributes in the current folder.
Consecutive invocations of this command cause sequentially earlier
changes to be undone.  After an intervening command between undos,
the undos themselves become undoable."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (let ((inhibit-quit t)
	(modified (buffer-modified-p)))
    (if (not (eq last-command 'vm-undo))
	(setq vm-undo-record-pointer vm-undo-record-list))
    (if (not vm-undo-record-pointer)
	(error "No further VM undo information available"))
    ;; skip current record boundary
    (setq vm-undo-record-pointer (cdr vm-undo-record-pointer))
    (while (car vm-undo-record-pointer)
      (eval (car vm-undo-record-pointer))
      (setq vm-undo-record-pointer (cdr vm-undo-record-pointer)))
    (message "VM Undo!")
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary))
    (vm-update-summary-and-mode-line)))

(defun vm-set-xxxx-flag (m flag norecord function attr-index)
  (let* ((inhibit-quit t) m-list modflag-buffer mirror
	 (virtual (not (eq m (vm-real-message-of m))))
	 (read-only (save-excursion
		      (set-buffer
		       (marker-buffer (if virtual
					 (vm-su-start-of m)
				       (vm-start-of m))))
		      vm-folder-read-only)))
    (and (eq (vm-attributes-of m) (vm-attributes-of (vm-real-message-of m)))
	 (setq m (vm-real-message-of m)
	       mirror t))
    (cond ((not read-only)
	   (aset (vm-attributes-of m) attr-index flag)
	   (vm-mark-for-display-update m)))
    (if (and (not norecord) (not read-only))
	(progn
	  (save-excursion
	    (set-buffer 
	     (marker-buffer
	      (if (and virtual (not mirror))
		  (vm-su-start-of m)
		(vm-start-of m))))
	    (cond ((not vm-buffer-modified-p)
		   (setq modflag-buffer (current-buffer))
		   (vm-set-buffer-modified-p t)
		   (vm-undo-record (list 'vm-set-buffer-modified-p nil))
		   (setq vm-totals nil)))
	    (vm-undo-record (list function m (not flag)))
	    (vm-undo-boundary)
	    (vm-increment vm-modification-counter)
	    (if (eq vm-flush-interval t)
		(vm-stuff-virtual-attributes m)
	      (vm-set-modflag-of m t)))
	  (setq m-list (vm-virtual-messages-of m))
	  (and m-list
	       (save-excursion
		 (while m-list
		   (set-buffer (marker-buffer (vm-su-start-of (car m-list))))
		   (and modflag-buffer
			(vm-undo-record
			 (list 'vm-set-buffer-modified-p nil nil modflag-buffer)))
		   (vm-undo-record (list function m (not flag)))
		   (vm-undo-boundary)
		   (vm-increment vm-modification-counter)
		   (setq vm-totals nil)
		   (setq m-list (cdr m-list)))))))))

(defun vm-set-new-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-new-flag 0))

(defun vm-set-unread-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-unread-flag 1))

(defun vm-set-deleted-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-deleted-flag 2))

(defun vm-set-filed-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-filed-flag 3))

(defun vm-set-replied-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-replied-flag 4))

(defun vm-set-written-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-written-flag 5))

(defun vm-set-forwarded-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-forwarded-flag 6))

;; this is solely for the use of vm-stuff-attributes and appears here
;; only because this function should be grouped with others of its kind
;; together for maintenance purposes.
(defun vm-set-deleted-flag-in-vector (v flag)
  (aset v 2 flag))
