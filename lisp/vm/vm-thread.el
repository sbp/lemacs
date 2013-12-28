;;; Thread support for VM
;;; Copyright (C) 1994 Kyle E. Jones
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

(defun vm-toggle-threads-display ()
  "Toggle the threads display on and off.
When the threads display is on, the folder will be sorted by
thread and thread indention (via the %I summary format specifier)
will be visible."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-set-summary-redo-start-point t)
  (setq vm-summary-show-threads (not vm-summary-show-threads))
  (if vm-summary-show-threads
      (vm-sort-messages "thread")
    (vm-sort-messages "physical-order")))

(defun vm-build-threads (message-list)
  (if (null vm-thread-obarray)
      (setq vm-thread-obarray (make-vector 641 0)
	    vm-thread-subject-obarray (make-vector 641 0)))
  (let ((mp (or message-list vm-message-list))
	(n 0)
	;; Just for laughs, make the update interval vary.
	(modulus (+ (% (vm-abs (random)) 11) 40))
	;; no need to schedule reindents of reparented messages
	;; unless there were already messages present.
	(schedule-reindents message-list)
	parent parent-sym id id-sym subject subject-sym date)
    (while mp
      (setq parent (vm-th-parent (car mp))
	    id (vm-su-message-id (car mp))
	    id-sym (intern id vm-thread-obarray)
	    subject (vm-so-sortable-subject (car mp))
	    date (vm-so-sortable-datestring (car mp)))
      (put id-sym 'messages (cons (car mp) (get id-sym 'messages)))
      (if (and (null (cdr (get id-sym 'messages)))
	       schedule-reindents)
	  (vm-thread-mark-for-summary-update (get id-sym 'children)))
      (if parent
	  (progn
	    (setq parent-sym (intern parent vm-thread-obarray))
	    (if (not (boundp id-sym))
		(set id-sym parent-sym))
	    (put parent-sym 'children
		 (cons (car mp) (get parent-sym 'children))))
	(set id-sym nil))
      ;; we need to make sure the asets below are an atomic group.
      (let ((inhibit-quit t))
	(setq subject-sym (intern subject vm-thread-subject-obarray))
	(if (not (boundp subject-sym))
	    (set subject-sym
		 (vector id-sym (vm-so-sortable-datestring (car mp))
			 nil (list (car mp))))
	  (aset (symbol-value subject-sym) 3
		(cons (car mp) (aref (symbol-value subject-sym) 3)))
	  (if (string< date (aref (symbol-value subject-sym) 1))
	      (let* ((vect (symbol-value subject-sym))
		     (i-sym (aref vect 0)))
		(if (or (not (boundp i-sym))
			(null (symbol-value i-sym)))
		    (aset vect 2 (append (get i-sym 'messages)
					 (aref vect 2))))
		(aset vect 0 id-sym)
		(aset vect 1 date)
		;; this loops _and_ recurses and I'm worried
		;; about it going into a spin someday.  So I
		;; unblock interrupts here.  It's not critical
		;; that it finish... the summary will just be out
		;; of sync.
		(if schedule-reindents
		    (let ((inhibit-quit nil))
		      (vm-thread-mark-for-summary-update (aref vect 2)))))
	    (if (null parent)
		(aset (symbol-value subject-sym) 2
		      (cons (car mp) (aref (symbol-value subject-sym) 2))))))
	(setq mp (cdr mp) n (1+ n))
	(if (zerop (% n modulus))
	    (message "Building threads... %d" n))))
    (if (> n modulus)
	(message "Building threads... done"))))

(defun vm-thread-mark-for-summary-update (message-list)
  (while message-list
    (vm-mark-for-summary-update (car message-list) t)
    (vm-set-thread-list-of (car message-list) nil)
    (vm-set-thread-indention-of (car message-list) nil)
    (vm-thread-mark-for-summary-update
     (get (intern (vm-su-message-id (car message-list))
		  vm-thread-obarray)
	  'children))
    (setq message-list (cdr message-list))))

(defun vm-thread-list (message)
  (let ((done nil)
	(m message)
	thread-list id-sym subject-sym loop-sym root-date)
    (save-excursion
      (set-buffer (vm-buffer-of m))
      (setq id-sym (intern (vm-su-message-id m) vm-thread-obarray)
	    thread-list (list id-sym))
      (fillarray vm-thread-loop-obarray 0)
      (while (not done)
	(setq loop-sym (intern (symbol-name id-sym) vm-thread-loop-obarray))
	(if (boundp loop-sym)
	    ;; loop detected, bail...
	    (setq done t
		  thread-list (cdr thread-list))
	  (set loop-sym t)
	  (if (and (boundp id-sym) (symbol-value id-sym))
	      (progn
		(setq id-sym (symbol-value id-sym)
		      thread-list (cons id-sym thread-list)
		      m (car (get id-sym 'messages))))
	    (if (null m)
		(setq done t)
	      (setq subject-sym
		    (intern (vm-so-sortable-subject m)
			    vm-thread-subject-obarray))
	      (if (or (not (boundp subject-sym))
		      (eq (aref (symbol-value subject-sym) 0) id-sym))
		  (setq done t)
		(setq id-sym (aref (symbol-value subject-sym) 0)
		      thread-list (cons id-sym thread-list)
		      m (car (get id-sym 'messages))))))))
      ;; save the date of the oldest message in this thread
      (setq root-date (get id-sym 'oldest-date))
      (if (or (null root-date)
	      (string< (vm-so-sortable-datestring message) root-date))
	  (put id-sym 'oldest-date (vm-so-sortable-datestring message)))
      thread-list )))

;; remove message struct from thread data.
;;
;; optional second arg non-nil means forget information that
;; might be different if the mesage contents changed.
;;
;; message must be a real message
(defun vm-unthread-message (message &optional message-changing)
  (save-excursion
    (let ((mp (cons message (vm-virtual-messages-of message)))
	  id-sym subject-sym vect p-sym)
      (while mp
	(let ((inhibit-quit t))
	  (vm-set-thread-list-of (car mp) nil)
	  (vm-set-thread-indention-of (car mp) nil)
	  (set-buffer (vm-buffer-of (car mp)))
	  (setq id-sym (intern (vm-su-message-id (car mp)) vm-thread-obarray)
		subject-sym (intern (vm-so-sortable-subject (car mp))
				    vm-thread-subject-obarray))
	  (if (boundp id-sym)
	      (progn
		(put id-sym 'messages (delq (car mp) (get id-sym 'messages)))
		(vm-thread-mark-for-summary-update (get id-sym 'children))
		(setq p-sym (symbol-value id-sym))
		(and p-sym (put p-sym 'children
				(delq (car mp) (get p-sym 'children))))
		(if message-changing
		    (set id-sym nil))))
	  (if (and (boundp subject-sym) (setq vect (symbol-value subject-sym)))
	      (if (not (eq id-sym (aref vect 0)))
		  (aset vect 2 (delq (car mp) (aref vect 2)))
		(if message-changing
		    (if (null (cdr (aref vect 3)))
			(makunbound subject-sym)
		      (let ((p (aref vect 3))
			    oldest-msg oldest-date children)
			(setq oldest-msg (car p)
			      oldest-date (vm-so-sortable-datestring (car p))
			      p (cdr p))
			(while p
			  (if (and (string-lessp (vm-so-sortable-datestring (car p))
						 oldest-date)
				   (not (eq (car mp) (car p))))
			      (setq oldest-msg (car p)
				    oldest-date (vm-so-sortable-datestring (car p))))
			  (setq p (cdr p)))
			(aset vect 0 (intern (vm-su-message-id oldest-msg)
					     vm-thread-obarray))
			(aset vect 1 oldest-date)
			(setq children (delq oldest-msg (aref vect 2)))
			(aset vect 2 children)
			(aset vect 3 (delq (car mp) (aref vect 3)))
			;; I'm not sure there aren't situations
			;; where this might loop forever.
			(let ((inhibit-quit nil))
			  (vm-thread-mark-for-summary-update children))))))))
	  (setq mp (cdr mp))))))

(defun vm-th-parent (m)
  (or (vm-parent-of m)
      (vm-set-parent-of
       m
       (or (let (in-reply-to)
	     (setq in-reply-to (vm-get-header-contents m "In-Reply-To:"))
	     (and in-reply-to
		  (car (vm-parse in-reply-to "[^<]*\\(<[^>]+>\\)"))))
	   (let (references)
	     (setq references (vm-get-header-contents m "References:"))
	     (and references
		  (car (vm-last
			(vm-parse references "[^<]*\\(<[^>]+>\\)")))))))))

(defun vm-th-thread-indention (m)
  (or (vm-thread-indention-of m)
      (let ((p (vm-th-thread-list m)))
	(while (and p (null (get (car p) 'messages)))
	  (setq p (cdr p)))
	(vm-set-thread-indention-of m (1- (length p)))
	(vm-thread-indention-of m))))

(defun vm-th-thread-list (m)
  (or (vm-thread-list-of m)
      (progn
	(vm-set-thread-list-of m (vm-thread-list m))
	(vm-thread-list-of m))))
