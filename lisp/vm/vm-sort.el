;;; Sorting and moving messages inside VM
;;; Copyright (C) 1993 Kyle E. Jones
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

(defun vm-move-message-forward (count)
  "Move a message forward in a VM folder.
Prefix arg COUNT causes the current message to be moved COUNT messages forward.
A negative COUNT causes movement to be backward instead of forward.
COUNT defaults to 1.  The current message remains selected after being
moved."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let* ((ovmp vm-message-pointer) vmp-prev ovmp-prev
	 (vm-message-pointer vm-message-pointer)
	 (direction (if (> count 0) 'forward 'backward))
	 (count (vm-abs count)))
    (while (not (zerop count))
      (vm-move-message-pointer direction)
      (vm-decrement count))
    (if (> (string-to-int (vm-number-of (car vm-message-pointer)))
	   (string-to-int (vm-number-of (car ovmp))))
	(setq vm-message-pointer (cdr vm-message-pointer)))
    (if (eq vm-message-pointer ovmp)
	()
      (if (null vm-message-pointer)
	  (setq vmp-prev (vm-last vm-message-list))
	(setq vmp-prev (vm-reverse-link-of (car vm-message-pointer))))
      (setq ovmp-prev (vm-reverse-link-of (car ovmp)))
      (if ovmp-prev
	  (progn
	    (setcdr ovmp-prev (cdr ovmp))
	    (and (cdr ovmp)
		 (vm-set-reverse-link-of (car (cdr ovmp)) ovmp-prev)))
	(setq vm-message-list (cdr ovmp))
	(vm-set-reverse-link-of (car vm-message-list) nil))
      (if vmp-prev
	  (progn
	    (setcdr vmp-prev ovmp)
	    (vm-set-reverse-link-of (car ovmp) vmp-prev))
	(setq vm-message-list ovmp)
	(vm-set-reverse-link-of (car vm-message-list) nil))
      (setcdr ovmp vm-message-pointer)
      (and vm-message-pointer
	   (vm-set-reverse-link-of (car vm-message-pointer) ovmp))
      (setq vm-message-order-changed t)
      (and (eq vm-retain-message-order t) (not vm-folder-read-only)
	   (vm-set-buffer-modified-p t))
      (cond ((null ovmp-prev)
	     (setq vm-numbering-redo-start-point vm-message-list
		   vm-numbering-redo-end-point vm-message-pointer
		   vm-summary-pointer (car vm-message-list)))
	    ((null vmp-prev)
	     (setq vm-numbering-redo-start-point vm-message-list
		   vm-numbering-redo-end-point (cdr ovmp-prev)
		   vm-summary-pointer (car ovmp-prev)))
	    ((or (not vm-message-pointer)
		 (< (string-to-int (vm-number-of (car ovmp-prev)))
		    (string-to-int (vm-number-of (car vm-message-pointer)))))
	     (setq vm-numbering-redo-start-point (cdr ovmp-prev)
		   vm-numbering-redo-end-point (cdr ovmp)
		   vm-summary-pointer (car (cdr ovmp-prev))))
	    (t
	     (setq vm-numbering-redo-start-point ovmp
		   vm-numbering-redo-end-point (cdr ovmp-prev)
		   vm-summary-pointer (car ovmp-prev))))
      (if vm-summary-buffer
	  (let (list mp)
	    (vm-copy-local-variables vm-summary-buffer 'vm-summary-pointer)
	    (setq vm-need-summary-pointer-update t)
	    (setq mp vm-numbering-redo-start-point)
	    (while (not (eq mp vm-numbering-redo-end-point))
	      (vm-mark-for-summary-update (car mp))
	      (setq list (cons (car mp) list)
		    mp (cdr mp)))
	    (vm-mapc
	     (function
	      (lambda (m p)
		(vm-set-su-start-of m (car p))
		(vm-set-su-end-of m (car (cdr p)))))
	     (setq list (nreverse list))
	     (sort
	      (mapcar
	       (function
		(lambda (p)
		  (list (vm-su-start-of p) (vm-su-end-of p))))
	       list)
	      (function
	       (lambda (p q)
		 (< (car p) (car q))))))))))
  (vm-update-summary-and-mode-line))

(defun vm-move-message-backward (count)
  "Move a message backward in a VM folder.
Prefix arg COUNT causes the current message to be moved COUNT
messages backward.  A negative COUNT causes movement to be
forward instead of backward.  COUNT defaults to 1.  The current
message remains selected after being moved."
  (interactive "p")
  (vm-move-message-forward (- count)))

(defun vm-so-sortable-datestring (m)
  (or (vm-sortable-datestring-of m)
      (progn
	(vm-set-sortable-datestring-of
	 m
	 (timezone-make-date-sortable
	  (or (vm-get-header-contents m "Date:")
	      (vm-grok-From_-date m)
	      "Thu, 1 Jan 1970 00:00:00 GMT")
	  "GMT" "GMT"))
	(vm-sortable-datestring-of m))))

(defun vm-so-sortable-subject (m)
  (or (vm-sortable-subject-of m)
      (progn
	(vm-set-sortable-subject-of
	 m
	 (let ((case-fold-search t))
	   (if (string-match "\\(re: *\\)+" (vm-su-subject m))
	       (substring (vm-su-subject m) (match-end 0))
	     (vm-su-subject m))))
	(vm-sortable-subject-of m))))

(defun vm-sort-messages (keys &optional lets-get-physical)
  "Sort message in a folder by the specified KEYS.
You may sort by more than one particular message key.  If
messages compare equal by the first key, the second key will be
compared and so on.  When called interactively the keys will be
read from the minibuffer.  Valid keys are

\"date\"		\"reversed-date\"
\"author\"		\"reversed-author\"
\"subject\"		\"reversed-subject\"
\"recipients\"		\"reversed-recipients\"
\"line-count\"		\"reversed-line-count\"
\"byte-count\"		\"reversed-byte-count\"
\"physical-order\"	\"reversed-physical-order\"

Optional second arg (prefix arg interactively) means the sort
should change the physical order of the messages in the folder.
Normally VM changes presentation order only, leaving the
folder in the order in which the messages arrived."
  (interactive
   (list (vm-read-string (if current-prefix-arg
			     "Physically sort messages by: "
			   "Sort messages by: ")
			 vm-supported-sort-keys t)
	 current-prefix-arg))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let (key-list key-funcs key
	physical-order-list old-message-list new-message-list mp-old mp-new
	doomed-start doomed-end offset
	virtual)
    (setq key-list (vm-parse keys "[ \t]*\\([^ \t]+\\)")
	  key-funcs nil
	  old-message-list vm-message-list
	  virtual (eq major-mode 'vm-virtual-mode))
    (while key-list
      (setq key (car key-list))
      (cond ((equal key "author")
	     (setq key-funcs (cons 'vm-sort-compare-author key-funcs)))
	    ((equal key "reversed-author")
	     (setq key-funcs (cons 'vm-sort-compare-author-r key-funcs)))
	    ((equal key "date")
	     (require 'timezone)
	     (setq key-funcs (cons 'vm-sort-compare-date key-funcs)))
	    ((equal key "reversed-date")
	     (require 'timezone)
	     (setq key-funcs (cons 'vm-sort-compare-date-r key-funcs)))
	    ((equal key "subject")
	     (setq key-funcs (cons 'vm-sort-compare-subject key-funcs)))
	    ((equal key "reversed-subject")
	     (setq key-funcs (cons 'vm-sort-compare-subject-r key-funcs)))
	    ((equal key "recipients")
	     (setq key-funcs (cons 'vm-sort-compare-recipients key-funcs)))
	    ((equal key "reversed-recipients")
	     (setq key-funcs (cons 'vm-sort-compare-recipients-r key-funcs)))
	    ((equal key "byte-count")
	     (setq key-funcs (cons 'vm-sort-compare-byte-count key-funcs)))
	    ((equal key "reversed-byte-count")
	     (setq key-funcs (cons 'vm-sort-compare-byte-count-r key-funcs)))
	    ((equal key "line-count")
	     (setq key-funcs (cons 'vm-sort-compare-line-count key-funcs)))
	    ((equal key "reversed-line-count")
	     (setq key-funcs (cons 'vm-sort-compare-line-count-r key-funcs)))
	    ((equal key "physical-order")
	     (setq key-funcs (cons 'vm-sort-compare-physical-order key-funcs)))
	    ((equal key "reversed-physical-order")
	     (setq key-funcs (cons 'vm-sort-compare-physical-order-r key-funcs)))
	    (t (error "Unknown key: %s" key)))
      (setq key-list (cdr key-list)))
    (message "Sorting...")
    (let ((vm-key-functions (nreverse key-funcs)))
      (setq new-message-list
	    (sort (copy-sequence old-message-list) 'vm-sort-compare-xxxxxx)
	    vm-key-functions '(vm-sort-compare-physical-order)
	    physical-order-list
	    (sort (copy-sequence old-message-list) 'vm-sort-compare-xxxxxx)))
    (message "Sorting... done")
    (let ((inhibit-quit t))
      (setq mp-old old-message-list
	    mp-new new-message-list)
      (while mp-new
	(if (eq (car mp-old) (car mp-new))
	    (setq mp-old (cdr mp-old)
		  mp-new (cdr mp-new))
	  (vm-set-numbering-redo-start-point mp-new)
	  (if vm-summary-buffer
	      (progn
		(setq vm-need-summary-pointer-update t)
		(vm-set-summary-redo-start-point mp-new)
		;; start point of this message's summary is now
		;; wrong relative to where it is in the
		;; message list.  fix it and the summary rebuild
		;; will take care of the rest.
		(vm-set-su-start-of (car mp-new)
				    (vm-su-start-of (car mp-old)))))
	  (setq mp-new nil)))
      (if (and lets-get-physical (not virtual))
	  (let ((buffer-read-only nil))
	    (message "Moving messages... ")
	    (save-excursion
	      (vm-save-restriction
	       (widen)
	       (setq mp-old physical-order-list
		     mp-new new-message-list)
	       (while mp-new
		 (if (> (vm-start-of (car mp-old)) (vm-end-of (car mp-new)))
		     (setq mp-old (cdr mp-old))
		   (if (eq (car mp-old) (car mp-new))
		       (setq mp-old (cdr mp-old)
			     mp-new (cdr mp-new))
		     (goto-char (vm-start-of (car mp-old)))
		     (insert-buffer-substring (current-buffer)
					      (vm-start-of (car mp-new))
					      (vm-end-of (car mp-new)))
		     (setq doomed-start (marker-position
					 (vm-start-of (car mp-new)))
			   doomed-end (marker-position (vm-end-of (car mp-new))))
		     (setq offset (- (vm-start-of (car mp-new))
				     (vm-start-of (car mp-old))))
		     ;; make sure vm-headers-of and vm-text-of are
		     ;; non-nil in their slots before we try to
		     ;; move them.  otherwise, they may get the
		     ;; offset applied to them twice by way of a
		     ;; relative offset from one of the other
		     ;; location markers that has already been
		     ;; moved.
		     (vm-vheaders-of (car mp-new))
		     (vm-text-of (car mp-new))
		     (set-marker (vm-start-of (car mp-new))
				 (- (vm-start-of (car mp-new)) offset))
		     (set-marker (vm-headers-of (car mp-new))
				 (- (vm-headers-of (car mp-new)) offset))
		     (set-marker (vm-text-end-of (car mp-new))
				 (- (vm-text-end-of (car mp-new)) offset))
		     (set-marker (vm-end-of (car mp-new))
				 (- (vm-end-of (car mp-new)) offset))
		     (set-marker (vm-text-of (car mp-new))
				 (- (vm-text-of (car mp-new)) offset))
		     (set-marker (vm-vheaders-of (car mp-new))
				 (- (vm-vheaders-of (car mp-new)) offset))
		     ;; now fixed the start of mp-old since it didn't
		     ;; move forward.
		     (set-marker (vm-start-of (car mp-old))
				 (marker-position (vm-end-of (car mp-new))))
		     ;; delete the old copy of the message
		     (delete-region doomed-start doomed-end)
		     ;; move mp-new but not mp-old because we moved
		     ;; mp-old down one message by inserting a
		     ;; message in front of it.
		     (setq mp-new (cdr mp-new)))))))
	    (message "Moving messages... done")))
      (setq vm-message-list new-message-list)
      (vm-reverse-link-messages)
      (setq vm-message-pointer
	    (or (cdr (vm-reverse-link-of (car vm-message-pointer)))
		vm-message-list))
      (if vm-last-message-pointer
	  (setq vm-last-message-pointer
		(or (cdr (vm-reverse-link-of (car vm-last-message-pointer)))
		    vm-message-list))))
    (vm-update-summary-and-mode-line)))

(defun vm-sort-compare-xxxxxx (m1 m2)
  (let ((key-funcs vm-key-functions) result)
    (while (and key-funcs
		(eq '= (setq result (funcall (car key-funcs) m1 m2))))
      (setq key-funcs (cdr key-funcs)))
    (and key-funcs result) ))

(defun vm-sort-compare-author (m1 m2)
  (let ((s1 (vm-su-from m1))
	(s2 (vm-su-from m2)))
    (cond ((string-lessp s1 s2) t)
	  ((string-equal s1 s2) '=)
	  (t nil))))

(defun vm-sort-compare-author-r (m1 m2)
  (let ((s1 (vm-su-from m1))
	(s2 (vm-su-from m2)))
    (cond ((string-lessp s1 s2) nil)
	  ((string-equal s1 s2) '=)
	  (t t))))

(defun vm-sort-compare-date (m1 m2)
  (let ((s1 (vm-so-sortable-datestring m1))
	(s2 (vm-so-sortable-datestring m2)))
    (cond ((string-lessp s1 s2) t)
	  ((string-equal s1 s2) '=)
	  (t nil))))

(defun vm-sort-compare-date-r (m1 m2)
  (let ((s1 (vm-so-sortable-datestring m1))
	(s2 (vm-so-sortable-datestring m2)))
    (cond ((string-lessp s1 s2) nil)
	  ((string-equal s1 s2) '=)
	  (t t))))

(defun vm-sort-compare-recipients (m1 m2)
  (let ((s1 (vm-su-to m1))
	(s2 (vm-su-to m2)))
    (cond ((string-lessp s1 s2) t)
	  ((string-equal s1 s2) '=)
	  (t nil))))

(defun vm-sort-compare-recipients-r (m1 m2)
  (let ((s1 (vm-su-to m1))
	(s2 (vm-su-to m2)))
    (cond ((string-lessp s1 s2) nil)
	  ((string-equal s1 s2) '=)
	  (t t))))

(defun vm-sort-compare-subject (m1 m2)
  (let ((s1 (vm-so-sortable-subject m1))
	(s2 (vm-so-sortable-subject m2)))
    (cond ((string-lessp s1 s2) t)
	  ((string-equal s1 s2) '=)
	  (t nil))))

(defun vm-sort-compare-subject-r (m1 m2)
  (let ((s1 (vm-so-sortable-subject m1))
	(s2 (vm-so-sortable-subject m2)))
    (cond ((string-lessp s1 s2) nil)
	  ((string-equal s1 s2) '=)
	  (t t))))

(defun vm-sort-compare-line-count (m1 m2)
  (let ((n1 (string-to-int (vm-su-line-count m1)))
	(n2 (string-to-int (vm-su-line-count m2))))
    (cond ((< n1 n2) t)
	  ((= n1 n2) '=)
	  (t nil))))

(defun vm-sort-compare-line-count-r (m1 m2)
  (let ((n1 (string-to-int (vm-su-line-count m1)))
	(n2 (string-to-int (vm-su-line-count m2))))
    (cond ((> n1 n2) t)
	  ((= n1 n2) '=)
	  (t nil))))

(defun vm-sort-compare-byte-count (m1 m2)
  (let ((n1 (string-to-int (vm-su-byte-count m1)))
	(n2 (string-to-int (vm-su-byte-count m2))))
    (cond ((< n1 n2) t)
	  ((= n1 n2) '=)
	  (t nil))))

(defun vm-sort-compare-byte-count-r (m1 m2)
  (let ((n1 (string-to-int (vm-su-byte-count m1)))
	(n2 (string-to-int (vm-su-byte-count m2))))
    (cond ((> n1 n2) t)
	  ((= n1 n2) '=)
	  (t nil))))

(defun vm-sort-compare-physical-order (m1 m2)
  (let ((n1 (vm-start-of m1))
	(n2 (vm-start-of m2)))
    (cond ((< n1 n2) t)
	  ((= n1 n2) '=)
	  (t nil))))

(defun vm-sort-compare-physical-order-r (m1 m2)
  (let ((n1 (vm-start-of m1))
	(n2 (vm-start-of m2)))
    (cond ((> n1 n2) t)
	  ((= n1 n2) '=)
	  (t nil))))
