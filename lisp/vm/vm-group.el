;;; Commands to rearrange (group) message presentation
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

(defun vm-group-by (compare-function)
  (let (buckets bp cons (mp vm-message-list))
    (while mp
      (setq bp buckets)
      (catch 'found
	(while bp
	  (if (funcall compare-function (car mp) (car (car bp)))
	      (progn
		(setq cons mp
		      mp (cdr mp))
		(setcdr cons nil)
		(setcar bp (nconc cons (car bp)))
		(throw 'found t)))
	  (setq bp (cdr bp)))
	(setq cons mp
	      mp (cdr mp))
	(setcdr cons nil)
	(setq buckets (cons cons buckets))))
    (setq vm-message-list
	  (apply 'nconc (nreverse (mapcar 'nreverse buckets)))
	  vm-message-order-changed t)
    (vm-reverse-link-messages)
    (and (eq vm-retain-message-order t) (not vm-folder-read-only)
	 (vm-set-buffer-modified-p t))))

(defconst vm-group-by-subject-closure (cons t t))

(defun vm-group-by-subject (m1 m2)
  (let ((subject (vm-su-subject m1)))
    (if (eq subject (car vm-group-by-subject-closure))
	(setq subject (cdr vm-group-by-subject-closure))
      (setcar vm-group-by-subject-closure subject)
      (if (string-match "^\\(re: *\\)+" subject)
	  (setq subject (substring subject (match-end 0))))
      (if (string-match " +$" subject)
	  (setq subject (substring subject 0 (match-beginning 0))))
      (setq subject (concat "^\\(re: *\\)*"
			    (regexp-quote subject)
			    " *$"))
      (setcdr vm-group-by-subject-closure subject))
    (string-match subject (vm-su-subject m2))))

(defun vm-group-by-author (m1 m2)
  (string= (vm-su-full-name m1) (vm-su-full-name m2)))

(defun vm-group-by-recipient (m1 m2)
  (equal (vm-su-to-names m1) (vm-su-to-names m2)))

(defun vm-group-by-date-sent (m1 m2)
  (and (string= (vm-su-monthday m1) (vm-su-monthday m2))
       (string= (vm-su-month m1) (vm-su-month m2))
       (string= (vm-su-year m1) (vm-su-year m2))))

(defun vm-revert-to-physical-order ()
  (let ((curr (car vm-message-pointer))
	(last (car vm-last-message-pointer)))
    (setq curr nil last nil)
    (setq vm-message-list
	  (sort vm-message-list
		(function
		 (lambda (p q) (< (vm-start-of p) (vm-start-of q)))))
	  vm-message-order-changed vm-message-order-stuffed)
    (vm-reverse-link-messages)
    (and vm-message-order-changed
	 (eq vm-retain-message-order t)
	 (not vm-folder-read-only)
	 (vm-set-buffer-modified-p t))))

(defun vm-group-messages (grouping)
  "Group messages by the argument GROUPING.
Interactively this argument is prompted for in the minibuffer,
with completion."
  (interactive
   (list 
    (completing-read
     (format "Group messages by (default %s): "
	     (or vm-group-by "physical-order"))
     vm-supported-groupings-alist 'identity t)))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (if (equal grouping "")
      (setq grouping vm-group-by))
  (cond ((and grouping (not (stringp grouping)))
	 (error "Unsupported grouping: %s" grouping))
	((equal grouping "physical-order")
	 (setq grouping nil)))
  (if grouping
      (let ((group-function (intern (concat "vm-group-by-" grouping))))
	(if (not (fboundp group-function))
	    (error "Unsupported grouping: %s" grouping))
	(vm-revert-to-physical-order)
	(message "Grouping messages by %s..." grouping)
	(vm-group-by group-function)
	(message "Grouping messages by %s... done" grouping)
	(setq vm-current-grouping grouping
	      vm-numbering-redo-start-point t
	      vm-summary-redo-start-point t))
    (vm-revert-to-physical-order)
    (setq vm-current-grouping grouping
	  vm-numbering-redo-start-point t
	  vm-summary-redo-start-point t)
    (if (interactive-p)
	(message "Reverted to folder's physical ordering")))
  (vm-update-summary-and-mode-line))

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
	      (vm-mark-for-display-update (car mp))
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
