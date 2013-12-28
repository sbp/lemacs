;;; Miscellaneous functions for VM
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

(defun vm-parse (string regexp &optional matchn)
  (or matchn (setq matchn 1))
  (let (list)
    (store-match-data nil)
    (while (string-match regexp string (match-end 0))
      (setq list (cons (substring string (match-beginning matchn)
				  (match-end matchn)) list)))
    (nreverse list)))

(defun vm-parse-addresses (string)
  (if (null string)
      ()
    (let (work-buffer)
      (save-excursion
       (unwind-protect
	   (let (list start s char)
	     (setq work-buffer (generate-new-buffer "*VM parse*"))
	     (set-buffer work-buffer)
	     (insert string)
	     (goto-char (point-min))
	     (skip-chars-forward "\t\f\n\r ")
	     (setq start (point))
	     (while (not (eobp))
	       (skip-chars-forward "^\"\\,(")
	       (setq char (following-char))
	       (cond ((= char ?\\)
		      (forward-char 1)
		      (if (not (eobp))
			  (forward-char 1)))
		     ((= char ?,)
		      (setq s (buffer-substring start (point)))
		      (if (or (null (string-match "^[\t\f\n\r ]+$" s))
			      (not (string= s "")))
			  (setq list (cons s list)))
		      (forward-char 1)
		      (skip-chars-forward "\t\f\n\r ")
		      (setq start (point)))
		     ((= char ?\")
		      (forward-char 1)
		      (re-search-forward "[^\\]\"" nil 0))
		     ((= char ?\()
		      (let ((parens 1))
			(forward-char 1)
			(while (and (not (eobp)) (not (zerop parens)))
			  (re-search-forward "[^\\][()]" nil 0)
			  (cond ((eobp))
				((= (preceding-char) ?\()
				 (setq parens (1+ parens)))
				((= (preceding-char) ?\))
				 (setq parens (1- parens)))))))))
	     (setq s (buffer-substring start (point)))
	     (if (and (null (string-match "^[\t\f\n\r ]+$" s))
		      (not (string= s "")))
		 (setq list (cons s list)))
	     (nreverse list)) ; jwz: fixed order
	(and work-buffer (kill-buffer work-buffer)))))))

(defmacro vm-marker (pos &optional buffer)
  (list 'set-marker '(make-marker) pos buffer))

(defmacro vm-increment (variable)
  (list 'setq variable (list '1+ variable)))

(defmacro vm-decrement (variable)
  (list 'setq variable (list '1- variable)))

(defmacro vm-select-folder-buffer ()
  '(and vm-mail-buffer (buffer-name vm-mail-buffer)
	(set-buffer vm-mail-buffer)))

(defmacro vm-check-for-killed-summary ()
  '(and (bufferp vm-summary-buffer) (null (buffer-name vm-summary-buffer))
	(setq vm-summary-buffer nil)))

(defmacro vm-error-if-folder-read-only ()
  '(while vm-folder-read-only
     (signal 'folder-read-only (list (current-buffer)))))

(put 'folder-read-only 'error-conditions '(folder-read-only error))
(put 'folder-read-only 'error-message "Folder is read-only")

(defmacro vm-error-if-referenced-virtually ()
  '(and (setq vm-virtual-buffers (vm-trim-dead-buffers vm-virtual-buffers))
	(error "Can't execute command: folder is referenced virtually.")))

(defmacro vm-error-if-virtual-folder ()
  '(and (eq major-mode 'vm-virtual-mode)
	(error "%s cannot be applied to virtual folders." this-command)))

(defmacro vm-nuke-dead-virtual-buffers ()
  '(setq vm-virtual-buffers (vm-trim-dead-buffers vm-virtual-buffers)))

(defmacro vm-check-message-clipping ()
  '(and vm-virtual-buffers
	(or (< (point-min) (vm-start-of (car vm-message-pointer)))
	    (> (point-max) (vm-text-end-of (car vm-message-pointer))))
	(vm-preview-current-message)))

(defun vm-trim-dead-buffers (list)
  (vm-delete 'buffer-name list t))

(defun vm-deferred-message (&rest args)
  (setq vm-deferred-message (apply 'format args)))

(defun vm-abs (n) (if (< n 0) (- n) n))

(defun vm-read-number (prompt)
  (let (result)
    (while
	(null
	 (string-match "^[ \t]*-?[0-9]+" (setq result (read-string prompt)))))
    (string-to-int result)))

;; save-restriction flubs restoring the clipping region if you
;; (widen) and modify text outside the old region.
;; This should do it right.
(defmacro vm-save-restriction (&rest forms)
  (let ((vm-sr-clip (make-symbol "vm-sr-clip"))
	(vm-sr-min (make-symbol "vm-sr-min"))
	(vm-sr-max (make-symbol "vm-sr-max")))
    (list 'let (list (list vm-sr-clip '(> (buffer-size)
					  (- (point-max) (point-min))))
		     ;; this shouldn't be necessary but the
		     ;; byte-compiler turns these into interned symbols
		     ;; which utterly defeats the purpose of the
		     ;; make-symbol calls above.  Soooo, until the compiler
		     ;; is fixed, these must be made into (let ...)
		     ;; temporaries so that nested calls to this macros
		     ;; won't misbehave.
		     vm-sr-min vm-sr-max)
	  (list 'and vm-sr-clip
		(list 'setq vm-sr-min '(set-marker (make-marker) (point-min)))
		(list 'setq vm-sr-max '(set-marker (make-marker) (point-max))))
	  (list 'unwind-protect (cons 'progn forms)
		'(widen)
		(list 'and vm-sr-clip
		      (list 'progn
			    (list 'narrow-to-region vm-sr-min vm-sr-max)
			    (list 'set-marker vm-sr-min nil)
			    (list 'set-marker vm-sr-max nil)))))))

(defmacro vm-save-buffer-excursion (&rest forms)
  (list 'let '((vm-sbe-buffer (current-buffer)))
	(list 'unwind-protect
	      (cons 'progn forms)
	      '(and (not (eq vm-sbe-buffer (current-buffer)))
		    (buffer-name vm-sbe-buffer)
		    (set-buffer vm-sbe-buffer)))))

(defmacro vm-current-message-buffer ()
  (list 'marker-buffer
	(list 'vm-start-of
	      (list 'car 'vm-message-pointer))))

(defmacro vm-within-current-message-buffer (&rest forms)
  (list 'let '((vm-sbe-buffer (current-buffer)))
	'(and (eq major-mode 'vm-virtual-mode) vm-message-list
	      (set-buffer (marker-buffer (vm-start-of
					  (car vm-message-pointer)))))
	(list 'unwind-protect
	      (cons 'progn forms)
	      '(and (not (eq vm-sbe-buffer (current-buffer)))
		    (buffer-name vm-sbe-buffer)
		    (set-buffer vm-sbe-buffer)))))

(defun vm-last (list) (while (cdr-safe list) (setq list (cdr list))) list)

(defun vm-vector-to-list (vector)
  (let ((i (1- (length vector)))
	list)
    (while (>= i 0)
      (setq list (cons (aref vector i) list))
      (vm-decrement i))
    list ))

(defun vm-extend-vector (vector length &optional fill)
  (let ((vlength (length vector)))
    (if (< vlength length)
	(apply 'vector (nconc (vm-vector-to-list vector)
			      (make-list (- length vlength) fill)))
      vector )))

(defun vm-mapcar (function &rest lists)
  (let (arglist result)
    (while (car lists)
      (setq arglist (mapcar 'car lists))
      (setq result (cons (apply function arglist) result))
      (setq lists (mapcar 'cdr lists)))
    (nreverse result)))

(defun vm-mapc (function &rest lists)
  (let (arglist)
    (while (car lists)
      (setq arglist (mapcar 'car lists))
      (apply function arglist)
      (setq lists (mapcar 'cdr lists)))))

(defun vm-delete (predicate list &optional reverse)
  (let ((p list) (reverse (if reverse 'not 'identity)) prev)
    (while p
      (if (funcall reverse (funcall predicate (car p)))
	  (if (null prev)
	      (setq list (cdr list) p list)
	    (setcdr prev (cdr p))
	    (setq p (cdr p)))
	(setq prev p p (cdr p))))
    list ))

(defun vm-delete-duplicates (list &optional all)
  (setq list (sort list 'string<))
  (let ((p list) prev)
    (while p
      (if (not (equal (car p) (car (cdr p))))
	  (setq prev p p (cdr p))
	(setq p (cdr p))
	(while (and p (equal (car p) (car (cdr p))))
	  (setq p (cdr p)))
	(if (null prev)
	    (setq list (if all (cdr p) p)
		  prev (if all nil p)
		  p (cdr p))
	  (setcdr prev (if all (cdr p) p))
	  (setq p (cdr p)))))
    list ))

(defun vm-copy-local-variables (buffer &rest variables)
  (let ((values (mapcar 'symbol-value variables)))
    (save-excursion
      (set-buffer buffer)
      (vm-mapc 'set variables values))))

(put 'folder-empty 'error-conditions '(folder-empty error))
(put 'folder-empty 'error-message "Folder is empty")

(defun vm-error-if-folder-empty ()
  (while (null vm-message-list)
    (signal 'folder-empty nil)))

(defun vm-copy (object)
  (cond ((consp object)
	 (let (return-value cons)
	   (setq return-value (cons (vm-copy (car object)) nil)
		 cons return-value
		 object (cdr object))
	   (while (consp object)
	     (setcdr cons (cons (vm-copy (car object)) nil))
	     (setq cons (cdr cons)
		   object (cdr object)))
	   (setcdr cons object)
	   return-value ))
	((vectorp object) (apply 'vector (mapcar 'vm-copy object)))
	((stringp object) (copy-sequence object))
	(t object)))

; buffer-flush-undo renamed in FSF v19
(or (fboundp 'buffer-disable-undo)
    (fset 'buffer-disable-undo 'buffer-flush-undo))

(defun vm-lucid-emacs-p ()
  (string-match "Lucid" emacs-version))

(defun vm-fsf-emacs-19-p ()
  (and (string-match "^19" emacs-version) (not (vm-lucid-emacs-p))))
