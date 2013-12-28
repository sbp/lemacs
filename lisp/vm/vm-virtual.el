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
  (let ((buffer-name (concat "virtual " folder-name))
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
		vm-virtual-folder-definition
		  (assoc folder-name vm-virtual-folder-alist))
	  (vm-build-virtual-message-list nil)
	  (use-local-map vm-mode-map)
	  ;; save this for last in case the user interrupts.
	  ;; an interrupt anywhere before this point will cause
	  ;; everything to be redone next revisit.
	  (setq major-mode 'vm-virtual-mode)
	  (vm-emit-totals-blurb)
	  (if (vm-thoughtfully-select-message)
	      (vm-preview-current-message)
	    (vm-update-summary-and-mode-line))))
    (switch-to-buffer (current-buffer))
    (and (not vm-inhibit-startup-message) (not vm-startup-message-displayed)
	 (vm-display-startup-message))))

(put 'vm-virtual-mode 'mode-class 'special)

;; This function builds the virtual message list.
;;
;; If the new-messages argument is nil, the message list is
;; derived from the folders listed in the virtual folder
;; definition and selected by the various selectors.  The
;; resulting message list is assigned to vm-message-list.
;;
;; If new-messages is non-nil then it is a list of messages to be
;; tried against the selector parts of the virtual folder
;; definition.  Matching messages are added to
;; vm-message-list, instead of replacing it.
;;
;; The messages in new-messages must all be in the same real folder.
(defun vm-build-virtual-message-list (new-messages)
  (let ((clauses (cdr vm-virtual-folder-definition))
	;; letter bomb protection
	;; set inhibit-local-variables to t for v18 Emacses
	;; set enable-local-variables to nil for newer Emacses
	(inhibit-local-variables t)
	(enable-local-variables nil)
	(vbuffer (current-buffer))
	(case-fold-search t)
	(tail-cons (vm-last vm-message-list))
	location-vector
	message message-list mp folders folder
	selectors sel-list selector arglist i
	real-buffers-used)
    ;; Since there is at most one virtual message in the folder
    ;; buffer of a virtual folder, the location data vector (and
    ;; the markers in it) of all virtual messages in a virtual
    ;; folder is shared.  We initialize the vector here if it
    ;; hasn't been created already.
    (if vm-message-list
	(setq location-vector (vm-location-data-of (car vm-message-pointer)))
      (setq i 0
	    location-vector (make-vector vm-location-data-vector-length nil))
      (while (< i vm-location-data-vector-length)
	(aset location-vector i (vm-marker nil))
	(vm-increment i)))
    (save-excursion
      (while clauses
	(setq folders (car (car clauses))
	      ;; change any to vm-vs-any, text to vm-vs-text, etc.
	      selectors (mapcar (function
				 (lambda (s)
				   (cons
				    (intern
				     (concat "vm-vs-"
					     (symbol-name (car s))))
				    (cdr s))))
				    (cdr (car clauses))))
	(while folders
	  (setq folder (expand-file-name (car folders) vm-folder-directory))
	  (cond
	   ((file-directory-p folder)
	    (setq folders (nconc (cdr folders)
				 (vm-delete-directories
				  (directory-files folder t nil)))))
	   ;; If we're assimilating messages into an existing
	   ;; virtual folder, only allow selectors that would be
	   ;; normally applied to this folder.
	   ((or (null new-messages)
		(eq (marker-buffer (vm-start-of (car new-messages)))
		    (find-file-noselect folder)))
	    (set-buffer (or (get-file-buffer folder)
			    (find-file-noselect folder)))
	    (if (not (memq (current-buffer) real-buffers-used))
		(setq real-buffers-used (cons (current-buffer)
					      real-buffers-used)))
	    (if (not (eq major-mode 'vm-mode))
		(vm-mode))
	    ;; if new-messages non-nil use it instead of the whole message list
	    (setq mp (or new-messages vm-message-list))
	    (while mp
	      (setq sel-list selectors)
	      (while sel-list
		(setq selector (car (car sel-list))
		      arglist (cdr (car sel-list)))
		(if (apply selector (car mp) arglist)
		    (progn
		      (setq message (copy-sequence (car mp)))
		      (vm-set-message-type-of message vm-folder-type)
		      (if vm-virtual-mirror
			  ()
			(vm-set-mirror-data-of
			 message
			 (make-vector vm-mirror-data-vector-length nil))
			(vm-set-attributes-of
			 message
			 (make-vector vm-attributes-vector-length nil)))
		      (vm-set-location-data-of message location-vector)
		      (vm-set-softdata-of
		       message
		       (make-vector vm-softdata-vector-length nil))
		      (vm-set-message-id-number-of message vm-message-id-number)
		      (vm-increment vm-message-id-number)
		      (vm-set-modflag-of message nil)
		      (vm-set-reverse-link-sym-of message (make-symbol "<--"))
		      (vm-set-reverse-link-of message tail-cons)
		      (if (null tail-cons)
			  (setq message-list (list message)
				tail-cons message-list)
			(setcdr tail-cons (list message))
			(setq tail-cons (cdr tail-cons)))))
		(setq sel-list (cdr sel-list)))
	      (setq mp (cdr mp)))))
	  (setq folders (cdr folders)))
	(setq clauses (cdr clauses))))
    ;; Until this point the user doesn't really have a virtual
    ;; folder, as the virtual messages haven't been linked to the
    ;; real messages, virtual buffers to the real buffers, and no
    ;; message list has been installed.
    ;;
    ;; Now we tie it all together, with this section of code being
    ;; uninterruptible.
    (let ((inhibit-quit t))
      (setq vm-real-buffers
	    (if new-messages
		(append vm-real-buffers real-buffers-used)
	      real-buffers-used))
      (save-excursion
	(while real-buffers-used
	  (set-buffer (car real-buffers-used))
	  (if (not (memq vbuffer vm-virtual-buffers))
	      (setq vm-virtual-buffers (cons vbuffer vm-virtual-buffers)))
	  (setq real-buffers-used (cdr real-buffers-used))))
      (if vm-virtual-mirror
	  (progn
	    (setq mp message-list)
	    (while mp
	      (vm-set-virtual-messages-of
	       (vm-real-message-of (car mp))
	       (cons (car mp) (vm-virtual-messages-of (car mp))))
	      (setq mp (cdr mp)))))
      (if new-messages
	  (progn
	    (setq vm-message-list (nconc vm-message-list message-list))
	    (vm-set-summary-redo-start-point message-list)
	    (vm-set-numbering-redo-start-point message-list))
	(vm-set-summary-redo-start-point t)
	(vm-set-numbering-redo-start-point t)
	(setq vm-message-list message-list)))))

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
      (goto-char (vm-headers-of m))
      (re-search-forward arg (vm-text-of m) t))))

(defun vm-vs-text (m arg)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (vm-text-of m))
      (re-search-forward arg (vm-text-end-of m) t))))

;; clear away links between real and virtual folders when
;; a vm-quit is performed in either type folder.
(defun vm-virtual-quit ()
  (save-excursion
    (cond ((eq major-mode 'vm-virtual-mode)
	   (let ((bp vm-real-buffers)
		 (mp vm-message-list)
		 (b (current-buffer))
		 ;; lock out interrupts here
		 (inhibit-quit t))
	     (while bp
	       (set-buffer (car bp))
	       (setq vm-virtual-buffers (delq b vm-virtual-buffers)
		     bp (cdr bp)))
	     (while mp
	       (vm-set-virtual-messages-of
		(vm-real-message-of (car mp))
		(delq (car mp) (vm-virtual-messages-of
				(vm-real-message-of (car mp)))))
	       (setq mp (cdr mp)))))
	  ((eq major-mode 'vm-mode)
	   (let ((bp vm-virtual-buffers)
		 (mp vm-message-list)
		 vmp
		 (b (current-buffer))
		 ;; lock out interrupts here
		 (inhibit-quit t))
	     (while mp
	       (setq vmp (vm-virtual-messages-of (car mp)))
	       (while vmp
		 ;; we'll clear these messages from the virtual
		 ;; folder by looking for messages that have a 0
		 ;; id number associated with them.
		 (vm-set-message-id-number-of (car vmp) 0)
		 (setq vmp (cdr vmp)))
	       (vm-set-virtual-messages-of (car mp) nil)
	       (setq mp (cdr mp)))
	     (while bp
	       (set-buffer (car bp))
	       (setq vm-real-buffers (delq b vm-real-buffers))
	       ;; set the message pointer to a new value if it is
	       ;; now invalid.
	       (while (and vm-message-pointer
			   (zerop (vm-message-id-number-of
				   (car vm-message-pointer))))
		 (setq vm-message-pointer
		       (cdr vm-message-pointer)))
	       (if (null vm-message-pointer)
		   (setq vm-message-pointer vm-message-list))
	       ;; same for vm-last-message-pointer
	       (if (null vm-last-message-pointer)
		   (setq vm-last-message-pointer nil))
	       ;; expunge the virtual messages associated with
	       ;; real messages that are going away.
	       (setq vm-message-list
		     (vm-delete (function
				 (lambda (m)
				   (zerop (vm-message-id-number-of m))))
				vm-message-list nil))
	       (vm-clear-virtual-quit-invalidated-undos)
	       (vm-reverse-link-messages)
	       (vm-set-numbering-redo-start-point t)
	       (vm-set-summary-redo-start-point t)
	       (if vm-message-pointer
		   (vm-preview-current-message)
		 (vm-update-summary-and-mode-line))
	       (setq bp (cdr bp))))))))

(defun vm-make-virtual-copy (m)
  (widen)
  (let ((virtual-buffer (current-buffer))
	(real-m (vm-real-message-of m))
	(buffer-read-only nil))
    (save-excursion
      (set-buffer (marker-buffer (vm-start-of real-m)))
      (save-restriction
	(widen)
	(copy-to-buffer virtual-buffer (vm-start-of real-m)
			(vm-end-of real-m))))
    (set-marker (vm-start-of m) (point-min))
    (set-marker (vm-headers-of m) (+ (vm-start-of m)
				     (- (vm-headers-of real-m)
					(vm-start-of real-m))))
    (set-marker (vm-vheaders-of m) (+ (vm-start-of m)
				      (- (vm-vheaders-of real-m)
					 (vm-start-of real-m))))
    (set-marker (vm-text-of m) (+ (vm-start-of m) (- (vm-text-of real-m)
						     (vm-start-of real-m))))
    (set-marker (vm-text-end-of m) (+ (vm-start-of m)
				      (- (vm-text-end-of real-m)
					 (vm-start-of real-m))))
    (set-marker (vm-end-of m) (+ (vm-start-of m) (- (vm-end-of real-m)
						    (vm-start-of real-m))))))
