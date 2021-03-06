;;; Virtual folders for VM
;;; Copyright (C) 1990, 1993, 1994 Kyle E. Jones
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

	(message-set (make-vector 311 0))
	(vbuffer (current-buffer))
	(mirrored vm-virtual-mirror)
	(case-fold-search t)
	(tail-cons (vm-last vm-message-list))
	(new-message-list nil)

	;; selectors
	(any 'vm-vs-any)
	(and 'vm-vs-and)
	(or 'vm-vs-or)
	(not 'vm-vs-not)
	(header 'vm-vs-header)
	(label 'vm-vs-label)
	(text 'vm-vs-text)
	(recipient 'vm-vs-recipient)
	(author 'vm-vs-author)
	(subject 'vm-vs-subject)
	(sent-before 'vm-vs-sent-before)
	(sent-after 'vm-vs-sent-after)
	(more-chars-than 'vm-vs-more-chars-than)
	(less-chars-than 'vm-vs-less-chars-than)
	(more-lines-than 'vm-vs-more-lines-than)
	(less-lines-than 'vm-vs-less-lines-than)
	(new 'vm-vs-new)
	(unread 'vm-vs-unread)
	(read 'vm-vs-read)
	(deleted 'vm-vs-deleted)
	(replied 'vm-vs-replied)
	(forwarded 'vm-vs-forwarded)
	(filed 'vm-vs-filed)
	(written 'vm-vs-written)
	(edited 'vm-vs-edited)
	(marked 'vm-vs-marked)

	virtual location-vector
	message mp folders folder
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
    ;; To keep track of the messages in a virtual folder to
    ;; prevent duplicates we create and maintain a set that
    ;; contain all the real messages.
    (setq mp vm-message-list)
    (while mp
      (intern (vm-message-id-number-of (vm-real-message-of (car mp)))
	      message-set)
      (setq mp (cdr mp)))
    ;; now select the messages
    (save-excursion
      (while clauses
	(setq folders (car (car clauses))
	      selectors (cdr (car clauses)))
	(while folders
	  (setq folder (car folders))
	  (and (stringp folder)
	       (setq folder (expand-file-name folder vm-folder-directory)))
	  (cond
	   ((and (stringp folder) (file-directory-p folder))
	    (setq folders (nconc (cdr folders)
				 (vm-delete-directories
				  (directory-files folder t nil)))))
	   ((or (null new-messages)
		;; If we're assimilating messages into an
		;; existing virtual folder, only allow selectors
		;; that would be normally applied to this folder.
		(and (bufferp folder)
		     (eq (vm-buffer-of (car new-messages)) folder))
		(and (stringp folder)
		     (eq (vm-buffer-of (car new-messages))
			 ;; letter bomb protection
			 ;; set inhibit-local-variables to t for v18 Emacses
			 ;; set enable-local-variables to nil for newer Emacses
			 (let ((inhibit-local-variables t)
			       (enable-local-variables nil))
			   (find-file-noselect folder)))))
	    (set-buffer (or (and (bufferp folder) folder)
			    (vm-get-file-buffer folder)
			    (find-file-noselect folder)))
	    (if (eq major-mode 'vm-virtual-mode)
		(setq virtual t
		      real-buffers-used
		      (append vm-real-buffers real-buffers-used))
	      (setq virtual nil)
	      (if (not (memq (current-buffer) real-buffers-used))
		  (setq real-buffers-used (cons (current-buffer)
						real-buffers-used)))
	      (if (not (eq major-mode 'vm-mode))
		  (vm-mode)))
	    ;; change (#<somebuffer>) into ("/file" "/file2" ...)
	    ;; this assumes that there will never be
	    ;; (#<somebuffer> #<somebuffer2>) in a virtual
	    ;; folder spec.
	    (if (bufferp folder)
		(if virtual
		    (setcar (car clauses)
			    (delq nil
				  (mapcar 'buffer-file-name vm-real-buffers)))
		  (setcar (car clauses)
			  (list (or buffer-file-name (current-buffer))))))
	    ;; if new-messages non-nil use it instead of the
	    ;; whole message list
	    (setq mp (or new-messages vm-message-list))
	    (while mp
	      (if (and (not (intern-soft
			     (vm-message-id-number-of
			      (vm-real-message-of (car mp)))
			     message-set))
		       (if virtual
			   (save-excursion
			     (set-buffer
			      (vm-buffer-of
			       (vm-real-message-of
				(car mp))))
			     (apply 'vm-vs-or (vm-real-message-of (car mp))
				    selectors))
			 (apply 'vm-vs-or (car mp) selectors)))
		  (progn
		    (intern
		     (vm-message-id-number-of
		      (vm-real-message-of (car mp)))
		      message-set)
		    (setq message (copy-sequence
				   (vm-real-message-of (car mp))))
		    (if mirrored
			()
		      (vm-set-mirror-data-of
		       message
		       (make-vector vm-mirror-data-vector-length nil))
		      (vm-set-virtual-messages-sym-of
		       message (make-symbol "<v>"))
		      (vm-set-virtual-messages-of message nil)
		      (vm-set-attributes-of
		       message
		       (make-vector vm-attributes-vector-length nil)))
		    (vm-set-location-data-of message location-vector)
		    (vm-set-softdata-of
		     message
		     (make-vector vm-softdata-vector-length nil))
		    (vm-set-real-message-sym-of
		     message
		     (vm-real-message-sym-of (car mp)))
		    (vm-set-message-type-of message vm-folder-type)
		    (vm-set-message-id-number-of message
						 vm-message-id-number)
		    (vm-increment vm-message-id-number)
		    (vm-set-buffer-of message vbuffer)
		    (vm-set-reverse-link-sym-of message (make-symbol "<--"))
		    (vm-set-reverse-link-of message tail-cons)
		    (if (null tail-cons)
			(setq new-message-list (list message)
			      tail-cons new-message-list)
		      (setcdr tail-cons (list message))
		      (if (null new-message-list)
			  (setq new-message-list (cdr tail-cons)))
		      (setq tail-cons (cdr tail-cons)))))
	      (setq mp (cdr mp)))))
	  (setq folders (cdr folders)))
	(setq clauses (cdr clauses))))
; this doesn't need to work currently, but it might someday
;    (if virtual
;	(setq real-buffers-used (vm-delete-duplicates real-buffers-used)))
    (vm-increment vm-modification-counter)
    ;; Until this point the user doesn't really have a virtual
    ;; folder, as the virtual messages haven't been linked to the
    ;; real messages, virtual buffers to the real buffers, and no
    ;; message list has been installed.
    ;;
    ;; Now we tie it all together, with this section of code being
    ;; uninterruptible.
    (let ((inhibit-quit t))
      (if (null vm-real-buffers)
	  (setq vm-real-buffers real-buffers-used))
      (save-excursion
	(while real-buffers-used
	  (set-buffer (car real-buffers-used))
	  (if (not (memq vbuffer vm-virtual-buffers))
	      (setq vm-virtual-buffers (cons vbuffer vm-virtual-buffers)))
	  (setq real-buffers-used (cdr real-buffers-used))))
      (setq mp new-message-list)
      (while mp
	(vm-set-virtual-messages-of
	 (vm-real-message-of (car mp))
	 (cons (car mp) (vm-virtual-messages-of (car mp))))
	(setq mp (cdr mp)))
      (if vm-message-list
	  (progn
	    (vm-set-summary-redo-start-point new-message-list)
	    (vm-set-numbering-redo-start-point new-message-list))
	(vm-set-summary-redo-start-point t)
	(vm-set-numbering-redo-start-point t)
	(setq vm-message-list new-message-list)))))

(defun vm-create-virtual-folder (selector &optional arg read-only)
  "Create a new virtual folder from messages in the current folder.
The messages will be chosen by applying the selector you specify,
which is normally read from the minibuffer.

Prefix arg means the new virtual folder should be visited read only."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command)
	 (prefix current-prefix-arg))
     (vm-select-folder-buffer)
     (nconc (vm-read-virtual-selector "Create virtual folder of messages: ")
	    (list prefix))))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let (vm-virtual-folder-alist name)
    (if arg
	(setq name (format "%s %s %s" (buffer-name) selector arg))
      (setq name (format "%s %s" (buffer-name) selector)))
    (setq vm-virtual-folder-alist
	  (list
	   (list name
		 (list (list (current-buffer))
		       (if arg (list selector arg) (list selector))))))
    (vm-visit-virtual-folder name read-only)))

(defun vm-apply-virtual-folder (name &optional read-only)
  "Apply the selectors of a named virtual folder to the current folder
and create a virtual folder containing the selected messages.

Prefix arg means the new virtual folder should be visited read only."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (list
      (completing-read "Apply this virtual folder's selectors: "
		       vm-virtual-folder-alist nil t)
      current-prefix-arg)))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((vfolder (assoc name vm-virtual-folder-alist))
	clauses vm-virtual-folder-alist)
    (or vfolder (error "No such virtual folder, %s" name))
    (setq vfolder (vm-copy vfolder))
    (setq clauses (cdr vfolder))
    (while clauses
      (setcar (car clauses) (list (current-buffer)))
      (setq clauses (cdr clauses)))
    (setcar vfolder (format "%s/%s" (buffer-name) (car vfolder)))
    (setq vm-virtual-folder-alist (list vfolder))
    (vm-visit-virtual-folder (car vfolder) read-only)))

(defun vm-toggle-virtual-mirror ()
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (if (not (eq major-mode 'vm-virtual-mode))
      (error "This is not a virtual folder."))
  (let ((mp vm-message-list)
	(inhibit-quit t)
	modified undo-list)
    (setq undo-list vm-saved-undo-record-list
	  vm-saved-undo-record-list vm-undo-record-list
	  vm-undo-record-list undo-list
	  vm-undo-record-pointer undo-list)
    (setq modified vm-saved-buffer-modified-p
	  vm-saved-buffer-modified-p (buffer-modified-p))
    (set-buffer-modified-p modified)
    (if vm-virtual-mirror
	(while mp
	  (vm-set-attributes-of
	   (car mp) (or (vm-saved-virtual-attributes-of (car mp))
			(make-vector vm-attributes-vector-length nil)))
	  (vm-set-mirror-data-of
	   (car mp) (or (vm-saved-virtual-mirror-data-of (car mp))
			(make-vector vm-mirror-data-vector-length nil)))
	  (vm-mark-for-summary-update (car mp) t)
	  (setq mp (cdr mp)))
      (while mp
	;; mark for summary update _before_ we set this message to
	;; be mirrored.  this will prevent the real message and
	;; the other messages that will share attributes with
	;; this message from having their summaries
	;; updated... they don't need it.
	(vm-mark-for-summary-update (car mp) t)
	(vm-set-saved-virtual-attributes-of
	 (car mp) (vm-attributes-of (car mp)))
	(vm-set-saved-virtual-mirror-data-of
	 (car mp) (vm-mirror-data-of (car mp)))
	(vm-set-attributes-of
	 (car mp) (vm-attributes-of (vm-real-message-of (car mp))))
	(vm-set-mirror-data-of
	 (car mp) (vm-mirror-data-of (vm-real-message-of (car mp))))
	(setq mp (cdr mp))))
    (setq vm-virtual-mirror (not vm-virtual-mirror))
    (vm-increment vm-modification-counter))
  (vm-update-summary-and-mode-line)
  (message "Virtual folder now %s the underlying real folder%s."
	   (if vm-virtual-mirror "mirrors" "does not mirror")
	   (if (cdr vm-real-buffers) "s" "")))

(defun vm-virtual-help ()
  (interactive)
  (vm-display nil nil '(vm-virtual-help) '(vm-virtual-help))
  (message "VV = visit, VA = apply selectors, VC = create, VM = toggle virtual mirror"))

(defun vm-delete-directories (list)
  (vm-delete 'file-directory-p list))

(defun vm-vs-or (m &rest selectors)
  (let ((result nil) selector arglist)
    (while selectors
      (setq selector (car (car selectors))
	    arglist (cdr (car selectors))
	    result (apply (symbol-value selector) m arglist)
	    selectors (if result nil (cdr selectors))))
    result ))

(defun vm-vs-and (m &rest selectors)
  (let ((result t) selector arglist)
    (while selectors
      (setq selector (car (car selectors))
	    arglist (cdr (car selectors))
	    result (apply (symbol-value selector) m arglist)
	    selectors (if (null result) nil (cdr selectors))))
    result ))

(defun vm-vs-not (m arg)
  (let ((selector (car arg))
	(arglist (cdr arg)))
    (not (apply (symbol-value selector) m arglist))))

(defun vm-vs-any (m) t)

(defun vm-vs-author (m arg)
  (or (string-match arg (vm-su-full-name m))
      (string-match arg (vm-su-from m))))

(defun vm-vs-recipient (m arg)
  (or (string-match arg (vm-su-to m))
      (string-match arg (vm-su-to-names m))))

(defun vm-vs-subject (m arg)
  (string-match arg (vm-su-subject m)))

(defun vm-vs-sent-before (m arg)
  (string< (vm-so-sortable-datestring m) (vm-timezone-make-date-sortable arg)))

(defun vm-vs-sent-after (m arg)
  (string< (vm-timezone-make-date-sortable arg) (vm-so-sortable-datestring m)))

(defun vm-vs-header (m arg)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (vm-headers-of m))
      (re-search-forward arg (vm-text-of m) t))))

(defun vm-vs-label (m arg)
  (vm-member arg (vm-labels-of m)))

(defun vm-vs-text (m arg)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (vm-text-of m))
      (re-search-forward arg (vm-text-end-of m) t))))

(defun vm-vs-more-chars-than (m arg)
  (> (string-to-int (vm-su-byte-count m)) arg))

(defun vm-vs-less-chars-than (m arg)
  (< (string-to-int (vm-su-byte-count m)) arg))

(defun vm-vs-more-lines-than (m arg)
  (> (string-to-int (vm-su-line-count m)) arg))

(defun vm-vs-less-lines-than (m arg)
  (< (string-to-int (vm-su-line-count m)) arg))

(defun vm-vs-new (m) (vm-new-flag m))
(defun vm-vs-unread (m) (vm-unread-flag m))
(defun vm-vs-read (m) (not (or (vm-new-flag m) (vm-unread-flag m))))
(defun vm-vs-deleted (m) (vm-deleted-flag m))
(defun vm-vs-replied (m) (vm-replied-flag m))
(defun vm-vs-forwarded (m) (vm-forwarded-flag m))
(defun vm-vs-filed (m) (vm-filed-flag m))
(defun vm-vs-written (m) (vm-written-flag m))
(defun vm-vs-marked (m) (vm-mark-of m))
(defun vm-vs-edited (m) (vm-edited-flag m))

(put 'header 'vm-virtual-selector-clause "with header matching")
(put 'label 'vm-virtual-selector-clause "with label of")
(put 'text 'vm-virtual-selector-clause "with text matching")
(put 'recipient 'vm-virtual-selector-clause "with recipient matching")
(put 'author 'vm-virtual-selector-clause "with author matching")
(put 'subject 'vm-virtual-selector-clause "with subject matching")
(put 'sent-before 'vm-virtual-selector-clause "sent before")
(put 'sent-after 'vm-virtual-selector-clause "sent after")
(put 'more-chars-than 'vm-virtual-selector-clause
     "with more characters than")
(put 'less-chars-than 'vm-virtual-selector-clause
     "with less characters than")
(put 'more-lines-than 'vm-virtual-selector-clause "with more lines than")
(put 'less-lines-than 'vm-virtual-selector-clause "with less lines than")

(defun vm-read-virtual-selector (prompt)
  (let (selector (arg nil))
    (setq selector
	  (completing-read prompt vm-supported-interactive-virtual-selectors
			   nil t)
	  selector (intern selector))
    (if (memq selector '(header label text recipient
			 author subject
			 sent-before sent-after
			 more-chars-than more-lines-than
			 less-chars-than less-lines-than))
	(progn
	  (setq prompt (concat (substring prompt 0 -2) " "
			       (get selector 'vm-virtual-selector-clause)
			       ": "))
	  (cond ((memq selector '(more-chars-than more-lines-than
			          less-chars-than less-lines-than))
		 (setq arg (vm-read-number prompt)))
		((eq selector 'label)
		 (let ((vm-completion-auto-correct nil)
		       (completion-ignore-case t))
		   (setq arg (downcase
			      (vm-read-string
			       prompt
			       (vm-obarray-to-string-list
				vm-label-obarray)
			       nil)))))
		(t (setq arg (read-string prompt))))))
    (list selector arg)))

;; clear away links between real and virtual folders when
;; a vm-quit is performed in either type folder.
(defun vm-virtual-quit ()
  (save-excursion
    (cond ((eq major-mode 'vm-virtual-mode)
	   ;; don't trust blindly, user might have killed some of
	   ;; these buffers.
	   (setq vm-real-buffers (vm-delete 'buffer-name vm-real-buffers t))
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
	   ;; don't trust blindly, user might have killed some of
	   ;; these buffers.
	   (setq vm-virtual-buffers
		 (vm-delete 'buffer-name vm-virtual-buffers t))
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
		 ;; folder by looking for messages that have a "Q"
		 ;; id number associated with them.
		 (vm-set-message-id-number-of (car vmp) "Q")
		 (setq vmp (cdr vmp)))
	       (vm-set-virtual-messages-of (car mp) nil)
	       (setq mp (cdr mp)))
	     (while bp
	       (set-buffer (car bp))
	       (setq vm-real-buffers (delq b vm-real-buffers))
	       ;; set the message pointer to a new value if it is
	       ;; now invalid.
	       (setq vmp vm-message-pointer)
	       (while (and vm-message-pointer
			   (equal "Q" (vm-message-id-number-of
				       (car vm-message-pointer))))
		 (setq vm-message-pointer
		       (cdr vm-message-pointer)))
	       ;; if there were no good messages ahead, try going
	       ;; backward.
	       (if (null vm-message-pointer)
		   (progn
		     (setq vm-message-pointer vmp)
		     (while (and vm-message-pointer
				 (equal "Q" (vm-message-id-number-of
					     (car vm-message-pointer))))
		       (setq vm-message-pointer
			     (vm-reverse-link-of (car vm-message-pointer))))))
	       ;; expunge the virtual messages associated with
	       ;; real messages that are going away.
	       (setq vm-message-list
		     (vm-delete (function
				 (lambda (m)
				   (equal "Q" (vm-message-id-number-of m))))
				vm-message-list nil))
	       (if (null vm-message-pointer)
		   (setq vm-message-pointer vm-message-list))
	       ;; same for vm-last-message-pointer
	       (if (null vm-last-message-pointer)
		   (setq vm-last-message-pointer nil))
	       (vm-clear-virtual-quit-invalidated-undos)
	       (vm-reverse-link-messages)
	       (vm-set-numbering-redo-start-point t)
	       (vm-set-summary-redo-start-point t)
	       (if vm-message-pointer
		   (vm-preview-current-message)
		 (vm-update-summary-and-mode-line))
	       (setq bp (cdr bp))))))))

(defun vm-virtual-save-folder (prefix)
  (save-excursion
    ;; don't trust blindly, user might have killed some of
    ;; these buffers.
    (setq vm-real-buffers (vm-delete 'buffer-name vm-real-buffers t))
    (let ((bp vm-real-buffers))
      (while bp
	(set-buffer (car bp))
	(vm-save-folder prefix)
	(setq bp (cdr bp)))))
  (vm-set-buffer-modified-p nil)
  (vm-clear-modification-flag-undos)
  (vm-update-summary-and-mode-line))

(defun vm-virtual-get-new-mail ()
  (save-excursion
    ;; don't trust blindly, user might have killed some of
    ;; these buffers.
    (setq vm-real-buffers (vm-delete 'buffer-name vm-real-buffers t))
    (let ((bp vm-real-buffers))
      (while bp
	(set-buffer (car bp))
	(condition-case error-data
	    (vm-get-new-mail)
	  (folder-read-only
	   (message "Folder is read only: %s"
		    (or buffer-file-name (buffer-name)))
	   (sit-for 1))
	  (unrecognized-folder-type
	   (message "Folder type is unrecognized: %s"
		    (or buffer-file-name (buffer-name)))
	   (sit-for 1)))
	(setq bp (cdr bp)))))
  (vm-emit-totals-blurb))

(defun vm-make-virtual-copy (m)
  (widen)
  (let ((virtual-buffer (current-buffer))
	(real-m (vm-real-message-of m))
	(buffer-read-only nil)
	(modified (buffer-modified-p)))
    (unwind-protect
	(save-excursion
	  (set-buffer (vm-buffer-of real-m))
	  (save-restriction
	    (widen)
	    ;; must reference this now so that headers will be in
	    ;; their final position before the message is copied.
	    ;; otherwise the vheader offset computed below will be wrong.
	    (vm-vheaders-of real-m)
	    (copy-to-buffer virtual-buffer (vm-start-of real-m)
			    (vm-end-of real-m))))
      (set-buffer-modified-p modified))
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
