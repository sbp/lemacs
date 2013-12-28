;;; VM folder related functions
;;; Copyright (C) 1989, 1990, 1991, 1993 Kyle E. Jones
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

(defun vm-number-messages (&optional start-point end-point)
  (let ((n 1) (message-list (or start-point vm-message-list)))
    (if (and start-point (vm-reverse-link-of (car start-point)))
	(setq n (1+ (string-to-int
		     (vm-number-of
		      (car
		       (vm-reverse-link-of
			(car start-point))))))))
    (while (not (eq message-list end-point))
      (vm-set-number-of (car message-list) (int-to-string n))
      (setq n (1+ n) message-list (cdr message-list)))
    (or end-point (setq vm-ml-highest-message-number (int-to-string (1- n))))
    (if vm-summary-buffer
	(vm-copy-local-variables vm-summary-buffer
				 'vm-ml-highest-message-number))))

(defun vm-set-numbering-redo-start-point (start-point)
  (if (and (consp start-point) (consp vm-numbering-redo-start-point))
      (let ((mp vm-message-list))
	(while (not (or (eq mp start-point)
			(eq mp vm-numbering-redo-start-point)))
	  (setq mp (cdr mp)))
	(if (null mp)
	    (error "Something is wrong in vm-set-numbering-redo-start-point"))
	(if (eq mp start-point)
	    (setq vm-numbering-redo-start-point start-point)))
    (setq vm-numbering-redo-start-point start-point)))

(defun vm-set-numbering-redo-end-point (end-point)
  (cond ((eq end-point t)
	 (setq vm-numbering-redo-end-point t))
	((and (consp end-point)
	      (> (string-to-int
		  (vm-number-of
		   (car end-point)))
		 (string-to-int
		  (vm-number-of
		   (car vm-numbering-redo-end-point)))))
	 (setq vm-numbering-redo-end-point end-point))
	((null end-point)
	 (setq vm-numbering-redo-end-point end-point))))

(defun vm-do-needed-renumbering ()
  (if vm-numbering-redo-start-point
      (progn
	(vm-number-messages (and (consp vm-numbering-redo-start-point)
				 vm-numbering-redo-start-point)
			    vm-numbering-redo-end-point)
	(setq vm-numbering-redo-start-point nil
	      vm-numbering-redo-end-point nil))))

(defun vm-reverse-link-messages ()
  (let ((mp vm-message-list)
	(prev nil))
    (while mp
      (vm-set-reverse-link-of (car mp) prev)
      (setq prev mp mp (cdr mp)))))

;; used by header ordering code
;; alist looks like this (("From") ("To"))
;; this function returns the alist element whose car matches the
;; header starting at point
;; the header ordering code typically uses the cdr of the element
;; returned to hold headers to be output later.
(defun vm-match-ordered-header (alist)
  (let ((case-fold-search t))
    (catch 'match
      (while alist
	(if (looking-at (car (car alist)))
	    (throw 'match (car alist)))
	(setq alist (cdr alist)))
      nil)))

;; match a header and init a vector containing the start and end
;; points of the match segments.  Vector looks like this
;;
;; [ header-start header-end
;;   header-name-start header-name-end
;;   header-contents-start header-contents-end ]
;;
;; Elements are integers.
;; There are functions to access and use this info.
(defun vm-match-header (&optional header-name)
  (let ((case-fold-search t)
	(header-name-regexp "\\([^ \t\n:]+\\):"))
    (if (if header-name
	    (and (looking-at header-name) (looking-at header-name-regexp))
	  (looking-at header-name-regexp))
	(save-excursion
	  (aset vm-matched-header-vector 0 (point))
	  (aset vm-matched-header-vector 2 (point))
	  (aset vm-matched-header-vector 3 (match-end 1))
	  (goto-char (match-end 0))
	  ;; skip leading whitespace
	  (skip-chars-forward " \t")
	  (aset vm-matched-header-vector 4 (point))
	  (forward-line 1)
	  (while (looking-at "[ \t]")
	    (forward-line 1))
	  ;; drop the trailing newline
	  (aset vm-matched-header-vector 1 (point))
	  (aset vm-matched-header-vector 5 (1- (point)))))))

(defun vm-matched-header ()
  (buffer-substring (aref vm-matched-header-vector 0)
		    (aref vm-matched-header-vector 1)))

(defun vm-matched-header-name ()
  (buffer-substring (aref vm-matched-header-vector 2)
		    (aref vm-matched-header-vector 3)))

(defun vm-matched-header-contents ()
  (buffer-substring (aref vm-matched-header-vector 4)
		    (aref vm-matched-header-vector 5)))

(defun vm-matched-header-start ()
  (aref vm-matched-header-vector 0))

(defun vm-matched-header-end ()
  (aref vm-matched-header-vector 1))

;; return folder type
;;  nil if folder has no type (empty)
;; 'unknown if the type is not known to VM
(defun vm-get-folder-type (&optional file)
  (let ((temp-buffer nil)
	b
	(case-fold-search nil))
    (unwind-protect
	(save-excursion
	  (if file
	      (progn
		(setq b (get-file-buffer file))
		(if b
		    (set-buffer b)
		  (setq temp-buffer (generate-new-buffer "*vm-work*"))
		  (set-buffer temp-buffer)
		  (if (file-readable-p file)
		      (if (or (vm-fsf-emacs-19-p) (vm-lucid-emacs-p))
			  (insert-file-contents file nil 0 80)
			(call-process "sed" file temp-buffer nil "1q"))))))
	  (save-excursion
	    (save-restriction
	      (widen)
	      (goto-char (point-min))
	      ;; i've seen folders that start with a blank line
	      (skip-chars-forward "\n")
	      (cond ((zerop (buffer-size)) nil)
		    ((looking-at "From ") 'From_)
		    ((looking-at "\001\001\001\001\n") 'mmdf)
		    (t 'unknown)))))
      (and temp-buffer (kill-buffer temp-buffer)))))

(defun vm-convert-folder-type (old-type new-type)
  (let ((vm-folder-type old-type)
	start)
    (goto-char (point-min))
    (while (vm-find-leading-message-separator)
      (setq start (point))
      (vm-skip-past-leading-message-separator)
      (delete-region start (point))
      (insert (vm-leading-message-separator new-type))
      (vm-find-trailing-message-separator)
      (setq start (point))
      (vm-skip-past-trailing-message-separator)
      (delete-region start (point))
      (insert (vm-trailing-message-separator)))))

(defun vm-compatible-folder-p (file)
  (let ((type (vm-get-folder-type file)))
    (or (not (and vm-folder-type type))
	(eq vm-folder-type type))))

(defun vm-leading-message-separator (&optional folder-type)
  (let ((type (or folder-type vm-folder-type)))
    (cond ((eq folder-type 'From_)
	   (concat "From VM " (current-time-string) "\n"))
	  ((eq folder-type 'mmdf)
	   "\001\001\001\001\n"))))

(defun vm-trailing-message-separator (&optional folder-type)
  (let ((type (or folder-type vm-folder-type)))
    (cond ((eq folder-type 'From_) "\n")
	  ((eq folder-type 'mmdf) "\001\001\001\001\n"))))

;; returns non-nil if the separator is found
(defun vm-find-leading-message-separator ()
  (cond
   ((eq vm-folder-type 'From_)
    (let ((reg1 "^From ")
	  (case-fold-search nil))
      (catch 'done
	(while (re-search-forward reg1 nil 'no-error)
	  (goto-char (match-beginning 0))
	  (if (or (bobp) (equal (char-after (- (point) 2)) ?\n))
	      (throw 'done t)
	    (forward-char 1)))
	nil )))
   ((eq vm-folder-type 'mmdf)
    (let ((reg1 "^\001\001\001\001")
	  (case-fold-search nil))
      (if (re-search-forward reg1 nil 'no-error)
	  (progn
	    (goto-char (match-beginning 0))
	    t )
	nil )))))

(defun vm-find-trailing-message-separator ()
  (cond
   ((eq vm-folder-type 'From_)
    (vm-find-leading-message-separator)
    (forward-char -1))
   ((eq vm-folder-type 'mmdf)
    (vm-find-leading-message-separator))))

(defun vm-skip-past-leading-message-separator ()
  (cond
   ((eq vm-folder-type 'From_)
    (let ((reg1 "^>From ")
	  (case-fold-search nil))
      (forward-line 1)
      (while (looking-at reg1)
	(forward-line 1))))
   ((eq vm-folder-type 'mmdf)
    (forward-char 5))))

(defun vm-skip-past-trailing-message-separator ()
  (cond
   ((eq vm-folder-type 'From_)
    (forward-char 1))
   ((eq vm-folder-type 'mmdf)
    (forward-char 5))))

;; Build a chain of message structures.
;; Find the start and end of each message and fill in the relevant
;; fields in the message structures.
;; vm-text-of and vm-vheaders-of field don't get filled until they are needed
(defun vm-build-message-list ()
  (setq vm-folder-type (vm-get-folder-type))
  (save-excursion
    (let ((tail-cons nil)
	  (n 0)
	  ;; Just for yucks, make the update interval vary.
	  (modulus (+ (% (vm-abs (random)) 11) 25))
	  message)
      (if vm-message-list
	  ;; there are already messages, therefore we're supposed
	  ;; to add to this list.
	  (let ((mp vm-message-list)
		(end (point-min)))
	    ;; first we have to find physical end of the folder
	    ;; prior to the new messages that just came in.
	    (while mp
	      (if (< end (vm-end-of (car mp)))
		  (setq end (vm-end-of (car mp))))
	      (if (not (consp (cdr mp)))
		  (setq tail-cons mp))
	      (setq mp (cdr mp)))
	    (goto-char end))
	;; there are no messages so we're building the whole list.
	;; start from the beginnig of the folder.
	(goto-char (point-min)))
      ;; parse the messages, set the markers that specify where
      ;; things are.
      (while (vm-find-leading-message-separator)
	(setq message (vm-make-message))
	(vm-set-message-type-of message vm-folder-type)
	(vm-set-start-of message (vm-marker (point)))
	(vm-skip-past-leading-message-separator)
	(vm-set-headers-of message (vm-marker (point)))
	(vm-find-trailing-message-separator)
	(vm-set-text-end-of message (vm-marker (point)))
	(vm-skip-past-trailing-message-separator)
	(vm-set-end-of message (vm-marker (point)))
	(vm-set-reverse-link-of message tail-cons)
	(if (null tail-cons)
	    (setq vm-message-list (list message)
		  tail-cons vm-message-list)
	  (setcdr tail-cons (list message))
	  (setq tail-cons (cdr tail-cons)))
	(vm-increment n)
	(if (zerop (% n modulus))
	    (message "Parsing messages... %d" n)))
      (if (>= n modulus)
	  (message "Parsing messages... done")))))

(defun vm-build-header-order-alist (vheaders)
  (let ((order-alist (cons nil nil))
	list)
    (setq list order-alist)
    (while vheaders
      (setcdr list (cons (cons (car vheaders) nil) nil))
      (setq list (cdr list) vheaders (cdr vheaders)))
    (cdr order-alist)))

;; Reorder the headers in a message.
;;
;; If a message struct is passed into this function, then we're
;; operating on a message in a folder buffer.  Headers are
;; grouped so that the headers that the user wants to see are at
;; the end of the headers section so we can narrow to them.  This
;; is done according to the preferences specified in
;; vm-visible-header and vm-invisible-header-regexp.  The
;; vheaders field of the message struct is also set.  This
;; function is called on demand whenever a vheaders field is
;; discovered to be nil for a particular message.
;;
;; If the message argument is nil, then we are operating on a
;; freestanding message that is not part of a folder buffer.  The
;; keep-list and discard-regexp parameters are used in this case.
;; Headers not matched by the keep list or matched by the discard
;; list are stripped form the message.  The remaining headers
;; are ordered according to the order of the keep list.

(defun vm-reorder-message-headers (message keep-list discard-regexp)
  (save-excursion
    (if message
	(progn
	  (set-buffer (marker-buffer (vm-start-of message)))
	  (setq keep-list vm-visible-headers
		discard-regexp vm-invisible-header-regexp)))
    (save-excursion
      (save-restriction
	(widen)
	;; if there is a cached regexp that points to the already
	;; ordered headers then use it and avoid a lot of work.
	(if (and message (vm-vheaders-regexp-of message))
	    (save-excursion
	      (goto-char (vm-headers-of message))
	      (let ((case-fold-search t))
		(re-search-forward (vm-vheaders-regexp-of message)
				   (vm-text-of message) t))
	      (vm-set-vheaders-of message (vm-marker (match-beginning 0))))
	  ;; oh well, we gotta do it the hard way.
	  ;;
	  ;; header-alist will contain an assoc list version of
	  ;; keep-list.  For messages associated with a folder
	  ;; buffer:  when a matching header is found, the header
	  ;; is stuffed into its corresponding assoc cell and the
	  ;; header text is deleted from the buffer.  After all
	  ;; the visible headers have been collected, they are
	  ;; inserted into the buffer in a clump at the end of
	  ;; the header section.  Unmatched headers are skipped over.
	  ;;
	  ;; For free standing messages, unmatched headers are
	  ;; stripped from the message.
	  (vm-save-restriction
	   (let ((header-alist (vm-build-header-order-alist keep-list))
		 (buffer-read-only nil)
		 (work-buffer nil)
		 (extras nil)
		 list end-of-header vheader-offset
		 (folder-buffer (current-buffer))
		 ;; This prevents file locking from occuring.  Disabling
		 ;; locking can speed things noticeably if the lock directory
		 ;; is on a slow device.  We don't need locking here because
		 ;; in a mail context reordering headers is harmless.
		 (buffer-file-name nil)
		 (case-fold-search t)
		 (old-buffer-modified-p (buffer-modified-p)))
	     (unwind-protect
		 (progn
		   (if message
		       (progn
			 (setq work-buffer (generate-new-buffer "*vm-work*"))
			 (set-buffer work-buffer)
			 (insert-buffer-substring
			  folder-buffer 
			  (vm-headers-of message)
			  (vm-text-of message))
			 (goto-char (point-min))))
		   (while (and (not (= (following-char) ?\n))
			       (vm-match-header))
		     (setq end-of-header (vm-matched-header-end)
			   list (vm-match-ordered-header header-alist))
		     ;; don't display/keep this header if
		     ;;  keep-list not matched
		     ;;  and discard-regexp is nil
		     ;;       or
		     ;;  discard-regexp is matched
		     (if (or (and (null list) (null discard-regexp))
			     (and discard-regexp (looking-at discard-regexp)))
			 ;; skip the unwanted header if doing
			 ;; work for a folder buffer otherwise
			 ;; discard the header.
			 (if message
			     (goto-char end-of-header)
			   (delete-region (point) end-of-header))
		       ;; got a match
		       ;; stuff the header into the cdr of the
		       ;; returned alist element
		       (if list
			   (if (cdr list)
			       (setcdr list 
				       (concat
					(cdr list)
					(buffer-substring (point)
							  end-of-header)))
			     (setcdr list (buffer-substring (point)
							    end-of-header)))
			 (setq extras
			       (cons (buffer-substring (point) end-of-header)
				     extras)))
		       (delete-region (point) end-of-header)))
		   ;; remember the offset of where the visible
		   ;; header start so we can initialize the
		   ;; vm-vheaders-of field later.
		   (if message
		       (setq vheader-offset (1- (point))))
		   ;; now dump out the headers we saved.
		   ;; the keep-list headers go first.
		   (setq list header-alist)
		   (while list
		     (if (cdr (car list))
			 (progn
			   (insert (cdr (car list)))
			   (setcdr (car list) nil)))
		     (setq list (cdr list)))
		   ;; now the headers that were not explicitly
		   ;; undesirable, if any.
		   (if extras
		       (progn
			 (setq extras (nreverse extras))
			 (while extras
			   (insert (car extras))
			   (setq extras (cdr extras)))))
		   ;; update the folder buffer if we're supposed to.
		   ;; lock out interrupts.
		   (if message
		       (let ((inhibit-quit t))
			 (set-buffer (marker-buffer (vm-start-of message)))
			 (goto-char (vm-headers-of message))
			 (insert-buffer-substring work-buffer)
			 (delete-region (point) (vm-text-of message))
			 (set-buffer-modified-p old-buffer-modified-p))))
	       (and work-buffer (kill-buffer work-buffer)))
	     (if message
		 (progn
		   (vm-set-vheaders-of message
				       (vm-marker (+ (vm-headers-of message)
						     vheader-offset)))
		   ;; cache a regular expression that can be used to
		   ;; find the start of the reordered header the next
		   ;; time this folder is visited.
		   (goto-char (vm-vheaders-of message))
		   (if (vm-match-header)
		       (vm-set-vheaders-regexp-of
			message
			(concat "^" (vm-matched-header-name) ":"))))))))))))

;; Reads the message attributes and cached header information from the
;; header portion of the each message, if our X-VM- attributes header is
;; present.  If the header is not present, assume the message is new,
;; unless we are being compatible with Berkeley Mail in which case we
;; also check for a Status header.
;;
;; If a message already has attributes don't bother checking the
;; headers.
;;
;; This function also discovers and stores the position where the
;; message text begins.
;;
;; Totals are gathered for use by vm-emit-totals-blurb.
;;
;; Supports version 4 format of attribute storage, for backward compatibility.

(defun vm-read-attributes (message-list)
  (save-excursion
    (let ((mp (or message-list vm-message-list))
	  (vm-new-count 0)
	  (vm-unread-count 0)
	  (vm-deleted-count 0)
	  (vm-total-count 0)
	  (modulus (+ (% (vm-abs (random)) 11) 25))
	  (case-fold-search t)
	  data)
      (while mp
	(vm-increment vm-total-count)
	(if (vm-attributes-of (car mp))
	    ()
	  (goto-char (vm-start-of (car mp)))
	  ;; find start of text section and save it
	  (search-forward "\n\n" (vm-text-end-of (car mp)) 0)
	  (vm-set-text-of (car mp) (point-marker))
	  ;; now look for our header
	  (goto-char (vm-start-of (car mp)))
	  (cond
	   ((re-search-forward vm-attributes-header-regexp
			       (vm-text-of (car mp)) t)
	    (goto-char (match-beginning 2))
	    (condition-case ()
		(setq data (read (current-buffer)))
	      (error (setq data
			   (list
			    (make-vector vm-attributes-vector-length nil)
			    (make-vector vm-cache-vector-length nil)))
		     ;; In lieu of a valid attributes header
		     ;; assume the message is new.
		     (aset (car data) 0 t)))
	    ;; support version 4 format
	    (cond ((vectorp data)
		   (setq data (vm-convert-v4-attributes data)))
		  (t
		   ;; extend vectors if necessary to accomodate
		   ;; more caching and attributes without alienating
		   ;; other version 5 folders.
		   (cond ((< (length (car data))
			     vm-attributes-vector-length)
			  (setcar data (vm-extend-vector
					(car data)
					vm-attributes-vector-length))))
		   (cond ((< (length (car (cdr data)))
			     vm-cache-vector-length)
			  (setcar (cdr data)
				  (vm-extend-vector
				   (car (cdr data))
				   vm-cache-vector-length))))))
	    (vm-set-cache-of (car mp) (car (cdr data)))
	    (vm-set-attributes-of (car mp) (car data)))
	   ((and vm-berkeley-mail-compatibility
		 (re-search-forward vm-berkeley-mail-status-header-regexp
				    (vm-text-of (car mp)) t))
	    (vm-set-cache-of (car mp) (make-vector vm-cache-vector-length
						   nil))
	    (goto-char (match-beginning 1))
	    (vm-set-attributes-of
	     (car mp)
	     (make-vector vm-attributes-vector-length nil))
	    (vm-set-unread-flag (car mp) (not (looking-at ".*R.*")) t))
	   (t
	    (vm-set-cache-of (car mp) (make-vector vm-cache-vector-length
						   nil))
	    (vm-set-attributes-of
	     (car mp)
	     (make-vector vm-attributes-vector-length nil))
	    (vm-set-new-flag (car mp) t t))))
	(cond ((vm-deleted-flag (car mp))
	       (vm-increment vm-deleted-count))
	      ((vm-new-flag (car mp))
	       (vm-increment vm-new-count))
	      ((vm-unread-flag (car mp))
	       (vm-increment vm-unread-count)))
	(if (zerop (% vm-total-count modulus))
	    (message "Reading attributes... %d" vm-total-count))
	(setq mp (cdr mp)))
      (if (>= vm-total-count modulus)
	  (message "Reading attributes... done"))
      (if (null message-list)
	  (setq vm-totals (list vm-modification-counter
				vm-total-count
				vm-new-count
				vm-unread-count
				vm-deleted-count))))))

(defun vm-emit-totals-blurb ()
  (save-excursion
    (vm-select-folder-buffer)
    (if (not (equal (nth 0 vm-totals) vm-modification-counter))
	(let ((mp vm-message-list)
	      (vm-new-count 0)
	      (vm-unread-count 0)
	      (vm-deleted-count 0)
	      (vm-total-count 0))
	  (while mp
	    (vm-increment vm-total-count)
	    (cond ((vm-deleted-flag (car mp))
		   (vm-increment vm-deleted-count))
		  ((vm-new-flag (car mp))
		   (vm-increment vm-new-count))
		  ((vm-unread-flag (car mp))
		   (vm-increment vm-unread-count)))
	    (setq mp (cdr mp)))
	  (setq vm-totals (list vm-modification-counter
				vm-total-count
				vm-new-count
				vm-unread-count
				vm-deleted-count))))
    (if (equal (nth 1 vm-totals) 0)
	(message "No messages.")
      (message "%d message%s, %d new, %d unread, %d deleted"
	       (nth 1 vm-totals) (if (= (nth 1 vm-totals) 1) "" "s")
	       (nth 2 vm-totals)
	       (nth 3 vm-totals)
	       (nth 4 vm-totals)))))

(defun vm-convert-v4-attributes (data)
  (list (apply 'vector
	       (nconc (vm-vector-to-list data)
		      (make-list (- vm-attributes-vector-length
				    (length data))
				 nil)))
	(make-vector vm-cache-vector-length nil)))

;; Go to the message specified in a bookmark and eat the bookmark.
;; Returns non-nil if successful, nil otherwise.
(defun vm-gobble-bookmark ()
  (let ((old-buffer-modified-p (buffer-modified-p))
	n
	(case-fold-search t)
	(buffer-read-only nil)
	;; This prevents file locking from occuring.  Disabling
	;; locking can speed things noticeably if the lock
	;; directory is on a slow device.  We don't need locking
	;; here because the user shouldn't care about VM removing
	;; its own status headers.
	(buffer-file-name nil))
    (save-excursion
      (vm-save-restriction
       (let (lim)
	 (widen)
	 (goto-char (point-min))
	 (search-forward "\n\n" nil t)
	 (setq lim (point))
	 (goto-char (point-min))
	 (if (re-search-forward vm-bookmark-header-regexp lim t)
	     (progn
	       (setq n (read (current-buffer)))
	       (goto-char (match-beginning 0))
	       (vm-match-header vm-bookmark-header)
	       (delete-region (vm-matched-header-start)
			      (vm-matched-header-end)))))))
    (set-buffer-modified-p old-buffer-modified-p)
    (if n
	(vm-record-and-change-message-pointer vm-message-pointer
					      (nthcdr (1- n) vm-message-list)))
    t ))

(defun vm-check-header-variables ()
  (save-excursion
    (vm-save-restriction
     (let ((case-fold-search t)
	   lim)
       (widen)
       (goto-char (point-min))
       (search-forward "\n\n" nil t)
       (setq lim (point))
       (goto-char (point-min))
       (if (re-search-forward vm-vheader-header-regexp lim t)
	   (let ((old-buffer-modified-p (buffer-modified-p))
		 ;; This prevents file locking from occuring.  Disabling
		 ;; locking can speed things noticeably if the lock
		 ;; directory is on a slow device.  We don't need locking
		 ;; here because the user shouldn't care about VM removing
		 ;; its own status headers.
		 (buffer-file-name nil)
		 (buffer-read-only nil)
		 vis invis got)
	     (condition-case ()
		 (setq vis (read (current-buffer))
		       invis (read (current-buffer))
		       got t)
	       (error nil))
	     (goto-char (match-beginning 0))
	     (vm-match-header vm-vheader-header)
	     (delete-region (vm-matched-header-start)
			    (vm-matched-header-end))
	     ;; if the variables don't match the values stored when this
	     ;; folder was saved, then we have to discard any cached
	     ;; vheader info so the user will see the right headers.
	     (and got (or (not (equal vis vm-visible-headers))
			  (not (equal invis vm-invisible-header-regexp)))
		  (let ((mp vm-message-list))
		    (message "Discarding visible header info...")
		    (while mp
		      (vm-set-vheaders-regexp-of (car mp) nil)
		      (vm-set-vheaders-of (car mp) nil)
		      (setq mp (cdr mp)))))
	     (set-buffer-modified-p old-buffer-modified-p)))))))

;; Read and delete the header that gives the folder's desired
;; message order.
(defun vm-gobble-message-order ()
  (let ((old-buffer-modified-p (buffer-modified-p))
	(case-fold-search t)
	(buffer-read-only nil)
	lim v order
	;; This prevents file locking from occuring.  Disabling
	;; locking can speed things noticeably if the lock
	;; directory is on a slow device.  We don't need locking
	;; here because the user shouldn't care about VM removing
	;; its own status headers.
	(buffer-file-name nil)
	(mp vm-message-list)
	(list-length (length vm-message-list)))
    (save-excursion
      (vm-save-restriction
       (widen)
       (goto-char (point-min))
       (search-forward "\n\n" nil t)
       (setq lim (point))
       (goto-char (point-min))
       (if (re-search-forward vm-message-order-header-regexp lim t)
	   (progn
	     (message "Reordering messages...")
	     (setq order (read (current-buffer))
		   v (make-vector (max list-length (length order)) nil))
	     (goto-char (match-beginning 0))
	     ;; have to do the delete this way because the length of the
	     ;; message order header sometimes makes Emacs'
	     ;; regexp matcher overflow.  vm-match-header uses
	     ;; simple patterns iteratively to match long headers
	     ;; and thereby avoids the problem.
	     (vm-match-header vm-message-order-header)
	     (delete-region (vm-matched-header-start) (vm-matched-header-end))
	     (while (and order mp)
	       (aset v (1- (car order)) (car mp))
	       (setq order (cdr order) mp (cdr mp)))
	     ;; lock out interrupts while the mesage list is in
	     ;; an inconsistent state.
	     (let ((inhibit-quit t))
	       (setq vm-message-list (delq nil (append v mp))
		     vm-message-order-changed t
		     vm-message-order-stuffed nil
		     vm-message-pointer (memq (car vm-message-pointer)
					      vm-message-list))
	       (vm-set-numbering-redo-start-point t)
	       (vm-reverse-link-messages))))))
    (set-buffer-modified-p old-buffer-modified-p)))

;; Stuff the message attributes back into the message as headers.
(defun vm-stuff-attributes (m &optional suppress-delete)
  (save-excursion
    (vm-save-restriction
     (widen)
     (let ((old-buffer-modified-p (buffer-modified-p))
	   attributes cache
	   (case-fold-search t)
	   (buffer-read-only nil)
	   opoint
	   ;; This prevents file locking from occuring.  Disabling
	   ;; locking can speed things noticeably if the lock
	   ;; directory is on a slow device.  We don't need locking
	   ;; here because the user shouldn't care about VM stuffing
	   ;; its own status headers.
	   (buffer-file-name nil)
	   (delflag (vm-deleted-flag m)))
       (setq attributes (vm-attributes-of m)
	     cache (vm-cache-of m))
       (and delflag suppress-delete
	    (vm-set-deleted-flag-in-vector (setq attributes (copy-sequence attributes)) nil))
       (goto-char (vm-headers-of m))
       (if (re-search-forward vm-attributes-header-regexp
			      (vm-text-of m) t)
	   (delete-region (match-beginning 0) (match-end 0)))
       (setq opoint (point))
       (insert-before-markers
	vm-attributes-header " ("
	(let ((print-escape-newlines t))
	  (prin1-to-string attributes))
	"\n\t"
	(let ((print-escape-newlines t))
	  (prin1-to-string cache))
	")\n")
       ;; If headers and vheaders markers are clumped fix headers
       ;; marker.
       (if (= (vm-headers-of m) (vm-vheaders-of m))
	   (set-marker (vm-headers-of m) opoint))
       (cond (vm-berkeley-mail-compatibility
	      (goto-char (vm-headers-of m))
	      (if (re-search-forward vm-berkeley-mail-status-header-regexp
				     (vm-text-of m) t)
		  (delete-region (match-beginning 0) (match-end 0)))
	      (cond ((not (vm-new-flag m))
		     (insert
		      vm-berkeley-mail-status-header
		      (if (vm-unread-flag m) "" "R")
		      "O\n")))))
       (vm-set-modflag-of m nil)
       (set-buffer-modified-p old-buffer-modified-p)))))

(defun vm-stuff-virtual-attributes (message)
  (let ((virtual (vm-virtual-message-p message)))
    (if (or (not virtual) (and virtual (vm-virtual-messages-of message)))
	(save-excursion
	  (set-buffer
	   (marker-buffer
	    (vm-start-of
	     (vm-real-message-of message))))
	  (vm-stuff-attributes message)))))

;; Insert a bookmark into the first message in the folder.
(defun vm-stuff-bookmark ()
  (if vm-message-pointer
      (save-excursion
	(vm-save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticeably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       (buffer-read-only nil)
	       lim)
	   (goto-char (point-min))
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-leading-message-separator)
	   (if (re-search-forward vm-bookmark-header-regexp lim t)
	       (progn (goto-char (match-beginning 0))
		      (if (vm-match-header vm-bookmark-header)
			  (delete-region (vm-matched-header-start)
					 (vm-matched-header-end)))))
	   ;; To insert or to insert-before-markers, that is the question.
	   ;;
	   ;; If we insert-before-markers we push a header behind
	   ;; vm-headers-of, which is clearly undesirable.  So we
	   ;; just insert.  This will cause the bookmark header
	   ;; to be visible if there are no non-visible headers,
	   ;; oh well, no way around this.
	   (insert vm-bookmark-header " "
		   (vm-number-of (car vm-message-pointer))
		   "\n")
	   (set-buffer-modified-p old-buffer-modified-p))))))

;; stuff the current values of the header variables for future messages.
(defun vm-stuff-header-variables ()
  (if vm-message-pointer
      (save-excursion
	(vm-save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       (print-escape-newlines t)
	       lim
	       (buffer-read-only nil)
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticeably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil))
	   (goto-char (point-min))
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-leading-message-separator)
	   (if (re-search-forward vm-vheader-header-regexp lim t)
	       (progn (goto-char (match-beginning 0))
		      (if (vm-match-header vm-vheader-header)
			  (delete-region (vm-matched-header-start)
					 (vm-matched-header-end)))))
	   ;; To insert or to insert-before-markers, that is the question.
	   ;;
	   ;; If we insert-before-markers we push a header behind
	   ;; vm-headers-of, which is clearly undesirable.  So we
	   ;; just insert.  This header will be visible if there
	   ;; are no non-visible headers, oh well, no way around this.
	   (insert vm-vheader-header " "
		   (prin1-to-string vm-visible-headers) " "
		   (prin1-to-string vm-invisible-header-regexp)
		   "\n")
	   (set-buffer-modified-p old-buffer-modified-p))))))

;; Insert a header into the first message of the folder that lists
;; the folder's message order.
(defun vm-stuff-message-order ()
  (if (cdr vm-message-list)
      (save-excursion
	(vm-save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       (case-fold-search t)
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticeably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       lim n
	       (buffer-read-only nil)
	       (mp (copy-sequence vm-message-list)))
	   (setq mp
		 (sort mp
		       (function
			(lambda (p q)
			  (< (vm-start-of p) (vm-start-of q))))))
	   (goto-char (point-min))
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (vm-skip-past-leading-message-separator)
	   (if (re-search-forward vm-message-order-header-regexp lim t)
	       (progn (goto-char (match-beginning 0))
		      (if (vm-match-header vm-message-order-header)
			  (delete-region (vm-matched-header-start)
					 (vm-matched-header-end)))))
	   ;; To insert or to insert-before-markers, that is the question.
	   ;;
	   ;; If we insert-before-markers we push a header behind
	   ;; vm-headers-of, which is clearly undesirable.  So we
	   ;; just insert.  This header will be visible if there
	   ;; are no non-visible headers, oh well, no way around this.
	   (insert vm-message-order-header "\n\t(")
	   (setq n 0)
	   (while mp
	     (insert (vm-number-of (car mp)))
	     (setq n (1+ n) mp (cdr mp))
	     (and mp (insert
		      (if (zerop (% n 15))
			  "\n\t "
			" "))))
	   (insert ")\n")
	   (setq vm-message-order-stuffed t)
	   (set-buffer-modified-p old-buffer-modified-p))))))

(defun vm-change-all-new-to-unread ()
  (let ((mp vm-message-list))
    (while mp
      (if (vm-new-flag (car mp))
	  (progn
	    (vm-set-new-flag (car mp) nil)
	    (vm-set-unread-flag (car mp) t)))
      (setq mp (cdr mp)))))

(defun vm-unread-message (&optional count)
  "Set the `unread' attribute for the current message.  If the message is
already new or unread, then it is left unchanged.

Numeric prefix argument N means to unread the current message plus the
next N-1 messages.  A negative N means unread the current message and
the previous N-1 messages.

When invoked on marked messages (via vm-next-command-uses-marks),
all marked messages are affected, other messages are ignored."
  (interactive "p")
  (or count (setq count 1))
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((mlist (vm-select-marked-or-prefixed-messages count)))
    (while mlist
      (if (and (not (vm-unread-flag (car mlist)))
	       (not (vm-new-flag (car mlist))))
	  (vm-set-unread-flag (car mlist) t))
      (setq mlist (cdr mlist))))
  (vm-update-summary-and-mode-line))

(defun vm-quit-no-change ()
  "Exit VM without saving changes made to the folder."
  (interactive)
  (vm-quit t))

(defun vm-quit (&optional no-change)
  "Quit VM, saving changes and expunging deleted messages."
  (interactive)
  (vm-select-folder-buffer)
  (if (not (memq major-mode '(vm-mode vm-virtual-mode)))
      (error "%s must be invoked from a VM buffer." this-command))
  (vm-check-for-killed-summary)
  (let ((virtual (eq major-mode 'vm-virtual-mode)))
    (cond
     ((and (not virtual) no-change (buffer-modified-p)
	   (not (zerop vm-messages-not-on-disk))
	   ;; Folder may have been saved with C-x C-s and attributes may have
	   ;; been changed after that; in that case vm-messages-not-on-disk
	   ;; would not have been zeroed.  However, all modification flag
	   ;; undos are cleared if VM actually modifies the folder buffer
	   ;; (as opposed to the folder's attributes), so this can be used
	   ;; to verify that there are indeed unsaved messages.
	   (null (assq 'vm-set-buffer-modified-p vm-undo-record-list))
	   (not
	    (y-or-n-p
	     (format
	      "%d message%s have not been saved to disk, quit anyway? "
	      vm-messages-not-on-disk
	      (if (= 1 vm-messages-not-on-disk) "" "s")))))
      (error "Aborted"))
     ((and (not virtual)
	   no-change (buffer-modified-p) vm-confirm-quit
	   (not (y-or-n-p "There are unsaved changes, quit anyway? ")))
      (error "Aborted"))
     ((and (eq vm-confirm-quit t)
	   (not (y-or-n-p "Do you really want to quit? ")))
      (error "Aborted")))
    (vm-virtual-quit)
    (if (and (not no-change) (not virtual))
	(progn
	  ;; this could take a while, so give the user some feedback
	  (message "Quitting...")
	  (or vm-folder-read-only (eq major-mode 'vm-virtual-mode)
	      (vm-change-all-new-to-unread))))
    (if (and (buffer-modified-p) (not no-change) (not virtual))
	(vm-save-folder t))
    (message "")
    (let ((summary-buffer vm-summary-buffer)
	  (mail-buffer (current-buffer)))
      (if summary-buffer
	  (progn
	    (if (eq vm-mutable-windows t)
		(delete-windows-on vm-summary-buffer))
	    (kill-buffer summary-buffer)))
      (set-buffer mail-buffer)
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    (vm-update-summary-and-mode-line)))

(defun vm-start-itimers-if-needed ()
  (if (or (integerp vm-flush-interval)
	  (integerp vm-auto-get-new-mail))
      (progn
	(if (null
	     (condition-case data
		 (progn (require 'itimer) t)
	       (error nil)))
	    (setq vm-flush-interval t
		  vm-auto-get-new-mail t)
	  (and (integerp vm-flush-interval) (not (get-itimer "vm-flush"))
	       (start-itimer "vm-flush" 'vm-flush-itimer-function
			     vm-flush-interval nil))
	  (and (integerp vm-auto-get-new-mail) (not (get-itimer "vm-get-mail"))
	       (start-itimer "vm-get-mail" 'vm-get-mail-itimer-function
			     vm-auto-get-new-mail nil))))))

;; support for numeric vm-auto-get-new-mail
(defun vm-get-mail-itimer-function ()
  (if (integerp vm-auto-get-new-mail)
      (set-itimer-restart current-itimer vm-auto-get-new-mail))
  (let ((b-list (buffer-list)))
    (while (and (not (input-pending-p)) b-list)
      (save-excursion
	(set-buffer (car b-list))
	(if (and (eq major-mode 'vm-mode)
		 (not (and (not (buffer-modified-p))
			   (file-newer-than-file-p
			    (make-auto-save-file-name)
			    buffer-file-name)))
		 (not vm-block-new-mail)
		 (vm-get-spooled-mail))
	    (progn
	      (vm-assimilate-new-messages)
	      (if (vm-thoughtfully-select-message)
		  (vm-preview-current-message)
		(vm-update-summary-and-mode-line)))))
      (setq b-list (cdr b-list)))))

;; support for numeric vm-flush-interval
(defun vm-flush-itimer-function ()
  (if (integerp vm-flush-interval)
      (set-itimer-restart current-itimer vm-flush-interval))
  ;; if no vm-mode buffers are found, we might as well shut down the
  ;; flush itimer.
  (if (not (vm-flush-cached-data))
      (set-itimer-restart current-itimer nil)))

;; flush cached data in all vm-mode buffers.
;; returns non-nil if any vm-mode buffers were found.
(defun vm-flush-cached-data ()
  (save-excursion
    (let ((buf-list (buffer-list))
	  (found-one nil))
      (while (and buf-list (not (input-pending-p)))
	(set-buffer (car buf-list))
	(cond ((eq major-mode 'vm-mode)
	       (setq found-one t)
	       (if (not (eq vm-modification-counter
			    vm-flushed-modification-counter))
		   (let ((mp vm-message-list))
		     (while (and mp (not (input-pending-p)))
		       (if (vm-modflag-of (car mp))
			   (vm-stuff-attributes (car mp)))
		       (setq mp (cdr mp)))
		     (and (null mp)
			  (setq vm-flushed-modification-counter
				vm-modification-counter))))))
	(setq buf-list (cdr buf-list)))
      ;; if we haven't checked them all return non-nil so
      ;; the flusher won't give up trying.
      (or buf-list found-one) )))

;; This allows C-x C-s to do the right thing for VM mail buffers.
;; Note that deleted messages are not expunged.
(defun vm-write-file-hook ()
  (if (and (eq major-mode 'vm-mode) (not vm-inhibit-write-file-hook))
    ;; The vm-save-restriction isn't really necessary here, since
    ;; the stuff routines clean up after themselves, but should remain
    ;; as a safeguard against the time when other stuff is added here.
    (vm-save-restriction
     (let ((mp vm-message-list)
	   (buffer-read-only))
	(while mp
	  (if (vm-modflag-of (car mp))
	      (vm-stuff-attributes (car mp)))
	  (setq mp (cdr mp)))
	(if vm-message-list
	    (progn
	      (vm-do-needed-renumbering)
	      (vm-stuff-bookmark)
	      (vm-stuff-header-variables)
	      (and vm-retain-message-order
		   vm-message-order-changed
		   (vm-stuff-message-order))))
	nil ))))

(defun vm-save-buffer (prefix)
  (interactive "P")
  (vm-select-folder-buffer)
  (vm-error-if-virtual-folder)
  (save-buffer prefix)
  (setq vm-block-new-mail nil
	vm-message-order-changed nil)
  (vm-update-summary-and-mode-line))

(defun vm-write-file ()
  (interactive)
  (vm-select-folder-buffer)
  (vm-error-if-virtual-folder)
  (call-interactively 'write-file)
  (setq vm-block-new-mail nil
	vm-message-order-changed nil)
  (vm-update-summary-and-mode-line))

(defun vm-save-folder (&optional quitting prefix)
  "Save current folder to disk.
Prefix args are handled the same for the command save-buffer."
  (interactive (list nil current-prefix-arg))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-virtual-folder)
  (if (buffer-modified-p)
      (let (mp)
	(message "Expunging...")
	;; may get error if folder is emptied by the expunge.
	(condition-case ()
	    (vm-expunge-folder quitting t)
	  (error nil))
	;; stuff the attributes of messages that need it.
	(message "Stuffing attributes...")
	(setq mp vm-message-list)
	(while mp
	  (if (vm-modflag-of (car mp))
	      (vm-stuff-attributes (car mp)))
	  (setq mp (cdr mp)))
	;; stuff bookmark and header variable values
	(if vm-message-list
	    (progn
	      (vm-do-needed-renumbering)
	      (vm-stuff-bookmark)
	      (vm-stuff-header-variables)
	      (and vm-retain-message-order
		   vm-message-order-changed
		   (vm-stuff-message-order))))
	(message "Saving...")
	(let ((vm-inhibit-write-file-hook t))
	  (save-buffer prefix))
	(vm-set-buffer-modified-p nil t)
	(setq vm-messages-not-on-disk 0)
	(setq vm-block-new-mail nil)
	(setq vm-message-order-changed nil)
	(and (zerop (buffer-size)) vm-delete-empty-folders
	     (condition-case ()
		 (progn
		   (delete-file buffer-file-name)
		   (message "%s removed" buffer-file-name))
	       ;; no can do, oh well.
	       (error nil)))
	(if (not quitting)
	    (if (null vm-message-pointer)
		(vm-update-summary-and-mode-line)
	      (vm-preview-current-message))))
    (message "No changes need to be saved")))

(defun vm-handle-file-recovery-or-reversion (recovery)
  (if (and vm-summary-buffer (buffer-name vm-summary-buffer))
      (kill-buffer vm-summary-buffer))
  (setq vm-message-list nil
	vm-message-pointer nil
	vm-summary-buffer nil)
  ;; If this is a recovery, we can't allow the user to get new
  ;; mail until a real save is performed.  Until then the buffer
  ;; and the disk don't match.
  (if recovery
      (setq vm-block-new-mail t))
  (vm buffer-file-name))

;; detect if a recover-file is being performed
;; and handle things properly.
(defun vm-handle-file-recovery ()
  (if (and (buffer-modified-p)
	   (eq major-mode 'vm-mode)
	   vm-message-list
	   (= (vm-end-of (car vm-message-list)) 1))
      (vm-handle-file-recovery-or-reversion t)))

;; detect if a revert-buffer is being performed
;; and handle things properly.
(defun vm-handle-file-reversion ()
  (if (and (not (buffer-modified-p))
	   (eq major-mode 'vm-mode)
	   vm-message-list
	   (= (vm-end-of (car vm-message-list)) 1))
      (vm-handle-file-recovery-or-reversion nil)))

(defun vm-visit-folder (folder &optional read-only)
  "Visit a mail file.
VM will parse and present its messages to you in the usual way.

First arg FOLDER specifies the mail file to visit.  When this
command is called interactively the file name is read from the
minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-select-folder-buffer)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder)))
       (list (read-file-name
	      (format "Visit%s folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil) current-prefix-arg))))
  (vm-session-initialization)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (setq vm-last-visit-folder folder)
  (vm folder read-only))

(defun vm-help ()
  "Display VM command and variable information."
  (interactive)
  (if (and vm-mail-buffer (get-buffer-window vm-mail-buffer))
      (set-buffer vm-mail-buffer))
  (let ((pop-up-windows (and pop-up-windows (eq vm-mutable-windows t))))
    (cond
     ((eq last-command 'vm-help)
      (describe-mode))
     ((eq vm-system-state 'previewing)
      (message "Type SPC to read message, n previews next message   (? gives more help)"))
     ((memq vm-system-state '(showing reading))
      (message "SPC and b scroll, (d)elete, (s)ave, (n)ext, (r)eply   (? gives more help)"))
     (t (describe-mode)))))

(defun vm-spool-move-mail (source destination)
  (let (status error-buffer)
    (setq error-buffer
	  (get-buffer-create
	   (format "*output of %s %s %s*"
		   vm-movemail-program source destination)))
    (save-excursion
      (set-buffer error-buffer)
      (erase-buffer))
    (setq status
	  (call-process vm-movemail-program nil	error-buffer t
			source destination))
    (save-excursion
      (set-buffer error-buffer)
      (if (and (numberp status) (not (= 0 status)))
	  (insert (format "\n%s exited with code %s\n"
			  vm-movemail-program status)))
      (if (> (buffer-size) 0)
	  (progn
	    (vm-display-buffer error-buffer)
	    (error "Failed getting new mail from %s" source))
	;; nag, nag, nag.
	(kill-buffer error-buffer))
      t )))

(defun vm-gobble-crash-box (crash-box)
  (save-excursion
    (vm-save-restriction
     (widen)
     (let ((opoint-max (point-max)) crash-buf
	   (buffer-read-only nil)
	   (inbox-buffer-file buffer-file-name)
	   (inbox-folder-type vm-folder-type)
	   got-mail crash-folder-type
	   (old-buffer-modified-p (buffer-modified-p))
           ;; crash box could contain a letter bomb...
	   ;; force user notification of file variables for v18 Emacses
	   ;; enable-local-variables == nil disables them for newer Emacses
	   (inhibit-local-variables t)
	   (enable-local-variables nil))
       (setq crash-buf (find-file-noselect crash-box))
       (save-excursion
	 (set-buffer crash-buf)
	 (setq crash-folder-type (vm-get-folder-type))
	 (if (and crash-folder-type vm-check-folder-types)
	     (cond ((null inbox-folder-type)
		    (if vm-default-folder-type
			(if (not (eq vm-default-folder-type crash-folder-type))
			    (if vm-convert-folder-types
				(vm-convert-folder-type crash-folder-type
							vm-default-folder-type)
			      (error "crash box %s mismatches vm-default-folder-type: %s, %s"
				     crash-box crash-folder-type
				     vm-default-folder-type)))))
		   ((not (eq inbox-folder-type crash-folder-type))
		    (if vm-convert-folder-types
			(vm-convert-folder-type crash-folder-type
						inbox-folder-type)
		      (error "crash box %s mismatches %s's folder type: %s, %s"
			     crash-box inbox-buffer-file
			     crash-folder-type inbox-folder-type))))))
       (goto-char (point-max))
       (insert-buffer-substring crash-buf
				1 (1+ (save-excursion
					(set-buffer crash-buf)
					(widen)
					(buffer-size))))
       (write-region opoint-max (point-max) buffer-file-name t t)
       (vm-increment vm-modification-counter)
       (setq got-mail (/= opoint-max (point-max)))
       ;; make sure primary inbox is private.  384 = octal 600
       (condition-case () (set-file-modes buffer-file-name 384) (error nil))
       (set-buffer-modified-p old-buffer-modified-p)
       (kill-buffer crash-buf)
       (condition-case () (delete-file crash-box)
	 (error nil))
       got-mail ))))

(defun vm-get-spooled-mail ()
  (if vm-block-new-mail
      (error "Can't get new mail until you save this folder."))
  (let ((triples nil)
	crash in maildrop popdrop
	(got-mail nil))
    (cond ((null (vm-spool-files))
	   (setq triples (list
			  (list vm-primary-inbox
				(concat vm-spool-directory (user-login-name))
				vm-crash-box))))
	  ((stringp (car (vm-spool-files)))
	   (setq triples
		 (mapcar (function
			  (lambda (s) (list vm-primary-inbox s vm-crash-box)))
			 (vm-spool-files))))
	  ((consp (car (vm-spool-files)))
	   (setq triples (vm-spool-files))))
    (while triples
      (setq in (nth 0 (car triples))
	    maildrop (nth 1 (car triples))
	    crash (nth 2 (car triples)))
      (if (eq (get-file-buffer buffer-file-name)
	      (get-file-buffer in))
	  (progn
	    (if (file-exists-p crash)
		(progn
		  (message "Recovering messages from %s..." crash)
		  (setq got-mail (or (vm-gobble-crash-box crash) got-mail))
		  (message "Recovering messages from %s... done" crash)))
	    (setq popdrop (and vm-recognize-pop-maildrops
			       (string-match vm-recognize-pop-maildrops
					     maildrop)
			       ;; maildrop with password clipped
			       (vm-safe-popdrop-string maildrop)))
	    (if (or popdrop
		    (and (not (equal 0 (nth 7 (file-attributes maildrop))))
			 (file-readable-p maildrop)))
		(progn
		  (setq crash (expand-file-name crash))
		  (if (not popdrop)
		      (setq maildrop (expand-file-name maildrop)))
		  (if (if popdrop
			  (vm-pop-move-mail maildrop crash)
			(vm-spool-move-mail maildrop crash))
		      (if (vm-gobble-crash-box crash)		      
			  (progn
			    (setq got-mail t)
			    (message "Got mail from %s."
				     (or popdrop maildrop)))))))))
      (setq triples (cdr triples)))
    (if got-mail
	(run-hooks 'vm-retrieved-spooled-mail-hook))
    got-mail ))

(defun vm-safe-popdrop-string (drop)
  (or (and (string-match "^\\([^:]+\\):[^:]+:[^:]+:\\([^:]+\\):[^:]+" drop)
	   (concat (substring drop (match-beginning 2) (match-end 2))
		   "@"
		   (substring drop (match-beginning 1) (match-end 1))))
      "???"))

(defun vm-get-new-mail (&optional arg)
  "Move any new mail that has arrived in the system mailbox into the
primary inbox.  New mail is appended to the disk and buffer copies of
the primary inbox.

Prefix arg means to gather mail from a user specified folder, instead of
the usual spool file(s).  The file name will be read from the minibuffer.
Unlike when getting mail from a spool file, the source file is left
undisturbed after its messages have been copied."
  (interactive "P")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-virtual-folder)
  (vm-error-if-folder-read-only)
  (cond ((null arg)
	 (if (not (eq major-mode 'vm-mode))
	     (vm-mode))
	 (if (consp (car (vm-spool-files)))
	     (message "Checking for new mail for %s..." buffer-file-name)
	   (message "Checking for new mail..."))
	 (let (new-messages)
	   (if (and (vm-get-spooled-mail)
		    (setq new-messages (vm-assimilate-new-messages)))
	       (progn
		 (if vm-arrived-message-hook
		     (while new-messages
		       (vm-run-message-hook (car new-messages)
					    'vm-arrived-message-hook)
		       (setq new-messages (cdr new-messages))))
		 (if (vm-thoughtfully-select-message)
		     (vm-preview-current-message)
		   (vm-update-summary-and-mode-line))
		 (vm-emit-totals-blurb)
		 (or (vm-set-window-configuration 'mail-arrived)
		     (progn
		       (if (not (or (get-buffer-window (current-buffer))
				    (and vm-summary-buffer
					 (get-buffer-window
					  vm-summary-buffer))))
			   (progn
			     (if vm-startup-with-summary
				 (switch-to-buffer vm-summary-buffer)
			       (switch-to-buffer (current-buffer)))
			     (if (eq vm-mutable-windows t)
				 (delete-other-windows)))))))
	     (if (consp (car (vm-spool-files)))
		 (message "No new mail for %s" buffer-file-name)
	       (message "No new mail."))
	     (sit-for 4)
	     (message ""))))
	(t
	 (let ((buffer-read-only nil)
	       folder mcount new-messages)
	   (setq folder (read-file-name "Gather mail from folder: "
					vm-folder-directory t))
	   (if (and vm-check-folder-types
		    (not (vm-compatible-folder-p folder)))
	       (error "Folder %s is not the same format as this folder."
		      folder))
	   (save-excursion
	     (vm-save-restriction
	      (widen)
	      (goto-char (point-max))
	      (insert-file-contents folder)))
	   (setq mcount (length vm-message-list))
	   (if (setq new-messages (vm-assimilate-new-messages))
	       (progn
		 (if vm-arrived-message-hook
		     (while new-messages
		       (vm-run-message-hook (car new-messages)
					    'vm-arrived-message-hook)
		       (setq new-messages (cdr new-messages))))
		 ;; say this NOW, before the non-previewers read
		 ;; a message, alter the new message count and
		 ;; confuse themselves.
		 (vm-emit-totals-blurb)
		 (if (vm-thoughtfully-select-message)
		     (vm-preview-current-message)
		   (vm-update-summary-and-mode-line))
		 ;; The gathered messages are actually still on disk
		 ;; unless the user deletes the folder himself.
		 ;; However, users may not understand what happened if
		 ;; the messages go away after a "quit, no save".
		 (setq vm-messages-not-on-disk
		       (+ vm-messages-not-on-disk
			  (- (length vm-message-list)
			     mcount))))
	     (message "No messages gathered."))))))

;; returns non-nil if there were any new messages
(defun vm-assimilate-new-messages ()
  (let ((tail-cons (vm-last vm-message-list))
	b-list new-messages)
    (save-excursion
      (vm-save-restriction
       (widen)
       (vm-build-message-list)
       (vm-read-attributes tail-cons))
      (setq new-messages (or (cdr tail-cons) vm-message-list))
      (vm-set-numbering-redo-start-point new-messages)
      (vm-set-summary-redo-start-point new-messages))
    (if (and tail-cons new-messages vm-virtual-buffers)
	(save-excursion
	  (setq b-list vm-virtual-buffers)
	  (while b-list
	    ;; buffer might be dead
	    (if (null (buffer-name (car b-list)))
		()
	      (set-buffer (car b-list))
	      (vm-build-virtual-message-list new-messages))
	    (setq b-list (cdr b-list)))))
    new-messages ))

;; return a list of all marked messages or the messages indicated by a
;; prefix argument.
(defun vm-select-marked-or-prefixed-messages (prefix)
  (let (mlist)
    (if (eq last-command 'vm-next-command-uses-marks)
	(setq mlist (vm-marked-messages))
      (let ((direction (if (< prefix 0) 'backward 'forward))
	     (count (vm-abs prefix))
	     (vm-message-pointer vm-message-pointer))
	(if (not (eq vm-circular-folders t))
	    (vm-check-count prefix))
	(while (not (zerop count))
	  (setq mlist (cons (car vm-message-pointer) mlist))
	  (vm-decrement count)
	  (if (not (zerop count))
	      (vm-move-message-pointer direction))))
      (nreverse mlist))))

(defun vm-display-startup-message ()
  (if (sit-for 5)
      (let ((lines vm-startup-message-lines))
	(message "VM %s, Copyright (C) 1993 Kyle E. Jones; type ? for help"
		 vm-version)
	(setq vm-startup-message-displayed t)
	(while (and (sit-for 4) lines)
	  (message (substitute-command-keys (car lines)))
	  (setq lines (cdr lines)))))
  (message ""))

(defun vm-load-init-file (&optional interactive)
  (interactive "p")
  (if (or (not vm-init-file-loaded) interactive)
      (load vm-init-file (not interactive) (not interactive) t))
  (setq vm-init-file-loaded t))

(defun vm-session-initialization ()
  ;; If this is the first time VM has been run in this Emacs session,
  ;; do some necessary preparations.
  (if (or (not (boundp 'vm-session-beginning))
	  vm-session-beginning)
      (progn
	(random t)
	(vm-load-init-file)
	(if vm-window-configuration-file
	    (if (condition-case () (progn (require 'tapestry) t))
		(vm-load-window-configurations vm-window-configuration-file)
	      (setq vm-window-configuration-file nil)))
	(require 'sets)
	(setq vm-buffers-needing-display-update (sets-make-set))
	(setq vm-messages-needing-summary-update (vm-make-message-set))
	(setq vm-session-beginning nil))))

(defun vm (&optional folder read-only)
  "Read mail under Emacs.
Optional first arg FOLDER specifies the folder to visit.  It defaults
to the value of vm-primary-inbox.  The folder buffer is put into VM
mode, a major mode for reading mail.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder.

Visiting the primary inbox causes any contents of the system mailbox to
be moved and appended to the resulting buffer.

All the messages can be read by repeatedly pressing SPC.  Use `n'ext and
`p'revious to move about in the folder.  Messages are marked for
deletion with `d', and saved to another folder with `s'.  Quitting VM
with `q' expunges deleted messages and saves the buffered folder to
disk.

See the documentation for vm-mode for more information."
  (interactive (list nil current-prefix-arg))
  (vm-session-initialization)
  ;; set inhibit-local-variables non-nil to protect
  ;; against letter bombs.
  ;; set enable-local-variables to nil for newer Emacses
  (catch 'done
    (let ((inhibit-local-variables t)
	  (enable-local-variables nil)
	  (full-startup (not (bufferp folder)))
	  folder-buffer summary-buffer already-existed first-time
	  preserve-auto-save-file)
      (setq folder-buffer
	    (if (bufferp folder)
		(setq already-existed folder)
	      (let ((file (or folder (expand-file-name vm-primary-inbox))))
		(if (file-directory-p file)
		    ;; MH code perhaps... ?
		    (error "%s is a directory" file)
		  (or (setq already-existed (get-file-buffer file))
		      (let ((default-directory
			      (or (and vm-folder-directory
				       (expand-file-name vm-folder-directory))
				  default-directory)))
			(message "Reading %s..." file)
			(prog1 (find-file-noselect file)
			  (message "Reading %s... done" file))))))))
      (set-buffer folder-buffer)
      (vm-check-for-killed-summary)
      ;; If the buffer's not modified then we know that there can be no
      ;; messages in the folder that are not on disk.
      (or (buffer-modified-p) (setq vm-messages-not-on-disk 0))
      (setq first-time (not (eq major-mode 'vm-mode))
	    preserve-auto-save-file (and buffer-file-name
					  (not (buffer-modified-p))
					  (file-newer-than-file-p
					   (make-auto-save-file-name)
					   buffer-file-name)))
      ;; Force the folder to be read only if if the auto
      ;; save file contains information the user might not
      ;; want overwritten, i.e. recover-file might be
      ;; desired.  What we want to avoid is an auto-save.
      ;; Making the folder read only will keep it
      ;; subsequent actions from modifying the buffer in a
      ;; way that triggers an auto save.
      (setq vm-folder-read-only (or preserve-auto-save-file read-only))
      ;; If this is not a VM mode buffer then some initialization
      ;; needs to be done 
      (if first-time
	  (progn
	    (buffer-disable-undo (current-buffer))
	    (abbrev-mode 0)
	    (auto-fill-mode 0)
	    (vm-mode-internal)))
      (vm-assimilate-new-messages)
      (if first-time
	  (progn
	    (vm-check-header-variables)
	    (vm-gobble-message-order)
	    (vm-gobble-bookmark)
	    (vm-start-itimers-if-needed)))

      ;; say this NOW, before the non-previewers read a message,
      ;; alter the new message count and confuse themselves.
      (if full-startup
	  (vm-emit-totals-blurb))

      (vm-thoughtfully-select-message)
      (if vm-message-list
	  (vm-preview-current-message)
	(vm-update-summary-and-mode-line))
      (if full-startup
	  (save-excursion
	    (if vm-startup-with-summary
		(progn (vm-summarize nil)
		       (setq summary-buffer vm-summary-buffer)))
	    (if (not (vm-set-window-configuration 'startup))
		(progn
		  (if (eq vm-mutable-windows t)
		      (delete-other-windows))
		  (cond ((eq vm-startup-with-summary nil)
			 (switch-to-buffer folder-buffer))
			((eq vm-startup-with-summary t)
			 (switch-to-buffer summary-buffer))
			;; vm-startup-with-summary is not nil and not t
			;; so we know we have to display folder and summary.
			;; now we need to find out how.
			((eq vm-mutable-windows t)
			 (split-window-vertically)
			 (switch-to-buffer summary-buffer)
			 (select-window (next-window))
			 (switch-to-buffer folder-buffer)
			 (vm-proportion-windows))
			((eq vm-mutable-windows nil)
			 ;; can't display both, pick one.
			 (switch-to-buffer summary-buffer))
			(t
			 (switch-to-buffer summary-buffer)
			 (select-window (next-window))
			 (switch-to-buffer folder-buffer)))))))
      ;; Warn user about auto save file, if appropriate.
      (if (and full-startup preserve-auto-save-file)
	  (message 
	   (substitute-command-keys
	    "Auto save file is newer; consider \\[recover-file].  Folder is read only.")))
      ;; if we're not doing a full startup or if doing more would
      ;; trash the auto save file that we need to preserve,
      ;; stop here.
      (if (or (not full-startup) preserve-auto-save-file)
	  (throw 'done t))
      (if (and vm-auto-get-new-mail (not vm-block-new-mail))
	  (progn
	    (message "Checking for new mail for %s..." buffer-file-name)
	    (if (vm-get-spooled-mail)
		(progn
		  (vm-assimilate-new-messages)
		  (if (vm-thoughtfully-select-message)
		      (vm-preview-current-message)
		    (vm-update-summary-and-mode-line))))
	    (message "")))

      (run-hooks 'vm-visit-folder-hook)

      ;; Display copyright and copying info unless
      ;; user says no.
      (if (not (or vm-inhibit-startup-message vm-startup-message-displayed))
	  (progn
	    (vm-display-startup-message)
	    (if (not (input-pending-p))
		(vm-emit-totals-blurb)))))))

(defun vm-toggle-read-only ()
  (interactive)
  (vm-select-folder-buffer)
  (setq vm-folder-read-only (not vm-folder-read-only))
  (message "Folder is now %s"
	   (if vm-folder-read-only "read-only" "modifiable")))

(defun vm-mode ()
  "Major mode for reading mail.

Commands:
   h - summarize folder contents
   j - discard cached information about the current message

   n - go to next message
   p - go to previous message
   N - like `n' but ignores skip-variable settings
   P - like `p' but ignores skip-variable settings
 M-n - go to next unread message
 M-p - go to previous unread message
 RET - go to numbered message (uses prefix arg or prompts in minibuffer)
 TAB - go to last message seen
 M-s - incremental search through the folder

   t - display hidden headers
 SPC - scroll forward a page (if at end of message, then display next message)
   b - scroll backward a page
   < - go to beginning of current message
   > - go to end of current message

   d - delete message, prefix arg deletes messages forward (flag as deleted)
 C-d - delete message, prefix arg deletes messages backward (flag as deleted)
   u - undelete
   k - flag for deletion all messages with same subject as the current message

   r - reply (only to the sender of the message)
   R - reply with included text for current message
 M-r - extract and resend bounced message
   f - followup (reply to all recipients of message)
   F - followup with included text from the current message
   z - forward the current message
   m - send a message
   B - resend the current message to another user.
   c - continue composing the most recent message you were composing

   @ - digestify and mail entire folder contents (the folder is not modified)
   * - burst a digest into individual messages, and append and assimilate these
       message into the current folder.

   G - sort messages by various keys

   g - get any new mail that has arrived in the system mailbox
       (new mail is appended to the disk and buffer copies of the
       primary inbox.)
   v - visit another mail folder
   V - visit a virtual folder

   e - edit the current message

   s - save current message in a folder (appends if folder already exists)
   w - write current message to a file without its headers (appends if exists)
   S - save entire folder to disk, expunging deleted messages
   A - save unfiled messages to their vm-auto-folder-alist specified folders
   # - expunge deleted messages (without saving folder)
   q - quit VM, deleted messages are expunged, folder saved to disk
   x - exit VM with no change to the folder

 M N - use marks; the next vm command will affect only marked messages
       if it makes sense for the command to do so

       M M - mark the current message
       M U - unmark the current message
       M m - mark all messages
       M u - unmark all messages
       M ? - help for the mark commands

 W S - save the current window configuration to a name
 W D - delete a window configuration
 W W - apply a configuration
 W ? - help for the window configuration commands

 C-_ - undo, special undo that retracts the most recent
             changes in message attributes.  Expunges and saves
             cannot be undone.  C-x u is also bound to this
             command.

   L - reload your VM init file, ~/.vm

   ? - help

   ! - run a shell command
   | - run a shell command with the current message as input

 M-C - view conditions under which you may redistribute VM
 M-W - view the details of VM's lack of a warranty

Variables:
   vm-auto-center-summary
   vm-auto-folder-alist
   vm-auto-folder-case-fold-search
   vm-auto-get-new-mail
   vm-auto-next-message
   vm-berkeley-mail-compatibility
   vm-check-folder-types
   vm-convert-folder-types
   vm-circular-folders
   vm-confirm-new-folders
   vm-confirm-quit
   vm-crash-box
   vm-delete-after-archiving
   vm-delete-after-bursting
   vm-delete-after-saving
   vm-delete-empty-folders
   vm-digest-burst-type
   vm-digest-center-preamble
   vm-digest-preamble-format
   vm-digest-send-type
   vm-folder-directory
   vm-folder-read-only
   vm-follow-summary-cursor
   vm-forwarded-headers
   vm-forwarding-digest-type
   vm-forwarding-subject-format
   vm-gargle-uucp
   vm-highlighted-header-regexp
   vm-honor-page-delimiters
   vm-in-reply-to-format
   vm-included-text-attribution-format
   vm-included-text-prefix
   vm-inhibit-startup-message
   vm-invisible-header-regexp
   vm-jump-to-new-messages
   vm-jump-to-unread-messages
   vm-keep-sent-messages
   vm-mail-header-from
   vm-mail-mode-hook
   vm-mail-window-percentage
   vm-mode-hook
   vm-move-after-deleting
   vm-move-after-undeleting
   vm-mutable-windows
   vm-preview-lines
   vm-preview-read-messages
   vm-primary-inbox
   vm-recognize-pop-maildrops
   vm-reply-ignored-addresses
   vm-reply-subject-prefix
   vm-resend-bounced-headers
   vm-resend-bounced-discard-header-regexp
   vm-resend-headers
   vm-resend-discard-header-regexp
   vm-retain-message-order
   vm-rfc1153-digest-discard-header-regexp
   vm-rfc1153-digest-headers
   vm-rfc934-digest-discard-header-regexp
   vm-rfc934-digest-headers
   vm-search-using-regexps
   vm-skip-deleted-messages
   vm-skip-read-messages
   vm-spool-files
   vm-startup-with-summary
   vm-strip-reply-headers
   vm-summary-format
   vm-unforwarded-header-regexp
   vm-virtual-folder-alist
   vm-virtual-mirror
   vm-visible-headers
   vm-visit-when-saving
   vm-window-configuration-file
"
  (interactive)
  (vm (current-buffer)))

;; this does the real major mode scutwork.
(defun vm-mode-internal ()
  (widen)
  (setq
;   case-fold-search t
   major-mode 'vm-mode
   mode-line-format vm-mode-line-format
   mode-name "VM"
   buffer-read-only t
   vm-message-list nil
   vm-message-pointer nil
   vm-folder-type (vm-get-folder-type))
  (use-local-map vm-mode-map)
  (run-hooks 'vm-mode-hook)
  ;; compatibility
  (run-hooks 'vm-mode-hooks))

(put 'vm-mode 'mode-class 'special)

(if (not (memq 'vm-write-file-hook write-file-hooks))
    (setq write-file-hooks
	  (cons 'vm-write-file-hook write-file-hooks)))

(if (not (memq 'vm-handle-file-recovery find-file-hooks))
    (setq find-file-hooks
	  (nconc find-file-hooks
		 '(vm-handle-file-recovery
		   vm-handle-file-reversion))))
