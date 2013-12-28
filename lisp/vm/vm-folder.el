;;; VM folder related functions
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

(defun vm-do-needed-renumbering ()
  (if vm-numbering-redo-start-point
      (progn
	(vm-number-messages (and (consp vm-numbering-redo-start-point)
				 vm-numbering-redo-start-point)
			    vm-numbering-redo-end-point)
	(setq vm-numbering-redo-start-point nil
	      vm-numbering-redo-end-point nil))))

(defun vm-reverse-link-messages ()
  (let ((mp vm-message-list) prev)
    (while mp
      (vm-set-reverse-link-of (car mp) prev)
      (setq prev mp mp (cdr mp)))))

(defun vm-match-visible-header (alist)
  (catch 'match
    (while alist
      (if (looking-at (car (car alist)))
	  (throw 'match (car alist)))
      (setq alist (cdr alist)))
    nil))

(defun vm-get-folder-type ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (skip-chars-forward "\n")
      (cond ((looking-at "From ") 'From_)
	    ((looking-at "\001\001\001\001\n") 'mmdf)
	    ((looking-at "^$") 'From_)))))

;; Build a chain of message structures.
;; Find the start and end of each message and fill in the relevant
;; fields in the message structures.

(defun vm-build-message-list (&optional rebuild-markers)
  ;; sanity check
  (if rebuild-markers
      (setq rebuild-markers vm-message-list))
  (save-excursion
    (vm-build-visible-header-alist)
    (let (tail-cons message prev-message case-fold-search list marker
	  start-regexp sep-pattern trailer-length)
      (if (eq vm-folder-type 'mmdf)
	  (setq start-regexp "^\001\001\001\001\n"
		separator-string "\n\001\001\001\001\n\001\001\001\001"
		trailer-length 6)
	(setq start-regexp "^From "
	      separator-string "\n\nFrom "
	      trailer-length 2))
      (if (and vm-message-list (not rebuild-markers))
	  (let ((mp vm-message-list)
		(end (point-min)))
	    (while mp
	      (if (< end (vm-end-of (car mp)))
		  (setq end (vm-end-of (car mp))))
	      (setq mp (cdr mp)))
	    ;; move back past trailer so separator-string will match below
	    (goto-char (- end trailer-length))
	    (setq tail-cons (vm-last vm-message-list)))
	(if rebuild-markers
	    (setq list vm-message-list))
	(goto-char (point-min))
	(re-search-forward start-regexp nil 0)
	(goto-char (or (match-beginning 0) (point)))
	(if (looking-at start-regexp)
	    (if rebuild-markers
		(progn
		  (setq message (car list)
			prev-message message
			tail-cons list
			list (cdr list))
		  (vm-set-start-of message (vm-marker (match-beginning 0)))
		  (vm-set-vheaders-of message nil)
		  (vm-set-text-of message nil))
	      (setq message (vm-make-message) prev-message message)
	      (vm-set-reverse-link-of message nil)
	      (vm-set-start-of message (vm-marker (match-beginning 0)))
	      (setq vm-message-list (list message)
		    tail-cons vm-message-list))))
      (while (and (or (null rebuild-markers) list)
		  (search-forward separator-string nil t))
	(setq marker (vm-marker (+ trailer-length (match-beginning 0)))
	      message (or (car list) (vm-make-message)))
	(vm-set-start-of message marker)
	(if prev-message
	    (vm-set-end-of prev-message marker))
	(cond (list
	       (vm-set-vheaders-of message nil)
	       (vm-set-text-of message nil)
	       (setq tail-cons list
		     list (cdr list)
		     prev-message message))
	      (tail-cons
	       (setcdr tail-cons (list message))
	       (vm-set-reverse-link-of message tail-cons)
	       (setq tail-cons (cdr tail-cons)
		     prev-message message))
	      (t
	       (vm-set-reverse-link-of message nil)
	       (setq vm-message-list (list message)
		     tail-cons vm-message-list
		     prev-message message))))
      (if prev-message
	  (vm-set-end-of prev-message (vm-marker (point-max))))
      ;; If there are still some messages whose markers have not been
      ;; fixed up there is a serious problem.
      ;; The message list will need to be rebuilt from scratch.
      ;; Force this to happen.
      (and rebuild-markers list (setq vm-message-list nil)))))

(defun vm-build-visible-header-alist ()
  (let ((header-alist (cons nil nil))
	(vheaders vm-visible-headers)
	list)
    (setq list header-alist)
    (while vheaders
      (setcdr list (cons (cons (car vheaders) nil) nil))
      (setq list (cdr list) vheaders (cdr vheaders)))
    (setq vm-visible-header-alist (cdr header-alist))))

;; Group the headers that the user wants to see at the end of the headers
;; section so we can narrow to them.  The vheaders field of the
;; message struct is set.  This function is called on demand whenever
;; a vheaders field is discovered to be nil for a particular message.

(defun vm-reorder-message-headers (message)
  (save-excursion
    ;; if there is a cached regexp that points to the ordered headers
    ;; then use it and avoid a lot of work.
    (if (and (vm-vheaders-regexp-of message)
	     (progn (goto-char (vm-start-of message))
		    (re-search-forward (vm-vheaders-regexp-of message)
				       (vm-text-of message) t)))
	(vm-set-vheaders-of message (vm-marker (match-beginning 0)))
      ;; oh well, we gotta do it the hard way.
      ;;
      ;; vm-visible-header-alist is an assoc list version of
      ;; vm-visible-headers.  When a matching header is found,
      ;; the header is stuffed into its corresponding assoc cell
      ;; and the header text is deleted from the buffer.  After all
      ;; the visible headers have been collected, they are inserted
      ;; into the buffer in a clump at the end of the header section.
      (vm-save-restriction
       (let ((header-alist vm-visible-header-alist)
	     list buffer-read-only match-end-0 extras
	     (inhibit-quit t)
	     ;; This prevents file locking from occuring.  Disabling
	     ;; locking can speed things noticably if the lock directory
	     ;; is on a slow device.  We don't need locking here because
	     ;; in a mail context reordering headers is harmless.
	     (buffer-file-name nil)
	     (old-buffer-modified-p (buffer-modified-p)))
	 (goto-char (vm-start-of message))
	 (forward-line)
	 (while (and (not (= (following-char) ?\n))
		     (looking-at vm-generic-header-regexp))
	   (setq match-end-0 (match-end 0)
		 list (vm-match-visible-header header-alist))
	   (if (and (null list)
		    (or (null vm-invisible-header-regexp)
			(looking-at vm-invisible-header-regexp)))
	       (goto-char match-end-0)
	     (if list
		 (if (cdr list)
		     (setcdr list 
			     (concat
			      (cdr list)
			      (buffer-substring (point) match-end-0)))
		   (setcdr list (buffer-substring (point) match-end-0)))
	       (setq extras
		     (cons (buffer-substring (point) match-end-0) extras)))
	     (delete-region (point) match-end-0)))
	 (vm-set-vheaders-of message (point-marker))
	 (save-excursion
	   ;; now dump out the visible headers
	   ;; the vm-visible-headers go first
	   (setq list header-alist)
	   (while list
	     (if (cdr (car list))
		 (progn
		   (insert (cdr (car list)))
		   (setcdr (car list) nil)))
	     (setq list (cdr list)))
	   ;; now the headers that were not explicitly ignored, if any.
	   (if extras
	       (progn
		 (setq extras (nreverse extras))
		 (while extras
		   (insert (car extras))
		   (setq extras (cdr extras)))))
	   (set-buffer-modified-p old-buffer-modified-p))
	 ;; cache a regular expression that can be used to find the start of
	 ;; the reordered header the next time this folder is visited.
	 (if (looking-at vm-generic-header-regexp)
	     (vm-set-vheaders-regexp-of
	      message
	      (concat "^" (buffer-substring (match-beginning 1) (match-end 1))
		      ":"))))))))

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

(defun vm-read-attributes ()
  (save-excursion
    (let ((mp vm-message-list)
	  (vm-new-count 0)
	  (vm-unread-count 0)
	  (vm-total-count 0)
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
	    (vm-set-attributes-of (car mp) (car data))
	    (vm-set-cache-of (car mp) (car (cdr data))))
	   ((and vm-berkeley-mail-compatibility
		 (re-search-forward vm-berkeley-mail-status-header-regexp
				    (vm-text-of (car mp)) t))
	    (goto-char (match-beginning 1))
	    (vm-set-attributes-of
	     (car mp)
	     (make-vector vm-attributes-vector-length nil))
	    (vm-set-unread-flag (car mp) (not (looking-at ".*R.*")) t)
	    (vm-set-cache-of (car mp) (make-vector vm-cache-vector-length
						   nil)))
	   (t
	    (vm-set-attributes-of
	     (car mp)
	     (make-vector vm-attributes-vector-length nil))
	    (vm-set-new-flag (car mp) t t)
	    (vm-set-cache-of (car mp) (make-vector vm-cache-vector-length
						   nil)))))
	(cond ((vm-deleted-flag (car mp))) ; don't count deleted messages
	      ((vm-new-flag (car mp))
	       (vm-increment vm-new-count))
	      ((vm-unread-flag (car mp))
	       (vm-increment vm-unread-count)))
	(setq mp (cdr mp)))
      (setq vm-totals (list vm-total-count vm-new-count vm-unread-count)))))

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
	buffer-read-only n
	;; This prevents file locking from occuring.  Disabling
	;; locking can speed things noticably if the lock
	;; directory is on a slow device.  We don't need locking
	;; here because the user shouldn't care about VM removing
	;; its own status headers.
	(buffer-file-name nil)
	(inhibit-quit t))
    (save-excursion
      (vm-save-restriction
       (let (lim)
	 (widen)
	 (goto-char (point-min))
	 (forward-line)
	 (search-forward "\n\n" nil t)
	 (setq lim (point))
	 (goto-char (point-min))
	 (forward-line)
	 (if (re-search-forward vm-bookmark-header-regexp lim t)
	     (progn
	       (setq n (string-to-int
			(buffer-substring (match-beginning 1) (match-end 1))))
	       (delete-region (match-beginning 0) (match-end 0))))))
    (set-buffer-modified-p old-buffer-modified-p)
    (if (null n)
	nil
      (condition-case ()
	  (vm-goto-message n)
	(error nil))
      t ))))

(defun vm-check-header-variables ()
  (save-excursion
    (vm-save-restriction
     (let (lim)
       (widen)
       (goto-char (point-min))
       (forward-line)
       (search-forward "\n\n" nil t)
       (setq lim (point))
       (goto-char (point-min))
       (forward-line)
       (if (re-search-forward vm-vheader-header-regexp lim t)
	   (let ((old-buffer-modified-p (buffer-modified-p))
		 ;; This prevents file locking from occuring.  Disabling
		 ;; locking can speed things noticably if the lock
		 ;; directory is on a slow device.  We don't need locking
		 ;; here because the user shouldn't care about VM removing
		 ;; its own status headers.
		 (buffer-file-name nil)
		 buffer-read-only vis invis got
		 (inhibit-quit t))
	     (goto-char (match-beginning 1))
	     (condition-case ()
		 (setq vis (read (current-buffer))
		       invis (read (current-buffer))
		       got t)
	       (error nil))
	     (delete-region (match-beginning 0) (match-end 0))
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
	buffer-read-only lim v order
	;; This prevents file locking from occuring.  Disabling
	;; locking can speed things noticably if the lock
	;; directory is on a slow device.  We don't need locking
	;; here because the user shouldn't care about VM removing
	;; its own status headers.
	(buffer-file-name nil)
	(mp vm-message-list)
	(list-length (length vm-message-list)) 
	(inhibit-quit t))
    (save-excursion
      (vm-save-restriction
       (widen)
       (goto-char (point-min))
       (forward-line)
       (search-forward "\n\n" nil t)
       (setq lim (point))
       (goto-char (point-min))
       (forward-line)
       (if (re-search-forward vm-message-order-header-regexp lim t)
	   (progn
	     (message "Reordering messages...")
	     (goto-char (match-beginning 1))
	     (setq order (read (current-buffer))
		   v (make-vector (max list-length (length order)) nil))
	     (delete-region (match-beginning 0) (match-end 0))
	     (while (and order mp)
	       (aset v (1- (car order)) (car mp))
	       (setq order (cdr order) mp (cdr mp)))
	     (setq vm-message-list (delq nil (append v mp))
		   vm-message-order-changed t
		   vm-message-order-stuffed nil
		   vm-numbering-redo-start-point t
		   vm-message-pointer (memq (car vm-message-pointer)
					    vm-message-list))
	     (vm-reverse-link-messages)))))
    (set-buffer-modified-p old-buffer-modified-p)))

;; Stuff the message attributes back into the message as headers.
(defun vm-stuff-attributes (m &optional suppress-delete)
  (save-excursion
    (vm-save-restriction
     (widen)
     (let ((old-buffer-modified-p (buffer-modified-p))
	   attributes cache buffer-read-only
	   ;; This prevents file locking from occuring.  Disabling
	   ;; locking can speed things noticably if the lock
	   ;; directory is on a slow device.  We don't need locking
	   ;; here because the user shouldn't care about VM stuffing
	   ;; its own status headers.
	   (buffer-file-name nil)
	   (delflag (vm-deleted-flag m))
	   (inhibit-quit t))
       (setq attributes (vm-attributes-of m)
	     cache (vm-cache-of m))
       (and delflag suppress-delete
	    (vm-set-deleted-flag-in-vector attributes nil))
       (goto-char (vm-start-of m))
       (forward-line)
       (if (re-search-forward vm-attributes-header-regexp
			      (vm-text-of m) t)
	   (delete-region (match-beginning 0) (match-end 0)))
       (insert-before-markers vm-attributes-header " ("
			      (let ((print-escape-newlines t))
				(prin1-to-string attributes))
			      "\n\t"
			      (let ((print-escape-newlines t))
				(prin1-to-string cache))
			      ")\n")
       (vm-set-modflag-of m nil)
       (cond (vm-berkeley-mail-compatibility
	      (goto-char (vm-start-of m))
	      (forward-line)
	      (if (re-search-forward vm-berkeley-mail-status-header-regexp
				     (vm-text-of m) t)
		  (delete-region (match-beginning 0) (match-end 0)))
	      (cond ((not (vm-new-flag m))
		     (insert-before-markers
		      vm-berkeley-mail-status-header
		      (if (vm-unread-flag m) "" "R")
		      "O\n")))))
       (and delflag suppress-delete
	    (vm-set-deleted-flag-in-vector attributes t))
       (set-buffer-modified-p old-buffer-modified-p)))))

(defun vm-stuff-virtual-attributes (message)
  (let ((virtual (eq message (vm-real-message-of message)))
	(mirror (eq (vm-attributes-of message)
		    (vm-attributes-of (vm-real-message-of message)))))
    (if (or (not virtual) (and virtual mirror))
	(save-excursion
	  (set-buffer (marker-buffer (vm-start-of message)))
	  (vm-stuff-attributes message)))))

;; Insert a bookmark into the first message in the folder.
(defun vm-stuff-bookmark ()
  (if vm-message-pointer
      (save-excursion
	(vm-save-restriction
	 (widen)
	 (let ((old-buffer-modified-p (buffer-modified-p))
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       buffer-read-only lim
	       (inhibit-quit t))
	   (goto-char (point-min))
	   (forward-line)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (forward-line)
	   (if (re-search-forward vm-bookmark-header-regexp lim t)
	       (delete-region (match-beginning 0) (match-end 0)))
	   (insert-before-markers vm-bookmark-header " "
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
	       (print-escape-newlines t)
	       buffer-read-only lim
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       (inhibit-quit t))
	   (goto-char (point-min))
	   (forward-line)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (forward-line)
	   (if (re-search-forward vm-vheader-header-regexp lim t)
	       (delete-region (match-beginning 0) (match-end 0)))
	   (insert-before-markers vm-vheader-header " "
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
	       ;; This prevents file locking from occuring.  Disabling
	       ;; locking can speed things noticably if the lock
	       ;; directory is on a slow device.  We don't need locking
	       ;; here because the user shouldn't care about VM stuffing
	       ;; its own status headers.
	       (buffer-file-name nil)
	       buffer-read-only lim n
	       (mp (copy-sequence vm-message-list))
	       (inhibit-quit t))
	   (setq mp
		 (sort mp
		       (function
			(lambda (p q)
			  (< (vm-start-of p) (vm-start-of q))))))
	   (goto-char (point-min))
	   (forward-line)
	   (search-forward "\n\n" nil t)
	   (setq lim (point))
	   (goto-char (point-min))
	   (forward-line)
	   (if (re-search-forward vm-message-order-header-regexp lim t)
	       (delete-region (match-beginning 0) (match-end 0)))
	   (insert-before-markers vm-message-order-header "\n\t(")
	   (setq n 0)
	   (while mp
	     (insert-before-markers (vm-number-of (car mp)))
	     (setq n (1+ n) mp (cdr mp))
	     (and mp (insert-before-markers
		      (if (zerop (% n 15))
			  "\n\t "
			" "))))
	   (insert-before-markers ")\n")
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

(defun vm-highlight-headers (message window)
  (and vm-highlighted-header-regexp window
       (<= (window-start window) (vm-text-of message))
       (save-excursion
	 ;; As of v18.52, this call to save-window-excursion is needed!
	 ;; Somehow window point can get fouled in here, and drag the
	 ;; buffer point along with it.  This problem only manifests
	 ;; itself when operating VM from the summary buffer, subsequent
	 ;; to using vm-beginning-of-message or vm-end-of-message.
	 ;; After running a next or previous message command, point
	 ;; somehow ends up at the end of the message.
	 (save-window-excursion
	   (goto-char (window-start window))
	   (while (re-search-forward vm-highlighted-header-regexp
				     (vm-text-of message) t)
	     (save-restriction
	       (goto-char (match-beginning 0))
	       (if (looking-at vm-generic-header-regexp)
		   (progn
		     (goto-char (match-beginning 2))
		     (narrow-to-region (point-min) (point))
		     (sit-for 0)
		     (setq inverse-video t)
		     (goto-char (point-min))
		     (widen)
		     (narrow-to-region (point) (match-end 2))
		     (sit-for 0)
		     (setq inverse-video nil)
		     (goto-char (match-end 0)))
		 (goto-char (match-end 0)))))))))

(defun vm-unread-message (&optional count)
  "Set the `unread' attribute for the current message.  If the message is
already new or unread, then it left unchanged.

Numeric prefix argument N mans to unread the current message plus the
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
  (vm-error-if-referenced-virtually)
  (if (eq major-mode 'vm-virtual-mode)
      (vm-virtual-quit)
    (cond
     ((and no-change (buffer-modified-p)
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
     ((and no-change (buffer-modified-p) vm-confirm-quit
	   (not (y-or-n-p "There are unsaved changes, quit anyway? ")))
      (error "Aborted"))
     ((and (eq vm-confirm-quit t)
	   (not (y-or-n-p "Do you really want to quit? ")))
      (error "Aborted")))
    (message "")
    (let ((inhibit-quit t))
      (if (not no-change)
	  (progn
	    ;; this could take a while, so give the user some feedback
	    (message "Quitting...")
	    (or vm-folder-read-only (vm-change-all-new-to-unread))
	    (if (not (buffer-modified-p))
		(message ""))))
      (if (and (buffer-modified-p) (not no-change))
	  (vm-save-folder t))
      (let ((summary-buffer vm-summary-buffer)
	    (mail-buffer (current-buffer)))
	(if summary-buffer
	    (progn
	      (if (eq vm-mutable-windows t)
		  (delete-windows-on vm-summary-buffer))
	      (kill-buffer summary-buffer)))
	(set-buffer mail-buffer)
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer))))))

;; in support of timer based checkpointing.
(defun vm-checkpoint-prologue-hook ()
  (if (eq vm-checkpoint-modification-counter vm-modification-counter)
      nil
    (let ((mp vm-message-list))
      (while mp
	(if (vm-modflag-of (car mp))
	    (vm-stuff-attributes (car mp)))
	(setq mp (cdr mp))))
    (setq vm-checkpoint-modification-counter
	  vm-modification-counter)
    t ))

(defun vm-checkpoint-epilogue-hook ()
  (set-file-modes buffer-auto-save-file-name 384))

;; support for numeric vm-flush-interval
(defun vm-flush-timer-function ()
  (set-timer-restart current-timer vm-flush-interval)
  ;; if no vm-mode buffers are found, we might as well shut down the
  ;; flush timer.
  (if (not (vm-flush-cached-data))
      (set-timer-restart current-timer nil)))

;; flush cached data in all vm-mode buffers.
;; returns non-nil if any vm-mode buffers were found.
(defun vm-flush-cached-data ()
  (save-excursion
    (let ((buf-list (buffer-list)) found-one)
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
      ;; we may not have checked them all so return non-nil so
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
     (let ((inhibit-quit t)
	   (mp vm-message-list)
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
	;; We can't be sure the write is going to succeed, so we can't set
	;; this variable to nil.  We can't leave it set to t either since
	;; the user will be confused, since she thought the folder was saved.
	;; The solution, set vm-buffer-modified-p to a value that indicates
	;; uncertainty.
	(setq vm-buffer-modified-p "--??-")
	(if vm-summary-buffer
	    (save-excursion
	      (set-buffer vm-summary-buffer)
	      (setq vm-buffer-modified-p "--??-")))
	nil ))))

(defun vm-save-buffer (prefix)
  (interactive "P")
  (vm-select-folder-buffer)
  (vm-error-if-virtual-folder)
  (save-buffer prefix)
  (setq vm-block-new-mail nil
	vm-buffer-modified-p nil
	vm-message-order-changed nil)
  (vm-update-summary-and-mode-line))

(defun vm-write-file ()
  (interactive)
  (vm-select-folder-buffer)
  (vm-error-if-virtual-folder)
  (call-interactively 'write-file)
  (setq vm-block-new-mail nil
	vm-buffer-modified-p nil
	vm-message-order-changed nil)
  (vm-update-summary-and-mode-line))

(defun vm-save-folder (&optional quitting prefix)
  "Save current folder to disk.
Prefix args are handled the same for the command save-buffer."
  (interactive (list nil current-prefix-arg))
  (vm-select-folder-buffer)
  (vm-sanity-check-modification-flag)
  (vm-check-for-killed-summary)
  (vm-error-if-virtual-folder)
  (if (buffer-modified-p)
      (let ((inhibit-quit t) mp)
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
	       (error nil)))
	(if (not quitting)
	    (if vm-message-pointer
		(vm-update-summary-and-mode-line)
	      (vm-next-message))))
    (message "No changes need to be saved")))

;; detect if a recover-file is being performed
;; and handle things properly.
(defun vm-handle-file-recovery ()
  (if (and (buffer-modified-p)
	   (eq major-mode 'vm-mode)
	   vm-message-list
	   (= (vm-end-of (car vm-message-list)) 1))
      (progn
	(if (and vm-summary-buffer (bufferp vm-summary-buffer))
	    (kill-buffer vm-summary-buffer))
	(setq vm-message-list nil
	      vm-message-pointer nil
	      vm-summary-buffer nil)
	;; We can't allow the user to get new mail until a real
	;; save is performed.  Until then the buffer and the disk
	;; don't match.
	(setq vm-block-new-mail t)
	(vm buffer-file-name))))

;; detect if a revert-buffer is being performed
;; and handle things properly.
(defun vm-handle-file-reversion ()
  (if (and (not (buffer-modified-p))
	   (eq major-mode 'vm-mode)
	   vm-message-list
	   (= (vm-end-of (car vm-message-list)) 1))
      (progn
	(save-excursion
	  ;; save-excursion required to restore current buffer
	  ;; due to a bug in kill-buffer (really replace-buffer-in-windows)
	  (if (and vm-summary-buffer (bufferp vm-summary-buffer))
	      (kill-buffer vm-summary-buffer)))
	(setq vm-summary-buffer nil)
	(vm-build-message-list t)
	(if (null vm-message-list)
	    (progn
	      (message "Partial revert failed; disk version of folder has too few messages...")
	      (sleep-for 3)
	      (message "Assuming file changed completely; rebuilding message list...")
	      (sleep-for 1)
	      (setq vm-message-pointer nil)
	      (vm buffer-file-name))
	  (vm-assimilate-new-messages)
	  (vm-preview-current-message)))))

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
     (let ((dir (if vm-folder-directory
		    (expand-file-name vm-folder-directory)
		  default-directory)))
       (list (read-file-name
	      (format "Visit%s folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if vm-last-save-folder
			  (format " (default %s)" vm-last-save-folder)
			""))
	      dir vm-last-save-folder t) current-prefix-arg))))
  (vm-session-initialization)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
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

(defun vm-move-mail (source destination)
  (let ((status
	 (call-process vm-movemail-program nil nil nil
		       (expand-file-name source)
		       (expand-file-name destination))))
    (if (not (= 0 status))
	(error "vm: %s exited with code %s" vm-movemail-program status))
    status))

(defun vm-gobble-crash-box ()
  (save-excursion
    (vm-save-restriction
     (widen)
     (let ((opoint-max (point-max)) crash-buf buffer-read-only
	   (old-buffer-modified-p (buffer-modified-p))
           ;; crash box could contain a letter bomb...
	   ;; force user notification of file variables.
	   (inhibit-local-variables t)
	   (enable-local-variables nil))
       (setq crash-buf (find-file-noselect vm-crash-box))
       (goto-char (point-max))
       (insert-buffer-substring crash-buf
				1 (1+ (save-excursion
					(set-buffer crash-buf)
					(widen)
					(buffer-size))))
       (write-region opoint-max (point-max) buffer-file-name t t)
       ;; make sure primary inbox is private.  384 = octal 600
       (condition-case () (set-file-modes buffer-file-name 384) (error nil))
       (set-buffer-modified-p old-buffer-modified-p)
       (kill-buffer crash-buf)
       (condition-case () (delete-file vm-crash-box)
	 (error nil))))))

(defun vm-compatible-folder-p (file)
  (while (not (string= file (setq file (expand-file-name file)))))
  (let (buffer (type vm-folder-type))
    (if (zerop (buffer-size))
	t
      (if (null (setq buffer (get-file-buffer file)))
	  (if (not (file-exists-p file))
	      t
	    (save-excursion
	      (unwind-protect
		  (progn
		    (setq buffer (generate-new-buffer " *vm work*"))
		    (call-process "sed" file buffer nil "-n" "1p")
		    (save-excursion
		      (set-buffer buffer)
		      (or (zerop (buffer-size))
			  (eq type (vm-get-folder-type)))))
		(and buffer (kill-buffer buffer)))))
	(save-excursion
	  (set-buffer buffer)
	  (or (zerop (buffer-size))
	      (eq type (vm-get-folder-type))))))))

(defun vm-check-for-spooled-mail ()
  (let ((spool-files
	 (append (or vm-spool-files
		     (list (concat vm-spool-directory (user-login-name))))
		 (list vm-crash-box)))
	(new-mail nil))
    (while spool-files
      (if (and (not (equal 0 (nth 7 (file-attributes (car spool-files)))))
	       (file-readable-p (car spool-files))
	       (vm-compatible-folder-p (car spool-files)))
	  (setq spool-files nil
		new-mail t)
	(setq spool-files (cdr spool-files))))
    new-mail ))

(defun vm-get-spooled-mail ()
  (if vm-block-new-mail
      (error "Can't get new mail until you save this folder."))
  (let ((spool-files (or vm-spool-files
			 (list (concat vm-spool-directory (user-login-name)))))
	(inhibit-quit t)
	(got-mail))
    (if (file-exists-p vm-crash-box)
	(progn
	  (message "Recovering messages from crash box...")
	  (vm-gobble-crash-box)
	  (message "Recovering messages from crash box... done")
	  (setq got-mail t)))
    (while spool-files
      (if (and (not (equal 0 (nth 7 (file-attributes (car spool-files)))))
	       (file-readable-p (car spool-files))
	       (vm-compatible-folder-p (car spool-files)))
	  (progn
	    (message "Getting new mail from %s..." (car spool-files))
	    (vm-move-mail (car spool-files) vm-crash-box)
	    (vm-gobble-crash-box)
	    (message "Getting new mail from %s... done" (car spool-files))
	    (setq got-mail t)))
      (setq spool-files (cdr spool-files)))
    got-mail ))

(defun vm-get-new-mail (&optional arg)
  "Move any new mail that has arrived in the system mailbox into the
primary inbox.  New mail is appended to the disk and buffer copies of
the primary inbox.

Prefix arg means to gather mail from a user specified folder, instead of
the usual spool file(s).  The file name will be read from the minibuffer.
Unlike when getting mail from a spool file, in this case the folder is left
undisturbed after its messages have been copied."
  (interactive "P")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-virtual-folder)
  (vm-error-if-folder-read-only)
  (if (and (null arg) (not vm-primary-inbox-p)
	   (vm-check-for-spooled-mail))
      (progn
	(switch-to-buffer (or (get-file-buffer vm-primary-inbox)
			      (find-file-noselect vm-primary-inbox)))
	(if (not (eq major-mode 'vm-mode))
	    (vm-mode))))
  (if (null arg)
      (if (not (and (vm-get-spooled-mail) (vm-assimilate-new-messages)))
 	  (progn
 	    (message "No new mail.")
	    ;; don't let this message stay up forever...
 	    (sit-for 4)
 	    (message ""))
	(vm-deferred-message (vm-emit-totals-blurb))
	(or (vm-thoughtfully-select-message)
	    (vm-update-summary-and-mode-line)))
    (let (folder mcount buffer-read-only)
      (setq folder (read-file-name "Gather mail from folder: "
				   vm-folder-directory t))
      (if (not (vm-compatible-folder-p folder))
	  (error "Folder %s is not the same format as this folder." folder))
      (save-excursion
	(vm-save-restriction
	 (widen)
	 (goto-char (point-max))
	 (insert-file-contents folder)))
      (if (null vm-totals)
	  (vm-read-attributes))
      (setq mcount (car vm-totals))
      (if (vm-assimilate-new-messages)
	  (progn
	    (vm-deferred-message (vm-emit-totals-blurb))
	    (vm-update-summary-and-mode-line)
	    ;; The gathered messages are actually still on disk
	    ;; unless the user deletes the folder himself.
	    ;; However, users may not understand what happened if
	    ;; the messages go away after a "quit, no save".
	    (setq vm-messages-not-on-disk
		  (+ vm-messages-not-on-disk (- (car vm-totals) mcount))))
	(message "No messages gathered.")))))

(defun vm-emit-totals-blurb ()
  (save-excursion
    (vm-select-folder-buffer)
    (if (null vm-totals)
	(vm-read-attributes))
    (message "%d message%s, %d new, %d unread."
	     (car vm-totals) (if (= (car vm-totals) 1) "" "s") 
	     (car (cdr vm-totals))
	     (car (cdr (cdr vm-totals))))))

;; returns non-nil if there were any new messages
(defun vm-assimilate-new-messages ()
  (let ((tail-cons (vm-last vm-message-list))
	(new-messages-p (null vm-message-list)))
    (save-excursion
      (vm-save-restriction
       (widen)
       (vm-build-message-list)
       (vm-read-attributes)
       (setq new-messages-p (or new-messages-p (cdr tail-cons))
	     vm-numbering-redo-start-point new-messages-p
	     vm-summary-redo-start-point new-messages-p)
       (cond ((and vm-current-grouping new-messages-p)
	      (condition-case data
		  (vm-group-messages vm-current-grouping)
		;; presumably an unsupported grouping
		(error (message (car (cdr data)))
		       (sleep-for 2))))))
      (setq vm-need-summary-pointer-update t)
      new-messages-p )))

;; return a list of all marked messages or the messages indicated by a
;; prefix argument.
(defun vm-select-marked-or-prefixed-messages (prefix)
  (let (mlist)
    (if (eq last-command 'vm-next-command-uses-marks)
	(setq mlist (vm-marked-messages))
      (let* ((direction (if (< prefix 0) 'backward 'forward))
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
	(message "VM %s, Copyright (C) 1991 Kyle E. Jones; type ? for help"
		 vm-version)
	(setq vm-startup-message-displayed t)
	(while (and (sit-for 4) lines)
	  (message (substitute-command-keys (car lines)))
	  (setq lines (cdr lines)))))
  (message ""))

(defun vm-load-rc (&optional interactive)
  (interactive "p")
  (if (or (not vm-rc-loaded) interactive)
      (load "~/.vm" (not interactive) (not interactive) t))
  (setq vm-rc-loaded t))

(defun vm-session-initialization ()
  ;; If this is the first time VM has been run in this Emacs session,
  ;; do some necessary preparations.
  (if (or (not (boundp 'vm-session-beginning)) vm-session-beginning)
      (progn
	(random t)
	(vm-load-rc)
	(if vm-window-configuration-file
	    (if (condition-case () (progn (require 'screen) t))
		(vm-load-window-configurations vm-window-configuration-file)
	      (message "can't support window configurations without the screen package... sorry.")
	      (setq vm-window-configuration-file nil)
	      (sleep-for 2)))
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
  (let ((inhibit-local-variables t)
	(enable-local-variables nil)
	(full-startup (not (bufferp folder)))
	mail-buffer already-existed)
    (setq mail-buffer
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
		      (find-file-noselect file)))))))
    (set-buffer mail-buffer)
    (setq vm-folder-read-only read-only)
    (vm-sanity-check-modification-flag)
    (vm-check-for-killed-summary)
    ;; If the buffer's not modified then we know that there can be no
    ;; messages in the folder that are not on disk.
    (or (buffer-modified-p) (setq vm-messages-not-on-disk 0))
    (let ((first-time (not (eq major-mode 'vm-mode)))
	  (auto-save-newer (and buffer-file-name
				(file-newer-than-file-p
				 (make-auto-save-file-name)
				 buffer-file-name)))
	  (inhibit-quit t)
	  blurb)
      ;; If this is not a VM mode buffer then some initialization
      ;; needs to be done 
      (if first-time
	  (progn
	    (buffer-flush-undo (current-buffer))
	    (abbrev-mode 0)
	    (auto-fill-mode 0)
	    (vm-mode-internal)))
      (if (or (and vm-primary-inbox-p
		   ;; We demand full-startup and first-time here
		   ;; to make sure that vm-mode being in the
		   ;; auto-mode-alist doesn't cause an auto
		   ;; save file to be ignored.  Otherwise an auto
		   ;; save file might be ignored on an "M-x vm"
		   ;; if vm-mode were in in the auto-folder-alist.
		   full-startup
		   first-time
		   (not vm-folder-read-only)
		   (not auto-save-newer)
		   (vm-get-spooled-mail))
	      first-time
	      ;; If the message list is empty, take a second look: the
	      ;; buffer may have been encrypted the first time around.
	      ;; This is a concession to users who use crypt.el and put
	      ;; vm-mode into auto-mode-alist.
	      (null vm-message-list))
	  (save-excursion
	    (save-window-excursion
	      (save-restriction
		(delete-windows-on (current-buffer))
		(set-buffer mail-buffer)
		(and vm-summary-buffer (delete-windows-on vm-summary-buffer))
		(set-buffer mail-buffer)
		(vm-assimilate-new-messages)
		;; Can't allow a folder-empty error here because execution
		;; would abort before the code below.
		(if (null vm-message-list)
		    (and full-startup (message "Folder is empty."))
		  (if first-time
		      (progn
			(vm-check-header-variables)
			(vm-gobble-message-order)
			(vm-gobble-bookmark)))
		  (setq blurb (vm-emit-totals-blurb))
		  (vm-thoughtfully-select-message))))))
      (and full-startup (not blurb)
	   (setq blurb (vm-emit-totals-blurb)))
      (if (and full-startup vm-message-list vm-startup-with-summary)
	  (progn
	    (vm-summarize)
	    (message blurb)))
      (if vm-mutable-windows
	  (if (not (vm-set-window-configuration 'startup))
	      (let ((pop-up-windows (and pop-up-windows
					 (eq vm-mutable-windows t))))
		(switch-to-buffer (current-buffer))
		(and vm-summary-buffer (display-buffer vm-summary-buffer))
		(if (and (eq vm-startup-with-summary t)
			 (eq vm-mutable-windows t)
			 vm-summary-buffer)
		    (progn
		      (select-window (get-buffer-window vm-summary-buffer))
		      (delete-other-windows)))
		(if (eq vm-mutable-windows t)
		    (vm-proportion-windows))))
	(switch-to-buffer (current-buffer)))
      (set-buffer mail-buffer)
      (vm-preview-current-message)
      (if (and (numberp vm-flush-interval)
	       (condition-case data
		   (progn (require 'timer) t)
		 (error
		  (message "can't support numeric vm-flush-interval without interval timers... sorry.")
		  (sleep-for 2)
		  nil ))
	       (not (get-timer "vm-flush")))
	  (start-timer "vm-flush" 'vm-flush-timer-function vm-flush-interval nil))
      (if (and (not already-existed) auto-save-newer)
	  (progn
	    (discard-input)
	    (if vm-primary-inbox-p
		(message "Not checking for new mail... auto save file is newer; consider M-x recover-file")
	      (message "Auto save file is newer; consider M-x recover-file"))
	    (sit-for 3)))
      ;; Display copyright and copying info unless
      ;; user says no.
      (if (and full-startup
	       (not (or vm-inhibit-startup-message vm-startup-message-displayed)))
	  (progn
	    (vm-display-startup-message)
	    (if (not (input-pending-p))
		(message blurb)))))))

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

   G - group messages according to some criteria

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
       M m - mark all messsages
       M u - unmark all messsages
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
   vm-auto-next-message
   vm-berkeley-mail-compatibility
   vm-circular-folders
   vm-confirm-new-folders
   vm-confirm-quit
   vm-crash-box
   vm-delete-after-archiving
   vm-delete-after-bursting
   vm-delete-after-saving
   vm-delete-empty-folders
   vm-digest-center-preamble
   vm-digest-preamble-format
   vm-folder-directory
   vm-folder-read-only
   vm-follow-summary-cursor
   vm-forwarding-subject-format
   vm-gargle-uucp
   vm-group-by
   vm-highlighted-header-regexp
   vm-honor-page-delimiters
   vm-in-reply-to-format
   vm-included-text-attribution-format
   vm-included-text-prefix
   vm-inhibit-startup-message
   vm-invisible-header-regexp
   vm-keep-sent-messages
   vm-mail-header-from
   vm-mail-window-percentage
   vm-mode-hooks
   vm-move-after-deleting
   vm-move-after-undeleting
   vm-mutable-windows
   vm-preview-lines
   vm-preview-read-messages
   vm-primary-inbox
   vm-retain-message-order
   vm-reply-ignored-addresses
   vm-reply-subject-prefix
   vm-rfc934-forwarding
   vm-search-using-regexps
   vm-skip-deleted-messages
   vm-skip-read-messages
   vm-spool-files
   vm-startup-with-summary
   vm-strip-reply-headers
   vm-summary-format
   vm-virtual-folder-alist
   vm-virtual-mirror
   vm-visible-headers
   vm-visit-when-saving
   vm-window-configuration-file"
  (interactive)
  (vm (current-buffer)))

;; this does the real major mode scutwork.
(defun vm-mode-internal ()
  (widen)
  (setq
   case-fold-search t
   checkpoint-direct-conversion-hooks '(vm-checkpoint-prologue-hook)
   checkpoint-epilogue-hooks '(vm-checkpoint-epilogue-hook)
   major-mode 'vm-mode
   mode-line-format vm-mode-line-format
   mode-name "VM"
   buffer-read-only t
   vm-message-list nil
   vm-message-pointer nil
   vm-current-grouping vm-group-by
   vm-folder-type (vm-get-folder-type)
   vm-primary-inbox-p (equal buffer-file-name
			     (expand-file-name vm-primary-inbox)))
  (use-local-map vm-mode-map)
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
