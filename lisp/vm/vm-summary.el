;;; Summary gathering and formatting routines for VM
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

(defun vm-summary-mode ()
  "Major mode for VM folder summaries.
This major mode uses the same keymap as vm-mode.  See the vm-mode documentation
for a list of available commands."
  (setq mode-name "VM Summary"
	major-mode 'vm-summary-mode
	mode-line-format vm-mode-line-format
	buffer-read-only t
	vm-summary-pointer nil
	truncate-lines t)
  (use-local-map vm-summary-mode-map)
  (run-hooks 'vm-summary-mode-hook)
  ;; Lucid Emacs apparently used this name
  (run-hooks 'vm-summary-mode-hooks))

(put 'vm-summary-mode 'mode-class 'special)

(defun vm-summarize (&optional display)
  "Summarize the contents of the folder in a summary buffer. 
The format is as described by the variable vm-summary-format.  Generally
one line per message is most pleasing to the eye but this is not
mandatory."
  (interactive "p")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
;  (vm-error-if-folder-empty)
  (if (null vm-summary-buffer)
      (let ((b (current-buffer)))
	(setq vm-summary-buffer
	      (get-buffer-create (format "%s Summary" (buffer-name))))
	(save-excursion
	  (set-buffer vm-summary-buffer)
	  (abbrev-mode 0)
	  (auto-fill-mode 0)
	  (setq vm-mail-buffer b)
	  (vm-summary-mode))
	(vm-set-summary-redo-start-point t)))
  (if display
      (if (not (vm-set-window-configuration 'summarize))
	  (progn
	    (vm-display-buffer vm-summary-buffer)
	    (if (eq vm-mutable-windows t)
		(vm-proportion-windows)))))
  (vm-select-folder-buffer)
  (vm-update-summary-and-mode-line))

(defun vm-set-summary-redo-start-point (start-point)
  (sets-set-insert vm-buffers-needing-display-update (current-buffer))
  (if (and (consp start-point) (consp vm-summary-redo-start-point))
      (let ((mp vm-message-list))
	(while (not (or (eq mp start-point)
			(eq mp vm-summary-redo-start-point)))
	  (setq mp (cdr mp)))
	(if (null mp)
	    (error "Something is wrong in vm-set-summary-redo-start-point"))
	(if (eq mp start-point)
	    (setq vm-summary-redo-start-point start-point)))
    (setq vm-summary-redo-start-point start-point)))

(defun vm-do-summary (&optional start-point)
  (let ((mp (or start-point vm-message-list))
	(n 0)
	;; Just for laughs, make the update interval vary.
	(modulus (+ (% (vm-abs (random)) 11) 10))
	summary)
    (save-excursion
      (set-buffer vm-summary-buffer)
      (let ((buffer-read-only nil))
	(if start-point
	    (if (vm-su-start-of (car mp))
		(progn
		  (goto-char (vm-su-start-of (car mp)))
		  (delete-region (point) (point-max)))
	      (goto-char (point-max)))
	  (erase-buffer)
	  (setq vm-summary-pointer nil))
	(while mp
	  (set-buffer vm-mail-buffer)
	  (setq summary (vm-sprintf 'vm-summary-format (car mp)))
	  (set-buffer vm-summary-buffer)
	  (vm-set-su-start-of (car mp) (point-marker))
	  ;; the leading spaces are for the summary arrow
	  (insert "  " summary)
	  (vm-set-su-end-of (car mp) (point-marker))
	  (setq mp (cdr mp) n (1+ n))
	  (if (zerop (% n modulus))
	      (message "Generating summary... %d" n)))))
    (if (>= n modulus)
	(message "Generating summary... done"))
    (run-hooks vm-summary-redo-hook)))

(defun vm-do-needed-summary-rebuild ()
  (if (and vm-summary-redo-start-point vm-summary-buffer)
      (progn
	(vm-do-summary (and (consp vm-summary-redo-start-point)
			    vm-summary-redo-start-point))
	(setq vm-summary-redo-start-point nil)
	(and vm-message-pointer
	     (vm-set-summary-pointer (car vm-message-pointer)))
	(setq vm-need-summary-pointer-update nil))
    (and vm-need-summary-pointer-update
	 vm-summary-buffer
	 vm-message-pointer
	 (progn
	   (vm-set-summary-pointer (car vm-message-pointer))
	   (setq vm-need-summary-pointer-update nil)))))

(defun vm-update-message-summary (m)
  (let (summary)
    (if (and (vm-su-start-of m)
	     (marker-buffer (vm-su-start-of m)))
	(save-excursion
	  (setq summary (vm-sprintf 'vm-summary-format m))
	  (set-buffer (marker-buffer (vm-su-start-of m)))
	  (let ((buffer-read-only nil))
	    (save-excursion
	      (goto-char (vm-su-start-of m))
	      (insert (if (= (following-char) ?\ ) "  " "->") summary)
	      (delete-region (point) (vm-su-end-of m))))))))

(defun vm-set-summary-pointer (m)
  (if vm-summary-buffer
      (let ((w (get-buffer-window vm-summary-buffer))
	    (old-window nil))
	(vm-save-buffer-excursion
	  (unwind-protect
	      (progn
		(set-buffer vm-summary-buffer)
		(if w
		    (progn
		      (setq old-window (selected-window))
		      (select-window w)))
		(let ((buffer-read-only nil))
		  (if vm-summary-pointer
		      (progn
			(goto-char (vm-su-start-of vm-summary-pointer))
			(insert "  ")
			(delete-char 2)))
		  (setq vm-summary-pointer m)
		  (goto-char (vm-su-start-of m))
		  (insert "->")
		  (delete-char 2)
		  (forward-char -2)
		  (and w vm-auto-center-summary (vm-auto-center-summary))))
	    (and old-window (select-window old-window)))))))

(defun vm-mark-for-summary-update (m)
  (let ((m-list (vm-virtual-messages-of m)))
    (while m-list
      (if (eq (vm-attributes-of m) (vm-attributes-of (car m-list)))
	  (progn
	    (sets-set-insert vm-buffers-needing-display-update
			     (marker-buffer (vm-start-of (car m-list))))
	    (sets-set-insert vm-messages-needing-summary-update (car m-list))))
      (setq m-list (cdr m-list)))
    (if (or (not (vm-virtual-message-p m)) (null (vm-virtual-messages-of m)))
	(progn
	  (sets-set-insert vm-buffers-needing-display-update
			   (marker-buffer (vm-start-of m)))
	  (sets-set-insert vm-messages-needing-summary-update m)))))

(defun vm-force-mode-line-update ()
  (save-excursion
    (set-buffer (other-buffer))
    (set-buffer-modified-p (buffer-modified-p))))

(defun vm-do-needed-mode-line-update ()
  (if (null vm-message-pointer)
      ;; erase the leftover message if the folder is really empty.
      (if (eq major-mode 'vm-virtual-mode)
	  (erase-buffer))
    (setq vm-ml-message-number (vm-number-of (car vm-message-pointer)))
    (setq vm-ml-message-new (vm-new-flag (car vm-message-pointer)))
    (setq vm-ml-message-unread (vm-unread-flag (car vm-message-pointer)))
    (setq vm-ml-message-read
	  (and (not (vm-new-flag (car vm-message-pointer)))
	       (not (vm-unread-flag (car vm-message-pointer)))))
    (setq vm-ml-message-edited (vm-edited-flag (car vm-message-pointer)))
    (setq vm-ml-message-filed (vm-filed-flag (car vm-message-pointer)))
    (setq vm-ml-message-written (vm-written-flag (car vm-message-pointer)))
    (setq vm-ml-message-replied (vm-replied-flag (car vm-message-pointer)))
    (setq vm-ml-message-forwarded (vm-forwarded-flag (car vm-message-pointer)))
    (setq vm-ml-message-deleted (vm-deleted-flag (car vm-message-pointer)))
    (setq vm-ml-message-marked (vm-mark-of (car vm-message-pointer))))
  (if vm-summary-buffer
      (let ((modified (buffer-modified-p)))
	(save-excursion
	  (vm-copy-local-variables vm-summary-buffer
				   'vm-ml-message-new
				   'vm-ml-message-unread
				   'vm-ml-message-read
				   'vm-ml-message-edited
				   'vm-ml-message-replied
				   'vm-ml-message-forwarded
				   'vm-ml-message-filed
				   'vm-ml-message-written
				   'vm-ml-message-deleted
				   'vm-ml-message-marked
				   'vm-ml-message-number
				   'vm-ml-highest-message-number
				   'vm-message-list)
	  (set-buffer vm-summary-buffer)
	  (set-buffer-modified-p modified))))
  (vm-force-mode-line-update))

(defun vm-update-summary-and-mode-line ()
  (and vm-buffers-needing-display-update
       (save-excursion
	 (sets-mapset (function
		       (lambda (b)
			 (if (and (bufferp b) (buffer-name b))
			     (progn
			       (set-buffer b)
			       (vm-do-needed-renumbering)
			       (vm-do-needed-summary-rebuild)
			       (vm-do-needed-mode-line-update)))))
		      vm-buffers-needing-display-update)))
  (setq vm-buffers-needing-display-update (sets-make-set))
  (and vm-messages-needing-summary-update
       (sets-mapset 'vm-update-message-summary
		    vm-messages-needing-summary-update))
  (setq vm-messages-needing-summary-update (vm-make-message-set))
  (vm-force-mode-line-update))

(defun vm-auto-center-summary ()
  (if vm-auto-center-summary
      (if (or (eq vm-auto-center-summary t) (not (one-window-p t)))
	  (recenter '(4)))))

(defun vm-follow-summary-cursor ()
  (and vm-follow-summary-cursor (eq major-mode 'vm-summary-mode)
       (let ((point (point))
	     message-pointer message-list mp)
	 (save-excursion
	   (set-buffer vm-mail-buffer)
	   (setq message-pointer vm-message-pointer
		 message-list vm-message-list))
	 (if (or (null message-pointer)
		 (and (>= point (vm-su-start-of (car message-pointer)))
		      (< point (vm-su-end-of (car message-pointer)))))
	     ()
	   (if (< point (vm-su-start-of (car message-pointer)))
	       (setq mp message-list)
	     (setq mp (cdr message-pointer) message-pointer nil))
	   (while (and (not (eq mp message-pointer))
		       (>= point (vm-su-end-of (car mp))))
	     (setq mp (cdr mp)))
	   (if (not (eq mp message-pointer))
	       (save-excursion
		 (set-buffer vm-mail-buffer)
		 (vm-record-and-change-message-pointer
		  vm-message-pointer mp)
		 (setq vm-need-summary-pointer-update t)
		 (vm-preview-current-message)
		 ;; return non-nil so the caller will know that
		 ;; a new message was selected.
		 t ))))))

(defun vm-sprintf (format-variable message)
  ;; compile the format into an eval'able s-expression
  ;; if it hasn't been compiled already.
  (if (not (eq (get format-variable 'vm-compiled-format)
	       (symbol-value format-variable)))
      (vm-compile-format format-variable))
  ;; The local variable name `vm-su-message' is mandatory here for
  ;; the format s-expression to work.
  (let ((vm-su-message message))
    (eval (get format-variable 'vm-format-sexp))))

(defun vm-compile-format (format-variable)
  (let ((format (symbol-value format-variable))
	(case-fold-search nil)
	sexp sexp-fmt conv-spec last-match-end)
    (store-match-data nil)
    (while (string-match
"%\\(-\\)?\\([0-9]+\\)?\\(\\.\\(-?[0-9]+\\)\\)?\\([aAcdfFhHilmMnstTwyz*%]\\|U[A-Za-z]\\)"
	    format (match-end 0))
      (setq conv-spec (aref format (match-beginning 5)))
      (if (memq conv-spec '(?a ?A ?c ?d ?f ?F ?h ?H ?i ?l ?M
			    ?m ?n ?s ?t ?T ?U ?w ?y ?z ?*))
	  (progn
	    (cond ((= conv-spec ?a)
		   (setq sexp (cons (list 'vm-su-attribute-indicators
					  'vm-su-message) sexp)))
		  ((= conv-spec ?A)
		   (setq sexp (cons (list 'vm-su-attribute-indicators-long
					  'vm-su-message) sexp)))
		  ((= conv-spec ?c)
		   (setq sexp (cons (list 'vm-su-byte-count
					  'vm-su-message) sexp)))
		  ((= conv-spec ?d)
		   (setq sexp (cons (list 'vm-su-monthday
					  'vm-su-message) sexp)))
		  ((= conv-spec ?f)
		   (setq sexp (cons (list 'vm-su-interesting-from
					  'vm-su-message) sexp)))
		  ((= conv-spec ?F)
		   (setq sexp (cons (list 'vm-su-interesting-full-name
					  'vm-su-message) sexp)))
		  ((= conv-spec ?h)
		   (setq sexp (cons (list 'vm-su-hour
					  'vm-su-message) sexp)))
		  ((= conv-spec ?H)
		   (setq sexp (cons (list 'vm-su-hour-short
					  'vm-su-message) sexp)))
		  ((= conv-spec ?i)
		   (setq sexp (cons (list 'vm-su-message-id
					  'vm-su-message) sexp)))
		  ((= conv-spec ?l)
		   (setq sexp (cons (list 'vm-su-line-count
					  'vm-su-message) sexp)))
		  ((= conv-spec ?m)
		   (setq sexp (cons (list 'vm-su-month
					  'vm-su-message) sexp)))
		  ((= conv-spec ?M)
		   (setq sexp (cons (list 'vm-su-month-number
					  'vm-su-message) sexp)))
		  ((= conv-spec ?n)
		   (setq sexp (cons (list 'vm-su-message-number
					  'vm-su-message) sexp)))
		  ((= conv-spec ?s)
		   (setq sexp (cons (list (if vm-summary-no-newlines-in-subject
					      'vm-su-subject-no-newlines
					      'vm-su-subject)
					  'vm-su-message) sexp)))
		  ((= conv-spec ?T)
		   (setq sexp (cons (list 'vm-su-to-names
					  'vm-su-message) sexp)))
		  ((= conv-spec ?t)
		   (setq sexp (cons (list 'vm-su-to
					  'vm-su-message) sexp)))
		  ((= conv-spec ?U)
		   (setq sexp
			 (cons (list 'vm-run-user-summary-function
				     (list 'quote
					   (intern
					    (concat
					     "vm-summary-function-"
					     (substring
					      format
					      (1+ (match-beginning 5))
					      (+ 2 (match-beginning 5))))))
				     'vm-su-message) sexp)))
		  ((= conv-spec ?w)
		   (setq sexp (cons (list 'vm-su-weekday
					  'vm-su-message) sexp)))
		  ((= conv-spec ?y)
		   (setq sexp (cons (list 'vm-su-year
					  'vm-su-message) sexp)))
		  ((= conv-spec ?z)
		   (setq sexp (cons (list 'vm-su-zone
					  'vm-su-message) sexp)))
		  ((= conv-spec ?*)
		   (setq sexp (cons (list 'vm-su-mark
					  'vm-su-message) sexp))))
	    (cond ((match-beginning 1)
		   (setcar sexp
			   (list 'vm-left-justify-string (car sexp)
				 (string-to-int (substring format
							   (match-beginning 2)
							   (match-end 2))))))
		  ((match-beginning 2)
		   (setcar sexp
			   (list 'vm-right-justify-string (car sexp)
				 (string-to-int (substring format
							   (match-beginning 2)
							   (match-end 2)))))))
	    (cond ((match-beginning 3)
		   (setcar sexp
			   (list 'vm-truncate-string (car sexp)
				 (string-to-int (substring format
							   (match-beginning 4)
							   (match-end 4)))))))
	    (setq sexp-fmt
		  (cons "%s"
			(cons (substring format
					 (or last-match-end 0)
					 (match-beginning 0))
			      sexp-fmt))))
	(setq sexp-fmt
	      (cons "%%"
		    (cons (substring format
				     (or last-match-end 0)
				     (match-beginning 0))
			  sexp-fmt))))
      (setq last-match-end (match-end 0)))
    (setq sexp-fmt 
	  (cons (substring format
			   (or last-match-end 0)
			   (length format))
		sexp-fmt)
	  sexp-fmt (apply 'concat (nreverse sexp-fmt))
	  sexp (cons 'format (cons sexp-fmt (nreverse sexp))))
    (put format-variable 'vm-format-sexp sexp)
    (put format-variable 'vm-compiled-format format)))

(defun vm-get-header-contents (message header-name-regexp)
  (let ((contents nil)
	regexp)
    (setq regexp (concat "^" header-name-regexp)
	  message (vm-real-message-of message))
    (save-excursion
      (set-buffer (marker-buffer (vm-start-of (vm-real-message-of message))))
      (save-restriction
	(widen)
	(goto-char (vm-headers-of message))
	(let ((case-fold-search t))
	  (while (and (re-search-forward regexp (vm-text-of message) t)
		      (save-excursion (goto-char (match-beginning 0))
				      (vm-match-header)))
	    (if contents
		(setq contents
		      (concat contents ", " (vm-matched-header-contents)))
	      (setq contents (vm-matched-header-contents))))))
      contents )))

(defun vm-left-justify-string (string width)
  (if (>= (length string) width)
      string
    (concat string (make-string (- width (length string)) ?\ ))))

(defun vm-right-justify-string (string width)
  (if (>= (length string) width)
      string
    (concat (make-string (- width (length string)) ?\ ) string)))

(defun vm-truncate-string (string width)
  (cond ((<= (length string) width)
	 string)
	((< width 0)
	 (substring string width))
	(t
	 (substring string 0 width))))

(defun vm-su-attribute-indicators (m)
  (concat
   (cond ((vm-deleted-flag m) "D")
	 ((vm-new-flag m) "N")
	 ((vm-unread-flag m) "U")
	 (t " "))
   (cond ((vm-filed-flag m) "F")
	 ((vm-written-flag m) "W")
	 (t " "))
   (cond ((vm-replied-flag m) "R")
	 ((vm-forwarded-flag m) "Z")
	 (t " "))
   (cond ((vm-edited-flag m) "E")
	 (t " "))))

(defun vm-su-attribute-indicators-long (m)
  (concat
   (cond ((vm-deleted-flag m) "D")
	 ((vm-new-flag m) "N")
	 ((vm-unread-flag m) "U")
	 (t " "))
   (if (vm-replied-flag m) "r" " ")
   (if (vm-forwarded-flag m) "z" " ")
   (if (vm-filed-flag m) "f" " ")
   (if (vm-written-flag m) "w" " ")
   (if (vm-edited-flag m) "e" " ")))

(defun vm-su-byte-count (m)
  (or (vm-byte-count-of m)
      (vm-set-byte-count-of
       m
       (int-to-string
	(- (vm-text-end-of (vm-real-message-of m))
	   (vm-text-of (vm-real-message-of m)))))))

(defun vm-su-weekday (m)
  (or (vm-weekday-of m)
      (progn (vm-su-do-date m) (vm-weekday-of m))))

(defun vm-su-monthday (m)
  (or (vm-monthday-of m)
      (progn (vm-su-do-date m) (vm-monthday-of m))))

(defun vm-su-month (m)
  (or (vm-month-of m)
      (progn (vm-su-do-date m) (vm-month-of m))))

(defun vm-su-month-number (m)
  (or (vm-month-number-of m)
      (progn (vm-su-do-date m) (vm-month-number-of m))))

(defun vm-su-year (m)
  (or (vm-year-of m)
      (progn (vm-su-do-date m) (vm-year-of m))))

(defun vm-su-hour-short (m)
  (let ((string (vm-su-hour m)))
    (if (> (length string) 5)
	(substring string 0 -3)
      string)))

(defun vm-su-hour (m)
  (or (vm-hour-of m)
      (progn (vm-su-do-date m) (vm-hour-of m))))

(defun vm-su-zone (m)
  (or (vm-zone-of m)
      (progn (vm-su-do-date m) (vm-zone-of m))))

(defun vm-su-mark (m) (if (vm-mark-of m) "*" " "))

;; Some yogurt-headed delivery agents don't provide a Date: header.
(defun vm-grok-From_-date (message)
  ;; If this is MMDF, forget it.
  (if (eq (vm-message-type-of message) 'mmdf)
      nil
    (save-excursion
      (set-buffer (marker-buffer (vm-start-of (vm-real-message-of message))))
      (save-restriction
	(widen)
	(goto-char (vm-start-of message))
	(let ((case-fold-search nil))
	  (if (looking-at "From [^ \t\n]*[ \t]+\\([^ \t\n].*\\)")
	      (buffer-substring (match-beginning 1) (match-end 1))))))))

(defvar vm-parse-date-workspace (make-vector 6 nil)) ; a little GC avoidance
(defun vm-parse-date (date)
  (let ((weekday "")
	(monthday "")
	(month "")
	(year "")
	(hour "")
	(timezone "")
	(start nil)
	string
	(case-fold-search t))
    (if (string-match "sun\\|mon\\|tue\\|wed\\|thu\\|fri\\|sat" date)
	(setq weekday (substring date (match-beginning 0) (match-end 0))))
    (if (string-match "jan\\|feb\\|mar\\|apr\\|may\\|jun\\|jul\\|aug\\|sep\\|oct\\|nov\\|dec" date)
	(setq month (substring date (match-beginning 0) (match-end 0))))
    (if (string-match "[0-9]?[0-9]:[0-9][0-9]\\(:[0-9][0-9]\\)?" date)
	(setq hour (substring date (match-beginning 0) (match-end 0))))
    (if (or (string-match "[^a-z][+---][0-9][0-9][0-9][0-9]" date)
	    (string-match "e[ds]t\\|c[ds]t\\|p[ds]t\\|m[ds]t" date)
	    (string-match "ast\\|nst\\|met\\|eet\\|jst\\|bst\\|ut" date)
	    (string-match "gmt\\([+---][0-9]+\\)?" date))
	(setq timezone (substring date (match-beginning 0) (match-end 0))))
    (while (string-match "\\(\\`\\|[^:+---0-9]\\|[a-z]-\\)[0-9]+\\(\\'\\|[^:]\\)"
			 date start)
      (setq string (substring date (match-end 1) (match-beginning 2))
	    start (match-end 0))
      (cond ((string-match "\\`[4-9]." string)
	     ;; Assume that any two digits less than 40 are a date and not
	     ;; a year.  The world will surely end soon.
	     (setq year (concat "19" string)))
	    ((< (length string) 3)
	     (setq monthday string))
	    (t (setq year string))))
    
    (aset vm-parse-date-workspace 0 weekday)
    (aset vm-parse-date-workspace 1 monthday)
    (aset vm-parse-date-workspace 2 month)
    (aset vm-parse-date-workspace 3 year)
    (aset vm-parse-date-workspace 4 hour)
    (aset vm-parse-date-workspace 5 timezone)
    vm-parse-date-workspace))

(defun vm-su-do-date (m)
  (let ((case-fold-search t)
	vector date)
    (setq date (or (vm-get-header-contents m "Date:") (vm-grok-From_-date m)))
    (cond
     ((null date)
      (vm-set-weekday-of m "")
      (vm-set-monthday-of m "")
      (vm-set-month-of m "")
      (vm-set-month-number-of m "")
      (vm-set-year-of m "")
      (vm-set-hour-of m "")
      (vm-set-zone-of m ""))
     ((string-match
;; The date format recognized here is the one specified in RFC 822.
;; Some slop is allowed e.g. dashes between the monthday, month and year
;; because such malformed headers have been observed.
"\\(\\([a-z][a-z][a-z]\\),\\)?[ \t\n]*\\([0-9][0-9]?\\)[ \t\n---]*\\([a-z][a-z][a-z]\\)[ \t\n---]*\\([0-9]*[0-9][0-9]\\)[ \t\n]*\\([0-9:]+\\)[ \t\n]*\\([a-z][a-z]?[a-z]?\\|[---+][0-9][0-9][0-9][0-9]\\)"
       date)
      (if (match-beginning 2)
	  (vm-set-weekday-of m (substring date (match-beginning 2)
					  (match-end 2)))
	(vm-set-weekday-of m ""))
      (vm-set-monthday-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-su-do-month m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-year-of m (substring date (match-beginning 5) (match-end 5)))
      (if (= 2 (length (vm-year-of m)))
	  (vm-set-year-of m
	   (concat
	    ;; In an unprescedented burst of optomism that the world won't be
	    ;; plunged into chaos and darkness on 1-Jan-2000, let's assume that
	    ;; two digit years <70 are in the 21st century and not the 20th.
	    (if (memq (aref (vm-year-of m) 0) '(?7 ?8 ?9)) "19" "20")
	    (vm-year-of m))))
      (vm-set-hour-of m (substring date (match-beginning 6) (match-end 6)))
      (vm-set-zone-of m (substring date (match-beginning 7) (match-end 7))))
     ((string-match
;; UNIX ctime(3) format, with slop allowed in the whitespace, and we allow for
;; the possibility of a timezone at the end.
"\\([a-z][a-z][a-z]\\)[ \t\n]*\\([a-z][a-z][a-z]\\)[ \t\n]*\\([0-9][0-9]?\\)[ \t\n]*\\([0-9:]+\\)[ \t\n]*\\([0-9][0-9][0-9][0-9]\\)[ \t\n]*\\([a-z][a-z]?[a-z]?\\|[---+][0-9][0-9][0-9][0-9]\\)?"
       date)
      (vm-set-weekday-of m (substring date (match-beginning 1) (match-end 1)))
      (vm-su-do-month m (substring date (match-beginning 2) (match-end 2)))
      (vm-set-monthday-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-set-hour-of m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-year-of m (substring date (match-beginning 5) (match-end 5)))
      (if (match-beginning 6)
	  (vm-set-zone-of m (substring date (match-beginning 6)
				       (match-end 6)))))
     (t
      (setq vector (vm-parse-date date))
      (vm-set-weekday-of m (elt vector 0))
      (vm-set-monthday-of m (elt vector 1))
      (vm-su-do-month m (elt vector 2))
      (vm-set-year-of m (elt vector 3))
      (vm-set-hour-of m (elt vector 4))
      (vm-set-zone-of m (elt vector 5)))))

  ;; Normalize all hour and date specifications to avoid jagged margins.
  ;; If the hour is " 3:..." or "3:...", turn it into "03:...".
  ;; If the date is "03" or "3", turn it into " 3".
  (cond ((null (vm-hour-of m)) nil)
	((string-match "\\`0[0-9]:" (vm-hour-of m))
	 (aset (vm-hour-of m) 0 ?0))
	((string-match "\\`[0-9]:" (vm-hour-of m))
	 (vm-set-hour-of m (concat "0" (vm-hour-of m)))))
  (cond ((null (vm-monthday-of m)) nil)
	((string-match "\\`0[0-9]\\'" (vm-monthday-of m))
	 (aset (vm-monthday-of m) 0 ? ))
	((string-match "\\`[0-9]\\'" (vm-monthday-of m))
	 (vm-set-monthday-of m (concat " " (vm-monthday-of m)))))
  )

(defun vm-su-do-month (m month-abbrev)
  (condition-case ()
      (let ((val (symbol-value (intern (concat "vm-su-month-sym-"
					       (downcase month-abbrev))))))
	(vm-set-month-of m (car val))
	(vm-set-month-number-of m (car (cdr val))))
    (error (vm-set-month-of m "")
	   (vm-set-month-number-of m ""))))

(defun vm-run-user-summary-function (function message)
  (save-excursion
    (set-buffer (marker-buffer (vm-start-of (vm-real-message-of message))))
    (save-restriction
      (widen)
      (save-excursion
	(narrow-to-region (vm-headers-of message) (vm-text-end-of message))
	(funcall function message)))))

(defun vm-su-full-name (m)
  (or (vm-full-name-of m)
      (progn (vm-su-do-author m) (vm-full-name-of m))))

(defun vm-su-interesting-full-name (m)
  (if vm-summary-uninteresting-senders
      (let ((case-fold-search nil))
	(if (string-match vm-summary-uninteresting-senders (vm-su-from m))
	    (concat vm-summary-uninteresting-senders-arrow (vm-su-to-names m))
	  (vm-su-full-name m)))
    (vm-su-full-name m)))

(defun vm-su-from (m)
  (or (vm-from-of m)
      (progn (vm-su-do-author m) (vm-from-of m))))

(defun vm-su-interesting-from (m)
  (if vm-summary-uninteresting-senders
      (let ((case-fold-search nil))
	(if (string-match vm-summary-uninteresting-senders (vm-su-from m))
	    (concat vm-summary-uninteresting-senders-arrow (vm-su-to m))
	  (vm-su-from m)))
    (vm-su-from m)))

;; Some yogurt-headed delivery agents don't even provide a From: header.
(defun vm-grok-From_-author (message)
  ;; If this is MMDF, forget it.
  (if (eq vm-folder-type 'mmdf)
      nil
    (save-excursion
      (set-buffer (marker-buffer (vm-start-of message)))
      (save-restriction
	(widen)
	(goto-char (vm-start-of message))
	(let ((case-fold-search nil))
	  (if (looking-at "From \\([^ \t\n]+\\)")
	      (buffer-substring (match-beginning 1) (match-end 1))))))))

(defun vm-su-do-author (m)
  (let ((full-name (vm-get-header-contents m "Full-Name:"))
	(from (or (vm-get-header-contents m "From:")
		  (vm-grok-From_-author m)))
	pair)
    (if (null from)
	(progn
	  (setq from "???")
	  (if (null full-name)
	      (setq full-name "???")))
      (setq pair (funcall vm-chop-full-name-hook from)
	    from (or (nth 1 pair) from)
	    full-name (or full-name (nth 0 pair) from)))
    (vm-set-full-name-of m full-name)
    (vm-set-from-of m from)))

(defun vm-default-chop-full-name (address)
  (let ((from nil)
	(full-name nil))
    (cond ((string-match
"\\`[ \t\n]*\\([^< \t\n]+\\([ \t\n]+[^< \t\n]+\\)*\\)?[ \t\n]*<\\([^>]+\\)>[ \t\n]*\\'"
			 address)
	   (if (match-beginning 1)
	       (setq full-name
		     (substring address (match-beginning 1) (match-end 1))))
	   (setq from
		 (substring address (match-beginning 3) (match-end 3))))
	  ((string-match
"\\`[ \t\n]*\\(\\(\"[^\"]+\"\\|[^\"( \t\n]\\)+\\)[ \t\n]*(\\([^ \t\n]+\\([ \t\n]+[^ \t\n]+\\)*\\)?)[ \t\n]*\\'"
			 address)
	   (if (match-beginning 3)
	       (setq full-name
		     (substring address (match-beginning 3) (match-end 3))))
	   (setq from
		 (substring address (match-beginning 1) (match-end 1)))))
    (list full-name from)))

(autoload 'rfc822-addresses "rfc822")
(autoload 'mail-extract-address-components "mail-extr")

;; test for existence and functionality of mail-extract-address-components
;; there are versions out there that don't work right, so we run
;; some test data through it to see if we can trust it.
(defun vm-choose-chop-full-name-hook (address)
  (let ((test-data '(("kyle@uunet.uu.net" . (nil "kyle@uunet.uu.net"))))
	(failed nil)
	result)
    (while test-data
      (setq result (condition-case nil
		       (mail-extract-address-components (car (car test-data)))
		     (error nil)))
      (if (not (equal result (cdr (car test-data))))
	  ;; failed test, use default
	  (setq failed t
		test-data nil)
	(setq test-data (cdr test-data))))
    (if failed
	;; it failed, use default
	(setq vm-chop-full-name-hook 'vm-default-chop-full-name)
      ;; it passed the tests
      (setq vm-chop-full-name-hook 'mail-extract-address-components))
    (funcall vm-chop-full-name-hook address)))

(defun vm-su-do-recipients (m)
  (let ((mail-use-rfc822 t) names addresses to cc all list)
    (setq to (or (vm-get-header-contents m "To:")
		 (vm-get-header-contents m "Apparently-To:")
		 ;; desperation....
		 (user-login-name))
	  cc (vm-get-header-contents m "Cc:")
	  all to
	  all (if all (concat all ", " cc) cc)
	  addresses (rfc822-addresses all))
    (setq list (vm-parse-addresses all))
    (while list
      (cond ((string= (car list) ""))
	    ((string-match "^\\(\\([^<]+[^ \t\n]\\)[ \t\n]+\\)?<\\([^>]+\\)>"
			   (car list))
	     (if (match-beginning 2)
		 (setq names
		       (cons
			(substring (car list) (match-beginning 2)
				   (match-end 2))
			names))
	       (setq names
		     (cons
		      (substring (car list) (match-beginning 3)
				 (match-end 3))
		      names))))
	    ((string-match "[\000-\177]*(\\([^)]+\\))[\000-\177]*" (car list))
	     (setq names
		   (cons (substring (car list) (match-beginning 1)
				    (match-end 1))
			 names)))
	    (t (setq names (cons (car list) names))))
      (setq list (cdr list)))
    (setq names (nreverse names)) ; added by jwz for fixed vm-parse-addresses
    (if vm-gargle-uucp
	(while list
	  (if (string-match
"\\([^!@:.]+\\)\\(\\.[^!@:]+\\)?!\\([^!@: \t\n]+\\)\\(@\\([^!@:. \t\n]+\\)\\(.[^ \t\n]+\\)?\\)?[ \t\n]*$"
	       (car list))
	      (setcar
	       list
	       (concat
		(substring (car list) (match-beginning 3)
			   (match-end 3))
		"@"
		(if (and (match-beginning 5) (match-beginning 2)
			 (not (match-beginning 6)))
		    (concat (substring (car list) (match-beginning 5)
				       (match-end 5))
			    ".")
		  "")
		(substring (car list) (match-beginning 1)
			   (or (match-end 2) (match-end 1)))
		(if (match-end 2) "" ".UUCP"))))
	  (setq list (cdr list))))
    (vm-set-to-of m (mapconcat 'identity addresses ", "))
    (vm-set-to-names-of m (mapconcat 'identity names ", "))))

(defun vm-su-to (m)
  (or (vm-to-of m) (progn (vm-su-do-recipients m) (vm-to-of m))))

(defun vm-su-to-names (m)
  (or (vm-to-names-of m) (progn (vm-su-do-recipients m) (vm-to-names-of m))))
				  
(defun vm-su-message-id (m)
  (or (vm-message-id-of m)
      (vm-set-message-id-of m
			    (or (vm-get-header-contents m "Message-Id:")
				""))))

(defun vm-su-line-count (m)
  (or (vm-line-count-of m)
      (vm-set-line-count-of
       m
       (save-excursion
	 (set-buffer (marker-buffer (vm-start-of (vm-real-message-of m))))
	 (save-restriction
	   (widen)
	   (int-to-string
	    (count-lines (vm-text-of m) (vm-text-end-of m))))))))

(defun vm-su-message-number (m)
  (vm-number-of m))

(defun vm-su-subject (m)
  (or (vm-subject-of m)
      (vm-set-subject-of m
			 (or (vm-get-header-contents m "Subject:") ""))))

(defun vm-su-subject-no-newlines (m)
  (let ((s (vm-su-subject m)))
    (while (string-match "[ \t]*\n[ \t\n]*" s)
      (setq s (concat (substring s 0 (match-beginning 0)) " "
		      (substring s (match-end 0)))))
    s))
