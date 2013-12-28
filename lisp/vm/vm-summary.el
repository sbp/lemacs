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
  (use-local-map vm-mode-map)
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
  (vm-error-if-folder-empty)
  (if (null vm-summary-buffer)
      (let ((b (current-buffer))
	    (inhibit-quit t))
	(setq vm-summary-buffer
	      (get-buffer-create (format "%s Summary" (buffer-name))))
	(save-excursion
	  (set-buffer vm-summary-buffer)
	  (abbrev-mode 0)
	  (auto-fill-mode 0)
	  (setq vm-mail-buffer b)
	  (vm-summary-mode))
	(setq vm-summary-redo-start-point t)))
  (if display
      (if vm-mutable-windows
	  (if (not (vm-set-window-configuration 'summarize))
	      (let ((pop-up-windows (and pop-up-windows (eq vm-mutable-windows t))))
		(display-buffer vm-summary-buffer)
		(if (eq vm-mutable-windows t)
		    (vm-proportion-windows))))
	(switch-to-buffer vm-summary-buffer)))
  (vm-select-folder-buffer)
  (vm-update-summary-and-mode-line))

(defun vm-do-summary (&optional start-point)
  (let ((mp (or start-point vm-message-list))
	(n 0)
	;; Just for laughs, make the update interval vary.
	(modulus (+ (% (vm-abs (random)) 11) 10))
	summary)
    (message "Generating summary...")
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
    (message "Generating summary... done")))

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
  (let ((m-list (cons m
		      (and (eq (vm-attributes-of m)
			       (vm-attributes-of (vm-real-message-of m)))
			   (vm-virtual-messages-of m))))
	summary)
    (while m-list
      (if (and (vm-su-start-of (car m-list))
	       (buffer-name (marker-buffer (vm-su-start-of (car m-list)))))
	(save-excursion
	  (setq summary (vm-sprintf 'vm-summary-format (car m-list)))
	  (set-buffer (marker-buffer (vm-su-start-of (car m-list))))
	  (let ((inhibit-quit t) buffer-read-only)
	    (save-excursion
	      (goto-char (vm-su-start-of (car m-list)))
	      (insert (if (= (following-char) ?\ ) "  " "->") summary)
	      (delete-region (point) (vm-su-end-of (car m-list)))))))
      (setq m-list (cdr m-list)))))

(defun vm-set-summary-pointer (m)
  (if vm-summary-buffer
      (let ((w (get-buffer-window vm-summary-buffer))
	    (old-window nil)
	    (inhibit-quit t))
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

(defun vm-mark-for-display-update (message)
  (setq vm-messages-needing-display-update
	(cons message vm-messages-needing-display-update)))

(defun vm-force-mode-line-update ()
  (save-excursion
    (set-buffer (other-buffer))
    (set-buffer-modified-p (buffer-modified-p))))

(defun vm-update-summary-and-mode-line ()
  (vm-do-needed-renumbering)
  (vm-do-needed-summary-rebuild)
  (if (null vm-message-pointer)
      ()
    (setq vm-ml-message-number (vm-number-of (car vm-message-pointer)))
    (cond ((vm-new-flag (car vm-message-pointer))
	   (setq vm-ml-attributes-string "new"))
	  ((vm-unread-flag (car vm-message-pointer))
	   (setq vm-ml-attributes-string "unread"))
	  (t (setq vm-ml-attributes-string "read")))
    (cond ((vm-edited-flag (car vm-message-pointer))
	   (setq vm-ml-attributes-string
		 (concat vm-ml-attributes-string " edited"))))
    (cond ((vm-filed-flag (car vm-message-pointer))
	   (setq vm-ml-attributes-string
		 (concat vm-ml-attributes-string " filed"))))
    (cond ((vm-written-flag (car vm-message-pointer))
	   (setq vm-ml-attributes-string
		 (concat vm-ml-attributes-string " written"))))
    (cond ((vm-replied-flag (car vm-message-pointer))
	   (setq vm-ml-attributes-string
		 (concat vm-ml-attributes-string " replied"))))
    (cond ((vm-forwarded-flag (car vm-message-pointer))
	   (setq vm-ml-attributes-string
		 (concat vm-ml-attributes-string " forwarded"))))
    (cond ((vm-deleted-flag (car vm-message-pointer))
	   (setq vm-ml-attributes-string
		 (concat vm-ml-attributes-string " deleted"))))
    (cond ((vm-mark-of (car vm-message-pointer))
	   (setq vm-ml-attributes-string
		 (concat vm-ml-attributes-string " MARKED")))))
  (if (and vm-summary-buffer (not vm-real-buffers))
      (vm-copy-local-variables vm-summary-buffer
			       'vm-ml-attributes-string
			       'vm-ml-message-number
			       'vm-ml-highest-message-number
			       'vm-buffer-modified-p
			       'vm-message-list))
  (while vm-messages-needing-display-update
    (vm-update-message-summary (car vm-messages-needing-display-update))
    (setq vm-messages-needing-display-update
	  (cdr vm-messages-needing-display-update)))
  (and vm-deferred-message
       (progn
	 (message vm-deferred-message)
	 (setq vm-deferred-message nil)))
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
  (if (not (eq (get format-variable 'vm-compiled-format)
	       (symbol-value format-variable)))
      (vm-compile-format format-variable))
  ;; The local variable name `vm-su-message' is mandatory here for
  ;; the format s-expression to work.
  (let ((vm-su-message message))
    (eval (get format-variable 'vm-format-sexp))))

(defun vm-compile-format (format-variable)
  (if (null vm-uninteresting-senders) ; jwz: added this.
      (setq vm-uninteresting-senders (concat "\\b" (user-login-name) "\\b")))
  (let ((format (symbol-value format-variable))
	sexp sexp-fmt conv-spec last-match-end case-fold-search)
    (store-match-data nil)
    (while (string-match   ; jwz: added "uUH".
"%\\(-\\)?\\([0-9]+\\)?\\(\\.\\([0-9]+\\)\\)?\\([aAcdfFhHilmMnstTuUwyz*%]\\)"
	    format (match-end 0))
      (setq conv-spec (aref format (match-beginning 5)))
      (if (memq conv-spec '(?a ?A ?c ?d ?f ?F ?h ?H ?i ?l ?M
			    ?m ?n ?s ?t ?T ?u ?U ?w ?y ?z ?*)) ; jwz: ?[uUH]
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
		   (setq sexp (cons (list 'vm-su-from
					  'vm-su-message) sexp)))
		  ((= conv-spec ?F)
		   (setq sexp (cons (list 'vm-su-full-name
					  'vm-su-message) sexp)))
		  ((= conv-spec ?h)
		   (setq sexp (cons (list 'vm-su-hour
					  'vm-su-message) sexp)))
		  ;; jwz: added this.
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
		  ;; jwz: added this.
		  ((= conv-spec ?U)
		   (setq sexp (cons (list 'vm-su-dwim-user-name
					  'vm-su-message) sexp)))
		  ;; jwz: added this.
		  ((= conv-spec ?u)
		   (setq sexp (cons (list 'vm-su-dwim-user
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

(defun vm-get-header-contents (message header-name)
  (let (contents regexp)
    (setq regexp (format vm-header-regexp-format header-name))
    (save-excursion
      (set-buffer (marker-buffer (vm-start-of message)))
      (save-restriction
	(widen)
	(goto-char (vm-start-of message))
	(while (re-search-forward regexp (vm-text-of message) t)
	  (if contents
	      (setq contents
		    (concat
		     contents ", "
		     (buffer-substring (match-beginning 1) (match-end 1))))
	    (setq contents
		  (buffer-substring (match-beginning 1) (match-end 1)))))
	contents))))

(defun vm-left-justify-string (string width)
  (if (>= (length string) width)
      string
    (concat string (make-string (- width (length string)) ?\ ))))

(defun vm-right-justify-string (string width)
  (if (>= (length string) width)
      string
    (concat (make-string (- width (length string)) ?\ ) string)))

(defun vm-truncate-string (string width)
  (if (<= (length string) width)
      string
    (substring string 0 width)))

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
      (vm-set-byte-count-of m (int-to-string
			       (- (vm-text-end-of m) (vm-text-of m))))))

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
  ;; always return a 2-digit year
  (let ((year (or (vm-year-of m)
		  (progn (vm-su-do-date m) (vm-year-of m)))))
    (if (string-match "\\`[0-9][0-9][0-9][0-9]\\'" year)
	(substring year 2 4)
      year)))

(defun vm-su-hour (m)
  (or (vm-hour-of m)
      (progn (vm-su-do-date m) (vm-hour-of m))))

(defun vm-su-hour-short (m)
  (let ((string (vm-su-hour m)))
    (cond ((eq 8 (length string))
	   (substring string 0 5))
	  ((eq 7 (length string))
	   (concat "0" (substring string 0 4)))
	  (t string))))

(defun vm-su-zone (m)
  (or (vm-zone-of m)
      (progn (vm-su-do-date m) (vm-zone-of m))))

(defun vm-su-mark (m) (if (vm-mark-of m) "*" " "))

;; Some yogurt-headed delivery agents don't provide a Date: header.
(defun vm-grok-From_-date (message)
  ;; If this is MMDF, forget it.
  (if (eq vm-folder-type 'mmdf)
      nil
    (save-excursion
      (set-buffer (marker-buffer (vm-start-of message)))
      (save-restriction
	(widen)
	(goto-char (vm-start-of message))
	(if (looking-at "From [^ \t\n]+[ \t]+\\([^ \t\n].*\\)")
	    (buffer-substring (match-beginning 1) (match-end 1)))))))

(defun vm-su-do-date (m)
  (let (date
	(case-fold-search t))
    (setq date (or (vm-get-header-contents m "Date") (vm-grok-From_-date m)))
    (cond
     ((null date)
      (vm-set-weekday-of m "")
      (vm-set-monthday-of m "")
      (vm-set-month-of m "")
      (vm-set-month-number-of m "")
      (vm-set-year-of m "")
      (vm-set-hour-of m "")
      (vm-set-zone-of m "")
      nil)
     ((string-match
;; The date format recognized here is the one specified in RFC 822.
;; Some slop is allowed e.g. dashes between the monthday, month and year
;; because such malformed headers have been observed.
;; jwz: added some more slop - dashes between time and zone.
;; Also allow DOTW to be at end, optionally in parens.
;;
;; Handles:
;;     Mon, 3 dec 90 15:25:36 PST
;;     Mon 3 dec 90 15:25:36 PST
;;     03-dec-90 15:25:36 PST
;;     Mon, 3 dec 90 15:25:36-PST
;;     3 Dec 90 15:25 PST (Mon)
"\\(\\([a-z][a-z][a-z]\\),?\\)?[ \t\n]*0?\\([0-9][0-9]?\\)[ \t\n---]*\\([a-z][a-z][a-z]\\)[ \t\n---]*\\([0-9]*[0-9][0-9]\\)[ \t\n]*\\([0-9:]+\\)[- \t\n]*\\([a-z][a-z]?[a-z]?\\|[---+][0-9][0-9][0-9][0-9]\\)[ \t\n]*(?\\([a-z][a-z][a-z]\\)?)?"
       date)
      (if (match-beginning 2)
	  (vm-set-weekday-of m (substring date (match-beginning 2)
					  (match-end 2)))
	  (if (match-beginning 8)
	      (vm-set-weekday-of m (substring date (match-beginning 8)
					      (match-end 8)))
	      (vm-set-weekday-of m "")))
      (vm-set-monthday-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-su-do-month m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-year-of m (substring date (match-beginning 5) (match-end 5)))
      (vm-set-hour-of m (substring date (match-beginning 6) (match-end 6)))
      (vm-set-zone-of m (substring date (match-beginning 7) (match-end 7)))
      t)
     ((string-match
;; UNIX ctime(3) format, with slop allowed in the whitespace, and we allow for
;; the possibility of a timezone at the end.
;; jwz: more slop - allow a comma after the weekday.
;; Handles:
;;     Tue Dec 11 00:13:13 1990
;;     Mon Sep  2 10:42:17 1991
;;     Tue, Dec 11 00:13:13 90 PST
 "\\([a-z][a-z][a-z]\\)[ \t\n,]+\\([a-z][a-z][a-z]\\)[ \t\n]+\\([0-9][0-9]?\\)[ \t\n]*\\([0-9:]+\\)[ \t\n]*[0-9][0-9]\\([0-9][0-9]\\)[ \t\n]*\\([a-z][a-z]?[a-z]?\\|[---+][0-9][0-9][0-9][0-9]\\)?"
       date)
      (vm-set-weekday-of m (substring date (match-beginning 1) (match-end 1)))
      (vm-su-do-month m (substring date (match-beginning 2) (match-end 2)))
      (vm-set-monthday-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-set-hour-of m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-year-of m (substring date (match-beginning 5) (match-end 5)))
      ;; jwz: don't allow nil in the zone slot.
      (vm-set-zone-of m (if (match-beginning 6)
			    (substring date (match-beginning 6)
				       (match-end 6))
			    ""))
      t)
     ((string-match
;; This piece of crap handles:
;;    Aug 29, 1991 08:51 EDT
;;    Aug 29 1991, 08:51-EDT
 "\\`[ \t]*\\([a-z][a-z][a-z]\\)[ \t\n]+\\([0-9][0-9]?\\)[ \t\n,]*\\([0-9]?[0-9]?[0-9][0-9]\\)[- \t,]*\\([0-9:]+\\)[- \t]*\\([a-z][a-z][a-z]\\)?"
	date)
      (vm-su-do-month m (substring date (match-beginning 1) (match-end 1)))
      (vm-set-monthday-of m (substring date (match-beginning 2) (match-end 2)))
      (vm-set-year-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-set-hour-of m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-zone-of m (if (match-beginning 5)
			    (substring date (match-beginning 5)
				       (match-end 5))
			    ""))
      t)
     ((string-match
;; YASTF: Yet Another Sucky Time Format.  Dan Jacobson found this one.
;; Handles:
;;    Tue, Dec 11 12:11:45 CST 1990
;;    Tue Dec 11 12:11 CST 1990
"\\`[ \t]*\\([a-z][a-z][a-z]\\)[ \t\n,]*\\([a-z][a-z][a-z]\\)[ \t\n]*\\([0-9][0-9]?\\)[ \t\n]*\\([0-9:]+\\)[ \t\n]*\\([a-z][a-z][a-z]\\|[-+]?[0-9][0-9][0-9][0-9]\\)[ \t\n]*\\([0-9]?[0-9]?[0-9][0-9]\\)?"
       date)
      (vm-set-weekday-of m (substring date (match-beginning 1) (match-end 1)))
      (vm-su-do-month m (substring date (match-beginning 2) (match-end 2)))
      (vm-set-monthday-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-set-hour-of m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-zone-of m (if (match-beginning 5)
			    (substring date (match-beginning 5)
				       (match-end 5))
			    ""))
      (vm-set-year-of m (if (match-beginning 6)
			    (substring date (match-beginning 6) (match-end 6))
			    ""))
      t)
     ((string-match
;; jwz: this one is for
;;    Tue, 7 Jan  08:46:34 1992
 "\\`[ \t]*\\([a-z][a-z][a-z]\\)[ \t\n,]+\\([0-9][0-9]?\\)[ \t\n]+\\([a-z][a-z][a-z]\\)[ \t\n]+\\([0-9:]+\\)[ \t\n]*\\([0-9]?[0-9]?[0-9][0-9]\\)?"
	date)
      (vm-set-weekday-of m (substring date (match-beginning 1) (match-end 1)))
      (vm-set-monthday-of m (substring date (match-beginning 2) (match-end 2)))
      (vm-su-do-month m (substring date (match-beginning 3) (match-end 3)))
      (vm-set-hour-of m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-year-of m (if (match-beginning 5)
			    (substring date (match-beginning 5)
				       (match-end 5))
			  ""))
      (vm-set-zone-of m "")
      t)
     ((string-match
;; jwz: I've seen dates of the form "13-AUG-1990 16:11:21.26" from VMS.
	"\\`[ \t]*\\([0-9]+\\)[- \t]*\\([A-Z][A-Z][A-Z]+\\)[- \t]*\\([0-9][0-9][0-9]?[0-9]?\\)[ \t]+\\([0-9:]+\\)\\(\\.[0-9]+\\)?"
	date)
      (vm-set-weekday-of m "")
      (vm-set-monthday-of m (substring date (match-beginning 1) (match-end 1)))
      (vm-su-do-month m (substring date (match-beginning 2) (match-end 2)))
      (vm-set-year-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-set-hour-of m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-zone-of m "")
      t)
     (t
      ;; otherwise, punt.
      (vm-set-weekday-of m "")
      (vm-set-monthday-of m "")
      (vm-set-month-of m "")
      (vm-set-month-number-of m "")
      (vm-set-year-of m "")
      (vm-set-hour-of m "")
      (vm-set-zone-of m "")
      nil))))

(defconst vm-su-month-sym-jan '("January" "1"))
(defconst vm-su-month-sym-feb '("February" "2"))
(defconst vm-su-month-sym-mar '("March" "3"))
(defconst vm-su-month-sym-apr '("April" "4"))
(defconst vm-su-month-sym-may '("May" "5"))
(defconst vm-su-month-sym-jun '("June" "6"))
(defconst vm-su-month-sym-jul '("July" "7"))
(defconst vm-su-month-sym-aug '("August" "8"))
(defconst vm-su-month-sym-sep '("September" "9"))
(defconst vm-su-month-sym-oct '("October" "10"))
(defconst vm-su-month-sym-nov '("November" "11"))
(defconst vm-su-month-sym-dec '("December" "12"))

(defun vm-su-do-month (m month-abbrev)
  (condition-case ()
      (let ((val (symbol-value (intern (concat "vm-su-month-sym-"
					       (downcase month-abbrev))))))
	(vm-set-month-of m (car val))
	(vm-set-month-number-of m (car (cdr val))))
    (error (vm-set-month-of m "???")
	   (vm-set-month-number-of m "?"))))


(defun vm-su-full-name (m)
  (or (vm-full-name-of m)
      (progn (vm-su-do-author m) (vm-full-name-of m))))

(defun vm-su-from (m)
  (or (vm-from-of m)
      (progn (vm-su-do-author m) (vm-from-of m))))

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
	(if (looking-at "From \\([^ \t\n]+\\)")
	    (buffer-substring (match-beginning 1) (match-end 1)))))))

;;; There are many systems where the user's real name is encoded in the
;;; user id.  This version of vm-su-do-author parses the user-id to extract
;;; the real name, so that the %F directive does what you want more of the
;;; time.  It handles the following forms of addresses:
;;;
;;;	Jamie.Zawinski@somehost		--> "Jamie Zawinski"
;;;	Jamie_Zawinski@somehost		--> "Jamie Zawinski"
;;;	Jamie_W._Zawinski@somehost	--> "Jamie W. Zawinski"
;;;	Jamie.W.Zawinski@somehost	--> "Jamie W Zawinski"
;;;	"Jamie Zawinski"@somehost	--> "Jamie Zawinski"
;;; also
;;;	jwz ("Jamie Zawinski")		--> "Jamie Zawinski"
;;;	jwz (Jamie Zawinski (comment))	--> "Jamie Zawinski"
;;;	jwz (Jamie Zawinski -- comment)	--> "Jamie Zawinski"
;;; and likewise in the "name <uid>" form.

(defun vm-su-do-author (m)
  (let (full-name from)
    (setq full-name (vm-get-header-contents m "Full-Name"))
    (setq from (or (vm-get-header-contents m "From") (vm-grok-From_-author m)))
    (cond ((null from)
	   (setq from "???")
	   (if (null full-name)
	       (setq full-name "???")))
	  ((string-match "^\\([^< \t\n]+\\([ \t\n]+[^< \t\n]+\\)*\\)?[ \t\n]*\\(<\\([^>]+\\)>\\)"
			 from)
	   ;; Matches "Real Name <uid>"
	   (if (and (match-beginning 1) (null full-name))
	       (setq full-name
		     (substring from (match-beginning 1) (match-end 1))))
	   (setq from (substring from (match-beginning 4) (match-end 4))))
	  ((string-match "^[^(]*(\\(.*\\))[^)]*$" from)
	   ;; Matches "uid (Real Name)" as well as "uid (real (really) name)"
	   ;; and "uid (real name (comment))"
	   (if (null full-name)
	       (setq full-name (substring from (match-beginning 1)
					  (match-end 1))))
	   (setq from
		 (concat
		  (substring from (match-beginning 0) (1- (match-beginning 1)))
		  (substring from (1+ (match-end 1)) (match-end 0))))))
    ;; ewe ewe see pee...
    (if (and vm-gargle-uucp (string-match
"\\([^!@:.]+\\)\\(\\.[^!@:]+\\)?!\\([^!@: \t\n]+\\)\\(@\\([^!@:. \t\n]+\\)\\(.[^ \t\n]+\\)?\\)?[ \t\n]*$"
			     from))
	(setq from
	      (concat
	       (substring from (match-beginning 3) (match-end 3)) "@"
	       (if (and (match-beginning 5) (match-beginning 2)
			(not (match-beginning 6)))
		   (concat (substring from (match-beginning 5) (match-end 5))
			   ".")
		 "")
	       (substring from (match-beginning 1)
			  (or (match-end 2) (match-end 1)))
	       (if (match-end 2) "" ".UUCP"))))
    (if (or (null full-name) (string-match "^[ \t]*$" full-name))
	(setq full-name from))
    ;; derive username from address if address is of the form "User.Name@Host"
    ;; or "User_Name@Host" or "\"User Name\"@Host".
    (if (or (string-match "^[^!@%]+[._][^._%@]*[^@%][@%]" full-name)
	    (string-match "^[^!@%]*\"[^!@%]+ [^ %@]*[^@%][@%]" full-name))
	(setq full-name (substring full-name 0 (1- (match-end 0)))))
    (setq full-name (vm-clean-username full-name))
    (vm-set-full-name-of m full-name)
    (vm-set-from-of m from)))

(defun vm-clean-username (string)
  "Strips garbage from the user full name string."
  (if (string-match "[%@!]" string)  ; this ain't no user name!  It's an address!
      string
    (let ((case-fold-search t))
      ;; take off leading and trailing non-alpha chars (quotes, parens, digits, etc)
      (if (string-match "\\`[^a-z]+" string)
	  (setq string (substring string (match-end 0))))
      (if (string-match "[^a-z]+\\'" string)
	  (setq string (substring string 0 (match-beginning 0))))
      ;; replace tabs, multiple spaces, dots, and underscores with a single space.
      ;; but don't replace ". " with " " because that could be an initial.
      (while (string-match "\\(\t\\|  +\\|\\(\\.\\)[^ \t_]\\|_+\\)" string)
	(setq string (concat (substring string 0
					(or (match-beginning 2)
					    (match-beginning 1)))
			     " "
			     (substring string (or (match-end 2)
						   (match-end 1))))))
      ;; If the string contains trailing parenthesized comments, nuke 'em.
      ;; (As in "John Doe -- Pinhead" or "John Doe (Pinhead)".)
      (if (string-match "[^ \t]\\([ \t]*\\((\\| --\\)\\)" string)
	  (progn
	    (setq string (substring string 0 (match-beginning 1)))
	    ;; lose any non-alpha rubbish this may have exposed.
	    (if (string-match "[^a-z]+\\'" string)
		(setq string (substring string 0 (match-beginning 0))))))
      string)))

(autoload 'rfc822-addresses "rfc822")

(defun vm-su-do-recipients (m)
  (let ((mail-use-rfc822 t) names addresses to cc all list)
    (setq to (or (vm-get-header-contents m "To")
		 (vm-get-header-contents m "Apparently-To")
		 ;; desperation....
		 (user-login-name))
	  cc (vm-get-header-contents m "Cc")
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
				  
(defun vm-su-dwim-user (m)
  (let ((from (vm-su-from m)))
    (if (string-match vm-uninteresting-senders from)
	(concat vm-uninteresting-arrow 
	  (let ((to (vm-su-to m)))
	    (if (string= to (user-login-name)) ; vm-do-recipients returns this
		(or (vm-get-header-contents m "Newsgroups") ; if there's no To:
		    to)
		to)))
	from)))

(defun vm-su-dwim-user-name (m)
  (if (string-match vm-uninteresting-senders (vm-su-from m))
      (concat vm-uninteresting-arrow 
	(let ((to (vm-su-to-names m)))
	  (if (string= to (user-login-name)) ;; vm-do-recipients returns this
	      (or (vm-get-header-contents m "Newsgroups")  ;; if there's no To:
		  to)
	      to)))
      (vm-su-full-name m)))

(defun vm-su-message-id (m)
  (or (vm-message-id-of m)
      (vm-set-message-id-of m
			    (or (vm-get-header-contents m "Message-Id")
				""))))

(defun vm-su-line-count (m)
  (or (vm-line-count-of m)
      (vm-set-line-count-of
       m
       (vm-within-current-message-buffer
       (save-restriction
	 (widen)
	 (int-to-string
	  (count-lines (vm-text-of m) (vm-text-end-of m))))))))

(defun vm-su-message-number (m)
  (vm-number-of m))

(defun vm-su-subject (m)
  (or (vm-subject-of m)
      (vm-set-subject-of m
			 (or (vm-get-header-contents m "Subject") ""))))

(defun vm-su-subject-no-newlines (m)
  (let ((s (vm-su-subject m)))
    (while (string-match "[ \t]*\n[ \t\n]*" s)
      (setq s (concat (substring s 0 (match-beginning 0)) " "
		      (substring s (match-end 0)))))
    s))
