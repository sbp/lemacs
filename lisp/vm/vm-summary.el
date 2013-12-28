;;; Summary gathering and formatting routines for VM
;;; Copyright (C) 1989, 1990, 1993, 1994 Kyle E. Jones
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
	vm-summary-=> (if (stringp vm-summary-arrow) vm-summary-arrow "")
	vm-summary-no-=> (make-string (length vm-summary-=>) ? )
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
  (if (null vm-summary-buffer)
      (let ((b (current-buffer))
	    (read-only vm-folder-read-only))
	(setq vm-summary-buffer
	      (get-buffer-create (format "%s Summary" (buffer-name))))
	(save-excursion
	  (set-buffer vm-summary-buffer)
	  (abbrev-mode 0)
	  (auto-fill-mode 0)
	  (if (fboundp 'buffer-disable-undo)
	      (buffer-disable-undo (current-buffer))
	    ;; obfuscation to make the v19 compiler not whine
	    ;; about obsolete functions.
	    (let ((x 'buffer-flush-undo))
	      (funcall x (current-buffer))))
	  (setq vm-mail-buffer b
		vm-folder-read-only read-only)
	  (vm-summary-mode))
	(vm-set-summary-redo-start-point t)))
  (if display
      (vm-display vm-summary-buffer t
		  '(vm-summarize
		    vm-summarize-other-frame)
		  (list this-command))
    (vm-display nil nil '(vm-summarize vm-summarize-other-frame)
		(list this-command)))
  (vm-update-summary-and-mode-line))

(defun vm-summarize-other-frame (&optional display)
  "Like vm-summarize, but run in a newly created frame."
  (interactive "p")
  (vm-goto-new-frame)
  (vm-summarize display))

(defun vm-do-summary (&optional start-point)
  (let ((m-list (or start-point vm-message-list))
	mp
	(n 0)
	;; Just for laughs, make the update interval vary.
	(modulus (+ (% (vm-abs (random)) 11) 10))
	summary)
    (setq mp m-list)
    (save-excursion
      (set-buffer vm-summary-buffer)
      (let ((buffer-read-only nil)
	    (modified (buffer-modified-p)))
	(unwind-protect
	    (progn
	      (if start-point
		  (if (vm-su-start-of (car mp))
		      (progn
			(goto-char (vm-su-start-of (car mp)))
			(delete-region (point) (point-max)))
		    (goto-char (point-max)))
		(erase-buffer)
		(setq vm-summary-pointer nil))
	      ;; avoid doing long runs down the marker chain while
	      ;; building the summary.  use integers to store positions
	      ;; and then convert them to markers after all the
	      ;; insertions are done.
	      (while mp
		(setq summary (vm-su-summary (car mp)))
		(vm-set-su-start-of (car mp) (point))
		(insert vm-summary-no-=>)
		(vm-tokenized-summary-insert (car mp) (vm-su-summary (car mp)))
		(vm-set-su-end-of (car mp) (point))
		(setq mp (cdr mp) n (1+ n))
		(if (zerop (% n modulus))
		    (message "Generating summary... %d" n)))
	      ;; now convert the ints to markers.
	      (if (>= n modulus)
		  (message "Generating summary markers... "))
	      (setq mp m-list)
	      (while mp
		(vm-set-su-start-of (car mp) (vm-marker (vm-su-start-of (car mp))))
		(vm-set-su-end-of (car mp) (vm-marker (vm-su-end-of (car mp))))
		(setq mp (cdr mp))))
	  (set-buffer-modified-p modified))
	(run-hooks 'vm-summary-redo-hook)))
    (if (>= n modulus)
	(message "Generating summary... done"))))

(defun vm-do-needed-summary-rebuild ()
  (if (and vm-summary-redo-start-point vm-summary-buffer)
      (progn
	(vm-copy-local-variables vm-summary-buffer 'vm-summary-show-threads)
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
  (let ((modified (buffer-modified-p))
	summary)
    (if (and (vm-su-start-of m)
	     (marker-buffer (vm-su-start-of m)))
	(save-excursion
	  (setq summary (vm-su-summary m))
	  (set-buffer (marker-buffer (vm-su-start-of m)))
	  (let ((buffer-read-only nil)
		(selected nil)
		(modified (buffer-modified-p)))
	    (unwind-protect
		(save-excursion
		  (goto-char (vm-su-start-of m))
		  (if (looking-at vm-summary-no-=>)
		      (insert vm-summary-no-=>)
		    (setq selected t)
		    (if vm-summary-overlay
			(vm-summary-delete-overlay))
		    (insert vm-summary-=>))
		  (vm-tokenized-summary-insert m (vm-su-summary m))
		  (delete-region (point) (vm-su-end-of m))
		  (if (and selected vm-summary-highlight-face)
		      (vm-summary-highlight-region (vm-su-start-of m) (point)
						   vm-summary-highlight-face)))
	      (set-buffer-modified-p modified)))))))

(defun vm-set-summary-pointer (m)
  (if vm-summary-buffer
      (let ((w (vm-get-buffer-window vm-summary-buffer))
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
			(insert vm-summary-no-=>)
			(delete-char (length vm-summary-=>))
			(if vm-summary-overlay
			    (vm-summary-delete-overlay))))
		  (setq vm-summary-pointer m)
		  (goto-char (vm-su-start-of m))
		  (let ((modified (buffer-modified-p)))
		    (unwind-protect
			(progn
			  (insert vm-summary-=>)
			  (delete-char (length vm-summary-=>)))
		      (set-buffer-modified-p modified)))
		  (forward-char (- (length vm-summary-=>)))
		  (if vm-summary-highlight-face
		      (vm-summary-highlight-region
		       (vm-su-start-of m) (vm-su-end-of m)
		       vm-summary-highlight-face))
		  (and w vm-auto-center-summary (vm-auto-center-summary))
		  (run-hooks 'vm-summary-pointer-update-hook)))
	    (and old-window (select-window old-window)))))))

(defun vm-summary-highlight-region (start end face)
  (cond ((fboundp 'make-overlay)
	 (setq vm-summary-overlay (make-overlay start end))
	 (overlay-put vm-summary-overlay 'face face))
	((fboundp 'make-extent)
	 (setq vm-summary-overlay (make-extent start end))
	 (set-extent-property vm-summary-overlay 'face face))))

(defun vm-summary-delete-overlay ()
  (let ((o vm-summary-overlay))
    (setq vm-summary-overlay nil)
    (cond ((fboundp 'delete-overlay)
	   (delete-overlay o))
	  ((fboundp 'delete-extent)
	   (delete-extent o)))))

(defun vm-auto-center-summary ()
  (if vm-auto-center-summary
      (if (or (eq vm-auto-center-summary t) (not (one-window-p t)))
	  (recenter '(4)))))

(defun vm-sprintf (format-variable message &optional tokenize)
  ;; compile the format into an eval'able s-expression
  ;; if it hasn't been compiled already.
  (if (not (eq (get format-variable 'vm-compiled-format)
	       (symbol-value format-variable)))
      (vm-compile-format format-variable tokenize))
  ;; The local variable name `vm-su-message' is mandatory here for
  ;; the format s-expression to work.
  (let ((vm-su-message message))
    (eval (get format-variable 'vm-format-sexp))))

(defun vm-tokenized-summary-insert (message tokens)
  (if (stringp tokens)
      (insert tokens)
    (let (token)
      (while tokens
	(setq token (car tokens))
	(cond ((stringp token)
	       (insert token))
	      ((eq token 'number)
	       (insert (vm-padded-number-of message)))
	      ((eq token 'mark)
	       (insert (vm-su-mark message)))
	      ((eq token 'thread-indent)
	       (if (and vm-summary-show-threads
			(natnump vm-summary-thread-indent-level))
		   (insert-char ?\ (* vm-summary-thread-indent-level
				      (vm-th-thread-indention message))))))
	(setq tokens (cdr tokens))))))

(defun vm-compile-format (format-variable &optional tokenize)
  (let ((format (symbol-value format-variable))
	(case-fold-search nil)
	(done nil)
	(list nil)
	(sexp nil)
	(sexp-fmt nil)
	(last-match-end 0)
	token conv-spec)
    (store-match-data nil)
    (while (not done)
      (setq token nil)
      (while
	  (and (not token)
	       (string-match
		"%\\(-\\)?\\([0-9]+\\)?\\(\\.\\(-?[0-9]+\\)\\)?\\([aAcdfFhHiIlLmMnstTwyz*%]\\|U[A-Za-z]\\)"
		format (match-end 0)))
	(setq conv-spec (aref format (match-beginning 5)))
	(if (memq conv-spec '(?a ?A ?c ?d ?f ?F ?h ?H ?i ?L ?I ?l ?M
				 ?m ?n ?s ?t ?T ?U ?w ?y ?z ?* ))
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
		    ((= conv-spec ?I)
		     (if tokenize
			 (setq token ''thread-indent)
		       (setq sexp (cons (list 'vm-su-thread-indent
					      'vm-su-message) sexp))))
		    ((= conv-spec ?l)
		     (setq sexp (cons (list 'vm-su-line-count
					    'vm-su-message) sexp)))
		    ((= conv-spec ?L)
		     (setq sexp (cons (list 'vm-su-labels
					    'vm-su-message) sexp)))
		    ((= conv-spec ?m)
		     (setq sexp (cons (list 'vm-su-month
					    'vm-su-message) sexp)))
		    ((= conv-spec ?M)
		     (setq sexp (cons (list 'vm-su-month-number
					    'vm-su-message) sexp)))
		    ((= conv-spec ?n)
		     (if tokenize
			 (setq token ''number)
		       (setq sexp (cons (list 'vm-padded-number-of
					      'vm-su-message) sexp))))
		    ((= conv-spec ?s)
		     (setq sexp (cons (list 'vm-su-subject
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
		     (if tokenize
			 (setq token ''mark)
		       (setq sexp (cons (list 'vm-su-mark
					      'vm-su-message) sexp)))))
	      (cond ((and (not token) (match-beginning 1))
		     (setcar sexp
			     (list 'vm-left-justify-string (car sexp)
				   (string-to-int
				    (substring format
					       (match-beginning 2)
					       (match-end 2))))))
		    ((and (not token) (match-beginning 2))
		     (setcar sexp
			     (list 'vm-right-justify-string (car sexp)
				   (string-to-int
				    (substring format
					       (match-beginning 2)
					       (match-end 2)))))))
	      (cond ((and (not token) (match-beginning 3))
		     (setcar sexp
			     (list 'vm-truncate-string (car sexp)
				   (string-to-int
				    (substring format
					       (match-beginning 4)
					       (match-end 4)))))))
	      (setq sexp-fmt
		    (cons (if token "" "%s")
			  (cons (substring format
					   last-match-end
					   (match-beginning 0))
				sexp-fmt))))
	  (setq sexp-fmt
		(cons "%%"
		      (cons (substring format
				       (or last-match-end 0)
				       (match-beginning 0))
			    sexp-fmt))))
	  (setq last-match-end (match-end 0)))
      (if (not token)
	  (setq sexp-fmt
		(cons (substring format last-match-end (length format))
		      sexp-fmt)
		done t))
      (setq sexp-fmt (apply 'concat (nreverse sexp-fmt)))
      (if sexp
	  (setq sexp (cons 'format (cons sexp-fmt (nreverse sexp))))
	(setq sexp sexp-fmt))
      (if tokenize
	  (setq list (nconc list (if (equal sexp "") nil (list sexp))
			    (and token (list token)))
		sexp nil
		sexp-fmt nil)))
    (put format-variable 'vm-compiled-format format)
    (put format-variable 'vm-format-sexp (if list (cons 'list list) sexp))))

(defun vm-get-header-contents (message header-name-regexp)
  (let ((contents nil)
	regexp)
    (setq regexp (concat "^\\(" header-name-regexp "\\)")
	  message (vm-real-message-of message))
    (save-excursion
      (set-buffer (vm-buffer-of (vm-real-message-of message)))
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
	 ((vm-redistributed-flag m) "B")
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
   (if (vm-redistributed-flag m) "b" " ")
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
	(substring string 0 5)
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
      (set-buffer (vm-buffer-of (vm-real-message-of message)))
      (save-restriction
	(widen)
	(goto-char (vm-start-of message))
	(let ((case-fold-search nil))
	  (if (looking-at "From [^ \t\n]*[ \t]+\\([^ \t\n].*\\)")
	      (buffer-substring (match-beginning 1) (match-end 1))))))))

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
;;	  (vm-set-year-of m (concat "19" (vm-year-of m)))
	  (vm-set-year-of m
	   (concat
	    ;; In an unprescedented burst of optomism that the world won't be
	    ;; plunged into chaos and darkness on 1-Jan-2000, let's assume that
	    ;; two digit years <70 are in the 21st century and not the 20th.
	    (if (memq (aref (vm-year-of m) 0) '(?7 ?8 ?9)) "19" "20")
	    (vm-year-of m)))
	)
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
	((string-match "\\`[0-9]:" (vm-hour-of m))
	 (vm-set-hour-of m (concat "0" (vm-hour-of m)))))
  (cond ((null (vm-monthday-of m)) nil)
	((string-match "\\`0[0-9]\\'" (vm-monthday-of m))
	 (aset (vm-monthday-of m) 0 ? ))
	((string-match "\\`[0-9]\\'" (vm-monthday-of m))
	 (vm-set-monthday-of m (concat " " (vm-monthday-of m)))))
  )

(defun vm-su-do-month (m month-abbrev)
  (let ((val (assoc (downcase month-abbrev) vm-month-alist)))
    (if val
	(progn (vm-set-month-of m (nth 1 val))
	       (vm-set-month-number-of m (nth 2 val)))
      (vm-set-month-of m "")
      (vm-set-month-number-of m ""))))

(defun vm-run-user-summary-function (function message)
  (save-excursion
    (set-buffer (vm-buffer-of (vm-real-message-of message)))
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
      (set-buffer (vm-buffer-of message))
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
      (setq pair (funcall vm-chop-full-name-function from)
	    from (or (nth 1 pair) from)
	    full-name (or full-name (nth 0 pair) from)))
    (if (string-match "\\`\"\\([^\"]+\\)\"\\'" full-name)
 	(setq full-name
 	      (substring full-name (match-beginning 1) (match-end 1))))
    (vm-set-full-name-of m full-name)
    (vm-set-from-of m from)))

(defun vm-default-chop-full-name (address)
  (let ((from address)
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

;; test for existence and functionality of mail-extract-address-components
;; there are versions out there that don't work right, so we run
;; some test data through it to see if we can trust it.
(defun vm-choose-chop-full-name-function (address)
  (let ((test-data '(("kyle@uunet.uu.net" .
		      (nil "kyle@uunet.uu.net"))
		     ("c++std=lib@inet.research.att.com" .
		      (nil "c++std=lib@inet.research.att.com"))
		     ("\"Piet.Rypens\" <rypens@reks.uia.ac.be>" .
		      ("Piet Rypens" "rypens@reks.uia.ac.be"))
		     ("makke@wins.uia.ac.be (Marc.Gemis)" .
		      ("Marc Gemis" "makke@wins.uia.ac.be"))))
	(failed nil)
	result)
    (while test-data
      (setq result (condition-case c
		       (mail-extract-address-components (car (car test-data)))
		     (error c)))
      (if (not (equal result (cdr (car test-data))))
	  ;; failed test, use default
	  (setq failed (car test-data)
		test-data nil)
	(setq test-data (cdr test-data))))
    (if failed
	;; it failed, use default
	;;(setq vm-chop-full-name-function 'vm-default-chop-full-name)
	;; jwz wants to know this...
	(error "mail-extr.el failed tests: %s %s" failed result)
      ;; it passed the tests
      (setq vm-chop-full-name-function 'mail-extract-address-components))
    (funcall vm-chop-full-name-function address)))

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
    (vm-set-to-of m (mapconcat 'identity addresses ", "))
    (vm-set-to-names-of m (mapconcat 'identity names ", "))))

(defun vm-su-to (m)
  (or (vm-to-of m) (progn (vm-su-do-recipients m) (vm-to-of m))))

(defun vm-su-to-names (m)
  (or (vm-to-names-of m) (progn (vm-su-do-recipients m) (vm-to-names-of m))))
				  
(defun vm-su-message-id (m)
  (or (vm-message-id-of m)
      (vm-set-message-id-of
       m
       (or (vm-get-header-contents m "Message-Id:")
	   ;; try running md5 on the message body to produce an ID
	   ;; better than nothing.
	   (save-excursion
	     (set-buffer (vm-buffer-of (vm-real-message-of m)))
	     (save-restriction
	       (widen)
	       (condition-case nil
		   (concat "<fake-VM-id."
			   (vm-pop-md5-string
			    (buffer-substring
			     (vm-text-of (vm-real-message-of m))
			     (vm-text-end-of (vm-real-message-of m))))
			   "@talos.iv>")
		 (error nil))))
	   (concat "<" (int-to-string (vm-abs (random))) "@toto.iv>")))))

(defun vm-su-line-count (m)
  (or (vm-line-count-of m)
      (vm-set-line-count-of
       m
       (save-excursion
	 (set-buffer (vm-buffer-of (vm-real-message-of m)))
	 (save-restriction
	   (widen)
	   (int-to-string
	    (count-lines (vm-text-of (vm-real-message-of m))
			 (vm-text-end-of (vm-real-message-of m)))))))))

(defun vm-su-subject (m)
  (or (vm-subject-of m)
      (vm-set-subject-of
       m
       (let ((subject (or (vm-get-header-contents m "Subject:") ""))
	     (i nil))
	 (if vm-summary-subject-no-newlines
	     (while (setq i (string-match "\n" subject i))
	       (aset subject i ?\ )))
	 subject ))))

(defun vm-su-summary (m)
  (if (and (vm-virtual-message-p m) (not (vm-virtual-messages-of m)))
      (or (vm-virtual-summary-of m)
	  (save-excursion
	    (vm-select-folder-buffer)
	    (vm-set-virtual-summary-of m (vm-sprintf 'vm-summary-format m t))
	    (vm-virtual-summary-of m)))
    (or (vm-summary-of m)
	(save-excursion
	  (vm-select-folder-buffer)
	  (vm-set-summary-of m (vm-sprintf 'vm-summary-format m t))
	  (vm-summary-of m)))))

(defun vm-fix-my-summary!!! ()
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (message "Fixing your summary...")
  (let ((mp vm-message-list))
    (while mp
      (vm-set-summary-of (car mp) nil)
      (vm-mark-for-summary-update (car mp))
      (vm-stuff-attributes (car mp))
      (setq mp (cdr mp)))
    (set-buffer-modified-p t)
    (vm-update-summary-and-mode-line))
  (message "Fixing your summary... done"))

(defun vm-su-thread-indent (m)
  (if (natnump vm-summary-thread-indent-level)
      (make-string (* (vm-thread-indention m) vm-summary-thread-indent-level)
		   ?\ )
    "" ))

(defun vm-su-labels (m)
  (or (vm-label-string-of m)
      (vm-set-label-string-of
       m
       (mapconcat 'identity (vm-labels-of m) ","))
      (vm-label-string-of m)))
