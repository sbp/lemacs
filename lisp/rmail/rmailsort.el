;;; Rmail: sort messages
;; Copyright (C) 1990, 1992 Masanobu UMEDA (umerin@mse.kyutech.ac.jp)
;; Header: rmailsort.el,v 1.4 93/01/26 12:11:29 umerin Locked 

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; I would like to thank bob_weiner@pts.mot.com and
;; bruno@yakima.inria.fr for their improvements.

(provide 'rmailsort)
(require 'rmail)
(require 'sort)

(autoload 'timezone-make-date-sortable "timezone")

;; GNUS compatible key bindings.

(define-key rmail-mode-map "\C-c\C-s\C-d" 'rmail-sort-by-date)
(define-key rmail-mode-map "\C-c\C-s\C-s" 'rmail-sort-by-subject)
(define-key rmail-mode-map "\C-c\C-s\C-a" 'rmail-sort-by-author)
(define-key rmail-mode-map "\C-c\C-s\C-r" 'rmail-sort-by-recipient)
(define-key rmail-mode-map "\C-c\C-s\C-l" 'rmail-sort-by-lines)

;; Key binding may not be installed unless Rmail Summary mode is loaded.
(if (boundp 'rmail-summary-mode-map)
    (progn
      (define-key rmail-summary-mode-map
	"\C-c\C-s\C-d" 'rmail-summary-sort-by-date)
      (define-key rmail-summary-mode-map
	"\C-c\C-s\C-s" 'rmail-summary-sort-by-subject)
      (define-key rmail-summary-mode-map
	"\C-c\C-s\C-a" 'rmail-summary-sort-by-author)
      (define-key rmail-summary-mode-map
	"\C-c\C-s\C-r" 'rmail-summary-sort-by-recipient)
      (define-key rmail-summary-mode-map
	"\C-c\C-s\C-l" 'rmail-summary-sort-by-lines)
      ))


;; Sorting messages in Rmail buffer

(defun rmail-sort-by-date (reverse)
  "Sort messages of current Rmail file by date.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (rmail-make-date-sortable
			   (rmail-fetch-field msg "Date"))))))

(defun rmail-sort-by-subject (reverse)
  "Sort messages of current Rmail file by subject.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (let ((key (or (rmail-fetch-field msg "Subject") ""))
				(case-fold-search t))
			    ;; Remove `Re:'
			    (if (string-match "^\\(re:[ \t]+\\)*" key)
				(substring key (match-end 0)) key))))))

(defun rmail-sort-by-author (reverse)
  "Sort messages of current Rmail file by author.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (downcase	;Canonical name
			   (mail-strip-quoted-names
			    (or (rmail-fetch-field msg "From")
				(rmail-fetch-field msg "Sender") "")))))))

(defun rmail-sort-by-recipient (reverse)
  "Sort messages of current Rmail file by recipient.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (downcase	;Canonical name
			   (mail-strip-quoted-names
			    (or (rmail-fetch-field msg "To")
				(rmail-fetch-field msg "Apparently-To") "")
			    ))))))

(defun rmail-sort-by-lines (reverse)
  "Sort messages of current Rmail file by lines of the message.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  ;; Basic ideas by pinard@IRO.UMontreal.CA
  (rmail-sort-messages reverse
		       (function
			(lambda (msg)
			  (count-lines (rmail-msgbeg msgnum)
				       (rmail-msgend msgnum))))))

;; Sorting messages in Rmail Summary buffer.

(defun rmail-summary-sort-by-date (reverse)
  "Sort messages of current Rmail summary by date.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-date) reverse))

(defun rmail-summary-sort-by-subject (reverse)
  "Sort messages of current Rmail summary by subject.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-subject) reverse))

(defun rmail-summary-sort-by-author (reverse)
  "Sort messages of current Rmail summary by author.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-author) reverse))

(defun rmail-summary-sort-by-recipient (reverse)
  "Sort messages of current Rmail summary by recipient.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-recipient) reverse))

(defun rmail-summary-sort-by-lines (reverse)
  "Sort messages of current Rmail summary by lines of the message.
If prefix argument REVERSE is non-nil, sort them in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-lines) reverse))


;; Basic functions

(defun rmail-sort-messages (reverse keyfun)
  "Sort messages of current Rmail file.
If 1st argument REVERSE is non-nil, sort them in reverse order.
2nd argument KEYFUN is called with a message number, and should return a key."
  (let ((buffer-read-only nil)
	(predicate nil)			;< or string-lessp
	(sort-lists nil))
    (message "Finding sort keys...")
    (widen)
    (let ((msgnum 1))
      (while (>= rmail-total-messages msgnum)
	(setq sort-lists
	      (cons (list (funcall keyfun msgnum) ;Make sorting key
			  (eq rmail-current-message msgnum) ;True if current
			  (aref rmail-message-vector msgnum)
			  (aref rmail-message-vector (1+ msgnum)))
		    sort-lists))
	(if (zerop (% msgnum 10))
	    (message "Finding sort keys...%d" msgnum))
	(setq msgnum (1+ msgnum))))
    (or reverse (setq sort-lists (nreverse sort-lists)))
    ;; Decide predicate: < or string-lessp
    (if (numberp (car (car sort-lists))) ;Is a key numeric?
	(setq predicate (function <))
      (setq predicate (function string-lessp)))
    (setq sort-lists
	  (sort sort-lists
		(function
		 (lambda (a b)
		   (funcall predicate (car a) (car b))))))
    (if reverse (setq sort-lists (nreverse sort-lists)))
    ;; Now we enter critical region.  So, keyboard quit is disabled.
    (message "Reordering messages...")
    (let ((inhibit-quit t)		;Inhibit quit
	  (current-message nil)
	  (msgnum 1)
	  (msginfo nil))
      ;; There's little hope that we can easily undo after that.
      (buffer-flush-undo (current-buffer))
      (goto-char (rmail-msgbeg 1))
      ;; To force update of all markers.
      (insert-before-markers ?Z)
      (backward-char 1)
      ;; Now reorder messages.
      (while sort-lists
	(setq msginfo (car sort-lists))
	;; Swap two messages.
	(insert-buffer-substring
	 (current-buffer) (nth 2 msginfo) (nth 3 msginfo))
	(delete-region  (nth 2 msginfo) (nth 3 msginfo))
	;; Is current message?
	(if (nth 1 msginfo)
	    (setq current-message msgnum))
	(setq sort-lists (cdr sort-lists))
	(if (zerop (% msgnum 10))
	    (message "Reordering messages...%d" msgnum))
	(setq msgnum (1+ msgnum)))
      ;; Delete the garbage inserted before.
      (delete-char 1)
      (setq quit-flag nil)
      (buffer-enable-undo)
      (rmail-set-message-counters)
      (rmail-show-message current-message))
    ))

(defun rmail-sort-from-summary (sortfun reverse)
  "Sort Rmail messages from Summary buffer and update it after sorting."
  (pop-to-buffer rmail-buffer)
  (funcall sortfun reverse)
  (rmail-summary))

(defun rmail-fetch-field (msg field)
  "Return the value of the header FIELD of MSG.
Arguments are MSG and FIELD."
  (save-restriction
    (widen)
    (let ((next (rmail-msgend msg)))
      (goto-char (rmail-msgbeg msg))
      (narrow-to-region (if (search-forward "\n*** EOOH ***\n" next t)
			    (point)
			  (forward-line 1)
			  (point))
			(progn (search-forward "\n\n" nil t) (point)))
      (mail-fetch-field field))))

(defun rmail-make-date-sortable (date)
  "Make DATE sortable using the function string-lessp."
  ;; Assume the default time zone is GMT.
  (timezone-make-date-sortable date "GMT" "GMT"))

;; Copy of the function gnus-comparable-date in gnus.el version 3.13
;
;(defun rmail-make-date-sortable (date)
;  "Make sortable string by string-lessp from DATE."
;  (let ((month '(("JAN" . " 1")("FEB" . " 2")("MAR" . " 3")
;		 ("APR" . " 4")("MAY" . " 5")("JUN" . " 6")
;		 ("JUL" . " 7")("AUG" . " 8")("SEP" . " 9")
;		 ("OCT" . "10")("NOV" . "11")("DEC" . "12")))
;	(date (or date "")))
;    ;; Can understand the following styles:
;    ;; (1) 14 Apr 89 03:20:12 GMT
;    ;; (2) Fri, 17 Mar 89 4:01:33 GMT
;    (if (string-match
;	 "\\([0-9]+\\) \\([^ ,]+\\) \\([0-9]+\\) \\([0-9:]+\\)" date)
;	(concat
;	 ;; Year
;	 (substring date (match-beginning 3) (match-end 3))
;	 ;; Month
;	 (cdr
;	  (assoc
;	   (upcase (substring date (match-beginning 2) (match-end 2))) month))
;	 ;; Day
;	 (format "%2d" (string-to-int
;			(substring date
;				   (match-beginning 1) (match-end 1))))
;	 ;; Time
;	 (substring date (match-beginning 4) (match-end 4)))
;      ;; Cannot understand DATE string.
;      date
;      )
;    ))
