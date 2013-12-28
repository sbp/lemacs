;;; -*- Mode:Emacs-Lisp -*-

;;; Sorting VM messages; based on rmailsort.el (c) 1990 by Masanobu Umeda.
;;; This requires VM version 5.31 or newer.
;;; To sort by date, you need Umeda's "timezone.el" package as well.
;;;
;;; created 6 nov 90 by Jamie Zawinski <jwz@lucid.com>;
;;; modified 4 dec 90 by jwz.
;;; modified 2 may 91 by mleisher@nmsu.edu (Mark Leisher)
;;; modified 18 may 91 by jwz.

;;(require 'vm)	; can't do this is building it into vm.elc
(require 'sort)
(require 'mail-utils)
(autoload 'timezone-make-date-sortable "timezone")

;; GNUS compatible key bindings.
(defvar vm-sort-map nil)
(if vm-sort-map nil
  (setq vm-sort-map (make-sparse-keymap))
  (define-key vm-mode-map "\C-c\C-s" vm-sort-map)
  (define-key vm-sort-map "\C-d" 'vm-sort-by-date)
  (define-key vm-sort-map "\C-s" 'vm-sort-by-subject)
  (define-key vm-sort-map "\C-a" 'vm-sort-by-author)
  (define-key vm-sort-map "\C-r" 'vm-sort-by-recipient)
  (define-key vm-sort-map "\C-l" 'vm-sort-by-lines)
  (define-key vm-sort-map "\C-h" 'vm-sort-help)
  (define-key vm-sort-map "?"    'vm-sort-help)
  )

(defun vm-sort-help ()
  (interactive)
  (message (substitute-command-keys (format "\\<vm-sort-map>\
 date: `\\[vm-sort-by-date]'; subject: `\\[vm-sort-by-subject]';\
 author: `\\[%s]'; recipient: `\\[vm-sort-by-recipient]';\
 lines: `\\[vm-sort-by-lines]'."
   (if (where-is-internal 'vm-sort-by-author-dwim vm-sort-map t)
       'vm-sort-by-author-dwim
       'vm-sort-by-author)))))

(defun vm-sort-by-date (reverse)
  "Sort messages of current VM file by date.
If prefix argument REVERSE is non-nil, sort them in reverse order.
This actually changes the order of messages in the folder.
You may need to set the variable `vm-default-timezone' before running this."
  (interactive "P")
  (vm-sort-messages reverse 'vm-sortable-date-string))

(defun vm-sort-by-subject (reverse)
  "Sort messages of current VM file by subject.
If prefix argument REVERSE is non-nil, sort them in reverse order.
This actually changes the order of messages in the folder."
  (interactive "P")
  (vm-sort-messages reverse
		    (function
		      (lambda (msg)
		      (let ((key (or (vm-su-subject msg) ""))
			    (case-fold-search t))
			(downcase
			;; Remove `Re:'
			(if (string-match "^\\([ \t]*Re:[ \t]+\\)*" key)
			    (substring key (match-end 0)) key)))))))

(defun vm-sort-by-author (reverse)
  "Sort messages of current VM file by author.
If prefix argument REVERSE is non-nil, sort them in reverse order.
This actually changes the order of messages in the folder."
  (interactive "P")
  (vm-sort-messages reverse
		    (function
		      (lambda (msg)
		       (downcase
		        (mail-strip-quoted-names (or (vm-su-from msg) "")))))))

(defun vm-sort-by-recipient (reverse)
  "Sort messages of current VM file by recipient.
If prefix argument REVERSE is non-nil, sort them in reverse order.
This actually changes the order of messages in the folder."
  (interactive "P")
  (vm-sort-messages reverse
		    (function
		      (lambda (msg)
		       (downcase
		        (mail-strip-quoted-names (or (vm-su-to msg) "")))))))

;; This depends on the variable `vm-uninteresting-senders' which is defined
;; in jwz-vm-summary.el.
(defun vm-sort-by-author-dwim (reverse)
  "Sort messages of current VM file by author, or by recipient if the author
is you.  The variable `vm-uninteresting-senders' controls which addresses are
considered to be yours.  If prefix argument REVERSE is non-nil, sort them in 
reverse order.  This actually changes the order of messages in the folder."
  (interactive "P")
  (vm-sort-messages reverse
		    (function
		      (lambda (msg)
		       (downcase
		        (mail-strip-quoted-names
			  (let ((from (vm-su-from msg)))
			    (or (if (string-match vm-uninteresting-senders from)
				    (vm-su-to msg)
				    from)
				""))))))))


(defun vm-sort-by-lines (reverse)
  "Sort messages of current VM file by number-of-lines.
If prefix argument REVERSE is non-nil, sort them in reverse order.
This actually changes the order of messages in the folder."
  (interactive "P")
  (vm-sort-messages reverse
		    (function
		      (lambda (msg)
			(let* ((ml (vm-su-line-count msg))
			       (sl (length ml)))
			  (if (< sl 8)
			      (concat (make-string (- 8 sl) ? ) ml)
			    ml))))))


(defun vm-sort-messages (reverse keyfunc)
  "Sort messages of current VM folder.
1st argument REVERSE is non-nil, sort them in reverse order.
2nd argument KEYFUNC is called with the message, and should return a key."
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((buffer-read-only nil)
	(sort-lists nil)
	(buf-header-end (vm-start-of (car vm-message-list))))
    (message "Finding sort keys...")
    (widen)
    (let ((rest vm-message-list)
	  msg off)
      (while rest
	(setq msg (car rest))
	;; make the text-of be an int offset from start-of-message (temp).
	(setq off (- (vm-text-of msg) (vm-start-of msg)))
	;;## (if (< (vm-text-of msg) 0) (error "badly out of synch...")) ;debug
	(setq sort-lists
	      (cons (list (funcall keyfunc msg) ;A sort key.
			  msg
			  (prog1
			      (buffer-substring
			       (vm-start-of msg) (vm-end-of msg))
			    ;; Is this safe?  I think so.
			    (delete-region (vm-start-of msg) (vm-end-of msg))))
		    sort-lists))
	(vm-set-text-of msg off)
	(setq rest (cdr rest))))
    (or reverse (setq sort-lists (nreverse sort-lists)))
    (message "Sorting...")
    (setq sort-lists
	  (sort sort-lists
		(function
		 (lambda (a b)
		   (string-lessp (car a) (car b))))))
    (if reverse (setq sort-lists (nreverse sort-lists)))
    (message "Reordering buffer...")
    ;;(delete-region buf-header-end (point-max)) ;necessary?
    (let ((new-msg-list '()))
      (while sort-lists
	(let ((body (nth 2 (car sort-lists)))
	      (msg (nth 1 (car sort-lists))))
	  (set-marker (vm-start-of msg) (point))
	  (insert body)
	  (set-marker (vm-end-of msg) (point))
	  (vm-set-text-of msg
	    (set-marker (make-marker) (+ (vm-start-of msg) (vm-text-of msg))))
	  ;; ## debug
	  ;;(if (< (vm-end-of msg) (vm-text-of msg)) (error "end less than text"))
	  ;;(if (< (vm-end-of msg) (vm-start-of msg)) (error "end less than start"))
	  ;;(if (< (vm-text-of msg) (vm-start-of msg)) (error "text less than start"))
	  (vm-set-reverse-link-of msg new-msg-list)
	  (setq new-msg-list (cons msg new-msg-list)))
	(setq sort-lists (cdr sort-lists)))
      (let ((curr (car vm-message-pointer))
	    (last (car vm-message-pointer)))
	(setq vm-last-message-pointer new-msg-list ; cute, eh?
	      vm-message-list (nreverse new-msg-list)
	      vm-message-pointer new-msg-list)
	(if curr (setq vm-message-pointer (memq curr vm-message-list)))
	(if last (setq vm-last-message-pointer (memq last vm-message-list)))))
    (setq vm-current-grouping nil)
    (vm-build-message-list t)
    (vm-number-messages)
    (setq vm-need-summary-pointer-update t)
    (cond (vm-summary-buffer
	   (vm-do-summary)
	   (vm-update-summary-and-mode-line)))
    (vm-preview-current-message)
    ))


;;; Time stuff.  What a pain in the butt.

(defvar vm-default-timezone nil
  "*The string-name of the local timezone, \"PDT\" for example.
If this is nil, we will -try- to compute it when necessary.")

(defun vm-compute-local-timezone-via-date ()
  (require 'timezone)
  (save-excursion
    (set-buffer (get-buffer-create " *zone-hackery*"))
    (let (h1 d1 h2 d2)
      (erase-buffer)
      ;;(shell-command-on-region 1 1 "date '+%H %w'" t)
      (call-process "date" nil (current-buffer) nil "+%H %w")
      ;;
      ;; If you have SunOS 2.0, which has a broken "date -u", you might need
      ;; to use this instead.  From Tony.Bennett@east.Sun.COM  6/92.
      ;;  (call-process "/bin/sh" nil (current-buffer) nil "-c"
      ;;                "TZ=GMT /bin/date '+%H %w'")
      ;;
      (goto-char 1)
      (and (looking-at "\\([0-9][0-9]?\\) \\([0-9][0-9]?\\)")
	   (setq h1 (string-to-int (buffer-substring
				    (match-beginning 1) (match-end 1))))
	   (setq d1 (string-to-int (buffer-substring
				    (match-beginning 2) (match-end 2)))))
      (erase-buffer)
      ;;(shell-command-on-region 1 1 "date -u '+%H %w'" t)
      (call-process "date" nil (current-buffer) nil "-u" "+%H %w")
      (goto-char 1)
      (and (looking-at "\\([0-9][0-9]?\\) \\([0-9][0-9]?\\)")
	   (setq h2 (string-to-int (buffer-substring
				    (match-beginning 1) (match-end 1))))
	   (setq d2 (string-to-int (buffer-substring
				    (match-beginning 2) (match-end 2)))))
      (kill-buffer " *zone-hackery*")
      (let ((offset (% (- h2 h1) 24)))
	(if (< offset 0)
	    (setq offset (+ 24 offset)))
	;; daylight savings time might make this be goofy:
	;; MST instead of PDT, for example.  This still works.
	(car (rassq (* -100 offset) timezone-world-timezones))))))

(defun vm-compute-local-timezone ()
  "Try to figure out what timezone we're in."
  (or (condition-case ()
	  ;; even if current-time-zone is defined, it may error...
	  (let ((zoneinfo (current-time-zone)))
	    (or (if (nth 1 zoneinfo) (nth 3 zoneinfo))
		(nth 2 zoneinfo)))
	(error nil))
      (vm-compute-local-timezone-via-date)))


(defun vm-sortable-date-string (msg)
  "Make a string sortable by string-lessp of the date of the message."
  (let ((year (vm-su-year msg))
	(month (or (vm-su-month msg) "---"))
	(day (or (vm-su-monthday msg) " 0"))
	(time (or (vm-su-hour msg) "00:00:00"))
	(zone (or (vm-su-zone msg) "")))
    (if (zerop (length year)) (setq year "0000"))
    (if (= 2 (length year)) (setq year (concat "19" year)))
    (if (> (length month) 3) (setq month (substring month 0 3)))
    (if (and (= 2 (length day)) (= ?0 (aref day 0)))
	(aset day 0 32))
    (if (= 1 (length day))
	(setq day (concat " " day))
      (if (and (= 2 (length day)) (= ?0 (aref day 0)))
	  (setq day (concat " " (substring day 1)))))
    (or vm-default-timezone
	(setq vm-default-timezone (vm-compute-local-timezone)))
    (if (equal "" zone) (setq zone vm-default-timezone))
    (timezone-make-date-sortable
     (concat day " " month " " year " " time " " zone)
     "GMT" "GMT")))

;; CORRECT:
;; (timezone-make-date-sortable " 3 May 1991 17:34:45 EDT" "GMT" "GMT")  "91050321:34:45"
;; (timezone-make-date-sortable " 3 May 1991 14:38:58 PDT" "GMT" "GMT")  "91050321:38:58"

;; INCORRECT:
;; (timezone-make-date-sortable " 3 May 1991 17:34:45 EDT" "GMT" "GMT")  "91050313:34:45"
;; (timezone-make-date-sortable " 3 May 1991 14:38:58 PDT" "GMT" "GMT")  "91050307:38:58"

(provide 'vm-sort)
