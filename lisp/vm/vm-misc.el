;;; Miscellaneous functions for VM
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

(defun vm-minibuffer-complete-word ()
  "Interpret the word under or before the cursor as a filename and complete
it as far as possible."
  (interactive)
  (let ((opoint (point))
	c-list beg end diff word word-prefix-regexp completion)
    ;; find the beginning and end of the word we're trying to complete
    (if (or (eobp) (memq (following-char) '(?\t ?\n ?\ )))
	(progn
	  (skip-chars-backward " \t\n")   
	  (and (not (eobp)) (forward-char))
	  (setq end (point)))
      (skip-chars-forward "^ \t\n")
      (setq end (point)))
    (skip-chars-backward "^ \t\n")
    (setq beg (point))
    (goto-char opoint)
    ;; copy the word into a string
    (setq word (buffer-substring beg end))
    ;; trim the completion list down to just likely candidates
    ;; then convert it to an alist.
    (setq word-prefix-regexp (concat "^" (regexp-quote word))
	  c-list (vm-delete-non-matching-strings
		  word-prefix-regexp
		  vm-minibuffer-completion-table)
	  c-list (mapcar 'list c-list))
    ;; Try the word against the completion list.
    (and c-list (setq completion (try-completion word c-list)))
    ;; If completion is nil, figure out what prefix of the word would prefix
    ;; something in the completion list... but only if the user is interested.
    (if (and (null completion) vm-completion-auto-correct c-list)
	(let ((i -1))
	  (while (null (setq completion
			     (try-completion (substring word 0 i) c-list)))
	    (vm-decrement i))
	  (setq completion (substring word 0 i))))
    ;; If completion is t, we had a perfect match already.
    (if (eq completion t)
	(cond ((cdr c-list)
	       (vm-minibuffer-completion-message "[Complete, but not unique]"))
	      (vm-completion-auto-space
	       (goto-char end)
	       (insert " "))
	      (t
	       (vm-minibuffer-completion-message "[Sole completion]")))
      ;; Compute the difference in length between the completion and the
      ;; word.  A negative difference means no match and the magnitude
      ;; indicates the number of chars that need to be shaved off the end
      ;; before a match will occur.  A positive difference means a match
      ;; occurred and the magnitude specifies the number of new chars that
      ;; can be appended to the word as a completion.
      ;;
      ;; `completion' can be nil here, but the code works anyway because
      ;; (length nil) still equals 0!
      (setq diff (- (length completion) (length word)))
      (cond
       ;; We have some completion chars.  Insert them.
       ((> diff 0)
	(goto-char end)
	(insert (substring completion (- diff)))
	(if (and vm-completion-auto-space
		 (null (cdr c-list)))
	    (insert " ")))
       ;; The word prefixed more than one string, but we can't complete
       ;; any further.  Either give help or say "Ambiguous".
       ((zerop diff)
	(if (assoc word c-list)
	    (vm-minibuffer-completion-message "[Complete, but not unique]")
	  (if (null completion-auto-help)
	      (vm-minibuffer-completion-message "[Ambiguous]")
	    (vm-minibuffer-show-completions (sort (mapcar 'car c-list)
						  'string-lessp)))))
       ;; The word didn't prefix anything... if vm-completion-auto-correct is
       ;; non-nil strip the offending characters and try again.
       (vm-completion-auto-correct
	(goto-char end)
	(delete-char diff)
	(vm-minibuffer-complete-word))
       ;; completion utterly failed, tell the user so.
       (t
	(vm-minibuffer-completion-message "[No match]"))))))

(defun vm-minibuffer-completion-message (string &optional seconds)
  "Briefly display STRING to the right of the current minibuffer input.
Optional second arg SECONDS specifies how long to keep the message visible;
the default is 2 seconds.

A keypress causes the immediate erasure of the STRING, and return of control
to the calling program."
  (let (omax (inhibit-quit t))
    (save-excursion
      (goto-char (point-max))
      (setq omax (point))
      (insert " " string))
    (sit-for (or seconds 2))
    (delete-region omax (point-max))))

(defun vm-minibuffer-show-completions (list)
  "Display LIST in a multi-column listing in the \" *Completions*\" buffer.
LIST should be a list of strings."
  (save-excursion
    (let (longest rows columns list-length q i)
      (set-buffer (get-buffer-create " *Completions*"))
      (erase-buffer)
      (insert "Possible completions are:\n")
      (setq q list
	    list-length 0
	    longest 0)
      (while q
	(setq longest (max longest (length (car q)))
	      list-length (1+ list-length)
	      q (cdr q)))
      ;; provide for separation between columns
      (setq longest (+ 3 longest))
      (setq columns (/ (- (screen-width) 2) longest)
	    rows (/ list-length columns)
	    rows
	    (+ (if (zerop (% list-length columns)) 0 1)
	       rows))
      (setq i columns
	    tab-stop-list nil)
      (while (not (zerop i))
	(setq tab-stop-list (cons (* longest i) tab-stop-list)
	      i (1- i)))
      (setq q list
	    i 0)
      (while q
	(insert (car q))
	(setq i (1+ i)
	      q (cdr q))
	(if (zerop (% i columns))
	    (insert "\n")
	  (tab-to-tab-stop)))
      (goto-char (point-min))
      (vm-display-buffer " *Completions*"))))

(defun vm-delete-non-matching-strings (regexp list &optional destructively)
  "Delete strings matching REGEXP from LIST.
Optional third arg non-nil means to destructively alter LIST, instead of
working on a copy.

The new version of the list, minus the deleted strings, is returned."
  (or destructively (setq list (copy-sequence list)))
  (let ((curr list) (prev nil))
    (while curr
      (if (string-match regexp (car curr))
	  (setq prev curr
		curr (cdr curr))
	(if (null prev)
	    (setq list (cdr list)
		  curr list)
	  (setcdr prev (cdr curr))
	  (setq curr (cdr curr)))))
    list ))

(defun vm-read-string (prompt completion-list &optional multi-word)
  (let ((minibuffer-local-map (copy-keymap minibuffer-local-map))
	(vm-completion-auto-space multi-word)
	(vm-minibuffer-completion-table completion-list))
    (define-key minibuffer-local-map "\t" 'vm-minibuffer-complete-word)
    (read-string prompt)))

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
	     (setq work-buffer (generate-new-buffer "*vm-work*"))
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

(defun vm-write-string (where string)
  (if (bufferp where)
      (vm-save-buffer-excursion
	(set-buffer where)
	(goto-char (point-max))
	(insert string))
    (let ((temp-buffer nil))
      (unwind-protect
	  (save-excursion
	    (setq temp-buffer (generate-new-buffer "*vm-work*"))
	    (set-buffer temp-buffer)
	    (insert string)
	    (write-region (point-min) (point-max) where t 'quiet))
	(and temp-buffer (kill-buffer temp-buffer))))))

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

(defmacro vm-error-if-virtual-folder ()
  '(and (eq major-mode 'vm-virtual-mode)
	(error "%s cannot be applied to virtual folders." this-command)))

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

;; rewritten by jwz
(defun vm-delete-duplicates (list &optional all hack-addresses)
  "Delete duplicate equivalent strings from the list.
If ALL is t, then if there is more than one occurence of a string in the list,
 then all occurences of it are removed instead of just the subseqent ones.
If HACK-ADDRESSES is t, then the strings are considered to be mail addresses,
 and only the address part is compared (so that \"Name <foo>\" and \"foo\"
 would be considered to be equivalent.)"
  (setq list (copy-sequence list))
  (let ((test (if hack-addresses
		  (function
		   (lambda (x y)
		     (or (equal x y)
			 (equal (nth 1 (funcall vm-chop-full-name-hook x))
				(nth 1 (funcall vm-chop-full-name-hook y))))))
		(function equal)))
	(rest list))
    (while rest
      (let ((this (car rest))
	    (rest2 (cdr rest)))
	(if this
	    (while rest2
	      (cond ((null (car rest2)))
		    ((funcall test this (car rest2))
		     (setcar rest2 nil)
		     (if all (setcar rest nil))))
	      (setq rest2 (cdr rest2)))))
      (setq rest (cdr rest)))
    (delq nil list)))

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
  (let ((case-fold-search nil))
    (string-match "Lucid" emacs-version)))

(defun vm-fsf-emacs-19-p ()
  (and (string-match "^19" emacs-version) (not (vm-lucid-emacs-p))))

(defun vm-run-message-hook (message &optional hook-variable)
  (save-excursion
    (set-buffer (marker-buffer (vm-start-of message)))
    (save-restriction
      (widen)
      (save-excursion
	(narrow-to-region (vm-headers-of message) (vm-text-end-of message))
	(run-hooks hook-variable)))))

(defun vm-submit-bug-report ()
  "Submit a bug report, with pertinent information to the VM bug list."
  (interactive)
  (require 'reporter)
  ;; make sure the user doesn't try to use VM here.
  (let ((reporter-mailer '(mail)))
    (reporter-submit-bug-report
     vm-maintainer-address
     (concat "VM " vm-version)
     (list
      'vm-arrived-message-hook
      'vm-auto-center-summary
;; don't send this by default, might be personal stuff in here.
;;      'vm-auto-folder-alist
      'vm-auto-folder-case-fold-search
      'vm-auto-get-new-mail
      'vm-auto-next-message
      'vm-berkeley-mail-compatibility
      'vm-check-folder-types
      'vm-circular-folders
      'vm-confirm-new-folders
      'vm-confirm-quit
      'vm-convert-folder-types
      'vm-crash-box
      'vm-default-folder-type
      'vm-delete-after-archiving
      'vm-delete-after-bursting
      'vm-delete-after-saving
      'vm-delete-empty-folders
      'vm-digest-burst-type
      'vm-digest-center-preamble
      'vm-digest-preamble-format
      'vm-digest-send-type
      'vm-edit-message-hook
      'vm-edit-message-mode
      'vm-flush-interval
      'vm-folder-directory
      'vm-folder-read-only
      'vm-follow-summary-cursor
      'vm-forward-message-hook
      'vm-forwarded-headers
      'vm-forwarding-digest-type
      'vm-forwarding-subject-format
      'vm-gargle-uucp
      'vm-highlighted-header-regexp
      'vm-honor-page-delimiters
      'vm-in-reply-to-format
      'vm-included-text-attribution-format
      'vm-included-text-prefix
      'vm-inhibit-startup-message
      'vm-init-file
      'vm-invisible-header-regexp
      'vm-jump-to-new-messages
      'vm-jump-to-unread-messages
      'vm-keep-sent-messages
      'vm-mail-header-from
      'vm-mail-hook
      'vm-mail-mode-hook
      'vm-mail-window-percentage
      'vm-mode-hook
      'vm-mode-hooks
      'vm-move-after-deleting
      'vm-move-after-undeleting
      'vm-movemail-program
      'vm-mutable-windows
      'vm-pop-md5-program
      'vm-preview-lines
      'vm-preview-read-messages
      'vm-primary-inbox
      'vm-recognize-pop-maildrops
      'vm-reply-hook
      'vm-reply-ignored-addresses
      'vm-reply-subject-prefix
      'vm-resend-bounced-discard-header-regexp
      'vm-resend-bounced-headers
      'vm-resend-bounced-message-hook
      'vm-resend-discard-header-regexp
      'vm-resend-headers
      'vm-resend-message-hook
      'vm-retain-message-order
      'vm-retrieved-spooled-mail-hook
      'vm-rfc1153-digest-discard-header-regexp
      'vm-rfc1153-digest-headers
      'vm-rfc934-digest-discard-header-regexp
      'vm-rfc934-digest-headers
      'vm-search-using-regexps
      'vm-select-message-hook
      'vm-select-new-message-hook
      'vm-select-unread-message-hook
      'vm-send-digest-hook
      'vm-skip-deleted-messages
      'vm-skip-read-messages
;; don't send vm-spool-files by default, might contain passwords
;;      'vm-spool-files
      'vm-startup-with-summary
      'vm-strip-reply-headers
      'vm-summary-format
      'vm-summary-mode-hook
      'vm-summary-mode-hooks
      'vm-summary-redo-hook
      'vm-summary-uninteresting-senders
      'vm-summary-uninteresting-senders-arrow
      'vm-unforwarded-header-regexp
      'vm-virtual-folder-alist
      'vm-virtual-mirror
      'vm-visible-headers
      'vm-visit-folder-hook
      'vm-visit-when-saving
      'vm-window-configuration-file
;; not a user variable technically, but useful to know
      'vm-window-configurations
;; non-VM variables, but related to the display, useful to know
      'pop-up-windows
      (if (vm-fsf-emacs-19-p) 'pop-up-frames)
      'next-screen-context-lines
;; see what the user had loaded
      'features
      ))))

