;;; Mailing, forwarding, and replying commands for VM
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

(defun vm-do-reply (to-all include-text count)
  (save-restriction
    (widen)
    (let ((mail-buffer (current-buffer))
	  (mlist (vm-select-marked-or-prefixed-messages count))
	  (dir default-directory)
	  (message-pointer vm-message-pointer)
	  to cc subject mp in-reply-to references tmp tmp2 newsgroups)
      (setq mp mlist)
      (while mp 
	(cond
	 ((eq mlist mp)
	  (cond ((setq to (vm-get-header-contents (car mp) "Reply-To")))
		((setq to (vm-get-header-contents (car mp) "From")))
		((setq to (vm-grok-From_-author (car mp))))
		(t (error "No From: or Reply-To: header in message")))
	  (setq subject (vm-get-header-contents (car mp) "Subject")
		in-reply-to (and vm-in-reply-to-format
				 (vm-sprintf 'vm-in-reply-to-format (car mp)))
		in-reply-to (and (not (equal "" in-reply-to)) in-reply-to))
	  (and subject vm-reply-subject-prefix
	       (let ((case-fold-search t))
		 (not
		  (equal
		   (string-match (regexp-quote vm-reply-subject-prefix)
				 subject)
		   0)))
	       (setq subject (concat vm-reply-subject-prefix subject))))
	 (t (cond ((setq tmp (vm-get-header-contents (car mp) "Reply-To"))
		   (setq to (concat to "," tmp)))
		  ((setq tmp (vm-get-header-contents (car mp) "From"))
		   (setq to (concat to "," tmp)))
		  ((setq tmp (vm-grok-From_-author (car mp)))
		   (setq to (concat to "," tmp)))
		  (t (error "No From: or Reply-To: header in message")))))
	(if to-all
	    (progn
	      (setq tmp (vm-get-header-contents (car mp) "To"))
	      (setq tmp2 (vm-get-header-contents (car mp) "Cc"))
	      (if tmp
		  (if cc
		      (setq cc (concat cc "," tmp))
		    (setq cc tmp)))
	      (if tmp2
		  (if cc
		      (setq cc (concat cc "," tmp2))
		    (setq cc tmp2)))))
	(setq references
	      (cons (vm-get-header-contents (car mp) "References")
		    (cons (vm-get-header-contents (car mp) "In-reply-to")
			  (cons (vm-get-header-contents (car mp) "Message-ID")
				references))))
	(setq newsgroups
	      (cons (or (and to-all (vm-get-header-contents (car mp) "Followup-To"))
			(vm-get-header-contents (car mp) "Newsgroups"))
		    newsgroups))
	(setq mp (cdr mp)))
      (if vm-strip-reply-headers
	  (let ((mail-use-rfc822 t))
	    (and to (setq to (mail-strip-quoted-names to)))
	    (and cc (setq cc (mail-strip-quoted-names cc)))))
      (setq to (vm-parse-addresses to)
	    cc (vm-parse-addresses cc))
      (if vm-reply-ignored-addresses
	  (setq to (vm-strip-ignored-addresses to)
		cc (vm-strip-ignored-addresses cc)))
      (setq to (vm-delete-duplicates to))
      (setq cc (vm-delete-duplicates
		(append (vm-delete-duplicates cc)
			to (copy-sequence to))
		t))
      (and to (setq to (mapconcat 'identity to ",\n    ")))
      (and cc (setq cc (mapconcat 'identity cc ",\n    ")))
      (and (null to) (setq to cc cc nil))
      (setq references (delq nil references)
	    references (mapconcat 'identity references " ")
	    references (vm-parse references "[^<]*\\(<[^>]+>\\)")
	    references (vm-delete-duplicates references)
	    references (if references (mapconcat 'identity references "\n\t")))
      (setq newsgroups (delq nil newsgroups)
	    newsgroups (mapconcat 'identity newsgroups ",")
	    newsgroups (vm-parse newsgroups "[ \t\f\r\n,]*\\([^ \t\f\r\n,]+\\)")
	    newsgroups (vm-delete-duplicates newsgroups)
	    newsgroups (if newsgroups (mapconcat 'identity newsgroups ",")))
      (vm-mail-internal
       (format "reply to %s%s" (vm-su-full-name (car mlist))
	       (if (cdr mlist) ", ..." ""))
       to subject in-reply-to cc references newsgroups)
      (use-local-map (copy-keymap (current-local-map)))
      (local-set-key "\C-c\C-y" 'vm-yank-message)
      (local-set-key "\C-cy" 'vm-yank-message-other-folder)
      (local-set-key "\C-c\C-s" 'vm-mail-send)
      (local-set-key "\C-c\C-c" 'vm-mail-send-and-exit)
      (local-set-key "\C-c\C-v" vm-mode-map)
      (make-local-variable 'vm-reply-list)
      (setq vm-mail-buffer mail-buffer
	    vm-system-state 'replying
	    vm-message-pointer message-pointer
	    vm-reply-list mlist
	    default-directory dir)
      (if include-text
	  (save-excursion
	    (goto-char (point-min))
	    (re-search-forward (concat "^" mail-header-separator "$") nil 0)
	    (forward-char 1)
	    (while mlist
	      (vm-yank-message (car mlist))
	      (goto-char (point-max))
	      (setq mlist (cdr mlist))))))))

(defun vm-strip-ignored-addresses (addresses)
  (setq addresses (copy-sequence addresses))
  (let (re-list list addr-list)
    (setq re-list vm-reply-ignored-addresses)
    (while re-list
      (setq addr-list addresses)
      (while addr-list
	(if (string-match (car re-list) (car addr-list))
	    (setq addresses (delq (car addr-list) addresses)))
	(setq addr-list (cdr addr-list)))
      (setq re-list (cdr re-list))))
  addresses )

(defun vm-mail-yank-default (message)
  (save-excursion
    (delete-region (point) (progn (search-forward "\n\n") (point)))
    (if vm-included-text-attribution-format
	(insert (vm-sprintf 'vm-included-text-attribution-format message)))
    ; turn off zmacs-regions for Lucid Emacs 19
    ; and get around transient-mark-mode in FSF Emacs 19
    ; all this so that (mark) does what it did in v18, sheesh.
    (let ((zmacs-regions nil)
	  (mark-even-if-active t))
      (while (and (re-search-forward "^" nil t) (< (point) (mark)))
	(replace-match vm-included-text-prefix t t)))))

(defun vm-yank-message-other-folder (folder &optional prefix-argument)
  "Like vm-yank-message except the message is yanked from a folder other
than the one that spawned the current Mail mode buffer.  The name of the
folder is read from the minibuffer.

Don't call this function from a program."
  (interactive
   (list
    (let ((dir (if vm-folder-directory
		    (expand-file-name vm-folder-directory)
		  default-directory)))
      (read-file-name "Yank from folder: " dir nil t))
    current-prefix-arg ))
  (let ((b (current-buffer)) newbuf sumbuf)
    (set-buffer (or (get-file-buffer folder) (find-file-noselect folder)))
    (setq newbuf (current-buffer))
    (if (not (eq major-mode 'vm-mode))
	(vm-mode))
    (if (null vm-message-pointer)
	(error "No messages in folder %s" folder))
    (save-excursion
      (save-window-excursion
	(save-window-excursion
	  (vm-summarize))
	(switch-to-buffer vm-summary-buffer)
	(setq sumbuf (current-buffer))
	(delete-other-windows)
	(set-buffer b)
	(unwind-protect
	    (let ((prefix-arg prefix-argument)
		  (vm-mail-buffer newbuf))
	      (command-execute 'vm-yank-message))
	  (bury-buffer newbuf)
	  (bury-buffer sumbuf))))))

(defun vm-yank-message (message &optional prefix)
  "Yank message number N into the current buffer at point.
When called interactively N is always read from the minibuffer.  When
called non-interactively the first argument is expected to be a
message struct.

This command is meant to be used in VM created Mail mode buffers; the
yanked message comes from the mail buffer containing the message you
are replying to, forwarding, or invoked VM's mail command from.

All message headers are yanked along with the text.  Point is left
before the inserted text, the mark after.  Any hook functions bound to
`mail-citation-hook' are run, after inserting the text and setting
point and mark.

Prefix arg means to ignore `mail-citation-hook', don't set the mark,
prepend the value of `vm-included-text-prefix' to every yanked line,
and don't yank any headers other than those specified in
`vm-visible-headers'/`vm-invisible-headers'.  For backwards
compatibility, if `mail-citation-hook' is set to nil,
`mail-yank-hooks' is run instead.  If that is also nil, a default
action is taken."
  (interactive
   (list
   ;; What we really want for the first argument is a message struct,
   ;; but if called interactively, we let the user type in a message
   ;; number instead.
    (let (mp default (result 0) prompt)
      (save-excursion
	(vm-select-folder-buffer)
	(setq default (and vm-message-pointer
			   (vm-number-of (car vm-message-pointer)))
	      prompt (if default
			 (format "Yank message number: (default %s) "
				 default)
		       "Yank message number: "))
	(while (zerop result)
	  (setq result (read-string prompt))
	  (and (string= result "") default (setq result default))
	  (setq result (string-to-int result)))
	(if (null (setq mp (nthcdr (1- result) vm-message-list)))
	    (error "No such message.")))
      (car mp))
    current-prefix-arg ))
  (if (not (bufferp vm-mail-buffer))
      (error "This is not a VM Mail mode buffer."))
  (if (null (buffer-name vm-mail-buffer))
      (error "The mail buffer containing message %d has been killed."
	     (vm-number-of message)))
  (let ((b (current-buffer)) (start (point)) mp end)
    (save-restriction
      (widen)
      (save-excursion
	(set-buffer (marker-buffer (vm-start-of message)))
	(save-restriction
	  (widen)
	  (append-to-buffer b (if prefix
				  (vm-vheaders-of message)
				(vm-start-of message))
			    (vm-text-end-of message))
	  (setq end (vm-marker (+ start (- (vm-text-end-of message)
					   (if prefix
					       (vm-vheaders-of message)
					     (vm-start-of message)))) b))))
      (if prefix
	  (save-excursion
	    (while (and (< (point) end) (re-search-forward "^" end t))
	      (replace-match vm-included-text-prefix t t)
	      (forward-line)))
	;; Delete UNIX From or MMDF ^A^A^A^A line
	(delete-region (point) (progn (forward-line) (point)))
	(push-mark end)
	(cond
	 (mail-citation-hook (run-hooks 'mail-citation-hook))
	 (mail-yank-hooks    (run-hooks 'mail-yank-hooks))
	 (t (vm-mail-yank-default message)))))))

(defun vm-mail-send-and-exit (arg)
  "Just like mail-send-and-exit except that VM flags the appropriate message(s)
as having been replied to, if appropriate."
  (interactive "P")
  (let ((reply-buf (current-buffer)))
    (mail-send-and-exit arg)
    (save-excursion
      (set-buffer reply-buf)
      (cond ((eq vm-system-state 'replying)
	     (vm-mark-replied))
	    ((eq vm-system-state 'forwarding)
	     (vm-mark-forwarded)))
      (vm-rename-current-mail-buffer)
      ;; keep this buffer if the user demands it
      (if (memq (current-buffer) vm-kept-mail-buffers)
	  (setq vm-kept-mail-buffers
		(delq (current-buffer) vm-kept-mail-buffers)))
      (setq vm-kept-mail-buffers (cons (current-buffer) vm-kept-mail-buffers))
      (if (not (eq vm-keep-sent-messages t))
	  (let ((extras (nthcdr (or vm-keep-sent-messages 0) vm-kept-mail-buffers)))
	    (mapcar (function (lambda (b) (and (buffer-name b) (kill-buffer b)))) extras)
	    (and vm-kept-mail-buffers extras
		 (setcdr (memq (car extras) vm-kept-mail-buffers) nil)))))))

(defun vm-mail-send ()
  "Just like mail-send except that VM flags the appropriate message(s)
as having been replied to, if appropriate."
  (interactive)
  (mail-send)
  (vm-rename-current-mail-buffer)
  (cond ((eq vm-system-state 'replying)
	 (vm-mark-replied))
	((eq vm-system-state 'forwarding)
	 (vm-mark-forwarded))))

(defun vm-rename-current-mail-buffer ()
  (if (not (string-match "^sent " (buffer-name)))
      (let (prefix name n)
	(if (not (= ?* (aref (buffer-name) 0)))
	    (setq prefix (format "sent %s" (buffer-name)))
	  (let (recipients)
	    (cond ((not (zerop (length (setq recipients (mail-fetch-field "To"))))))
		  ((not (zerop (length (setq recipients (mail-fetch-field "Cc"))))))
		  ((not (zerop (length (setq recipients (mail-fetch-field "Bcc"))))))
		  ; can't happen?!?
		  (t (setq recipients "the horse with no name")))
	    (setq prefix (format "sent mail to %s" recipients))))
	(setq name prefix n 1)
	(while (get-buffer name)
	  (setq name (format "%s<%d>" prefix n))
	  (vm-increment n))
	(rename-buffer name))))

(defun vm-mark-replied ()
  (save-excursion
    (let ((mp vm-reply-list))
      (while mp
	(if (null (marker-buffer (vm-start-of (car mp))))
	    ()
	  (set-buffer (marker-buffer (vm-start-of (car mp))))
	  (cond ((and (memq (car mp) vm-message-list)
		      (null (vm-replied-flag (car mp))))
		 (vm-set-replied-flag (car mp) t))))
	(setq mp (cdr mp)))
      (vm-update-summary-and-mode-line))))

(defun vm-mark-forwarded ()
  (save-excursion
    (let ((mp vm-forward-list))
      (while mp
	(if (null (marker-buffer (vm-start-of (car mp))))
	    ()
	  (set-buffer (marker-buffer (vm-start-of (car mp))))
	  (cond ((and (memq (car mp) vm-message-list)
		      (null (vm-forwarded-flag (car mp))))
		 (vm-set-forwarded-flag (car mp) t))))
	(setq mp (cdr mp)))
      (vm-update-summary-and-mode-line))))

(defun vm-reply (count)
  "Reply to the sender of the current message.
Numeric prefix argument N mans to reply to the current message plus the
next N-1 messages.  A negative N means reply to the current message and
the previous N-1 messages. 

If invoked on marked messages (via vm-next-command-uses-marks),
all marked messages will be replied to.

You will be placed into a standard Emacs Mail mode buffer to compose and
send your message.  See the documentation for the function `mail' for
more info.

Note that the normal binding of C-c C-y in the reply buffer is
automatically changed to vm-yank-message during a reply.  This
allows you to yank any message from the current folder into a
reply.

Normal VM commands may be accessed in the reply buffer by prefixing them
with C-c C-v."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-do-reply nil nil count))

(defun vm-reply-include-text (count)
  "Reply to the sender (only) of the current message and include text
from the message.  See the documentation for function vm-reply for details."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-do-reply nil t count))

(defun vm-followup (count)
  "Reply to all recipients of the current message.
See the documentation for the function vm-reply for details."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-do-reply t nil count))

(defun vm-followup-include-text (count)
  "Reply to all recipients of the current message and include text from
the message.  See the documentation for the function vm-reply for details."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-do-reply t t count))

(defun vm-forward-message ()
  "Forward the current message to one or more third parties.
You will be placed in a Mail mode buffer as is usual with replies, but you
must fill in the To: and Subject: headers manually."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if (eq last-command 'vm-next-command-uses-marks)
      (progn (setq this-command 'vm-next-command-uses-marks)
	     (command-execute 'vm-send-digest))
    (let ((b (current-buffer))
	  (dir default-directory)
	  (mp vm-message-pointer)
	  start)
      (save-restriction
	(widen)
	(vm-mail-internal
	 (format "forward of %s:%s" (buffer-name)
		 (vm-number-of (car vm-message-pointer)))
	 nil
	 (and vm-forwarding-subject-format
	      (vm-sprintf 'vm-forwarding-subject-format
			  (car mp))))
	(use-local-map (copy-keymap (current-local-map)))
	(local-set-key "\C-c\C-y" 'vm-yank-message)
	(local-set-key "\C-cy" 'vm-yank-message-other-folder)
	(local-set-key "\C-c\C-s" 'vm-mail-send)
	(local-set-key "\C-c\C-c" 'vm-mail-send-and-exit)
	(local-set-key "\C-c\C-v" vm-mode-map)
	(make-local-variable 'vm-forward-list)
	(setq vm-mail-buffer b
	      vm-system-state 'forwarding
	      vm-forward-list (list (car mp))
	      vm-message-pointer mp
	      default-directory dir)
	(goto-char (point-max))
	(insert "------- Start of forwarded message -------\n")
	(setq start (point))
	(insert-buffer-substring b
				 (save-excursion
				   (set-buffer b)
				   (goto-char (vm-start-of (car mp)))
				   (forward-line 1)
				   (point))
				 (vm-text-end-of (car mp)))
	(if vm-rfc934-forwarding
	    (vm-rfc934-char-stuff-region start (point)))
	(insert "------- End of forwarded message -------\n")
	(goto-char (point-min))
	(end-of-line)))))

(defun vm-mail ()
  "Send a mail message from within VM, or from without."
  (interactive)
  (vm-session-initialization)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (let ((mail-buffer (if (memq major-mode '(vm-mode vm-virtual-mode))
			 (current-buffer))))
    (vm-mail-internal)
    (if (null mail-buffer)
	()
      (use-local-map (copy-keymap (current-local-map)))
      (local-set-key "\C-c\C-y" 'vm-yank-message)
      (local-set-key "\C-c\C-v" vm-mode-map)
      (setq vm-mail-buffer mail-buffer))
    (local-set-key "\C-c\C-s" 'vm-mail-send)
    (local-set-key "\C-c\C-c" 'vm-mail-send-and-exit)
    (local-set-key "\C-cy" 'vm-yank-message-other-folder)))

(defun vm-resend-bounced-message ()
  "Extract the original text from a bounced message and resend it.
You will be placed in a Mail mode buffer with the extracted message and
you can change the recipient address before resending the message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (let ((b (current-buffer)) start
	(dir default-directory)
	(lim (vm-text-end-of (car vm-message-pointer))))
      (save-restriction
	(widen)
	(save-excursion
	  (goto-char (vm-text-of (car vm-message-pointer)))
	  (let (case-fold-search)
	    ;; What a wonderful world it would be if mailers used the
	    ;; message encapsulation standard instead the following
	    ;; ad hockeries.
	    (or
	     ;; sendmail
	     (search-forward "----- Unsent message follows" lim t)
	     ;; smail 2.x
	     (search-forward "======= text of message follows" lim t)
	     ;; smail 3.x (?)
	     (search-forward "- Message text follows:" lim t)
	     ;; MMDF
	     (search-forward "Your message follows:\n" lim t)
 	     (search-forward "    Your message begins as follows:\n" lim t)
	     ;; zmailer (?)
	     (search-forward "---  Original Message  ---" lim t)
	     ;; Grapevine
	     (search-forward "The text of your message was\n---" lim t)
	     (error "This does not appear to be a bounced message."))
	    (forward-line 1)
	    (setq start (point))))
	(vm-mail-internal
	 (format "retry of %s:%s" (buffer-name)
		 (vm-number-of (car vm-message-pointer))))
	(use-local-map (copy-keymap (current-local-map)))
	(local-set-key "\C-c\C-y" 'vm-yank-message)
	(local-set-key "\C-cy" 'vm-yank-message-other-folder)
	(local-set-key "\C-c\C-s" 'vm-mail-send)
	(local-set-key "\C-c\C-c" 'vm-mail-send-and-exit)
	(local-set-key "\C-c\C-v" vm-mode-map)
	(goto-char (point-min))
	(insert-buffer-substring b start lim)
	(delete-region (point) (point-max))
	(goto-char (point-min))
	;; some mailers leave grot at the top of the message.
	;; trim it.
	(while (not (looking-at vm-generic-header-regexp))
	  (delete-region (point) (progn (forward-line 1) (point))))
	;; delete all but pertinent headers
	(while (looking-at vm-generic-header-regexp)
	  (let ((match-end-0 (match-end 0)))
	    (if (or
		 (looking-at "From:\\|To:\\|Cc:\\|Subject:\\|In-Reply-To\\|Resent-")
		 (looking-at "Newsgroups\\|References"))
		(goto-char match-end-0)
	      (delete-region (point) match-end-0))))
	(insert mail-header-separator)
	(if (= (following-char) ?\n)
	    (forward-char 1)
	  (insert "\n"))
	(setq vm-mail-buffer b
	      default-directory dir))))

(defun vm-resend-message (recipients)
  "Resend the current message to someone else.
You will be proompted in the minibuffer for the space or comma
separated recipient list."
  (interactive "sResend message to: ")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((vmp vm-message-pointer)
	recipient-list t-buffer m-buffer
	doomed-buffers
	(pop-up-windows (and pop-up-windows (eq vm-mutable-windows t)))
	(mail-header-seaprator ""))
    (setq recipient-list (vm-parse recipients "[ \t,]*\\([^ ,\t]+\\)[ \t,]*"))
    (unwind-protect
	(progn
	  (vm-within-current-message-buffer
	   (vm-save-restriction
	    (widen)
	    (save-excursion
	      (setq m-buffer (generate-new-buffer "*VM resend*"))
	      (set-buffer m-buffer)
	      (make-local-variable 'vm-forward-list)
	      (setq vm-system-state 'forwarding
		    vm-forward-list (list (car vmp)))
	      (erase-buffer)
	      (insert-buffer-substring
	       (marker-buffer (vm-start-of (car vmp)))
	       (vm-start-of (car vmp))
	       (vm-text-end-of (car vmp))))))
	  (set-buffer m-buffer)
	  (goto-char (point-min))
	  ;; some mailers leave grot at the top of the message.
	  ;; trim it.
	  (while (not (looking-at vm-generic-header-regexp))
	    (delete-region (point) (progn (forward-line 1) (point))))
	  ;; indicate that this is a resend.  this should be all
	  ;; that's needed, as sendmail will generate the rest.
	  ;; But generate a Resent-From is the user is picky
	  ;; about the From header.
	  (if vm-mail-header-from
	      (insert "Resent-From: " vm-mail-header-from "\n"))
	  (insert "Resent-To: " (mapconcat 'identity recipient-list ", ") "\n")
	  ;; delete all but pertinent headers
	  (while (looking-at vm-generic-header-regexp)
	    (let ((match-end-0 (match-end 0)))
	      (if (not (looking-at "X-VM-\\|Status:"))
		  (goto-char match-end-0)
		(delete-region (point) match-end-0))))
	  (setq t-buffer
		(generate-new-buffer
		 (format "transcript of resend to %s" recipients)))
	  (goto-char (point-min))
	  (message "Sending...")
	  (apply 'call-process-region
		 (nconc (list (point-min) (point-max)
			      sendmail-program
			      nil t-buffer mail-interactive
			      "-oi" "-oem")
			(if mail-interactive
			    (list "-v")
			  (list "-odb"))
			recipient-list))
	  (vm-mark-forwarded)
	  (set-buffer t-buffer)
	  (if (zerop (buffer-size))
	      (message "Sent.")
	    (display-buffer t-buffer)
	    (bury-buffer t-buffer)
	    (setq t-buffer nil)
	    (message "")))
      (and t-buffer (kill-buffer t-buffer))
      (and m-buffer (kill-buffer m-buffer)))))

(defun vm-send-digest (&optional prefix)
  "Send a digest of all messages in the current folder to recipients.
You will be placed in a *mail* buffer as is usual with replies, but you
must fill in the To: and Subject: headers manually.

Prefix arg means to insert a list of preamble lines at the beginning of
the digest.  One line is generated for each message being digestified.
The variable vm-digest-preamble-format determines the format of the
preamble lines.

If invoked on marked messages (via vm-next-command-uses-marks),
only marked messages will be put into the digest."
  (interactive "P")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((b (current-buffer))
	(dir default-directory)
	(mp vm-message-pointer)
	;; prefix arg doesn't have "normal" meaning here, so only call
	;; vm-select-marked-or-prefixed-messages if we're using marks.
	(mlist (if (eq last-command 'vm-next-command-uses-marks)
		   (vm-select-marked-or-prefixed-messages 0)
		 vm-message-list))
	start)
    (save-restriction
      (widen)
      (vm-mail-internal (format "digest from %s" (buffer-name)))
      (use-local-map (copy-keymap (current-local-map)))
      (local-set-key "\C-c\C-y" 'vm-yank-message)
      (local-set-key "\C-cy" 'vm-yank-message-other-folder)
      (local-set-key "\C-c\C-s" 'vm-mail-send)
      (local-set-key "\C-c\C-c" 'vm-mail-send-and-exit)
      (local-set-key "\C-c\C-v" vm-mode-map)
      (make-local-variable 'vm-forward-list)
      (setq vm-mail-buffer b
	    vm-message-pointer mp
	    vm-system-state 'forwarding
	    vm-forward-list mlist
	    default-directory dir)
      (goto-char (point-max))
      (setq start (point)
	    mp mlist)
      (message "Building digest...")
      (while mp
	(insert-buffer-substring (marker-buffer (vm-start-of (car mp)))
				 (vm-start-of (car mp))
				 (vm-end-of (car mp)))
	(setq mp (cdr mp)))
      (vm-digestify-region start (point))
      (goto-char start)
      (setq mp mlist)
      (if prefix
	  (progn
	    (message "Building digest preamble...")
	    (while mp
	      (insert (vm-sprintf 'vm-digest-preamble-format (car mp)) "\n")
	      (if vm-digest-center-preamble
		  (progn
		    (forward-char -1)
		    (center-line)
		    (forward-char 1)))
	      (setq mp (cdr mp)))))
      (goto-char (point-min))
      (end-of-line)
      (message "Building digest... done"))))

(defun vm-continue-composing-message (&optional not-picky)
  "Find and select the most recently used mail composition buffer.
If the selected buffer is already a Mail mode buffer then it is
buried before beginning the search.  Non Mail mode buffers and
unmodified Mail buffers are skipped.  Prefix arg means unmodified
Mail mode buffers are not skipped.  If no suitable buffer is
found, the current buffer remains selected."
  (interactive "P")
  (if (eq major-mode 'mail-mode)
      (bury-buffer (current-buffer)))
  (let ((b (vm-find-composition-buffer not-picky)))
    (if (not (or (null b) (eq b (current-buffer))))
	(switch-to-buffer b)
      (message "No composition buffers found"))))

(defun vm-find-composition-buffer (&optional not-picky)
  (let ((b-list (buffer-list)) choice alternate)
    (save-excursion
     (while b-list
       (set-buffer (car b-list))
       (if (eq major-mode 'mail-mode)
	   (if (buffer-modified-p)
	       (setq choice (current-buffer)
		     b-list nil)
	     (and not-picky (null alternate)
		  (setq alternate (current-buffer)))
	     (setq b-list (cdr b-list)))
	 (setq b-list (cdr b-list))))
    (or choice alternate))))

(defun vm-mail-internal
     (&optional buffer-name to subject in-reply-to cc references newsgroups)
  (set-buffer (generate-new-buffer (or buffer-name "*VM-mail*")))
  (auto-save-mode auto-save-default)
  (mail-mode)
  (switch-to-buffer (current-buffer))
  (if (fboundp 'mail-aliases-setup)
      (mail-aliases-setup)
    (if (eq mail-aliases t)
	(progn
	  (setq mail-aliases nil)
	  (if (file-exists-p "~/.mailrc")
	      (build-mail-aliases)))))
  (if (stringp vm-mail-header-from)
      (insert "From: " vm-mail-header-from "\n"))
  (insert "To: " (or to "") "\n")
  (and cc (insert "Cc: " cc "\n"))
  (insert "Subject: " (or subject "") "\n")
  (and newsgroups (insert "Newsgroups: " newsgroups "\n"))
  (and in-reply-to (insert "In-Reply-To: " in-reply-to "\n"))
  (and references (insert "References: " references "\n"))
  (if mail-default-reply-to
      (insert "Reply-To: " mail-default-reply-to "\n"))
  (if mail-self-blind
      (insert "Bcc: " (user-login-name) "\n"))
  (if mail-archive-file-name
      (insert "FCC: " mail-archive-file-name "\n"))
  (insert mail-header-separator "\n")
  (save-excursion (vm-set-window-configuration 'composing-message))
  (if to
      (goto-char (point-max))
    (mail-position-on-field "To"))
  (run-hooks 'mail-setup-hook))

(require 'mail-utils)
(require 'sendmail)
