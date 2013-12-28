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
	  (case-fold-search t)
	  to cc subject mp in-reply-to references tmp tmp2 newsgroups)
      (setq mp mlist)
      (while mp 
	(cond
	 ((eq mlist mp)
	  (cond ((setq to (vm-get-header-contents (car mp) "Reply-To:")))
		((setq to (vm-get-header-contents (car mp) "From:")))
		((setq to (vm-grok-From_-author (car mp))))
		(t (error "No From: or Reply-To: header in message")))
	  (setq subject (vm-get-header-contents (car mp) "Subject:")
		in-reply-to
		(and vm-in-reply-to-format
		     (let ((vm-summary-uninteresting-senders nil))
		       (vm-sprintf 'vm-in-reply-to-format (car mp))))
		in-reply-to (and (not (equal "" in-reply-to)) in-reply-to))
	  (and subject vm-reply-subject-prefix
	       (let ((case-fold-search t))
		 (not
		  (equal
		   (string-match (regexp-quote vm-reply-subject-prefix)
				 subject)
		   0)))
	       (setq subject (concat vm-reply-subject-prefix subject))))
	 (t (cond ((setq tmp (vm-get-header-contents (car mp) "Reply-To:"))
		   (setq to (concat to "," tmp)))
		  ((setq tmp (vm-get-header-contents (car mp) "From:"))
		   (setq to (concat to "," tmp)))
		  ((setq tmp (vm-grok-From_-author (car mp)))
		   (setq to (concat to "," tmp)))
		  (t (error "No From: or Reply-To: header in message")))))
	(if to-all
	    (progn
	      (setq tmp (vm-get-header-contents (car mp) "To:"))
	      (setq tmp2 (vm-get-header-contents (car mp) "Cc:"))
	      (if tmp
		  (if cc
		      (setq cc (concat cc "," tmp))
		    (setq cc tmp)))
	      (if tmp2
		  (if cc
		      (setq cc (concat cc "," tmp2))
		    (setq cc tmp2)))))
	(setq references
	      (cons (vm-get-header-contents (car mp) "References:")
		    (cons (vm-get-header-contents (car mp) "In-reply-to:")
			  (cons (vm-get-header-contents (car mp) "Message-ID:")
				references))))
	(setq newsgroups
	      (cons (or (and to-all (vm-get-header-contents (car mp) "Followup-To:"))
			(vm-get-header-contents (car mp) "Newsgroups:"))
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
      (setq to (vm-delete-duplicates to nil t))
      (setq cc (vm-delete-duplicates
		(append (vm-delete-duplicates cc nil t)
			to (copy-sequence to))
		t t))
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
      (use-local-map vm-mail-mode-map)
      (make-local-variable 'vm-reply-list)
      (setq vm-mail-buffer mail-buffer
	    vm-system-state 'replying
	    vm-message-pointer message-pointer
	    vm-reply-list mlist
	    default-directory dir)
      (if include-text
	  (save-excursion
	    (goto-char (point-min))
	    (let ((case-fold-search nil))
	      (re-search-forward (concat "^" mail-header-separator "$") nil 0))
	    (forward-char 1)
	    (while mlist
	      (vm-yank-message (car mlist))
	      (goto-char (point-max))
	      (setq mlist (cdr mlist)))))
      (run-hooks 'vm-reply-hook)
      (run-hooks 'vm-mail-mode-hook))))

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
	(let ((vm-summary-uninteresting-senders nil))
	  (insert (vm-sprintf 'vm-included-text-attribution-format message))))
    ; turn off zmacs-regions for Lucid Emacs 19
    ; and get around transient-mark-mode in FSF Emacs 19
    ; all this so that (mark) does what it did in v18, sheesh.
    (let* ((zmacs-regions nil)
	   (mark-even-if-inactive t)
	   (end (mark-marker)))
      (while (< (point) end)
	(insert vm-included-text-prefix)
	(forward-line 1)))))

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
  (let ((b (current-buffer)) newbuf sumbuf default result prompt mp)
    (set-buffer (or (get-file-buffer folder) (find-file-noselect folder)))
    (setq newbuf (current-buffer))
    ;; make folder read-only temporarily to prevent
    ;; vm-preview-lines == nil from causing a buffer modification
    (let ((vm-folder-read-only t))
      (if (not (eq major-mode 'vm-mode))
	  (vm-mode)))
    (if (null vm-message-pointer)
	(error "No messages in folder %s" folder))
    (setq default (vm-number-of (car vm-message-pointer)))
    (save-excursion
      (save-window-excursion
	(save-window-excursion
	  (vm-summarize))
	(switch-to-buffer vm-summary-buffer)
	(setq sumbuf (current-buffer))
	(if (eq vm-mutable-windows t)
	    (delete-other-windows))
	(setq prompt (format "Yank message number: (default %s) " default)
	      result 0)
	(while (zerop result)
	  (setq result (read-string prompt))
	  (and (string= result "") default (setq result default))
	  (setq result (string-to-int result)))
	(if (null (setq mp (nthcdr (1- result) vm-message-list)))
	    (error "No such message."))))
    (set-buffer b)
    (unwind-protect
	(let ((vm-mail-buffer newbuf))
	  (vm-yank-message (car mp) prefix-argument))
      (bury-buffer newbuf)
      (bury-buffer sumbuf))))

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
      (error "The folder buffer containing message %d has been killed."
	     (vm-number-of message)))
  (setq message (vm-real-message-of message))
  (let ((b (current-buffer)) (start (point)) mp end)
    (save-restriction
      (widen)
      (save-excursion
	(set-buffer (marker-buffer (vm-start-of message)))
	(save-restriction
	  (widen)
	  (append-to-buffer b (if prefix
				  (vm-vheaders-of message)
				(vm-headers-of message))
			    (vm-text-end-of message))
	  (setq end (vm-marker (+ start (- (vm-text-end-of message)
					   (if prefix
					       (vm-vheaders-of message)
					     (vm-headers-of message)))) b))))
      (if prefix
	  (save-excursion
	    (while (< (point) end)
	      (insert vm-included-text-prefix)
	      (forward-line 1)))
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
	  (let ((extras (nthcdr (or vm-keep-sent-messages 0)
				vm-kept-mail-buffers)))
	    (mapcar (function
		     (lambda (b)
		       (and (buffer-name b) (kill-buffer b))))
		    extras)
	    (and vm-kept-mail-buffers extras
		 (setcdr (memq (car extras) vm-kept-mail-buffers) nil)))))))

(defun vm-mail-send ()
  "Just like mail-send except that VM flags the appropriate message(s)
as replied to, forwarded, etc, if appropriate."
  (interactive)
  (mail-send)
  (vm-rename-current-mail-buffer)
  (cond ((eq vm-system-state 'replying)
	 (vm-mark-replied))
	((eq vm-system-state 'forwarding)
	 (vm-mark-forwarded))))

(defun vm-rename-current-mail-buffer ()
  (let ((case-fold-search nil))
    (if (not (string-match "^sent " (buffer-name)))
	(let (prefix name n)
	  (if (not (= ?* (aref (buffer-name) 0)))
	      (setq prefix (format "sent %s" (buffer-name)))
	    (let (recipients)
	      (cond ((not (zerop (length (setq recipients
					       (mail-fetch-field "To"))))))
		    ((not (zerop (length (setq recipients
					       (mail-fetch-field "Cc"))))))
		    ((not (zerop (length (setq recipients
					       (mail-fetch-field "Bcc"))))))
					; can't happen?!?
		    (t (setq recipients "the horse with no name")))
	      (setq prefix (format "sent mail to %s" recipients))))
	  (setq name prefix n 1)
	  (while (get-buffer name)
	    (setq name (format "%s<%d>" prefix n))
	    (vm-increment n))
	  (rename-buffer name)))))

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
Numeric prefix argument N means to reply to the current message plus the
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

(defun vm-forward-message (&optional all-headers)
  "Forward the current message to one or more recipients.
You will be placed in a Mail mode buffer as you would with a
reply, but you must fill in the To: header and perhaps the
Subject: header manually.

Normally consults the variables `vm-forwarded-headers' and
`vm-unforwarded-header-regexp' to determine which headers are
forwarded and in what order.  With a prefix arg, the headers
are forwarded as-is."
  (interactive "P")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if (eq last-command 'vm-next-command-uses-marks)
      (let ((vm-digest-send-type vm-forwarding-digest-type))
	(setq this-command 'vm-next-command-uses-marks)
	(command-execute 'vm-send-digest))
    (let ((b (current-buffer))
	  (dir default-directory)
	  (mp vm-message-pointer)
	  (forwarded (if all-headers nil vm-forwarded-headers))
	  (unforwarded (if all-headers "^$" vm-unforwarded-header-regexp)))
      (save-restriction
	(widen)
	(vm-mail-internal
	 (format "forward of %s's note re: %s"
		 (vm-su-full-name (car vm-message-pointer))
		 (vm-su-subject (car vm-message-pointer)))
	 nil
	 (and vm-forwarding-subject-format
	      (let ((vm-summary-uninteresting-senders nil))
		(vm-sprintf 'vm-forwarding-subject-format (car mp)))))
	(use-local-map vm-mail-mode-map)
	(make-local-variable 'vm-forward-list)
	(setq vm-mail-buffer b
	      vm-system-state 'forwarding
	      vm-forward-list (list (car mp))
	      vm-message-pointer mp
	      default-directory dir)
	(goto-char (point-max))
	(cond ((equal vm-forwarding-digest-type "rfc934")
	       (vm-rfc934-encapsulate-messages
		vm-forward-list forwarded unforwarded))
	      ((equal vm-forwarding-digest-type "rfc1153")
	       (vm-rfc1153-encapsulate-messages
		vm-forward-list forwarded unforwarded))
	      ((equal vm-forwarding-digest-type nil)
	       (vm-no-frills-encapsulate-message
		(car vm-forward-list) forwarded unforwarded)))
	(mail-position-on-field "To"))
      (run-hooks 'vm-forward-message-hook)
      (run-hooks 'vm-mail-mode-hook))))

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
    (use-local-map vm-mail-mode-map)
    (if mail-buffer
	(setq vm-mail-buffer mail-buffer)))
  (run-hooks 'vm-mail-hook)
  (run-hooks 'vm-mail-mode-hook))

(defun vm-resend-bounced-message ()
  "Extract the original text from a bounced message and resend it.
You will be placed in a Mail mode buffer with the extracted message and
you can change the recipient address before resending the message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((b (current-buffer)) start
	(dir default-directory)
	(lim (vm-text-end-of (car vm-message-pointer))))
      (save-restriction
	(widen)
	(save-excursion
	  (goto-char (vm-text-of (car vm-message-pointer)))
	  (let ((case-fold-search t))
	    ;; What a wonderful world it would be if mailers used a single
	    ;; message encapsulation standard instead all the weird variants
	    ;; It is useless to try to cover them all.
	    ;; This simple rule should cover the sanest of the formats
	    (if (not (re-search-forward "^Received:" lim t))
		(error "This doesn't look like a bounced message."))
	    (beginning-of-line)
	    (setq start (point))))
	;; briefly nullify vm-mail-header-from to keep vm-mail-internal
	;; from inserting another From header.
	(let ((vm-mail-header-from nil))
	  (vm-mail-internal
	   (format "retry of bounce from %s"
		   (vm-su-from (car vm-message-pointer)))))
	(use-local-map vm-mail-mode-map)
	(goto-char (point-min))
	(insert-buffer-substring b start lim)
	(delete-region (point) (point-max))
	(goto-char (point-min))
	;; delete all but pertinent headers
	(vm-reorder-message-headers nil nil "\\(X-VM-\\|Status:\\)")
	(vm-reorder-message-headers nil vm-resend-bounced-headers
				    vm-resend-bounced-discard-header-regexp)
	(if (search-forward "\n\n" nil t)
	    (replace-match ""))
	(insert ?\n mail-header-separator ?\n)
	(setq vm-mail-buffer b
	      default-directory dir)))
  (run-hooks 'vm-resend-bounced-message-hook)
  (run-hooks 'vm-mail-mode-hook))

(defun vm-resend-message ()
  "Resend the current message to someone else.
The current message will be copied to a Mail mode buffer and you
can edit the message and send it as usual.

NOTE: since you are doing a resend, a Resent-To header is
provided for you to fill in.  If you don't fill it in, when you
send the message it will go to the original recipients listed in
the To and Cc headers.  You may also create a Resent-Cc header."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (save-restriction
    (widen)
    (let ((b (current-buffer))
	  (dir default-directory)
	  (vmp vm-message-pointer)
	  (start (vm-headers-of (car vm-message-pointer)))
	  (lim (vm-text-end-of (car vm-message-pointer))))
      ;; briefly nullify vm-mail-header-from to keep vm-mail-internal
      ;; from inserting another From header.
      (let ((vm-mail-header-from nil))
	(vm-mail-internal
	 (format "resend of %s's note re: %s"
		 (vm-su-full-name (car vm-message-pointer))
		 (vm-su-subject (car vm-message-pointer)))))
      (use-local-map vm-mail-mode-map)
      (goto-char (point-min))
      (insert-buffer-substring b start lim)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (if vm-mail-header-from
	  (insert "Resent-From: " vm-mail-header-from ?\n))
      (insert "Resent-To: \n")
      ;; delete all but pertinent headers
      (vm-reorder-message-headers nil nil "\\(X-VM-\\|Status:\\)")
      (vm-reorder-message-headers nil vm-resend-headers
				  vm-resend-discard-header-regexp)
      (if (search-forward "\n\n" nil t)
	  (replace-match ""))
      (insert ?\n mail-header-separator ?\n)
      (goto-char (point-min))
      (mail-position-on-field "Resent-To")
      (setq vm-mail-buffer b
	    vm-system-state 'forwarding
	    vm-forward-list (list (car vmp))
	    default-directory dir)
      (run-hooks 'vm-resend-message-hook)
      (run-hooks 'vm-mail-mode-hook))))

(defun vm-send-digest (&optional prefix)
  "Send a digest of all messages in the current folder to recipients.
The type of the digest is specified by the variable vm-digest-send-type.
You will be placed in a Mail mode buffer as is usual with replies, but you
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
      (use-local-map vm-mail-mode-map)
      (make-local-variable 'vm-forward-list)
      (setq vm-mail-buffer b
	    vm-message-pointer mp
	    vm-system-state 'forwarding
	    vm-forward-list mlist
	    default-directory dir)
      (goto-char (point-max))
      (setq start (point)
	    mp mlist)
      (message "Building %s digest..." vm-digest-send-type)
      (cond ((equal vm-digest-send-type "rfc934")
	     (vm-rfc934-encapsulate-messages
	      mlist vm-rfc934-digest-headers
	      vm-rfc934-digest-discard-header-regexp))
	    ((equal vm-digest-send-type "rfc1153")
	     (vm-rfc1153-encapsulate-messages
	      mlist vm-rfc1153-digest-headers
	      vm-rfc1153-digest-discard-header-regexp)))
      (goto-char start)
      (setq mp mlist)
      (if prefix
	  (progn
	    (message "Building digest preamble...")
	    (while mp
	      (let ((vm-summary-uninteresting-senders nil))
		(insert (vm-sprintf 'vm-digest-preamble-format (car mp)) "\n"))
	      (if vm-digest-center-preamble
		  (progn
		    (forward-char -1)
		    (center-line)
		    (forward-char 1)))
	      (setq mp (cdr mp)))))
      (goto-char (point-min))
      (end-of-line)
      (message "Building %s digest... done" vm-digest-send-type)))
  (run-hooks 'vm-send-digest-hook)
  (run-hooks 'vm-mail-mode-hook))

(defun vm-send-rfc934-digest (&optional preamble)
  "Like vm-send-digest but always sends an RFC 934 digest."
  (interactive "P")
  (let ((vm-digest-send-type "rfc934"))
    (vm-send-digest preamble)))

(defun vm-send-rfc1153-digest (&optional preamble)
  "Like vm-send-digest but always sends an RFC 1153 digest."
  (interactive "P")
  (let ((vm-digest-send-type "rfc1153"))
    (vm-send-digest preamble)))

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
  (if mail-default-headers
      (insert mail-default-headers))
  (if (not (= (preceding-char) ?\n))
      (insert ?\n))
  (insert mail-header-separator "\n")
  (if mail-signature
      (condition-case nil
	  (insert-file-contents (if (eq mail-signature t)
				    "~/.signature"
				  mail-signature))
	(error nil)))
  (save-excursion (vm-set-window-configuration 'composing-message))
  (if to
      (goto-char (point-max))
    (mail-position-on-field "To"))
  (run-hooks 'mail-setup-hook))

(require 'mail-utils)
(require 'sendmail)
