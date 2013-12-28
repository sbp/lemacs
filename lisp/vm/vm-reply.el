;;; Mailing, forwarding, and replying commands for VM
;;; Copyright (C) 1989, 1990, 1991, 1993, 1994 Kyle E. Jones
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
    (let ((mlist (vm-select-marked-or-prefixed-messages count))
	  (dir default-directory)
	  (message-pointer vm-message-pointer)
	  (case-fold-search t)
	  to cc subject mp in-reply-to references tmp tmp2 newsgroups)
      (setq mp mlist)
      (while mp 
	(cond
	 ((eq mlist mp)
	  (cond ((setq to
		       (let ((reply-to
			      (vm-get-header-contents (car mp) "Reply-To:")))
			 (if (vm-ignored-reply-to reply-to)
			     nil
			   reply-to ))))
		((setq to (vm-get-header-contents (car mp) "From:")))
		;; bad, but better than nothing for some
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
		  ;; bad, but better than nothing for some
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
      (make-local-variable 'vm-reply-list)
      (setq vm-system-state 'replying
	    vm-reply-list mlist
	    default-directory dir)
      (if include-text
	  (save-excursion
	    (goto-char (point-min))
	    (let ((case-fold-search nil))
	      (re-search-forward
	       (concat "^" (regexp-quote mail-header-separator) "$") nil 0))
	    (forward-char 1)
	    (while mlist
	      (vm-yank-message (car mlist))
	      (goto-char (point-max))
	      (setq mlist (cdr mlist)))))
      (run-hooks 'vm-reply-hook)
      (run-hooks 'vm-mail-mode-hook)))

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

(defun vm-ignored-reply-to (reply-to)
  (if reply-to
      (let (re-list result)
	(setq re-list vm-reply-ignored-reply-tos)
	(while re-list
	  (if (string-match (car re-list) reply-to)
	      (setq result t re-list nil)
	    (setq re-list (cdr re-list))))
	result )))

(defun vm-mail-yank-default (message)
  (save-excursion
    (vm-reorder-message-headers nil vm-included-text-headers
				vm-included-text-discard-header-regexp)
    ;; if all the headers are gone, delete the trailing blank line, too.
    (if (eq (following-char) ?\n)
	(delete-char 1))
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

(defun vm-yank-message-other-folder (folder)
  "Like vm-yank-message except the message is yanked from a folder other
than the one that spawned the current Mail mode buffer.  The name of the
folder is read from the minibuffer.

Don't call this function from a program."
  (interactive
   (list
    (let ((dir (if vm-folder-directory
		    (expand-file-name vm-folder-directory)
		  default-directory))
	  (last-command last-command)
	  (this-command this-command))
      (read-file-name "Yank from folder: " dir nil t))))
  (let ((b (current-buffer)) newbuf sumbuf default result prompt mp)
    (set-buffer (or (vm-get-file-buffer folder) (find-file-noselect folder)))
    (setq newbuf (current-buffer))
    (if (not (eq major-mode 'vm-mode))
	(vm-mode))
    (if (null vm-message-pointer)
	(error "No messages in folder %s" folder))
    (setq default (vm-number-of (car vm-message-pointer)))
    (save-excursion
      (save-window-excursion
	(save-window-excursion
	  (vm-summarize))
	(vm-display vm-summary-buffer t '(vm-yank-message-other-folder)
		    '(vm-yank-message-other-folder composing-message))
	(setq sumbuf (current-buffer))
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
	  (vm-yank-message (car mp)))
      (bury-buffer newbuf)
      (bury-buffer sumbuf))))

(defun vm-yank-message (message)
  "Yank message number N into the current buffer at point.
When called interactively N is always read from the minibuffer.  When
called non-interactively the first argument is expected to be a
message struct.

This command is meant to be used in VM created Mail mode buffers; the
yanked message comes from the mail buffer containing the message you
are replying to, forwarding, or invoked VM's mail command from.

All message headers are yanked along with the text.  Point is
left before the inserted text, the mark after.  Any hook
functions bound to mail-citation-hook are run, after inserting
the text and setting point and mark.  For backward compatibility,
if mail-citation-hook is set to nil, `mail-yank-hooks' is run
instead.

If mail-citation-hook and mail-yank-hooks are both nil, this
default action is taken: the yanked headers are trimmed as
specified by vm-included-text-headers and
vm-included-text-discard-header-regexp, and the value of
vm-included-text-prefix is prepended to every yanked line."
  (interactive
   (list
   ;; What we really want for the first argument is a message struct,
   ;; but if called interactively, we let the user type in a message
   ;; number instead.
    (let (mp default
	  (result 0)
	  prompt
	  (last-command last-command)
	  (this-command this-command))
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
      (car mp))))
  (if (not (bufferp vm-mail-buffer))
      (error "This is not a VM Mail mode buffer."))
  (if (null (buffer-name vm-mail-buffer))
      (error "The folder buffer containing message %d has been killed."
	     (vm-number-of message)))
  (vm-display nil nil '(vm-yank-message) '(vm-yank-message composing-message))
  (setq message (vm-real-message-of message))
  (let ((b (current-buffer)) (start (point)) end)
    (save-restriction
      (widen)
      (save-excursion
	(set-buffer (vm-buffer-of message))
	(save-restriction
	  (widen)
	  (append-to-buffer b (vm-headers-of message) (vm-text-end-of message))
	  (setq end (vm-marker (+ start (- (vm-text-end-of message)
					   (vm-headers-of message))) b))))
      (push-mark end)
      (cond (mail-citation-hook (run-hooks 'mail-citation-hook))
	    (mail-yank-hooks (run-hooks 'mail-yank-hooks))
	    (t (vm-mail-yank-default message))))))

(defun vm-mail-send-and-exit (arg)
  "Just like mail-send-and-exit except that VM flags the appropriate message(s)
as having been replied to, if appropriate."
  (interactive "P")
  (let ((b (current-buffer)))
    (vm-mail-send)
    (if (eq b (current-buffer))
	(progn
	  (bury-buffer (current-buffer))
	  (vm-display (current-buffer) nil '(vm-mail-send-and-exit)
		      '(vm-mail-send-and-exit reading-message startup)))
      (vm-display nil nil '(vm-mail-send-and-exit)
		  '(vm-mail-send-and-exit reading-message startup)))))

(defun vm-keep-mail-buffer (buffer)
  ;; keep this buffer if the user demands it
  (if (memq buffer vm-kept-mail-buffers)
      (setq vm-kept-mail-buffers
	    (delq buffer vm-kept-mail-buffers)))
  (setq vm-kept-mail-buffers (cons buffer vm-kept-mail-buffers)
	vm-kept-mail-buffers (vm-delete 'buffer-name
					vm-kept-mail-buffers t))
  (if (not (eq vm-keep-sent-messages t))
      (let ((extras (nthcdr (or vm-keep-sent-messages 0)
				vm-kept-mail-buffers)))
	(mapcar (function
		 (lambda (b)
		   (and (buffer-name b) (kill-buffer b))))
		extras)
	(and vm-kept-mail-buffers extras
	     (setcdr (memq (car extras) vm-kept-mail-buffers) nil)))))

(defun vm-help-tale ()
  (save-excursion
    (goto-char (point-min))
    (while (vm-match-header)
      (if (not (vm-match-header "To:\\|Resent-To:\\|Cc:\\|Resent-Cc:"))
	  (goto-char (vm-matched-header-end))
	(goto-char (vm-matched-header-contents-start))
	(if (re-search-forward "[^, \t][ \t]*\n[ \t\n]+[^ \t\n]"
			       (vm-matched-header-contents-end)
			       t)
	    (error "tale is an idiot, and so are you. :-)" (user-login-name)))
	(goto-char (vm-matched-header-end))))))

(defun vm-mail-send ()
  "Just like mail-send except that VM flags the appropriate message(s)
as replied to, forwarded, etc, if appropriate."
  (interactive)
  (if vm-tale-is-an-idiot
      (vm-help-tale))
  (if (and vm-confirm-mail-send
	   (not (y-or-n-p "Send the message? ")))
      (error "Message not sent."))
  ;; this to prevent Emacs 19 from asking whether a message that
  ;; has already been sent should be sent again.  VM renames mail
  ;; buffers after the message has beem sent, so the user should
  ;; already know that the message has been sent.
  (set-buffer-modified-p t)
  ;; don't want a buffer change to occur here
  ;; save-excursion to be sure.
  (save-excursion
    (mail-send))
  (vm-rename-current-mail-buffer)
  (cond ((eq vm-system-state 'replying)
	 (vm-mail-mark-replied))
	((eq vm-system-state 'forwarding)
	 (vm-mail-mark-forwarded))
	((eq vm-system-state 'redistributing)
	 (vm-mail-mark-redistributed)))
  (vm-keep-mail-buffer (current-buffer))
  (vm-display nil nil '(vm-mail-send) '(vm-mail-send)))

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

(defun vm-mail-mark-replied ()
  (save-excursion
    (let ((mp vm-reply-list))
      (while mp
	(if (null (buffer-name (vm-buffer-of (car mp))))
	    ()
	  (set-buffer (vm-buffer-of (car mp)))
	  (cond ((and (memq (car mp) vm-message-list)
		      (null (vm-replied-flag (car mp))))
		 (vm-set-replied-flag (car mp) t))))
	(setq mp (cdr mp)))
      (vm-update-summary-and-mode-line))))

(defun vm-mail-mark-forwarded ()
  (save-excursion
    (let ((mp vm-forward-list))
      (while mp
	(if (null (buffer-name (vm-buffer-of (car mp))))
	    ()
	  (set-buffer (vm-buffer-of (car mp)))
	  (cond ((and (memq (car mp) vm-message-list)
		      (null (vm-forwarded-flag (car mp))))
		 (vm-set-forwarded-flag (car mp) t))))
	(setq mp (cdr mp)))
      (vm-update-summary-and-mode-line))))

(defun vm-mail-mark-redistributed ()
  (save-excursion
    (let ((mp vm-redistribute-list))
      (while mp
	(if (null (buffer-name (vm-buffer-of (car mp))))
	    ()
	  (set-buffer (vm-buffer-of (car mp)))
	  (cond ((and (memq (car mp) vm-message-list)
		      (null (vm-redistributed-flag (car mp))))
		 (vm-set-redistributed-flag (car mp) t))))
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

(defun vm-forward-message-all-headers ()
  "Like vm-forward-message but always forwards all the headers."
  (interactive)
  (let ((vm-forwarded-headers nil)
	(vm-unforwarded-header-regexp "only-drop-this-header"))
    (vm-forward-message)))

(defun vm-forward-message ()
  "Forward the current message to one or more recipients.
You will be placed in a Mail mode buffer as you would with a
reply, but you must fill in the To: header and perhaps the
Subject: header manually."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if (eq last-command 'vm-next-command-uses-marks)
      (let ((vm-digest-send-type vm-forwarding-digest-type))
	(setq this-command 'vm-next-command-uses-marks)
	(command-execute 'vm-send-digest))
    (let ((dir default-directory)
	  (mp vm-message-pointer))
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
	(make-local-variable 'vm-forward-list)
	(setq vm-system-state 'forwarding
	      vm-forward-list (list (car mp))
	      default-directory dir)
	(goto-char (point-min))
	(re-search-forward
	 (concat "^" (regexp-quote mail-header-separator) "\n") nil 0)
	(cond ((equal vm-forwarding-digest-type "rfc934")
	       (vm-rfc934-encapsulate-messages
		vm-forward-list vm-forwarded-headers
		vm-unforwarded-header-regexp))
	      ((equal vm-forwarding-digest-type "rfc1153")
	       (vm-rfc1153-encapsulate-messages
		vm-forward-list vm-forwarded-headers
		vm-unforwarded-header-regexp))
	      ((equal vm-forwarding-digest-type nil)
	       (vm-no-frills-encapsulate-message
		(car vm-forward-list) vm-forwarded-headers
		vm-unforwarded-header-regexp)))
	(mail-position-on-field "To"))
      (run-hooks 'vm-forward-message-hook)
      (run-hooks 'vm-mail-mode-hook))))

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
	(goto-char (point-min))
	(insert-buffer-substring b start lim)
	(delete-region (point) (point-max))
	(goto-char (point-min))
	;; delete all but pertinent headers
	(vm-reorder-message-headers nil nil "\\(X-VM-\\|Status:\\|Sender:\\)")
	(vm-reorder-message-headers nil vm-resend-bounced-headers
				    vm-resend-bounced-discard-header-regexp)
	(if (search-forward "\n\n" nil t)
	    (replace-match "")
	  (goto-char (point-max)))
	(insert ?\n mail-header-separator ?\n)
	(mail-position-on-field "To")
	(setq default-directory dir)))
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
      (goto-char (point-min))
      (insert-buffer-substring b start lim)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (if vm-mail-header-from
	  (insert "Resent-From: " vm-mail-header-from ?\n))
      (insert "Resent-To: \n")
      ;; delete all but pertinent headers
      (vm-reorder-message-headers nil nil "\\(X-VM-\\|Status:\\|Sender:\\)")
      (vm-reorder-message-headers nil vm-resend-headers
				  vm-resend-discard-header-regexp)
      (if (search-forward "\n\n" nil t)
	  (replace-match ""))
      (insert ?\n mail-header-separator ?\n)
      (goto-char (point-min))
      (mail-position-on-field "Resent-To")
      (make-local-variable 'vm-redistribute-list)
      (setq vm-system-state 'redistributing
	    vm-redistribute-list (list (car vmp))
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
  (let ((dir default-directory)
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
      (make-local-variable 'vm-forward-list)
      (setq vm-system-state 'forwarding
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
      (mail-position-on-field "To")
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
	(vm-display b t '(vm-continue-composing-message)
		    '(vm-continue-composing-message composing-message))
      (message "No composition buffers found"))))

;; to quiet the v19 byte compiler
(defvar mail-mode-map)
(defvar mail-aliases)
(defvar mail-default-reply-to)
(defvar mail-signature-file)

(defun vm-mail-internal
    (&optional buffer-name to subject in-reply-to cc references newsgroups)
  (let ((folder-buffer nil))
    (if (memq major-mode '(vm-mode vm-virtual-mode))
	(setq folder-buffer (current-buffer)))
    (set-buffer (generate-new-buffer (or buffer-name "*VM-mail*")))
    (auto-save-mode auto-save-default)
    (mail-mode)
    (setq vm-mail-buffer folder-buffer)
    (use-local-map vm-mail-mode-map)
    (if (fboundp 'mail-aliases-setup) ; use mail-abbrevs.el if present
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
    (cond ((stringp mail-signature)
	   (save-excursion
	     (insert mail-signature)))
	  ((eq mail-signature t)
	   (save-excursion
	     (insert "-- \n")
	     (insert-file-contents (or (and (boundp 'mail-signature-file)
					    (stringp mail-signature-file)
					    mail-signature-file)
				       "~/.signature")))))
    ;; move this buffer to the head of the buffer list so window
    ;; config stuff will select it as the composition buffer.
    (save-window-excursion
      (switch-to-buffer (current-buffer)))
    (vm-display (current-buffer) t
		'(vm-mail
		  vm-mail-other-frame
		  vm-mail-other-window
		  vm-reply
		  vm-reply-other-frame
		  vm-reply-include-text
		  vm-reply-include-text-other-frame
		  vm-followup
		  vm-followup-other-frame
		  vm-followup-include-text
		  vm-followup-include-text-other-frame
		  vm-send-digest
		  vm-send-digest-other-frame
		  vm-send-rfc934-digest
		  vm-send-rfc934-digest-other-frame
		  vm-send-rfc1153-digest
		  vm-send-rfc1153-digest-other-frame
		  vm-forward-message
		  vm-forward-message-other-frame
		  vm-forward-message-all-headers
		  vm-forward-message-all-headers-other-frame
		  vm-resend-message
		  vm-resend-message-other-frame
		  vm-resend-bounced-message
		  vm-resend-bounced-message-other-frame)
		(list this-command 'composing-message))
    (if (null to)
	(mail-position-on-field "To"))
    (run-hooks 'mail-setup-hook)))

(defun vm-reply-other-frame (count)
  "Like vm-reply, but run in a newly created frame."
  (interactive "p")
  (vm-goto-new-frame)
  (vm-reply count))

(defun vm-reply-include-text-other-frame (count)
  "Like vm-reply-include-text, but run in a newly created frame."
  (interactive "p")
  (vm-goto-new-frame)
  (vm-reply-include-text count))

(defun vm-followup-other-frame (count)
  "Like vm-followup, but run in a newly created frame."
  (interactive "p")
  (vm-goto-new-frame)
  (vm-followup count))

(defun vm-followup-include-text-other-frame (count)
  "Like vm-followup-include-text, but run in a newly created frame."
  (interactive "p")
  (vm-goto-new-frame)
  (vm-followup-include-text count))

(defun vm-forward-message-all-headers-other-frame ()
  "Like vm-forward-message-all-headers, but run in a newly created frame."
  (interactive)
  (vm-goto-new-frame)
  (vm-forward-message-all-headers))

(defun vm-forward-message-other-frame ()
  "Like vm-forward-message, but run in a newly created frame."
  (interactive)
  (vm-goto-new-frame)
  (vm-forward-message))

(defun vm-resend-message-other-frame ()
  "Like vm-resend-message, but run in a newly created frame."
  (interactive)
  (vm-goto-new-frame)
  (vm-resend-message))

(defun vm-resend-bounced-message-other-frame ()
  "Like vm-resend-bounced-message, but run in a newly created frame."
  (interactive)
  (vm-goto-new-frame)
  (vm-resend-bounced-message))

(defun vm-send-digest-other-frame (&optional prefix)
  "Like vm-send-digest, but run in a newly created frame."
  (interactive "P")
  (vm-goto-new-frame)
  (vm-send-digest prefix))

(defun vm-send-rfc934-digest-other-frame (&optional prefix)
  "Like vm-send-rfc934-digest, but run in a newly created frame."
  (interactive "P")
  (vm-goto-new-frame)
  (vm-send-rfc934-digest prefix))

(defun vm-send-rfc1153-digest-other-frame (&optional prefix)
  "Like vm-send-rfc1153-digest, but run in a newly created frame."
  (interactive "P")
  (vm-goto-new-frame)
  (vm-send-rfc1153-digest prefix))
