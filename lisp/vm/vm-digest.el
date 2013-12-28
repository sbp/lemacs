;;; Message encapsulation
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

(defun vm-no-frills-encapsulate-message (m keep-list discard-regexp)
  "Encapsulate a message M for forwarding, simply.
No message encapsulation standard is used.  The message is
inserted at point in the current buffer, surrounded by two dashed
start/end separator lines.  Point is not moved.

M should be a message struct for a real message, not a virtual message.
This is the message that will be encapsulated.
KEEP-LIST should be a list of regexps matching headers to keep.
DISCARD-REGEXP should be a regexp that matches headers to be discarded.
KEEP-LIST and DISCARD-REGEXP are used to order and trim the headers
to be forwarded.  See the docs for vm-reorder-message-headers
to find out how KEEP-LIST and DISCARD-REGEXP are used."
  (let ((target-buffer (current-buffer))
	(end (make-marker))
	source-buffer)
    (insert "------- start of forwarded message -------\n")
    (setq source-buffer (vm-buffer-of m))
    (save-excursion
      (set-buffer source-buffer)
      (save-restriction
	(widen)
	(save-excursion
	  (set-buffer target-buffer)
	  (let ((beg (point)))
	    (insert-buffer-substring source-buffer (vm-headers-of m)
				     (vm-text-end-of m))
	    (set-marker end (point))
	    (goto-char beg)
	    (vm-reorder-message-headers nil nil "\\(X-VM-\\|Status:\\)")
	    (vm-reorder-message-headers nil keep-list discard-regexp)))))
    (goto-char end)
    (insert "------- end of forwarded message -------\n")))

(defun vm-rfc934-char-stuff-region (start end)
  "Quote RFC 934 message separators between START and END.
START and END are buffer positions in the current buffer.
Lines beginning with `-' in the region have `- ' prepended to them."
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end) (re-search-forward "^-" end t))
      (replace-match "- -" t t)))
  (set-marker end nil))

(defun vm-rfc934-char-unstuff-region (start end)
  "Unquote lines in between START and END as per RFC 934.
START and END are buffer positions in the current buffer.
Lines beginning with `- ' in the region have that string stripped
from them."
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end) (re-search-forward "^- "  end t))
      (replace-match "" t t)
      (forward-char)))
  (set-marker end nil))

(defun vm-rfc934-encapsulate-messages (message-list keep-list discard-regexp)
  "Encapsulate the messages in MESSAGE-LIST as per RFC 934.
The resulting digest is inserted at point in the current buffer.
Point is not moved.

MESSAGE-LIST should be a list of message structs (real or virtual).
These are the messages that will be encapsulated.
KEEP-LIST should be a list of regexps matching headers to keep.
DISCARD-REGEXP should be a regexp that matches headers to be discarded.
KEEP-LIST and DISCARD-REGEXP are used to order and trim the headers
to be forwarded.  See the docs for vm-reorder-message-headers
to find out how KEEP-LIST and DISCARD-REGEXP are used."
  (if message-list
      (let ((target-buffer (current-buffer))
	    (mlist message-list)
	    (end (make-marker))
	    source-buffer m start n)
	(setq start (point))
	(while mlist
	  (insert "---------------\n")
	  (setq m (vm-real-message-of (car mlist))
		source-buffer (vm-buffer-of m))
	  (save-excursion
	    (set-buffer source-buffer)
	    (save-restriction
	      (widen)
	      (save-excursion
		(set-buffer target-buffer)
		(let ((beg (point)))
		  (insert-buffer-substring source-buffer (vm-headers-of m)
					   (vm-text-end-of m))
		  (set-marker end (point))
		  (goto-char beg)
		  (vm-reorder-message-headers nil nil "\\(X-VM-\\|Status:\\)")
		  (vm-reorder-message-headers nil keep-list discard-regexp)
		  (vm-rfc934-char-stuff-region beg end)))))
	  (goto-char end)
	  (insert "---------------")
	  (setq mlist (cdr mlist)))
	(beginning-of-line)
	(delete-region (point) (progn (end-of-line) (point)))
	(insert "------- end -------\n")
	(goto-char start)
	(delete-region (point) (progn (forward-line 1) (point)))
	(setq n (length message-list))
	(insert (format "------- start of %s%s(RFC 934 encapsulation) -------\n"
			(if (cdr message-list)
			    "digest "
			  "forwarded message ")
			(if (cdr message-list)
			    (format "(%d messages) " n)
			  "")))
	(goto-char start))))

(defun vm-rfc1153-char-stuff-region (start end)
  "Quote RFC 1153 message separators between START and END.
START and END are buffer positions in the current buffer.
Lines consisting only of 30 hyphens have the first hyphen
converted to a space."
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
		(re-search-forward "^------------------------------$" end t))
      (replace-match " -----------------------------" t t)))
  (set-marker end nil))

(defun vm-rfc1153-char-unstuff-region (start end)
  "Unquote lines in between START and END as per RFC 1153.
START and END are buffer positions in the current buffer.
Lines consisting only of a space following by 29 hyphens have the space
converted to a hyphen."
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
		(re-search-forward "^ -----------------------------$" end t))
      (replace-match "------------------------------" t t)))
  (set-marker end nil))

(defun vm-rfc1153-encapsulate-messages (message-list keep-list discard-regexp)
  "Encapsulate the messages in MESSAGE-LIST as per RFC 1153.
The resulting digest is inserted at point in the current buffer.
Point is not moved.

MESSAGE-LIST should be a list of message structs (real or virtual).
These are the messages that will be encapsulated.
KEEP-LIST should be a list of regexps matching headers to keep.
DISCARD-REGEXP should be a regexp that matches headers to be discarded.
KEEP-LIST and DISCARD-REGEXP are used to order and trim the headers
to be forwarded.  See the docs for vm-reorder-message-headers
to find out how KEEP-LIST and DISCARD-REGEXP are used."
  (if message-list
      (let ((target-buffer (current-buffer))
	    (mlist message-list)
	    (end (make-marker))
	    source-buffer m start)
	(setq start (point))
	(while mlist
	  (insert "---------------\n\n")
	  (setq m (vm-real-message-of (car mlist))
		source-buffer (vm-buffer-of m))
	  (save-excursion
	    (set-buffer source-buffer)
	    (save-restriction
	      (widen)
	      (save-excursion
		(set-buffer target-buffer)
		(let ((beg (point)))
		  (insert-buffer-substring source-buffer (vm-headers-of m)
					   (vm-text-end-of m))
		  (set-marker end (point))
		  (goto-char beg)
		  (vm-reorder-message-headers nil nil "\\(X-VM-\\|Status:\\)")
		  (vm-reorder-message-headers nil keep-list discard-regexp)
		  (vm-rfc1153-char-stuff-region beg end)))))
	  (goto-char end)
	  (insert "\n---------------")
	  (setq mlist (cdr mlist)))
	(insert "---------------\n\nEnd of this Digest\n******************\n")
	(goto-char start)
	(delete-region (point) (progn (forward-line 1) (point)))
	(insert (format "This is an RFC 1153 digest.\n(%d message%s)\n----------------------------------------------------------------------\n" (length message-list) (if (cdr message-list) "s" "")))
	(goto-char start))))

(defun vm-rfc1153-or-rfc934-burst-message (m rfc1153)
  "Burst messages from the digest message M.
M should be a message struct for a real message.
If RFC1153 is non-nil, assume the digest is of the form specified by
RFC 1153.  Otherwise assume RFC 934 digests."
  (let ((work-buffer nil)
	(match t)
	(prev-sep nil)
	(ident-header nil)
	after-prev-sep prologue-separator-regexp separator-regexp
	(folder-type vm-folder-type))
    (if vm-digest-identifier-header-format
	(setq ident-header (vm-sprintf 'vm-digest-identifier-header-format m)))
    (if rfc1153
	(setq prologue-separator-regexp "^----------------------------------------------------------------------\n"
	      separator-regexp "^------------------------------\n")
      (setq prologue-separator-regexp "^-[^ ].*\n"
	    separator-regexp "^-[^ ].*\n"))
    (save-excursion
      (vm-save-restriction
       (widen)
       (unwind-protect
	   (catch 'done
	     (setq work-buffer (generate-new-buffer "*vm-work*"))
	     (set-buffer work-buffer)
	     (insert-buffer-substring (vm-buffer-of m)
				      (vm-text-of m)
				      (vm-text-end-of m))
	     (goto-char (point-min))
	     (if (not (re-search-forward prologue-separator-regexp nil t))
		 (throw 'done nil))
	     ;; think of this as a do-while loop.
	     (while match
	       (cond ((null prev-sep)
		      ;; from (point-min) to end of match
		      ;; is the digest prologue, devour it and
		      ;; carry on.
		      (delete-region (point-min) (match-end 0)))
		     (t
		      ;; munge previous messages message separators
		      (let ((md (match-data)))
			(unwind-protect
			    (vm-munge-message-separators
			     folder-type
			     after-prev-sep
			     (match-beginning 0))
			  (store-match-data md)))
		      ;; eat preceding newlines
		      (while (= (preceding-char) ?\n)
			(delete-char -1))
		      ;; put one back
		      (insert ?\n)))
	       ;; insert a trailing message separator
	       ;; delete the digest separator
	       ;; insert the leading separator
	       (if prev-sep
		   (progn
		     (delete-region (match-beginning 0) (match-end 0))
		     (insert (vm-trailing-message-separator folder-type))))
	       (setq prev-sep (point))
	       (insert (vm-leading-message-separator folder-type))
	       (setq after-prev-sep (point))
	       ;; eat trailing newlines
	       (while (= (following-char) ?\n)
		 (delete-char 1))
	       (insert ident-header)
	       ;; try to match message separator and repeat.
	       (setq match (re-search-forward separator-regexp nil t)))
	     ;; from the last separator to eof is the digest epilogue.
	     ;; discard it.
	     (delete-region (or prev-sep (point-min)) (point-max))
	     ;; do header conversions.
	     (let ((vm-folder-type folder-type))
	       (goto-char (point-min))
	       (while (vm-find-leading-message-separator)
		 (vm-skip-past-leading-message-separator)
		 (vm-convert-folder-type-headers folder-type folder-type)
		 (vm-find-trailing-message-separator)
		 (vm-skip-past-trailing-message-separator)))
	     ;; now insert the messages into the folder buffer
	     (cond ((not (zerop (buffer-size)))
		    ;; undo the quoting of the embedded message separators
		    (if rfc1153
			(vm-rfc1153-char-unstuff-region (point-min) (point-max))
		      (vm-rfc934-char-unstuff-region (point-min) (point-max)))
		    (set-buffer (vm-buffer-of m))
		    (let ((old-buffer-modified-p (buffer-modified-p))
			  (buffer-read-only nil)
			  (inhibit-quit t))
		      (goto-char (point-max))
		      (insert-buffer-substring work-buffer)
		      (set-buffer-modified-p old-buffer-modified-p)
		      ;; return non-nil so caller knows we found some messages
		      t ))
		   ;; return nil so the caller knows we didn't find anything
		   (t nil)))
	 (and work-buffer (kill-buffer work-buffer)))))))

(defun vm-rfc934-burst-message (m)
  "Burst messages from the RFC 934 digest message M.
M should be a message struct for a real message."
  (vm-rfc1153-or-rfc934-burst-message m nil))

(defun vm-rfc1153-burst-message (m)
  "Burst messages from the RFC 1153 digest message M.
M should be a message struct for a real message."
  (vm-rfc1153-or-rfc934-burst-message m t))

(defun vm-burst-digest (&optional digest-type)
  "Burst the current message (a digest) into its individual messages.
The digest's messages are assimilated into the folder as new mail
would be.

Optional argument DIGEST-TYPE is a string that tells VM what kind
of digest the current message is.  If it is not given the value
defaults to the value of vm-digest-burst-type.  When called
interactively DIGEST-TYPE will be read from the minibuffer."
  (interactive
   (list
    (let ((type nil)
	  (this-command this-command)
	  (last-command last-command))
      (setq type (completing-read (format "Digest type: (default %s) "
					  vm-digest-burst-type)
				  (append vm-digest-type-alist
					  (list '("guess")))
				  'identity nil))
      (if (string= type "")
	  vm-digest-burst-type
	type ))))
  (or digest-type (setq digest-type vm-digest-burst-type))
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((start-buffer (current-buffer)) m totals-blurb)
    (if (vm-virtual-message-p (car vm-message-pointer))
	(progn
	  (setq m (vm-real-message-of (car vm-message-pointer)))
	  (set-buffer (vm-buffer-of m)))
      (setq m (car vm-message-pointer)))
    (vm-error-if-folder-read-only)
    (if (equal digest-type "guess")
	(progn
	  (setq digest-type (vm-guess-digest-type m))
	  (if (null digest-type)
	      (error "Couldn't guess digest type."))))
    (message "Bursting %s digest..." digest-type)
    (cond
     ((cond ((equal digest-type "rfc934")
	     (vm-rfc934-burst-message m))
	    ((equal digest-type "rfc1153")
	     (vm-rfc1153-burst-message m))
	    (t (error "Unknown digest type: %s" digest-type)))
      (message "Bursting %s digest... done" digest-type)
      (vm-clear-modification-flag-undos)
      (vm-set-buffer-modified-p t)
      (vm-increment vm-modification-counter)
      (and vm-delete-after-bursting
	   ;; if start folder was virtual, we're now in the wrong
	   ;; buffer.  switch back.
	   (save-excursion
	     (set-buffer start-buffer)
	     (vm-delete-message 1)))
      (vm-assimilate-new-messages t)
      ;; say this NOW, before the non-previewers read a message,
      ;; alter the new message count and confuse themselves.
      (setq totals-blurb (vm-emit-totals-blurb))
      (vm-display nil nil '(vm-burst-digest
			    vm-burst-rfc934-digest
			    vm-burst-rfc1153-digest)
		  (list this-command))
      (if (vm-thoughtfully-select-message)
	  (vm-preview-current-message)
	(vm-update-summary-and-mode-line))
      (message totals-blurb))
     (t "No messages found in digest."))))

(defun vm-burst-rfc934-digest ()
  "Burst an RFC 934 style digest"
  (interactive)
  (vm-burst-digest "rfc934"))

(defun vm-burst-rfc1153-digest ()
  "Burst an RFC 1153 style digest"
  (interactive)
  (vm-burst-digest "rfc1153"))

(defun vm-guess-digest-type (m)
  "Guess the digest type of the message M.
M should be the message struct of a real message.
Returns either \"rfc934\" or \"rfc1153\"."
  (save-excursion
    (set-buffer (vm-buffer-of m))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (vm-text-of m))
	(if (search-forward "\n----------------------------------------------------------------------\n" nil t)
	    "rfc1153"
	  "rfc934")))))
