;; Allows GNUS to make posts in the background (via a second NNTP connection.)
;; By Jamie Zawinski <jwz@lucid.com> 6-jun-93
;; Copyright (C) 1993 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; This makes it possible for GNUS to post new articles by opening a second
;; (or third, or fourth...) NNTP connection and posting on that asynchronously,
;; instead of tying up the primary NNTP connection until the post completes.
;; This is nice because on some servers, posting a new article takes an 
;; obscenely long time.  Drawbacks of using this are slightly less feedback
;; about whether your post has succeeded (you might exit emacs before you got
;; to see the diagnostics) and that opening these subsequent connections uses
;; more resources on the NNTP server (and I suppose some servers might limit
;; the number of connections as well.)
;;
;; It would be nice if, instead of having to decide whether to post in the
;; foreground or background up front, we could arrange so that ^G during a
;; foreground post turned it into a background post (the zmacs mailer did
;; this, it was swell) but that's hard, because by the time a foreground post
;; has begun, the primary nntp connection is already wedged.  To do this, we'd
;; have to make the primary connection be the posting connection (the one that
;; will get nuked when the post completes) and install a new primary
;; connection.  Actually I guess that's not so hard after all.

(require 'nntp)

;; It probably wouldn't be too hard to make this work; 
;; if you do it send me the changes.
(if (string-match "flee" nntp-version)
    (error "nntp-post-bg doesn't work with flee's version of nntp.el"))

(defvar nntp-post-background 'query
  "If t, each post will make a new connection to the NNTP server.
This means that you don't have to wait for the post to complete to
continue reading articles.  If nil, posts will be made on the same
connection.  If any other value, you will be asked.")

(or (fboundp 'buffer-disable-undo)
    (fset 'buffer-disable-undo 'buffer-flush-undo))

(defun nntp-open-server-internal (host &optional service)
  "Open connection to news server on HOST by SERVICE (default is nntp)."
  (save-excursion
    ;; Use TCP/IP stream emulation package if needed.
    (or (fboundp 'open-network-stream)
	(require 'tcp))
    ;; Initialize communication buffer.
    ;; jwz: changed to use generate-new-buffer to support multiple connections.
    (if (and nntp-server-buffer (buffer-name nntp-server-buffer))
	(kill-buffer nntp-server-buffer))
    (setq nntp-server-buffer (generate-new-buffer " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    (setq nntp-server-process
	  (open-network-stream "nntpd" (current-buffer)
			       host (or service "nntp")))
    (setq nntp-server-name host)
    ;; It is possible to change kanji-fileio-code in this hook.
    (run-hooks 'nntp-server-hook)
    ;; Kill this process without complaint when exiting Emacs.
    (process-kill-without-query nntp-server-process)
    ;; Return the server process.
    nntp-server-process
    ))

(defvar nntp-post-bg-state) ;local to buffer of bg process
(defvar nntp-post-bg-source) ;the buffer of the message to post (modified)
(defvar background-post-queue nil) ;pending procs

(defun nntp-request-post ()
  "Post a new news article from the text of the current buffer."
  (cond
   ((or (eq nntp-post-background t)
	(and nntp-post-background (y-or-n-p "Post in background? ")))
    (let ((nntp-status-string nil)
	  (nntp-server-process nil)
	  (nntp-server-buffer nil)
	  (message (current-buffer)))
      (message "Opening NNTP connection for posting...")
      (or (nntp-open-server gnus-nntp-server gnus-nntp-service)
	  (error "couldn't open new NNTP connection for posting."))
      (set-process-filter nntp-server-process 'nntp-post-bg-filter)
      (message "Opening NNTP connection for posting...  encoding text...")
      (save-excursion
	(set-buffer (generate-new-buffer " *bg-post-text*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))
	(insert-buffer message)
	(nntp-encode-text)
	(setq message (current-buffer)))
      (save-excursion
	(set-buffer message)
	(rename-buffer (let ((b (generate-new-buffer " *bg-post*")))
			 (prog1 (buffer-name b) (kill-buffer b)))))
      (save-excursion
	(set-buffer nntp-server-buffer)
	(set (make-local-variable 'nntp-post-bg-state) 'post)
	(set (make-local-variable 'nntp-post-bg-source) message)
	(setq background-post-queue
	      (cons nntp-server-process background-post-queue))
	(nntp-send-command nil "POST"))
      (message "Posting will proceed in background.")))
   (t
    (message "NNTP: awaiting POST connection...")
    (if (nntp-send-command "^[23].*\r$" "POST")
	(progn
	  (message "NNTP: got POST connection; encoding text...")
	  (nntp-encode-text)
	  (message "NNTP: got POST connection; sending text...")
	  (nntp-send-region-to-server (point-min) (point-max))
	  ;; 1.2a NNTP's post command is buggy. "^M" (\r) is not
	  ;;  appended to end of the status message.
	  (message "NNTP: text sent; awaiting response...")
	  (prog1
	      (nntp-wait-for-response "^[23].*$")
	    (message "NNTP: post complete.")))))))

(defun nntp-post-bg-error (proc error)
  (let ((buffer (process-buffer proc)))
    (save-excursion
      (set-buffer buffer)
      (setq nntp-post-bg-state 'error)
      (kill-buffer nntp-post-bg-source)
      (delete-process proc)
      (kill-buffer buffer)))
  (setq background-post-queue (delq proc background-post-queue))
  (if error (error error)))

(defun nntp-post-bg-filter (proc string)
  (save-excursion
    (let ((buffer (process-buffer proc)))
      (set-buffer buffer)
      (goto-char (point-max))
      (insert string)
      (cond
       ((eq nntp-post-bg-state 'post)
	;; have sent POST command, waiting for response
	(cond ((not (re-search-backward "^[2345].*\r$")) ; no response yet
	       nil)
	      ((looking-at "[23]")	; ok, now send text
	       (erase-buffer)
	       (message "NNTP: got POST connection; sending text...")
	       ;; This synchronously sends the entire post.  It may be
	       ;; that this will block for an unacceptably long time on
	       ;; some servers, while waiting for the streams to drain.
	       ;; It might therefore be worthwhile to write an async
	       ;;version of nntp-send-region-to-server.
	       (save-excursion
		 (set-buffer nntp-post-bg-source)
		 (let ((nntp-server-process proc)
		       (nntp-server-buffer buffer)
		       (ok nil))
		   (unwind-protect
		       (progn
			 (nntp-send-region-to-server (point-min) (point-max))
			 (setq ok t))
		     (or ok (nntp-post-bg-error proc nil)))))
	       (message "NNTP: text sent; awaiting response in background.")
	       (setq nntp-post-bg-state 'sent))
	      (t
	       (nntp-post-bg-error proc
		(concat "POST failed: "
		 (buffer-substring (point) (progn (end-of-line) (point))))))))
       ((eq nntp-post-bg-state 'sent)
	;; have sent text of message, waiting for response
	(cond ((not (re-search-backward "^[2345].*\r$")) ; no response yet
	       nil)
	      ((looking-at "[23]")	; ok, we're done.
	       (erase-buffer)
	       (setq nntp-post-bg-state 'closed)
	       (set-buffer (other-buffer))
	       (delete-process proc)
	       (kill-buffer buffer)
	       (setq background-post-queue (delq proc background-post-queue))
	       (message "Background post complete!")
	       )
	      (t
	       (nntp-post-bg-error proc
		(concat "POST failed: "
		 (buffer-substring (point) (progn (end-of-line) (point))))))))
       (t
	(error "internal error: nntp-post-bg-state is %s"
	       nntp-post-bg-state))))))

(defun nntp-post-bg-maybe-warn-about-pending-posts ()
  (if (null background-post-queue)
      nil
    (or (if (cdr background-post-queue)
	    (yes-or-no-p
	     (format "There are %d background posts in progress; exit anyway? "
		     (length background-post-queue)))
	  (yes-or-no-p
	   "There is a background post in progress; exit anyway? "))
	(error ""))))

(if (fboundp 'add-hook)
    (add-hook 'kill-emacs-hook 'nntp-post-bg-maybe-warn-about-pending-posts))

(provide 'nntp-post-bg)
