;;; nntp.el --- GNUS interface to NNTP servers

;;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.
;;;
;; Author: Felix Lee <flee@cse.psu.edu>
;; Version: !Id: nntp.el,v 1.10 1993/02/04 18:23:39 flee Exp !
;; Modified by jwz.

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

;;; Code:

(require 'gnus)		; for the (poorly named) 'nntp-' accessor macros.
(require 'chat)
(or (fboundp 'open-network-stream) (require 'tcp))

(defvar nntp/rcs-revision (purecopy "!Revision: 1.10.3 !"))

(defvar nntp/default-nntp-port (purecopy "nntp")
  "The default tcp port to use for nntp connections.")

(defun nntp-last (x)
  "Returns the last link in the list LIST."
  (while (cdr x)
    (setq x (cdr x)))
  x)

;;;;;;;;;;;;;;;;
;;; NNTP state.

;; Right now, we're assuming we only talk to one NNTP server at a
;; time.  It might be nice to do multiple NNTP connections, but
;; there's no point in doing this from the bottom up.

;; (To handle multiple connections, you need to create connection
;; handles that you pass around.  Ideally, nnspool et al would be
;; just different types of connection handles.)

(defvar nntp/connection nil
  "The current NNTP connection.")

;; lemacs addition
(defvar nntp/group nil
  "The most-recently-selected NNTP group.")

;;; jwz: call this nntp-status-string instead of nntp/error-message because
;;; existing code uses that variable (in particular, nnspool.el and mhspool.el
;;; set it.)
(defvar nntp-status-string nil
  "The error message from the last NNTP command.  'nil if no error.
Don't use this, call the function `nntp-status-message' instead.")

(defvar nntp/can-xover t
  "Does this server understand the XOVER command?  (Computed.)")

;; lemacs addition
(defvar nntp/inhibit-xover nil
  "If you have XOVER but it doesn't work, set this to t.")

;;;;;;;;;;;;;;;;
;;; The GNUS interface.

;; These are the symbols that GNUS knows about and expects.

;; The interaction between GNUS and nntp.el (or nnspool.el) is a
;; little messy and not particularly well defined.

(defvar nntp-version
  (purecopy
   (concat "flee/nntp/Lucid " (substring nntp/rcs-revision 11 -2))))

(defvar nntp-server-buffer nil
  "Buffer that GNUS looks at when it wants data.")

;; lemacs addition
(defvar nntp-authinfo-string nil
  "*String sent to NNTP server if the connection fails.")

(defun nntp-open-server (host service)
  "Start a connection to the given HOST and SERVICE.  Returns true
if successful."
  ;; XXX already open?
  (or service (setq service nntp/default-nntp-port))
  (setq nntp-status-string nil)
  (setq nntp/can-xover (not nntp/inhibit-xover))  ; lemacs change
  (setq nntp-server-buffer (generate-new-buffer "*nntp*"))
  ;;(buffer-flush-undo nntp-server-buffer)
  (buffer-disable-undo nntp-server-buffer)
  (setq nntp/group nil) ; lemacs addition
  (setq nntp/connection
	(open-network-stream "nntp" nntp-server-buffer host service))
  (set-process-sentinel nntp/connection 'nntp/sentinel)
  (process-kill-without-query nntp/connection)

  (let ((code (nntp/response)))

    ;; lemacs addition: with some INN servers, we need to do this to convince
    ;; it to behave in the conventional way and not assume we want to do a
    ;; batch transfer of news.  With older NNTP servers, this will provoke a
    ;; "500 unknown command", which we can simply ignore.
    (nntp/command "MODE READER")
    (nntp/response) ; ignored.

    (or (eq code 200) (eq code 201)
	;; lemacs change: if it fails, send AUTHINFO and try once more.
	(and nntp-authinfo-string
	     (progn
	       (nntp/command "AUTHINFO" nntp-authinfo-string)
	       (setq code (nntp/response))
	       (or (eq code 200) (eq code 201)))))))
	       

(defun nntp-server-opened ()
  "Are we currently connected?"
  ;; if the buffer has died on us, kill all the rest.
  (if (and nntp/connection
	   (or (null nntp-server-buffer)
	       (null (buffer-name nntp-server-buffer))))
      (nntp-close-server))
  (and nntp/connection
       (memq (process-status nntp/connection) '(run open))))
;; XXX should we add stopped to this list?

(defun nntp-close-server ()
  "Terminate the connection.  Returns nothing."
  (let ((proc nntp/connection)
	(buffer nntp-server-buffer))
    (setq nntp/connection nil)
    (setq nntp/group nil) ; lemacs addition
    (setq nntp-server-buffer nil)
    (and proc (delete-process proc))
    (and buffer (kill-buffer buffer))))

(defun nntp-status-message ()
  "Returns the error message from the last NNTP request."
  ;; jwz: return "" instead of "okay" just in case old code expected that.
  (or nntp-status-string ""))

(defun nntp-request-list ()
  "Retrieve the list of newsgroups into 'nntp-server-buffer.
Returns true if successful."
  (nntp/command "LIST")
  (if (eq (nntp/response) 215)
      ;; We don't do any text format conversion here.  It's wasted
      ;; effort, since the text needs to be parsed by GNUS anyway.
      (nntp/wait-for-text)))

(defun nntp-request-group (group)
  "Select group GROUP.  Returns true if successful."
  (nntp/command "GROUP" group)
  (if (not (eq (nntp/response) 211))
      nil
    (setq nntp/group group) ; lemacs addition
    t))

(defun nntp-request-article (id)
  "Retrieve article ID (either a number or a message-id) into
'nntp-server-buffer.  Returns true if successful."
  (nntp/command "ARTICLE" id)
  (if (eq (nntp/response) 220)
      (nntp/get-text)))

(defun nntp-request-post ()
  "Modify and post the current buffer.  Returns true if successful."
  ;; The trick here is we want to make sure the conversation is in a
  ;; sane state even if we're interrupted in middle of transmission.
  ;; Right now, we just prematurely terminate the posting.  While this
  ;; isn't ideal, it's better than continually adding junk to the end.
  ;; The problem is NNTP doesn't let you abort a posting.
  ;; XXX A better approach is to open a new connection for posting,
  ;; but this is going to be slower, unless you anticipate the user by
  ;; opening the connection early.
  (nntp/command "POST")
  (if (eq (nntp/response) 340)
      (let ( (finished nil) )
	(unwind-protect
	    (progn
	      (nntp/unix-to-smtp-text)
	      (process-send-region nntp/connection (point-min) (point-max))
	      (setq finished t)
	      (eq (nntp/response) 240))
	  (or finished
	      (process-send-string nntp/connection "\r\n.\r\n")
	      nil)))))

;; lemacs: ckd addition to support by-ID retrieval
(defun nntp-retrieve-headers-by-id (messageid)
  "Returns the header data for MESSAGE-ID.
MESSAGE-ID is a string like \"<12345@foo.com>\"."
  (and messageid
       (let ((result nil))
	 (message "NNTP: retrieving headers...")
	 ;; ckd: strictly speaking, nntp/headers wants a sequence but the
	 ;; underlying NNTP command (HEAD) doesn't actually care, so this
	 ;; still works.
	 (setq result (nntp/headers messageid))
	 (message "NNTP: retrieving headers...done")
	 result)))

(defun nntp-retrieve-headers (sequence)
  "Returns the header data for SEQUENCE in the current group.
SEQUENCE is a sorted list of article numbers.
XXX describe the return value."
  (and sequence
       (let ((result nil))
	 (message "NNTP: retrieving headers...")
	 (if nntp/can-xover
	     (setq result (nntp/try-xover sequence)))
	 (if (not nntp/can-xover)
  	     (setq result (nntp/headers sequence)))
	 (message "NNTP: retrieving headers...done")
	 result)))

;;;;;;;;;;;;;;;;
;;; Talking to the NNTP server.

(defun nntp/sentinel (proc delta)
  (or (nntp-server-opened)
      (error "NNTP connection closed.")))

(defun nntp/clear ()
  ;; XXX This resynchronization is imperfect, but is probably good
  ;; enough for normal use.
  (chat/delete-pending-data nntp/connection))

(defun nntp/command (&rest strings)
  "Start a new NNTP command."
  (nntp/clear)
  (process-send-string
   nntp/connection
   (concat (mapconcat 'identity strings " ") "\r\n")))

;;;;;;;;;;;;;;;;
;;; Reading from the NNTP server.

;; This is almost 4x faster than (string-to-int (buffer-substring ... ))
;; primarily because of garbage collection.  -jwz
(defmacro nntp/read-integer (&optional point move-p)
  (` ((, (if move-p 'progn 'save-excursion))
      (,@ (if point (list (list 'goto-char point))))
      (if (and (<= (following-char) ?9)
	       (>= (following-char) ?0))
	  (read (current-buffer))
	0))))

(defun nntp/response ()
  "Wait for an NNTP response and return the response code.  Also sets
'nntp-status-string."
  ;; XXX Emacs 18.xx has a bug that turns undo back on after a gc, so
  ;; we continually flush undo here.
  ;;(buffer-flush-undo nntp-server-buffer)
  (chat/with-data-until-string "\n" nntp/connection
    (let ((code (nntp/read-integer (point-min))))
      ;; Codes 400 and up are error conditions.
      (setq nntp-status-string
	    (and (<= 400 code)
		 (buffer-substring (+ (point-min) 4) (- (point-max) 2))))
      code)))

(defun nntp/wait-for-text ()
  "Wait for an NNTP text response.  Returns true."
  (chat/wait-for-dot-crlf nntp/connection))

(defun nntp/get-text ()
  "Wait for an NNTP text response and convert it to Unix text format.
Returns true."
  (nntp/wait-for-text)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (nntp/smtp-to-unix-text))
  t)

;;;;;;;;;;;;;;;;
;;; Handling the funny dot-CRLF text format used by SMTP/NNTP.

(defun nntp/smtp-to-unix-text ()
  "Convert the current buffer from SMTP text format to Unix text
format.  Modifies point.  Returns nothing."
  (goto-char (point-min))
  (while (not (eobp))
    (if (eq (following-char) ?.)
	(delete-char 1))
    (end-of-line)
    (if (eq (preceding-char) ?\r)
	(delete-char -1))
    (forward-char))
  ;; Delete the last line, which had the dot-crlf terminator.
  (backward-char)
  (if (eq (preceding-char) ?\n)
      (delete-char 1))
  )

(defun nntp/unix-to-smtp-text ()
  "Convert the current buffer form Unix text format to SMTP text
format.  Modifies point.  Returns nothing."
  (goto-char (point-min))
  (while (not (eobp))
    (if (eq (following-char) ?.)
	(insert ?.))
    (end-of-line)
    (insert ?\r)
    (forward-line))
  ;; Add the terminator, but first insert a CRLF if necessary.
  (or (bobp)
      (eq (preceding-char) ?\n)
      (insert "\r\n"))
  (insert ".\r\n"))

;;;;;;;;;;;;;;;;
;;; Fetch headers using XOVER.

;; XXX We could probably try splitting a sequence into segments and
;; sending multiple XOVER commands, one for each segment.  However,
;; this is a little more expensive for the news server to process, and
;; mostly just reduces network traffic.  There isn't much difference
;; in response, unless you're in the habit of leaving 100+ article
;; gaps.  A couple hundred extra overview lines are unnoticeable on a
;; Sun SLC.

;; XXX In general, maybe we should have a gap threshhold: if a gap is
;; larger than N, split it into two XOVER requests, but the actual
;; tradeoffs are more complex than that.  This is really a flaw in
;; XOVER; you should be able to give XOVER a monotonically increasing
;; sequence of ranges, which is something that can be processed
;; efficiently.

;; XXX There's an unhappy synchronization problem here with C News.
;; The bounds in the active file are updated before the overview data
;; is updated, which may not happen until minutes later.  If you read
;; the active file and enter a newsgroup soon after it receives new
;; articles, then the overview fetch will leave out the new articles.
;; GNUS will wrongly conclude that the articles don't exist, mark them
;; as read, and you'll never see them.

(defun nntp/try-xover (sequence)
  "Try using the XOVER command to retrieve headers."
  (let ((lo (car sequence))
	(hi (car (nntp-last sequence))))
    (nntp/command "XOVER" (concat (int-to-string lo) "-" (int-to-string hi)))
    (if (eq (nntp/response) 224)
	(chat/with-data-until-dot-crlf nntp/connection
	  (nov/parse sequence))
      (setq nntp/can-xover nil)
      nil)))

;;;;;;;;;;;;;;;;
;;; News overview parsing.

;; XXX This section isn't really nntp-specific.  It probably could be
;; a separate module by itself.

;; Small changes to this code can have large impact on performance.

;; You'd think that using skip-chars-forward would be faster than
;; search-forward, but for some reason it ends up marginally slower.
;; I suspect it's because the setup overhead for both is about the
;; same, but the inner loop for search-forward is much more carefully
;; coded.

(defmacro nov/skip-field ()
  '(search-forward "\t" eol 'end))

(defmacro nov/field ()
  '(buffer-substring
    (point)
    (progn (nov/skip-field) (1- (point)))))

(defun nov/parse (sequence)
  "Parse the news overview data in the current buffer, and return a
list of headers that match SEQUENCE (see 'nntp-retrieve-headers)."
  (let ( (number nil)
	 (header nil)
	 (headers nil)
	 (eol nil) )
    (goto-char (point-min))
    (while (and sequence (not (eobp)))
      (setq number (nntp/read-integer nil t))

      ;; INN can report XOVER information *if* it is set up to do so.
      ;; However, if it is not configured to do so, instead of replying
      ;; to XOVER with some status code that we can detect, it responds
      ;; with 224.  However, the data that it returns is useless to us:
      ;; it's just a list of article numbers, with no other fields
      ;; present.
      ;;
      ;; So, if after reading the number, the next character is not a
      ;; tab, we must be in this state.  At that point, give up on XOVER
      ;; and return nil.
      ;;
      ;; However, a working XOVER server will return an empty response
      ;; for articles which don't exist - compare to the above-bug, which
      ;; returns a line containing only an article number.  Be careful
      ;; not to turn off XOVER in that case.
      ;;
      ;; ckd says that this bug is fixed in INN 1.4 and later: nnrpd
      ;; will generate XOVER information the fly if there is no overview
      ;; file.
      ;;
      (if (not (= (following-char) ?\t))
	  (if (= (following-char) ?.)
	      ;; EXCEPT if the next character is "." which means we are at
	      ;; the end of the XOVER data.  In that case we probably tried
	      ;; to get a cancelled article.  Clear the sequence and exit.--ckd
	      (progn
		(setq sequence nil)
 		(goto-char (point-max)))
	    ;; We're losing.
	    (setq sequence nil
		  headers nil
		  nntp/can-xover nil)
	    (goto-char (point-max))
	    )

	;; else, the XOVER data appears to be for real.

	(while (and sequence (< (car sequence) number))
	  (setq sequence (cdr sequence)))
	(if (and sequence (eq number (car sequence)))
	    (progn
	      (setq sequence (cdr sequence))
	      (save-excursion
		(end-of-line)
		(setq eol (point)))
	      ;; header: [num subject from xref lines date id refs]
	      ;; overview: [num subject from date id refs lines chars misc]
	      (setq header (make-vector 8 nil))
	      (nntp-set-header-number	  header number)
	      (forward-char)		  ; move past the "\t"
	      (nntp-set-header-subject	  header (nov/field))
	      (nntp-set-header-from	  header (nov/field))
	      (nntp-set-header-date	  header (nov/field))
	      (nntp-set-header-id	  header (nov/field))
	      (nntp-set-header-references header (nov/field))
	      (nov/skip-field)
	      ;; #### this could benefit from using nntp/read-integer
	      (nntp-set-header-lines	  header (string-to-int (nov/field)))
	      (backward-char)
	      (if (search-forward "\txref: " eol t)
		  (nntp-set-header-xref	  header (nov/field)))
	      (setq headers (cons header headers))
	      ))
	(forward-line)
	)
      )
    (setq headers (nreverse headers))
    headers))

;;;;;;;;;;;;;;;;
;;; A workaround for missing Xrefs in the overview data.

;(defun nntp/add-to-hook (hook-name value)
;  (let ((hook nil))
;    (if (boundp hook-name)
;	(setq hook (symbol-value hook-name)))
;    (if (or (subrp hook)
;	    (and hook (symbolp hook))
;	    (and (listp hook) (eq (car hook) 'lambda)))
;	(setq hook (list hook)))
;    (or (memq value hook)
;	(setq hook (cons value hook)))
;    (set hook-name hook)))

;(nntp/add-to-hook
; 'gnus-Article-prepare-hook
; 'nntp/article-get-xrefs)

(add-hook 'gnus-article-prepare-hook 'nntp/article-get-xrefs)


(defvar gnus-current-headers nil)	; from gnus.el

(defun nntp/article-get-xrefs ()
  "Fill in the Xref value in 'gnus-current-headers, if necessary.
This is meant to be called in 'gnus-Article-prepare-hook."
  (or gnus-digest-mode
      (nntp-header-xref gnus-current-headers)
      (let ((case-fold-search t))       ;lemacs, was nil
	(goto-char (point-min))
	(search-forward "\n\n" nil 'end)
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (goto-char (point-min))
	  (if (or (and (eq (downcase (following-char)) ?x)
		       (looking-at "Xref:"))
		  (search-forward "\nXref:" nil t))
	      (progn
		(goto-char (match-end 0))
		(forward-char)
		(aset gnus-current-headers 3
		      (buffer-substring
		       (point) (progn (end-of-line) (point))))
		))))))

;;;;;;;;;;;;;;;;
;;; Fetch headers using HEAD.

(defun nntp/headers (sequence)
  (nntp/clear)
  (nntp/send-head-requests sequence)
  (nntp/parse-headers sequence))

(defun nntp/send-head-requests (sequence)
  (message "NNTP: requesting headers...")
  (let ((L (length sequence))
	(count 0))
    (while sequence
      (process-send-string
       nntp/connection
       (concat "HEAD " (car sequence) "\r\n"))
      (if (= 0 (% count 5000))
	  (gnus-lazy-message "NNTP: requesting headers... %d%%" (/ count L)))

      ;; lemacs change: in order to avoid a potential deadlock, it is necessary
      ;; to synchronize with the server occasionally.  At first I thought to do
      ;; this by ressurecting the `nntp-maximum-request' variable, but that
      ;; seems not to be enough.  Consider the following situation:
      ;; - send N "head" requests
      ;; - wait for a reply; get a bunch of lines back for the first N/2
      ;;   requests.  As soon as no more input is available (because the
      ;;   the server has momentarily stopped sending for whatever reason)
      ;;   we continue
      ;; - send N more "head" requests
      ;; - get back another N/2 responses.
      ;; In this way, we can easily fill up the buffer between us and the
      ;; server, and get a deadlock, no matter what the value of
      ;; `nntp-maximum-request' so long as it is greater than one.
      ;;
      ;; So, after each HEAD request, we wait for a reply for that request.
      ;; This stinks.  It would be better to send N head requests, and then
      ;; block waiting for N responses to come in.  However, the current
      ;; structure of this code (in particular the implementation of
      ;; nntp/parse-headers, using chat/with-data-until-string) makes that
      ;; tricky.
      ;;
      (accept-process-output nntp/connection)

      (setq count (+ count 100))
      (setq sequence (cdr sequence))))
  )

(defun nntp/parse-headers (sequence)
  (message "NNTP: parsing headers...")
  (let ((headers nil)
	(code nil)
	(L (length sequence))
	(count 0))
    (while sequence
      (chat/with-data-until-string "\n" nntp/connection
	(setq code (nntp/read-integer (point-min))))
      (if (eq code 221)
	  (chat/with-data-until-dot-crlf nntp/connection
	    (setq headers (cons (nntp/parse-header (car sequence)) headers)))
	(chat/with-buffer-of nntp/connection		; jwz: added this
          (forward-line)))
      (if (= 0 (% count 5000))
	  (gnus-lazy-message "NNTP: parsing headers... %d%%" (/ count L)))
      (setq count (+ count 100))
      (setq sequence (cdr sequence)))
    (nreverse headers)))

;;; #### should this be inline?
(defun nntp/header-value ()
  (goto-char (match-end 0))
  (skip-chars-forward "\t ")
  (buffer-substring
   (point)
   (progn
     (while
	 (progn
	   (end-of-line)
	   (if (eq (preceding-char) ?\r)
	       (delete-char -1))
	   (forward-char)
	   (memq (following-char) '(?\t ? )))
       (delete-char -1)
       (delete-char 1)
       (insert ? ))
     (1- (point))))
  )

(defun nntp/parse-header (number)
  (let ((header (make-vector 8 nil))
	(case-fold-search t)
	char)
    ;; The old nntp.el used to always use 0 as the message number of
    ;; articles which were requested by message-id.  It might make more
    ;; sense to put the message-id in there, but it breaks things.  --jwz
    (aset header 0 (if (numberp number) number 0))

    (aset header 4 0)
    (while (not (eobp))
      ;; header: [num subject from xref lines date id refs]
      (if (not (looking-at "subject:\\|from:\\|xref:\\|lines:\\|date:\\|message-id:\\|references:"))
	  (forward-line)
	(setq char (downcase (following-char))) ; lemacs addition
	(cond
	 ((eq char ?s)
	  (nntp-set-header-subject header (nntp/header-value)))
	 ((eq char ?f)
	  (nntp-set-header-from header (nntp/header-value)))
	 ((eq char ?x)
	  (nntp-set-header-xref header (nntp/header-value)))
	 ((eq char ?l)
	  ;; #### this could benefit from using nntp/read-integer
	  (nntp-set-header-lines header (string-to-int (nntp/header-value))))
	 ((eq char ?d)
	  (nntp-set-header-date header (nntp/header-value)))
	 ((eq char ?m)
	  (nntp-set-header-id header (nntp/header-value)))
	 ((eq char ?r)
	  (nntp-set-header-references header (nntp/header-value)))
	 ))
      )
    ;; lemacs addition: must have a subject and sender.
    (or (nntp-header-subject header) (nntp-set-header-subject header ""))
    (or (nntp-header-from header) (nntp-set-header-from header ""))

    ;; lemacs addition: if this article was requested by message id, try to
    ;; figure out what its article number in the current group is.  This can
    ;; only succeed if the article is a crosspost, or if this NNTP server adds
    ;; an "Xref:" field to all articles (some do.)  In any event, if this can
    ;; succeed, it returns a more meaningful result, so give it a try.  The
    ;; consing and funcalls in here are not a big deal because the only time
    ;; articles are retrieved by message-id is when we're only getting a
    ;; single article.
    (and (not (numberp number))
	 (stringp (nntp-header-xref header))
	 nntp/group
	 (string-match (concat "[ \t]"
			       (regexp-quote nntp/group)
			       ":\\([0-9]+\\)")
		       (nntp-header-xref header))
	 (setq number
	       (aset header 0 (car (read-from-string (nntp-header-xref header)
						     (match-beginning 1)
						     (match-end 1))))))

    ;; lemacs change from ckd: if the XPATH command is available, it will give
    ;; us a list of paths for the article.  This means even if it is only
    ;; posted to one group, and has no Xref, if that group is the current
    ;; group then we can get a useful article number.  XPATH response is in
    ;; the form '223 alt/foo/bar/350 alt/baz/ugh/1023'.  This serves as an
    ;; extra fallback for the 'Xref' trick above.  XXX this code will do the
    ;; wrong thing with alt.foo.bar if there also exists an alt.foo-bar group
    ;; (because it uses the group name's dots as regexp characters).  We
    ;; really should change . to / and use regexp-quote, but I haven't coded
    ;; that part yet -- ckd 930124
    ;;
    ;; jwz asks: what NNTP servers always provide Xref, and which provide
    ;; XPATH (and is that standardized at all) and is ther any overlap?
    ;;
;; Ok, I've disabled this because it breaks the world when -retrieve-headers
;; or -retrieve-headers-by-id are called with more than one message ID.  You
;; can't go and issue another command before you're done parsing the rest on
;; the queue because that throws all that data away!!  -jwz
;    (and (not (numberp number))
;	 (let ((code nil)
;	       (response-string nil))
;	   (and (= 0 (aref header 0))
;		nntp/group
;		(progn
;		  (nntp/command "XPATH" (aref header 6))
;		  ;; we can't use nntp/response since we need the text of
;		  ;; the line
;		  (chat/with-data-until-string
;		   "\n" nntp/connection
;		   (setq code (nntp/read-integer (point-min))
;			 response-string
;			 (concat " "
;				 (buffer-substring (+ (point-min) 4)
;						   (- (point-max) 2))
;				 " "))))
;		(eq code 223)
;		(string-match (concat " " nntp/group "/\\([0-9]+\\) ")
;			      response-string)
;		(aset header 0 (car (read-from-string response-string
;						      (match-beginning 1)
;						      (match-end 1)))))))
    header))

;;; ADDED for gnus3.15

(defun nntp-request-list-newsgroups ()
  "List newsgroups (defined in NNTP2)."
  (nntp/command "LIST NEWSGROUPS")
  (if (eq (nntp/response) 215)
      (nntp/wait-for-text)))

(defun nntp-request-list-distributions ()
  "List distributions (defined in NNTP2)."
  (nntp/command "LIST DISTRIBUTIONS")
  (if (eq (nntp/response) 215)
      (nntp/wait-for-text)))

(provide 'nntp)

;;; nntp.el ends here
