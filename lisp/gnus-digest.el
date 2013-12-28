;;; Undigestification commands for GNUS newsreader
;; Copyright (C) 1991 Jamie Zawinski
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


;; In some newsgroups, like comp.risks, every message is a digest of
;; other messages.  
;;
;; This is stupid.  Those digests should be exploded into individual messages
;; before being inserted in the USENET stream.  There is absolutely no benefit
;; to doing it any other way.
;;
;; GNUS used to read digests by invoking RMAIL on the message in such a way
;; that it would do the undigestification.  However, this has the extremely
;; bad side-effect that you have to use the RMAIL interface instead of the
;; GNUS interface.  GNUS and RMAIL do many things differently, and it's
;; horribly distracting to have to change gears when reading certain
;; newsgroups.
;;
;; This code makes GNUS understand digests directly.  The command
;; gnus-Subject-read-digest (bound to C-d) will expand the current message
;; as a digest.  The *Subject* buffer will be replaced with lines representing
;; the messages in the digest; the normal GNUS commands will work on the 
;; sub-messages of the digest, including kill files.
;;
;; Typing \\[gnus-Subject-exit] at the *Subject* buffer will replace the list
;; of messages in the digest with the list of (digest) messages in the
;; newsgroup.  Reading a digest is something like a recursive edit.
;;
;; INSTALLATION:
;;
;; In addition to loading this file, you must apply the following patch
;; to gnus.el.  This patch was made against version 3.13:
;;
;; ------------------------------------------------------------------------
;; *** /gnuemacs/lisp/gnus.el	Thu Oct 11 06:40:32 1990
;; --- gnus.el	Sat Feb  1 23:39:04 1992
;; ***************
;; *** 3748,3751 ****
;; --- 3748,3754 ----
;;   gnus-Exit-group-hook is called with no arguments if that value is non-nil."
;;     (interactive)
;; +   (if gnus-digest-mode
;; +       (gnus-unselect-digest-article)
;; +     ;; else
;;     (let ((updated nil)
;;   	(gnus-newsgroup-headers gnus-newsgroup-headers)
;; ***************
;; *** 3788,3792 ****
;;   	(bury-buffer gnus-Article-buffer))
;;       (gnus-configure-windows 'ExitNewsgroup)
;; !     (pop-to-buffer gnus-Group-buffer)))
;;   
;;   (defun gnus-Subject-quit ()
;; --- 3791,3795 ----
;;   	(bury-buffer gnus-Article-buffer))
;;       (gnus-configure-windows 'ExitNewsgroup)
;; !     (pop-to-buffer gnus-Group-buffer))))
;;   
;;   (defun gnus-Subject-quit ()
;; ***************
;; *** 3882,3885 ****
;; --- 3885,3890 ----
;;         ))
;;   
;; + (defvar gnus-digest-mode nil)
;; + 
;;   (defun gnus-Article-prepare (article &optional all-headers)
;;     "Prepare ARTICLE in Article mode buffer.
;; ***************
;; *** 3889,3893 ****
;;       (let ((buffer-read-only nil))
;;         (erase-buffer)
;; !       (if (gnus-request-article article)
;;   	  (progn
;;   	    ;; Prepare article buffer
;; --- 3894,3900 ----
;;       (let ((buffer-read-only nil))
;;         (erase-buffer)
;; !       (if (if gnus-digest-mode
;; ! 	      (gnus-request-digest-article article)
;; ! 	    (gnus-request-article article))
;;   	  (progn
;;   	    ;; Prepare article buffer
;; ***************
;; *** 4988,4991 ****
;; --- 4995,4999 ----
;;     (if (gnus-request-group group)
;;         (let ((articles nil))
;; + 	(gnus-digest-reset)
;;   	(setq gnus-newsgroup-name group)
;;   	(setq gnus-newsgroup-unreads
;; ------------------------------------------------------------------------
;;
;; I also suggest adding some variation of this code to your .emacs file:
;;
;;   (defvar gnus-digestified-newsgroups
;;     '("comp.risks" "comp.sys.ibm.pc.digest" "comp.sys.mac.digest"
;;       "sci.psychology.digest" "soc.human-nets" "soc.politics.arms-d"))
;;   
;;   (setq gnus-Select-article-hook
;;         '(lambda ()
;;            (or gnus-digest-mode ; don't do it if we're already doing it
;;                (if (string-match (mapconcat 'regexp-quote
;;                                             gnus-digestified-newsgroups
;;                                             "\\|")
;;                                  gnus-newsgroup-name)
;;                    (gnus-Subject-read-digest)))))
;;
;; IMPLEMENTATION:
;;
;;   Selecting a message as a digest copies the message to a temporary buffer,
;;   and parses it into sub-messages.  The message-serperators are removed,
;;   and dummy "Newsgroups" and "Message-ID" fields are inserted for each
;;   sub-message (so that followups and message-yanking works).  The variable
;;   gnus-newsgroup-headers is set to a vector of nntp-header structures
;;   corresponding to the sub-messages.  This makes the normal Subject-buffer
;;   generation (and commands) work.  The article-number of each of these
;;   new message descriptors is the buffer-index of the message in the 
;;   temporary buffer.  When GNUS is in digest-mode, gnus-Article-prepare
;;   will take the messages out of this buffer instead of calling 
;;   gnus-request-article.
;;
;;   Exiting a digest restores gnus-newsgroup-headers and related variables
;;   to their previous values, representing the newsgroup itself instead of
;;   the messages in one article of the newsgroup.
;;
;; TODO:
;;
;;   o  The `gnus-auto-select-next' functionality is disabled when reading
;;      a digest.  When you attempt to select the next message and there
;;      are no more messages in the digest, then digest-mode should be
;;      exited and the next (real) message selected.
;;
;;   o  Instead of changing the contents of the *Subject* buffer, I think
;;      there should be a seperate buffer forthe sub-message subjects.  It
;;      should be possible to have a four window display: Newsgroup list;
;;      Subject list (the digests); Subject list (the messages in the current
;;      digest); and Article (the current message in the current digest.)
;;
;;   o  Perhaps all of the messages in a newsgroup should be undigestified
;;      at once; that way, the Subject buffer would be filled with all of
;;      the messages, instead of all of the messages of one digest, followed
;;      by the digest list, followed by the messages of the next digest, etc.
;;

(require 'gnus)

(define-key gnus-Subject-mode-map "\C-d" 'gnus-Subject-read-digest)


(defun gnus-parse-digest-1 ()
  ;; The current buffer is assumed to contain a digest message.
  ;; This function returns a list of buffer-points (integers) which
  ;; are the starting points of the digestified sub-articles.
  ;; The buffer is modified, to remove the message-seperators, and to
  ;; insert fake Newsgroup: and Message-ID: headers for the sub-messages.
  ;; This doesn't do RFC-934 digests because comp.risks isn't one of them.
  (goto-char (point-min))
  (let ((case-fold-search t)
	newsgroups-header
	subject-header
	(message-id-tick 1)
	p)
    (search-forward "\n\n")
    (setq p (point))
    (save-restriction
      (narrow-to-region (point-min) p)
      (setq newsgroups-header (or (mail-fetch-field "Newsgroups")
				  gnus-newsgroup-name)
	    subject-header (or (mail-fetch-field "Subject")
			       (concat gnus-newsgroup-name " digest"))))
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    ;; What a repulsive hack this is...
    (forward-line -10)
    (if (re-search-forward "^End of.*Digest.*\n" nil t)
	(delete-region (match-beginning 0) (point-max)))
    (goto-char p)
    (let ((result (list (point-min))))
      (while (re-search-forward "^\\(---+\\|-\\)\n" nil t)
	(setq p (match-beginning 0))
	(skip-chars-forward "\n\r\t ")
	(delete-region p (point))
	(if (looking-at "[ \t\n\r]*\\'")
	    nil
	  (setq result (cons (point) result))
	  (insert "Newsgroups: " newsgroups-header "\n")
	  (insert "Message-ID: <" subject-header " message #"
		  (+ message-id-tick ?0) ">\n")
	  (setq message-id-tick (1+ message-id-tick))
	  ))
      ;;(if (eq (car result) (point-max)) (setq result (cdr result)))
      (nreverse result))))

(defvar gnus-digest-divisions) ; buffer-local in the digest source buffer

(defun gnus-parse-digest ()
  ;; workalike for nntp-retrieve-headers: returns a list of the form
  ;;  ([NUMBER SUBJECT FROM XREF LINES DATE MESSAGE-ID REFERENCES] ...)
  ;; for the sub-articles of a digest.  The number is the buffer-point
  ;; of the sub-message, rather than an NNTP message id.
  (let ((points (gnus-parse-digest-1))
	(case-fold-search t)
	(opm (point-max))
	(result nil))
    (make-local-variable 'gnus-digest-divisions)
    (setq gnus-digest-divisions points)
    (save-restriction
      (while points
	(let (point subject from xref lines date message-id references)
	  (widen)
	  (goto-char (setq point (car points)))
	  (narrow-to-region point (or (car (cdr points)) opm))
	  
	  ;; Mostly lifted from nntp-retrieve-headers: this is really
	  ;; inefficient.
	  (while (and (not (eobp))
		      (not (looking-at "\n"))) ; eoh
	    ;; Note that we accept ">From:" as well as "From:", since the
	    ;; boneheads who maintain comp.sys.mac.digest allow their digests
	    ;; to pass through some even-more-broken-than-sendmail gateway
	    ;; along the way, thus corrupting the From: field in every
	    ;; message.  Fuck me harder!
	    (if (looking-at "\\(>?From\\|Subject\\|Date\\|Lines\\|Xref\\|References\\|Message-ID\\):[ \t]+\\([^ \t\n]+.*\\)\r?$")
		(let ((s (buffer-substring (match-beginning 2) (match-end 2)))
		      (c (char-after (match-beginning 0))))
		  ;; We don't have to worry about letter case.
		  (cond ((char-equal c ?F)	;From:
			 (setq from s))
			((char-equal c ?>)	;>From (gag me with a chainsaw)
			 (setq from s))
			((char-equal c ?S)	;Subject:
			 (setq subject s))
			((char-equal c ?D)	;Date:
			 (setq date s))
			((char-equal c ?L)	;Lines:
			 (setq lines (string-to-int s)))
			((char-equal c ?X)	;Xref:
			 (setq xref s))
			((char-equal c ?R)	;References:
			 (setq references s))
			((char-equal c ?M)	;Message-ID:
			 (setq message-id s))
			)))
	    (forward-line 1))
	  (if (null subject) (setq subject "(None)"))
	  (if (null from) (setq from "(Unknown User)"))
	  (if (null message-id) (error "no message id?"))
	  (if (null lines) (setq lines (count-lines (point-min) (point-max))))
	  (setq result (cons (vector point subject from xref lines date
				     message-id references)
			     result)
		points (cdr points)))))
    (nreverse result)))

(defvar gnus-digest-save-state nil) ; ack pfffft.
(defvar gnus-digest-buffer nil)

(defun gnus-select-digest-article ()
  (if gnus-digest-save-state (error "already reading a digest"))
  (gnus-Subject-select-article)
  (gnus-Subject-show-all-headers)
  (if (not (and gnus-digest-buffer (buffer-name gnus-digest-buffer)))
      (setq gnus-digest-buffer (get-buffer-create " *gnus-digest-buffer*")))
  (save-excursion
    (set-buffer gnus-digest-buffer)
    (erase-buffer)
    ;; this contortion is because insert-buffer-contents can't be made
    ;; to grab text outside of the narrowed region.
    (save-excursion
      (save-restriction
	(set-buffer gnus-Article-buffer)
	(widen)
	(save-excursion
	  (set-buffer gnus-digest-buffer)
	  (insert-buffer gnus-Article-buffer))))
    (let ((header-data (gnus-parse-digest)))
      ;; I wish we didn't have to restore all of this crap, but we do...
      (setq gnus-digest-save-state (list gnus-newsgroup-unreads
					 gnus-newsgroup-marked
					 gnus-newsgroup-begin
					 gnus-newsgroup-headers
					 gnus-auto-select-next
					 (save-excursion
					   (set-buffer gnus-Subject-buffer)
					   (point))
					 ))
      (setq gnus-newsgroup-unreads
	      (mapcar (function (lambda (x) (nntp-header-number x)))
		      header-data)
	    gnus-newsgroup-marked nil
	    gnus-newsgroup-begin (car gnus-newsgroup-unreads)
	    gnus-newsgroup-end (gnus-last-element gnus-newsgroup-unreads)
	    gnus-newsgroup-headers header-data
	    gnus-auto-select-next nil  ; oh, foo.
	    )
      ;; Reset article pointer etc.
      (setq gnus-current-article nil)
      (setq gnus-current-headers nil)
      (setq gnus-current-history nil)
      (setq gnus-have-all-headers nil)
      (setq gnus-last-article nil)
      )))


(defun gnus-digest-reset ()
  (let (p)
    (if gnus-digest-save-state
	(setq gnus-newsgroup-unreads (nth 0 gnus-digest-save-state)
	      gnus-newsgroup-marked  (nth 1 gnus-digest-save-state)
	      gnus-newsgroup-begin   (nth 2 gnus-digest-save-state)
	      gnus-newsgroup-headers (nth 3 gnus-digest-save-state)
	      gnus-auto-select-next  (nth 4 gnus-digest-save-state)
	      p (nth 5 gnus-digest-save-state)
	      gnus-digest-save-state nil
	      gnus-digest-mode nil))
    p))


(defun gnus-unselect-digest-article ()
  (if (not gnus-digest-save-state) (error "not reading a digest"))
  (let ((p (gnus-digest-reset)))
    (gnus-Subject-exit t)
    ;; We have to adjust the point of Group mode buffer because the current
    ;; point was moved to the next unread newsgroup by exiting.
    (gnus-Subject-jump-to-group gnus-newsgroup-name)
    
    (gnus-Subject-setup-buffer)
    (run-hooks 'gnus-Select-group-hook)
    (gnus-Subject-prepare)
    (goto-char p)))


(defvar inside-select-digest nil) ; hands off

(defun gnus-Subject-read-digest ()
  "Read the current message as a digest.
The *Subject* buffer will be replaced with lines representing the messages
in the digest; the normal GNUS commands will work on the sub-messages of
the digest.  Typing \\[gnus-Subject-exit] at the *Subject* buffer will 
replace the list of messages in the digest with the list of (digest) 
messages in the newsgroup.  Reading a digest is something like a recursive
edit."
  (interactive)
  (if inside-select-digest
      nil
    (let ((inside-select-digest t))
  ;; most of this is copied from gnus-Subject-read-group.
  (gnus-select-digest-article)
  (gnus-Subject-setup-buffer)
  (run-hooks 'gnus-Select-group-hook)
  (gnus-Subject-prepare)
  (run-hooks 'gnus-Apply-kill-hook)
  (if (zerop (buffer-size)) (error "empty digest?"))
  (setq gnus-digest-mode t)
  ;; Hide conversation thread subtrees.  We cannot do this in
  ;; gnus-Subject-prepare-hook since kill processing may not
  ;; work with hidden articles.
  ;; ## Do any digest-groups include References: fields in the submessages?
  ;; ## I think not, but if they do, threading should work.
  (and gnus-show-threads
       gnus-thread-hide-subtree
       (gnus-Subject-hide-all-threads))
  ;; Show first unread article if requested.
  (goto-char (point-min))
  (if (and gnus-auto-select-first
	   (gnus-Subject-first-unread-article))
      ;; Window is configured automatically.
      ;; Current buffer may be changed as a result of hook
      ;; evaluation, especially by gnus-Subject-rmail-digest
      ;; command, so we should adjust cursor point carefully.
      (if (eq (current-buffer) (get-buffer gnus-Subject-buffer))
	  (progn
	    ;; Adjust cursor point.
	    (beginning-of-line)
	    (search-forward ":" nil t)))
    (gnus-configure-windows 'SelectNewsgroup)
    (pop-to-buffer gnus-Subject-buffer)
    (gnus-Subject-set-mode-line)
    ;; I sometime get confused with the old Article buffer.
    (if (get-buffer gnus-Article-buffer)
	(if (get-buffer-window gnus-Article-buffer)
	    (save-excursion
	      (set-buffer gnus-Article-buffer)
	      (let ((buffer-read-only nil))
		(erase-buffer)))
	  (kill-buffer gnus-Article-buffer)))
    ;; Adjust cursor point.
    (beginning-of-line)
    (search-forward ":" nil t))
  )))

(defun gnus-request-digest-article (article)
  ;; article is the article-number of the message, which in this case,
  ;; is a buffer-index into gnus-digest-buffer of the beginning of the
  ;; message.
  (save-excursion
    (set-buffer gnus-digest-buffer)
    (let ((rest gnus-digest-divisions)
	  next)
      (while (and rest (not next))
	(if (= (car rest) article)
	    (setq next (or (car (cdr rest)) (buffer-size))))
	(setq rest (cdr rest)))
      (or next (error "no digest data for %s" article))
      (goto-char next)
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (insert-buffer-substring gnus-digest-buffer article next)
      t)))

(provide 'gnus-digest)
