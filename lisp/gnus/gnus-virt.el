;;; gnus-virt.el --- framework for "virtual" newsgroups

;; Copyright (C) 1993 Jamie Zawinski <jwz@lucid.com>

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

;; The basic idea here is this: given a set of message identifiers (either
;; message-id strings, or newsgroup/message-number pairs), display a standard
;; GNUS *Summary* buffer in which all the usual commands "just work," as if
;; all of these articles appeared in the same newsgroup (even though they
;; don't, necessarily.)
;;
;; This file does not implement the method of getting the message identifiers,
;; but it implements the substrate necessary to navigate them.

;; This requires NNTP, because though GNUS makes it easy to define entirely
;; new access methods, it doesn't provide an easy way of subtly modifying the
;; behavior of article-access in ways that shouldn't require knowledge of the
;; underlying method.


(require 'gnus)
(require 'nntp)

(defvar gnus-virt-message-numbers nil
  "A vector mapping pseudo-message-id-numbers to message-id strings
or to conses of (\"newsgroup-name\" . \"article-number\").")

(or (fboundp 'gnus-virt-orig-nntp-request-article)
    (fset 'gnus-virt-orig-nntp-request-article
	  (symbol-function 'nntp-request-article)))

(defun nntp-request-article (id)
  "Select article by message ID (or number)."
  (if (and (numberp id) gnus-virt-message-numbers)
      (setq id (aref gnus-virt-message-numbers id)))
  (if (consp id)
      (progn
	(gnus-request-group (car id))
	(setq id (cdr id))))
  (gnus-virt-orig-nntp-request-article id))


(defun gnus-virt-cleanup ()
  (setq gnus-virt-message-numbers nil))

(add-hook 'gnus-exit-group-hook 'gnus-virt-cleanup)


(defun gnus-virt-canonicalize-message-id (id)
  "C-News has screwy notions about case sensitivity."
  (if (string-match "@[^@]+\\'" id)
      (concat (substring id 0 (match-beginning 0))
	      (downcase (substring id (match-beginning 0))))
    id))

(defun gnus-virt-retrieve-headers (sequence)
  "Return list of article headers specified by SEQUENCE of article id.
The article ids may be message-id strings, or conses of the form
  (\"newsgroup-name\" . \"article-number\").
The format of the returned list is
 `([NUMBER SUBJECT FROM XREF LINES DATE MESSAGE-ID REFERENCES] ...)'.
If there is no References: field, In-Reply-To: field is used instead.
Reader macros for the vector are defined as `nntp-header-FIELD'.
Writer macros for the vector are defined as `nntp-set-header-FIELD'."
  (setq gnus-virt-message-numbers (apply 'vector sequence))
  (let* ((nmessages (length gnus-virt-message-numbers))
	 (i 0)
	 (last-group nil)
	 (rest sequence)
	 (per-group-queue nil)
	 (mid nil)
	 (messages (make-vector nmessages nil))
	 ;; local function to snarf the headers from what's on
	 ;; per-group-queue and slightly munge the result.
	 (grab-headers
	  (function
	   (lambda ()
	     (if last-group (gnus-request-group last-group))
	     (setq per-group-queue (nreverse per-group-queue))
	     (let* ((new (gnus-retrieve-headers per-group-queue))
		    (new-rest new)
		    (ids-rest per-group-queue)
		    )
	       (while new-rest
		 ;; Find the appropriate slot in `messages' for this message.
		 ;; Note that message-IDs are not completely case insensitive!
		 ;; The NNTP server downcases the part after the "@" in the
		 ;; message ID on the "221" reply line, but doesn't touch the
		 ;; Message-ID: field in the headers, so the two don't match.
		 (while (not (equal (gnus-virt-canonicalize-message-id
				     (aref gnus-virt-message-numbers i))
				    (gnus-virt-canonicalize-message-id
				     (nntp-header-id (car new-rest)))))
		   (setq i (1+ i))
		   (if (>= i nmessages)
		       (error "couldn't find %s"
			      (nntp-header-id (car new-rest))))
		   )
		 (aset messages i (car new-rest))

		 ;; For newgroup/number pairs, add an entry to the Xref field
		 ;; if there's nothing there already (meaning it was a single
		 ;; post and not a crosspost.)  Since we'll be reading this in
		 ;; a virtual group, all posts are corossposts as far as GNUS
		 ;; is concerned.
		 (or (nntp-header-xref (car new-rest))
		     (nntp-set-header-xref
		      (car new-rest)
		      (concat (system-name) " " last-group ":"
			      (nntp-header-number (car new-rest)))))
		 ;; NNTP HEAD command run on message-ids returns 0 as message
		 ;; number; so store some unique number there, and store the
		 ;; the message descriptor in a variable.  (We don't just
		 ;; store the descriptor in the header because things expect
		 ;; it to be numeric, dammit...)
		 (nntp-set-header-number (car new-rest) i)
		 ;; Possibly replace conses of (group . n) with message-ids.
		 (aset gnus-virt-message-numbers i
		       (nntp-header-id (car new-rest)))
		 (setq ids-rest (cdr ids-rest)
		       new-rest (cdr new-rest)))
	       )
	     (setq last-group nil
		   per-group-queue nil)))))
    (while rest
      (setq mid (car rest))
       (cond ((consp mid)		; newsgroup/article-number pair
	     (if (equal last-group (car mid))
		 ;; the queue has other article-numbers in this group; add.
		 (setq per-group-queue (cons (cdr mid) per-group-queue))
	       ;; else the queue has article-numbers in another group, or
	       ;; it has raw message-ids.  Flush the queue, and restart it.
	       (if per-group-queue (funcall grab-headers))
	       (setq last-group (car mid)
		     per-group-queue (cons (cdr mid) nil))))

	    (t				; message-id
	     (if last-group
		 ;; the queue has article-numbers of some group on it;
		 ;; flush and restart it.
		 (funcall grab-headers))
	     ;; in this case the queue has message-ids (in no group.)
	     (setq per-group-queue (cons mid per-group-queue))))
      (setq rest (cdr rest)))
    ;;
    ;; at the end, if there is still stuff on the queue, flush it.
    (if per-group-queue (funcall grab-headers))
    ;;
    ;; Now notice messages we weren't able to get (why?) and flag them.
    (setq i 0)
    (while (< i nmessages)
      (if (null (aref messages i))
	  (let ((id (format "%s" (aref gnus-virt-message-numbers i))))
	    (aset messages i
		  (vector i id "EXPIRED?" nil 0 "" id nil))))
      (setq i (1+ i)))
    (append messages nil) ; coerce vector to list
    ))

(defun gnus-virt-select-newsgroup (name message-ids)
  "Select a \"virtual\" newsgroup consisting of the given message-ids.
The message ids may be actual message-id strings, or conses of the form
  (\"newsgroup-name\" . \"article-number\")."
  ;; much of this copied from gnus-select-newsgroup
  (gnus-start-news-server)
  (setq gnus-newsgroup-name name)
;;  (setq gnus-newsgroup-unreads ...)
  (setq gnus-newsgroup-unselected nil)

  ;; Get headers list.
  (setq gnus-newsgroup-headers (gnus-virt-retrieve-headers message-ids))

  (setq gnus-newsgroup-unreads
	(mapcar 'gnus-header-number gnus-newsgroup-headers))

  ;; ## do something about expired articles here?
  ;; ## do something about marked articles here?

  ;; First and last article in this newsgroup.
  (setq gnus-newsgroup-begin
	(if gnus-newsgroup-headers
	    (nntp-header-number (car gnus-newsgroup-headers))
	  0
	  ))
  (setq gnus-newsgroup-end
	(if gnus-newsgroup-headers
	    (nntp-header-number
	     (gnus-last-element gnus-newsgroup-headers))
	  0
	  ))
  ;; File name that an article was saved last.
  (setq gnus-newsgroup-last-rmail nil)
  (setq gnus-newsgroup-last-mail nil)
  (setq gnus-newsgroup-last-folder nil)
  (setq gnus-newsgroup-last-file nil)
  ;; Reset article pointer etc.
  (setq gnus-current-article nil)
  (setq gnus-current-headers nil)
  (setq gnus-current-history nil)
  (setq gnus-have-all-headers nil)
  (setq gnus-last-article nil)
  ;; Clear old hash tables for the variable gnus-newsgroup-headers.
  (gnus-clear-hashtables-for-newsgroup-headers)
  ;; GROUP is successfully selected.
  t
  )

(defun gnus-virt-summary-read-group (group message-ids &optional no-article)
  "Start reading news in a \"virtual\" newsgroup of the given message-ids.
The message ids may be actual message-id strings, or conses of the form
  (\"newsgroup-name\" . \"article-number\").
If NO-ARTICLE is non-nil, no article is selected initially."

  (setq message-ids (mapcar 'gnus-virt-canonicalize-message-id message-ids))

  ;; delete duplicate message ids
  (let ((rest message-ids))
    (while rest
      (if (member (car rest) (cdr rest))
	  (setcar rest nil))
      (setq rest (cdr rest)))
    (setq message-ids (delq nil message-ids)))

  ;; make there be a newsgroup to prevent error when quitting...
  (or (gnus-gethash group gnus-newsrc-hashtb)
      (gnus-sethash group (list group nil) gnus-newsrc-hashtb))

  ;; this is a real kludge; we need to cause gnus-virt-select-newsgroup to
  ;; be called instead of gnus-select-newsgroup so we temporarily redefine
  ;; it...  We could do this by advising gnus-select-newsgroup and using
  ;; some special bindings for communication, but that's not really much
  ;; less gross than this.
  (let ((old (symbol-function 'gnus-select-newsgroup)))
    (unwind-protect
	(progn
	  (fset 'gnus-select-newsgroup
		(function (lambda (group show-all)
			    (gnus-virt-select-newsgroup group message-ids))))
	  (gnus-summary-read-group group nil no-article))
      ;; protected
      (fset 'gnus-select-newsgroup old))))

;;; A simple user-level interface; this could be greatly improved.
;;; In fact, doing that is what all of the interesting problems here
;;; are about.

(defun gnus-virt-read-merged-groups (virtual-name newsgroup-names
				     &optional all no-article)
  "Read news in the given newsgroup as if they were one group.
VIRTUAL-NAME is the name to assign to this new group;
NEWSGROUP-NAMES is a list of the names of the newsgroups to combine.
If argument ALL is non-nil, already read articles become readable.
If optional argument NO-ARTICLE is non-nil, no article body is displayed."
  (let ((articles nil)
	(rest (reverse newsgroup-names)))
    (while rest
      (let* ((group (car rest))
	     (numbers (gnus-uncompress-sequence
		       (nthcdr 2 (gnus-gethash group
					       (if all
						   gnus-active-hashtb
						 gnus-unread-hashtb))))))
	(setq articles (nconc (mapcar '(lambda (x) (cons group x)) numbers)
			      articles)))
      (setq rest (cdr rest)))
    ;; ## We should compute gnus-newsgroup-unreads and gnus-newsgroup-marked
    ;;    as the union of the unread/marked articles of all of these groups.
    (gnus-virt-summary-read-group virtual-name articles no-article)))


(provide 'gnus-virt)
