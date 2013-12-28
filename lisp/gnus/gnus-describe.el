;;; -*- Mode: Emacs-Lisp -*-
;; From: lutzeb@cs.tu-berlin.de (Dirk Lutzebaeck)
;; Subject: Source: description of newsgroups via NNRP
;; Date: 08 Jan 93 15:02:08 GMT
;; 
;; Hi all,
;; 
;; The NNRP protocol (which is a variation of the NNTP protocol used in
;; the INN news administration package, I think) provides a command named
;; `XGTITLE'.  This command takes a newsgroup as an argument and returns
;; a short description of it. I made this available to GNUS, i.e. if you
;; press `i' in the group, subject or killed groups buffer it looks for a
;; newsgroup at the current point and prints the description in the
;; minibuffer.  A prefix `C-u i' asks for a newsgroup name. Another
;; command `I' in the group buffer gives you the description of all
;; available newsgroups of your NNRP server.

(define-key gnus-group-mode-map "i" 'gnus-group-describe)
(define-key gnus-group-mode-map "I" 'gnus-group-describe-all)
(define-key gnus-summary-mode-map "i" 'gnus-group-describe)
;(define-key gnus-browse-killed-mode-map "i" 'gnus-group-describe)


(defun gnus-group-describe (&optional group)
  "Describe current newsgroup.
If optional argument GROUP is non-nil ask for a newsgroup, otherwise
search around point for a newsgroup. If no newsgroup found take
current newsgroup."
  (interactive "P")
  (cond
   ((not group)
    (setq group (gnus-group-group-name))) ; search around point
   ((listp group)
    (setq group
	  (completing-read "Newsgroup: "
			   gnus-newsrc-assoc nil 'require-match))))
  (if (not group)
      (setq group gnus-newsgroup-name))	; otherwise set current newsgroup name
  (if (nntp-request-xgtitle group)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(if (> (point-max) (+ (point-min) 4))
	    (message (buffer-substring (point-min) (- (point-max) 5)))
	  (message "No description for %s." group)))
    (message "cannot find description for %s." group)))


(defun gnus-group-describe-all ()
  "Describe all newsgroups."
  (interactive)
  (switch-to-buffer (get-buffer-create " *GNUS newsgroup descriptions*"))
  (erase-buffer)
  (if (nntp-request-xgtitle "*")
      (insert-buffer-substring nntp-server-buffer))
  (goto-char (point-min)))


(defun nntp-request-xgtitle (group)
  "Get description of a group via XGTITLE NNRP-command."
  (nntp/command "XGTITLE" group)
  (if (eq (nntp/response) 282)
      (nntp/wait-for-text)))
