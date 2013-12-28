;; GNU Emacs and this file "rmail-kill.el", is distributed in the hope
;; that it will be useful, but WITHOUT ANY WARRANTY.  No author or
;; distributor accepts responsibility to anyone for the consequences
;; of using it or for whether it serves any particular purpose or
;; works at all, unless he says so in writing.  Refer to the GNU Emacs
;; General Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs and rmail-kill.el, but only under the conditions described in
;; the GNU Emacs General Public License.  A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you can
;; know your rights and responsibilities.  It should be in a file
;; named COPYING.  Among other things, the copyright notice and this
;; notice must be preserved on all copies.

(setq rmail-message-filter 'rmail-maybe-execute-message
      rmail-mode-hook '((lambda ()
			  (define-key rmail-mode-map "e" 'rmail-extract-rejected-message)
			  (define-key rmail-mode-map "b" 'rmail-beginning-of-message)
			  (define-key rmail-mode-map "K" 'rmail-execute-messages))))

;; a-list with each entry (rmail-field-name . pattern)
(defvar rmail-usual-suspects
  '(("subject" . "Smithsonian Astronomical Observatory")
    ("subject" . "MGR, Bellcore window manager, Part"))
  "An alist used to kill rmail messages based on regex matches to different fields.
The car of each entry is the name of a mail header, the cdr is a pattern.
Case is not significant.)

See also the documentation for rmail-maybe-execute-message and
rmail-execute-messages.")

(setq kill-emacs-hook 'maybe-book-some-suspects)

(defun maybe-book-some-suspects ()
  (save-window-excursion
    (find-file "~/.emacs")
    (goto-char (point-min))
    (re-search-forward "^(defvar rmail-usual-suspects$")
    (down-list 1)
    (backward-char 1)
    (if (not (equal rmail-usual-suspects
		    (save-excursion (read (current-buffer)))))
	(progn
	  (switch-to-buffer-other-window "SUSPECTS")
	  (erase-buffer)
	  (mapcar '(lambda (x) (print x (current-buffer)))
		  rmail-usual-suspects)
	  (set-buffer-modified-p nil)
	  (if (y-or-n-p "Save the usual suspects? ")
	      (progn
		(set-buffer ".emacs")
		(kill-sexp 1)
		(prin1 rmail-usual-suspects (get-buffer ".emacs"))
		(save-buffer)))))))

(defun rmail-maybe-execute-message (&optional suspects dont-move)
  "Kill the current message if it matches an entry in SUSPECTS.
SUSPECTS is alist of the form of rmail-usual-suspects (which see).
If the current message contains a mail header that matches pattern,
it is deleted.

This function can be used as a rmail-message-filter (which see)."
  (if (null suspects)
      (setq suspects rmail-usual-suspects))
  (while suspects
    (if (and (string-match (cdr (car suspects))
			   ;; if not such field, can never match
			   (or (mail-fetch-field (car (car suspects))) "$^"))
	     (not (rmail-message-deleted-p rmail-current-message)))
	(progn
	  (message "Deleted message %d" rmail-current-message)
	  (if dont-move
	      (rmail-delete-message)
	    (rmail-delete-forward))
	  (setq suspects nil))
      (setq suspects (cdr suspects)))))

(defun rmail-execute-messages (round-up-the-usual-suspects)
  "Kill some rmail messages based on regex matches to a kill-alist.
With a prefix arg, use rmail-usual-suspects as the kill-alist, otherwise
prompt for a field name."
  (interactive "P")
  (let ((scene-of-the-crime rmail-current-message)
	(alleged-perpetrator)
	(cuffed-all-suspects nil))
    (if round-up-the-usual-suspects
	(setq alleged-perpetrator rmail-usual-suspects)
      (let* ((weapon (rmail-get-current-header "Kill what field? (default Subject) " "Subject"))
	     (default-description (or (regexp-quote (mail-fetch-field weapon))
				      "some regex"))
	     (most-wanted-notice (format "Kill messages having a \"%s\" field matching? (default %s) "
					 weapon default-description))
	     (suspect-description (read-string-with-default most-wanted-notice default-description)))
	(setq alleged-perpetrator (list (cons weapon suspect-description)))
	(if (y-or-n-p "Add it to rmail-usual-suspects? ")
	    (setq rmail-usual-suspects (append alleged-perpetrator rmail-usual-suspects)))))

    (while (not cuffed-all-suspects)
      (rmail-maybe-execute-message alleged-perpetrator 'dont-move)
      ;;
      ;; rmail-next-undeleted-message returns a string when there are no more, but
      ;; we also want a chance to delete that last message...
      ;;
      (if (stringp alleged-perpetrator)
	  (setq cuffed-all-suspects t)
	(setq cuffed-all-suspects (rmail-next-undeleted-message 1))))

    (rmail-show-message scene-of-the-crime)
    (if (rmail-message-deleted-p rmail-current-message)
	(rmail-next-undeleted-message 1))
    (if (rmail-message-deleted-p rmail-current-message)
	(rmail-previous-undeleted-message 1))))

(defun rmail-get-current-header (prompt default)
  (save-excursion
    (let* ((end (progn (end-of-line) (point))))
      (beginning-of-line)
      (if (re-search-forward "^\\([^ \t]*\\):" end t)
	  (buffer-substring (match-beginning 1) (match-end 1))
	(read-string-with-default prompt default)))))

(defun read-string-with-default (prompt default)
  (let ((s (read-string prompt)))
    (if (string= s "") default s)))
