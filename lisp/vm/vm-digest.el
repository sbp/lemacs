;;; Support code for RFC934 digests
;;; Copyright (C) 1989, 1990 Kyle E. Jones
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

(require 'vm)

(defun vm-rfc934-char-stuff-region (start end)
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end) (re-search-forward "^-" end t))
      (replace-match "- -" t t)))
  (set-marker end nil))

(defun vm-rfc934-char-unstuff-region (start end)
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end) (re-search-forward "^- "  end t))
      (replace-match "" t t)
      (forward-char)))
  (set-marker end nil))

(defun vm-digestify-region (start end)
  (setq end (vm-marker end))
  (let ((separator-regexp (if (eq vm-folder-type 'mmdf)
			      "\n+\001\001\001\001\n\001\001\001\001"
			    "\n+\nFrom .*")))
    (save-excursion
      (vm-rfc934-char-stuff-region start end)
      (goto-char start)
      (insert-before-markers "------- Start of digest -------\n")
      (delete-region (point) (progn (forward-line) (point)))
      (while (re-search-forward separator-regexp end t)
	(replace-match "\n\n------------------------------\n" t nil))
      (goto-char end)
      (if (eq vm-folder-type 'mmdf)
	  (delete-region (point) (progn (forward-line -1) (point))))
      (insert-before-markers "------- End of digest -------\n")))
  (set-marker end nil))

(defun vm-burst-digest (&optional grotty-digest)
  "Burst the current message (a digest) into its individual messages.
The digest's messages are assimilated into the folder as new mail would be,
e.g. message grouping takes place and if you're not reading a message
you will be moved to the first new or unread message.

By default VM expects digests to be in the standard RFC 934 format.
A prefix will inveigle VM into coping with other digest formats,
with mixed results."
  (interactive "P")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-virtual-folder)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (message "Bursting %sdigest..." (if grotty-digest "(possible grotty) " ""))
  (let ((inhibit-quit t) start end reg-start leader trailer error-data
	(reg-end (vm-marker nil))
	(text-start (vm-marker nil))
	(buffer-read-only)
	(old-buffer-modified-p (buffer-modified-p))
	(m (car vm-message-pointer)))
    (save-excursion
      (vm-save-restriction
       (condition-case error-data
	   (progn
	     (widen)
	     (goto-char (point-max))
	     (setq start (point))
	     (insert-buffer-substring (current-buffer)
				      (vm-text-of (car vm-message-pointer))
				      (vm-text-end-of
				       (car vm-message-pointer)))
	     (if (not
		  (re-search-backward "\\(^-[^ ].*\n+\\|^-\n+\\)+" start t))
		 (error "final message separator not found")
	       (setq end (point-marker))
	       ;; Reverse searchs are funky.  The above expression simply
	       ;; will not match  more than one message separator despite
	       ;; the "1 or more" directive at the end.
	       ;; This will have to suffice.
	       (while
		   (and
		    (save-excursion
		      (re-search-backward "\\(^-[^ ].*\n+\\|^-\n+\\)+" start t)
		      (= end (match-end 0))))
		 (set-marker end (match-beginning 0))
		 (goto-char end))
	       (skip-chars-backward "\n")
	       (set-marker end (point))
	       (delete-region end (point-max)))
	     (goto-char start)
	     (if (not (re-search-forward "^-[^ ]" end t))
		 (error "first message separator not found")
	       (delete-region start (match-beginning 0)))
	     ;; Now that we know that the digest has the basics covered
	     ;; try to detect and fix any bogus message separators,
	     ;; if the user requested it.
	     (and grotty-digest (vm-fix-grotty-digest start (point-max)))
	     ;; Concoct suitable separator strings for the future messages.
	     (if (eq vm-folder-type 'mmdf)
		 (setq leader "\001\001\001\001\n"
		       trailer "\n\001\001\001\001\n")
	       (setq leader (concat "From " (vm-from-of m) " "
				    (current-time-string) "\n")
		     trailer "\n\n"))
	     (goto-char start)
	     (while (re-search-forward
		     "\\(\\(\n+\\)\\|\\(^\\)\\)\\(-[^ ].*\n+\\|-\n+\\)+"
		     end 0)
	       ;; delete message separator
	       (replace-match "" t t)
	       ;; stuff separator
	       (if (match-beginning 2)
		   (insert trailer))
	       (insert leader)
	       ;; Delete attribute headers so message will appear
	       ;; brand new to the user
	       (setq reg-start (point))
	       (save-excursion
		 (search-forward "\n\n" nil 0)
		 (set-marker text-start (point)))
	       (while (re-search-forward vm-attributes-header-regexp
					 text-start t)
		 (delete-region (match-beginning 0) (match-end 0)))
	       (if vm-berkeley-mail-compatibility
		   (progn
		     (goto-char reg-start)
		     (while (re-search-forward vm-berkeley-mail-status-header-regexp
					    text-start t)
			 (delete-region (match-beginning 0) (match-end 0)))))
	       ;; find end of message separator and unstuff the message
	       (goto-char reg-start)
	       (set-marker reg-end (if (re-search-forward "\n+-[^ ]" end 0)
				       (match-beginning 0)
				     (point)))
	       (vm-rfc934-char-unstuff-region reg-start reg-end)
	       (goto-char reg-end))
	     (goto-char end)
	     (insert trailer)
	     (set-marker end nil)
	     (set-marker reg-end nil)
	     (vm-clear-modification-flag-undos)
	     (vm-set-buffer-modified-p (buffer-modified-p))
	     (and vm-delete-after-bursting (vm-delete-message 1)))
	 (error (and start (delete-region start (point-max)))
		(set-buffer-modified-p old-buffer-modified-p)
		(if (memq (car error-data)
			  '(file-supersession buffer-file-locked))
		    (signal (car error-data) (cdr error-data)))
		(error "Malformed digest")))))
    (if (vm-assimilate-new-messages)
	(progn
	  (vm-emit-totals-blurb)
	  (or (vm-thoughtfully-select-message)
	      (vm-update-summary-and-mode-line))))))

;; This is a kludge.
;; We try to accomodate some of the prevalent styles of digest
;; out there, by converting them into rfc 934 conformant digests.
;; In this we can only be partially successful.  Such is life.
(defun vm-fix-grotty-digest (start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (forward-line 1)
      (while (re-search-forward "^-[^ ]" nil t)
	(goto-char (match-beginning 0))
	(if (not (catch 'real-separator
		   ;; expect reasonably long message separators
		   (if (not (looking-at "------------"))
		       (throw 'real-separator nil))
		   ;; expect message separators to be bracketed by blank lines
		   (if (/= (char-after (- (point) 2)) ?\n)
		       (throw 'real-separator nil))
		   (if (not (looking-at ".+\n\n"))
		       (throw 'real-separator nil))
		   t ))
	    (insert "- "))
	(forward-line 1)))))
