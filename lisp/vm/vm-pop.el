;;; Simple POP (RFC 1460) client for VM
;;; Copyright (C) 1993, 1994 Kyle E. Jones
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

;; Nothing fancy here.
;; Our goal is to drag the mail from the POP maildrop to the crash box.
;; just as if we were using movemail on a spool file.
(defun vm-pop-move-mail (source destination)
  (let ((process nil)
	(folder-type vm-folder-type)
	(save-password nil)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source nil)
			(wrong-number-of-arguments
			  (find-file-name-handler source)))))
	greeting timestamp n message-count
	host port auth user pass source-list process-buffer)
    (unwind-protect
	(catch 'done
	  (if handler
	      (throw 'done
		     (funcall handler 'vm-pop-move-mail source destination)))
	  ;; parse the maildrop
	  (setq source-list (vm-parse source "\\([^:]+\\):?")
		host (nth 0 source-list)
		port (nth 1 source-list)
		auth (nth 2 source-list)
		user (nth 3 source-list)
		pass (nth 4 source-list))
	  ;; carp if parts are missing
	  (if (null host)
	      (error "No host in POP maildrop specification, \"%s\""
		     source))
	  (if (null port)
	      (error "No port in POP maildrop specification, \"%s\""
		     source))
	  (if (string-match "^[0-9]+$" port)
	      (setq port (string-to-int port)))
	  (if (null auth)
	      (error
	       "No authentication method in POP maildrop specification, \"%s\""
	       source))
	  (if (null user)
	      (error "No user in POP maildrop specification, \"%s\""
		     source))
	  (if (null pass)
	      (error "No password in POP maildrop specification, \"%s\""
		     source))
	  (if (equal pass "*")
	      (progn
		(setq pass (car (cdr (assoc source vm-pop-passwords))))
		(if (null pass)
		    (setq pass
			  (vm-read-password
			   (format "POP password for %s: "
				   (vm-safe-popdrop-string source)))
			  save-password t))))
	  ;; get the trace buffer
	  (setq process-buffer
		(get-buffer-create (format "trace of POP session to %s" host)))
	  ;; clear the trace buffer of old output
	  (save-excursion
	    (set-buffer process-buffer)
	    (erase-buffer))
	  ;; open the connection to the server
	  (setq process (open-network-stream "POP" process-buffer host port))
	  (and (null process) (throw 'done nil))
	  (set-process-filter process 'vm-pop-process-filter)
	  (save-excursion
	    (set-buffer process-buffer)
	    (make-local-variable 'vm-pop-read-point)
	    (setq vm-pop-read-point (point-min)
		  vm-folder-type (or folder-type vm-default-folder-type))
	    (and (null (setq greeting (vm-pop-read-response process t)))
		 (throw 'done nil))
	    ;; authentication
	    (cond ((equal auth "pass")
		   (vm-pop-send-command process (format "USER %s" user))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil))
		   (vm-pop-send-command process (format "PASS %s" pass))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil)))
		  ((equal auth "rpop")
		   (vm-pop-send-command process (format "USER %s" user))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil))
		   (vm-pop-send-command process (format "RPOP %s" pass))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil)))
		  ((equal auth "apop")
		   (setq timestamp (vm-parse greeting "[^<]+\\(<[^>]+>\\)")
			 timestamp (car timestamp))
		   (if (null timestamp)
		       (progn
			 (goto-char (point-max))
		 (insert "<<< ooops, no timestamp found in greeting! >>>\n")
			 (throw 'done nil)))
		   (vm-pop-send-command
		    process
		    (format "APOP %s" (vm-pop-md5 (concat timestamp pass))))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil)))
		  (t (error "Don't know how to authenticate with %s" auth)))
	    ;; we're in.
	    ;; save the password if we read it from the user.
	    (if save-password
		(setq vm-pop-passwords (cons (list source pass)
					     vm-pop-passwords)))
	    ;; find out how many messages are in the box.
	    (vm-pop-send-command process "STAT")
	    (setq message-count (vm-pop-read-stat-response process))
	    ;; forget it if the command fails
	    ;; or if there are no messages present.
	    (if (or (null message-count)
		    (< message-count 1))
		(throw 'done nil))
	    ;; loop through the maildrop retrieving and deleting
	    ;; messages as we go.
	    (setq n 1)
	    (while (<= n message-count)
	      (vm-pop-send-command process (format "RETR %d" n))
	      (and (null (vm-pop-read-response process))
		   (throw 'done (not (equal n 1))))
	      (and (null (vm-pop-retrieve-to-crashbox process destination))
		   (throw 'done (not (equal n 1))))
	      (vm-pop-send-command process (format "DELE %d" n))
	      ;; DELE can't fail but Emacs or this code might
	      ;; blow a gasket and spew filth down the
	      ;; connection, so...
	      (and (null (vm-pop-read-response process))
		   (throw 'done (not (equal n 1))))
	      (vm-increment n))
	     t ))
      (if process
	  (save-excursion
	    (set-buffer (process-buffer process))
	    (vm-pop-send-command process "QUIT")
	    (vm-pop-read-response process)
	    (delete-process process))))))

(defun vm-pop-process-filter (process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)))

(defun vm-pop-send-command (process command)
  (goto-char (point-max))
  (insert command "\r\n")
  (setq vm-pop-read-point (point))
  (process-send-string process command)
  (process-send-string process "\r\n"))

(defun vm-pop-read-response (process &optional return-response-string)
  (let ((case-fold-search nil)
	 match-end)
    (goto-char vm-pop-read-point)
    (while (not (search-forward "\r\n" nil t))
      (accept-process-output process)
      (goto-char vm-pop-read-point))
    (setq match-end (point))
    (goto-char vm-pop-read-point)
    (if (not (looking-at "+OK"))
	(progn (setq vm-pop-read-point match-end) nil)
      (setq vm-pop-read-point match-end)
      (if return-response-string
	  (buffer-substring (point) match-end)
	t ))))

(defun vm-pop-read-stat-response (process)
  (let ((response (vm-pop-read-response process t)))
    (string-to-int (nth 1 (vm-parse response "\\([^ ]+\\) *")))))

(defun vm-pop-retrieve-to-crashbox (process crash)
  (let ((start vm-pop-read-point) end)
    (goto-char start)
    (while (not (re-search-forward "^\\.\r\n" nil t))
      (accept-process-output process)
      (goto-char start))
    (setq vm-pop-read-point (point-marker))
    (goto-char (match-beginning 0))
    (setq end (point-marker))
    (vm-pop-cleanup-region start end)
    ;; Some POP servers strip leading and trailing message
    ;; separators, some don't.  Figure out what kind we're
    ;; talking to and do the right thing.
    (if (eq (save-restriction (narrow-to-region start end)
			      (vm-get-folder-type))
	    'unknown)
	(progn
	  (vm-munge-message-separators vm-folder-type start end)
	  (goto-char start)
	  ;; avoid the consing and stat() call for all but babyl
	  ;; files, since this will probably slow things down.
	  ;; only babyl files have the folder header, and we
	  ;; should only insert it if the crash box is empty.
	  (if (and (eq vm-folder-type 'babyl)
		   (let ((attrs (file-attributes crash)))
		     (or (null attrs) (equal 0 (nth 7 attrs)))))
	      (let ((opoint (point)))
		(vm-convert-folder-header nil vm-folder-type)
		;; if start is a marker, then it was moved
		;; forward by the insertion.  restore it.
		(setq start opoint)
		(goto-char start)
		(vm-skip-past-folder-header)))
	  (insert (vm-leading-message-separator))
	  ;; this will not find the trailing message separator but
	  ;; for the Content-Length stuff counting from eob is
	  ;; the same thing in this case.
	  (vm-convert-folder-type-headers nil vm-folder-type)
	  (goto-char end)
	  (insert-before-markers (vm-trailing-message-separator))))
    (write-region start end crash t 0)
    (delete-region start end)
    t ))

(defun vm-pop-cleanup-region (start end)
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    ;; CRLF -> LF
    (while (and (< (point) end) (search-forward "\r\n"  end t))
      (replace-match "\n" t t))
    (goto-char start)
    ;; chop leading dots
    (while (and (< (point) end) (re-search-forward "^\\."  end t))
      (replace-match "" t t)
      (forward-char)))
  (set-marker end nil))

(defun vm-pop-md5 (string)
  (let ((buffer nil))
    (unwind-protect
	(save-excursion
	  (setq buffer (generate-new-buffer "*vm-work*"))
	  (set-buffer buffer)
	  (insert string)
	  (call-process-region (point-min) (point-max)
			       "/bin/sh" t buffer nil
			       "-c" vm-pop-md5-program)
	  ;; MD5 digest is 32 chars long
	  ;; mddriver adds a newline to make neaten output for tty
	  ;; viewing, make sure we leave it behind.
	  (buffer-substring (point-min) (+ (point-min) 32)))
      (and buffer (kill-buffer buffer)))))
