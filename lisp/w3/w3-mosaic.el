;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for compatibility with XMosaic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for global history file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-write-global-history (&optional fname)
  "Write the global history file into w3-global-history-file"
  (interactive)
  (if (not fname) (setq fname w3-global-history-file))
  (if (not (file-exists-p w3-global-history-file))
      (progn
	(message "Creating history file %s." w3-global-history-file)
	(set-buffer (get-buffer-create " *W3HIST*"))
	(erase-buffer)
	(insert "ncsa-mosaic-history-format-1\nGlobal\n"))
    (progn
      (set-buffer (get-buffer-create " *W3HIST*"))
      (erase-buffer)
      (insert-file-contents w3-global-history-file)))
  (let ((tmp w3-history-list)
	url)
    (while tmp
      (setq url (car (car tmp)))
      (if url
	  (progn
	    (goto-char (point-min))
	    (if (not (re-search-forward (regexp-quote url) nil t))
		(progn
		  (goto-char (point-max))
		  (insert (format "%s %s\n" url (current-time-string)))))))
      (setq tmp (cdr tmp))))
  (write-file w3-global-history-file)
  (kill-buffer (current-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hotlist Handling Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-remove-from-hotlist ()
  "Deletes a document from your hotlist file"
  (interactive)
  (save-excursion
    (if (not w3-hotlist) (message "No hotlist in memory!")
      (if (not (file-exists-p w3-hotlist-file))
	  (message "Hotlist file %s does not exist." w3-hotlist-file)
	(let* ((title (car (assoc (completing-read "Delete Document: "
						   w3-hotlist nil t)
				  w3-hotlist)))
	       (buffer (get-buffer-create " *HOTW3*")))
	  (set-buffer buffer)
	  (erase-buffer)
	  (insert-file-contents w3-hotlist-file)
	  (if (re-search-forward (regexp-quote title) nil t)
	      (progn
		(previous-line 1)
		(beginning-of-line)
		(kill-line 2)
		(write-file w3-hotlist-file)
		(setq w3-hotlist (w3-delete-from-alist title w3-hotlist))
		(kill-buffer (current-buffer)))
	    (message "%s was not found in %s" title w3-hotlist-file))))))
  (if (and w3-running-FSF19 (eq window-system 'x))
      (progn
	(delete-menu-item '("Navigate"))
	(w3-build-FSF19-menu))))

(defun w3-rename-hotlist-entry (title)
  "Rename a personal annotation"
  (interactive (list (completing-read "Rename entry: " w3-hotlist nil t)))
  (cond					; Do the error handling first
   ((not w3-hotlist) (error "No hotlist in memory!"))
   ((not (file-exists-p (expand-file-name w3-hotlist-file)))
    (error "Hotlist file %s does not exist." w3-hotlist-file))
   ((not (file-readable-p (expand-file-name w3-hotlist-file)))
    (error "Hotlist file %s exists, but is unreadable." w3-hotlist-file)))
  (save-excursion
    (let ((obj (assoc title w3-hotlist))
	  (used (mapcar 'car w3-hotlist))
	  (buff (get-buffer-create " *HOTW3*"))
	  (new nil)
	  )
      (while (or (null new) (w3-member new used))
	(setq new (read-string "New name: ")))
      (set-buffer buff)
      (erase-buffer)
      (insert-file-contents (expand-file-name w3-hotlist-file))
      (goto-char (point-min))
      (if (re-search-forward (regexp-quote title) nil t)
	  (progn
	    (previous-line 1)
	    (beginning-of-line)
	    (kill-line 2)
	    (insert (format "%s %s\n%s\n" (nth 1 obj) (current-time-string)
			    new))
	    (setq w3-hotlist (cons (list new (nth 1 obj))
				   (w3-delete-from-alist title w3-hotlist)))
	    (write-file w3-hotlist-file)
	    (kill-buffer (current-buffer))
	    (if (and w3-running-FSF19 window-system)
		(progn
		  (delete-menu-item '("Navigate"))
		  (w3-build-FSF19-menu))))
	(message "%s was not found in %s" title w3-hotlist-file)))))    
       
(defun w3-append-hotlist (fname)
  "Append a hotlist to the one in memory"
  (interactive "fAppend hotlist file: ")
  (let ((x w3-hotlist))
    (w3-parse-hotlist fname)
    (setq w3-hotlist (nconc x w3-hotlist))))

(defun w3-parse-hotlist (&optional fname)
  "Read in the hotlist specified by FNAME"
  (if (not fname) (setq fname w3-hotlist-file))
  (setq w3-hotlist nil)
  (if (not (file-exists-p fname))
      (message "%s does not exist!" fname)
    (let* ((old-buffer (current-buffer)) 
	   (buffer (get-buffer-create " *HOTW3*"))
	   cur-link
	   cur-alias)
      (set-buffer buffer)
      (erase-buffer)
      (insert-file-contents fname)
      (goto-char (point-min))
      (while (re-search-forward "^\n" nil t) (replace-match ""))
      (goto-line 3)
      (while (not (equal (point) (point-max)))
	(re-search-forward "^[^ ]*" nil t)
	(setq cur-link (buffer-substring (match-beginning 0) (match-end 0)))
	(setq cur-alias (buffer-substring (progn
					    (forward-line 1)
					    (beginning-of-line)
					    (point))
					  (progn
					    (end-of-line)
					    (point))))
	(if (not (equal cur-alias ""))
	    (setq w3-hotlist (cons (list cur-alias cur-link) w3-hotlist))))
      (kill-buffer buffer)
      (set-buffer old-buffer))))

(defun w3-use-hotlist ()
  "Possibly go to a link in the hotlist"
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (not w3-hotlist) (message "No hotlist in memory!")
    (let* ((url (car (cdr (assoc
			   (completing-read "Goto Document: " w3-hotlist nil t)
			   w3-hotlist)))))
      (w3-fetch url))))

(defun w3-add-document-to-hotlist ()
  "Add this documents url to the hotlist"
  (interactive)
  (save-excursion
    (let* ((buffer (get-buffer-create " *HOTW3*"))
	   (title (buffer-name))
	   (url (w3-view-url t)))
      (set-buffer buffer)
      (erase-buffer)
      (setq w3-hotlist (cons (list title url) w3-hotlist))
      (if (not (file-exists-p w3-hotlist-file))
	  (progn
	    (message "Creating hotlist file %s" w3-hotlist-file)
	    (insert "ncsa-xmosaic-hotlist-format-1\nDefault\n\n")
	    (backward-char 1))
	(progn
	  (insert-file-contents w3-hotlist-file)
	  (goto-char (point-max))
	  (backward-char 1)))
      (insert "\n" url " " (current-time-string) "\n" title)
      (write-file w3-hotlist-file)
      (kill-buffer (current-buffer))))
  (if (and w3-running-FSF19 (eq window-system 'x))
      (progn
	(delete-menu-item '("Navigate" ))
	(w3-build-FSF19-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annotation server handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-fetch-annotations ()
  "Fetch the annotations for the current document"
  (if (not w3-group-annotation-server)
      (error "No group annotation server defined!")
    (let ((cmd (format "ANN_GET /url=\"%s\";=\n" (w3-view-url t)))
	  (proc nil)
	  (tmp 0)
	  (tmp2 0))
      (save-excursion
	(set-buffer (get-buffer-create w3-working-buffer))
	(goto-char (point-max))
	(insert "\n"))
      (setq proc (w3-open-stream "*anno*" (get-buffer-create w3-working-buffer)
				 w3-group-annotation-server
				 w3-group-annotation-port))
      (message "Fetching annotations...")
      (if (processp proc)
	  (progn
	    (process-send-string proc cmd)
	    (while (memq (process-status proc) '(run open))
	      (if (= 0 (setq tmp2 (% (1+ tmp2) 200)))
		  (message "Fetching annotations..%s" (make-string
						       (setq tmp (% (1+ tmp) 50))
						       ?.)))
	      (accept-process-output)))
	(message proc))
      
      (condition-case ()
	  (delete-process proc);; make sure its dead
	(error nil))
      (w3-replace-regexp (regexp-quote (substring cmd 0 -1)) "")
      (w3-replace-regexp
       "Process .*anno.* \\(exit\\|kill\\|finish\\).*" ""))))

(defun w3-is-annotation ()
  "Is this a group annotation?"
  (and (equal w3-current-server w3-group-annotation-server)
       (= (string-to-int w3-current-port) w3-group-annotation-port)))

(defun w3-delete-group-annotation ()
  "Delete this group annotation"
  (interactive)
  (if (not w3-group-annotation-server)
      (error "No group annotation server defined!")
    (if (w3-is-annotation)
	(let ((cmd (format "ANN_DELETE /url=\"%s\";=\n" (w3-view-url t)))
	      (proc nil)
	      (parseit nil)
	      (buff (current-buffer)))
	  (save-excursion
	    (set-buffer (get-buffer-create w3-working-buffer))
	    (erase-buffer)
	    (setq proc (w3-open-stream "*anno*" (get-buffer-create w3-working-buffer)
				       w3-group-annotation-server
				       w3-group-annotation-port))
	    (if (processp proc)
		(process-send-string proc cmd))
	    (while (memq (process-status proc) '(run open))
	      (accept-process-output))
	    (condition-case ()
		(delete-process proc);; make sure its dead
	      (error nil))
	    (w3-replace-regexp (regexp-quote cmd) "")
	    (w3-replace-regexp "Process .*anno.* exit.*" "")
	    (goto-char (point-min))
	    (setq w3-current-type nil
		  w3-current-file "historylist"
		  w3-current-last-buffer buff)
	    (if (and (re-search-forward "[^ \t\n]+" nil t)
		     (not
		      (progn
			(goto-char (point-min))
			(re-search-forward "success!" nil t))))
		(setq parseit t)
	      (setq parseit nil)))
	  (if parseit
	      (w3-sentinel nil nil)
	    (message "Deleted...")))
      (message "This is not an annotation."))))

(defun w3-add-group-annotation ()
  "Add an annotation to the current url"
  (interactive)
  (let ((url (w3-view-url t))
	(buf (get-buffer-create "*Annotation*")))
    (if w3-mutable-windows (pop-to-buffer buf) (switch-to-buffer buf))
    (set-buffer buf)
    (erase-buffer)
    (insert "</PRE>\n\n")
    (html-mode)
    (setq w3-current-annotation url)
    (define-key html-mode-map "\C-c\C-c" 'w3-do-group-annotation)
    (message "Hit C-cC-c to send this annotation.")))	

(defun w3-do-group-annotation ()
  "Finish adding an annotation"
  (interactive)
  (let* ((bufsize (buffer-size))
	 (bufstr  (buffer-string))
	 (cmd (format "ANN_%s /url=\"%s\";title=\"%s\";user=\"%s\";date=%s;length=%d;=%s\n"
		      (if w3-editing-annotation "CHANGE" "SET")
		      w3-current-annotation
		      (read-string "Title: "
				   (format "Annotation by %s (%s@%s)"
					   (user-full-name)
					   (user-real-login-name)
					   (system-name)))
		      (format "%s@%s" (user-real-login-name)
			      (system-name))
		      (current-time-string)
		      bufsize bufstr))
	 (proc nil)
	 (parseit nil))
    (save-excursion
      (not-modified)
      (kill-buffer (current-buffer))
      (set-buffer (get-buffer-create w3-working-buffer))
      (erase-buffer)
      (setq proc (w3-open-stream "*anno*" (get-buffer-create w3-working-buffer)
				 w3-group-annotation-server
				 w3-group-annotation-port))
      (process-send-string proc cmd)
      (while (memq (process-status proc) '(run open))
	(accept-process-output))
      (condition-case ()
	  (delete-process proc);; make sure its dead
	(error nil))
      (w3-replace-regexp (regexp-quote cmd) "")
      (w3-replace-regexp "Process .*anno.* exit.*" "")
      (goto-char (point-min))
      (setq w3-current-type nil
	    w3-current-file "historylist")
      (if (re-search-forward "[^ \t\n]+" nil t)
	  (setq parseit t)
	(setq parseit nil)))
    (if parseit
	(w3-sentinel nil nil)
      (message "Annotation delivered..."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private annotation support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-parse-personal-annotations ()
  "Read in personal annotation file"
  (if (and 
       (file-exists-p (format "%s/LOG" w3-personal-annotation-directory))
       (file-readable-p (format "%s/LOG" w3-personal-annotation-directory)))
      (save-excursion
	(setq w3-personal-annotations nil);; nuke the old list
	(let ((start nil)
	      (end nil)
	      (txt nil)
	      (url nil)
	      (num nil))
	  (set-buffer (get-buffer-create " *panno*"))
	  (erase-buffer)
	  (insert-file-contents
	   (format "%s/LOG" w3-personal-annotation-directory))
	  (goto-char (point-min))
	  (w3-replace-regexp "\\\n+" "\\\n")
	  (goto-char (point-min)) 
	  (kill-line 2);; nuke the header lines
	  (goto-char (point-min))
	  (cond
	   ((eobp) nil)			; Empty LOG file
	   (t
	    (if (/= (char-after (1- (point-max))) ?\n)
		(save-excursion
		  (goto-char (point-max))
		  (insert "\n")))
	    (while (/= (point-max) (point))
	      (setq start (point)
		    end (prog2 (end-of-line) (point) (forward-char 1))
		    txt (buffer-substring start end)
		    url (substring txt 0 (string-match " " txt))
		    num (w3-split
			 (substring txt (1+ (string-match " " txt)) nil)
			 "[ \\\t]"))
	      (while num
		(setq w3-personal-annotations 
		      (cons
		       (list url
			     (list (car (car num))
				   (w3-grok-annotation-format
				    (car (car num)))))
		       w3-personal-annotations)
		      num (cdr num))))))
	  (kill-buffer " *panno*")))))

(defun w3-grok-annotation-format (anno)
  "Grab the title from an annotation"
  (save-excursion
    (set-buffer (get-buffer-create " *annotmp*"))
    (erase-buffer)
    (if (file-exists-p (format "%s/PAN-%s.html"
			       w3-personal-annotation-directory anno))
	(insert-file-contents (format "%s/PAN-%s.html"
				      w3-personal-annotation-directory anno)))
    (goto-char (point-min))
    (prog1
	(if (re-search-forward "<title>\\(.*\\)</title>" nil t)
	    (buffer-substring (match-beginning 1) (match-end 1))
	  "No title")
      (kill-buffer " *annotmp*"))))    

(defun w3-fetch-personal-annotations ()
  "Grab any personal annotations for the current url"
  (let ((url  (w3-view-url t))
	(anno w3-personal-annotations)
	(annolist nil))
    (if (assoc url anno)
	(while anno
	  (if (equal (car (car anno)) url)
	      (setq annolist
		    (cons
		     (format "<A HREF=\"file:%s%s/PAN-%s.html\">%s</A>"
			     (if (= ?/ (string-to-char
					w3-personal-annotation-directory)) ""
			       "/")
			     w3-personal-annotation-directory
			     (car (car (cdr (car anno))))
			     (car (cdr (car (cdr (car anno))))))
		     annolist)))
	  (setq anno (cdr anno))))
    annolist))

(defun w3-is-personal-annotation (url)
  "Is URL a personal annotation?"
  (string-match "file:/.*/PAN-.*\\.html" url))

(defun w3-delete-personal-annotation ()
  "Delete a personal annotation."
  (interactive)
  (if (w3-is-personal-annotation (w3-view-url t))
      (let ((num nil)
	    (annotated-url nil)
	    (anno w3-personal-annotations))
	(string-match "file:/.*/PAN-\\(.*\\)\\.html" (w3-view-url t))
	(setq num (substring (w3-view-url t) (match-beginning 1)
			     (match-end 1)))
	(while anno
	  (if (equal num (car (car (cdr (car anno)))))
	      (setq annotated-url (car (car anno))))
	  (setq anno (cdr anno)))
	(if annotated-url
	    (save-excursion
	      (set-buffer (get-buffer-create " *annotmp*"))
	      (erase-buffer)
	      (insert-file-contents (format "%s/LOG"
					    w3-personal-annotation-directory))
	      (replace-regexp (format "[ \\\t]+\\b%s\\b[ \\\t]*" num) " ")
	      (goto-char (point-min))
	      (delete-matching-lines (format "^%s +$" annotated-url))
	      (let ((make-backup-files nil)
		    (version-control nil)
		    (require-final-newline t))
		(write-region (point-min) (point-max)
			      (format "%s/LOG"
				      w3-personal-annotation-directory)))
	      (kill-buffer " *annotmp*")
	      (setq anno w3-personal-annotations
		    w3-personal-annotations nil)
	      (while anno
		(if (not (string= num (car (car (cdr (car anno))))))
		    (setq w3-personal-annotations
			  (cons (car anno) w3-personal-annotations)))
		(setq anno (cdr anno)))
	      (delete-file (format "%s/PAN-%s.html"
				   w3-personal-annotation-directory num)))
	  (message "Couldn't find url that this is annotating!")))
    (message "This isn't a personal annotation.")))  

(defun w3-add-personal-annotation ()
  "Add an annotation to this document."
  (interactive)
  (let ((url (w3-view-url t))
	(buf (get-buffer-create "*Personal Annotation*")))
    (set-buffer buf)
    (if w3-mutable-windows (pop-to-buffer buf) (switch-to-buffer buf))
    (erase-buffer)
    (insert "</PRE>\n\n")
    (html-mode)
    (setq w3-current-annotation url)
    (define-key html-mode-map "\C-c\C-c" 'w3-do-personal-annotation)
    (message "Hit C-cC-c to send this annotation.")))

(defun w3-find-highest-annotation-number ()
  "Find the highest annotation number in this buffer"
  (let (x)
    (goto-char (point-min))
    (while (re-search-forward "[^ \\\t\\\n]*[ \\\t]\\(.*\\)" nil t)
      (setq x (nconc (mapcar (function (lambda (x) (string-to-int (car x))))
			     (w3-split (buffer-substring (match-beginning 1)
							 (match-end 1))
				       "[ \\\t]")) x)))
    (if (not x) (setq x '(0)))
    (1+ (car (sort x '>)))))

(defun w3-do-personal-annotation ()
  "Finish doing a personal annotation."
  (interactive)
  (if (or (not w3-personal-annotation-directory)
	  (not (file-exists-p w3-personal-annotation-directory))
	  (not (file-directory-p w3-personal-annotation-directory)))
      (error "No personal annotation directory!")
    (let ((url w3-current-annotation)
	  (txt (buffer-string))
	  (title (read-string "Title: "
			      (format "Annotation by %s on %s"
				      (user-real-login-name)
				      (current-time-string))))
	  (fname nil)
	  (num nil))
      (save-excursion
	(not-modified)
	(kill-buffer (current-buffer))
	(set-buffer (get-buffer-create " *annotmp*"))
	(erase-buffer)
	(if (file-exists-p		; Insert current LOG file if
					; it exists.
	     (format "%s/LOG" w3-personal-annotation-directory))
	    (insert-file-contents
	     (format "%s/LOG" w3-personal-annotation-directory))
	  (progn			; Otherwise, create a file
	    (goto-char (point-min))	; that conforms to first
					; annotation format from NCSA
	    (insert "ncsa-mosaic-personal-annotation-log-format-1\n")
	    (insert "Personal\n")))
	(goto-char (point-min))
	(setq num (int-to-string (w3-find-highest-annotation-number))
	      fname (format "%s/PAN-%s.html"
			    w3-personal-annotation-directory num))
	(goto-char (point-min))
	(if (re-search-forward (regexp-quote url) nil t)
	    (progn
	      (end-of-line)
	      (insert " "))
	  (goto-char (point-max))
	  (insert "\n" url " "))
	(insert num)
	(let ((make-backup-files nil)
	      (version-control nil)
	      (require-final-newline t))
	  (write-region (point-min) (point-max)
			(format "%s/LOG" w3-personal-annotation-directory))
	  (erase-buffer)
	  (insert (format "%s\n<title>%s</title>\n<h1>%s</h1>\n"
			  w3-annotation-marker
			  title title))
	  (insert
	   (format "<address>%s (%s@%s)</address>\n<address>%s</address>\n"
		   (user-full-name)
		   (user-real-login-name)
		   (system-name)
		   (current-time-string)))
	  (insert "<HR>\n<pre>" txt)
	  (write-region (point-min) (point-max) fname))
	(setq w3-personal-annotations
	      (cons (list url (list num title)) w3-personal-annotations))))))

(provide 'w3-mosaic)
