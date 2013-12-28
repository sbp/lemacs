;;; -*- Mode:Emacs-Lisp -*-

;;; gnus-mark.el v1.4
;;; Operating on more than one news article at a time.
;;; Created: 28-Jun-91 by Jamie Zawinski <jwz@lucid.com>
;;; Modified: 28-Jun-91 by Sebastian Kremer <sk@thp.Uni-Koeln.DE>
;;; Modified: 1-Dec-91 by Jamie Zawinski <jwz@lucid.com>
;;; Modified: 05-Dec-91 by Paul D. Smith <paul_smith@dg.com>
;;; Modified: 28-Nov-92 by A1C Tim Miller <tjm@hrt213.brooks.af.mil>
;;;
;;; typing `@' in the subject buffer will mark the current article with
;;; an `@'.  After marking more than one article this way, you can use one
;;; of the commands in this file on all of them at once.
;;;
;;; `M-@' will prompt you for a regular expression, and will mark all f
;;; articles which match.
;;;
;;; `^U@' will prompt you for a mark-character to use other than `@'.
;;;
;;; To unmark an article, use `u', `d', or `^U M-@ SPC RET'.
;;;
;;; `F' (gnus-forward-marked-articles) will put you in a send-mail buffer 
;;; along with the contents of all of the marked articles in RFC-944 digest
;;; format, suitable for later explosion with any reasonable mail reader.
;;;
;;; `M-x gnus-uudecode-marked-messages' (M-x gnus-uu RET works) will strip the
;;; junk from the beginning and end of the marked articles, concatenate them
;;; together, and pipe the result through uudecode.  If the resultant file is
;;; a tar file and/or is compressed, this command offers to unpack/uncompress
;;; as well.  See also the variables gnus-uudecode-file-mode, 
;;; gnus-uudecode-auto-chmod, and gnus-uudecode-auto-touch.  If the first
;;; marked message is not the first part of the uuencoded file, or if the last
;;; marked message is not the last part of the uuencoded file, it complains.
;;; However, it's not possible to tell if the middle parts are out of order,
;;; so make sure you use ^C^S^S to get the articles in the right order first.
;;; It also complains about obviously-corrupted files.
;;;
;;; `M-x gnus-unshar-marked-articles' (M-x gnus-un RET works) will strip the 
;;; junk from the beginning and end of the marked articles, and run each of
;;; them through sh in turn.  This doesn't work on shar files that don't 
;;; begin with "#!".
;;;
;;; Both of the above commands prompt you for a directory in which to do the
;;; dirty work.  If the directory you specify doesn't exist, you have the
;;; option of creating it.
;;;
;;; M-x gnus-save-marked-articles-in-file will save marked articles in a file;
;;; prompts with gnus' standard filename query for each article unless
;;; gnus-save-marked-in-same-file is non-nil.
;;
;; LCD Archive Entry:
;; gnus-mark|Jamie Zawinski|jwz@lucid.com
;; |Operate on more than one news article at a time
;; |92-11-30||~/misc/gnus-mark.el.Z|

;; 05 Dec 91 pds - Paul D. Smith (paul_smith@dg.com)
;;
;;  * Changed the key binding for gnus-forward-marked-articles to C-f
;;    instead of F: `F' is Followup-yank-original which is a very
;;    common function!
;;
;;  * Added local gnus-mark-shell-command function which performs
;;    more exactly what we want to happen; namely allows the shell
;;    output buffer to be displayed before the command is executed and
;;    to be filled real-time.  It will either erase the buffer or just
;;    add text to the end.
;;
;;  * Fixed a bug where if you had one of the articles you marked
;;    actually read in when you used one of the three mark processing
;;    commands the mark would not be deleted.
;;
;;  * Modified gnus-Subject-mark-article to use GNUS functions to do
;;    the marking instead of modifying the character itself.  This
;;    keeps the cursor in the right position in the buffer, doesn't
;;    allow it to go beyond the end of the buffer even if you select
;;    the last article, etc.

(require 'gnus)

(define-key gnus-Subject-mode-map "@" 'gnus-Subject-mark-article)
(define-key gnus-Subject-mode-map "\C-F" 'gnus-forward-marked-articles)
(define-key gnus-Subject-mode-map "\M-@" 'gnus-Subject-mark-regexp)
;;; See also gnus-uudecode-marked-messages and gnus-unshar-marked-articles.

(defvar gnus-default-mark-char ?@
  "*Character used to mark articles for later commands in GNUS.")

(defun gnus-Subject-mark-article (mark)
"Mark the current article for later commands.
This marker comes from variable `gnus-default-mark-char'.
You can change this variable by giving a prefix argument to this command,
in which case you will be prompted for the character to use."
  (interactive (list (if current-prefix-arg
			 (let ((cursor-in-echo-area t))
			   (message "Mark message with: ")
			   (setq gnus-default-mark-char (read-char)))
			 gnus-default-mark-char)))
  (or (eq (current-buffer) (get-buffer gnus-Subject-buffer))
      (error "not in subject buffer"))
  (gnus-Subject-mark-as-read nil gnus-default-mark-char)
  (gnus-Subject-next-subject 1 nil))

;; Actually, gnus-kill should have an interactive spec!
(defun gnus-Subject-mark-regexp (regexp &optional marker)
  "Mark all articles with subjects matching REGEXP.
With a prefix ARG, prompt for the marker.  Type RET immediately to
mark them as unread or enter SPC RET to remove all kinds of marks."
  (interactive
   (list (read-string "Mark (regexp): ")
	 (if current-prefix-arg
	     (read-string
	      "Mark with char (RET to mark as unread, SPC RET to remove existing markers): "))))
  (setq marker (or marker (char-to-string gnus-default-mark-char)))
  (gnus-kill "Subject" regexp
	     (if (equal "" marker)
		 '(gnus-Subject-mark-as-unread)
	       (list 'gnus-Subject-mark-as-read nil marker))
	     ;; overwrite existing marks:
	     t))

(defun gnus-Subject-mark-map-articles (mark function)
  (save-excursion
    (set-buffer gnus-Subject-buffer)
    (let ((str (concat "^" (make-string 1 mark) " +\\([0-9]+\\):"))
	  got-one)
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at str)
	      (progn
		(setq got-one t)
		(save-excursion
		  (funcall function
		    (gnus-find-header-by-number gnus-newsgroup-headers 
		      (string-to-int
			(buffer-substring
			  (match-beginning 1) (match-end 1))))))))
	  (forward-line 1)))
      (cond ((not got-one)
	     (let ((article (gnus-Subject-article-number)))
	       (if (or (null gnus-current-article)
		       (/= article gnus-current-article))
		   ;; Selected subject is different from current article's.
		   (gnus-Subject-display-article article))
	       (funcall function 
			(gnus-find-header-by-number gnus-newsgroup-headers 
						    article)))))
      )))


;;; simpler, more specific to gnus-mark version of shell-command

(defun gnus-mark-shell-command (start end command erase)
  "Execute string COMMAND in inferior shell with region as input.
Display output (if any) in temp buffer interactively.
If ERASE is non-nil the buffer is erased, otherwise the output is
appended to the end of the buffer."
    (let ((buffer (get-buffer-create "*Shell Command Output*"))
	  (orig-buffer (current-buffer)))
      (set-buffer buffer)
      (if erase
	  (erase-buffer)
	(goto-char (point-max)))
      (set-buffer orig-buffer)
      (if (eq buffer orig-buffer)
	  (setq start 1 end 1))
      (display-buffer buffer)
      (bury-buffer buffer)
      (call-process-region start end shell-file-name
			   nil buffer t "-c" command)))


;;; RFC944 forwarding of multiple messages

(defun gnus-forward-marked-articles ()
  "Forward the marked messages to another user, RFC944 style."
  (interactive)
  (let (subj p
	(state 'first)
	tmp-buf)
    (unwind-protect
	(progn
	  (setq tmp-buf (get-buffer-create "*gnus-forward-tmp*"))
	  (save-excursion (set-buffer tmp-buf) (erase-buffer))
	  (gnus-Subject-mark-map-articles
	   gnus-default-mark-char
	   (function (lambda (msg)
	     (if (eq state 'first) (setq state t) (setq state nil))
	     (message "Snarfing article %s..." (aref msg 0))
	     (if (eq gnus-current-article (aref msg 0))
		 (gnus-Subject-mark-as-read)
	       (gnus-Subject-display-article (aref msg 0)))
	     (set-buffer gnus-Article-buffer)
	     (widen)
	     (set-buffer tmp-buf)
	     (goto-char (point-max))
	     (if subj
		 (insert "----------\n")
	       (setq subj (aref msg 1)))
	     (setq p (point))
	     (insert-buffer gnus-Article-buffer)
	     (goto-char p)
	     (while (re-search-forward "^-" nil t)
	       (insert " -"))
	     )))
	  (mail nil nil (concat "[Fwd: " subj "]"))
	  (save-excursion
	    (goto-char (point-max))
	    (insert (if state
			"---------- Begin forwarded message\n"
		      "---------- Begin digest\n"))
	    (insert-buffer tmp-buf)
	    (goto-char (point-max))
	    (insert (if state
			"\n---------- End forwarded message\n"
		      "\n---------- End digest\n"))))
      ;; protected
      (and tmp-buf (kill-buffer tmp-buf)))))


;;; reading a directory name, and offering to create if it doesn't exist.

(defun gnus-mark-read-directory (prompt &optional default-dir)
  (let ((dir
	 (read-file-name prompt
			 (or default-dir default-directory)
			 (or default-dir default-directory))))
    (if (string-match "/$" dir)
	(setq dir (substring dir 0 (match-beginning 0))))
    (setq dir
      (cond ((file-directory-p dir) dir)
	    ((file-exists-p dir)
	     (ding)
	     (message "%s exists and is not a directory!" dir)
	     (sleep-for 2)
	     (gnus-mark-read-directory prompt dir))
	    ((y-or-n-p (format "directory %s doesn't exist, create it? " dir))
	     (make-directory dir)
	     dir)
	    (t (gnus-mark-read-directory prompt dir))))
    (if (string-match "/$" dir)
	dir
      (concat dir "/"))))


;;; uudecode

(defconst gnus-uudecode-begin-pattern
    "^begin[ \t]+\\([0-9][0-9][0-9][0-9]?\\)[ \t]+\\([^ \t\n]*\\)$")

(defconst gnus-uudecode-body-pattern
    "^M.............................................................?$")

(defconst gnus-uudecode-begin-or-body-pattern
    (concat "\\(" gnus-uudecode-begin-pattern "\\|"
	    gnus-uudecode-body-pattern "\\)"))

(defvar gnus-uudecode-file-mode "644"
  "*If non-nil, this overrides the mode specified in the `begin' line of
a uuencoded file being unpacked by vm-uudecode.  This should be a string,
which is the mode desired in octal.")

(defvar gnus-uudecode-auto-chmod "u+w"
  "*If non-nil, then when gnus is untarring a file for you, it will
apply this chmod modifier to each of the unpacked files.  This should be
a string like \"u+w\".")

(defvar gnus-uudecode-auto-touch t
  "*If non-nil, then when vm-uudecode is untarring a file for you, it will
cause the write-date of each of the unpacked files to be the current time.
Normally tar unpacks files with the time at which they are packed; this can
cause your `make' commands to fail if you are installing a new version of
a package which you have modified.")

(defvar gnus-uudecode-picture-pattern "\\.\\(gif\\|p[bgp]m\\|rast\\|pic\\|jpg\\)$"
  "*If non-nil, this should be a pattern which matches files which are 
images.  When gnus-uudecode-marked-articles creates a file which matches
this pattern, it will ask you if you want to look at it now.  If so, it
invokes gnus-uudecode-picture-viewer with the filename as an argument.
After doing this, it asks you if you want to keep the picture or delete it.")

(defvar gnus-uudecode-picture-viewer "xv"
  "*The picture viewer that gnus-uudecode-marked-messages uses.  See doc of
variable gnus-uudecode-picture-pattern.")

(defvar gnus-uudecode-default-directory nil "*")

(defun gnus-uudecode-marked-articles (directory)
  "Strip the junk from the beginning and end of the marked articles, 
concatenate them together, and pipe the result through uudecode.  If
the resultant file is a tar file and/or is compressed, this command
offers to unpack/uncompress as well.  See also the variables
gnus-uudecode-file-mode, gnus-uudecode-auto-chmod, and
 gnus-uudecode-auto-touch."
  (interactive (list (gnus-mark-read-directory "uudecode in directory: "
		       gnus-uudecode-default-directory)))
  (setq gnus-uudecode-default-directory directory)
  (let ((state 'first)
	tmp-buf
	name)
    (unwind-protect
      (progn
       (setq tmp-buf  (get-buffer-create "*gnus-uudecode-tmp*"))
       (save-excursion (set-buffer tmp-buf) (erase-buffer))
       (gnus-Subject-mark-map-articles
	gnus-default-mark-char
	(function (lambda (msg)
	  (message "Snarfing article %s..." (aref msg 0))
	  (if (eq state 'last)
	      (error "articles out of order: articles follow `end' line."))
	  (if (eq gnus-current-article (aref msg 0))
	      (gnus-Subject-mark-as-read)
	    (gnus-Subject-display-article (aref msg 0)))
	  (set-buffer gnus-Article-buffer)
	  (widen)
	  (set-buffer tmp-buf)
	  (goto-char (point-max))
	  (let ((p (point))
		(case-fold-search nil))
	    (insert-buffer gnus-Article-buffer)
	    (cond
	     ((eq state 'first)
	      (or (re-search-forward gnus-uudecode-begin-pattern nil t)
		  (error "couldn't find `begin' line in first article."))
	      ;; I'd like to second-guess the losers who use mixed-case
	      ;; and upper-case filenames, but this trashes trailing ".Z"
	      ;;(downcase-region (match-beginning 2) (match-end 2))
	      (setq name (buffer-substring (match-beginning 2) (match-end 2)))
	      ;; don't tolerate bogus umasks.
	      (if gnus-uudecode-file-mode
		  (progn
		    (goto-char (match-beginning 1))
		    (delete-region (match-beginning 1) (match-end 1))
		    (insert gnus-uudecode-file-mode)))
	      (setq state 'middle))
	     (t
	      (or (re-search-forward gnus-uudecode-begin-or-body-pattern nil t)
		  (error "couldn't find beginning of data."))))
	    (beginning-of-line)
	    (delete-region p (point))
	    (let (c len tmp)
	      ;; This could be sped up a lot, but then we'd lose the
	      ;; error checking it does; maybe that's ok.
	      (while (progn
		       (forward-line)
		       (setq c (- (following-char) ? ))
		       (end-of-line)
		       (setq tmp (/ (1- (current-column)) 4))
		       (beginning-of-line)
		       (= (+ tmp (+ tmp tmp)) c))
		)
	      ;; Slack.
	      (setq p (point))
	      (if (or (looking-at "end\n")
		      (progn (forward-line 1) (looking-at "end\n"))
		      (progn (forward-line 1) (looking-at "end\n"))
		      (progn (forward-line 1) (looking-at "end\n")))
		  (progn
		    (forward-line 1)
		    (setq state 'last))
		(goto-char p))
	      )
	    (delete-region (point) (point-max))))))
       (or (eq state 'last) (error "no `end' line in last article."))
       (set-buffer tmp-buf)
       (let* ((base-file (file-name-nondirectory name))
	      (final-file (concat directory base-file))
	      (command (concat "cd " directory " ; uudecode"))
	      tar-p)
	 (cond ((string-match "\\.tar\\.Z$" base-file)
		(if (y-or-n-p "uncompress/untar? ")
		    (setq command (concat command " && zcat "
					  base-file " | tar -vxf -")
			  final-file nil
			  tar-p t)))
	       ((string-match "\\.tar$" base-file)
		(if (y-or-n-p "untar? ")
		    (setq command (concat command " && tar -vxf " base-file)
			  final-file nil
			  tar-p t)))
	       ((string-match "\\.Z$" base-file)
		(if (y-or-n-p "uncompress? ")
		    (setq command (concat command " ; uncompress " base-file)
			  final-file (substring base-file 0
						(match-beginning 0))))))
	 (let ((str (concat "executing \"" command "\" ...")))
	   (message str)
	   (gnus-mark-shell-command (point-min) (point-max) command t)
;	   (if final-file
;	      (dired-add-entry-all-buffers directory
;	     (file-name-nondirectory final-file)))
	   (message (concat str " done.")))
	 (cond
	  (tar-p
	   (set-buffer (get-buffer "*Shell Command Output*"))
	   (let ((all (concat command "\n" (buffer-string)))
		 files files-str)
	     (goto-char (point-min))
	     (while (not (eobp))
	       (if (looking-at "^x \\([^,\n]+\\), ")
		   (setq files (cons (buffer-substring
				      (match-beginning 1) (match-end 1))
				     files)))
	       (forward-line 1))
	     (setq files (nreverse files)
		   files-str (mapconcat 'identity files " "))
	     (cond
	      (files
	       (cond
		(gnus-uudecode-auto-chmod
		 (setq command (concat "cd " directory " ; chmod "
				       gnus-uudecode-auto-chmod " " files-str))
		 (gnus-mark-shell-command (point) (point) command nil)
		 (setq all (concat all "\n" command "\n" (buffer-string)))))
	       (cond
		(gnus-uudecode-auto-touch
		 (setq command (concat "cd " directory " ; touch " files-str))
		 (gnus-mark-shell-command (point) (point) command nil)
		 (setq all (concat all "\n" command "\n" (buffer-string)))))
	      (goto-char (point-min))
	      (insert all "\n")
;	      (mapcar (function (lambda (x)
;			(dired-add-entry-all-buffers directory x)))
;		      files)
	      ))))
	  (t
	   (message "wrote file %s" final-file)
	   (let ((case-fold-search t))
	     (cond ((null gnus-uudecode-picture-pattern) nil)
		   ((and (string-match gnus-uudecode-picture-pattern
				       final-file)
			 (y-or-n-p
			   (format "look at the picture in %s? " final-file)))
		    (gnus-mark-shell-command (point) (point)
		      (if (string-match (regexp-quote directory) final-file)
			  (concat "cd " directory " ; "
                                  gnus-uudecode-picture-viewer " "
				  (substring final-file (match-end 0)))
			(concat gnus-uudecode-picture-viewer " " final-file))
		      nil)
		    (if (y-or-n-p (format "delete file %s? " final-file))
			(progn
			  (delete-file final-file)
			  (message "%s deleted." final-file))
			)
		    (display-buffer "*Article*")))))
	  )))
      ;; protected
      (and tmp-buf (kill-buffer tmp-buf)))))


;;; shar (ack pffleughhh barf)

(defvar gnus-unshar-program "/bin/sh"
  "*The program to use to unshar files; you might want to use something
that is less of a gaping security hole than /bin/sh.")

(defvar gnus-unshar-default-directory nil "*")

(defun gnus-unshar-marked-articles (directory)
  "For each of the marked articles, strip the junk from the beginning and end
and then run the result through gnus-unshar-program (typically /bin/sh.)"
  (interactive (list (gnus-mark-read-directory
		       "unshar in directory: " gnus-unshar-default-directory)))
  (setq gnus-unshar-default-directory directory)
  (let (tmp-buf
	(command (concat "cd " directory " ; " gnus-unshar-program)))
    (unwind-protect
      (progn
       (if (setq tmp-buf (get-buffer "*Shell Command Output*"))
	   (save-excursion
	     (set-buffer tmp-buf)
	     (erase-buffer)))
       (setq tmp-buf (get-buffer-create "*gnus-unshar-tmp*"))
       (gnus-Subject-mark-map-articles
	gnus-default-mark-char
	(function (lambda (msg)
	  (message "Snarfing article %s..." (aref msg 0))
	  (if (eq gnus-current-article (aref msg 0))
	      (gnus-Subject-mark-as-read)
	    (gnus-Subject-display-article (aref msg 0)))
	  (set-buffer gnus-Article-buffer)
	  (widen)
	  (set-buffer tmp-buf)
	  (erase-buffer)
	  (insert-buffer gnus-Article-buffer)
	  (or (re-search-forward "^#!" nil t)
	      (re-search-forward "^: This is a shar archive" nil t)
	      (re-search-forward "^# This is a shell archive" nil t)
	      (re-search-forward "^# type \"sh file -c\"." nil t)
	      (re-search-forward "^#!" nil nil)) ; for the error message
	  (beginning-of-line)
	  (delete-region (point-min) (point))
	  (goto-char (point-max))
	  ;; what kind of shithead has a signature after a shar file?
	  (if (re-search-backward "^--" nil t)
	      (delete-region (point) (point-max)))
	  (message "unsharing article %s..." (aref msg 0))
	  (gnus-mark-shell-command (point-min) (point-max) command nil)
	  (message "unsharing article %s...done." (aref msg 0))
	  ))))
      ;; protected
      (kill-buffer tmp-buf)
;      (if (y-or-n-p "Display *Article* buffer? ")
;	  (display-buffer "*Article*"))
      )))

(defvar gnus-save-marked-in-same-file nil
  "*Tells GNUS whether or not to prompt for a filename for each marked 
article being saved.")

;;; Save marked articles in a file
;;; Started: 28 Nov 92, because I need this kind of functionality
;;; By: A1C Tim Miller, tjm@hrt213.brooks.af.mil
;;; Map the function down the marked articles that grabs the article
;;; and saves it in a file, requesting a filename to save in

(defun gnus-save-marked-articles-in-file ()
  "Save marked messages in a file."
  (interactive)
  (let ((state 'first))
    (unwind-protect
	(progn
	  (gnus-Subject-mark-map-articles
	   gnus-default-mark-char
	   (function (lambda (msg)
		       (if (eq state 'first) (setq state t) (setq state nil))
		       (message "Grabbing Article %s..." (aref msg 0))
		       (or (eq gnus-current-article (aref msg 0))
			   (gnus-Subject-display-article (aref msg 0)))
		       (if (and (not state) gnus-save-marked-in-same-file)
			   (gnus-Subject-save-in-file gnus-newsgroup-last-file)
			 (gnus-Subject-save-in-file)))))))))

(provide 'gnus-mark)
