; From: klm@goon.cme.nbs.gov (Ken Manheimer)
; Newsgroups: gnu.emacs
; Subject: Re: BABYL to unix mail format converter?
; Message-ID: <KLM.89Aug17110317@goon.cme.nbs.gov>
; Date: 17 Aug 89 15:03:17 GMT
; References: <31142@cornell.UUCP>
; Organization: Nat'l Institute of Standards and Technology
; Lines: 362
; 
; I developed this script a few weeks ago, made some major changes last
; week, and was getting ready to post to the world (beyond my site)
; today, and then someone lets me know about the current activity on
; just this topic in this newsgroup.
; 
; Below is a set of functions that provide a fairly thorough means to
; generate vm versions of rmail files in your directory hierarchy.  The
; function you probably want to use, if you're like me and have a lot of
; rmail files in some directory hierarchy (or scattered around your
; general directory hierarchy), is the function 'rmail-hierarchy-to-vm'.
; You can do the conversion on a folder-by-folder basis with the
; function 'rmail-folder-to-vm'.  (See comments at the top of the code
; for more details about operation and customization.)
; 
; The folder and hierarchy functions move the original, rmail version of
; the folder to a file with the suffix ".rmail" appended to their name,
; and leave the original name assigned to the vm-translation of the
; folder.  Once you've run it and have satisfied yourself that it worked
; ok you can use, eg, find to identify and delete the "<fn>.rmail" files.
; 
; I was careful to preserve the sundry message characteristics (like
; replied, filed, etc) in the translation, though some of the mapping
; was inferred by reverse engineering and may not be exactly right.
; (I'm inclined to think it is, though - anyone who feels otherwise
; please let me know.)
; 
; One other caution - i added some code to prevent the programs from
; following symbolic links only to discover that pretty much everyone at
; my sight who needed to do the conversion had already used my previous
; release, and so i had no one to test the fairly simple mods.  I did
; some complicated-case tests but couldn't do anything really extensive,
; as i did with the prior release.  Once again, i'm fairly assured about
; the code but if you have any problems (or comments) please let me know
; and i'll look into it asap...
; 
; Ken Manheimer		 	Nat'l Inst of Standards and Technology
; (301) 975-3539			(Formerly "National Bureau of Standards")
; klm@cme.nist.gov		CME, Factory Automation Systems Division
; or ..!uunet!cme-durer!klm	Integrated Systems Group
; 
; "Gadzooks," he said stupidly as he jumped into his convertible lemon and
; drove off with his egg-shaped wife.  - Mad Libs, published example.
; 

; Translate rmail entities (folders residing in a directory hierarchy, folder,
; buffer, and message) into a vm equivalent.
;
; THERE IS NO EXPLICIT OR IMPLICIT WARRANTY ON THIS CODE.  I, the author,
; intend for everyone to have the right to share this code as stated in the
; GNU EMACS GENERAL PUBLIC LICENSE (as stated in a version on or after 11 Feb
; 1988).  In particular, i permit everyone to use it free of charge, and to
; redistribute it in whole or in part free of charge, with the condition that
; no one redistributing it charge for the code itself.
;
; Ken Manheimer	10-Aug-1989 	Nat'l Inst of Standards and Technology
; (301) 975-3539		(Formerly "National Bureau of Standards")
; klm@cme.nist.gov		CME, Factory Automation Systems Division
; or ..!uunet!cme-durer!klm	Integrated Systems Group
;
; The functions fall into two levels.  At the base are rmail-message-to-vm
; and rmail-buffer-to-vm, which do the actual text conversion from rmail to vm.
; They operate on the current buffer and have nothing to do with the business
; of visiting or saving files.  (rmail-buffer-to-vm takes care of the rmail
; file header and then dispatches rmail-message-to-vm to take care of the
; individual messages.)  Above them are the functions that deal with the file-
; system business.
;
; rmail-folder-to-vm actually creates a vm file for a designated rmail file and
; moves the rmail file to "<filename>.rmail", leaving the vm translation as
; <filename>.  Though the original file is renamed, it is not otherwise
; affected.
;
; Finally, rmail-hierarchy-to-vm will traverse an rmail directory
; hierarchy, starting at a source directory you specify, applying
; rmail-folder-to-vm to every rmail folder it finds.  It reports
; each directory that it completes.  This is the one you probably
; want to use if you have a bunch of files to convert.  Symbolic 
; links are not be traversed.  Iff 'rmail-to-vm-ignore-src-backups'
; (default t) is t then backup versions of rmail files (as determined by
; the elisp function 'backup-file-name-p') are skipped.  Iff the variable
; rmail-to-vm-dont-redo (default t) is t then previously processed
; rmail files (as indicated by their having an ".rmail" extension and a
; corresponding file whose name lacks that extension) will not be
; reprocessed.
;
; interactive functions:  perform translation:
; ---------------------	  -------------------
; rmail-hierarchy-to-vm	- create vm versions of any rmail files located in
;			  hierarchy designated by directory argument.  A few
;			  variable (see below) affects whether rmail backup
;			  versions are processed.
; rmail-folder-to-vm	- create vm version of rmail file, moving original
;			  rmail file to same name with ".rmail" appended and
;			  leaving the vm version with the original name.
; rmail-buffer-to-vm	- transform contents of current buffer.  The contents
;			  must start with rmail (ie, "Babyl") header.
; rmail-message-to-vm	- transform next rmail message somewhere after point in
;			  current buffer.  Need not have rmail header.
;
; Customization variables - after loading the file you can do an
; -----------------------   'ESC-x set-variable <var>CR<value>' to alter them.
; rmail-to-vm-ignore-src-backups	- default t
;	if t, rmail-hierarchy-to-vm won't create corresponding vm versions for
;	backups of rmail files (ie, won't process backup files).
; rmail-to-vm-dont-redo			- default t
;	iff t, rmail-folder-to-vm won't process rmail files when they already
;	have a ".rmail" extension and another file exists whose name is the
;	same excluding the ".rmail" suffix
;
; NOTE for all you recursion buffs out there - some of these functions are
; iterative where recursion looks appropriate - it turns out there are some
; stack limits that can be circumvented, but it seemed more expedient (for
; a few reasons) to just unravel some of the recursion to iteration.  (I
; happen to prefer reading and writing recursive code myself...  klm.)

(defconst r-to-v-notice "rmail-to-vm"
  "Preface for rmail-to-vm utility prompts")
(defvar rmail-to-vm-ignore-src-backups t
  "If true, rmail-hierarchy-to-vm skips translating rmail backup files")
(defvar rmail-to-vm-dont-redo t
  "If true, don't process rmail files in hierarchy scan that already have
existing vm versions")

(defconst rmail-file-head-line "^BABYL OPTIONS:.*$" "First line in rmail file")
(defconst rmail-entry-start "\^L\n")
(defconst rmail-entry-end "^\^_")
(defconst rmail-entry-msg-delim "^\\*\\*\\* EOOH \\*\\*\\*\n")
(defconst rmail-attrs "[01],.*\n")
(defconst rmail-attrs-line (concat "^" rmail-attrs))
(defconst rmail-summary-line "^Summary-line:.*$")
(defconst vm-attr-start "X-VM-Attributes: [")
(defconst rtv-done-suffix ".rmail")


(defun rmail-hierarchy-to-vm (srcDir)
  "Apply rmail-folder-to-vm to all rmail folders in hierarchy rooted at SRCDIR.
Non-rmail files in hierarchy ignored.  Original rmail files are renamed to
'<fn>.rmail' (but otherwise unaffected) and new vm versions are given original
name '<fn>'.  If rmail-to-vm-ignore-src-backups t then backup versions aren't
translated."
  (interactive "Drmail-to-vm on hierarchy: ")
					; ensure srcDir is directory format
  (if (file-directory-p srcDir)
      (setq srcDir (file-name-as-directory srcDir))
    (error "rmail-hierarchy-to-vm: %s not a directory" srcDir))
					; iterate through current dir entries 
  (let ((dirEntries (directory-files srcDir)))
    (while dirEntries
      (let ((entry (car dirEntries)))
	(cond						; skip . and ..:
	 ((or (string= entry ".")(string= entry "..")))
					; skip backups if indicated:
	 ((and rmail-to-vm-ignore-src-backups (backup-file-name-p entry)))
					; skip already done files if indicated:
	 ((and rmail-to-vm-dont-redo
	       (rtv-already-did (concat srcDir entry))))
					; don't follow symlinks:
	 ((file-symlink-p (concat srcDir entry))
	  (message "%s: symlink %s disregarded"
		   r-to-v-notice (concat srcDir entry)))
					; disregard unfathomable nonsense:
	 ((not (file-exists-p (concat srcDir entry))))

	 ((file-directory-p (concat srcDir entry))	; recurse on dirs
	  (rmail-hierarchy-to-vm (concat srcDir entry "/")))
	 (t						; translate files
	  (condition-case failure
	      (rmail-folder-to-vm (concat srcDir entry))
	    (file-error (if (not (y-or-n-p
				  (format "can't access %s, continue onwards? "
					  (concat srcDir entry))))
			    (error "rmail-hierarchy-to-vm foiled on %s"
				   (concat srcDir entry))))
	    (error
	     (if (not (y-or-n-p
		       (format "ignoring %s; bad rmail format, continue on? "
					  (concat srcDir entry))))
			    (error "rmail-hierarchy-to-vm foiled on %s"
				   (concat srcDir entry))))))))
      (setq dirEntries (cdr dirEntries))))
  (message "%s %s done." r-to-v-notice srcDir)
  )

(defun rtv-already-did (fn)
  (if (file-exists-p (concat fn ".rmail"))
      t
    (let ((fnlen (length fn))
	  (sufflen (length rtv-done-suffix)))
      (if (string= (substring fn (- fnlen sufflen) fnlen) rtv-done-suffix)
	  (file-exists-p (substring fn 0 (- fnlen sufflen)))))))

(defun rmail-folder-to-vm (src)
  "create vm version of rmail file, leaving original rmail version with
'.rmail' appended on name and leaving the vm version with the original name."
  (interactive "fRmail source folder: ")
  (cond					; validate:
   ((file-directory-p src) (error "Rmail source must not be a directory"))
   ((not (file-exists-p src)) (error "Rmail source %s not found" src))
   ((not (file-readable-p src)) (error "Rmail source %s unreadable" src)))
  (let ((dstBuf (create-file-buffer src)))
    (save-excursion
      (set-buffer dstBuf)		 ; Obtain rmail folder in dstBuf:
      (condition-case failure
	  (insert-file-contents src t)
	(error
	 (progn (set-buffer-modified-p nil)
		(kill-buffer dstBuf)
		(error "can't read %s; %s" src failure))))
      (if (looking-at rmail-file-head-line) ; Do cursory verify of rmail format
	   
	  (progn (condition-case failure
		     (rmail-buffer-to-vm)		; Do translation
		   (error (set-buffer-modified-p nil)
			  (kill-buffer dstBuf)
			  (error "%s bad format, giving up..." src)))

		 (goto-char (point-min))
		 (if (looking-at "\\(\n+\\)From ")
		     (delete-region (match-beginning 1) (match-end 1)))
		 (if (looking-at "From ")	; good enough...
		     (condition-case failure
			 (progn			; mv rmail file aside:
			   (rename-file src (concat src ".rmail") 1)
			   (write-file src))	; save vm version
		       (error
			(progn (set-buffer-modified-p nil)
			       (kill-buffer dstBuf)
			       (error "can't write %s; %s" src failure)))))))
      (set-buffer-modified-p nil)
      (kill-buffer dstBuf)))		; free up buffer
  )

(defun rmail-amt-entries ()
  (let ((count 0) opoint)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
		  (re-search-forward rmail-entry-start nil t))
	(setq count (1+ count))))
    count))

(defun current-match (ord)
  (buffer-substring (match-beginning ord) (match-end ord)))

(defun rmail-buffer-to-vm ()
  "Translate rmail-format contents of current buffer to vm format."
  (interactive)
  (let ((delFrom (point)))		; Delete Babyl header
    (re-search-forward rmail-entry-end)
    (delete-region delFrom (point)))
  (message "%s buffer %s" r-to-v-notice (buffer-file-name))
  (let ((amt-done 0)
	(total-amt (rmail-amt-entries)))
					; Massage messages to vm format
					; while we have more messages:
    (while (rmail-message-to-vm)
      (setq amt-done (1+ amt-done))
      (message "%s buffer %s: %d of %d done"
	       r-to-v-notice (buffer-file-name) amt-done total-amt)
      )

    )
)

(defun rmail-message-to-vm ()
  "Convert message following point in current buffer from rmail to vm format,
or return nil if no message following."
  (interactive)
  (if (re-search-forward
       (concat rmail-entry-start rmail-attrs) (1+ (buffer-size)) t)
      (progn
	(goto-char (match-beginning 0))
	(looking-at rmail-entry-start)
	(delete-region (match-beginning 0)(match-end 0)) ; dispose of delimiter
					; Determine and insert standard
					; mail-entry initial line and vm attrs:
	(let* ((eocm			; End-Of-Current-Message
		(save-excursion (re-search-forward rmail-entry-end)
				(point)))
	       (rmail-attrs-string
		(if (re-search-forward rmail-attrs-line eocm t)
		    (prog1 (current-match 0)
		      (delete-region (match-beginning 0) (match-end 0)))
		  "1,,"))
	       (eocm			; End-Of-Current-Message
		(save-excursion (re-search-forward rmail-entry-end)
				(point)))
					; toggled-header indicates whether
					; stuff after "***EOOH***" is full
					; header or not:
	       (toggled-header (string-match "0," rmail-attrs-string))
	       (new-attr "nil")
	       (unseen-attr
		(if (string-match "unseen" rmail-attrs-string) "t" "nil"))
	       (unread-attr new-attr)
	       (deleted-attr "nil")	; ignore saved "deleted" flags
	       (filed-attr
		(if (string-match "filed" rmail-attrs-string) "t" "nil"))
	       (replied-attr
		(if (string-match "answered"rmail-attrs-string)"t" "nil"))
					; insert mail-format line:
					; "From <user> <date>" 
	       (From-addr-field
		(save-excursion
					; Two main forms -
					; "^From: ProperNm .. <actual@address>"
					; or "^From: actual@address stuff..."
					; then progressively less likely forms
		  (cond ((re-search-forward "^From: .*<\\(.*\\)>" eocm t))
			((re-search-forward "^From: \\([^ \n]*\\)" eocm t))
			((re-search-forward
			  "^Really-From: \\([^ \n]*\\)"eocm t))
			((re-search-forward "^Sender: .*<\\(.*\\)>" eocm t))
			((re-search-forward "^Sender: \\([^ \n]*\\)" eocm t)))
		  (current-match 1)))
	       (From-date-field
		(save-excursion
		  (cond
		   ((re-search-forward
					; Suitable for the mailer at my site -
					; u may need to revise it for yours...
					; klm 19-Jul-1989
		     (concat
			; prelim vv    weekday vv     monthday vv
		      "^\^Iid [^ ]* " "\\([^,]*\\), " "\\([^ ]*\\) "
			; month vv	year vv	      clock time vv
		      "\\([^ ]*\\) " "\\([^ ]*\\) " "\\([^ ]*\\)") eocm t)
		    (concat
		     (current-match 1) " " ; weekday
		     (current-match 3) " " ; month
		     (current-match 2) " " ; monthday
		     (current-match 5) " " ; clock time
		     "19" (current-match 4))) ; year
		   ((re-search-forward "Date: \\(.*\\)$" eocm t)
		    (current-match 1))
		   (t "Previously"))))
	       )
					; Insert mail-entry initial line:
	  (insert
	   (concat "From " From-addr-field " " From-date-field "\n"))
					; Insert vm attributes line:
	  (insert
	   (concat "X-VM-Attributes: ["
		   new-attr " "
		   unseen-attr " "
		   deleted-attr " "
		   filed-attr " "
		   replied-attr "]\n"))
					; deal with digested/uprocessed header:
	  (let* ((eocm			; Recompute End-Of-Current-Message
		  (save-excursion (re-search-forward rmail-entry-end)
				  (point))))
	    (re-search-forward rmail-entry-msg-delim eocm t)
	    (delete-region (1- (match-beginning 0)) (1- (match-end 0)))
	    (if (not toggled-header)
		(delete-region (1- (point))
			       (progn (re-search-forward "^$") (point)))))
	  )
					; Delete entry-end delim, loop to next:
	(re-search-forward rmail-entry-end)
	(delete-backward-char 1)
	t
	)
    )
  )
