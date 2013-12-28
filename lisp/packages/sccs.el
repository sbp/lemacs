;; sccs.el -- easy-to-use SCCS control from within Emacs
;;	@(#)sccs.el	3.5

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
;;;
;;; Author: Eric S. Raymond (eric@snark.thyrsus.com).
;;;
;;; It is distantly derived from an rcs mode written by Ed Simpson
;;; ({decvax, seismo}!mcnc!duke!dukecdu!evs) in years gone by
;;; and revised at MIT's Project Athena.
;;; 
;;; Modified: Made to work for Lucid Emacs by persons who don't know SCCS.
;;; Modified: Ben Wing (Ben.Wing@eng.sun.com) -- fixed up and redid menus
;;;

;; User options

(defvar sccs-bin-directory nil
  "*Directory that holds the SCCS executables.
Initialized automatically the first time you execute an SCCS command,
if not already set.")

(defvar sccs-max-log-size 510
  "*Maximum allowable size of an SCCS log message.")
(defvar sccs-diff-command '("diff" "-c")
  "*The command/flags list to be used in constructing SCCS diff commands.")
(defvar sccs-headers-wanted '("\%\W\%")
  "*SCCS header keywords to be inserted when sccs-insert-header is executed.")
(defvar sccs-insert-static t
  "*Insert a static character string when inserting SCCS headers in C mode.")
(defvar sccs-mode-expert nil
  "*Treat user as expert; suppress yes-no prompts on some things.")

;; Vars the user doesn't need to know about.

(defvar sccs-log-entry-mode nil)
(defvar sccs-current-major-version nil)

;; Some helper functions

(defun sccs-name (file &optional letter)
  "Return the sccs-file name corresponding to a given file."
  (format "%sSCCS/%s.%s"
	  (concat (file-name-directory (expand-file-name file)))
	  (or letter "s")
	  (concat (file-name-nondirectory (expand-file-name file)))))

(defun sccs-lock-info (file index)
   "Return the nth token in a file's SCCS-lock information."
   (let
       ((pfile (sccs-name file "p")))
     (and (file-exists-p pfile)
	  (save-excursion
	    (find-file pfile)
	    (auto-save-mode nil)
	    (replace-string " " "\n")
	    (goto-char (point-min))
	    (forward-line index)
	    (prog1
		(buffer-substring (point) (progn (end-of-line) (point)))
	      (set-buffer-modified-p nil)
	      (kill-buffer (current-buffer)))
	    )
	  )
     )
   )

(defun sccs-locking-user (file)
  "Return the name of the person currently holding a lock on FILE.
Return nil if there is no such person."
  (sccs-lock-info file 2)
  )

(defun sccs-locked-revision (file)
  "Return the revision number currently locked for FILE, nil if none such."
  (sccs-lock-info file 1)
  )

(defmacro error-occurred (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(nil))) '(error t)))

;; There has *got* to be a better way to do this...
(defmacro chmod (perms file)
  (list 'call-process "chmod" nil nil nil perms file))

(defun sccs-save-vars (sid)
  (save-excursion
    (find-file "SCCS/emacs-vars.el")
    (erase-buffer)
    (insert "(setq sccs-current-major-version \"" sid "\")")
    (basic-save-buffer)
    )
  )

(defun sccs-load-vars ()
  (if (error-occurred (load-file "SCCS/emacs-vars.el"))
      (setq sccs-current-major-version "1"))
)

(defun sccs-init-bin-directory ()
  (setq sccs-bin-directory
	(cond ((file-executable-p "/usr/sccs/unget") "/usr/sccs")
	      ((file-executable-p "/usr/bin/unget") "/usr/bin")
	      ((file-directory-p "/usr/sccs") "/usr/sccs")
	      ((file-directory-p "/usr/bin/sccs") "/usr/bin/sccs")
	      (t "/usr/bin"))))

;; The following functions do most of the real work

(defun sccs-get-version (file sid)
   "For the given FILE, retrieve a copy of the version with given SID.
The text is retrieved into a tempfile.  Return the tempfile name, or nil
if no such version exists."
  (let (oldversion vbuf)
    (setq oldversion (sccs-name file (or sid "new")))
    (setq vbuf (create-file-buffer oldversion))
    (prog1
	(if (not (error-occurred
	     (sccs-do-command vbuf "get" file
			      (and sid (concat "-r" sid))
			      "-p" "-s")))
	    (save-excursion
	      (set-buffer vbuf)
	      (write-region (point-min) (point-max) oldversion t 0)
	      oldversion)
	  )
      (kill-buffer vbuf)
      )
    )
  )

(defun sccs-mode-line (file)
  "Set the mode line for an SCCS buffer.
FILE is the file being visited to put in the modeline."
  (setq mode-line-process
	(if (file-exists-p (sccs-name file "p"))
	    (format " <SCCS: %s>" (sccs-locked-revision file))
	  ""))

    ; force update of screen
    (save-excursion (set-buffer (other-buffer)))
    (sit-for 0)
    )

(defun sccs-do-command (buffer command file &rest flags)
  "  Execute an SCCS command, notifying the user and checking for errors."
  (setq file (expand-file-name file))
  (message (format "Running %s on %s..." command file))
  (or sccs-bin-directory (sccs-init-bin-directory))
  (let ((status
	 (save-window-excursion
	   (set-buffer (get-buffer-create buffer))
	   (erase-buffer)
	   (while (and flags (not (car flags)))
	     (setq flags (cdr flags)))
	   (setq flags (append flags (and file (list (sccs-name file)))))
	   (let ((default-directory (file-name-directory (or file "./")))
		 (exec-path (cons sccs-bin-directory exec-path)))
	     (apply 'call-process command nil t nil flags)
	     )
	   (goto-char (point-max))
	   (previous-line 1)
	   (if (looking-at "ERROR")
	       (progn
		 (previous-line 1)
		 (print (cons command flags))
		 (next-line 1)
		 nil)
	     t))))
    (if status
	(message (format "Running %s...OK" command))
      (pop-to-buffer buffer)
      (error (format "Running %s...FAILED" command))))
  (if file (sccs-mode-line file)))

(defun sccs-shell-command (command)
  "Like shell-command except that the *Shell Command Output*buffer
is created even if the command does not output anything"
  (shell-command command)
  (get-buffer-create "*Shell Command Output*"))

(defun sccs-tree-walk (func &rest optargs)
  "Apply FUNC to each SCCS file under the default directory.
If present, OPTARGS are also passed."
  (sccs-shell-command (concat "/bin/ls -1 " default-directory "SCCS/s.*"))
  (set-buffer "*Shell Command Output*")
  (goto-char (point-min))
  (replace-string "SCCS/s." "")
  (goto-char (point-min))
  (if (eobp)
      (error "No SCCS files under %s" default-directory))
  (while (not (eobp))
    (let ((file (buffer-substring (point) (progn (end-of-line) (point)))))
      (apply func file optargs)
      )
    (forward-line 1)
    )
  )

(defun sccs-init ()
  (or (current-local-map) (use-local-map (make-sparse-keymap)))
  (condition-case nil
      ;; If C-c s is already defined by another mode, then we
      ;; will get an error.  In that case, just don't do anything.
      (progn
	(define-key (current-local-map) "\C-cs?" 'describe-mode)
	(define-key (current-local-map) "\C-csn" 'sccs)
	(define-key (current-local-map) "\C-csm" 'sccs-register-file)
	(define-key (current-local-map) "\C-csh" 'sccs-insert-headers)
	(define-key (current-local-map) "\C-csd" 'sccs-revert-diff)
	(define-key (current-local-map) "\C-csp" 'sccs-prs)
	(define-key (current-local-map) "\C-csr" 'sccs-revert-buffer)
	(define-key (current-local-map) "\C-cs\C-d" 'sccs-version-diff)
	(define-key (current-local-map) "\C-cs\C-p" 'sccs-pending)
	(define-key (current-local-map) "\C-cs\C-r" 'sccs-registered)
	)
    (error nil)))

;; Here's the major entry point

(defun sccs (verbose)
  "*Do the next logical SCCS operation on the file in the current buffer.
You must have an SCCS subdirectory in the same directory as the file being
operated on.
   If the file is not already registered with SCCS, this does an admin -i
followed by a get -e.
   If the file is registered and not locked by anyone, this does a get -e.
   If the file is registered and locked by the calling user, this pops up a
buffer for creation of a log message, then does a delta -n on the file.
A read-only copy of the changed file is left in place afterwards.
   If the file is registered and locked by someone else, an error message is
returned indicating who has locked it."
  (interactive "P")
  (sccs-init)
  (if (buffer-file-name)
      (let
	  (do-update revision owner
		     (file (buffer-file-name))
		     (sccs-file (sccs-name (buffer-file-name)))
		     (sccs-log-buf (get-buffer-create "*SCCS-Log*"))
		     (err-msg nil))

	;; if there is no SCCS file corresponding, create one
	(if (not (file-exists-p sccs-file))
	    (progn
	      (sccs-load-vars)
	      (sccs-admin 
	       file
	       (cond 
		(verbose (read-string "Initial SID: "))
		((error-occurred (load-file "SCCS/emacs-vars.el")) "1")
		(t sccs-current-major-version))
	       )
	      )
	  )

	(cond

	 ;; if there is no lock on the file, assert one and get it
	 ((not (file-exists-p (sccs-name file "p")))
	  (progn
	    (sccs-get file t)
	    (revert-buffer nil t)
	    (sccs-mode-line file)
	    ))

	 ;; a checked-out version exists, but the user may not own the lock
	 ((not (string-equal
		(setq owner (sccs-locking-user file)) (user-login-name)))
	  (error "Sorry, %s has that file checked out" owner))

	 ;; OK, user owns the lock on the file 
	 (t (progn

	      ;; if so, give luser a chance to save before delta-ing.
	      (if (and (buffer-modified-p)
		       (or
			sccs-mode-expert
			(y-or-n-p (format "%s has been modified. Write it out? "
					  (buffer-name)))))
		       (save-buffer))

	      (setq revision (sccs-locked-revision file))

	      ;; user may want to set nonstandard parameters
	      (if verbose
		  (if (or sccs-mode-expert (y-or-n-p 
		       (format "SID: %s  Change revision level? " revision)))
		      (setq revision (read-string "New revision level: "))))

	      ;; OK, let's do the delta
	      (if
		  ;; this excursion returns t if the new version was saved OK
		  (save-window-excursion
		    (pop-to-buffer (get-buffer-create "*SCCS*"))
		    (erase-buffer)
		    (set-buffer-modified-p nil)
		    (sccs-mode)
		    (message 
		     "Enter log message. Type C-c C-c when done, C-c ? for help.")
		    (prog1
			(and (not (error-occurred (recursive-edit)))
			     (not (error-occurred (sccs-delta file revision))))
		      (setq buffer-file-name nil)
		      (bury-buffer "*SCCS*")))

		  ;; if the save went OK do some post-checking
		  (if (buffer-modified-p)
		      (error
		       "Delta-ed version of file does not match buffer!")
		    (progn
		      ;; sccs-delta already turned off write-privileges on the
		      ;; file, let's not re-fetch it unless there's something
		      ;; in it that get would expand
		      ;;
		      ;; fooey on this.  You always need to refetch the
		      ;; file; otherwise weirdness will ensue when you're
		      ;; trying to do a make. --bpw
		      ; (if (sccs-check-headers)
		      (sccs-get file nil)
		      (revert-buffer nil t)
		      (sccs-mode-line file)
		      (run-hooks 'sccs-delta-ok)
		      )
		    ))))))
    (error "There is no file associated with buffer %s" (buffer-name))))

(defun sccs-insert-last-log ()
  "*Insert the log message of the last SCCS check in at point."
  (interactive)
  (insert-buffer sccs-log-buf))

;;; These functions help the sccs entry point

(defun sccs-get (file writeable)
  "Retrieve a copy of the latest delta of the given file."
    (sccs-do-command "*SCCS*" "get" file (if writeable "-e")))

(defun sccs-admin (file sid)
  "Checks a file into sccs.
FILE is the unmodified name of the file.  SID should be the base-level sid to
check it in under."
  ; give a change to save the file if it's modified
  (if (and (buffer-modified-p)
	   (y-or-n-p (format "%s has been modified. Write it out? "
			     (buffer-name))))
      (save-buffer))
  (sccs-do-command "*SCCS*" "admin" file
		   (concat "-i" file) (concat "-r" sid))
  (chmod "-w" file)
  (if (sccs-check-headers)
      (sccs-get file nil))	;; expand SCCS headers
  (revert-buffer nil t)
  (sccs-mode-line file)
)

(defun sccs-delta (file &optional rev comment)
   "Delta the file specified by FILE.
The optional argument REV may be a string specifying the new revision level
\(if nil increment the current level). The file is retained with write
permissions zeroed. COMMENT is a comment string; if omitted, the contents of
the current buffer up to point becomes the comment for this delta."
  (if (not comment)
      (progn
	(goto-char (point-max))
	(if (not (bolp)) (newline))
	(newline)
	(setq comment (buffer-substring (point-min) (1- (point)))))
    )
  (sccs-do-command "*SCCS*" "delta" file "-n"
	   (if rev (format "-r%s" rev))
	   (format "-y%s" comment))
  (chmod "-w" file))

(defun sccs-delta-abort ()
  "Abort an SCCS delta command."
  (interactive)
  (if (or sccs-mode-expert (y-or-n-p "Abort the delta? "))
      (progn
	(delete-window)
	(error "Delta aborted")))
  )

(defun sccs-log-exit ()
  "Leave the recursive edit of an SCCS log message."
  (interactive)
  (if (< (buffer-size) sccs-max-log-size)
	 (progn
	   (copy-to-buffer sccs-log-buf (point-min) (point-max))
	   (exit-recursive-edit)
	   (delete-window))
	 (progn
	   (goto-char sccs-max-log-size)
	   (error
	    "Log must be less than %d characters. Point is now at char %d."
	    sccs-max-log-size sccs-max-log-size)))
)

;; Additional entry points for examining version histories

(defun sccs-revert-diff (&rest flags)
  "*Compare the version being edited with the last checked-in revision.
Or, if given a prefix argument, with another specified revision."
  (interactive)
  (let (old file)
    (if
	(setq old (sccs-get-version (buffer-file-name) 
				    (and
				     current-prefix-arg
				     (read-string "Revision to compare against: "))
				    ))
	(progn
	  (if (and (buffer-modified-p)
		   (or
		    sccs-mode-expert
		    (y-or-n-p (format "%s has been modified. Write it out? "
				      (buffer-name)))))
	      (save-buffer))

	  (setq file (buffer-file-name))
	  (set-buffer (get-buffer-create "*SCCS*"))
	  (erase-buffer)
	  (apply 'call-process (car sccs-diff-command) nil t nil
		 (append (cdr sccs-diff-command) flags (list old) (list file)))
	  (set-buffer-modified-p nil)
	  (goto-char (point-min))
	  (delete-file old)
	  (if (equal (point-min) (point-max))
	      (message (format "No changes to %s since last get." file))
	      (pop-to-buffer "*SCCS*")
	      )
	  )
      )
    )
  )

(defun sccs-prs ()
  "*List the SCCS log of the current buffer in an emacs window."
  (interactive)
  (if (and buffer-file-name (file-exists-p (sccs-name buffer-file-name "s")))
      (progn
	(sccs-do-command "*SCCS*" "prs" buffer-file-name)
	(pop-to-buffer (get-buffer-create "*SCCS*"))
	)
    (error "There is no SCCS file associated with this buffer")
    )
  )

(defun sccs-version-diff (file rel1 rel2)
  "*For FILE, report diffs between two stored deltas REL1 and REL2 of it."
  (interactive "fFile: \nsOlder version: \nsNewer version: ")
  (if (string-equal rel1 "") (setq rel1 nil))
  (if (string-equal rel2 "") (setq rel2 nil))
  (set-buffer (get-buffer-create "*SCCS*"))
  (erase-buffer)
  (sccs-vdiff file rel1 rel2)
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (if (equal (point-min) (point-max))
      (message (format "No changes to %s between %s and %s." file rel1 rel2))
    (pop-to-buffer "*SCCS*")
    )
  )

(defun sccs-vdiff (file rel1 rel2 &optional flags)
  "Compare two deltas into the current buffer."
  (let (vers1 vers2)
    (and
     (setq vers1 (sccs-get-version file rel1))
     (setq vers2 (if rel2 (sccs-get-version file rel2) file))
;     (prog1
;	 (save-excursion
;	   (not (error-occurred
;		 (call-process "prs" nil t t
;			       (sccs-name file))))
;	 )
;       )
     (unwind-protect
	 (apply 'call-process (car sccs-diff-command) nil t t
		(append (cdr sccs-diff-command) flags (list vers1) (list vers2)))
       (condition-case () (delete-file vers1) (error nil))
       (if rel2
	   (condition-case () (delete-file vers2) (error nil)))
       )
     )
    )
  )

;; SCCS header insertion code

(defun sccs-insert-headers ()
  "*Insert headers for use with the Source Code Control System.
Headers desired are inserted at the start of the buffer, and are pulled from 
the variable sccs-headers-wanted"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (if (or (not (sccs-check-headers))
	      (y-or-n-p "SCCS headers already exist.  Insert another set?"))
	  (progn
	     (goto-char (point-min))
	     (run-hooks 'sccs-insert-headers-hook)
	     (cond ((eq major-mode 'c-mode) (sccs-insert-c-header))
		   ((eq major-mode 'lisp-mode) (sccs-insert-lisp-header))
		   ((eq major-mode 'emacs-lisp-mode) (sccs-insert-lisp-header))
		   ((eq major-mode 'scheme-mode) (sccs-insert-lisp-header))
		   ((eq major-mode 'nroff-mode) (sccs-insert-nroff-header))
		   ((eq major-mode 'plain-tex-mode) (sccs-insert-tex-header))
		   ((eq major-mode 'texinfo-mode) (sccs-insert-texinfo-header))
		   (t (sccs-insert-generic-header))))))))

(defun sccs-insert-c-header ()
  (let (st en)
    (insert "/*\n")
    (mapcar '(lambda (s)
	       (insert " *\t" s "\n"))
	    sccs-headers-wanted)
    (insert " */\n\n")
    (if (and sccs-insert-static 
	     (not (string-match "\\.h$" (buffer-file-name))))
	(progn
	  (insert "#ifndef lint\n"
		  "static char *sccsid")
;;	  (setq st (point))
;;	  (insert (file-name-nondirectory (buffer-file-name)))
;;	  (setq en (point))
;;	  (subst-char-in-region st en ?. ?_)
	  (insert " = \"\%\W\%\";\n"
		  "#endif /* lint */\n\n")))
    (run-hooks 'sccs-insert-c-header-hook)))

(defun sccs-insert-lisp-header ()
  (mapcar '(lambda (s) 
		  (insert ";;;\t" s "\n"))
	  sccs-headers-wanted)
  (insert "\n")
  (run-hooks 'sccs-insert-lisp-header-hook))

(defun sccs-insert-nroff-header ()
  (mapcar '(lambda (s) 
		  (insert ".\\\"\t" s "\n"))
	  sccs-headers-wanted)
  (insert "\n")
  (run-hooks 'sccs-insert-nroff-header-hook))

(defun sccs-insert-tex-header ()
  (mapcar '(lambda (s) 
		  (insert "%%\t" s "\n"))
	  sccs-headers-wanted)
  (insert "\n")
  (run-hooks 'sccs-insert-tex-header-hook))

(defun sccs-insert-texinfo-header ()
  (mapcar '(lambda (s) 
		  (insert "@comment\t" s "\n"))
	  sccs-headers-wanted)
  (insert "\n")
  (run-hooks 'sccs-insert-texinfo-header-hook))

(defun sccs-insert-generic-header ()
  (let* ((comment-start-sccs (or comment-start "#"))
	 (comment-end-sccs (or comment-end ""))
	 (dont-insert-nl-p (string-match "\n" comment-end-sccs)))
    (mapcar '(lambda (s)
	       (insert comment-start-sccs "\t" s ""
		       comment-end-sccs (if dont-insert-nl-p "" "\n")))
	  sccs-headers-wanted)
  (insert comment-start-sccs comment-end-sccs (if dont-insert-nl-p "" "\n"))))

(defun sccs-check-headers ()
  "Check if the current file has any SCCS headers in it."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward  "%[MIRLBSDHTEGUYFPQCZWA]%" (point-max) t)))

;; Status-checking functions

(defun sccs-status (prefix legend)
   "List all files underneath the current directory matching a prefix type."
   (sccs-shell-command
    (concat "/bin/ls -1 SCCS/" prefix ".*"))
   (if
       (save-excursion
	 (set-buffer "*Shell Command Output*")
	 (if (= (point-max) (point-min))
	     (not (message
		   "No files are currently %s under %s"
		   legend default-directory))
	   (progn
	     (goto-char (point-min))
	     (insert
	      "The following files are currently " legend
	      " under " default-directory ":\n")
	     (replace-string (format "SCCS/%s." prefix) "")
	     )
	   )
	 )
       (pop-to-buffer "*Shell Command Output*")
       )
     )

(defun sccs-pending ()
  "*List all files currently SCCS locked."
  (interactive)
  (sccs-status "p" "locked"))

(defun sccs-registered ()
  "*List all files currently SCCS registered."
  (interactive)
  (sccs-status "s" "registered"))
       
(defun sccs-register-file (override)
  "*Register the file visited by the current buffer into SCCS."
  (interactive "P")
  (if (file-exists-p (sccs-name (buffer-file-name)))
      (error "This file is already registered into SCCS.")
    (progn
      (if (and (buffer-modified-p)
	       (or
		sccs-mode-expert
		(y-or-n-p (format "%s has been modified. Write it out? "
				  (buffer-name)))))
	  (save-buffer))
      (sccs-load-vars)
      (sccs-admin 
       (buffer-file-name)
       (cond 
	(override (read-string "Initial SID: "))
	((error-occurred (load-file "SCCS/emacs-vars.el")) "1")
	(t sccs-current-major-version))
       )
      )
    )
  )

;; Major functions for release-tracking and generation.

(defun sccs-release-diff (rel1 rel2)
  "*Diff all files below default-directory between versions REL1 and REL2.
The report goes to a shell output buffer which is popped to.  If REL2 is
omitted or nil, the comparison is done against the most recent version."
  (interactive "sOlder version: \nsNewer version: ")
  (if (string-equal rel1 "") (setq rel1 nil))
  (if (string-equal rel2 "") (setq rel2 nil))
  (sccs-shell-command (concat
		       "/bin/ls -1 " default-directory "SCCS/s.*"
		       ))
  (set-buffer "*Shell Command Output*")
  (goto-char (point-min))
  (replace-string "SCCS/s." "")
  (goto-char (point-min))
  (if (eobp)
      (error "No SCCS files under %s" default-directory))
  (let
      ((sccsbuf (get-buffer-create "*SCCS*")))
    (save-excursion
      (set-buffer sccsbuf)
      (erase-buffer)
      (insert (format "Diffs from %s to %s.\n\n"
		      (or rel1 "current") (or rel2 "current"))))
    (while (not (eobp))
	 (let ((file (buffer-substring (point) (progn (end-of-line) (point)))))
	   (save-excursion
	     (set-buffer sccsbuf)
	     (set-buffer-modified-p nil)

	     (sccs-vdiff file rel1 rel2)
	     (if (buffer-modified-p)
		 (insert "\n"))
	     )
	   (forward-line 1)
	   )
	 )
    (kill-buffer "*Shell Command Output*")
    (pop-to-buffer sccsbuf)
    (insert "\nEnd of diffs.\n")
    (goto-char (point-min))
    (replace-string (format "/SCCS/%s." rel1) "/")
    (goto-char (point-min))
    (replace-string (format "/SCCS/%s." rel2) "/new/")
    (goto-char (point-min))
    (replace-string "/SCCS/new." "/new/")
    (goto-char (point-min))
    (replace-regexp (concat "^*** " default-directory) "*** ")
    (goto-char (point-min))
    (replace-regexp (concat "^--- " default-directory) "--- ")
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    )
  )

(defun sccs-dummy-delta (file sid)
  "Make a dummy delta to the given FILE with the given SID."
  (interactive "sFile: \nsRelease ID: ")
  (if (not (sccs-locked-revision file))
      (sccs-get file t))
  ;; Grottiness alert -- to get around SCCS's obsessive second-guessing we
  ;; have to mung the p-file
  (save-excursion
    (let ((pfile (sccs-name file "p")))
      (chmod "u+w" pfile)
      (find-file pfile)
      (auto-save-mode nil)
      (replace-regexp "^\\([0-9.]+\\) \\([0-9.]+\\)" (concat "\\1 " sid) t)
      (write-region (point-min) (point-max) pfile t 0)
      (chmod "u-w" pfile)
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))
      )
    )
  (sccs-delta file sid (concat "Release " sid))
  (sccs-get file nil)
  (sccs-save-vars sid)
  )

(defun sccs-delta-release (sid)
  "*Delta everything underneath the current directory to mark it as a release."
  (interactive "sRelease: ")
  (sccs-tree-walk 'sccs-dummy-delta sid)
  (kill-buffer "*SCCS*")
  )

;; Miscellaneous other entry points

(defun sccs-revert-buffer ()
  "*Revert the current buffer's file back to the last saved version."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (y-or-n-p (format "Revert file %s to last SCCS revision?" file))
	(progn
	  (delete-file file)
	  (delete-file (sccs-name file "p"))
	  (rename-file (sccs-get-version file nil) file)
	  (chmod "-w" file)
	  (revert-buffer nil t)
	  (sccs-mode-line file)))))

(defun sccs-rename-file (old new)
  "*Rename a file, taking its SCCS files with it."
  (interactive "fOld name: \nFNew name: ")
  (let ((owner (sccs-locking-user old)))
    (if (and owner (not (string-equal owner (user-login-name))))
	(error "Sorry, %s has that file checked out" owner))
    )
  (rename-file old new)
  (if (file-exists-p (sccs-name old "p"))
      (rename-file (sccs-name old "p") (sccs-name new "p")))
  (if (file-exists-p (sccs-name old "s"))
      (rename-file (sccs-name old "s") (sccs-name new "s")))
  )

;; Set up key bindings for SCCS use, e.g. while editing log messages

(defun sccs-mode ()
  "Minor mode for driving the SCCS tools.

These bindings are added to the global keymap when you enter this mode:
\\[sccs]	perform next logical SCCS operation (`sccs') on current file
\\[sccs-register-file]		register current file into SCCS
\\[sccs-insert-headers]		insert SCCS headers in current file
\\[sccs-prs]		display change history of current file
\\[sccs-revert-buffer]		revert buffer to last saved version
\\[sccs-revert-diff]		show difference between buffer and last saved delta
\\[sccs-pending]		show all files currently locked by any user in or below .
\\[sccs-registered]		show all files registered into SCCS in or below .
\\[sccs-version-diff]		show diffs between saved versions for all files in or below .

When you generate headers into a buffer using C-c h, the value of
sccs-insert-headers-hook is called before insertion. If the file is
recognized a C or Lisp source, sccs-insert-c-header-hook or
sccs-insert-lisp-header-hook is called after insertion respectively.

While you are entering a change log message for a delta, the following
additional bindings will be in effect.

\\[sccs-log-exit]		proceed with check in, ending log message entry
\\[sccs-insert-last-log]		insert log message from last check-in
\\[sccs-delta-abort]		abort this delta check-in

Entry to the change-log submode calls the value of text-mode-hook, then
the value sccs-mode-hook.

Global user options:
        sccs-mode-expert        suppresses some conformation prompts,
				notably for delta aborts and file saves.
	sccs-max-log-size	specifies the maximum allowable size
				of a log message plus one.
	sccs-diff-command	A list consisting of the command and flags
				to be used for generating context diffs.
	sccs-headers-wanted	which %-keywords to insert when adding
				SCCS headers with C-c h
	sccs-insert-static	if non-nil, SCCS keywords inserted in C files
				get stuffed in a static string area so that
				what(1) can see them in the compiled object
				code.
"
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map sccs-log-entry-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'sccs-mode)
  (setq mode-name "SCCS Change Log Entry")
  (run-hooks 'text-mode-hook 'sccs-mode-hook)
)

;; Initialization code, to be done just once at load-time
(if sccs-log-entry-mode
    nil
  (setq sccs-log-entry-mode (make-sparse-keymap))
  (define-key sccs-log-entry-mode "\C-ci" 'sccs-insert-last-log)
  (define-key sccs-log-entry-mode "\C-c\C-i" 'sccs-insert-last-log)
  (define-key sccs-log-entry-mode "\C-ca" 'sccs-delta-abort)
  (define-key sccs-log-entry-mode "\C-c\C-a" 'sccs-delta-abort)
  (define-key sccs-log-entry-mode "\C-c\C-c" 'sccs-log-exit)
  (define-key sccs-log-entry-mode "\C-x\C-s" 'sccs-log-exit)
  )


;;; Lucid Emacs support

(defconst sccs-menu
  '("SCCS Commands"

    ["SCCS"			sccs			t	nil] ; C-c s n
    ["Insert Headers"		sccs-insert-headers	t]	     ; C-c s h
    ["Archive History:"		sccs-prs		t	nil] ; C-c s p
    ["Diffs from Archive:"	sccs-revert-diff	t	nil] ; C-c s d
    ["Revert to Archive:"	sccs-revert-buffer	t	nil] ; C-c s r
    "----"
    ["Check In..."		sccs-dummy-delta	t]
    ["Create Archive..."	sccs-register-file	t] ; C-c s h
    ["Rename Archive..."	sccs-rename-file	t]
    "----"
    ["List Checked-Out Files"	sccs-pending		t]	   ; C-c s C-p
    ["List Registered Files"	sccs-registered		t]	   ; C-c s C-r
    ["Diff Directory"		sccs-release-diff	t]
    ["Delta Directory"		sccs-delta-release	t]
    ))

(progn
  (delete-menu-item '("SCCS"))
  (add-menu '() "SCCS" (cdr sccs-menu)))

(defun sccs-sensitize-menu ()
  (let* ((rest (cdr (car (find-menu-item current-menubar '("SCCS")))))
	 (case-fold-search t)
	 (file (if buffer-file-name
		   (file-name-nondirectory buffer-file-name)
		 (buffer-name)))
	 (dir (file-name-directory
	       (if buffer-file-name buffer-file-name default-directory)))
	 (sccs-file (and buffer-file-name (sccs-name buffer-file-name)))
	 (known-p (and sccs-file (file-exists-p sccs-file)))
	 (checked-out-p (and known-p
			     (file-exists-p (sccs-name buffer-file-name "p"))))
	 command
	 item)
    (while rest
      (setq item (car rest))
      (if (not (vectorp item))
	  nil
	(setq command (aref item 1))
	(if (eq 'sccs command)
	    (aset item 0
		  (cond ((or (null sccs-file) (not known-p))
			 "Create Archive:")
			((not checked-out-p)
			 "Check Out")
			(t
			 "Check In"))))
	(cond
	 ((and (> (length item) 3)
	       (string-match "directory" (aref item 0)))
	  (aset item 3 dir))
	 ((> (length item) 3)
	  (aset item 3 file))
	 (t nil))
	(aset item 2
	      (cond
	       ((memq command '(sccs-prs))
		known-p)
	       ((memq command '(sccs-revert-diff sccs-revert-buffer))
		checked-out-p)
	       (t))))
	(setq rest (cdr rest))))
  nil)

(add-hook 'activate-menubar-hook 'sccs-sensitize-menu)

;; sccs.el ends here
