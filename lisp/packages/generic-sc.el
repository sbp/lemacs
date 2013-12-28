;; Generic Interface to Source Control Systems, devin@lucid.com
;; Copyright (C) 1992-1993 Free Software Foundation, Inc.

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

;; The generic interface provide a common set of functions that can be
;; used to interface with a source control system like SCCS, RCS or
;; CVS.  
;; 
;; You chose which source control system to use by calling sc-mode
;; 
;; The module is based on the sccs.el mode of Eric S. Raymond
;; (eric@snark.thyrsus.com) which was distantly derived from an rcs
;; mode written by Ed Simpson ({decvax, seismo}!mcnc!duke!dukecdu!evs)
;; in years gone by and revised at MIT's Project Athena.

;;; This can be customized by the user
(defvar sc-diff-command '("diff")
  "*The command/flags list to be used in constructing diff commands.")

(defvar sc-mode-expert ()
  "*Treat user as expert; suppress yes-no prompts on some things.")

(defvar sc-max-log-size 510
  "*Maximum allowable size of a source control log message.")

;; default keybindings
(defvar sc-prefix-map (lookup-key global-map "\C-xv"))
(if (not (keymapp sc-prefix-map))
    (progn
      (setq sc-prefix-map (make-sparse-keymap))
      (define-key global-map "\C-xv" sc-prefix-map)
      (define-key sc-prefix-map "v" 'sc-next-operation)
      (define-key sc-prefix-map "=" 'sc-show-changes)
      (define-key sc-prefix-map "l" 'sc-show-history)
      (define-key sc-prefix-map "p" 'sc-visit-previous-revision)
      (define-key sc-prefix-map "u" 'sc-revert-file)
      (define-key sc-prefix-map "d" 'sc-list-registered-files)
      (define-key sc-prefix-map "\C-d" 'sc-update-directory)
      (define-key sc-prefix-map "\C-r" 'sc-rename-file)
      ))


;;; The user does not change these
(defvar sc-generic-name ""
  "Name of the source control system used.  Is displayed in the modeline.")

(defvar sc-mode-line-string ()
  "Revision number to show in the mode line")

(defvar sc-generic-log-buf ()
  "Buffer for entering log message")

(defvar sc-log-entry-keymap ()
  "Additional keybindings used when entering the log message")

(defmacro chmod (perms file)
  (list 'call-process "chmod" nil nil nil perms file))

(defmacro error-occurred (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(nil))) '(error t)))


;;; User level functions
(defun sc-next-operation (verbose)
  "Do the next logical source-control operation on the file in the current buffer.
The current subdirectory must be under source control.
   If the file is not already registered with the source control, this registers it 
and checks it out.
   If the file is registered and not locked by anyone, this checks it out.
   If the file is registered and locked by the calling user, this pops up a
buffer for creation of a log message, then checks the file in.
A read-only copy of the changed file is left in place afterwards.
   If the file is registered and locked by someone else, an error message is
returned indicating who has locked it."
  (interactive "P")
  (if (not buffer-file-name)
      (error "There is no file associated with buffer %s" (buffer-name)))
  (let* (revision
	 (file buffer-file-name)
	 (lock-info (sc-lock-info file))
	 (sc-generic-log-buf
	  (get-buffer-create (format "*%s-Log*" sc-generic-name)))
	 (err-msg nil))
	
    ;; if file is not registered register it and set lock-info to show it's not locked
    (if (not lock-info)
	(progn
	  (sc-register-file verbose)
	  (setq lock-info (list () ()))))
	
    (cond ((not (car lock-info))
	   ;; if there is no lock on the file, assert one and get it
	   (sc-check-out file t)
	   (revert-buffer nil t)
	   (sc-mode-line))
	      
	  ((not (string-equal (car lock-info) (user-login-name)))
	   ;; file is locked by someone else
	   (error "Sorry, %s has that file locked." (car lock-info)))

	  (t 
	   ;; OK, user owns the lock on the file 
	   ;; if so, give user a chance to save before delta-ing.
	   (if (and (buffer-modified-p)
		    (or
		     sc-mode-expert
		     (y-or-n-p (format "%s has been modified. Write it out? "
				       (buffer-name)))))
	       (save-buffer))
	       
	   (setq revision (car (cdr lock-info)))
	       
	   ;; user may want to set nonstandard parameters
	   (if verbose
	       (if (or sc-mode-expert
		       (y-or-n-p 
			(format "revision: %s  Change revision level? "
				revision)))
		   (setq revision (read-string "New revision level: "))))
	       
	   ;; OK, let's do the delta
	   (let ((buffer (sc-temp-buffer)))
	     (if (save-window-excursion
		   ;; this excursion returns t if the new version was saved OK
		   (pop-to-buffer buffer)
		   (erase-buffer)
		   (set-buffer-modified-p nil)
		   (sc-log-entry-mode)
		   (message 
		    "Enter log message. Type C-c C-c when done, C-c ? for help.")
		   (prog1
		       (and (not (error-occurred (recursive-edit)))
			    (not (error-occurred
				  (sc-check-in file revision
					       (buffer-string)))))
		     (setq buffer-file-name nil)
		     (bury-buffer buffer)))
		   
		 ;; if the save went OK do some post-checking
		 (if (buffer-modified-p)
		     (error
		      "Checked-in version of file does not match buffer!")
		   (revert-buffer nil t)
		   (sc-mode-line)
		   (run-hooks 'sc-check-in-ok))))))))

(defun sc-insert-last-log ()
  "Insert the log message of the last check in at point."
  (interactive)
  (insert-buffer sc-generic-log-buf))

(defun sc-abort-check-in ()
  "Abort a source control check-in command."
  (interactive)
  (if (or sc-mode-expert (y-or-n-p "Really Abort Check-in? "))
      (progn
	(delete-window)
	(abort-recursive-edit))))

(defun sc-log-exit ()
  "Proceed with checkin with the contents of the current buffer as message."
  (interactive)
  (if (< (buffer-size) sc-max-log-size)
      (progn
	(copy-to-buffer sc-generic-log-buf (point-min) (point-max))
	(exit-recursive-edit)
	(delete-window))
    (goto-char sc-max-log-size)
    (error
     "Log must be less than %d characters. Point is now at char %d."
     sc-max-log-size (point))))


;;; Functions to look at the edit history
(defun sc-show-changes (arg)
  "Compare the version being edited with the last checked-in revision.
With a prefix argument prompt for revision to compare with."
  (interactive "P")
  ;; check that the file is not modified
  (if (and (buffer-modified-p)
	   (or
	    sc-mode-expert
	    (y-or-n-p (format "%s has been modified. Write it out? "
			      (buffer-name)))))
      (save-buffer))
  (let* ((revision (and arg (read-string "Revision to compare against: ")))
	 (file buffer-file-name)
	 (name (file-name-nondirectory file))
	 (old (sc-get-version-in-temp-file file revision))
	 (buffer (sc-temp-buffer))
	 status)
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (setq default-directory (file-name-directory file))
      (setq status
	    (apply 'call-process (car sc-diff-command) () t ()
		   (append (cdr sc-diff-command) (list old) (list file)))))
    (if (not (or (eq 0 status) (eq 1 status))) ; see man diff.1
	(progn
	  (display-buffer buffer)
	  (error "diff FAILED")))
    (delete-file old)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (if (equal (point-min) (point-max))
	  (insert
	   (format "No changes to %s since last update."
		   (file-name-nondirectory file)))
	(insert "==== Diffs for " file "\n")
	(insert "==== ")
	(mapcar '(lambda (i) (insert i " ")) sc-diff-command)
	(insert name "<" (or revision "current") ">" " " name "\n\n")))
    (display-buffer buffer)))

(defun sc-show-revision-changes ()
  "Prompt for a revision to diff against."
  (interactive)
  (sc-show-changes 4))

(defun sc-version-diff-file (file rel1 rel2)
  "For FILE, report diffs between two revisions REL1 and REL2 of it."
  (interactive "fFile: \nsOlder version: \nsNewer version: ")
  (if (string-equal rel1 "") (setq rel1 nil))
  (if (string-equal rel2 "") (setq rel2 nil))
  (let ((buffer (sc-temp-buffer)))
    (set-buffer buffer)
    (erase-buffer)
    (let ((v1 (sc-get-version-in-temp-file file rel1))
	  (v2 (if rel2 (sc-get-version-in-temp-file file rel2) file)))
      (and v1
	   v2
	   (unwind-protect
	       (apply 'call-process (car sc-diff-command) nil t t
		      (append (cdr sc-diff-command) (list v1) (list v2)))))
      (condition-case () (delete-file v1) (error nil))
      (if rel2
	  (condition-case () (delete-file v2) (error nil)))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (if (equal (point-min) (point-max))
	  (message
	   (format "No changes to %s between %s and %s." file rel1 rel2))
	(display-buffer buffer)))))

(defun sc-show-history ()
  "List the edit history of the current buffer."
  (interactive)
  (let ((file buffer-file-name))
    (if (not file)
	(error "There is no file associated with buffer %s" (buffer-name)))
    (if (not (sc-lock-info file))
	(error "The file is not registered in the source control system"))
    (let ((buffer (sc-temp-buffer)))
      (save-excursion
	(set-buffer buffer)
	(erase-buffer)
	(sc-history file)
	(goto-char (point-min)))
      (display-buffer buffer))))

(defun sc-visit-previous-revision (revision)
  "Show a previous revision of the current file"
  (interactive "sShow previous revision number: ")
  (let ((file buffer-file-name))
    (if (not file)
	(error "There is no file associated with buffer %s" (buffer-name)))
    (let ((other-file (sc-get-version-in-temp-file file revision))
	  (buffer-name (concat (file-name-nondirectory file)
			       "<" sc-generic-name " " revision ">")))
      (pop-to-buffer (get-buffer-create buffer-name))
      (erase-buffer)
      (insert-file other-file)
      ;; get the same major mode as the original file
      (setq buffer-file-name file)
      (normal-mode)
      (setq buffer-file-name ())
      (set-buffer-modified-p ())
      (toggle-read-only)
      (delete-file other-file))))

(defun sc-revert-file ()
  "Revert the current buffer's file back to the last saved version."
  (interactive)
  (let ((file buffer-file-name))
    (if (y-or-n-p (format "Revert file %s to last checked-in revision?" file))
	(progn
	  (sc-revert file)
	  (revert-buffer nil t)
	  (sc-mode-line)))))

;; Functions to get directory level information

(defun sc-list-all-locked-files (arg)
  "List all files currently locked under the revision control system.
With prefix arg list only the files locked by the user."
  (interactive "P")
  (let* ((locker (and arg (user-login-name)))
	 (buffer (sc-tree-walk 'sc-list-file-if-locked locker)))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (if (= (point-min) (point-max))
	  (insert "No files locked ")
	(insert "Files locked "))
      (if locker
	  (insert "by " locker " "))
      (insert "in " default-directory "\n\n"))
    (display-buffer buffer)))
      
(defun sc-list-locked-files ()
  "List all files currently locked by me"
  (interactive)
  (sc-list-all-locked-files 4))

(defun sc-list-registered-files ()
  "List all files currently registered under the revision control system."
  (interactive)
  (let ((buffer (sc-tree-walk 'sc-list-file)))
    (save-excursion
      (set-buffer buffer)
      (if (= (point-min) (point-max))
	  (insert "No files registered in " sc-generic-name
		  " in " default-directory)
	(goto-char (point-min))
	(insert "Files registered in " sc-generic-name " in " default-directory
		"\n\n")))
    (display-buffer buffer)))
       
(defun sc-update-directory ()
  "Updates the current directory by getting the latest copies of the files"
  (interactive)
  (save-some-buffers)
  (let ((buffer (sc-tree-walk 'sc-update-file)))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (if (= (point-min) (point-max))
	  (insert "No files needed to be updated in " default-directory "\n\n")
	(insert "Files updated in " default-directory "\n\n")))
    (display-buffer buffer)))

;; Miscellaneous other entry points

(defun sc-register-file (verbose)
  "Register the file visited by the current buffer into source control.
Prefix argument register it under an explicit revision number."
  (interactive "P")
  (let ((file buffer-file-name))
    (if (not file)
	(error "There is no file associated with buffer %s" (buffer-name)))
    (let ((lock-info (sc-lock-info file))
	  (revision ()))
      (if lock-info
	  (error "This file is already registered into %s" sc-generic-name))
      ;; propose to save the file if it's modified
      (if (and (buffer-modified-p)
	       (or
		sc-mode-expert
		(y-or-n-p (format "%s has been modified. Write it out? "
				  (buffer-name)))))
	  (save-buffer))
      ;; get the revision number
      (if verbose
	  (setq revision (read-string "Initial Revision Number: ")))
      (sc-register file revision)
      (revert-buffer nil t)
      (sc-mode-line))))

(defun sc-rename-file (old new)
  "Rename a file, taking its source control archive with it."
  (interactive "fOld name: \nFNew name: ")
  (let ((owner (sc-locking-user old)))
    (if (and owner (not (string-equal owner (user-login-name))))
	(error "Sorry, %s has that file checked out" owner)))
  (rename-file old new)
  (sc-rename old new))

(defun sc-rename-this-file (new)
  "Rename the file of the current buffer, taking its source control archive with it"
  (interactive "FNew name: ")
  (if (and (buffer-modified-p)
	   (y-or-n-p (format "%s has been modified. Write it out? "
			     (buffer-name))))
      (save-buffer))
  (sc-rename-file buffer-file-name new)
  (let ((old-buffer (current-buffer))
	(new-buffer (find-file-noselect new)))
    (set-window-buffer (selected-window) new-buffer)
    (pop-to-buffer (current-buffer))
    (bury-buffer old-buffer)))


;;; Mode independent functions 
;;; All those sc-... functions FUNCALL the corresponding sc-generic-... function.  
;;; The variables are set to functions that do the SCCS, RCS or CVS commands 
;;; depending on the mode chosen.

(defvar sc-generic-lock-info ()
  "Function to implement sc-lock-info")

(defun sc-lock-info (file)
  "Return a list of the current locker and current locked revision for FILE.
Returns NIL if FILE is not registered in the source control system.
Return (NIL NIL) if FILE is registered but not locked.
Return (locker revision) if file is locked."
  (funcall sc-generic-lock-info file))


(defvar sc-generic-register ()
  "Function to implement sc-register")

(defun sc-register (file revision)
  "Register FILE under source control with initial revision REVISION."
  (funcall sc-generic-register file revision))


(defvar sc-generic-check-out ()
  "Function to implement sc-check-out")

(defun sc-check-out (file lockp)
  "Checks out the latest version of FILE.  
If LOCKP is not NIL, FILE is also locked."
  (funcall sc-generic-check-out file lockp))


(defvar sc-generic-get-version ()
  "Function to implement sc-get-version")

(defun sc-get-version (file buffer revision)
  "Insert a previous revison of FILE in BUFFER.  
REVISION is the revision number requested."
  (funcall sc-generic-get-version file buffer revision))


(defvar sc-generic-check-in ()
  "Function to implement sc-check-in")

(defun sc-check-in (file revision message)
  "Check in FILE with revision REVISION.
MESSAGE is a string describing the changes."
  (funcall sc-generic-check-in file revision message))


(defvar sc-generic-history ()
  "Function to implement sc-history")

(defun sc-history (file)
  "Insert the edit history of FILE in the current buffer."
  (funcall sc-generic-history file))


(defvar sc-generic-tree-list ()
  "Function to implement sc-tree-list")

(defun sc-tree-list ()
  "List in the current buffer the files registered in the source control system"
  (funcall sc-generic-tree-list))
  

(defvar sc-generic-new-revision-p ()
  "Function to implement sc-new-revision-p")

(defun sc-new-revision-p (file)
  "True if a new revision of FILE was checked in since we last got a copy of it"
  (funcall sc-generic-new-revision-p file))


(defvar sc-generic-revert ()
  "Function to implement sc-revert")

(defun sc-revert (file)
  "Cancel a check out of FILE and get back the latest checked in version"
  (funcall sc-generic-revert file))


(defvar sc-generic-rename ()
  "Function to implement sc-rename")

(defun sc-rename (old new)
  "Rename the source control archives for OLD to NEW"
  (funcall sc-generic-rename old new))


(defvar sc-menu ()
  "Menu to use")
  

;;; Utilities functions
(defun sc-do-command (buffer message command file sc-file &rest flags)
  "Execute a command, notifying the user and checking for errors."
  (setq file (expand-file-name file))
  (message (format "Running %s on %s..." message file))
  (let ((status
	 (save-excursion
	   (set-buffer (get-buffer-create buffer))
	   (erase-buffer)
	   (setq flags (append flags (and file (list sc-file))))
	   (setq flags (delq () flags))
	   (let ((default-directory (file-name-directory (or file "./"))))
	     (eq (apply 'call-process command nil t nil flags) 0)))))
    (if status
	(message (format "Running %s...OK" message))
      (save-excursion
	(set-buffer buffer)
	(goto-char (point-min))
	(insert command)
	(mapcar '(lambda (i) (insert " " i)) flags)
	(insert "\n\n")
	(goto-char (point-min)))
      (display-buffer buffer)
      (error (format "Running %s...FAILED" message)))))

(defun sc-locking-user (file)
  "Return the login name of the locker of FILE.  Return nil if FILE is not locked"
  (car (sc-lock-info file)))

(defun sc-locked-revision (file)
  "Return the revision number currently locked for FILE, nil if FILE is not locked."
  (car (cdr (sc-lock-info file))))

(defun sc-mode-line ()
  "Set the mode line for the current buffer.
FILE is the file being visited."
  (let* ((file buffer-file-name)
	 (lock-info (sc-lock-info file)))
    ;; ensure that the global mode string is not NIL
    (or global-mode-string (setq global-mode-string '("")))
    ;; ensure that our variable is in the global-mode-string
    (or (memq 'sc-mode-line-string global-mode-string)
	(setq global-mode-string
	      (append global-mode-string '(sc-mode-line-string))))
    (make-local-variable 'sc-mode-line-string)
    (setq sc-mode-line-string
	  (cond ((null lock-info) ())
		((null (car lock-info))
		 (format " <%s:>" sc-generic-name))
		((equal (car lock-info) (user-login-name))
		 (format " <%s: %s>" sc-generic-name (car (cdr lock-info))))
		(t
		 (format " <%s: %s>" sc-generic-name (car lock-info)))))))

(defun sc-temp-buffer ()
  "Return a temporary buffer to use for output"
  (get-buffer-create (format "*%s*" sc-generic-name)))

(defun sc-tree-walk (func &rest args)
  "Apply FUNC to the files registered in the source control system.
FUNC is passed the file path and ARGS."
  (let* ((buffer-name (format "*%s directory*" sc-generic-name))
	 (buffer (get-buffer-create buffer-name))
	 (dir default-directory)
	 files)
    ;; recreate the directory buffer in the right directory
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (setq default-directory dir)
      ;; get a list of all the registered files
      (sc-tree-list)
      ;; remove the "not found" messages
      (goto-char (point-min))
      (while (search-forward "not found" () t)
	(beginning-of-line 1)
	(kill-line 1))
      ;; check if any file is listed
      (if (= (point-min) (point-max))
	  (error "No registered files under %s" default-directory))
      ;; build the list of files
      (goto-char (point-min))
      (setq files ())
      (while (not (eobp))
	(let ((file
	       (buffer-substring (point) (progn (end-of-line) (point)))))
	  (setq files (cons file files)))
	(forward-line 1))
      (setq files (nreverse files))
      ;; let the function output information in the buffer
      (erase-buffer))
    (display-buffer buffer)
    ;; apply the function
    (save-excursion
      (set-buffer buffer)
      (while files
	(apply func (car files) args)
	(setq files (cdr files)))
      buffer)))
  
(defun sc-get-version-in-temp-file (file revision)
  "For the given FILE, retrieve a copy of the version with given REVISION.
The text is retrieved into a tempfile.  Return the tempfile name."
  (let* ((oldversion
	  (make-temp-name
	   (concat (or revision "current")
		   "-"
		   (file-name-nondirectory file)
		   "-")))
	 (vbuf (get-buffer-create oldversion)))
    (sc-get-version file vbuf revision)
    (save-excursion
      (set-buffer vbuf)
      (write-region (point-min) (point-max) oldversion t 0))
    (kill-buffer vbuf)
    (chmod "-w" oldversion)
    oldversion))

;; Functions used to get directory level information

(defun sc-insert-file-lock-info (file lock-info)
  (insert (car lock-info) ":" (car (cdr lock-info)))
  (indent-to-column 16 1)
  (insert (file-name-nondirectory file) "\n"))
  
(defun sc-list-file-if-locked (file &optional arg)
   "List all files underneath the current directory matching a prefix type."
   (let ((lock-info (sc-lock-info file)))
     (if (and lock-info
	      (car lock-info)
	      (or (null arg) (equal arg (car lock-info))))
	 (progn
	   (sc-insert-file-lock-info file lock-info)
	   (sit-for 0)))))

(defun sc-list-file (file)
  (let ((lock-info (sc-lock-info file)))
    (cond ((car lock-info)
	   (sc-insert-file-lock-info file lock-info))
	  ((sc-new-revision-p file)
	   (insert "needs update")
	   (indent-to-column 16 1)
	   (insert (file-name-nondirectory file) "\n"))
	  (t
	   (indent-to-column 16 1)
	   (insert (file-name-nondirectory file) "\n")))
    (sit-for 0)))

;;; Function to update one file from the archive
(defun sc-update-file (file)
  "get the latest version of the file if a new one was checked-in"
  (if (sc-new-revision-p file)
      (let ((file-name (file-name-nondirectory file)))
	;; get the latest copy
	(rename-file (sc-get-version-in-temp-file file nil) file)
	(let ((b (get-file-buffer file)))
	  (if b
	      (save-excursion
		(set-buffer b)
		(revert-buffer nil t)
		(sc-mode-line))))
	;; show the file was updated
	(insert "updated")
	(indent-to-column 16 1)
	(insert file-name "\n")
	(sit-for 0))))

;; Set up key bindings for use while editing log messages

(if sc-log-entry-keymap
    nil
  (setq sc-log-entry-keymap (make-sparse-keymap))
  (define-key sc-log-entry-keymap "\C-ci" 'sc-insert-last-log)
  (define-key sc-log-entry-keymap "\C-c\C-i" 'sc-insert-last-log)
  (define-key sc-log-entry-keymap "\C-ca" 'sc-abort-check-in)
  (define-key sc-log-entry-keymap "\C-c\C-a" 'sc-abort-check-in)
  (define-key sc-log-entry-keymap "\C-c\C-c" 'sc-log-exit)
  (define-key sc-log-entry-keymap "\C-x\C-s" 'sc-log-exit))

(defvar sc-mode-hook nil
  "*Function or functions to run on entry to sc-mode.")

(defvar sc-mode ()
  "The currently active source control mode.  Use M-x sc-mode to set it")

(defun sc-mode (system)
  "Toggle sc-mode.
SYSTEM can be sccs, rcs or cvs.
Cvs requires the pcl-cvs package.

The following commands are available
\\[sc-next-operation]	perform next logical source control operation on current file
\\[sc-show-changes]	compare the version being edited with an older one
\\[sc-version-diff-file]	compare two older versions of a file
\\[sc-show-history]		display change history of current file
\\[sc-visit-previous-revision]	display an older revision of current file
\\[sc-revert-file]		revert buffer to last checked-in version
\\[sc-list-all-locked-files]		show all files locked in current directory
\\[sc-list-locked-files]		show all files locked by you in current directory
\\[sc-list-registered-files]		show all files under source control in current directory
\\[sc-update-directory]		get fresh copies of files checked-in by others in current directory
\\[sc-rename-file]		rename the current file and its source control file


While you are entering a change log message for a check in, sc-log-entry-mode
will be in effect.

Global user options:
    sc-diff-command	A list consisting of the command and flags
			to be used for generating context diffs.
    sc-mode-expert	suppresses some conformation prompts,
			notably for delta aborts and file saves.
    sc-max-log-size	specifies the maximum allowable size
			of a log message plus one.


When using SCCS you have additional commands and options

\\[sccs-insert-headers]		insert source control headers in current file

When you generate headers into a buffer using \\[sccs-insert-headers],
the value of sc-insert-headers-hook is called before insertion. If the
file is recognized a C or Lisp source, sc-insert-c-header-hook or
sc-insert-lisp-header-hook is called after insertion respectively.

    sccs-headers-wanted	which %-keywords to insert when adding
			headers with C-c h
    sccs-insert-static	if non-nil, keywords inserted in C files
			get stuffed in a static string area so that
			what(1) can see them in the compiled object code.

When using CVS you have additional commands

\\[sc-cvs-update-directory]	update the current directory using pcl-cvs
\\[sc-cvs-file-status]		show the CVS status of current file
"
  (interactive
   (if sc-mode
       '(())
     (list
      (intern
	(read-string "Turn on source control mode on for: " "SCCS")))))
  (cond ((eq system ())
	 (remove-hook 'find-file-hooks 'sc-mode-line)
	 (delete-menu-item (list sc-generic-name))
	 (remove-hook 'activate-menubar-hook 'sc-sensitize-menu)
	 (setq sc-mode ()))
	(sc-mode
	 (sc-mode ())
	 (sc-mode system))
	(t
	 (setq system (intern (upcase (symbol-name system))))
	 (let ((f (intern (format "sc-set-%s-mode" system))))
	   (if (not (fboundp f))
	       (error
		"No source control interface for \"%s\".  \
Please use SCCS, RCS, or CVS."
		system)
	     (funcall f)
	     (add-hook 'find-file-hooks 'sc-mode-line)
	     (add-menu '() sc-generic-name sc-menu)
	     (add-hook 'activate-menubar-hook 'sc-sensitize-menu)
	     (run-hooks 'sc-mode-hook)
	     (setq sc-mode system))))))

(defun sc-log-entry-mode ()
  "Major mode for editing log message.

These bindings are available when entering the log message
\\[sc-log-exit]		proceed with check in, ending log message entry
\\[sc-insert-last-log]		insert log message from last check-in
\\[sc-abort-check-in]		abort this check-in

Entry to the change-log submode calls the value of text-mode-hook, then
the value sc-log-entry-mode-hook.
"
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map sc-log-entry-keymap)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'sc-log-entry-mode)
  (setq mode-name "Source Control Change Log Entry")
  (run-hooks 'text-mode-hook 'sc-log-entry-mode-hook))



;;; SCCS specific part

;; Find a reasonable default for the SCCS bin directory
(defvar sccs-bin-directory
  (cond ((file-executable-p "/usr/sccs/unget") "/usr/sccs")
	((file-executable-p "/usr/bin/unget") "/usr/bin")
	((file-directory-p "/usr/sccs") "/usr/sccs")
	((file-directory-p "/usr/bin/sccs") "/usr/bin/sccs")
	(t "/usr/bin"))
  "*Directory where to find the sccs executables")

(defvar sccs-headers-wanted '("\%\W\%")
  "*SCCS header keywords to be inserted when sccs-insert-header is executed.")

(defvar sccs-insert-static t
  "*Insert a static character string when inserting source control headers in C mode.
Only relevant for the SCCS mode.")

;; Vars the user doesn't need to know about.

(defvar sccs-log-entry-mode nil)
(defvar sccs-current-major-version nil)

;; Some helper functions

(defun sccs-name (file &optional letter)
  "Return the sccs-file name corresponding to a given file."
  (if (null file)
      ()
    (let ((expanded-file (expand-file-name file)))
      (format "%sSCCS/%s.%s"
	      (concat (file-name-directory expanded-file))
	      (or letter "s")
	      (concat (file-name-nondirectory expanded-file))))))

(defun sccs-lock-info (file)
  "Lock-info method for SCCS.  See sc-generic-lock-info"
  (let ((sccs-file (sccs-name file "s"))
	(lock-file (sccs-name file "p")))
    (cond ((or (null file) (not (file-exists-p sccs-file)))
	   ())
	  ((not (file-exists-p lock-file))
	   (list () ()))
	  (t
	   (save-excursion
	     (set-buffer (get-buffer-create "*SCCS tmp*"))
	     (insert-file lock-file)
	     (while (search-forward " " () t)
	       (replace-match "\n" () t))
	     (goto-char (point-min))
	     (forward-line 1)
	     (let ((revision
		    (buffer-substring (point) (progn (end-of-line) (point))))
		   (name
		    (progn (forward-line 1)
			   (buffer-substring (point)
					     (progn (end-of-line) (point))))))
	       (kill-buffer (current-buffer))
	       (list name revision)))))))


(defun sccs-do-command (buffer command file &rest flags)
  "Execute an SCCS command, notifying the user and checking for errors."
  (let ((exec-path (cons sccs-bin-directory exec-path)))
    (apply 'sc-do-command buffer command command file (sccs-name file) flags)))

(defun sccs-admin (file sid)
  "Checks a file into sccs.
FILE is the unmodified name of the file.  SID should be the base-level sid to
check it in under."
  ;; give a change to save the file if it's modified
  (if (and (buffer-modified-p)
	   (y-or-n-p (format "%s has been modified. Write it out? "
			     (buffer-name))))
      (save-buffer))
  (sccs-do-command "*SCCS*" "admin" file
		   (concat "-i" file) (concat "-r" sid))
  (chmod "-w" file)
  ;; expand SCCS headers
  (sccs-check-out file nil))

(defun sccs-register (file revision)
  (sccs-load-vars)
  (if (and (not (file-exists-p "SCCS"))
	   (y-or-n-p "Directory SCCS does not exist, create it?"))
      (make-directory "SCCS"))
  (sccs-admin file
	      (cond 
	       (revision revision)
	       ((error-occurred (load-file "SCCS/emacs-vars.el")) "1")
	       (t sccs-current-major-version))))

(defun sccs-check-out (file lockp)
  "Retrieve a copy of the latest version of the given file."
  (sccs-do-command "*SCCS*" "get" file (if lockp "-e")))

(defun sccs-get-version (file buffer revision)
  (sccs-do-command buffer "get" file
		   (and revision (concat "-r" revision))
		   "-p" "-s"))

(defun sccs-check-in (file revision comment)
  "Check-in a given version of the given file with the given comment."
  (sccs-do-command "*SCCS*" "delta" file "-n"
		   (format "-r%s" revision)
		   (format "-y%s" comment))
  (chmod "-w" file)
  ;; sccs-delta already turned off write-privileges on the
  ;; file, let's not re-fetch it unless there's something
  ;; in it that get would expand
  (save-excursion
    (let ((buffer (get-file-buffer file)))
      (if buffer
	  (progn
	    (set-buffer buffer)
	    (sccs-check-out file nil))))))

(defun sccs-history (file)
  (sccs-do-command (current-buffer) "prs" file))

;; There has *got* to be a better way to do this...

(defun sccs-save-vars (sid)
  (save-excursion
    (find-file "SCCS/emacs-vars.el")
    (erase-buffer)
    (insert "(setq sccs-current-major-version \"" sid "\")")
    (basic-save-buffer)))

(defun sccs-load-vars ()
  (if (error-occurred (load-file "SCCS/emacs-vars.el"))
      (setq sccs-current-major-version "1")))

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
	     (not (string-match "\\.h$" buffer-file-name)))
	(progn
	  (insert "#ifndef lint\n"
		  "static char *sccsid")
;;	  (setq st (point))
;;	  (insert (file-name-nondirectory buffer-file-name))
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
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search ()))
      (re-search-forward  "%[MIRLBSDHTEGUYFPQCZWA]%" (point-max) t))))

(defun sccs-tree-list ()
  "List all the registered files in the current directory"
  (call-process "/bin/sh" () t () "-c"
		(concat "/bin/ls -1 " default-directory "SCCS/s.*"))
  (goto-char (point-min))
  (while (search-forward "SCCS/s." () t)
    (replace-match "" () t)))

(defun sccs-new-revision-p (file)
  "True if the SCCS archive is more recent than the file itself"
  (file-newer-than-file-p (sccs-name file) file))

(defun sccs-revert (file)
  "Cancel a check-out and get a fresh copy of the file"
  (delete-file (sccs-name file "p"))
  (delete-file file)
  (sccs-do-command "*SCCS*" "get" file "-s"))

(defun sccs-rename (old new)
  "Rename the SCCS archives for OLD to NEW"
  (if (file-exists-p (sccs-name old "p"))
      (rename-file (sccs-name old "p") (sccs-name new "p")))
  (if (file-exists-p (sccs-name old "s"))
      (rename-file (sccs-name old "s") (sccs-name new "s"))))


;;; RCS specific part

;; Some helper functions

(defun rcs-name (file)
  "Return the rcs-file corresponding to a given file."
  (if (null file)
      ()
    (let* ((name (expand-file-name file))
	   (rcs-file (concat name ",v")))
      (if (and (not (file-exists-p rcs-file))
	       (file-exists-p (concat (file-name-directory name) "RCS")))
	  (setq rcs-file 
		(format "%sRCS/%s,v" (file-name-directory name)
			(file-name-nondirectory name))))
      rcs-file)))

(defun rcs-lock-info (file)
  "Lock-info method for RCS.  See sc-generic-lock-info"
  (let ((rcs-file (rcs-name file))
	locks-regexp)
    (if (or (null rcs-file) (not (file-exists-p rcs-file)))
	()
      (save-excursion
	(set-buffer (get-buffer-create "*RCS tmp*"))
	(erase-buffer)
	(call-process "rlog" () t () "-L" "-h" rcs-file)
	(goto-char (point-min))
	(if (looking-at "\n.*Working file")
	    ;; RCS 4.x
	    (setq locks-regexp "^locks:")
	  ;; RCS 5.x
	  (setq locks-regexp "^locks:.*$\n"))
	(if (not (re-search-forward locks-regexp () t))
	    (list () ())
	  (if (not (looking-at (concat "[\t ]*\\([^:]*\\): \\([0-9\\.]*\\)")))
	      (list () ())
	    (list (buffer-substring (match-beginning 1) (match-end 1))
		  (buffer-substring (match-beginning 2) (match-end 2)))))))))


(defun rcs-register (file revision)
  (if (and (not (file-exists-p "RCS"))
	   (y-or-n-p "Directory RCS does not exist, create it?"))
      (make-directory "SCCS"))
  (sc-do-command "*RCS*" "ci" "ci" file (rcs-name file) "-u"))

(defun rcs-check-out (file lockp)
  (sc-do-command "*RCS*" "co" "co" file (rcs-name file) (if lockp "-l")))

(defun rcs-get-version (file buffer revision)
  (sc-do-command buffer "co" "co" file (rcs-name file)
		 (if revision (concat "-p" revision) "-p")
		 "-q"))

(defun rcs-check-in (file revision comment)
  "Check-in a given version of the given file with the given comment."
  (sc-do-command "*RCS*" "ci" "ci" file (rcs-name file) "-f"
		 (format "-m%s" comment)
		 (if (equal revision (sc-locked-revision file))
		     "-u"
		   (format "-u%s" revision))))

(defun rcs-history (file)
  (sc-do-command (current-buffer) "rlog" "rlog" file (rcs-name file)))

(defun rcs-tree-list ()
  "List all the registered files in the current directory"
  (call-process "/bin/sh" () t () "-c"
		(concat "/bin/ls -1 " default-directory "RCS/*,v"))
  (call-process "/bin/sh" () t () "-c"
		(concat "/bin/ls -1 " default-directory "*,v"))
  (goto-char (point-min))
  (while (search-forward "RCS/" () t)
    (replace-match "" () t))
  (goto-char (point-min))
  (while (search-forward ",v" () t)
    (replace-match "" () t)))

(defun rcs-new-revision-p (file)
  "True if the archive is more recent than the file itself"
  (file-newer-than-file-p (rcs-name file) file))

(defun rcs-revert (file)
  "Cancel a check-out and get a fresh copy of the file"
  (sc-do-command "*RCS*" "rcs" "rcs" file (rcs-name file) "-u")
  (delete-file file)
  (sc-do-command "*RCS*" "co" "co" file (rcs-name file)))

(defun rcs-rename (old new)
  "Rename the archives for OLD to NEW"
  (if (file-exists-p (rcs-name old))
      (rename-file (rcs-name old) (rcs-name new))))


;;; CVS specific part

;;; As we rely on pcl-cvs for the directory level functions the menu is
;;; much shorter in CVS mode


(defun cvs-lock-info (file)
  "Lock-info method for CVS, different from RCS and SCCS modes.
File are never locked in CVS."
  (list () ()))

(defun cvs-register (file revision)
  (sc-do-command "*CVS*" "cvs add" "cvs" file
		 (file-name-nondirectory file)
		 "add" "-mInitial revision"))

(defun cvs-check-out (file lockp)
  )

(defun cvs-get-version (file buffer revision)
  (sc-do-command buffer "cvs update" "cvs" file file "update" 
		 (if revision (concat "-r" revision))
		 "-p" "-q"))

(defun cvs-check-in (file revision comment)
  "Check-in a given version of the given file with the given comment."
  (sc-do-command "*CVS*" "cvs commit" "cvs" file file "commit"
		 (and revision (format "-r%s" revision))
		 (format "-m%s" comment)))

(defun cvs-history (file)
  (sc-do-command (current-buffer) "cvs log" "cvs" file file "log"))

(defun cvs-revert (file)
  "Cancel a check-out and get a fresh copy of the file"
  (delete-file file)
  (sc-do-command "*CVS*" "cvs update" "cvs" file file "update"))

(defun sc-cvs-update-directory ()
  "Update the current directory by calling cvs-update from pcl-cvs"
  (interactive)
  (cvs-update default-directory))

(defun sc-cvs-file-status ()
  "Show the CVS status of the current file"
  (interactive)
  (if (not buffer-file-name)
      (error "There is no file associated with buffer %s" (buffer-name)))
  (let ((file buffer-file-name))
    (sc-do-command "*CVS*" "cvs status" "cvs" file file "status" "-v"))
  (save-excursion
    (set-buffer "*CVS*")
    (goto-char (point-min)))
  (display-buffer "*CVS*"))


;;; Instanciation and installation of the menus

;;; Set the menubar for Lucid Emacs
(defvar sc-default-menu
  '(["NEXT-OPERATION"	sc-next-operation	t	nil]
    ["Update Current Directory"		sc-update-directory	t]
    "----"
    ["Revert File"		sc-revert-file	t	nil]
    ["Rename File"		sc-rename-this-file		t	nil]
    "----"
    ["Show Changes"		sc-show-changes		t]
    ["Show Changes Since Revision..."	sc-show-revision-changes	t]
    ["Visit Previous Revision..."	sc-visit-previous-revision	t]
    ["Show Edit History"		sc-show-history		t]
    "----"
    ["List Locked Files"	sc-list-locked-files	t]
    ["List Locked Files Any User"	sc-list-all-locked-files	t]
    ["List Registered Files"	sc-list-registered-files	t])
  "Menubar entry for using the revision control system.")

(defvar sc-cvs-menu
  '(["Update Current Directory"		sc-cvs-update-directory	t]
    ["Revert File"		sc-revert-file	t	nil]
    "----"
    ["Show Changes"		sc-show-changes		t]
    ["Show Changes Since Revision..."	sc-show-revision-changes	t]
    ["Visit Previous Revision..."	sc-visit-previous-revision	t]
    ["Show File Status"		sc-cvs-file-status		t]
    ["Show Edit History"		sc-show-history		t])
  "Menubar entry for using the revision control system with CVS.")

(defun sc-sensitize-menu ()
  (let* ((rest (cdr (car
		     (find-menu-item current-menubar (list sc-generic-name)))))
	 (case-fold-search t)
	 (file (if buffer-file-name
		   (file-name-nondirectory buffer-file-name)
		 (buffer-name)))
	 (dir (file-name-directory
	       (if buffer-file-name buffer-file-name default-directory)))
	 (lock-info (sc-lock-info buffer-file-name))
	 command
	 item)
    (while rest
      (setq item (car rest))
      (if (not (vectorp item))
	  nil
	(setq command (aref item 1))
	(cond ((eq 'sc-next-operation command)
	       (aset item 0
		     (cond ((not lock-info) "Register File")
			   ((not (car lock-info)) "Check out File")
			   (t "Check in File")))
	       ;; if locked by somebody else disable the next-operation
	       (if (or (not buffer-file-name)
		       (and (car lock-info)
			    (not (equal (car lock-info) (user-login-name)))))
		   (aset item 2 ())
		 (aset item 2 t)))
	      ((> (length item) 3)
	       (aset item 3 file))
	      (t nil))
	(let ((enable-file-items
	       (if (eq sc-mode 'CVS) buffer-file-name lock-info)))
	  (if (memq command
		    '(sc-force-check-in-file
		      sc-register-file
		      sc-revert-file
		      sc-rename-this-file
		      sc-show-history
		      sc-show-changes
		      sc-show-revision-changes
		      sc-visit-previous-revision
		      sc-cvs-file-status))
	      (aset item 2 enable-file-items))))
      (setq rest (cdr rest)))
    nil))


;;; Function to decide which Source control to use
(defun sc-set-SCCS-mode ()
  (setq sc-generic-name "SCCS")
  (setq sc-generic-lock-info 'sccs-lock-info)
  (setq sc-generic-register 'sccs-register)
  (setq sc-generic-check-out 'sccs-check-out)
  (setq sc-generic-get-version 'sccs-get-version)
  (setq sc-generic-check-in 'sccs-check-in)
  (setq sc-generic-history 'sccs-history)
  (setq sc-generic-tree-list 'sccs-tree-list)
  (setq sc-generic-new-revision-p 'sccs-new-revision-p)
  (setq sc-generic-revert 'sccs-revert)
  (setq sc-generic-rename 'sccs-rename)
  (setq sc-menu
	(cons (car sc-default-menu)
	      (cons ["Insert Headers"	sccs-insert-headers	t]
		    (cdr sc-default-menu))))
  (define-key sc-prefix-map "h" 'sccs-insert-headers)
  (define-key sc-prefix-map "\C-d" 'sc-update-directory))

(defun sc-set-RCS-mode ()
  (setq sc-generic-name "RCS")
  (setq sc-generic-lock-info 'rcs-lock-info)
  (setq sc-generic-register 'rcs-register)
  (setq sc-generic-check-out 'rcs-check-out)
  (setq sc-generic-get-version 'rcs-get-version)
  (setq sc-generic-check-in 'rcs-check-in)
  (setq sc-generic-history 'rcs-history)
  (setq sc-generic-tree-list 'rcs-tree-list)
  (setq sc-generic-new-revision-p 'rcs-new-revision-p)
  (setq sc-generic-revert 'rcs-revert)
  (setq sc-generic-rename 'rcs-rename)
  (setq sc-menu sc-default-menu)
  (define-key sc-prefix-map "\C-d" 'sc-update-directory))

(defun sc-set-CVS-mode ()
  (require 'pcl-cvs)
  (setq sc-generic-name "CVS")
  (setq sc-generic-lock-info 'cvs-lock-info)
  (setq sc-generic-register 'cvs-register)
  (setq sc-generic-check-out 'cvs-check-out)
  (setq sc-generic-get-version 'cvs-get-version)
  (setq sc-generic-check-in 'cvs-check-in)
  (setq sc-generic-history 'cvs-history)
  (setq sc-generic-tree-list 'cvs-tree-list)
  (setq sc-generic-new-revision-p 'cvs-new-revision-p)
  (setq sc-generic-revert 'cvs-revert)
  (setq sc-generic-rename 'cvs-rename)
  (setq sc-menu sc-cvs-menu)
  (define-key sc-prefix-map "\C-d" 'sc-cvs-update-directory)
  (define-key sc-prefix-map "s" 'sc-cvs-file-status))


;; the module is sucessfully loaded!
(provide 'generic-sc)
