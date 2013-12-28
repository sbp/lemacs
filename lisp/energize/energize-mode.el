;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright © 1991-1993 by Lucid, Inc.  All Rights Reserved.

(require 'gdb)
(require 'compile)
(or (and (fboundp 'ask-user-about-lock)
	 (not (eq 'autoload
		  (car-safe (symbol-function 'ask-user-about-lock)))))
    (load-library "userlock"))

;;; Energize advised functions

(defun energize-next-error (&optional arg)
  "If the current buffer is a Energize buffer, the server is consulted.
In this case, a prefix argument means ``previous error''.  Otherwise,
use the original definition of next-error."
  (interactive "P")
  (if (energize-buffer-p (current-buffer))
      (energize-execute-command (if arg "previouserror" "nexterror"))
    (energize-orig-next-error arg)))

(defun energize-previous-error (&optional arg)
  "If the current buffer is a Energize buffer, the server is consulted.
In this case, a prefix argument means ``next error''.  Otherwise,
use the original definition of previous-error."
  (interactive "P")
  (if (energize-buffer-p (current-buffer))
      (energize-execute-command (if arg "nexterror" "previouserror"))
    (energize-orig-previous-error arg)))

(defun energize-set-visited-file-name (filename)
  "This is illegal for Energize buffers."
  (interactive "FSet visited file name: ")
  (if (and (energize-write-file-buffer-p)
	   (not (equal filename buffer-file-name)))
      (error "Can't change the name associated with a Energize buffer.")
    (energize-orig-set-visited-file-name filename)
    (if (energize-buffer-p (current-buffer))
	(energize-mode-internal))))


(defun energize-find-file-noselect (filename &optional nowarn)
  "When connected to Energize, if the visited file is one that
Energize knows about, it will be correctly annotated."
  (if (and (connected-to-energize-p)
	   (not (file-directory-p filename))
	   (energize-query-buffer filename t))
      ;; after-find-file and abbreviate-file-name are called from
      ;; energize-buffer-creation-hook-function, which is run from
      ;; editorside.c (way down under energize-query-buffer).
      ;; This is a mess...
      (energize-query-buffer filename)
    (energize-orig-find-file-noselect filename nowarn)))


(defun energize-ask-user-about-lock (fn opponent)
  "Energize buffers do this by asking the server."
  (if (energize-buffer-p (current-buffer))
      t
    (energize-orig-ask-user-about-lock fn opponent)))

(defun energize-init ()
  (if (= 0 (energize-bits 1024))
      (energize-announce (get 'energize 'energize))))

;; true if current-buffer is an energize buffer that does not support
;; the real write-file and so has to do the special energize way of doing
;; write-file that loses the annotations.
(defun energize-write-file-buffer-p ()
;;  (and (energize-buffer-p (current-buffer)
;;       (not (eq major-mode 'energize-project-mode)))
  (energize-buffer-p (current-buffer)))

(defun energize-write-file (filename)
  "When executed on an Energize buffer, this will cause all 
annotations to be lost (that is, the buffer will become a normal
buffer, not one that the Energize server knows about.)"
  (interactive
   (list (let ((prompt (if (energize-write-file-buffer-p)
			   "Write Energize buffer to file: "
			 "Write file: ")))
	   (if buffer-file-name
	       (read-file-name prompt nil nil nil nil)
	     (read-file-name prompt (cdr (assq 'default-directory
					       (buffer-local-variables)))
			     nil nil (buffer-name))))))
  (if (not (energize-write-file-buffer-p))
      (energize-orig-write-file filename)
    ;; else...
    (if (and (file-exists-p filename)
	     (not
	      (yes-or-no-p (format "File %s already exists.  Overwrite it? "
				   filename))))
	(error "Aborted"))
    (write-region (point-min) (point-max) filename nil nil)
    (if buffer-file-name
	(revert-buffer t t)) ; revert this buffer from the Energize server
    (kill-buffer nil) ; kill the current buffer, to lose Energize properties
    (set-window-buffer ; and now visit the "new" file, and all that entails
     (selected-window)
     (find-file-noselect filename))))


(defun energize-gdb-break (arg)
  "If the current buffer is a Energize buffer, then this works by talking 
to the server."
  (interactive "P")
  (if (not (energize-buffer-p (current-buffer)))
      (energize-orig-gdb-break arg)
    (energize-execute-command "setbreakpoint")))

(defun energize-gdb-step (arg)
  "If the current buffer is a Energize buffer, then this works by talking to
the server."
  (interactive "p")
  (if (not (energize-buffer-p (current-buffer)))
      (energize-orig-gdb-step arg)
    (while (> arg 0)
      (energize-execute-command "steponce")
      (setq arg (1- arg)))))

(defun energize-gdb-stepi (arg)
  "If the current buffer is a Energize buffer, then this works by talking to
the server."
  (interactive "p")
  (if (not (energize-buffer-p (current-buffer)))
      (energize-orig-gdb-step arg)
    ;; there's no energize command for this, so do it this way...
    (save-excursion
      (set-buffer "*Debugger*")
      (goto-char (point-max))
      (insert (format "stepi %d" arg))
      (comint-send-input))))

(defun energize-beginning-of-defun (&optional arg)
  "Move point to the beginning of the current top-level form.
With a numeric argument, move back that many forms."
  (interactive "p")
  (or arg (setq arg 1))
  (if (not (energize-buffer-p (current-buffer)))
      ;;(energize-orig-beginning-of-defun arg)
      (error "Not an Energize buffer")
    (if (< arg 0)
	(energize-end-of-defun (- arg))
      (while (> arg 0)
        (or (bobp) (forward-char -1))
        (while (and (not (bobp)) (null (extent-at (point))))
          (forward-char -1))
        (let ((pos (point)))
          (map-extents 
           (function
            (lambda (extent dummy)
	      (if (< (setq pos (extent-start-position extent)) (point))
		  (goto-char pos))))
           (current-buffer) (point) (point) nil t))
        (setq arg (1- arg))))))

(defun energize-end-of-defun (&optional arg)
  "Move point to the end of the current top-level form.
With a numeric argument, move forward over that many forms."
  (interactive "p")
  (or arg (setq arg 1))
  (if (not (energize-buffer-p (current-buffer)))
      ;;(energize-orig-end-of-defun arg)
      (error "Not an Energize buffer")
    (if (< arg 0)
	(energize-beginning-of-defun (- arg))
      (while (> arg 0)
        (or (eobp) (forward-char 1))
        (while (and (not (eobp)) (null (extent-at (point))))
          (forward-char 1))
        (let ((pos (point)))
          (map-extents 
           (function
            (lambda (extent dummy)
	      (if (> (setq pos (extent-end-position extent)) (point))
		  (goto-char pos))))
           (current-buffer) (point) (point) nil t))
        (setq arg (1- arg))))))

(defun energize-normal-mode (&optional find-file)
"If this is an Energize buffer, then the Energize modes are turned on as well."
  (interactive)
  (energize-orig-normal-mode find-file)
  (if (and (energize-buffer-p (current-buffer))
	   (not inside-energize-buffer-creation-hook-function))
      (funcall energize-create-buffer-hook (current-buffer))))



;;; Patching Energize into file I/O via the standard hooks.

(defun energize-write-file-hook ()
  ;; for use as the last element of write-file-hooks in energize buffers.
  (if (not (energize-buffer-p (current-buffer)))
      (error "energize-write-file-hook called for a non-energize buffer"))
  (save-restriction
    (widen)
    (let ((evil (energize-guess-what-backup-buffer-is-going-to-do))
	  (name (or buffer-file-name (buffer-name (current-buffer))))
	  (setmodes (or buffer-backed-up (backup-buffer)))
	  (save-completed-normally nil))
      (message "saving %s to Energize..." name)
      (unwind-protect
	  (let ((inhibit-quit t))
	    (energize-execute-command "save")
	    (setq save-completed-normally (connected-to-energize-p)))
	;; protected
	(if (not save-completed-normally)
	    (oh-energize-deliver-me-from evil))) ; kernel crash!  Don't lose!
      (if (not save-completed-normally)
	  nil ; yow!
	(set-buffer-modtime (current-buffer))
	(energize-update-menubar)
	;; After energize has written the file to disk, maybe chmod it.
	(if setmodes
	    (condition-case nil
		(set-file-modes buffer-file-name setmodes)
	      (error nil))))
      (if save-completed-normally
	  (message "saved %s to Energize." name)
	(error "Server crashed while saving %s!" name))))
  ;; return t, meaning we wrote the file.
  ;; The user had better not have their own write-file-hook that writes
  ;; the file (like a crypt-mode) or they will surely lose.
  t)


(defun energize-guess-what-backup-buffer-is-going-to-do ()
  ;; If the kernel has crashed while we were in the process of saving a file
  ;; to it, (backup-buffer) may already have renamed the existing file to the
  ;; backup name, meaning that no file of the original name exists.  This
  ;; function attempts to guess what file backup-buffer picked, in order to
  ;; undo this unfortunate situation by reversing the effects of the
  ;; rename-file.  Almost all of this code is duplicated from backup-buffer.
  (let ((real-file-name buffer-file-name)
	backup-info backupname targets setmodes)
    (while (let ((tem (file-symlink-p real-file-name)))
	     (if tem
		 (setq real-file-name
		       (expand-file-name tem
			 (file-name-directory real-file-name))))
	     tem))
    (setq backup-info (find-backup-file-name real-file-name)
	  backupname (car backup-info)
	  targets (cdr backup-info))
    (let ((file-will-be-backed-up-by-copying
	   (or file-precious-flag
	       backup-by-copying
	       (and backup-by-copying-when-linked
		    (> (file-nlinks real-file-name) 1))
	       (and backup-by-copying-when-mismatch
		    (let ((attr (file-attributes real-file-name)))
		      (or (nth 9 attr)
			  (/= (nth 2 attr) (user-uid))))))))
      (if file-will-be-backed-up-by-copying
	  nil ; if it will-be a copy, then no damage will been done.
	(cons backupname real-file-name)))))

(defun oh-energize-deliver-me-from (evil)
  ;; this really really really sucks
  (if evil
      (condition-case ()
	  (rename-file (car evil) (cdr evil) t)
	(file-error
	 (error (if (connected-to-energize-p) ; oh give me a break...
		    "file \"%s\" not saved"
        "Energize server crash while saving \"%s\".  File may have been deleted.")
		(file-name-nondirectory (cdr evil)))))))


(defun energize-revert-buffer-insert-file-contents-hook (file noconfirm)
  ;; for use as the value of revert-buffer-insert-file-contents-function
  ;; in energize buffers.
  (if (not (energize-buffer-p (current-buffer)))
      (error "energize-revert-buffer-hook called for a non-energize buffer"))
  (widen)
  (cond ((equal file buffer-file-name)		; reverting from energize
	 (energize-execute-command "revert"))
	(t					; reverting from autosave
	 (if (not (file-exists-p file))
	     (error "File %s no longer exists!" file))
	 (erase-buffer)
	 (insert-file-contents file)))
  t)


(defun energize-kill-buffer-hook ()
  ;; for use as the value of kill-buffer-hook in energize buffers.
  (if (energize-buffer-p (current-buffer))
      (energize-request-kill-buffer (current-buffer))
      (error "energize-kill-buffer-hook called on a non-energize buffer"))
  t)


; 
; ;;; An importing version of find-file.
; 
; (fset 'find-file-in-energize 'energize-find-file)
; (defun energize-find-file (name)
;   "When connected to energize, energize will know about this file."
;   (interactive (list (read-file-name 
; 		      (if (connected-to-energize-p)
; 			  "Energize find file: "
; 			"Find file: "))))
;   (if (and (connected-to-energize-p)
; 	   (not (file-directory-p name))
; 	   (or (energize-query-buffer name t)
; 	       (y-or-n-p (format "Import \"%s\" into Energize? " name))))
;       (let ((buffer (energize-query-buffer name))) ; create if necessary
; 	(or (bufferp buffer) (error "couldn't import %s" name))
; 	;; change name, as energize might have trunamed it.
; 	(setq name (or (buffer-file-name buffer) (error "no name?")))
; 	;; if it was in a buffer already, give it the energize keybindings.
; 	(save-excursion
; 	  (set-buffer buffer)
; 	  (if (not energize-source-mode)
; 	      (energize-source-minor-mode)))
; 	))
;   ;; find-file will switch to the buffer if we created it above.
;   (find-file name))


;;; 

;;; This prompts in the minibuffer, ##### with no completion.
(defun energize-edit-definition (def)
  "If the current buffer is a Energize buffer, the Energize database is used.
Otherwise, invokes `find-tag'."
  (interactive
   (progn
     (or (and (fboundp 'find-tag-tag) (fboundp 'find-tag-default))
	 (require 'tags "etags"))
     (list
      (let ((default
	      (if (x-selection-owner-p)
		  (x-get-selection)
		(find-tag-default)))
	    def)
	(setq def
	      (if (energize-buffer-p (current-buffer))
		  (completing-read
		   (if default
		       (format "Edit definition [%s]: " default)
		     "Edit definition: ")
		   nil nil; 'energize-edit-def-predicate
		   nil nil)
		(find-tag-tag "Edit definition: ")))
	(if (consp def) (setq def (car def)))
	(if (equal "" def) (setq def default))
	def))))
  (if (energize-buffer-p (current-buffer))
      (energize-execute-command "editdef" () def t)
    (find-tag def)))

(define-key global-map "\M-." 'energize-edit-definition)

(defun disconnect-from-energize-query ()
  "Disconnect this emacs from the Energize server, after confirming."
  (interactive)
  (or (y-or-n-p "Disconnect from Energize? ") (error "not confirmed"))
  (disconnect-from-energize))


;;; Functions to add commands to the project buffers
(defun energize-insert-slots (got-to-top-p l)
  (if (not (eq major-mode 'energize-project-mode))
      (error "Command available only in project buffers"))
  ;; move to a suitable place
  (if got-to-top-p
      (beginning-of-buffer)
    (beginning-of-line))
  ;; go before "Associated Projects" and "Related Files"
  (if (or (search-backward "Related Projects:" () t)
	  (search-backward "Associated Files:" () t)
	  (looking-at "Related Projects:")
	  (looking-at "Associated Files:"))
      (previous-line 2))
  ;; find empty space
  (while (and (not (looking-at "$"))
	      (not (eq (point) (point-max))))
    (next-line 1))
  (newline)
  (save-excursion
    (mapcar '(lambda (i) (insert i) (newline)) l))
  ;; this is magic
  (forward-char 18))

(defun energize-insert-rule ()
  (interactive)
  (energize-insert-slots
   t
   '("           Rules:"
     "          <rule>: lcc -Xez -c -g -Xa -o $object $source")))

(defun energize-insert-file-target ()
  (interactive)
  (energize-insert-slots
   ()
   '("     Object File: <object-file>"
     "          Source: <source-file>"
     "      Build Rule: <rule>")))

(defun energize-insert-executable-target ()
  (interactive)
  (energize-insert-slots
   ()
   '("      Executable: <executable>"
     "   Build Command: lcc -Xf -Xez -o $object <object-file> ...")))

(defun energize-insert-library-target ()
  (interactive)
  (energize-insert-slots
   ()
   '("         Library: <library>"
     "   Build Command: energize_ar -Xez -remove -ranlib clq $object \\"
     "                    <object-file> ...")))

(defun energize-insert-collection-target ()
  (interactive)
  (energize-insert-slots
   ()
   '("      Collection: <collection>"
     "   Build Command: energize_collect -Xez -o $object <object-file> ...")))

(defun energize-insert-target-target ()
  (interactive)
  (energize-insert-slots
   ()
   '("          Target: <target>"
     "    Dependencies: <target> ..."
     "   Build Command: <shell-command>")))



;;; Keymaps for Energize buffers.

(defvar energize-map nil "*Parent keymap for all Energize buffers")
(defvar energize-top-level-map nil "*Keymap for the Energize top-level buffer")
(defvar energize-user-input-map nil
  "*Parent keymap for Energize input buffers")
(defvar energize-debugger-map nil "*Keymap for Energize debugger buffers")
(defvar energize-breakpoint-map nil "*Keymap for Energize breakpoint-lists")
(defvar energize-browser-map nil "*Keymap for Energize browser buffers")
(defvar energize-project-map nil "*Keymap for Energize project buffers")
(defvar energize-no-file-project-map nil
  "*Keymap for Energize project buffers not associated with a file")
(defvar energize-source-map nil "*Keymap for Energize source buffers")

(defvar energize-mode-hook nil
  "Hook called when each energize buffer is created.")
(defvar energize-top-level-mode-hook nil
  "Hook called when the energize top-level buffer is created.")
(defvar energize-project-mode-hook nil
  "Hook called when a energize project buffer is created.")
(defvar energize-no-file-project-mode-hook nil
  "Hook called when a energize project buffer with no file is created.")
(defvar energize-breakpoint-mode-hook nil
  "Hook called when a energize breakpoint-list buffer is created.")
(defvar energize-browser-mode-hook nil
  "Hook called when a energize browser buffer is created.")
(defvar energize-source-mode-hook nil
  "Hook called when any source buffer is placed in the Energize minor-mode.")


(if energize-map
    nil
  (setq energize-map (make-sparse-keymap))
  (define-key energize-map "\^C\^F"	'energize-find-project)
  (define-key energize-map "\^C\^B\^E"	'energize-browse-error)
  (define-key energize-map "\^C\^B\^L"	'energize-browse-language-elt)
  (define-key energize-map "\^C\^B\^T"	'energize-browse-tree)
  (define-key energize-map "\^C\^B\^C"	'energize-browse-class)
;;  (define-key energize-map "\^C\^B\^S"	'energize-browse-toolstat)
  (define-key energize-map "\M-B" 'energize-build-a-target) ; M-Sh-B
  (define-key energize-map "\M-C" 'energize-default-compile-file) ; M-Sh-C
  (define-key energize-map 'button3 'energize-popup-menu)
  )

(if energize-top-level-map
    nil
  (setq energize-top-level-map (make-sparse-keymap))
  (set-keymap-parent energize-top-level-map energize-map)
  (suppress-keymap energize-top-level-map)
  (define-key energize-top-level-map "?" 'describe-mode)
  (define-key energize-top-level-map " " 'energize-top-next-project)
  (define-key energize-top-level-map "n" 'energize-top-next-project)
  (define-key energize-top-level-map "p" 'energize-top-prev-project)
  (define-key energize-top-level-map "N" 'energize-top-next-project)
  (define-key energize-top-level-map "P" 'energize-top-prev-project)
  (define-key energize-top-level-map "\t" 'energize-top-next-project)
  (define-key energize-top-level-map '(shift tab) 'energize-top-prev-project)
  (define-key energize-top-level-map '(control I) 'energize-top-prev-project)

  (define-key energize-top-level-map "Q" 'disconnect-from-energize-query)

  (define-key energize-top-level-map "d" 'energize-top-debug)
  (define-key energize-top-level-map "\^D" 'energize-top-delete-project)
  (define-key energize-top-level-map "e" 'energize-top-edit-project)
  )

(if energize-project-map
    nil
  (setq energize-project-map (make-sparse-keymap))
  (set-keymap-parent energize-project-map energize-map)
  ;;(suppress-keymap energize-project-map)
  ;;(define-key energize-project-map "\t" 'energize-project-next-field)
  ;;(define-key energize-project-map '(shift tab) 'energize-project-prev-field)
  ;;(define-key energize-project-map '(control I) 'energize-project-prev-field)

  (define-key energize-project-map "\^C\^I" 'energize-import-file)
  (define-key energize-project-map "\^C\^E" 'energize-project-edit-file)
  (define-key energize-project-map "\^C\^S\^A" 'energize-project-sort-alpha)
  (define-key energize-project-map "\^C\^S\^L" 'energize-project-sort-link)
  (define-key energize-project-map "\^C\^V\^N" 'energize-project-view-names)
;  (define-key energize-project-map "\^C\^V\^L" 'energize-project-view-long)
  (define-key energize-project-map "\^C\^V\^C" 'energize-project-view-options)
  )


(if energize-no-file-project-map
    nil
  (setq energize-no-file-project-map (make-sparse-keymap))
  (set-keymap-parent energize-no-file-project-map energize-map))

(if energize-breakpoint-map
    nil
  (setq energize-breakpoint-map (make-sparse-keymap))
  (set-keymap-parent energize-breakpoint-map energize-map)
  )

(if energize-browser-map
    nil
  (setq energize-browser-map (make-sparse-keymap))
  (set-keymap-parent energize-browser-map energize-map)
  )

(if energize-source-map
    nil
  (setq energize-source-map (make-sparse-keymap))
  (set-keymap-parent energize-source-map energize-map)
;;  There are too many problems with using extents to determine where the
;;  top level forms are...
;;  (define-key energize-source-map "\M-\C-a" 'energize-beginning-of-defun)
;;  (define-key energize-source-map "\M-\C-e" 'energize-end-of-defun)
 )

(if energize-user-input-map
    nil
  (setq energize-user-input-map (copy-keymap comint-mode-map))
  (if (keymap-parent comint-mode-map) (error "comint-mode-map has a parent?"))
  (set-keymap-parent energize-user-input-map energize-map)
  (define-key energize-user-input-map "\M-\t" 'comint-dynamic-complete)
  (define-key energize-user-input-map "\M-?" 'comint-dynamic-list-completions)
  )


(defvar energize-menu-state nil
  "State of the energize menu items of the buffer.  
Automatically updated by the kernel when the state changes")

(defvar energize-default-menu-state nil
  "State of the energize default menu items.  
Automatically updated by the kernel when the state changes")

(defun energize-mode-internal ()
  ;; initialize stuff common to all energize buffers (hooks, etc).
  (make-local-variable 'write-file-hooks)
  (if (consp write-file-hooks)
      (setq write-file-hooks (copy-sequence write-file-hooks)))
  (add-hook 'write-file-hooks 'energize-write-file-hook t)
  ;;
  (make-local-variable 'revert-buffer-insert-file-contents-function)
  (setq revert-buffer-insert-file-contents-function
	'energize-revert-buffer-insert-file-contents-hook)
  ;;
  (make-local-variable 'kill-buffer-hook)
  (setq kill-buffer-hook 'energize-kill-buffer-hook)
  ;;
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  ;;
  (make-local-variable 'energize-menu-state)
  ;;
  (run-hooks 'energize-mode-hook))

(defun energize-non-file-mode-internal ()
  ;; do magic associated with energize-modes for buffers which are not
  ;; and cannot be associated with files.
;  (or (null buffer-file-name)
;      (equal buffer-file-name mode-name)
;      (error
;       "This buffer is associated with a file, it can't be placed in %s mode"
;       mode-name))
  ;; hack so that save-file doesn't prompt for a filename.
  (setq buffer-file-name (buffer-name))
  (set (make-local-variable 'version-control) 'never)
  nil)

(defun energize-top-level-mode ()
  "Major mode for the Energize top-level buffer.
In addition to normal cursor-motion commands, the following keys are bound:
\\{energize-top-level-map}"
  (interactive)
  (energize-mode-internal)
  (use-local-map energize-top-level-map)
  (setq major-mode 'energize-top-level-mode
	mode-name "Energize")
  (energize-non-file-mode-internal)
  ;; the default of "energize: Energize" is not very attractive.
  (if (equal screen-title-format "%S: %b")
      (set (make-local-variable 'screen-title-format) "%S: Top-Level"))
  (run-hooks 'energize-top-level-mode-hook))


(defun energize-project-mode ()
  "Major mode for the Energize Project buffers.
In addition to the normal editing commands, the following keys are bound:
\\{energize-project-map}"
  (interactive)
  (energize-mode-internal)
  (use-local-map energize-project-map)
  (setq major-mode 'energize-project-mode
	mode-name "Project")
  ;; in later revisions of the kernel the project is really a file.
  (if (< (cdr (energize-protocol-level)) 8)
      (energize-non-file-mode-internal))
  (run-hooks 'energize-project-mode-hook))

(defun energize-no-file-project-mode ()
  "Major mode for the Energize Project buffers not associated with a file.
In addition to the normal editing commands, the following keys are bound:
\\{energize-no-file-project-map}"
  (interactive)
  (energize-mode-internal)
  (use-local-map energize-no-file-project-map)
  (setq major-mode 'energize-no-file-project-mode
	mode-name "NoFileProject")
  (energize-non-file-mode-internal)
  (run-hooks 'energize-no-file-project-mode-hook))

(defun energize-breakpoint-mode ()
  "Major mode for the Energize Breakpoint-list buffers.
In addition to the normal editing commands, the following keys are bound:
\\{energize-breakpoint-map}"
  (interactive)
  (energize-mode-internal)
  (use-local-map energize-breakpoint-map)
  (setq major-mode 'energize-breakpoint-mode
	mode-name "Breakpoint")
  (energize-non-file-mode-internal)
  (run-hooks 'energize-breakpoint-mode-hook))

(defun energize-browser-mode ()
  "Major mode for the Energize Browser buffers.
In addition to the normal editing commands, the following keys are bound:
\\{energize-browser-map}"
  (interactive)
  (energize-mode-internal)
  (use-local-map energize-browser-map)
  (setq major-mode 'energize-browser-mode
	mode-name "Browser")
  (energize-non-file-mode-internal)
  (run-hooks 'energize-browser-mode-hook))

(defun energize-log-mode ()
  "Major mode for the Energize Error Log and System Log buffers.
In addition to the normal editing commands, the following keys are bound:
\\{energize-map}"
  (interactive)
  (energize-mode-internal)
  (use-local-map energize-map)
  (setq major-mode 'energize-log-mode
	mode-name "Energize-Log")
  (energize-non-file-mode-internal)
  (run-hooks 'energize-log-mode-hook))

(defvar energize-source-mode nil)
;;(put 'energize-source-mode 'permanent-local t) ; persists beyond mode-change

;;; Add energize-source-mode to minor-mode-alist so that it shows up in 
;;; the modeline when true.
;;;
(or (assq 'energize-source-mode minor-mode-alist)
    (setq minor-mode-alist
	  (append minor-mode-alist
		  '((energize-source-mode " Energize")))))

;;; add a function to the find-file-hooks that turns on the energize minor
;;; mode if this is a energize buffer.  This happens both with find-file and
;;; with revert-buffer.
;;;
(if (or (not (listp find-file-hooks))
	(and (consp find-file-hooks) (eq (car find-file-hooks) 'lambda)))
    (setq find-file-hooks (list find-file-hooks)))

(setq find-file-hooks
      (append find-file-hooks '(maybe-turn-on-energize-minor-mode)))

(defun maybe-turn-on-energize-minor-mode ()
  (if (and (energize-buffer-p (current-buffer))
	   (eq (energize-buffer-type (current-buffer))
	       'energize-source-buffer))
      (energize-source-minor-mode))
  ;; if evi mode is loaded and in use, put the new buffer in evi mode.
  (if (and (boundp 'evi-install-undo-list) evi-install-undo-list)
      (energize-evi-mode))
  )

(defun energize-source-minor-mode ()
  "Minor mode for adding additional keybindings to Energize source buffers.
The following key bindings are added:
\\{energize-source-map}

Since this minor mode defines keys, once it gets turned on you can't really
turn it off."
  (interactive)
  (energize-mode-internal)
  (make-local-variable 'energize-source-mode)
  (setq energize-source-mode t)
  (let ((source-map energize-source-map)
	(dest-map (make-sparse-keymap)))
    (set-keymap-parent dest-map (current-local-map))
    (while source-map
      (let (mapper prefixes)
	(setq mapper (function (lambda (key val)
				 (if (keymapp val)
				     (let ((prefixes (append prefixes
							     (cons key nil))))
				       (map-keymap val mapper))
				   (define-key dest-map
				     (apply 'vector
					    (append prefixes (cons key nil)))
				     val)
				   ))))
	(map-keymap source-map mapper))
      (setq source-map (keymap-parent source-map)))
    (use-local-map dest-map))
  (run-hooks 'energize-source-mode-hook))


;;; Commands in source buffers

(defun recenter-definition ()
  "Position the beginning of the current definition at the top of the screen."
  (interactive)
  (end-of-line)
  (if (eq major-mode 'c++-mode)
      (c++-beginning-of-defun 1)
    (beginning-of-defun 1))
  (recenter 1))

(define-key global-map "\M-\C-r" 'recenter-definition)

;;; Dired-like commands

(defun energize-next-extent-for (command prev not-this-one)
  (let ((last-e (if not-this-one 'none nil))
	e result)
    (save-excursion
      (let ((p (point)))
	(while (not (or result
			(if prev (bobp) (eobp))))
	  (setq e (extent-at (point) (current-buffer)))
	  (if (and (not (eq e last-e))
		   (not (eq last-e 'none)))
	      (setq result
		    (energize-menu-item-for-name e command)))
	  (forward-char (if prev -1 1))
	  (setq last-e e))))
    (if result e)))

(defun energize-next-extent-on-line-for (command not-this-one)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (progn (end-of-line) (point)))
      (goto-char (point-min))
      (energize-next-extent-for command nil not-this-one))))


;;; commands in the top-level buffer

(defun energize-top-next-project ()
  "Position the cursor at the beginning of the following project."
  (interactive)
  (let ((p (point)))
    (let ((e (energize-next-extent-for "editproject" nil t)))
      (if (and e (= p (extent-start-position e)))
	  (save-excursion
	    (forward-char (extent-length e))
	    (setq e (energize-next-extent-for "editproject" nil t))))
      (if e
	  (goto-char (extent-start-position e))
	(error "no next project")))))

(defun energize-top-prev-project ()
  "Position the cursor at the beginning of the preceeding project."
  (interactive)
  (let ((p (point)))
    (let ((e (energize-next-extent-for "editproject" t t)))
      (if (and e (= p (extent-start-position e)))
	  (save-excursion
	    (forward-char -1)
	    (setq e (energize-next-extent-for "editproject" t t))))
      (if e
	  (goto-char (extent-start-position e))
	(error "no previous project")))))

(defun energize-top-execute-command (command)
  (let ((e (or (energize-next-extent-on-line-for command nil)
	       (error
		(concat "no following field on this line that handles the `"
			command "' Energize command.")))))
    (energize-execute-command command e)))

(defun energize-top-debug ()
  "Execute the `Debug' command on the project at or following point."
  (interactive)
  (energize-top-execute-command "debugprogram"))

(defun energize-top-delete-project ()
  "Delete the project at or following point."
  (interactive)
  (energize-top-execute-command "deleteproject"))

(defun energize-top-edit-project ()
  "Edit the project at or following point."
  (interactive)
  (energize-top-execute-command "editproject"))

;;; commands in the project buffer

(defun energize-project-next-field (&optional prev)
  (interactive)
  (let ((e (extent-at (point) (current-buffer))))
    (if e
	(if prev
	    (goto-char (1- (extent-start-position e)))
	  (goto-char (1+ (extent-end-position e)))))
    (while (null (extent-at (point) (current-buffer)))
      (forward-char (if prev -1 1)))
    (while (extent-at (point) (current-buffer) 'write-protected)
      (forward-char (if prev -1 1)))
    (if prev
	(if (setq e (extent-at (point) (current-buffer)))
	    (goto-char (extent-start-position e))
	  (while (not (extent-at (point) (current-buffer)))
	    (forward-char -1))))))

(defun energize-project-prev-field () (interactive)
  (energize-project-next-field t))

(defun energize-project-edit-file () (interactive)
  (energize-top-execute-command "editfile"))
