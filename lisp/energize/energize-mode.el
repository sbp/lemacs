;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright � 1991-1993 by Lucid, Inc.  All Rights Reserved.


;; true if current-buffer is an energize buffer that does not support
;; the real write-file and so has to do the special energize way of doing
;; write-file that loses the annotations.
(defun energize-write-file-buffer-p ()
  ;;  (and (energize-buffer-p (current-buffer)
  ;;       (not (eq major-mode 'energize-project-mode)))
  (energize-buffer-p (current-buffer)))


(defun energize-beginning-of-defun (&optional arg)
  "Move point to the beginning of the current top-level form.
With a numeric argument, move back that many forms."
  (interactive "_p")
  (or arg (setq arg 1))
  (if (not (energize-buffer-p (current-buffer)))
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
  (interactive "_p")
  (or arg (setq arg 1))
  (if (not (energize-buffer-p (current-buffer)))
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


;;; Patching Energize into file I/O via the standard hooks.

(defun energize-write-data-hook (name)
  ;; for use as the last element of write-file-data-hooks
  ;; in energize buffers.
  (if (energize-buffer-p (current-buffer))
      (progn
	(message "saving %s to Energize..." name)
	(energize-execute-command "save")
	(energize-update-menubar)
	(message "saved %s to Energize." name)
	t)
    nil))

(defun energize-revert-buffer-insert-file-contents-hook (file noconfirm)
  ;; for use as the value of revert-buffer-insert-file-contents-function
  ;; in energize buffers.
  (if (not (energize-buffer-p (current-buffer)))
      (error "energize-revert-buffer-hook called for a non-energize buffer"))
  (widen)
  (cond ((equal file buffer-file-name)		; reverting from energize
	 ;; Do the default as in files.el
	 (if (file-exists-p file)
	     (progn
	       ;; Bind buffer-file-name to nil
	       ;; so that we don't try to lock the file.
	       (let ((buffer-file-name nil))
		 (unlock-buffer)
		 (erase-buffer))
	       (insert-file-contents file t)))
	 ;; Then asks the extents from Energize
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


;;; 

(defun energize-edit-definition-default ()
  (save-excursion
    (if (not (memq (char-syntax (preceding-char)) '(?w ?_)))
	(while (not (looking-at "\\sw\\|\\s_\\|\\'"))
	  (forward-char 1)))
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (re-search-backward "\\sw\\|\\s_" nil t)
	(progn (forward-char 1)
	       (buffer-substring (point)
				 (progn (forward-sexp -1)
					(while (looking-at "\\s'")
					  (forward-char 1))
					(point))))
      nil)))

;;; This prompts in the minibuffer, ##### with no completion.
(defun energize-edit-definition (def)
  "If connected to Energize, the Energize database is used.  
Otherwise, `find-tag' is invoked.
The X selection is used as a default, if it exists and contains no 
newlines.  Otherwise, the preceeding token is used as a default.  
If invoked from a mouse command, prompting happens with a dialog box; 
otherwise, the minibuffer is used."
  (interactive
   (if (and (connected-to-energize-p)
	    (or (menu-event-p last-command-event)
		(button-press-event-p last-command-event)
		(button-release-event-p last-command-event)))
       '(nil)
     (list
      (let (default
	    def)
	(cond ((x-selection-owner-p)
	       (setq default (x-get-selection))
	       (while (string-match "\\`[ \t\n]+" default)
		 (setq default (substring default (match-end 0))))
	       (while (string-match "[ \t\n]+\\'" default)
		 (setq default (substring default 0 (match-beginning 0))))
	       (if (string-match "[ \t\n]" default)
		   (setq default nil))))
	(or default (setq default (energize-edit-definition-default)))
	(setq def
	      (if (connected-to-energize-p)
		  (completing-read
		   (if default
		       (format "Edit definition [%s]: " default)
		     "Edit definition: ")
		   nil nil; 'energize-edit-def-predicate
		   nil nil)
		(or (and (fboundp 'find-tag-tag) (fboundp 'find-tag-default))
		    (require 'tags "etags"))
		(find-tag-tag "Edit definition: ")))
	(if (consp def) (setq def (car def)))
	(if (equal "" def) (setq def default))
	def))))
  (if (connected-to-energize-p)
      (energize-execute-command "editdef" () def t)
    (find-tag def)))

(define-key global-map "\M-." 'energize-edit-definition)
(define-key global-map "\M-B" 'energize-build-a-target)   ; M-Sh-B

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

(defun energize-insert-object-file-target ()
  (interactive)
  (energize-insert-slots
   ()
   '("     Object File: <object-file>"
     "     Source File: <source-file>"
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

(defun energize-insert-file-target ()
  (interactive)
  (energize-insert-slots
   ()
   '("     File Target: <target>"
     "    Dependencies: <target> ..."
     "   Build Command: <shell-command>")))

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
  "Hook called when an Energize project buffer is created.")
(defvar energize-no-file-project-mode-hook nil
  "Hook called when an Energize project buffer with no file is created.")
(defvar energize-breakpoint-mode-hook nil
  "Hook called when an Energize breakpoint-list buffer is created.")
(defvar energize-browser-mode-hook nil
  "Hook called when an Energize browser buffer is created.")
(defvar energize-log-mode-hook nil
  "Hook called when an Energize log buffer is created.")
(defvar energize-manual-mode-hook nil
  "Hook called when an Energize manual buffer is created.")
(defvar energize-source-mode-hook nil
  "Hook called when any source buffer is placed in the Energize minor-mode.")


(if energize-map
    nil
  (setq energize-map (make-sparse-keymap))
  (set-keymap-name energize-map 'energize-map)
  (define-key energize-map "\^C\^F"	'energize-find-project)
  (define-key energize-map "\^C\^B\^E"	'energize-browse-error)
  (define-key energize-map "\^C\^B\^L"	'energize-browse-language-elt)
  (define-key energize-map "\^C\^B\^T"	'energize-browse-tree)
  (define-key energize-map "\^C\^B\^C"	'energize-browse-class)
;;  now in global-map
;;  (define-key energize-map "\M-B" 'energize-build-a-target) ; M-Sh-B
  (define-key energize-map "\M-C" 'energize-default-compile-file) ; M-Sh-C
  (define-key energize-map 'button3 'energize-popup-menu)
  )

(if energize-top-level-map
    nil
  (setq energize-top-level-map (make-sparse-keymap))
  (set-keymap-name energize-top-level-map 'energize-top-level-map)
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
  (set-keymap-name energize-project-map 'energize-project-map)
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
  (set-keymap-name energize-no-file-project-map 'energize-no-file-project-map)
  (set-keymap-parent energize-no-file-project-map energize-map))

(if energize-breakpoint-map
    nil
  (setq energize-breakpoint-map (make-sparse-keymap))
  (set-keymap-name energize-breakpoint-map 'energize-breakpoint-map)
  (set-keymap-parent energize-breakpoint-map energize-map)
  )

(if energize-browser-map
    nil
  (setq energize-browser-map (make-sparse-keymap))
  (set-keymap-name energize-browser-map 'energize-browser-map)
  (set-keymap-parent energize-browser-map energize-map)
  )

(if energize-source-map
    nil
  (setq energize-source-map (make-sparse-keymap))
  (set-keymap-name energize-source-map 'energize-source-map)
  (set-keymap-parent energize-source-map energize-map)
;;  There are too many problems with using extents to determine where the
;;  top level forms are...
;;  (define-key energize-source-map "\M-\C-a" 'energize-beginning-of-defun)
;;  (define-key energize-source-map "\M-\C-e" 'energize-end-of-defun)
 )

(defvar energize-menu-state nil
  "State of the energize menu items of the buffer.  
Automatically updated by the kernel when the state changes")

(defvar energize-default-menu-state nil
  "State of the energize default menu items.  
Automatically updated by the kernel when the state changes")

(defun energize-mode-internal ()
  ;; initialize stuff common to all energize buffers (hooks, etc).
  (make-local-variable 'write-file-data-hooks)
  (add-hook 'write-file-data-hooks 'energize-write-data-hook t)
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
  (or buffer-file-name
      (setq buffer-file-name (buffer-name)))
  (set (make-local-variable 'version-control) 'never)
  nil)

;; don't create random new buffers in these modes
(put 'energize-top-level-mode		'mode-class 'special)
(put 'energize-project-mode		'mode-class 'special)
(put 'energize-no-file-project-mode	'mode-class 'special)
(put 'energize-breakpoint-mode		'mode-class 'special)
(put 'energize-browser-mode		'mode-class 'special)
(put 'energize-log-mode			'mode-class 'special)

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

(defun energize-manual-mode ()
  "Major mode for the Energize UNIX Manual buffers.
In addition to the normal editing commands, the following keys are bound:
\\{energize-map}"
  (interactive)
  (energize-mode-internal)
  (use-local-map energize-map)
  (setq major-mode 'energize-manual-mode
	mode-name "Energize-Manual")
  (energize-non-file-mode-internal)
  (run-hooks 'energize-manual-mode-hook))

(defvar energize-source-mode nil)
;;(put 'energize-source-mode 'permanent-local t) ; persists beyond mode-change

;;; Add energize-source-mode to minor-mode-alist so that it shows up in 
;;; the modeline when true.
;;;
(or (assq 'energize-source-mode minor-mode-alist)
    (setq minor-mode-alist
	  (append minor-mode-alist
		  '((energize-source-mode " Energize")))))


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
    (set-keymap-name dest-map 'energize-minor-mode-map)
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

(defun energize-hide-error-glyphs-in-form ()
  "Hides the error icons in the current toplevel form.
You cannot get them back until you recompile the file."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((start (progn (energize-beginning-of-defun) (point)))
	    (end (progn (energize-end-of-defun) (point)))
	    e)
	(narrow-to-region start end)
	(goto-char (point-min))
	(setq e (extent-at (point)))
	(while (and e
		    (< (extent-end-position e) (point-max)))
	  (if (extent-property e 'begin-glyph)
	      (set-extent-begin-glyph e nil))
	  (setq e (next-extent e)))))))

;;; Dired-like commands

(defun energize-next-extent-for (command prev not-this-one)
  (let ((last-e (if not-this-one 'none nil))
	e result)
    (save-excursion
      (while (not (or result
		      (if prev (bobp) (eobp))))
	(setq e (extent-at (point) (current-buffer)))
	(if (and (not (eq e last-e))
		 (not (eq last-e 'none)))
	    (setq result
		  (energize-menu-item-for-name e command)))
	(forward-char (if prev -1 1))
	(setq last-e e)))
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


(defun energize-project-prune-unused-rules ()
  "Deletes all unused rules from the Rules: part of a Project buffer,
and renumbers the remaining rules sequentally."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^[ \t]+Rules:")
    (forward-line 1)
    (let ((rules-regexp "^[ \t]*\\(\\.[a-zA-Z]+\\(([0-9]+)\\)?\\):")
	  (all-rules nil)
	  eor)
      ;;
      ;; Gather the contents of the Rule section, and find its end.
      ;;
      (save-excursion
	(while (looking-at rules-regexp)
	  (setq all-rules (cons (list (buffer-substring (match-beginning 1)
							(match-end 1))
				      (point-marker))
				all-rules))
	  (while (progn (end-of-line) (= (preceding-char) ?\\))
	    (forward-line 1))
	  (forward-line 1))
	(setq eor (point-marker)))
      (setq all-rules (nreverse all-rules))
      (let ((rest all-rules)
	    rule)
	;;
	;; Walk through the buffer gathering references to the rules.
	;;
	(while rest
	  (setq rule (car rest))
	  (goto-char eor)
	  (let ((pattern (concat "^[ \t]+" (regexp-quote (car rule)) ":")))
	    (while (re-search-forward pattern nil t)
	      (setcdr (cdr rule)
		      (cons (set-marker (make-marker) (match-beginning 0))
			    (cdr (cdr rule))))))
	  (setq rest (cdr rest)))
	;;
	;; Delete those rules that have no references.
	;;
	(goto-char eor)
	(setq rest all-rules)
	(while rest
	  (setq rule (car rest))
	  (if (null (cdr (cdr rule)))
	      (let ((p (nth 1 rule)))
		(goto-char p)
		(while (progn (end-of-line) (= (preceding-char) ?\\))
		  (forward-line 1))
		(forward-line 1)
		(delete-region p (point))
		(set-marker p nil)
		(setq all-rules (delq rule all-rules))
		))
	  (setq rest (cdr rest)))
	;;
	;; Renumber the remaining rules sequentally.
	;;
	(goto-char eor)
	(setq rest all-rules)
	(let ((i 1))
	  (while rest
	    (setq rule (car rest))
	    (let ((referents (cdr rule))) ; including definition
	      (while referents
		(goto-char (car referents))
		(or (and (looking-at
			  (concat "^[ \t]+" (regexp-quote (car rule)) ":"))
			 (looking-at "[^:(]+\\((\\([0-9]+\\))\\|\\):"))
		    (error "internal error"))
		(if (null (match-beginning 2))
		    (progn
		      (goto-char (match-beginning 1))
		      (insert "(" (int-to-string i) ")"))
		  (goto-char (match-beginning 2))
		  (delete-region (match-beginning 2) (match-end 2))
		  (insert (int-to-string i)))
		(set-marker (car referents) nil)
		(setq referents (cdr referents))))
	    (setq i (1+ i))
	    (setq rest (cdr rest))))
	;;
	;; TODO:
	;; - order the Rule Users list in the same order as the Rules list.
	;; - or, order the Rule Users list by number of files, and then
	;;   order the Rules list the same as that (numbered sequentially.)
	;; - or, order the Rules list by length-of-rule (= complicatedness.)
	)
      (set-marker eor nil))))
