;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright © 1992, 1993, 1994 by Lucid, Inc.  All Rights Reserved.

;;; The names of the menu items (as emacs sees them) are short and ugly.
;;; These are the names by which the Energize protocol knows the commands.
;;; The menu items are made to display in a more human-friendly way via the
;;; X resource database, which is expected to contain entries like
;;;
;;;	*buildanddebug.labelString:	Build and Debug
;;;
;;; in the Emacs app-defaults file.
;;;
;;; We need to map these short Energize-names to the functions which invoke
;;; them; we do this via the energize-menu-item-table, which is an obarray
;;; hash table associating the names with the functions.  We do the reverse
;;; association via an 'energize-name property on the function's name symbol.
;;;
;;; Sometimes the short ugly names show up in error messages; probably we
;;; should read the resource database to get the pretty names.

(require 'menubar)

(defvar sc-mode nil)		; just so it has a value even if not loaded
(defvar font-lock-mode nil)	; likewise

(defconst energize-menu-item-table (make-vector 511 nil)
  "obarray used for fast mapping of symbolic energize request-names to the 
functions that invoke them.")

(defvar energize-default-menu-state ()
  "List of the Energize menu items associated with every buffers.")

(defvar energize-menu-state ()
  "Buffer local variable listing the menu items associated with a buffer.")

;; When it is made local, don't kill it when kill-all-local-variables is
;; called (as from the major mode via revert-buffer) or else we tend to lose
;; the information, as the ProposeChoicesRequest comes in at an inopportune
;; time.
(put 'energize-menu-state 'permanent-local t)

;;; Hook to update the menu state when the kernel tells us it changed

(defun energize-update-menu-state (items)
  (let ((buffer (car items))
	(previous-buffer (current-buffer)))
    (if (null buffer)
	(setq energize-default-menu-state items)
      (unwind-protect
	  (progn
	    (set-buffer buffer)
	    (setq energize-menu-state items))
	(set-buffer previous-buffer)))))

(setq energize-menu-update-hook 'energize-update-menu-state)

;;; The energize-with-timeout macro is used to show to the user that we are 
;;; waiting for a reply from the energize kernel when it is too slow.

(defvar initial-energize-timeout-state
  (let ((l '("." ".." "..." "...." "....." "......" "......." "........")))
    (nconc l l)))

(defvar energize-timeout-state initial-energize-timeout-state)

(defun energize-warn-kernel-slow (pair)
  (setq energize-timeout-state (cdr energize-timeout-state))
  (message "%s Type %c to cancel%s"
	   (car pair) interrupt-char (car energize-timeout-state))
  (rplacd pair t))

(defmacro energize-with-timeout (notice &rest body)
  (list 'let* (list
	       (list 'timeout-pair (list 'cons notice nil))
	       '(timeout (add-timeout 1.5 'energize-warn-kernel-slow
				      timeout-pair 1.5)))
	(list 'unwind-protect (cons 'progn body)
	      '(disable-timeout timeout)
	      '(setq energize-timeout-state initial-energize-timeout-state)
	      '(if (cdr timeout-pair) (message "")))))

(defun energize-def-menu-item (name function &optional dont-define)
  ;; function->name mapping is on the function name's plist
  ;; name->function mapping is via an obarray
  ;; dont-define means it already has a function definition
  (put function 'energize-name (purecopy name))
  (set (intern name energize-menu-item-table) function)
  ;; Define the (trivial) function
  ;; It's ok that this function is interpreted, because it contains only
  ;; one function call with constant args, so it's just as fast as it would
  ;; be if it were byte-coded.
  (if (not dont-define)
      (fset function
	    (purecopy
	     (` (lambda ()
		  (, (format "Executes the Energize \"%s\" command." name))
		  (interactive)
		  (energize-execute-command (, name)))))))
  ;; Return the menu-item descriptor.
  (vector (purecopy name) function nil nil))

(defmacro energize-def-menu (menu-name &rest items)
  (` (list (, menu-name)
	   (,@ (mapcar
		'(lambda (x)
		   (if (and (consp x) (stringp (car x)))
		       (cons 'energize-def-menu-item
			     (mapcar '(lambda (xx)
					(if (stringp xx)
					    (purecopy xx)
					  (list 'quote xx)))
				     x))
		     x))
		items)))))

(put 'energize-def-menu 'lisp-indent-function 1)


;; If menubar-religion is 'winning, the menubar looks like jwz likes it.
;; If menubar-religion is 'losing, the menubar looks like Gareth and the
;; documentation folks like it.  See also menubar.el - it consults this
;; variable for the layout of the File menu which is inherited here.

(defconst energize-menubar
 (purecopy-menubar
  (list
   ["sheet" energize-toggle-psheet nil]

   ;; Perform some surgery on the default File menu to insert our items.
   ;; This is to avoid having to duplicate it here...  Don't try this at
   ;; home, kids!
   (let* ((file (copy-sequence
		 (car (find-menu-item default-menubar '("File")))))
	  (print (car (find-menu-item file '("Print Buffer"))))
	  (exit  (car (find-menu-item file '("Exit Emacs"))))
	  (print-cons (memq print file))
	  (exit-cons  (memq exit file))
	  )
     ;; Insert "Print Annotated" just after "Print"
     (setcdr print-cons (cons '["Print Annotated Buffer"
				energize-annotate-print-ps
				t]
			      (cdr print-cons)))

     ;; Insert "Checkpoint" and "Shutdown" just before "Exit Emacs".
     (setcar exit-cons ["Connect to Energize" energize-menu-connect-directly
			 (not (connected-to-energize-p))])
     (setcdr exit-cons
	     (nconc
	      (list (energize-def-menu-item "checkpoint"
					    'energize-checkpoint-database)
		    ["Disconnect from Energize" disconnect-from-energize
		     (connected-to-energize-p)]
		    "----"
		    (energize-def-menu-item "energizeShutdownServer"
					    'energize-kill-server)
		    )
	      (if (not (eq menubar-religion 'winning))
		  (list "----"))
	      (list exit)))
     file)
     
   ;; Energize also adds some menu items to the middle of the "Edit" menu.
   ;; Someday these should be moved to the default menubar, maybe, once it's
   ;; easier to define `energize-search' in a non-Energize world.
   (let* ((edit (copy-sequence
		 (car (find-menu-item default-menubar '("Edit")))))
	  (clear (car (find-menu-item edit '("Clear"))))
	  (clear-cons (memq clear edit))
	  )
     ;; Insert these just after "Clear"
     (setcdr clear-cons
	     (append '("-----"
		       ["Search and Replace..." energize-search t]
		       ["Search Selection Forward" ow-find
			(or ow-find-last-string (x-selection-owner-p))]
		       ["Search Selection Backward" ow-find-backward
			(or ow-find-last-string (x-selection-owner-p))]
		       )
		     (cdr clear-cons)))
     edit)

    (energize-def-menu "Browse" 
     ["editdef" energize-edit-definition t]
     ("editdec" energize-edit-declaration-dbox)
     ("calltreebrowser" energize-browse-tree)
     ("classbrowser" energize-browse-class)
     ("lebrowser" energize-browse-language-elt)
     ("includers" energize-where-included)
     "-----" 

     ;; Make Energize control the selectability of these, but don't define
     ;; the functions here (they are defined in lisp, not as aliases for
     ;; an Energize command.)

     ;; No, this doesn't seem to work. Energize disowns all knowledge.
     ["visituse" energize-next-use-start (connected-to-energize-p)]
     ["nextuse" energize-next-use-command (connected-to-energize-p)]
     "-----" 
     ["List History" energize-history (connected-to-energize-p)]
     ["Step Back in History" energize-history-previous (connected-to-energize-p)]
     "-----"
     ("energize" energize-pop-to-energize-buffer)
     ("showsystemlog"	energize-browse-system-log)
     ("errorbrowser" energize-browse-error)
     "-----"
     ("toolstatus"	energize-browse-toolstat)
     ["Shell" shell t]
     )

 (if (eq menubar-religion 'winning)

    (list
     ;; Winning
     "Options"
     (energize-def-menu-item "debuggerpanel" 'energize-show-debugger-panel)
     "------"
     ["Read Only" toggle-read-only :style toggle :selected buffer-read-only]
     ["Case Sensitive Search" (setq case-fold-search (not case-fold-search))
      :style toggle :selected (not case-fold-search)]
     ["Overstrike" overwrite-mode :style toggle :selected overwrite-mode]
     ["Auto Delete Selection" (if (memq 'pending-delete-pre-hook
					pre-command-hook)
				  (pending-delete-off nil)
				(pending-delete-on nil))
      :style toggle :selected (memq 'pending-delete-pre-hook pre-command-hook)]
     ["Teach Extended Commands" (setq teach-extended-commands-p
				      (not teach-extended-commands-p))
      :style toggle :selected teach-extended-commands-p]
;     ["Line Numbers" (line-number-mode nil)
;      :style toggle :selected line-number-mode]
     '("Syntax Highlighting" 
       ["None" (font-lock-mode 0) :style radio :selected (null font-lock-mode)]
       ["Fonts" (progn (require 'font-lock)
		       (font-lock-use-default-fonts)
		       (font-lock-mode 1))
       :style radio
       :selected (and font-lock-mode
		      (equal (find-face 'italic)  ; kind of a kludge...
			     (find-face 'font-lock-comment-face)))]
      ["Colors" (progn (require 'font-lock)
		       (font-lock-use-default-colors)
		       (font-lock-mode 1))
       :style radio
       :selected (and font-lock-mode
		      (not (equal (find-face 'italic)
				  (find-face 'font-lock-comment-face))))]
      "-----"
      ["Less" (progn (require 'font-lock)
		     (font-lock-use-default-minimal-decoration)
		     (font-lock-mode 0)
		     (font-lock-mode 1))
       :style radio
       :selected (and font-lock-mode
		      (eq c++-font-lock-keywords c-font-lock-keywords-1))]
      ["More" (progn (require 'font-lock)
		     (font-lock-use-default-maximal-decoration)
		     (font-lock-mode 0)
		     (font-lock-mode 1))
       :style radio
       :selected (and font-lock-mode
		      (eq c++-font-lock-keywords c-font-lock-keywords-2))]
      )
     '("Paren Highlighting"
       ["None" (blink-paren 0)
	:style radio
	:selected (not (memq 'blink-paren-pre-command pre-command-hook))]
       ["Blink" (progn
		  (setq highlight-paren-expression nil)
		  (blink-paren 1))
	:style radio
	:selected (and (not highlight-paren-expression)
		       (memq 'blink-paren-pre-command pre-command-hook))]
       ["Highlight" (progn
		      (setq highlight-paren-expression t)
		      (blink-paren 1))
	:style radio
	:selected (and highlight-paren-expression
		       (memq 'blink-paren-pre-command pre-command-hook))]
       )
     "------"
     '("Font"	"initialized later")
     '("Size"	"initialized later")
     '("Weight"	"initialized later")
     "-----"
     ["Energize Edit Modes..." energize-set-edit-modes t]
     (energize-def-menu-item "setprojectdisplay"
			     'energize-set-project-display)
     (list "Target Display"
	   (energize-def-menu-item "fulltargets"
				   'energize-full-targets)
	   (energize-def-menu-item "abbreviatetargets"
				   'energize-abbreviate-targets))
     '("Source Control"
       ["None" (sc-mode nil)   :style radio :selected (eq sc-mode nil)]
       ["SCCS" (sc-mode 'SCCS) :style radio :selected (eq sc-mode 'SCCS)]
       ["RCS"  (sc-mode 'RCS)  :style radio :selected (eq sc-mode 'RCS)]
       ["CVS"  (sc-mode 'CVS)  :style radio :selected (eq sc-mode 'CVS)]
       ["ClearCase" (sc-mode 'CCASE):style radio :selected (eq sc-mode 'CCASE)]
       )
     "-----"
     ["Buffers Menu Length..."
      (progn
	(setq buffers-menu-max-size
	      (read-number
	       "Enter number of buffers to display (or 0 for unlimited): "))
	(if (eq buffers-menu-max-size 0) (setq buffers-menu-max-size nil)))
      t]
     ["Buffers Sub-Menus" (setq complex-buffers-menu-p
				(not complex-buffers-menu-p))
      :style toggle :selected complex-buffers-menu-p]
     "-----"
     ["Save Options" save-options-menu-settings t]
     )

    (list
     ;; Non-winning
     "Options" 
     ["Split Screen"		split-window-vertically t]
     ["Unsplit"			delete-other-windows	t]
     "------"
     (energize-def-menu-item "debuggerpanel" 'energize-show-debugger-panel)
     "------"
     ["Read Only" toggle-read-only :style toggle :selected buffer-read-only]
     ["Overstrike " overwrite-mode :style toggle :selected overwrite-mode]
     ["Auto Delete Selection" (if (memq 'pending-delete-pre-hook
					pre-command-hook)
				  (pending-delete-off nil)
				(pending-delete-on nil))
      :style toggle :selected (memq 'pending-delete-pre-hook pre-command-hook)]
     ["Teach Extended" (setq teach-extended-commands-p
			     (not teach-extended-commands-p))
      :style toggle :selected teach-extended-commands-p]
     "------"
     '("Font"	"initialized later")
     '("Size"	"initialized later")
     '("Weight"	"initialized later")
     "------"
     '("Syntax Highlighting" 
       ["None" (font-lock-mode 0) :style radio :selected (null font-lock-mode)]
       ["Fonts" (progn (font-lock-use-default-fonts) (font-lock-mode 1))
	:style radio
	:selected (and font-lock-mode
		       (equal (find-face 'italic) ; kind of a kludge...
			      (find-face 'font-lock-comment-face)))]
       ["Colors" (progn (font-lock-use-default-colors) (font-lock-mode 1))
	:style radio
	:selected (and font-lock-mode
		       (not (equal (find-face 'italic)
				   (find-face 'font-lock-comment-face))))]
       "-----"
       ["Less" (progn (font-lock-use-default-minimal-decoration)
		      (font-lock-mode 0)
		      (font-lock-mode 1))
	:style radio
	:selected (and font-lock-mode
		       (eq c++-font-lock-keywords c-font-lock-keywords-1))]
       ["More" (progn (font-lock-use-default-maximal-decoration)
		      (font-lock-mode 0)
		      (font-lock-mode 1))
	:style radio
	:selected (and font-lock-mode
		       (eq c++-font-lock-keywords c-font-lock-keywords-2))]
       )
     '("Paren Highlighting"
       ["None" (blink-paren 0)
	:style radio
	:selected (not (memq 'blink-paren-pre-command pre-command-hook))]
       ["Blink" (progn
		  (setq highlight-paren-expression nil)
		  (blink-paren 1))
	:style radio
	:selected (and (not highlight-paren-expression)
		       (memq 'blink-paren-pre-command pre-command-hook))]
       ["Highlight" (progn
		      (setq highlight-paren-expression t)
		      (blink-paren 1))
	:style radio
	:selected (and highlight-paren-expression
		       (memq 'blink-paren-pre-command pre-command-hook))]
       )
     "-----"
     ["Energize Edit Modes..." energize-set-edit-modes t]
     (energize-def-menu-item "setprojectdisplay"
			     'energize-set-project-display)
     (list "Target Display"
	   (energize-def-menu-item "fulltargets"
				   'energize-full-targets)
	   (energize-def-menu-item "abbreviatetargets"
				   'energize-abbreviate-targets))
     "-----"
     ["Buffers Length..."
      (progn
	(setq buffers-menu-max-size
	      (read-number
	       "Enter number of buffers to display (or 0 for unlimited): "))
	(if (eq buffers-menu-max-size 0) (setq buffers-menu-max-size nil)))
      t]
     ["Buffers Menus" (setq complex-buffers-menu-p
			    (not complex-buffers-menu-p))
      :style toggle :selected complex-buffers-menu-p]
     "-----"
     '("Source Control"
       ["None" (sc-mode nil)   :style radio :selected (eq sc-mode nil)]
       ["SCCS" (sc-mode 'SCCS) :style radio :selected (eq sc-mode 'SCCS)]
       ["RCS"  (sc-mode 'RCS)  :style radio :selected (eq sc-mode 'RCS)]
       ["CVS"  (sc-mode 'CVS)  :style radio :selected (eq sc-mode 'CVS)]
       ["ClearCase" (sc-mode 'CCASE):style radio :selected (eq sc-mode 'CCASE)]
       )
     "-----"
     ["Save Options" save-options-menu-settings t]
     )
       
    )

 (if (eq menubar-religion 'winning)

   (energize-def-menu "Debug"
     ;; Winning
     ("debugprogram"	energize-debug-target)
     ("runprogram"	energize-run-target)
     "-----"
     ;; Make Energize control the selectability of the setbreakpoint item, but
     ;; don't define the function here (it just runs the existing gdb-break
     ;; command, which is advised to hack Energize.)
     ("setbreakpoint"	gdb-break t)
     ("breaklist"	energize-list-breakpoints)
     "-----"
     ["Next Error" next-error t]
     ["Previous Error" previous-error
      :keys "\\[universal-argument] \\[next-error]"]
     ("errorbrowser" energize-browse-error)
     ("clearerrorlog" energize-clear-error-log)
     ("cleardebuggerlog"	energize-clear-debugger-log)
     "-----" 
     ("closeprogram"	energize-debugger-kill-program)
     ("quitdebugger"	energize-quit-debugger)
     )

   (energize-def-menu "Debug"
     ;; Non-winning
     ("debugprogram"	energize-debug-target)
     ("runprogram"	energize-run-target)
     "-----"
     ;; Make Energize control the selectability of the setbreakpoint item, but
     ;; don't define the function here (it just runs the existing gdb-break
     ;; command, which is advised to hack Energize.)
     ("setbreakpoint"	gdb-break t)
     "-----"
     ("debuggerpanel"	energize-show-debugger-panel)
     "-----"
     ("breaklist"	energize-list-breakpoints)
     ("cleardebuggerlog"	energize-clear-debugger-log)
     "-----"
     ("errorbrowser" energize-browse-error)
     ("clearerrorlog" energize-clear-error-log)
     "-----" 
     ["Next Error" next-error t]
     ["Previous Error" previous-error
      :keys "\\[universal-argument] \\[next-error]"]
     "-----"
     ("closeprogram"	energize-debugger-kill-program)
     "-----"
     ("quitdebugger"	energize-quit-debugger)
     )
   )

 (if (eq menubar-religion 'winning)

   (energize-def-menu "Compile"
     ;; Winning
     ("buildatarget" energize-build-a-target)
     ("custombuildatarget" energize-custom-build-a-target)
;; Matthieu believed that this could be done now; however it would seem that
;; it still can't. So out it goes for the time being.
;;     "-----" 
;;     ("Terminate Build"  energize-abort-build)
     "-----"
     ["Next Error" next-error t]
     ["Previous Error" previous-error
      :keys "\\[universal-argument] \\[next-error]"]
     ("errorbrowser" energize-browse-error)
     ("clearerrorlog" energize-clear-error-log)
     "-----"
     ("defaultcompile" energize-default-compile-file)
     ("custombuildfile" energize-custom-build-file)
     "-----" 
     ("deleteallobjects" energize-delete-object-files)
     )

   (energize-def-menu "Compile" 
     ;; Non-winning
     ("buildatarget" energize-build-a-target)
     ("custombuildatarget" energize-custom-build-a-target)
     "-----"
     ("defaultcompile" energize-default-compile-file)
     ("custombuildfile" energize-custom-build-file)
     "-----"
     ("errorbrowser" energize-browse-error)
     ("clearerrorlog" energize-clear-error-log)
     "-----" 
     ["Next Error" next-error t]
     ["Previous Error" previous-error
      :keys "\\[universal-argument] \\[next-error]"]
;; Matthieu believed that this could be done now; however it would seem that
;; it still can't. So out it goes for the time being.
;;     "-----" 
;;     ("Terminate Build"  energize-abort-build)
     "-----" 
     ("deleteallobjects" energize-delete-object-files)
     )
   )

 (if (eq menubar-religion 'winning)

   (list "Project"
     ;; Winning
     (energize-def-menu-item "newproject" 'energize-new-project)
     (energize-def-menu-item "findproject" 'energize-find-project)
     ["Save Project" save-buffer (eq major-mode 'energize-project-mode)]
     ["currentproject" energize-pop-to-project-buffer nil nil]
     (energize-def-menu-item "energize" 'energize-pop-to-energize-buffer)
     "-----"
     '("addprojectentry"
       ["addobjectfiletarget"	energize-insert-object-file-target
       				(eq major-mode 'energize-project-mode)]
       "-----"
       ["addexecutabletarget"	energize-insert-executable-target
       				(eq major-mode 'energize-project-mode)]
       ["addlibrarytarget"	energize-insert-library-target
       				(eq major-mode 'energize-project-mode)]
       ["addcollectiontarget"	energize-insert-collection-target
				(eq major-mode 'energize-project-mode)]
       "-----"
       ["addtargettarget"	energize-insert-target-target
       				(eq major-mode 'energize-project-mode)]
       ["addfiletarget"		energize-insert-file-target
       				(eq major-mode 'energize-project-mode)]
       "-----"
       ["addrule"		energize-insert-rule
	                        (eq major-mode 'energize-project-mode)]
      )
     (energize-def-menu-item "instrumentatarget" 'energize-instrument-a-target)
     "-----"
     (energize-def-menu-item "importproject" 'energize-import-project)
     (energize-def-menu-item "importprojectlist" 'energize-import-project-list)
     (energize-def-menu-item "writeprojectlist" 'energize-write-project-list)
     "-----"
     (energize-def-menu-item "setprojectdisplay"
			     'energize-set-project-display)
     (list "Target Display"
	   (energize-def-menu-item "fulltargets"
				   'energize-full-targets)
	   (energize-def-menu-item "abbreviatetargets"
				   'energize-abbreviate-targets))
     "-----"
     (energize-def-menu-item "revertproject"
			     'energize-fully-revert-project-buffer)
     )

   (list "Project"
     ;; Non-winning
     (energize-def-menu-item "newproject" 'energize-new-project)
     (energize-def-menu-item "findproject" 'energize-find-project)
     ["Save Project" save-buffer (eq major-mode 'energize-project-mode)]
     "-----"
     (energize-def-menu-item "energize" 'energize-pop-to-energize-buffer)
     ["currentproject" energize-pop-to-project-buffer nil nil]
     "-----"
     ["New C/C++ File"		energize-insert-object-file-target
       				(eq major-mode 'energize-project-mode)]
     '("addprojectentry"
       ["addobjectfiletarget"	energize-insert-object-file-target
       				(eq major-mode 'energize-project-mode)]
       "-----"
       ["addexecutabletarget"	energize-insert-executable-target
       				(eq major-mode 'energize-project-mode)]
       ["addlibrarytarget"	energize-insert-library-target
       				(eq major-mode 'energize-project-mode)]
       ["addcollectiontarget"	energize-insert-collection-target
				(eq major-mode 'energize-project-mode)]
       "-----"
       ["addtargettarget"	energize-insert-target-target
       				(eq major-mode 'energize-project-mode)]
       ["addfiletarget"		energize-insert-file-target
       				(eq major-mode 'energize-project-mode)]
       "-----"
       ["addrule"		energize-insert-rule
	                        (eq major-mode 'energize-project-mode)]
      )
     "-----"
     (energize-def-menu-item "instrumentatarget" 'energize-instrument-a-target)
     "-----"
     (energize-def-menu-item "importproject" 'energize-import-project)
     (energize-def-menu-item "importprojectlist" 'energize-import-project-list)
     "-----"
     (energize-def-menu-item "writeprojectlist" 'energize-write-project-list)
     "-----"
     (energize-def-menu-item "setprojectdisplay"
			     'energize-set-project-display)
     (list "Target Display"
	   (energize-def-menu-item "fulltargets"
				   'energize-full-targets)
	   (energize-def-menu-item "abbreviatetargets"
				   'energize-abbreviate-targets))
     "-----"
     (energize-def-menu-item "revertproject"
			     'energize-fully-revert-project-buffer)
     )
   )


    '("Buffers" )

    nil		; the partition: menus after this are flushright

    ;; We don't make any changes to the Help menu.
    ;; WelcomeMat requires one change: added separately though
    (car (find-menu-item default-menubar '("Help")))
    )))

;; For this command, the menu name (the resource) is "currentproject"
;; but the Energize command is "project".  the Energize command is 
;; historical, and the resource name was changed so that the "Project"
;; menu and the "Project" menu item don't necessarily have to be the
;; same text.
;;
(energize-def-menu-item "project" 'energize-pop-to-project-buffer)

;; code for tighter integration with specific tools

(defun energize-menu-connect-directly ()
  (interactive)
  (connect-to-energize nil))

(defvar energize-instrument-menu-options nil
  "List of menu items which are instruments for Energize targets")

(defun energize-define-instrumentatarget-using-tool (tool)
  "Add a menu item (and function) supporting instrumenting a particular tool"
  (let ((function (intern (concat "energize-instrumentatarget-using-" tool)))
	(l energize-instrument-menu-options)
	(name (if (equal tool "") "DBX Compatible" (capitalize tool))))
    (add-menu-item '("Project") (cons name "")
		   function
		   '(connected-to-energize-p)
		   "instrumentatarget")
    (add-hook 'energize-hack-popup-hook 'energize-hack-instruments-in-popup)
    (while (and l (not (equal (car l) tool)))
      (setq l (cdr l)))
    (if (null l) (setq energize-instrument-menu-options
		       (cons tool energize-instrument-menu-options)))
    (fset function
	  (` (lambda ()
	       (, (format "Instruments a target using \"%s\"" tool))
	       (interactive)
	       (energize-execute-command "instrumentatarget" nil
					 (, tool) t))))))

(defun energize-hack-instruments-in-popup (ex m)
  (let ((l (cdr m)))
    (while l
      (if (equal (aref (car l) 0) "instrument")
	  (let ((r energize-instrument-menu-options)
		v)
	    (while r
	      (setq v (vconcat (car l)))
	      (let ((name
		     (if (equal (car r) "") "DBX Compatible"
		       (capitalize (car r)))))
		  (aset (car l) 0 name)) 
	      (aset (car l) 1 (intern (concat
				       "energize-instrumentatarget-using-"
				       (car r))))
	      (setcdr l (cons v (cdr l)))
	      (setq r (cdr r)))
	    (setq l nil))
	(setq l (cdr l))))
    m))

(defun energize-sensitize-instruments-hook ()
  "Sensitize the menubar by adding the executable to any derived
instrumented targets"
  (condition-case nil ; in case Project menu doesn't exist
      (let* ((l energize-instrument-menu-options)
	     (institem
	      (car (find-menu-item current-menubar
				   '("Project" "instrumentatarget"))))
	     (exenable (aref institem 2))
	     (exname (aref institem 3))
	     item)
	(while l
	  (let ((citem (if (equal (car l) "") "DBX Compatible" (car l))))
	    (setq item (car (find-menu-item current-menubar
					    (list "Project" citem)))))
	  (aset item 2 exenable)
	  (aset item 3 exname)
	  (setq l (cdr l))))
    (error nil)))


;; Why is this interactive?  Why is it so random?  -jwz
(defun energize-set-default-menubar ()
  (interactive)
  (set-menubar energize-menubar)
  (add-hook 'activate-menubar-hook 'build-buffers-menu-hook)
  (add-hook 'activate-menubar-hook 'sensitize-file-and-edit-menus-hook)
  (add-hook 'activate-menubar-hook 'energize-sensitize-instruments-hook 't)
  (setq buffers-menu-max-size 20)
  (setq complex-buffers-menu-p ()))

(energize-set-default-menubar)


;; enable purify & plain dbx by default
;; you can enable the others by copying to .emacs and uncommenting ...
;; can't do this here because this file comes preloaded.

(energize-define-instrumentatarget-using-tool "")
(energize-define-instrumentatarget-using-tool "purify")
;; (energize-define-instrumentatarget-using-tool "quantify")
;; (energize-define-instrumentatarget-using-tool "sentinel")
;; (energize-define-instrumentatarget-using-tool "tc")
;; (energize-define-instrumentatarget-using-tool "time")
;; (energize-define-instrumentatarget-using-tool "xproba")

;; add the menu item Help->About Energize for the Energize Welcome Mat
(add-menu-item '("Help") (purecopy "About Energize")
	       'energize-about-energize t)

(defun energize-about-energize ()
  (interactive)
  (start-process "about-energize" nil "about_energize"))

(defun energize-kill-server ()
  "Kill the Energize server and all buffers associated with it."
  (interactive)
  (condition-case nil
      (energize-execute-command "energizeShutdownServer")
    (error nil)))

(defun energize-unix-manual ()
  "Display a manual entry; if connected to Energize, uses the Energize version.
Otherwise, just runs the normal emacs `manual-entry' command."
  (interactive)
  (if (connected-to-energize-p)
      (energize-execute-command "manual")
    (call-interactively 'manual-entry)))

;;; These functions are used in the menubar activate hook to update the
;;; enable state of the menu items

(defvar active-items) ; quiet compiler
(defsubst activate-energize-menu-item-internal (item)
  (cond
   ((vectorp item)
    (let ((fn (aref item 1)))
      (if (not (and (symbolp fn) (get fn 'energize-name)))
	  nil
	;; Referencing special binding of `active-items' from a-e-m-i-hook.
	;; If the function which this item invokes is an Energize function
	;; (determined by the presence of an 'energize-name property) then
	;; make it be active iff it's on the active-items list.
	(let ((active-p (assq fn active-items))
	      (change-p nil))
	  (if (not (eq (not active-p) (not (aref item 2))))
	      (progn
		(aset item 2 (not (not active-p)))
		(setq change-p t)))
	  (if (and active-p
		   (not (equal (cdr active-p)
			       (if (> (length item) 3)
				   (aref item 3)
				 nil))))
	      (progn
		(aset item 3 (cdr active-p))
		(setq change-p t)))
	  change-p))))
   ((consp item)			; descend nested submenus
    (activate-energize-menu-items-internal (cdr item)))
   (t nil)))

(defun activate-energize-menu-items-internal (items)
  (let ((change-p nil))
    (if (not (consp items))
	(activate-energize-menu-item-internal items)
      (while items
	(setq change-p (or (activate-energize-menu-item-internal (car items))
			   change-p)
	      items (cdr items)))
      change-p)))

(defun energize-build-menubar-names ()
  ;;; makes the list of currently active menu items.
  (let* ((selection-p (x-selection-exists-p 'PRIMARY))
	 (menubar
	  (if (< (cdr (energize-protocol-level)) 7)
	      (energize-with-timeout
	       "Getting updated menubar from Energize server..."
	       (energize-list-menu (current-buffer) () selection-p))
	    (append energize-menu-state energize-default-menu-state))))
    (delq nil
	  (mapcar '(lambda (x)
		     (and (vectorp x)
			  (if (/= 0 (logand 1 (aref x 3)))
			      nil
			    (cons
			     (symbol-value
			      (intern-soft (aref x 0)
					   energize-menu-item-table))
			     (aref x 4)))))
		  menubar))))

(defun activate-energize-menu-items-hook ()
  ;; This is O^2 because of the `rassq', but it looks like the elisp part
  ;; of it only takes .03 seconds.  
  (if (connected-to-energize-p)
      (let* ((items current-menubar)
	     (change-p nil)
	     ;; dynamically used by activate-energize-menu-item-internal
	     (active-items (energize-build-menubar-names))
	     item)
	(while items
	  (setq item (car items)
		change-p (or (and item (activate-energize-menu-items-internal
					(if (consp item) (cdr item) item)))
			     change-p)
		items (cdr items)))
	(not change-p))))

(add-hook 'activate-menubar-hook 'activate-energize-menu-items-hook t)

(defun deactivate-all-energize-menu-items ()
  (let ((items current-menubar)
	;; dynamically used by activate-energize-menu-item-internal
	(active-items nil)
	item)
    (while items
      (if (setq item (car items))
	  (activate-energize-menu-items-internal
	   (if (consp item) (cdr item) item)))
      (setq items (cdr items)))))


;;; The Options menu

(setq options-menu-saved-forms
      (purecopy
       (append
	options-menu-saved-forms
	'((list 'energize-set-edit-modes
		(if energize-external-editor
		    (symbol-name energize-external-editor))
		(list 'quote energize-vi-terminal-emulator)
		(list 'quote energize-internal-viewer)
		(list 'quote energize-internal-editor)
		(cond ((get 'browser 'instance-limit) ''multi)
		      ((get 'energize-top-level-mode 'screen-name)
		       ''several)
		      (t ''single))
		(list 'quote energize-split-screens-p)
		)
	  (if sc-mode
	      (list 'sc-mode (list 'quote sc-mode))
	    '(if (featurep 'generic-sc) (sc-mode nil)))
	  ))))


;;; Popup-menus

(defvar energize-popup-menu)

(defvar energize-hack-popup-hook '()
  "Hook for all functions that want to hack at the Energize popup menus.
Each function takes two arguments: an extent (or nil if none) and a menu
(or nil if none currently). It should return a menu (or nil)")

(defun energize-popup-menu (event)
  (interactive "e")
  (if (popup-menu-up-p)
      ()
    (let* ((buffer (window-buffer (event-window event)))
	   (extent (if (extentp (event-glyph event))
		       (event-glyph event)
		     (energize-menu-extent-at (event-point event) buffer)))
	   choices)
      (select-window (event-window event))
      (if extent
	  (progn
	    (energize-with-timeout
	     "Asking Energize server for menu contents..."
	     (setq choices
		   (cdr
		    (cdr
		     (energize-list-menu buffer extent
					 (x-selection-exists-p 'PRIMARY))))))))
      (if (or (null extent) (null choices))
	  (if (null (setq energize-popup-menu
			  (energize-extent-run-hook energize-hack-popup-hook
						    nil nil)))
	    (error "No menu to pop up"))
	(force-highlight-extent extent t)
	(sit-for 0)
	(setq energize-popup-menu
	      (cons "energizePopup"
		    (mapcar
		     (function (lambda (item)
				 (vector
				  (aref item 0)
				  (list 'energize-execute-command
					(aref item 0)
					extent)
				  (= 0 (logand 1 (aref item 3)))
				  (aref item 4))))
		     choices)))
	(setq energize-popup-menu
	      (external-editor-hack-popup
	       (energize-extent-run-hook energize-hack-popup-hook
					 extent energize-popup-menu))))
      (if (equal (car energize-popup-menu) "energizePopup")
	  (let ((popup-menu-titles nil))
	    (popup-menu 'energize-popup-menu))
	(popup-menu 'energize-popup-menu)))))

(defun energize-extent-run-hook (f ex m)
  (if f
      (energize-extent-run-hook (cdr f) ex (funcall (car f) ex m))
    m))

;;; Functions to interactively execute menu items by their names.

(defun energize-menu-extent-at (pos buffer)
  (if (null pos)
      nil
    (let ((extent (energize-extent-at pos buffer)))
      (if (and extent (energize-extent-menu-p extent))
	  extent
	nil))))

;;; functions to execute the menu with the keyboard
(defun default-selection-value-for-item (menu-item)
  (let ((flags (aref menu-item 3)))
    (cond ((= (logand flags 2) 2)
	   (if (x-selection-owner-p 'PRIMARY)
	       (x-get-selection-internal 'PRIMARY 'STRING)))
	  ((= (logand flags 4) 4)
	   (if (x-selection-owner-p 'PRIMARY)
	       (x-get-selection-internal 'PRIMARY 'ENERGIZE_OBJECT)))
	  ((= (logand flags 128) 128)
	   (if (x-selection-owner-p 'SECONDARY)
	       (x-get-selection-internal 'SECONDARY 'STRING)))
	  ((= (logand flags 256) 256)
	   (if (x-selection-owner-p 'SECONDARY)
	       (x-get-selection-internal 'SECONDARY 'ENERGIZE_OBJECT))))))
  
(defun energize-execute-menu-item-with-selection (buffer
						  extent
						  item
						  selection
						  no-confirm)
  (if (/= 0 (logand 1 (aref item 3)))
      (error "The `%s' command is inappropriate in this context"
	     (aref item 0)))
  (if (null selection)
      (setq selection (default-selection-value-for-item item)))
  (energize-execute-menu-item buffer extent item selection no-confirm))

(defun energize-find-item (name list)
  (let ((l list) i (found ()))
    (while (and l (not found))
      (setq i (car l) l (cdr l))
      (if (and (vectorp i) (equal (aref i 0) name))
	  (setq found i)))
    found))

(defun energize-menu-item-for-name (extent name)
  (if (or extent (< (cdr (energize-protocol-level)) 7))
      (energize-with-timeout
       "Checking Energize command with kernel..."
       (energize-list-menu (current-buffer) extent
			   (x-selection-exists-p 'PRIMARY) name))
    (or (energize-find-item name energize-menu-state)
	(energize-find-item name energize-default-menu-state))))

(defun energize-execute-command (name &optional extent selection no-confirm)
  ;; add completion here...
  (interactive "sExecute Energize command named: ")

  (if (not (stringp name))
      (error "Can't execute a choice, %s, that is not a string" name))

  (or (connected-to-energize-p) (error "Not connected to Energize"))

  ;; patch the selection argument for "setbreakpoint"
  (if (and (equal name "setbreakpoint")
	   (null selection))
      (setq selection
	    (save-excursion
	      (vector (energize-buffer-id (current-buffer))
		      (progn (beginning-of-line) (1- (point)))
		      (progn (end-of-line) (1- (point)))))))

  (let* ((buffer (current-buffer))
	 (extent (if extent
		     (if (extentp extent)
			 extent
		       (energize-menu-extent-at (point) buffer))
		   nil)))
    (if (< (cdr (energize-protocol-level)) 7)
	;; old way
	(let ((item (energize-menu-item-for-name extent name)))
	  (if (not item)
	      (error "No Energize command named %s" name))
	  (energize-execute-menu-item-with-selection buffer extent item
						     selection no-confirm))
      ;; new way
      (if (and (null selection)
	       (x-selection-exists-p 'PRIMARY))
	  (setq selection
		(condition-case
		    ()
		    (x-get-selection-internal 'PRIMARY 'STRING)
		  (error ""))))
      (let ((energize-make-many-buffers-visible-should-enqueue-event
	     (equal name "save")))
	(energize-execute-command-internal buffer
					   extent
					   name
					   selection
					   no-confirm)))))



;;; Buffer modified the first time hook
;;; Should be in energize-init.el but is here to benefit from the 
;;; add-timeout macro  

(defun energize-check-if-buffer-locked ()
  (if (connected-to-energize-p)
      (energize-with-timeout
       "Asking Energize server if buffer is editable..."
       (energize-barf-if-buffer-locked))))

(add-hook 'first-change-hook 'energize-check-if-buffer-locked)


;;; Here's a converter that makes emacs understand how to convert to
;;; selections of type ENERGIZE.  Eventually the Energize server won't
;;; be using the selection mechanism any more, I hope.

(defun xselect-convert-to-energize (selection type value)
  (let (str id start end tmp)
    (cond ((and (consp value)
		(markerp (car value))
		(markerp (cdr value)))
	   (setq id (energize-buffer-id (marker-buffer (car value)))
		 start (1- (marker-position (car value)))  ; zero based
		 end (1- (marker-position (cdr value)))))
	  ((extentp value)
	   (setq id (extent-to-generic-id value)
		 start 0
		 end 0)))
    (if (null id)
	nil
      (setq str (make-string 12 0))
      (if (< end start) (setq tmp start start end end tmp))
      (aset str 0 (logand (ash (car id) -8) 255))
      (aset str 1 (logand (car id) 255))
      (aset str 2 (logand (ash (cdr id) -8) 255))
      (aset str 3 (logand (cdr id) 255))
      (aset str 4 (logand (ash start -24) 255))
      (aset str 5 (logand (ash start -16) 255))
      (aset str 6 (logand (ash start -8) 255))
      (aset str 7 (logand start 255))
      (aset str 8 (logand (ash end -24) 255))
      (aset str 9 (logand (ash end -16) 255))
      (aset str 10 (logand (ash end -8) 255))
      (aset str 11 (logand end 255))
      (cons 'ENERGIZE_OBJECT str))))


(or (assq 'ENERGIZE_OBJECT selection-converter-alist)
    (setq selection-converter-alist
	  (cons '(ENERGIZE_OBJECT . xselect-convert-to-energize)
		selection-converter-alist)))


;;; Function keys.

(defun energize-define-function-keys ()
  "Define some Borland/Motif-like `F' keys for Energize."
  (define-key global-map 'f1 'help-for-help)
  (define-key global-map 'f3 'energize-search)
  (define-key global-map '(shift delete) 'x-kill-primary-selection)
  (define-key global-map '(control insert) 'x-copy-primary-selection)
  (define-key global-map '(shift insert) 'x-yank-clipboard-selection)
  (define-key global-map '(control delete) 'x-delete-primary-selection)

  (define-key global-map 'f7 'energize-browse-error)
  (define-key global-map '(meta f7) 'next-error)
  (define-key global-map '(meta f8) 'previous-error)

  (define-key global-map 'f9 'energize-build-a-target)
  (define-key global-map '(meta f9) 'energize-default-compile-file)
  (define-key global-map '(control f9) 'energize-run-target)
  (define-key global-map '(meta shift f9) 'energize-abort-build)

  (define-key global-map '(meta control ?.) 'energize-edit-declaration-dbox)
  (define-key global-map 'f5 'energize-browse-language-elt)
  (define-key global-map '(shift f5) 'energize-next-use-start)
  (define-key global-map '(control f5) 'energize-next-use-command)
  )

(defun energize-x-initializations ()
  (energize-define-function-keys)
  )

;; Do these bindings after connecting to the X server, but before 
;;; loading any init files, so that init files can override them.
(add-hook 'before-init-hook 'energize-x-initializations)
