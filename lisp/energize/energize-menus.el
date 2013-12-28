;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright © 1992-1993 by Lucid, Inc.  All Rights Reserved.

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

(defconst energize-menu-item-table (make-vector 511 nil)
  "obarray used for fast mapping of symbolic energize request-names to the 
functions that invoke them.")

(defvar energize-menu-state ()
  "Buffer local variable listing the menu items associated with a buffer.")

(defvar energize-default-menu-state ()
  "List of the Energize menu items associated with every buffers.")

;;; Hook to update the menu state when the kernel tells us it changed

(defun energize-update-menu-state (items)
  (let ((buffer (car items))
	(previous-buffer (current-buffer))
	(extent (car (cdr items))))
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
  (put function 'energize-name name)
  (set (intern name energize-menu-item-table) function)
  ;; Define the (trivial) function
  ;; It's ok that this function is interpreted, because it contains only
  ;; one function call with constant args, so it's just as fast as it would
  ;; be if it were byte-coded.
  (if (not dont-define)
      (fset function
	    (` (lambda ()
		 (, (format "Executes the Energize \"%s\" command." name))
		 (interactive)
		 (energize-execute-command (, name))))))
  ;; Return the menu-item descriptor.
  (vector name function nil nil))

(defmacro energize-def-menu (menu-name &rest items)
  (` (list (, menu-name)
	   (,@ (mapcar
		'(lambda (x)
		   (if (and (consp x) (stringp (car x)))
		       (cons 'energize-def-menu-item
			     (mapcar '(lambda (xx)
					(if (stringp xx) xx (list 'quote xx)))
				     x))
		     x))
		items)))))

(put 'energize-def-menu 'lisp-indent-function 1)

(defconst energize-menubar
  (let ((file (or (assoc "File" default-menubar) (error "no File menu?")))
	(edit (or (assoc "Edit" default-menubar) (error "no Edit menu?")))
	(buffers (or (assoc "Buffers" default-menubar)
		     (error "no Buffers menu?"))))
    (list

 ;; The first thing in the menubar is the "sheet" button
 ["sheet" energize-toggle-psheet nil]

 ;; Add a "shutdown" menu item to the existing "File" menu.
 (append file
	 (list (energize-def-menu-item "quit" 'energize-kill-server t)))
     
 edit	; unchanged
     
 (energize-def-menu "Navigate"
   ["Next Error"	next-error			t]
   ["Previous Error"	previous-error			t]
   "-----"
   ["Visit Uses of Language Element"	energize-next-use-start 	t]
   ["Next Use"				energize-next-use-command	t]
   "-----"   
   ("findproject"	energize-find-project)
   ("project"		energize-pop-to-project-buffer)
   ("energize"		energize-pop-to-energize-buffer)
   ["Shell"		shell				t]
   )
 
 (energize-def-menu "Browse"
   ("editdef"		energize-edit-definition-dbox)
   ("editdec"		energize-edit-declaration-dbox)
   ("lebrowser"		energize-browse-language-elt)
   ("calltreebrowser"	energize-browse-tree)
   ("classbrowser"	energize-browse-class)
   "-----"
   ("errorbrowser"	energize-browse-error)
   ("clearerrorlog"	energize-clear-error-log)
   "-----"
   ("includers"		energize-where-included)
   "-----"
   ("toolstatus"	energize-browse-toolstat)
   ("showsystemlog"	energize-browse-system-log)
   )

 (energize-def-menu "Debug"
   ("debugprogram"	energize-debug-target)
   ("debuggerpanel"	energize-show-debugger-panel)
   "-----"
   ("breaklist"		energize-list-breakpoints)
   ("setbreakpoint"	energize-set-breakpoint)
   "-----"
   ("runprogram"	energize-run-target)
   ("openprogram"	energize-debugger-start-main)
   ("stopprogram"	energize-debugger-stop)
   ("continueprogram"	energize-debugger-continue)
   ("steponce"		energize-debugger-step)
   ("stepnext"		energize-debugger-next)
   ("continueuntilreturn"  energize-debugger-finish)
   "-----"
   ("innerframe"	energize-debugger-current-frame)
   ("upframe"		energize-debugger-up-frame)
   ("downframe"		energize-debugger-down-frame)
   ("listbacktrace"	energize-debugger-backtrace)
   "-----"
   ("printvalue"	energize-debugger-print)
   ("printstarvalue"	energize-debugger-print-star)
   "-----"
   ("cleardebuggerlog"	energize-clear-debugger-log)
   ("closeprogram"	energize-debugger-kill-program)
   ("quitdebugger"	energize-quit-debugger)
   )

 (energize-def-menu "Compile"
   ("buildatarget"	energize-build-a-target)
   ("custombuildatarget"	energize-custom-build-a-target)
   ("link"		energize-link-target)
   "-----"
   ("defaultcompile"	energize-default-compile-file)
   ("custombuildfile"	energize-custom-build-file)
   "-----"
   ("deleteallobjects"	energize-delete-object-files)
   )

 ;; this item cannot use the energize-def-menu macro because some of the
 ;; entries are not energize commands but lisp functions defined
 ;; in energize-mode.el
 (list "Project"
   (energize-def-menu-item "newproject"		'energize-new-project)
   (energize-def-menu-item "importproject"	'energize-import-project)
   "-----"
   (energize-def-menu-item "importprojectlist"	'energize-import-project-list)
   (energize-def-menu-item "writeprojectlist"	'energize-write-project-list)
   "-----"
   '("addprojectentry"
     ["addrule"			energize-insert-rule nil]
     ["addfiletarget"		energize-insert-file-target nil]
     ["addexecutabletarget"	energize-insert-executable-target nil]
     ["addlibrarytarget"	energize-insert-library-target nil]
     ["addcollectiontarget"	energize-insert-collection-target nil]
     ["addtargettarget"		energize-insert-target-target nil])
   (energize-def-menu-item "abbreviatetargets"	'energize-abbreviate-targets)
   (energize-def-menu-item "fulltargets"	'energize-full-targets)
   "-----"
   (energize-def-menu-item "showallfiles"	'energize-show-all-files)
   (energize-def-menu-item "onlyshowsources"	'energize-show-only-sources)
   (energize-def-menu-item "shownofiles"	'energize-show-no-files)
   "-----"
   (energize-def-menu-item "revertproject"
			   		'energize-fully-revert-project-buffer)
   )

 buffers

 nil
 
 '("Help"	["Info"			info			t]
		["Describe Mode"	describe-mode		t]
		["Command Apropos"	command-apropos		t]
		["List Keybindings"	describe-bindings	t]
		["Describe Key"		describe-key		t]
		["Describe Function"	describe-function	t]
		["Describe Variable"	describe-variable	t]
		"-----"
		;; the "manual" entry is the only difference between this
		;; and the default Help menu.
		["manual"		energize-unix-manual	t]
		["Emacs Tutorial"	help-with-tutorial	t]
		["Emacs News"		view-emacs-news		t]
		)
 ))
  "The emacs menubar used when Energize is installed.")

(set-menubar energize-menubar)

(defun energize-kill-server ()
  "Kill the Energize server and all buffers associated with it."
  (interactive)
  (condition-case nil
      (energize-execute-command "quit")
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
		   (not (equal (cdr active-p) (aref item 3))))
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



;;; Sensitization of the project menus
(defun sensitize-project-menu-hook ()
  "For use as a value of activate-menubar-hook.
This function activates the \"add target\" menu items in project buffers"
  (let* ((project-p (eq major-mode 'energize-project-mode))
	 (project-menu
	  (cdr (car (find-menu-item current-menubar '("Project")))))
	 (target-menu
	  (cdr (car (find-menu-item project-menu '("addprojectentry")))))
	 (add-rule (car (find-menu-item target-menu '("addrule"))))
	 (add-file (car (find-menu-item target-menu '("addfiletarget"))))
	 (add-executable
	  (car (find-menu-item target-menu '("addexecutabletarget"))))
	 (add-library (car (find-menu-item target-menu '("addlibrarytarget"))))
	 (add-collection
	  (car (find-menu-item target-menu '("addcollectiontarget"))))
	 (add-target
	  (car (find-menu-item target-menu '("addtargettarget"))))
	 (change-p
	  (or (and add-rule (not (eq project-p (aref add-rule 2))))
	      (and add-file (not (eq project-p (aref add-file 2))))
	      (and add-executable (not (eq project-p (aref add-executable 2))))
	      (and add-library (not (eq project-p (aref add-library 2))))
	      (and add-collection (not (eq project-p (aref add-collection 2))))
	      (and add-target (not (eq project-p (aref add-target 2)))))))
    (if add-rule (aset add-rule 2 project-p))
    (if add-file (aset add-file 2 project-p))
    (if add-executable (aset add-executable 2 project-p))
    (if add-library (aset add-library 2 project-p))
    (if add-collection (aset add-collection 2 project-p))
    (if add-target (aset add-target 2 project-p))
    ;; return t to mean "no change" and nil to mean "recompute menubar"
    (not change-p)))

(add-hook 'activate-menubar-hook 'sensitize-project-menu-hook t)

 
;;; Popup-menus

(defvar energize-popup-menu)

(defun energize-popup-menu (event)
  (interactive "e")
  (let* ((buffer (window-buffer (event-window event)))
	 (extent (if (extentp (event-glyph event))
		     (event-glyph event)
		   (energize-menu-extent-at (event-point event) buffer)))
	 choices)
    (select-window (event-window event))
    (if (null extent)
	(error "No extent with an Energize menu here"))
    (energize-with-timeout
     "Asking Energize server for menu contents..."
     (setq choices
	   (cdr
	    (cdr
	     (energize-list-menu buffer extent
				 (x-selection-exists-p 'PRIMARY))))))
    (if (null choices)
	(error "No Energize menu here"))
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
    (setq energize-popup-menu (external-editor-hack-popup energize-popup-menu))
    (popup-menu 'energize-popup-menu)))


;;; Functions to interactively execute menu items by their names.

(defun energize-menu-extent-at (pos buffer)
  (if (null pos)
      nil
    (let ((extent (extent-at pos buffer 'menu)))
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



;;; Sparc function keys.  This isn't the most appropriate place for this...

(defun setup-sparc-function-keys ()
  (if (not (eq window-system 'x))
      nil
    (define-key global-map 'f14 'undo)			     ; L4,  Undo
    (define-key global-map 'f16 'x-copy-primary-selection)   ; L6,  Copy
    (define-key global-map 'f18 'x-yank-clipboard-selection) ; L8,  Paste
    (define-key global-map 'f20 'x-kill-primary-selection)   ; L10, Cut
    (define-key global-map 'f27 'beginning-of-buffer)	     ; R7,  Home, KP_7
    (define-key global-map 'r13 'end-of-buffer)		     ; R13, End,  KP_1
    (define-key global-map 'f29 'scroll-down)		     ; R9,  PgUp, KP_9
    (define-key global-map 'f35 'scroll-up)		     ; R15, PgDn, KP_3
    ))

(add-hook 'window-setup-hook 'setup-sparc-function-keys)
(setup-sparc-function-keys)
(fset 'energize-announce 'play-sound)

;;; Buffer modified the first time hook
;;; Should be in energize-init.el but is here to benefit from the 
;;; add-timeout macro  

(defun energize-check-if-buffer-locked ()
  (if (connected-to-energize-p)
      (energize-with-timeout
       "Asking Energize server if buffer is editable..."
       (energize-barf-if-buffer-locked))))

(setq first-change-function 'energize-check-if-buffer-locked)


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
