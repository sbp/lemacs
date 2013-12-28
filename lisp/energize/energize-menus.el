;;; -*- Mode:Emacs-Lisp -*-

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

(require 'menubar)

(defconst energize-menu-item-table (make-vector 511 nil)
  "obarray used for fast mapping of symbolic energize request-names to the 
functions that invoke them.")

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
		 (execute-energize-choice (, name) t)))))
  ;; Return the menu-item descriptor.
  (vector name function nil))

(defmacro energize-def-menu (menu-name &rest items)
  (` (list (, menu-name)
	   (,@ (mapcar
		'(lambda (x)
		   (if (consp x)
		       (cons 'energize-def-menu-item
			     (mapcar '(lambda (xx)
					(if (stringp xx) xx (list 'quote xx)))
				     x))
		     x))
		items)))))

(put 'energize-def-menu 'lisp-indent-function 1)

(defun energize-kill-server ()
  "Kill the Energize server and all buffers associated with it."
  (interactive)
  (condition-case nil
      (execute-energize-choice "quit" t)
    (error nil)))

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
   ("findproject"	energize-find-project)
   ("project"		energize-pop-to-project-buffer)
   ("energize"		energize-pop-to-energize-buffer)
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
   ;; the automatically-generated -set-breakpoint function is redefined later.
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
   ("backtrace"		energize-debugger-backtrace)
   "-----"
   ("print"		energize-debugger-print)
   "-----"
   ("closeprogram"	energize-debugger-kill-program)
   ("quitdebugger"	energize-quit-debugger)
   )

 (energize-def-menu "Compile"
   ("buildanddebug"	energize-build-and-debug)
   ("incrementalbuild"	energize-build)
   ("buildprogram"	energize-full-build)
   ("compileprogram"	energize-compile-target)
   ("link"		energize-link-target)
   "-----"
   ("compilemods"	energize-compile-file)
   ("compilefile"	energize-full-compile-file)
   ("compilecheck"	energize-check-for-errors)
   "-----"
   ("deleteallobjects"	energize-delete-object-files)
   )

 (energize-def-menu "Project"
   ("newproject"	energize-new-project)
   "-----"
   ("import"		energize-import-file)
   ("addusedproject"	energize-add-used-project)
   ("changedirectory"	energize-set-project-directory)
   "-----"
   ("alphaorder"	energize-project-sort-alpha)
   ("linkorder"		energize-project-sort-link)
   ("nameinfo"		energize-project-view-names)
;;   ("lslinfo"		energize-project-view-long)
   ("compileinfo"	energize-project-view-options)
   )
 
 buffers
 
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

(defun energize-unix-manual ()
  "Display a manual entry; if connected to Energize, uses the Energize version.
Otherwise, just runs the normal emacs `manual-entry' command."
  (interactive)
  (if (connected-to-energize-p)
      (execute-energize-choice "manual" t)
    (call-interactively 'manual-entry)))

(defun energize-set-breakpoint ()
  "Executes the Energize \"setbreakpoint\" command, to set a breakpoint
at the current cursor position."
  (interactive)
  (execute-energize-choice "setbreakpoint" t 
			   (save-excursion
			     (vector (energize-buffer-id (current-buffer))
				     (progn (beginning-of-line) (1- (point)))
				     (progn (end-of-line) (1- (point)))))))


(defsubst activate-energize-menu-item-internal (item)
  (cond
   ((vectorp item)
    (let ((fn (aref item 1)))
      (if (not (and (symbolp fn) (get fn 'energize-name)))
	  nil
	;; Referencing special binding of `active-items' from a-c-m-i-hook.
	;; If the function which this item invokes is an Energize function
	;; (determined by the presence of an 'energize-name property) then
	;; make it be active iff it's on the active-items list.
	(let ((active-p (memq fn active-items)))
	  (if (eq (not active-p) (not (aref item 2)))
	      nil
	    (aset item 2 (not (not active-p)))
	    t)))))
   ((consp item)  ; descend nested submenus
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


(defun activate-energize-menu-items-hook ()
  ;; This is O^2 because of the `rassq', but it looks like the elisp part
  ;; of it only takes .03 seconds.  Calling `energize-list-menu' takes
  ;; almost .1 second, but that's still pretty negligible.
;;  (let ((i 100))
;;    (while (> i 0)
  (let* ((menubar (screen-menubar))
	 (items menubar)
	 (change-p nil)
	 (active-items
	  ;; convert a list of Energize names into a list of the functions
	  ;; which invoke them.
	  (delq nil
	    (mapcar '(lambda (x)
		       (if (/= 0 (logand 1 (aref x 3)))
			   nil
			 (symbol-value
			  (intern-soft (aref x 0) energize-menu-item-table))))
		    (cdr (cdr (energize-list-menu (current-buffer) nil))))))
	 item)
    (while items
      (setq item (car items)
	    change-p (or (and item (activate-energize-menu-items-internal
				    (if (consp item) (cdr item) item)))
			 change-p)
	    items (cdr items)))
    (setq change-p (or (energize-sensitize-sheet-button (selected-screen) t)
		       change-p))
    (if change-p
	(set-screen-menubar menubar)))
;;  (setq i (1- i))))
  )


(fset 'energize-announce 'play-sound)
(defun energize-sensitize-sheet-button (screen &optional no-update)
  (let* ((menubar (screen-menubar screen))
	 (sheet-button (car (find-menu-item menubar '("sheet"))))
	 (sheet-active-p (or (energize-psheets-visible-p)
			     (energize-buffer-has-psheets-p))))
    (if (and sheet-button
	     (not (eq (not (aref sheet-button 2)) (not sheet-active-p))))
	(progn
	  (aset sheet-button 2 (not (not sheet-active-p)))
	  (or no-update (set-screen-menubar menubar screen))
	  t)
      nil)))


(defvar energize-popup-menu)

(defun energize-popup-menu (event)
  (interactive "e")
  (let* ((buffer (window-buffer (event-window event)))
	 (extent (if (extentp (event-glyph event))
		     (event-glyph event)
		   (menu-extent-at (event-point event) buffer)))
	 choices)
    (select-window (event-window event))
    (if (null extent)
	(error "No Energize menu here"))
    (setq choices (cdr (cdr (energize-list-menu buffer extent))))
    (if energize-kernel-busy
      (error "Can't pop up a menu right now, Energize server is busy"))
    (if (null choices)
	(error "No Energize menu here"))
    (force-highlight-extent extent t)
    (sit-for 0)
    (setq energize-popup-menu
	  (cons "energizePopup"
		(mapcar
		 '(lambda (item)
		    (vector
		     (aref item 0)
		     (if (equal (aref item 0) "setbreakpoint")
			 ;; Evil!  Can be flushed when the choice protocol
			 ;; has a bit telling to report (point) as the argument
			 (list 'energize-execute-menu-item-with-selection
			       buffer extent item
			       (save-excursion
				 (vector (energize-buffer-id (current-buffer))
					 (progn (beginning-of-line) (1- (point)))
					 (progn (end-of-line) (1- (point)))))
			       nil)
		       (list 'energize-execute-menu-item-with-selection
			     buffer extent item nil nil))
		     (= 0 (logand 1 (aref item 3)))
		     ))
		 choices)))
    (popup-menu 'energize-popup-menu)
    ;;
    ;; Setting zmacs-region-stays is necessary here because executing a
    ;; command from a menu is really a two-command process: the first command
    ;; (bound to the button-click) simply pops up the menu, and returns.
    ;; This causes a sequence of magic-events (destined for the popup-menu
    ;; widget) to begin.  Eventually, a menu item is selected, and a menu-
    ;; event blip is pushed onto the end of the input stream, which is then
    ;; executed by the event loop.
    ;;
    ;; So there are two command-events, with a bunch of magic-events between
    ;; them.  We don't want the *first* command event to alter the state of
    ;; the region, so that the region can be available as an argument for the
    ;; second command.
    ;;
    (setq zmacs-region-stays t)))


(or (memq 'activate-energize-menu-items-hook menu-popup-hook)
    (setq menu-popup-hook (nconc menu-popup-hook
				 '(activate-energize-menu-items-hook))))


(defun install-energize-menu (screen)
  (set-screen-menubar energize-menubar screen))

(setq create-screen-hook 'install-energize-menu)


;;; Sparc function keys.  This isn't the most appropriate place for this...

(defun setup-sparc-function-keys ()
  (if (not (eq window-system 'x))
      nil
    (define-key global-map 'f20 'kill-primary-selection)   ; kp_cut
    (define-key global-map 'f16 'copy-primary-selection)   ; kp_copy
    (define-key global-map 'f18 'yank-clipboard-selection) ; kp_paste
    (define-key global-map 'f29 'scroll-down)		   ; kp_pgup
    (define-key global-map 'f35 'scroll-up)		   ; kp_pgdn
    (define-key global-map 'f27 'beginning-of-buffer)	   ; kp_home
    (define-key global-map 'r13 'end-of-buffer)		   ; kp_end
    ))

(or (memq 'setup-sparc-function-keys window-setup-hook)
    (setq window-setup-hook
	  (cons 'setup-sparc-function-keys window-setup-hook)))
(setup-sparc-function-keys)
