;;; sunpro-menubar.el --- Initialize the SunPro menubar

;; Copyright (C) 1993, 1994 Sun Microsystems, Inc

;; Author:	Aaron Endelman <endelman@Eng.Sun.COM>
;; Maintainer:	Vladimir Ivanovic <vladimir@Eng.Sun.COM>
;; Created:	93/09/13 15:16:24
;; Version:	1.21
;; Adapted-By:
;; Header:	@(#) sunpro-menubar.el: v1.21 94/04/19 01:43:26

;; Keywords:	SunPro menubar initialization

;;; Commentary:
;;  Creates the default SunPro menubar.

;;; Change log:
;;	94-05-18 vladimir
;;	Fiddle with the menu definitions.
;;      94-04-13 ben wing
;;	Moved unused code into attic/sunpro-misc.el.
;;	94-03-30 vladimir
;; 	(1) Changed the menubar around to agree with the recommendations of Rob
;;	Mori and Robin Jeffries (rmori@Eng.Sun.COM, jeffries@Eng.Sun.COM).  
;;	(2) Started to take out support for evi-mode.

;;; To Do:

;;; Code:

(defconst sunpro-menubar
  (list
   '("File"
     ["New:"			sunpro-new-buffer	t]
     ["Clone:"		make-screen		t]
     ["Open in New:"	find-file-other-screen	t]
     ["Open:"			find-file		t]
     ["Include File:"		insert-file		t]
     "-----"
     ["Save"			save-buffer		t nil]
     ["Save As:"		write-file		t]
     ["Revert Buffer:"		revert-buffer		t nil]
     ["Delete Buffer"		(kill-buffer nil)	t nil]
     "-----"
     ["Print Buffer"		lpr-buffer		t nil]
     "-----"
     ["Close"			delete-screen		t]
     ["Exit Emacs"		save-buffers-kill-emacs	t]
     )
   
   '("View"
     ["Split Window"		(split-window)		t]
     ["Unsplit Window"		delete-other-windows	t]
     )
     
   '("Edit"
     ["Undo"			advertised-undo		t]
     "-----"
     ["Cut"			x-kill-primary-selection   t]
     ["Copy"			x-copy-primary-selection   t]
     ["Paste"			x-yank-clipboard-selection t]
     ["Delete"			x-delete-primary-selection t]
     "-----"
     ["Select Block"		mark-paragraph 		t]
     ["Select All"		mark-whole-buffer	t]
     "-----"
     ["Cancel Command"		(sunpro-menu-quit)	t]
     )
   
   '("Find"
     ["Forward:"		sunpro-search-forward	t]
     ["Backward:"		sunpro-search-backward	t]
     ["And Replace:"		sunpro-query-replace	t]
     )

   ;; copy the options menu from the default menubar
   (car (find-menu-item default-menubar '("Options")))
   
   '("Buffers"
     "")
   
   '("Utilities"
     ["Execute Macro"		call-last-kbd-macro last-kbd-macro]
     ["Start Macro Recording"	start-kbd-macro     (not defining-kbd-macro)]
     ["End Macro Recording"	end-kbd-macro	    defining-kbd-macro]
     )
   
   ;; the following is supposed to be here!  It ensures that the
   ;; Help item is always the rightmost item.
   nil

   '("Help"
     ["About Xemacs"		about-lucid-emacs	t]
     "-----"
     ["Editor Documentation"	info			t]
     "-----"
     ["Describe Mode"		describe-mode		t]
     ["Command Apropos:"	command-apropos		t]
     ["Full Apropos:"		apropos			t]
     ["List Keybindings"	describe-bindings	t]
     "-----"
     ["Describe Key:"		describe-key		t]
     ["Describe Function:"	describe-function	t]
     ["Describe Variable:"	describe-variable	t]
     "-----"
     ["Unix Manual:"		manual-entry		t]
     ["Editor Tutorial"		help-with-tutorial	t]
     ["Editor News"		view-emacs-news		t]
     )))

(set-menubar sunpro-menubar)

;;;
;;; helper commands
;;;

(defun sunpro-new-buffer ()
  (interactive)
  (switch-to-buffer-other-screen (generate-new-buffer "Untitled")))

(defun sunpro-search-forward ()
  (interactive)
  (if (and (boundp 'evi-enabled) evi-enabled)
      (evi-execute-macro "/")
    (if isearch-mode (isearch-repeat-forward)
      (x-isearch-maybe-with-region))))

(defun sunpro-search-backward ()
  (interactive)
  (if (and (boundp 'evi-enabled) evi-enabled)
      (evi-execute-macro "?")
    (if isearch-mode (isearch-repeat-backward)
      (x-isearch-maybe-with-region t))))

(put 'sunpro-search-forward 'isearch-command t)
(put 'sunpro-search-backward 'isearch-command t)

(defun sunpro-query-replace ()
  (interactive)
  (if (and (boundp 'evi-enabled) evi-enabled)
      (evi-execute-macro ":s")
    (call-interactively 'query-replace)))

(defun sunpro-menu-quit ()
  "Abort minibuffer input if any."
  (while (not (zerop (minibuffer-depth)))
    (abort-recursive-edit)))

;;; sunpro-menubar.el ends here
