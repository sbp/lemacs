;; Light Weight Editor Integration for Sparcworks.
;; "Era on Sparcworks" (EOS)
;;
;; Author: Eduardo Pelegri-Llopart
;;
;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

;; This file contains functions that populate a SPARCworks menu
;;

(defun eos::toggle-sbrowser-selected-screen ()
  "Toggle whether this screen is selected for SBrowser"
  (interactive)
  (if (equal eos::sbrowser-screen (selected-screen))
      (eos::select-sbrowser-screen nil)
    (eos::select-sbrowser-screen (selected-screen))))

(defun eos::toggle-debugger-selected-screen ()
  "Toggle whether this screen is selected for Debugger"
  (interactive)
  (if (equal eos::debugger-screen (selected-screen))
      (eos::select-debugger-screen nil)
    (eos::select-debugger-screen (selected-screen))))

(defvar eos::long-menu
  '(
    ["News..." eos::sw-news t]
    "-----"
    ["Do Dbx Command" eos::dbx-cmd (not (eq eos::key-mode 'none))]
    ["Run" eos::run (not (eq eos::key-mode 'none))]
    ["Fix" eos::fix (not (eq eos::key-mode 'none))]
    "-----"
    ["Print" eos::print (not (eq eos::key-mode 'none))]
    ["Print *" eos::print* (not (eq eos::key-mode 'none))]
    ["Dismiss Print" eos::dismiss-print-screen (not (eq eos::key-mode 'none))]
    "-----"
    ["Continue" eos::cont (not (eq eos::key-mode 'none))]
    ["Stop" eos::stop-at (not (eq eos::key-mode 'none))]
    ["Clear" eos::clear-at (not (eq eos::key-mode 'none))]
    ["Next" eos::next (not (eq eos::key-mode 'none))]
    ["Step" eos::step (not (eq eos::key-mode 'none))]
    ["Step Up" eos::step-up (not (eq eos::key-mode 'none))]
    ["Continue To" eos::cont-to (not (eq eos::key-mode 'none))]
    "-----"
    ["Stack Up" eos::up (not (eq eos::key-mode 'none))]
    ["Stack Down" eos::down (not (eq eos::key-mode 'none))]
    "-----"
    ["Select for SBrowser" eos::toggle-sbrowser-selected-screen
     :style toggle
     :selected (equal eos::sbrowser-screen
		      (selected-screen))]
    ["Select for Debugger" eos::toggle-debugger-selected-screen
     :style toggle
     :selected (equal eos::debugger-screen
		      (selected-screen))]
    )
  )

(defvar eos::short-menu
  '(
    ["News..." eos::sw-news t]
    "-----"
    ["Select for SBrowser" eos::toggle-sbrowser-selected-screen
     :style toggle
     :selected (equal eos::sbrowser-screen
		      (selected-screen))]
    ["Select for Debugger" eos::toggle-debugger-selected-screen
     :style toggle
     :selected (equal eos::debugger-screen
		      (selected-screen))]
    )
  )

(defun eos::menubar-startup ()
  "Actions to do at startup for eos-menubar.el"
  (setq eos::sw-face
	(make-face 'eos::sw-face))
  (set-face-foreground eos::sw-face "red")
  (set-face-background eos::sw-face
		     (face-background (get-face 'default)))

  (setq eos::choose-me
	(make-pixmap "bullet.xbm"))

  (add-menu '("Options") "SPARCworks"
	    (copy-tree '(
			 ["Use only selection service" (eos::set-key-mode 'none)
			  :style radio
			  :selected (eq eos::key-mode 'none)]
			 ["Use C-c d prefix map" (eos::set-key-mode 'prefix)
			  :style radio
			  :selected (eq eos::key-mode 'prefix)]
			 ["Use function keys" (eos::set-key-mode 'function)
			  :style radio
			  :selected (eq eos::key-mode 'function)]
			 "---"
			 ["Use popup menu" eos::toggle-popup-menu
			  :style toggle
			  :selected eos::popup-mode]
			 ))
	    "Save Options"
	    )
  (add-menu-item '("Options") "--" nil t "Save Options")

  (add-menu-item '("Help") "--" nil t)
  (add-menu '("Help") "SPARCworks"
	    (copy-tree '(
			 ["Introduction..." eos::sw-intro t]
			 ["Installation for SW3.0..." eos::sw-install t]
			 ["Keyboard Mappings..." eos::sw-cheat-sheet t]
			 )))
	    
  (add-menu nil "SPARCworks"
	    (copy-tree eos::short-menu)
	    "SCCS"
	    )
)

;;
;; Insertion of text with a font
;;

(defun eos::insert-italics (a-string)
  (eos::insert-with-font a-string 'italic))

(defun eos::insert-red (a-string)
  (eos::insert-with-font a-string 'eos::sw-face))

(defun eos::insert-bold (a-string)
  (eos::insert-with-font a-string 'bold))

(defun eos::insert-with-font (a-string a-font)
  (interactive "")
  (let (a b ext)
    (setq a (point))
    (insert a-string)
    (setq b (point))
    (setq ext (make-extent a b))
    (set-extent-face ext (find-face a-font))
    ))

;;
;; Generic insert code
;;

(defun eos::insert (s)
  (let ((len (length s))
	(pos 0)
	(newpos 0)
	(state 'normal))
    (while (< pos len)
      (setq newpos (string-match "#[bnir]" s pos))
      (if (and newpos (> newpos pos))
	  (progn
	    (cond ((equal (aref s (+ newpos 1)) ?b) ; bold
		   (if (equal state 'normal)
		       (progn
			 (insert (substring s pos newpos))
			 (setq state 'bold))
		     (error "found bold when not in normal")))
		  ((equal (aref s (+ newpos 1)) ?r) ; red
		   (if (equal state 'normal)
		       (progn
			 (insert (substring s pos newpos))
			 (setq state 'red))
		     (error "found red when not in normal")))
		  ((equal (aref s (+ newpos 1)) ?i) ; italics
		   (if (equal state 'normal)
		       (progn
			 (insert (substring s pos newpos))
			 (setq state 'italics))
		     (error "found italics when not in normal")))
		  ((equal (aref s (+ newpos 1)) ?n) ; normal
		   (cond ((equal state 'italics)
			  (eos::insert-italics (substring s pos newpos))
			  (setq state 'normal))
			 ((equal state 'red)
			  (eos::insert-red (substring s pos newpos))
			  (setq state 'normal))
			 ((equal state 'bold)
			  (eos::insert-bold (substring s pos newpos))
			  (setq state 'normal))
			 ((equal state 'normal)
			  (error "found normal when in normal"))))
		  (t
		   (error "internal error"))
		  )
	    (setq pos (+ newpos 2))
	    )
	(if (equal state 'normal)
	    (progn
	      (insert (substring s pos))
	      (setq pos len))
	  (error "eos::insert with unclosed special font"))
	))
    ))

;;
;; Code for active regions
;;


(defun eos::insert-hot-area (pos proc)
  "Insert a graphical annotation at POS with PROC as its action item"
;;  (insert " ")				; bug in 0.86 & 0.87
  (let* ((ann (make-annotation eos::choose-me pos 'outside-margin))
	)
    (set-annotation-action ann proc)
    (set-annotation-data ann pos)
    (set-extent-face ann 'eos::sw-face)
    ann
    ))

;;
;; Introduction File
;;

(defun eos::sw-intro ()
  "Generate an intro buffer."
  (interactive)
  (let ((buffer1 (get-buffer-create " *SPARCworks Intro*"))
	)
    (switch-to-buffer buffer1)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (eos::insert "
		#bSPARCworks Editor Integration#n

#bIntroduction#n

#iSPARCworks#n is a set of integrated programming tools from SunPro that
support the program development cycle. The tools are available in
different product packages and include \"programming-in-the-small\" tools
as well as groupware. SPARCworks tools communicate with one another using
#iToolTalk#n.

#iXemacs#n is a version of the Emacs editor that includes interfaces
to the selection service and to the ToolTalk service.  The #iEos#n
package uses these two interfaces to provide a simple yet useful
editor integration with two SPARCworks tools: the #iSourceBrowser#n
and the #iDebugger#n.

To use this integration, the user starts the SourceBrowser and the
Debugger requesting that they do not contain a source pane, and then
selects any Xemacs screen (that is, a top-level X11 window) as the
source display for the SourceBrowser and the Debugger.  The user can
then interact with the Xemacs screen in a way very similar to how the
source panes of the SW tools were used.  The user can also drive the
debugger directly from Xemacs; in this mode a simple but fast visual
data inspector is also available.

#bRelevant Menus#n

There are several menus and sub-menus that describe #iEos#n.  The main
interaction is done using the top-level #iSPARCworks#n menu.  This
menu initially contains 3 menu items, one to present the latest news
of #iEos#n and two to control how a given Xemacs screen interacts with
the Debugger and the SourceBrowser.

Information on #iEos#n is available in the #iSPARCworks#n submenu of
the top-level #iHelp#n menu; this submenu includes this introductory
message, some instructions for using SPARCworks3.0, and a cheat sheet
for the keyboard accelerators (if you are using them).

The behavior of #iEos#n is customized using the #iSPARCworks#n submenu
of the top-level #iOptions#n menu.

#bInstructions#n

To use the editor integration, first start Xemacs.

If you are using #iEos#n with the Debugger of SPARCworks 3.0, you will
need to perform some customization.  The #iSPARCworks#n submenu of
#iHelp#n contains instructions on how to do this.

After selecting any desired options (see below), use the
#iSPARCworks#n top-level menu to select at most one Xemacs screen to
be used to display SourceBrowser or Debugger sources. Both menu items
toggle to deselect a screen for the tool.  If no screens are selected
for a given tool, the editor integration for that tool is disabled.

After doing any customization needed for SW3.0, start the desired
SPARCworks tool. #iEos#n uses glyphs in a manner similar to how they are
used in the source panes of the Source Browser and the Debugger. The
default mode of interaction is to click on command buttons in the
SourceBrowser and Debugger to invoke operations that operate on the
selection or its position.

#iSourceBrowser#n

Start the SourceBrowser with the #i-editor#n flag. All SourceBrowser
commands will work as expected, except that only the current match of
the current query will be given a glyph.  #iNext#n and #iprev#n will
change the current match and the selected Xemacs screen will display it.
Requesting #iSource#n in the Call Grapher, the Class Grapher and the
Class Browser will show the source in the selected Xemacs screen.

#iDebugger#n

In SW3.0, there is no #i-editor#n flag.  You should start the debugger
as usual.  In later releases, start the debugger with the #i-editor#n
flag.  All commands that operate on the contents of the selection will
work as expected.  Operations like #iup#n and #idown#n (done either in
the command pane or in the Stack Inspector) will work as expected.

The Debugger commands: #iStop At#n and #iClear#n operate on the
position of the selection. In SPARCworks 3.0, these commands do not
work when the selection is owned by Xemacs; therefore, an extension
file has been provided for the Debugger that contains two replacements
that work as expected. If you follow the instructions in #iInstall SW3.0#n
from the SPARCworks menu, the two original buttons will be replaced with
new buttons, which use the new commands.

#iDriving the Debugger from Xemacs#n

Some users may want to drive the Debugger from Xemacs, and #iEos#n
supports this.  The #i*debugger*#n buffer provides a place where
Debugger commands can be typed.  This mechanism is always available.
In addition, #iEos#n provides two different interfaces providing
keyboard accelerators for the most common debugger actions.  The two
interfaces can be activated and selected using the #iOptions#n menu;
one provides a new prefix map, C-c d, while the other uses function
keys.  In both cases there is a key sequence to type a single debugger
command.

#iEos#n also provides a popup accelerator for some common debugger
commands that act on the selection (either content or its position).  This
permits following a selection by an operation on the selection.  This feature
can be activated using the #iOptions#n menu.

#iA Visual Data Inspector in Xemacs#n

Users that choose to drive the debugger from Xemacs also have
available a very simple but fast visual data inspector.  The results
of #iprint#n and #iprint*#n commands (invoked either from a menu item
under the SPARCworks menu or the popup, or from a key accelerator) are
formatted into an Xemacs buffer and presented into a separate screen.
This screen is mapped and unmapped so that, except for the first time,
it appears quickly.

#bRestrictions#n

The current version of #iEos#n can only interact with a single instance
of each of SourceBrowser and Debugger.

#iEos#n cannot send commands to the Debugger until after the Debugger
has loaded a program; furthermore, the loading has to be done as an
explicit command in the Debugger, not passed as an argument in the
command line.

#bDisclaimers#n

The preceeding user model is not necessarily indicative of any future
directions for SPARCworks; however, you are encouraged to send us
feedback via the Comments button in the About Box of either SPARCworks
tool, or directly to eos-comments@cs.uiuc.edu.

The lightweight editor integration is not a supported feature in
SW3.0; the TT messages are supported, though, and that is the key to
making such an integration possible.

#bEnjoy.#n")
   (setq buffer-read-only t)
   (goto-char (point-min))
   ))

;;
;; Install file
;;

(defun eos::sw-install ()
  "Generate an install buffer. Will have hot areas"
  (interactive)
  (let ((buffer1 (get-buffer-create " *SPARCworks Install*"))
	(one nil)
	(two nil)
	(three nil)
	(four nil)
	(five nil)
	(six nil)
	)
    (switch-to-buffer buffer1)
    (setq buffer-read-only nil)
    (set-buffer-left-margin-width 3)
    (delete-region (point-min) (point-max))
    (eos::insert "
		#bInstallation for SPARCworks3.0#n
		     #rNOT FOR LATER RELEASES#n

Note: If you are using a release of SPARCworks that is later than 3.0
you #bneed not and should not apply #n the installations described here.

SPARCworks 3.0 requires some customization to provide better editor
integration; the steps are described below. This buffer contains some
#iannotations#n on the left margin, shown as bullets. A mouse click on
an annotation will activate the step described to its right

The SPARCworks 3.0 debugger requires the loading of a simple
customization file using the import command; we recommed you do this
automatically in your .dbxrc.  The imports command defines some new
commands; to make these more accesible we advise defining some new
buttons to access these commands; to avoid confusion we also recommend
removing the old buttons.

It is also possible to request the debugger to start without a source
pane.  This is done by modifying the .debugger-init file to include the
appropriate resource specification.

The source browser requires no customization, just start it using
the -editor flag

You can now click on the annotations shown below to follow these
recomendations

")

   (setq one (point))
   (insert "\
Insert the suggested new commands in ~/.dbxrc.
You may want to edit the file after.

")

   (setq two (point))
   (insert "\
Save ~/.dbxrc.\n\n")

   (setq three (point))
   (insert "\
Insert the suggested new resource specification in ~/.debugger-init.\n\
You may want to edit this file after.\n\n")

   (setq four (point))
   (insert "\
Save ~/.debugger-init.\n\n")

   (setq five (point))
   (insert "\
Insert commands into ~/.emacs so that the initial screen will disable\n\
the presentation of both source browser and debugger (on by default).\n\
You may want to edit this file after.\n\n")

   (setq six (point))
   (insert "\
Save ~/.emacs.\n\n")

   (eos::insert-hot-area one 'eos::edit-dbxrc)
   (eos::insert-hot-area two 'eos::save-dbxrc)
   (eos::insert-hot-area three 'eos::edit-debugger-init)
   (eos::insert-hot-area four 'eos::save-debugger-init)
   (eos::insert-hot-area five 'eos::edit-emacs)
   (eos::insert-hot-area six 'eos::save-emacs)

   (setq buffer-read-only t)
   (goto-char (point-min))
   ))

;;
;; Cheat Sheets for keyboard mappings
;;
;; This depends on the mapping being used!
;;

(defun eos::sw-cheat-sheet ()
  "Generate buffer that has a description of the key maps that can be
printed, cut and then taped somewhere (like on the keyboard or on your
monitor).  This is particularly useful for the function keys"
  (interactive)
  (let ((buffer1 (get-buffer-create " *Cheat Sheets*"))
	)
    (switch-to-buffer buffer1)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (eos::insert "
		#bCheat Sheets for Eos#n

This buffer has a description of the key maps that can be printed, cut
and then taped somewhere (like on the keyboard or on your monitor).
This is particularly useful for the function keys since their numbers
don't any particular mnemonic value.


#bWhen using function keys#n #i[Options->SPARCworks->Use Function Keys]#n

----------------------------------------

F6      F7        F8             F9

Do      Print     Cont    ----   Next
Run     Print*    Stop   <Ctrl>  Step
Fix     Dismiss   Clear  <Shft>  Step Up


----------------------------------------

#bWhen using prefix map#n #i[Options->SPARCworks->Use C-c d Prefix Map]#n

----------------------------------------
Basic prefix: C-c d


	Do	 %
	Run	 r
	Fix	 f

	Print	 p
	Print*	 C-p

	Cont	 c
	Stop	 b (for breakpoint)
	Clear	 C-b

	Next	 n
	Step	 s
	Step up  C-s

	Up       u
	Down     d
----------------------------------------

")
   (setq buffer-read-only t)
   (goto-char (point-min))
   ))

;;
;; News files
;;

(defun eos::sw-news ()
  "Generate a News buffer."
  (interactive)
  (let ((buffer1 (get-buffer-create " *Eos News*"))
	)
    (switch-to-buffer buffer1)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (eos::insert "
		#bEos News#n

This is the #iNews#n document for the SPARCworks lightweight editor
integration package, #bEos#n. This buffer has short descriptions of
the main changes at each new version. The end of the buffer lists some
possible future enhancements.

See the #iHelp#n top-level menu for additional information on the
SPARCworks lightweight editor integration (Eos), including actions
needed to use SPARCworks3.0 (later releases of SPARCworks need no
special configuration actions).  Option configuration is available
through the #iOptions#n top level menu.

The current version of Eos is available as the contents of the
variable eos::version.  


#bversion 1.3#n

Provided popup-menu bindings for those debugger actions that operate
on the contents of the selection or its position; selectable via options.

The *debugger* buffer now support M-p and M-n.


#bversion 1.2#n

Better support for interactions via *debugger* buffer and directly
using a prefix map and function keys.

Converted to use new toggle and radio menus, reorganizing SPARCworks
menu to factor out help and options into submenus, which are now
available under the Options and Help top-level menus.


#bversion 1.1#n

Some internal cleanup.

Eos now provides basic machinery to drive the debugger engine directly
using ToolTalk messages.  This feature is not yet very well
polished. You can try using it at your own risk, or await for release
1.2 (soon to come) that will provide a better interface and improved
functionality, as well as documentation for the interface.


#bversion 1.0#n

First widely available release.  Supports simple #iselect and click#n model.


#bPossible Future Enhancements#n

The comint package should be generalized to allow for TT-based interpreters
and it should be used in Eos.

Eos will use the tool bar as soon as it is available.

Key & popup bindings should probably be a minor mode.

Should support locking a print screen to force new print screens.

The current window management makes very cumbersome using *debugger* in
the same screen that is selected for displaying the debugger source; this will be
fixed.


#bFeedback#n

Send feedback to #ieos-comments@cs.uiuc.edu#n")
   (setq buffer-read-only t)
   (goto-char (point-min))
   ))
;;
;; Aux functions for install
;;

(setq eos::dbxrc-buffer nil)
(setq eos::debugger-init-buffer nil)
(setq eos::emacs-buffer nil)

(defun eos::recenter-install (where)
  (switch-to-buffer " *SPARCworks Install*")
  (goto-char where)
;; (recenter 1) shows a bug
  (recenter 2)
  )

(defun eos::edit-dbxrc (data)
  "will edit home .dbxrc"
  (eos::recenter-install data)
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  (find-file (expand-file-name "~/.dbxrc"))
  (goto-char (point-max))
  (setq eos::dbxrc-buffer (current-buffer))
  (insert "\
function sync\n\
{\n\
        visit_file $vfile $vlineno\n\
}	\n\
\n")
  (insert "import " eos::base-directory "/visit_file.so\n")
  (insert "unbutton \"stop at\"\n\
unbutton \"clear\"\n\
unbutton \"where\"\n\
unbutton \"display\"\n\
unbutton \"display *\"\n\
unbutton \"stop in\"\n\
unbutton \"up\"\n\
unbutton \"down\"\n\
button expand \"stop in\"\n\
button ignore \"stop_at\"\n\
button ignore \"clear_at\"\n\
button ignore \"sync\"\n\
")
  )

(defun eos::save-dbxrc (data)
  "will save home .dbxrc"
  (eos::recenter-install data)
  (if (not (bufferp eos::dbxrc-buffer))
      (message "~/.dbxrc buffer not there anymore"))
  (switch-to-buffer eos::dbxrc-buffer)
  (save-buffer)
  (kill-buffer eos::dbxrc-buffer)
  )

(defun eos::edit-debugger-init (data)
  "will edit .debugger-init"
  (eos::recenter-install data)
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  (find-file (expand-file-name "~/.debugger-init"))
  (goto-char (point-max))
  (setq eos::debugger-init-buffer (current-buffer))
  (insert "\
Debugger.SourceWindow.Lines: 0\n\
Debugger.CommandWindow.Lines: 15\n\
")
  )

(defun eos::save-debugger-init (data)
  "will save home .debugger-init"
  (eos::recenter-install data)
  (if (not (bufferp eos::debugger-init-buffer))
      (message "~/.debugger-init buffer not there anymore"))
  (switch-to-buffer eos::debugger-init-buffer)
  (save-buffer)
  (kill-buffer eos::debugger-init-buffer)
  )

(defun eos::edit-emacs (data)
  "will edit .emacs"
  (eos::recenter-install data)
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  (find-file (expand-file-name "~/.emacs"))
  (goto-char (point-max))
  (setq eos::emacs-buffer (current-buffer))
  (insert "\
(if (boundp 'era-version)\n\
  (progn\n\
    (eos::select-sbrowser-screen (selected-screen))\n\
    (eos::select-debugger-screen (selected-screen))\n\
    ))\n\
"))

(defun eos::save-emacs (data)
  "will save home .emacs"
  (eos::recenter-install data)
  (if (not (bufferp eos::emacs-buffer))
      (message "~/.emacs buffer not there anymore"))
  (switch-to-buffer eos::emacs-buffer)
  (save-buffer)
  (kill-buffer eos::emacs-buffer)
  )
