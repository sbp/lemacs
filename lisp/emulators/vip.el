;; 
;; This is Aamod Sane's new version of VIP (4.4.2) modified to work with
;; lemacs. I have added one customization variable,
;;
;; vip-ex-quit-can-exit-emacs
;;   Non-nil value will cause ex quit (i.e. ":q") to exit emacs if only
;;   one screen is left in the current lemacs session. If more than one
;;   screen is in use, the current buffer and all windows/screens displaying
;;   it will be killed, but emacs will not exit.
;;   A value of nil will simply kill the current buffer at all times (as
;;   in Epoch).
;;
;; I have tried to prevent unwanted zmacs screen highlighting when using
;; normal vi commands, while still allowing it when appropriate (mouse
;; highlighting, exchange-point-and-mark, etc). There may still be some
;; conditions I've missed; let me know if you run across one of these and
;; I'll try to fix it.
;;
;; Bill Clark
;; clarkw@stm.com
;;
;; November 19, 1993
;; 

;;; vip.el --- vi emulation

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Aamod Sane <sane@cs.uiuc.edu>
;; Maintainer: Aamod Sane <sane@cs.uiuc.edu>
;; Created: 5 Sep 1993
;; Version: 4.4.2
;; Keywords: emulation, editor

;; This file is part of GNU Emacs

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

;;; Commentary:

;; VIP is a VI emulation package for GNU Emacs. VIP implements most VI
;; and EX commands. VIP gives you the best of both worlds: VI keystrokes
;; for editing combined with the GNU Emacs environment. VIP also fixes
;; some common complaints with VI commands.

;; LCD Archive Entry:
;;
;; vip-mode|Aamod Sane|sane@cs.uiuc.edu
;; |Much improved vip mode - minor mode vi emulation
;; |93-7-2|4.4.2|~/modes/vip-mode.tar.Z

;;; Change log:
;;
;; Version 4.4.2: Aamod Sane (sane@cs.uiuc.edu)
;; 		  Corrected ex x;y, and bug in cw/dw/yw 
;; 		  Created vip-test-com-defun, and version vars
;; 		  and version-independent funcs for epoch/emacs18/19.
;; Version 4.4.1: Aamod Sane (sane@cs.uiuc.edu)
;; 		  Runs on Emacs 18/19. Changed vip-binding-of to confirm
;;		  to use lookup-key, and defined vip-mark-null
;;		  to work for both emacs 18 and 19. Also corrected "aD,Y.
;; Version 4.4: Aamod Sane (sane@cs.uiuc.edu)
;; 		Improved command dispatching, and a few bugs stomped.
;; Version 4.3.1: Aamod Sane (sane@cs.uiuc.edu)
;;		  Better minibuffer input code, no free vars.
;; Version 4.3: Aamod Sane (sane@cs.uiuc.edu)
;;		Jean Jacques Moreau (jjm@hplb.hpl.hp.com) code for 
;;		change,overwrite,:pre,:pwd,:cd and completion
;; Version 4.2: Aamod Sane (sane@cs.uiuc.edu)
;;		Jeff Lewis code for ex-style-motion
;; Version 4.1: Aamod Sane (sane@cs.uiuc.edu)
;; Version 3.5: Masahiko Sato (ms@sail.stanford.edu).
;;		In Japan masahiko@sato.riec.tohoku.junet

;; Acknowledgements: bug reports and helpful code from
;;	jjm@hplb.hpl.hp.com, jl@cse.ogi.edu
;;	rxga@ulysses.att.com,ascott@fws214.intel.com,lindstrom@stat.wisc.edu,
;;	toma@convex.convex.com,gvr@cs.brown.edu,dave@hellgate.utah.edu
;;	and others forgotten due to my limited mail archival abilities.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs versions, epoch etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst vip-epoch (boundp 'epoch::version))
(defconst vip-epoch-3 (and vip-epoch (boundp 'drag-button)))
(defconst vip-epoch-4 (and vip-epoch (boundp 'drag-zone)))
(defconst vip-emacs-19 (boundp 'terminal-frame))
(defconst vip-emacs-18 (boundp 'temp-buffer-show-hook))
(defconst vip-lucid (integerp (string-match "Lucid" emacs-version)))

(defmacro vip-errdefun (name)
  (` (if (not (fboundp (quote (, name))))
	 (defun (, name) (&rest args)
	   (error "VIP: %s invalid in emacs version %s" 
		  (symbol-name (quote (, name))) emacs-version)))))
		 
;; Stop bytecompiler complaints about epoch

(vip-errdefun find-buffer-other-screen)
(vip-errdefun epoch::delete-screen)
(vip-errdefun epoch::screens-of-buffer)

(defun vip-ex-find-buf (buf) 
  (if vip-epoch
      (find-buffer-other-screen buf)
    (if vip-lucid
	(switch-to-buffer-new-screen buf)
      (switch-to-buffer buf))))

(defvar vip-ex-quit-can-exit-emacs nil
  "Non-nil value will cause ex quit to exit emacs if only one screen is left")

(defvar win nil)

(defun vip-ex-kill-buf (buf)
  (if vip-epoch
      (mapcar 'epoch::delete-screen 
	      (epoch::screens-of-buffer (get-buffer buf))))
  (if vip-lucid
      (if (and vip-ex-quit-can-exit-emacs (eq (selected-screen) (next-screen)))
	  (save-buffers-kill-emacs)
	(catch 'dontquit
	  (progn
	    (if (buffer-modified-p buf)
		(if (not (y-or-n-p "Buffer modified, kill anyway? "))
		    (throw 'dontquit t)))
	    (setq win (get-buffer-window (get-buffer buf) t t))
	    (while (not (equal win nil))
	      (delete-window win)
	      (setq win (get-buffer-window (get-buffer buf) t t)))
	    (set-buffer buf)
	    (set-buffer-modified-p nil)
	    (message "%s%s" (buffer-name buf) " killed")
	    (kill-buffer buf))))
    (kill-buffer buf)))


;; Try stop bytecompiler complaints about emacs versions

(defvar unread-command-char)
;(defvar unread-command-event)

(defun vip-null-mark () 
  (if vip-emacs-18 (null (mark)) (null (mark t))))
  
(defun vip-unread (c) 
  (if vip-emacs-18
      (setq unread-command-char c)
    (setq unread-command-event (character-to-event c))))
;    (setq unread-command-event
;	  (copy-event (character-to-event c) unread-command-event))))
;    (setq unread-command-event (cons c unread-command-event))))
    
(defun vip-unreadp () 
  (if vip-emacs-18
      (>= unread-command-char 0) (eventp unread-command-event)))
;      (>= unread-command-char 0) (consp unread-command-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keymaps and related variables

(defvar vip-mode-map nil
  "Map for Vi mode bindings")

(defvar vip-insert-mode-map nil
  "Map for Insert mode bindings")

(defvar vip-emacs-local-map nil
  "Local map used in emacs mode. \(buffer specific\)")

(defvar vip-insert-local-map nil
  "Local map used in insert command mode. \(buffer specific\)")

(make-variable-buffer-local 'vip-emacs-local-map)
(make-variable-buffer-local 'vip-insert-local-map)

(defvar vip-insert-mode-vi-map t
  "* Whether you want the vi map in insert mode or want to use emacs mode maps.
Buffer specific so that that you can choose depending on the mode")

(make-variable-buffer-local 'vip-insert-mode-vi-map)

(defvar vip-minibuf-map nil
  "Map for minibuffer editing")

;; Modes and related variables

(defvar vip-current-mode nil
  "Current mode.  One of emacs-mode, vi-mode, insert-mode.")

(make-variable-buffer-local 'vip-current-mode)
(setq-default vip-current-mode 'emacs-mode)

(defvar vip-emacs-mode-line-buffer-identification nil
  "value of mode-line-buffer-identification in emacs-mode.")
(make-variable-buffer-local 'vip-emacs-mode-line-buffer-identification)
(setq-default vip-emacs-mode-line-buffer-identification
	      '("Emacs: %17b"))

(defvar vip-current-major-mode nil
  "vip-current-major-mode is the major-mode vi considers it is now.
\(buffer specific\)")

(make-variable-buffer-local 'vip-current-major-mode)

(defvar vip-toggle-key "\C-z"
  "*The key which will be used to change modes from emacs to vi and back.
In insert mode, this will also function as Meta")

(defvar vip-ESC-key "\e" 
  "* key used to ESC")

(defvar vip-no-multiple-ESC t
  "*If true, multiple ESC in vi mode will cause bell to ring.
\_ is then mapped to Meta")

(defvar vip-help-in-insert-mode nil
  "*if t then C-h is bound to help-command in insert mode, if nil then it is
bound to delete-backward-char.")

;; Replace mode and changing text

(defvar vip-c-string ""
  "Change string")

(defvar vip-need-to-finish-change nil
  "Change command still need to be finished.
\(buffer specific\)")

(make-variable-buffer-local 'vip-need-to-finish-change)

(defvar vip-need-to-exit-overwrite nil
  "Overwrite mode needs to aborted.\(buffer specific\)")

(make-variable-buffer-local 'vip-need-to-exit-overwrite)

(defvar vip-change-beg-point nil
  "Remember point where change is to start.\(buffer specific\)")

(set-default 'vip-change-beg-point (make-marker))
(make-variable-buffer-local 'vip-change-beg-point)

(defvar vip-change-end-point nil
  "Remember point where change is to end.\(buffer specific\)")

(set-default 'vip-change-end-point (make-marker))
(make-variable-buffer-local 'vip-change-end-point)

;; Autoindent in insert

(defvar vip-cted nil
  "cted - Control T'ed - keep track of whether C-t has been pressed.
\(buffer specific\)")
(make-variable-buffer-local 'vip-cted)

(defvar vip-current-indent 0
  "Preserve the indent value, used during ^^D.\(buffer specific\)")
(make-variable-buffer-local 'vip-current-indent)

(defvar vip-preserve-indent nil
  "Whether to preserve the indent, used by ^^D.\(buffer specific\)")
(make-variable-buffer-local 'vip-preserve-indent)

(defvar vip-auto-indent nil
  "* Autoindent if t.")

(defvar vip-shift-width 8
  "* the shiftwidth variable")

(defconst vip-ENOCMD "No such command from VIP")
(defconst vip-ENOKEY "Unknown Object in Keymap: %s") 
(defconst vip-EMJRMODE 
  "Keymap changed: execute M-x vip-keymap-error for more info")
(defconst vip-EINSERTMODE 
  "Insert mode to insert mode: this is a bug if repeatable :-)")

(defun vip-keymap-error()
  (interactive)
  (switch-to-buffer "VIP Keymap Error Message")
  (erase-buffer)
  (insert
   "The Keymap error happens when you have set the default
major mode to vip and the actual major mode of the
buffer does not call kill-all-local-variables. Then vip-mode is
called when the buffer is created and the major mode afterward
happily munges vip local keymaps. If you encounter this error,
a quick fix is to call vip-mode again afterward using the
vip-toggle-key (C-z by default). The real fix is to get the
offending major mode fixed. Known modes that have
this problem are texinfo and finder. Complain to the authors
or to gnu.emacs.help so that the mode is fixed.

Kill this buffer after you are done.\n"))

;; Variables for repeating destructive commands

(defvar vip-insert-point nil
  "Remember insert point as a marker. \(buffer specific\)")

(set-default 'vip-insert-point (make-marker))
(make-variable-buffer-local 'vip-insert-point)

(defvar vip-com-point nil
  "Remember com point as a marker. \(buffer specific\)")

(set-default 'vip-com-point (make-marker))
(make-variable-buffer-local 'vip-com-point)

(defvar vip-d-com nil
  "If non-nil, it's value is a list (M-COM VAL COM), and is used to
re-execute last destrcutive command")

(defvar vip-d-char nil
  "The character remembered by the vi \"r\" command")

(defvar vip-use-register nil
  "name of register to store deleted or yanked strings.")

;; Variables for Moves and Searches

(defvar vip-f-char nil
  "for use by \";\" command")

(defvar vip-F-char nil
  "for use by \".\" command")

(defvar vip-f-forward nil
  "for use by \";\" command")

(defvar vip-f-offset nil
  "for use by \";\" command")

(defvar vip-s-string nil
  "last search string")

(defvar vip-s-forward nil
  "if t, search is forward.")

(defvar vip-case-fold-search nil
  "*if t, search ignores cases.")

(defvar vip-re-search t
  "*if t, search is reg-exp search, otherwise vanilla search.")

(defvar vip-re-query-replace t
  "*If t then do regexp replace, if nil then do string replace.")

;; vip replace string is no longer used
(defvar vip-re-replace t
  "*If t then do regexp replace, if nil then do string replace.")

(defvar vip-ex-style-motion t
  "*l,h do not cross lines, ESC backs up etc.")

(defvar vip-buffer-search-char ?g
  "*Key bound for buffer-searching")

(defvar vip-search-wrap-around-t t
  "*if t, search wraps around")

(defvar vip-heading-start 
  (concat "^\\s-*(\\s-*defun\\s-\\|"			        ;; lisp
	  "^{\\s-*$\\|^[_a-zA-Z][^()]*[()].*{\\s-*$\\|"	        ;; C/C++
	  "^\\s-*class.*{\\|^\\s-*struct.*{\\|^\\s-*enum.*{\\|"
	  "^\\\\[sb][a-z]*{.*}\\s-*$\\|"	    		;; latex
	  "^@node\\|@table\\|^@m?enu\\|^@itemize\\|^@if\\|"	;; texinfo
	  "^.+:-")			                        ;; prolog
  "* Regexps for Headings. Used by [[, ]].")

(defvar vip-heading-end 
  (concat "^}\\|"						;; C/C++
	  "^\\\\end{\\|"					;; latex
	  "^@end \\|"						;; texinfo
	  ")\n\n[ \t\n]*\\|"					;; lisp
	  "\\.\\s-*$")						;; prolog
      "* Regexps to end Headings/Sections. Used by []")

;; VIP word definitions

(defvar vip-word-chars-alpha "a-zA-Z0-9_")
(defvar vip-word-chars-nonalpha "^a-zA-Z0-9_ \t\n")
(defvar vip-word-chars-separator " \t\n")

;; History variables

(defvar vip-history nil)
(defvar vip-search-history nil)
(defvar vip-ex-history nil)

(defvar vip-want-history nil
  "* History for vi searches and ex commands")

;; IO setup

(defvar vip-make-cc-quit nil
  "* If set, will call set-input-mode to make ^C the quit character.
However, there is no quit-char option for older emacsen. Moreover, the
first two options may need to change from system to system")

;; Keyboard macros

(defvar vip-last-macro-reg nil
  "Remember the last register used for keyboard macro")

;; Completion

(defvar vip-filename-complete nil
  "Tell the rest of VIP that completion in on")

;; Shell command histories. There should not be 2 of these.

(defvar vip-ex-last-shell-com nil
  "last shell command executed by :! command")

(defvar vip-last-shell-com nil
  "last shell command executed by ! command")

;; VIP setup

(defvar vip-inhibit-startup-message nil)

(defvar vip-always nil
  "* Default vip-mode for files and buffers")

(defvar vip-custom-file-name "~/.vip"
  "* Customisation file")

(defvar vip-is-vi nil
  "Ultimate compatibility test")

;; Misc

(defvar vip-quote-string "> "
  "string inserted at the beginning of region")

(defvar vip-tags-file-name "TAGS")


;; basic set up

(global-set-key vip-toggle-key 'vip-change-mode-to-vi)

(defun vip-redefine-toggle-key(key)
  "Redefine the toggle key to be the ARG"
  (global-unset-key key)
  (define-key vip-mode-map key nil)
  (setq vip-toggle-key key)
  (global-set-key vip-toggle-key 'vip-change-mode-to-vi)
  (define-key vip-mode-map vip-toggle-key 'vip-change-mode-to-emacs))

(defmacro vip-loop (count body)
  "(COUNT BODY) Execute BODY COUNT times."
  (list 'let (list (list 'count count))
	(list 'while (list '> 'count 0)
	      body
	      (list 'setq 'count (list '1- 'count)))))

(defun vip-push-mark-silent (&optional location)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
No message."
  (if (vip-null-mark)
      nil
    (setq mark-ring (cons (copy-marker (mark-marker t)) mark-ring))
    (if (> (length mark-ring) mark-ring-max)
	(progn
	  (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
	  (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil))))
  (set-mark (or location (point))))

(defun vip-goto-col (arg)
  "Go to ARG's column."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (save-excursion
      (end-of-line)
      (if (> val (1+ (current-column))) (error "")))
    (if com (move-marker vip-com-point (point)))
    (beginning-of-line)
    (forward-char (1- val))
    (if com (vip-execute-com 'vip-goto-col val com))))

(defun vip-refresh-mode-line ()
  "Redraw mode line."
  (set-buffer-modified-p (buffer-modified-p)))

(defun vip-copy-keymap (map)
  (if (null map) (make-sparse-keymap) (copy-keymap map)))


;; changing mode

(defun vip-change-mode (new-mode)
  "Change mode to NEW-MODE.  NEW-MODE is either emacs-mode, vi-mode,
or insert-mode."
  (cond ((eq new-mode 'vi-mode)
	 (cond ((eq vip-current-mode 'insert-mode)
		(setq vip-need-to-exit-overwrite t)
		(if (eq vip-need-to-finish-change t)
		    (vip-finish-change)
		  (progn
		    (vip-copy-region-as-kill (point) vip-insert-point)
		    (vip-repeat-insert-command)))
		(if vip-ex-style-motion
		    (if (not (bolp))
			(backward-char 1))))
	       ;; to support multiple successive (vip-mode) calls
	       ;; with keymap changes in between.
	       ;; patch specifically for texinfo mode.
	       ((eq vip-current-mode 'vi-mode)
		(if (not (eq vip-mode-map (current-local-map)))
		    (progn
		      (message vip-EMJRMODE)
		      (setq vip-emacs-local-map (current-local-map)
			    vip-insert-local-map (vip-copy-keymap
						  (current-local-map))))))
	       (t
		(setq vip-emacs-local-map (current-local-map)
		      vip-emacs-mode-line-buffer-identification
		      mode-line-buffer-identification
		      vip-insert-local-map (vip-copy-keymap
					    (current-local-map)))))
	 (vip-change-mode-line "Vi:   ")
	 (use-local-map vip-mode-map))
	((eq new-mode 'insert-mode)
	 (cond ((eq vip-current-mode 'emacs-mode)
		(move-marker vip-insert-point (point))
		(setq vip-emacs-local-map (current-local-map)
		      vip-emacs-mode-line-buffer-identification
		      mode-line-buffer-identification
		      vip-insert-local-map (vip-copy-keymap
					    (current-local-map))))
	       ((eq vip-current-mode 'insert-mode)
		;; Redo boundary lost, otherwise ok
		(if (not (eq vip-insert-local-map (current-local-map)))
		    (progn
		      (message vip-EMJRMODE)
		      (setq vip-insert-local-map (vip-copy-keymap
						  (current-local-map))))
		  (message vip-EINSERTMODE)))
	       (t
		(move-marker vip-insert-point (point))
		(setq vip-insert-local-map (vip-copy-keymap
					    vip-emacs-local-map))))
	 (vip-change-mode-line "Insert")
	 (use-local-map vip-insert-local-map)
(setq meta-prefix-char 28)
	 (define-key vip-insert-local-map vip-ESC-key 'vip-exit-insert-mode)
	 ;(define-key vip-insert-local-map vip-toggle-key 'vip-alternate-ESC)
	 (vip-insert-mode-bindings))
	((eq new-mode 'emacs-mode) 
	 (vip-change-mode-line "Emacs:")
	 (use-local-map vip-emacs-local-map)))
  (setq vip-current-mode new-mode)
  (vip-refresh-mode-line))

;; default insert mode bindings other than ESC and Meta
;; test fboundp to allow this function to be redefined before
;; or after loading.

;; Define an insert mode map. It is used so that map! can be used to redefine
;; insert mode entries

(setq vip-insert-mode-map (make-sparse-keymap))

(define-key vip-insert-mode-map "\C-c" 'vip-insert-quit)
(define-key vip-insert-mode-map "\C-g" 'self-insert-command)
(define-key vip-insert-mode-map "\C-v" 'quoted-insert)
(define-key vip-insert-mode-map "\C-h" 
  (if vip-help-in-insert-mode 'help-command 'delete-backward-char))
(define-key vip-insert-mode-map "\t" 'self-insert-command)
(define-key vip-insert-mode-map "\C-w" 'vip-delete-backward-word)
(define-key vip-insert-mode-map "\C-u" 'vip-erase-line)
(define-key vip-insert-mode-map "\C-t" 'vip-forward-indent)
(define-key vip-insert-mode-map "\C-d" 'vip-backward-indent)
(define-key vip-insert-mode-map "\177" 'delete-backward-char)
;;(define-key vip-insert-mode-map "\C-z" 'self-insert-command)

(or (fboundp 'vip-insert-mode-bindings)
    (defun vip-insert-mode-bindings()
      (if vip-insert-mode-vi-map
	  (progn
	    (vip-rebind-emacs-keys vip-insert-local-map 'self-insert-command)
	    (vip-rebind-other-keys vip-insert-local-map 'self-insert-command)
	    (vip-add-keymap vip-insert-mode-map vip-insert-local-map)
	    (if vip-auto-indent
		(progn
		  (define-key vip-insert-local-map "\C-m" 'vip-autoindent)
		  (define-key vip-insert-local-map "\C-j" 'vip-autoindent)))))
      ))

(defun vip-add-keymap(mapsrc mapdst)
  "Add contents of mapsrc to mapdst. It is assumed that mapsrc is sparse"
  (mapcar (function (lambda (p) 
		      (define-key mapdst (char-to-string (car p)) (cdr p))))
	  (cdr-safe mapsrc)))

(defun vip-copy-region-as-kill (beg end)
  "If BEG and END do not belong to the same buffer, it copies empty region."
  (condition-case nil
      (copy-region-as-kill beg end)
    (error (copy-region-as-kill beg beg))))

(defun vip-change-mode-line (string)
  "Assuming that the mode line format contains the string \"Emacs:\", this
function replaces the string by \"Vi:   \" etc."
  (setq mode-line-buffer-identification
	(if (string= string "Emacs:")
	    vip-emacs-mode-line-buffer-identification
	  (list (concat string " %17b")))))

(defvar vip-initialized nil
  "Set after initial mode start to prevent multiple .vip loads")

;;;###autoload
(defun vip-mode ()
  "Turn on VIP emulation of VI."
  (interactive)
  ;;; since I preload vip, put load .vip here and add new variable
  ;;; to keep from reloading everytime vip-mode is called (which
  ;;; happens a LOT
  (if (not vip-initialized)
    (if (file-exists-p vip-custom-file-name) (load vip-custom-file-name)))
  (setq vip-initialized t)
  (if (not vip-inhibit-startup-message)
      (progn
	(setq vip-inhibit-startup-message t)
	(switch-to-buffer "VIP Startup Message")
	(erase-buffer)
	(insert
	 (substitute-command-keys
	  "VIP is a VI emulation package for GNU Emacs.VIP provides most 
VI and EX commands.The important differences from VI are:
    1. VI EXIT functions (e.g. :wq) work on INDIVIDUAL files.
    2. \"ZZ\" and ^X^C EXITS EMACS.
    3. \"u\" will undo. Repeat undo by \".\". Another u changes direction.
    5. ^X will invoke emacs functions; ^Z will toggle vi/emacs modes.
    6. Emacs Meta functions are invoked by \"_\" instead of ESC.
    7. Try ^C,^G repeatedly and \\[abort-recursive-edit] if something strange happens. 
You can get more information on VIP by:
    1.  Typing `M-x info' and selecting menu item \"vip\".
    2.  Printing VIP manual which can be found as GNU/man/vip.texinfo
    3.  Printing VIP Reference Card which can be found as GNU/etc/vipcard.tex
Ultimate compatibilty: Execute vip-become-vi. Emacs is suppressed.
    
This startup message appears whenever you load VIP unless you type `y' now.
Type `n' to quit this window for now.\n"))
	(goto-char (point-min))
	(if (y-or-n-p "Inhibit VIP startup message? ")
	    (let ((buf (find-file-noselect (substitute-in-file-name 
					    vip-custom-file-name)))) 
	      (save-excursion
		(set-buffer buf)
		(goto-char (point-max))
		(insert "\n(setq vip-inhibit-startup-message t)\n")
		(save-buffer))
	      (kill-buffer buf)
	      (message "VIP startup message inhibited.")
	      (sit-for 2)))
	(kill-buffer (current-buffer))
	(message "")))
  (vip-change-mode-to-vi))

(defun vip-exit-insert-mode ()
  "Exit from insert mode to vi mode."
  (interactive)
  (setq meta-prefix-char 27)
  (vip-change-mode 'vi-mode))
;;   (if vip-ex-style-motion
;;       (if (and (eolp) (save-excursion (skip-chars-backward " \t") (bolp)))
;; 	  (while (not (bolp)) (delete-char -1))
;; 	(if (not (bolp)) (backward-char 1)))))
;; ;; the crabwalk here isn't strictly correct... vip only backs up when the
;; ;; cursor is at the current indent level and only whitespace preceeds it

(defun vip-insert-quit ()
  "Exit from insert mode to vi mode and do a keyboard quit."
  (interactive)
  (vip-exit-insert-mode)
  (vip-keyboard-quit))

(defun vip-change-mode-to-vi ()
  "Change mode to vi mode."
  (interactive)
  (vip-change-mode 'vi-mode))

(defun vip-change-mode-to-insert ()
  "Change mode to insert mode."
  (interactive)
  (vip-change-mode 'insert-mode))

(defun vip-change-mode-to-emacs ()
  "Change mode to emacs mode."
  (interactive)
  (vip-change-mode 'emacs-mode))


;; escape to emacs mode termporarilly

(defun vip-get-editor-command (l-map g-map &optional str)
  "Read characters from keyboard until an editor command is formed, using
local keymap L-MAP and global keymap G-MAP.  If the command is a
self-insert-command, the character just read is returned instead.  Optional
string STR is used as initial input string."
  (let (char l-bind g-bind)
    (setq char
	  (if (or (null str) (string= str ""))
	      (read-char)
	    (string-to-char str)))
    (setq last-command-char char)
    (setq l-bind (vip-binding-of char l-map))
    (if (null l-bind)
	;; since local binding is empty, we concentrate on global one.
	(progn
	  (setq g-bind (vip-binding-of char g-map))
	  (if (null g-bind)
	      nil ;; return nil, since both bindings are void.
	    (if (keymapp g-bind)
		(vip-get-editor-command nil g-bind (vip-string-tail str))
	      (if (eq g-bind 'self-insert-command) char g-bind))))
      ;; local binding is nonvoid
      (if (keymapp l-bind)
	  ;; since l-bind is a keymap, we consider g-bind as well.
	  (progn
	    (setq g-bind (vip-binding-of char g-map))
	    (if (null g-bind)
		(vip-get-editor-command l-bind nil (vip-string-tail str))
	      (if (keymapp g-bind)
		  ;; both bindings are keymap
		  (vip-get-editor-command l-bind g-bind (vip-string-tail str))
		;; l-bind is a keymap, so we neglect g-bind
		(vip-get-editor-command l-bind nil (vip-string-tail str)))))
	;; l-bind is a command
	(if (eq l-bind 'self-insert-command) char l-bind)))))

(defun vip-binding-of (char map)
  "Return key-binding of CHAR under keymap MAP.  It is nil if the binding
is void, or a command, or a keymap"
  (let ((val (if (null map) nil (lookup-key map (char-to-string char)))))
    (cond ((null val) nil)
	  ((keymapp val) val)
	  ((commandp val) val)
	  ((consp val)
	   (cond ((eq (car val) 'keymap) (error "keymapp failed!\n"))
		 ((eq (car val) 'lambda) (error "commandp failed!\n"))
		 ((keymapp (car val)) (vip-binding-of (car val) (cdr val)))
		 ((and (or vip-lucid vip-emacs-19) (stringp (car val)))
		  (while (stringp (car val)) (setq val (cdr val))) val)
		 (t (error vip-ENOKEY val))))
	  ((symbolp val)
	   (let ((fun (symbol-function val)))
	     (while (symbolp fun) (setq fun (symbol-function fun))) fun))
	  ((stringp val) val)
	  (t (error vip-ENOKEY val)))))

(defun vip-escape-to-emacs (arg &optional char)
  "Escape to emacs mode and execute one emacs command and then return to
vi mode.  ARG is used as the prefix value for the executed command.  If
CHAR is given it becomes the first character of the command."
  (interactive "P")
  (let (com (buff (current-buffer)) (first t))
    (if char (vip-unread char))
    (setq prefix-arg arg)
    (while (or first (vip-unreadp))
      ;; this while loop is executed until unread command char will be
      ;; exhausted.
      (setq first nil)
      (setq com (vip-get-editor-command vip-emacs-local-map global-map))
      (if (numberp com)
	  (vip-loop (vip-p-val prefix-arg)
		    (insert (char-to-string com)))
	(command-execute com prefix-arg)))
    (setq prefix-arg nil)  ;; reset prefix arg
    ))

(defun vip-message-conditions (conditions)
  "Print CONDITIONS as a message."
  (let ((case (car conditions)) (msg (cdr conditions)))
    (if (null msg)
	(message "%s" case)
      (message "%s %s" case (prin1-to-string msg)))
    (ding)))

(defun vip-ESC (arg)
  "Emulate ESC key in Emacs mode. Prevent multiple escape keystrokes if
vip-no-multiple-ESC is true. In that case \@ will be bound to ESC"
  (interactive "P")
  (if (or (not vip-no-multiple-ESC) (eq vip-current-mode 'insert-mode))
      (vip-escape-to-emacs arg ?\e)
    (ding)))

(defun vip-alternate-ESC (arg)
  "ESC key without checking for multiple keystrokes"
  (interactive "P")
  (vip-escape-to-emacs arg ?\e))

(defun vip-ctl-c (arg)
  "Emulate C-c key in Emacs mode."
  (interactive "P")
  (vip-escape-to-emacs arg ?\C-c))

(defun vip-ctl-x (arg)
  "Emulate C-x key in Emacs mode."
  (interactive "P")
  (vip-escape-to-emacs arg ?\C-x))

(defun vip-ctl-h (arg)
  "Emulate C-h key in Emacs mode."
  (interactive "P")
  (vip-escape-to-emacs arg ?\C-h))


;;
;; IMPLEMENTING VIP COMMANDS
;;

;; Generic predicates

;; generate test functions
;; given symbol foo, foo-p is the test function, foos is the set of
;; vip command keys
;; (macroexpand '(vip-test-com-defun foo))
;; (defun foo-p (com) (consp (memq (if (< com 0) (- com) com) foos)))

(defmacro vip-test-com-defun (name)
  (let* ((snm (make-symbol "s1")) (snm (symbol-name name))
	 (nm-p (make-symbol "s2")) (nm-p (intern (concat snm "-p")))
	 (nms (make-symbol "s3")) (nms (intern (concat snm "s"))))
    (` (defun (, nm-p) (com) 
	 (consp (memq (if (< com 0) (- com) com) (, nms)))))))
  
;; Variables for defining VI commands

(defconst vip-prefix-commands '(?c ?d ?y ?! ?= ?# ?< ?> ?\")
  "Modifying commands that can be prefixes to movement commands")
(vip-test-com-defun vip-prefix-command)
  
(defconst vip-charpair-commands '(?c ?d ?y ?! ?= ?< ?> ?r ?R)
  "Commands that are pairs eg. dd. r and R here are a hack")
(vip-test-com-defun vip-charpair-command)

(defconst vip-movement-commands '(?b ?B ?e ?E ?f ?F ?G ?h ?H ?j ?k ?l
				     ?H ?M ?n ?t ?T ?w ?W ?$ ?%
				     ?^ ?( ?) ?- ?+ ?| ?{ ?} ?[ ?] ?' ?`
				     ?\; ?, ?0 ?? ?/)
				     "Movement commands")
(vip-test-com-defun vip-movement-command)

(defconst vip-dotable-commands '(?c ?d ?y ?C ?Y ?D)
  "Commands that can be repeated by .(dotted)")
(vip-test-com-defun vip-dotable-command)

(defconst vip-hash-cmds '(?c ?C ?g ?q ?S)
  "Commands that can follow a #")
(vip-test-com-defun vip-hash-cmd)

(defconst vip-regsuffix-commands '(?d ?y ?Y ?D ?p ?P ?x ?X)
  "Commands that may have registers as prefix")
(vip-test-com-defun vip-regsuffix-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prefix argmument for vi mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In vi mode, prefix argument is a dotted pair (NUM . COM) where NUM
;; represents the numeric value of the prefix argument and COM represents
;; command prefix such as "c", "d", "m" and "y".

(defun vip-prefix-arg-value (char value com)
  "Compute numeric prefix arg value.  Invoked by CHAR.  VALUE is the value
obtained so far, and COM is the command part obtained so far."
  (while (and (>= char ?0) (<= char ?9))
    (setq value (+ (* (if (numberp value) value 0) 10) (- char ?0)))	    
    (setq char (read-char)))
  (setq prefix-arg value)
  (if com (setq prefix-arg (cons prefix-arg com)))
  (while (= char ?U)
    (vip-describe-arg prefix-arg)
    (setq char (read-char)))
  (vip-unread char))

;; Setup commands of the form {value command-char rest} as prefix-arg
;; (value.com) to be picked up by command rest. Thus 3d/foo becomes
;; (3.d) prefix-arg to "/"
;; Commands of the form dd, dr, == etc. get handled under the case
;; charpair. 

(defun vip-prefix-arg-com (char value com)
  "Vi operator as prefix argument."
  (let ((charpair nil))
    (cond 
     ((= char ?#)			; #<char><move>
      (setq com (+ 128 (read-char)))	; M-char
      (if (not (vip-hash-cmd-p (- 128 com)))
	  (error "#%c: %s" (- com 128) vip-ENOCMD)
	(setq char (read-char))
	(if (or (= char ?r) (= char ?R)) ; emacs region
	    (setq charpair t)
	  (if (not (or (vip-movement-command-p char) ; movement or
		       (and (>= char ?1) (<= char ?9)))) ; count or
	      (error "#%c%c: %s" (- com 128) char vip-ENOCMD)))))
		   		
     ((= char ?\")		; \"<reg>[pP], \"<reg>yd<move>,; \"<reg>YD 
      (let ((reg (read-char)))
	(if (or (and (<= ?A reg) (<= reg ?z))
		(and (<= ?1 reg) (<= reg ?9)))
	    (setq vip-use-register reg)
	  (error "%c: Not a register" reg))
	(setq char (read-char))
	(if (not (or (vip-regsuffix-command-p char)    ; command
		     (and (>= char ?1) (<= char ?9)))) ; or count
	    (error "\"%c%c: %s" reg char vip-ENOCMD))))
     
     (t					; other prefixables e.g. d'a, !} etc.
      (setq com char)
      (setq char (read-char))
      (if (vip-charpair-command-p char)	; e.g. dd, yy, dr etc.
	  (setq charpair t)
	(if (not (or (vip-movement-command-p char)  ; movement or
		     (and (>= char ?1) (<= char ?9)))) ; count
	    (error "%c%c: %s" com char vip-ENOCMD)))))
    
    (setq prefix-arg (cons value com))
    
    (if charpair
	(progn
	  (cond ((= char ?r) (vip-region prefix-arg))
		((= char ?R) (vip-Region prefix-arg))
		(t
		 (setq prefix-arg nil)
		 (vip-line (cons (if (null value) 1 value) (upcase char)))))
	  (setq prefix-arg nil))
      ;;(while (= char ?U)		; handle non-charpairs.
      ;;(vip-describe-arg prefix-arg)	; U loop for debugging.
      ;;(setq char (read-char)))
      (vip-unread char))	; unread for next command.
    ))

(defun vip-describe-arg (arg)
  (let (val com)
    (setq val (vip-P-val arg)
	  com (vip-getcom arg))
    (if (null val)
	(if (null com)
	    (message "Value is nil, and commmand is nil.")
	  (message "Value is nil, and command is %c." com))
      (if (null com)
	  (message "Value is %d, and command is nil." val)
	(message "Value is %d, and command is %c." val com)))))

(defun vip-digit-argument (arg)
  "Begin numeric argument for the next command."
  (interactive "P")
  (vip-prefix-arg-value last-command-char nil
			(if (consp arg) (cdr arg) nil)))

;; prefix can be
;; nil, integer, list of 1 integer, (value.com) e.g (3.d) for 3d/foo
;; The first three are normal emacs, and the integer is value.
;; The last is vip generated for prefix commands such as 3d etc.
;; In each case, the value and com is faithfully transmitted to
;; vip-prefix-arg-com. See vip-prefix-arg-com for example of
;; (value.com) generation.

(defun vip-command-argument (arg)
  "Accept a motion command as an argument."
  (interactive "P")
  (condition-case conditions
      (vip-prefix-arg-com
       last-command-char   
       (cond ((null arg) nil)
	     ((consp arg) (car arg))
	     ((numberp arg) arg)
	     (t (error "strange arg")))
       (cond ((null arg) nil)
	     ((consp arg) (cdr arg))
	     ((numberp arg) nil)
	     (t (error "strange arg"))))
    (quit
     (setq vip-use-register nil)
     (signal 'quit nil)))
)

(defun vip-p-val (arg)
  "Get value part of prefix-argument ARG."
  (cond ((null arg) 1)
	((consp arg) (if (null (car arg)) 1 (car arg)))
	(t arg)))

(defun vip-P-val (arg)
  "Get value part of prefix-argument ARG."
  (cond ((consp arg) (car arg))
	(t arg)))

(defun vip-getcom (arg)
  "Get com part of prefix-argument ARG."
  (cond ((null arg) nil)
	((consp arg) (cdr arg))
	(t nil)))

(defun vip-getCom (arg)
  "Get com part of prefix-argument ARG and modify it."
  (let ((com (vip-getcom arg)))
    (cond ((equal com ?c) ?C)
	  ((equal com ?d) ?D)
	  ((equal com ?y) ?Y)
	  (t com))))


;; repeat last destructive command

(defun vip-append-to-register (reg start end)
  "Append region to text in register REG.
START and END are buffer positions indicating what to append."
  (set-register reg (concat (or (get-register reg) "")
			    (buffer-substring start end))))

;; define functions to be executed

(defun vip-exec-change(com) 
  (if (= com ?c)
      (vip-change vip-com-point (point))
    (vip-change-subr vip-com-point (point))))

(defun vip-exec-Change(com)
  (save-excursion
    (set-mark vip-com-point)
    (vip-enlarge-region (mark t) (point))
    (if vip-use-register
	(progn
	  (cond ((and (<= ?a vip-use-register)
		      (<= vip-use-register ?z))
		 (copy-to-register
		  vip-use-register (mark t) (point) nil))
		((and (<= ?A vip-use-register)
		      (<= vip-use-register ?Z))
		 (vip-append-to-register
		  (+ vip-use-register 32) (mark t) (point)))
		(t (setq vip-use-register nil)
		   (error "")))
	  (setq vip-use-register nil)))
    (delete-region (mark t) (point)))
  (open-line 1)
  (if (= com ?C) (vip-change-mode-to-insert) (yank)))

(defun vip-exec-delete(com)
  (if vip-use-register
      (progn
	(cond ((and (<= ?a vip-use-register)
		    (<= vip-use-register ?z))
	       (copy-to-register
		vip-use-register vip-com-point (point) nil))
	      ((and (<= ?A vip-use-register)
		    (<= vip-use-register ?Z))
	       (vip-append-to-register
		(+ vip-use-register 32) vip-com-point (point)))
	      (t (setq vip-use-register nil)
		 (error "")))
	(setq vip-use-register nil)))
  (setq last-command
	(if (eq last-command 'd-command) 'kill-region nil))
  (kill-region vip-com-point (point))
  (setq this-command 'd-command)
  (if vip-ex-style-motion
      (if (and (eolp) (not (bolp))) (backward-char 1)))
)

;; Free variable reference here is intentional.
;; Somewhat better code organization would be required to correct that.
;; Do it later.
(defun vip-exec-Delete(com)
  (save-excursion
    (set-mark vip-com-point)
    (vip-enlarge-region (mark t) (point))
    (if vip-use-register
	(progn
	  (cond ((and (<= ?a vip-use-register)
		      (<= vip-use-register ?z))
		 (copy-to-register
		  vip-use-register (mark t) (point) nil))
		((and (<= ?A vip-use-register)
		      (<= vip-use-register ?Z))
		 (vip-append-to-register
		  (+ vip-use-register 32) (mark t) (point)))
		(t (setq vip-use-register nil)
		   (error "")))
	  (setq vip-use-register nil)))
    (setq last-command
	  (if (eq last-command 'D-command) 'kill-region nil))
    (kill-region (mark t) (point))
    (if (eq m-com 'vip-line) (setq this-command 'D-command)))
  (back-to-indentation))

(defun vip-exec-yank(com)
  (if vip-use-register
      (progn
	(cond ((and (<= ?a vip-use-register)
		    (<= vip-use-register ?z))
	       (copy-to-register
		vip-use-register vip-com-point (point) nil))
	      ((and (<= ?A vip-use-register)
		    (<= vip-use-register ?Z))
	       (vip-append-to-register
		(+ vip-use-register 32) vip-com-point (point)))
	      (t (setq vip-use-register nil)
		 (error "")))
	(setq vip-use-register nil)))
  (setq last-command nil)
  (copy-region-as-kill vip-com-point (point))
  (goto-char vip-com-point)
)

(defun vip-exec-Yank(com)
  (save-excursion
    (set-mark vip-com-point)
    (vip-enlarge-region (mark t) (point))
    (if vip-use-register
	(progn
	  (cond ((and (<= ?a vip-use-register)
		      (<= vip-use-register ?z))
		 (copy-to-register
		  vip-use-register (mark t) (point) nil))
		((and (<= ?A vip-use-register)
		      (<= vip-use-register ?Z))
		 (vip-append-to-register
		  (+ vip-use-register 32) (mark t) (point)))
		(t (setq vip-use-register nil)
		   (error "")))
	  (setq vip-use-register nil)))
    (setq last-command nil)
    (copy-region-as-kill (mark t) (point)))
  (goto-char vip-com-point))

(defun vip-exec-bang(com)
  (save-excursion
    (set-mark vip-com-point)
    (vip-enlarge-region (mark t) (point))
    (shell-command-on-region
     (mark t) (point)
     (if (= com ?!)
	 (setq vip-last-shell-com (vip-read-string "!"))
       vip-last-shell-com)
     t)))

(defun vip-exec-equals(com)
  (save-excursion
    (set-mark vip-com-point)
    (vip-enlarge-region (mark t) (point))
    (if (> (mark t) (point)) (exchange-point-and-mark))
    (indent-region (mark t) (point) nil)))

(defun vip-exec-shift(com)
  (save-excursion
      (set-mark vip-com-point)
      (vip-enlarge-region (mark t) (point))
      (if (> (mark t) (point)) (exchange-point-and-mark))
      (indent-rigidly (mark t) (point) 
		      (if (= com ?>) vip-shift-width (- vip-shift-width)))))

(defun vip-exec-buffer-search(com)
  (setq vip-s-string (buffer-substring (point) vip-com-point))
  (setq vip-s-forward t)
  (if vip-want-history			; refer to "gmhist.el"
      (progn
	(put 'vip-search-history 'default vip-s-string)
	(setq vip-search-history
	      (cons vip-s-string vip-search-history))))
  (vip-search vip-s-string vip-s-forward 1))

(defvar vip-exec-array (make-vector 128 nil))

;; Using a dispatch array allows adding functions like buffer search
;; without affecting other functions. Buffer search can now be bound
;; to any character.

(aset vip-exec-array ?c 'vip-exec-change)
(aset vip-exec-array ?C 'vip-exec-Change)
(aset vip-exec-array ?d 'vip-exec-delete)
(aset vip-exec-array ?D 'vip-exec-Delete)
(aset vip-exec-array ?y 'vip-exec-yank)
(aset vip-exec-array ?Y 'vip-exec-Yank)
(aset vip-exec-array ?! 'vip-exec-bang)
(aset vip-exec-array ?< 'vip-exec-shift)
(aset vip-exec-array ?> 'vip-exec-shift)
(aset vip-exec-array ?= 'vip-exec-equals)

;; the dispatcher for repeated commands

(defun vip-execute-com (m-com val com)
  "(M-COM VAL COM)  Execute command COM. The list (M-COM VAL COM) is set
to vip-d-com for later use by vip-repeat"
  (let ((reg vip-use-register))
    (if (> com 128)
	(vip-special-prefix-com (- com 128))
      (let ((fn (aref vip-exec-array (if (< com 0) (- com) com))))
	(if (null fn)
	    (error "%c: %s" com vip-ENOCMD)
	  (funcall fn com))))
    (if (vip-dotable-command-p com)
	(setq vip-d-com 
	      (list m-com val (if (or (= com ?c) (= com ?C) (= com ?!))
				  (- com) com)
		    reg)))))

(defun vip-repeat (arg)
  "(ARG)  Re-excute last destructive command.  vip-d-com has the form
(COM ARG CH REG), where COM is the command to be re-executed, ARG is the
argument for COM, CH is a flag for repeat, and REG is optional and if exists
is the name of the register for COM."
  (interactive "P")
  (if (eq last-command 'vip-undo)
      ;; if the last command was vip-undo, then undo-more
      (vip-undo-more)
    ;; otherwise execute the command stored in vip-d-com.  if arg is non-nil
    ;; its prefix value is used as new prefix value for the command.
    (let ((m-com (car vip-d-com))
	  (val (vip-P-val arg))
	  (com (car (cdr (cdr vip-d-com))))
	  (reg (nth 3 vip-d-com)))
      (if (null val) (setq val (car (cdr vip-d-com))))
      (if (null m-com) (error "No previous command to repeat."))
      (setq vip-use-register reg)
      (funcall m-com (cons val com)))))

(defun vip-special-prefix-com (char)
  "This command is invoked interactively by the key sequence #<char>"
  (cond ((= char ?c)
	 (downcase-region (min vip-com-point (point))
			  (max vip-com-point (point))))
	((= char ?C)
	 (upcase-region (min vip-com-point (point))
			(max vip-com-point (point))))
	((= char ?g)
	 (set-mark vip-com-point)
	 (vip-global-execute))
	((= char ?q)
	 (set-mark vip-com-point)
	 (vip-quote-region))
	((= char ?s) (spell-region vip-com-point (point)))
	(t (error "#%c: %s" char vip-ENOCMD))))


;; undoing

(defun vip-undo ()
  "Undo previous change."
  (interactive)
  (message "undo!")
  (let ((modified (buffer-modified-p)))
    (undo-start)
    (undo-more 2)
    (if (and (eolp) (not (bolp))) (backward-char 1))
    (if (not modified) (set-buffer-modified-p t)))
  (setq this-command 'vip-undo))

(defun vip-undo-more ()
  "Continue undoing previous changes."
  (message "undo more!")
  (undo-more 1)
  (if (and (eolp) (not (bolp))) (backward-char 1))
  (setq this-command 'vip-undo))


;; utilities

(defun vip-string-tail (str)
  (if (or (null str) (string= str "")) nil
    (substring str 1)))

(defun vip-yank-defun ()
  (mark-defun)
  (copy-region-as-kill (point) (mark t)))

(defun vip-enlarge-region (beg end)
  "Enlarge region between BEG and END."
  (setq zmacs-regions nil)
  (if (< beg end)
      (progn (goto-char beg) (set-mark end))
    (goto-char end)
    (set-mark beg))
  (beginning-of-line)
  (exchange-point-and-mark)
  (if (or (not (eobp)) (not (bolp))) (next-line 1))
  (beginning-of-line)
  (if (> beg end) (exchange-point-and-mark))
  (setq zmacs-regions t)
)

(defun vip-global-execute ()
  "Call last keyboad macro for each line in the region."
  (if (> (point) (mark t)) (exchange-point-and-mark))
  (beginning-of-line)
  (call-last-kbd-macro)
  (while (< (point) (mark t))
    (forward-line 1)
    (beginning-of-line)
    (call-last-kbd-macro)))

(defun vip-quote-region ()
  "Quote region by inserting the user supplied string at the beginning of
each line in the region."
  (setq vip-quote-string
	(let ((str
	       (vip-read-string "quote-string:" vip-quote-string)))
	  (if (string= str "") vip-quote-string str)))
  (vip-enlarge-region (point) (mark t))
  (if (> (point) (mark t)) (exchange-point-and-mark))
  (insert vip-quote-string)
  (beginning-of-line)
  (forward-line 1)
  (while (and (< (point) (mark t)) (bolp))
    (insert vip-quote-string)
    (beginning-of-line)
    (forward-line 1)))

(defun vip-end-with-a-newline-p (string)
  "Check if the string ends with a newline."
  (or (string= string "")
      (= (aref string (1- (length string))) ?\n)))

;; minibuffer i/o

(setq vip-minibuf-map (make-sparse-keymap))

(define-key vip-minibuf-map "\C-m" 'exit-minibuffer)
(define-key vip-minibuf-map "\C-j" 'exit-minibuffer)
(define-key vip-minibuf-map "\C-g" 'abort-recursive-edit)

(define-key vip-minibuf-map "\C-c" 'abort-recursive-edit)
(define-key vip-minibuf-map "\C-h" 'delete-backward-char)
(define-key vip-minibuf-map "\C-w" 'vip-delete-backward-word)
(define-key vip-minibuf-map "\C-u" 'vip-erase-line)
(define-key vip-minibuf-map "\C-v" 'quoted-insert)
(define-key vip-minibuf-map vip-ESC-key 'exit-minibuffer)

(defun vip-read-string (prompt &optional init history-var)
  (if vip-want-history
      (read-with-history-in
       (if history-var history-var 'vip-history) prompt init)
    (read-from-minibuffer prompt init vip-minibuf-map)))

;; completion support

(defun vip-read-string-complete ()
  (interactive)
  (completer-complete-goto
    "^ \t\n\""
    completer-words
    'read-file-name-internal
    default-directory))

(defun vip-read-string-help ()
  (interactive)
  (completer-help))
    
(defun vip-do-completion()
  "Do filename completion for :e commands"
  (require 'completer)
  (define-key vip-minibuf-map "\t" 'vip-read-string-complete)
  (define-key vip-minibuf-map "?" 'vip-read-string-help)
  (setq vip-filename-complete t))

;; history

(defun vip-get-history()
  (if (fboundp 'gmhist-define-keys)
      (message "WARNING: gmhist already loaded - cannot change mappings")
    (fset 'gmhist-define-keys 
	  (function 
	   (lambda (map)
	     "Bind the standard history commands in MAP, a key map."
	     
	     (define-key map "\C-p" 'gmhist-previous)
	     (define-key map "\C-n" 'gmhist-next)
	     (define-key map "\C-r" 'gmhist-search-backward)
	     (define-key map "\C-s" 'gmhist-search-forward)
	     (define-key map "\C-t" 'gmhist-beginning)
	     (define-key map "\C-l" 'gmhist-end)
	     (define-key map "\C-o" 'gmhist-show)
	     
	     (define-key map "\C-c" 'abort-recursive-edit)
	     (define-key map "\C-h" 'delete-backward-char)
	     (define-key map "\C-w" 'vip-delete-backward-word)
	     (define-key map "\C-u" 'vip-erase-line)
	     (define-key map "\C-v" 'quoted-insert)
	     
	     (if vip-filename-complete
		 (progn
		   (define-key map "\t" 'vip-read-string-complete)
		   (define-key map "?" 'vip-read-string-help)))
	     
	     (define-key map vip-ESC-key 'exit-minibuffer)))))
  
  (require 'gmhist)
  (setq vip-want-history t)
  )


;; insertion commands

(defun vip-repeat-insert-command ()
  "This function is called when mode changes from insertion mode to
vi command mode.  It will repeat the insertion command if original insertion
command was invoked with argument > 1."
  (let ((i-com (car vip-d-com)) (val (car (cdr vip-d-com))))
    (if (and val (> val 1)) ;; first check that val is non-nil
	(progn        
	  (setq vip-d-com (list i-com (1- val) ?r))
	  (vip-repeat nil)
	  (setq vip-d-com (list i-com val ?r))))))

(defun vip-insert (arg) ""
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-insert val ?r))
    (if com (vip-loop val (yank))
      (vip-change-mode-to-insert))))

(defun vip-append (arg)
  "Append after point."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-append val ?r))
    (if (not (eolp)) (forward-char))
    (if (equal com ?r)
	(vip-loop val (yank))
      (vip-change-mode-to-insert))))

(defun vip-Append (arg)
  "Append at end of line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-Append val ?r))
    (end-of-line)
    (if (equal com ?r)
	(vip-loop val (yank))
      (vip-change-mode-to-insert))))

(defun vip-Insert (arg)
  "Insert before first non-white."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-Insert val ?r))
    (back-to-indentation)
    (if (equal com ?r)
	(vip-loop val (yank))
      (vip-change-mode-to-insert))))

(defun vip-open-line (arg)
  "Open line below."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-open-line val ?r))
    (let ((col (current-indentation)))
      (if (equal com ?r)
	  (vip-loop val
		    (progn
		      (end-of-line)
		      (newline 1)
		      (if vip-auto-indent 
			  (progn (setq vip-cted t) (indent-to col)))
		      (yank)))
	(end-of-line)
	(newline 1)
	(if vip-auto-indent (progn (setq vip-cted t) (indent-to col)))
	(vip-change-mode-to-insert)))))

(defun vip-Open-line (arg)
  "Open line above."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-Open-line val ?r))
    (let ((col (current-indentation)))
      (if (equal com ?r)
	  (vip-loop val
		    (progn
		      (beginning-of-line)
		      (open-line 1)
		      (if vip-auto-indent 
			  (progn (setq vip-cted t) (indent-to col)))
		      (yank)))
	(beginning-of-line)
	(open-line 1)
	(if vip-auto-indent (progn (setq vip-cted t) (indent-to col)))
	(vip-change-mode-to-insert)))))

(defun vip-open-line-at-point (arg)
  "Open line at point."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-open-line-at-point val ?r))
    (if (equal com ?r)
	(vip-loop val
		  (progn
		    (open-line 1)
		    (yank)))
      (open-line 1)
      (vip-change-mode-to-insert))))

(defun vip-substitute (arg)
  "Substitute characters."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
      (set-mark (point))
      (forward-char val)
      (if (equal com ?r)
	  (vip-change-subr (mark t) (point))
	(vip-change (mark t) (point)))
    (setq vip-d-com (list 'vip-substitute val ?r))))

(defun vip-substitute-line (arg)
  "Substitute lines."
  (interactive "p")
  (vip-line (cons arg ?C)))

(defun vip-overwrite-execute (&optional arg exclusive limit)
  (interactive)
  (let (com overrun (cont t))
    (if limit
	(if (> limit (point))
	    (set-mark limit)
	  (setq cont nil)))
    (while (and (not vip-need-to-exit-overwrite) cont)
      (setq com (vip-get-editor-command vip-insert-local-map global-map))
      (if (numberp com)
	  (progn
	    (if exclusive
		(progn
		  (if (eolp) (forward-char 1))
		  (insert (char-to-string com))
		  (delete-char 1))
	      (insert (char-to-string com))
	      (if (not overrun)
		  (if (not (eolp))
		      (delete-char 1)
		    (setq overrun t))))
	    (if (and limit (= (point) (mark t)))
		(progn
		  (setq cont nil)
		  (vip-change-mode-line "Insert")
		  (vip-refresh-mode-line))))
	(command-execute com arg)))))

(defun vip-change-mode-to-overwrite (&optional arg exclusive limit)
  "Change mode to overwrite mode until optional limit is reached.
Also implement actual overwrite mode, eventually overwriting multiple
lines if exclusive is set to true."
  (interactive)
  (setq vip-need-to-exit-overwrite nil)
  (vip-change-mode-to-insert)
  (vip-change-mode-line "Replac")
  (vip-refresh-mode-line)
  (let ((echo echo-keystrokes))
    (setq echo-keystrokes 0)
    (vip-overwrite-execute arg exclusive limit)
    (setq echo-keystrokes echo)))

(defun vip-overwrite (arg) ""
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (setq vip-d-com (list 'vip-overwrite val ?r))
    (if com
	(vip-loop val (yank))
      (setq last-command 'vip-overwrite)
      (vip-change-mode-to-overwrite arg))))


;; line command

(defun vip-line (arg)
  (let ((val (car arg)) (com (cdr arg)))
    (move-marker vip-com-point (point))
    (next-line (1- val))
    (vip-execute-com 'vip-line val com)))

(defun vip-yank-line (arg)
  "Yank ARG lines (in vi's sense)"
  (interactive "P")
  (let ((val (vip-p-val arg)))
    (vip-line (cons val ?Y))))


;; region command

(defun vip-region (arg)
  (interactive "P")
  (let ((val (vip-P-val arg))
	(com (vip-getcom arg)))
    (move-marker vip-com-point (point))
    (exchange-point-and-mark)
    (vip-execute-com 'vip-region val com)))

(defun vip-Region (arg)
  (interactive "P")
  (let ((val (vip-P-val arg))
	(com (vip-getCom arg)))
    (move-marker vip-com-point (point))
    (exchange-point-and-mark)
    (vip-execute-com 'vip-Region val com)))

(defun vip-replace-char (arg)
  "Replace the following ARG chars by the character read."
  (interactive "P")
  (if (and (eolp) (bolp)) (error "No character to replace"))
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)) (at-end nil))
    (if (eolp) (backward-char 1))
    (setq vip-d-com (list 'vip-replace-char val ?r))
    (vip-replace-char-subr (if (equal com ?r) vip-d-char (read-char)) val)))

(defun vip-replace-char-subr (char arg)
  (delete-char arg t)
  (setq vip-d-char char)
  (vip-loop (if (> arg 0) arg (- arg)) 
	    (if (eq char ?\C-m) (insert "\n") (insert char)))
  (backward-char arg))

(defun vip-replace-string ()
  "Replace string.  If you supply null string as the string to be replaced,
the query replace mode will toggle between string replace and regexp replace."
  (interactive)
  (let (str)
    (setq str (vip-read-string
	       (if vip-re-replace "Replace regexp: " "Replace string: ")))
    (if (string= str "")
	(progn
	  (setq vip-re-replace (not vip-re-replace))
	  (message (format "Replace mode changed to %s."
			   (if vip-re-replace "regexp replace"
			     "string replace"))))
      (if vip-re-replace
	  (replace-regexp
	   str
	   (vip-read-string (format "Replace regexp \"%s\" with: " str)))
	(replace-string
	 str
	 (vip-read-string (format "Replace \"%s\" with: " str)))))))


;; basic cursor movement.  j, k, l, h commands.

(defun vip-forward-char (arg)
  "Move point right ARG characters (left if ARG negative).On reaching end
of line, stop and signal error."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (if vip-ex-style-motion
	(progn
	  ;; the boundary condition check gets weird here because forward-char
	  ;; may be the parameter of a delete, and 'dl' works just like 'x' for
	  ;; the last char on a line, so we have to allow the forward motion
	  ;; before the 'vip-execute-com', but of course 'dl' doesn't work on 
	  ;; an empty line, so we have to recognize that condition before the
	  ;; 'vip-execute-com'
	  (if (and (eolp) (bolp)) (error "") (forward-char val))
	  (if com (vip-execute-com 'vip-forward-char val com))
	  (if (eolp) (progn (backward-char 1) (error ""))))
      (forward-char val)
      (if com (vip-execute-com 'vip-forward-char val com))))
) 
(defun vip-backward-char (arg)
  "Move point left ARG characters (right if ARG negative).  On reaching
beginning of line, stop and signal error."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (if vip-ex-style-motion
	(progn
	  (if (bolp) (error "") (backward-char val))
	  (if com (vip-execute-com 'vip-backward-char val com)))
      (backward-char val)
      (if com (vip-execute-com 'vip-backward-char val com)))))


;; word command
;; vip3.5 is remarkably inconsistent in its backward and forward word movements
;; there are also a couple of bugs in my vi. All these are hopefully
;; removed in the following. Inheriting from ex, vip does not like to
;; do deletions across lines, and makes a mess of them. The mess is
;; reproduced below - to understand the code, just bring up vi and test
;; its behavior. Generally speaking, words are formed from alpha's and
;; nonalphas - <sp>,\t\n are separators for word movement. When executed
;; with a destructive command, \n is usually left untouched for the
;; last word. It is a shame the emacs word routines cannot be use.
;; Things would be much faster. Must try again someday, perhaps using  
;; regexps. - Aamod

;; skip only one \n
(defun vip-skip-separators(forward)
  (if forward
      (progn
	(skip-chars-forward " \t")
	(if (looking-at "\n")
	    (progn
	      (forward-char)
	      (skip-chars-forward " \t"))))
    (skip-chars-backward " \t")
    (backward-char)
    (if (looking-at "\n")
	(skip-chars-backward " \t")
      (forward-char))))

(defconst vip-ALPHA            "a-zA-Z0-9_")
(defconst vip-ALPHA-B          (concat "[" vip-ALPHA "]"))
(defconst vip-NONALPHA         (concat "^" vip-ALPHA))
(defconst vip-NONALPHA-B       (concat "[" vip-NONALPHA "]"))
(defconst vip-SEP               " \t\n")
(defconst vip-NONSEP           (concat "^" vip-SEP))
(defconst vip-SEP-B            (concat "[" vip-SEP "]"))
(defconst vip-ALPHASEP         (concat vip-ALPHA vip-SEP))
(defconst vip-ALPHASEP-B       (concat "[" vip-ALPHASEP "]"))
(defconst vip-NONALPHASEP      (concat "^" vip-ALPHASEP ))
(defconst vip-NONALPHASEP-B    (concat "[" vip-NONALPHASEP "]"))

(defun vip-forward-word-kernel(val)
  (while (> val 0)
    (cond ((looking-at vip-ALPHA-B)
	   (skip-chars-forward vip-ALPHA)
	   (vip-skip-separators t))
	  ((looking-at vip-SEP-B)
	   (vip-skip-separators t))
	  ((looking-at vip-NONALPHASEP-B)
	   (skip-chars-forward vip-NONALPHASEP)
	   (vip-skip-separators t)))
    (setq val (1- val))))
    
;;;;
;;;; new stuff got too fancy -- doesn't work for "cw" at eol and 
;;;; when word has "\t " after it
;;;;
;;;; don't have time to figure it out.. just used vers 4.3 routines
;;;; which work
;;;; -- BC 11/5/93
;;;;
;(defun vip-fwd-skip(pat lim)
;  (if (and (save-excursion 
;	     (re-search-backward pat lim t))
;	   (= (point) (match-end 0)))
;      (goto-char (match-beginning 0))))

;(defun vip-forward-word (arg)
;  "Forward word."
;  (interactive "P")
;  (let ((val (vip-p-val arg))
;	(com (vip-getcom arg)))
;    (if com (move-marker vip-com-point (point)))
;    (vip-forward-word-kernel val)
;    (if com (progn
;	      (cond ((or (= com ?c) (= com (- ?c)))
;		     (vip-fwd-skip "\n[ \t]*\\|[ \t]+" vip-com-point))
;		    ((vip-dotable-command-p com)
;		     (vip-fwd-skip "\n[ \t]*" vip-com-point)))
;	      (vip-execute-com 'vip-forward-word val com)))))

;(defun vip-forward-Word (arg)
;  "Forward word delimited by white character."
;  (interactive "P")
;  (let ((val (vip-p-val arg))
;	(com (vip-getcom arg)))
;    (if com (move-marker vip-com-point (point)))
;    (vip-loop val
;	      (progn
;		(skip-chars-forward vip-NONSEP)
;		(vip-skip-separators t)))
;    (if com (progn
;	      (cond ((or (= com ?c) (= com (- ?c)))
;		     (vip-fwd-skip "\n[ \t]*\\|[ \t]+" vip-com-point))
;		    ((vip-dotable-command-p com)
;		     (vip-fwd-skip "\n[ \t]*" vip-com-point)))
;	      (vip-execute-com 'vip-forward-word val com)))))
;;;;
;;;; from vers 4.3
;;;;
(defun vip-forward-word (arg)
  "Forward word."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (vip-forward-word-kernel val)
    (if com
	(progn
	  (cond ((or (= com ?c) (= com (- ?c)))
		 (skip-chars-backward " \t\n"))
		((or (= com ?y) (= com (- ?y)))
		 (skip-chars-backward "\n"))
		((or (= com ?d) (= com (- ?d)))
		 (skip-chars-backward "\n")))
	  (vip-execute-com 'vip-forward-word val com)))))
 
(defun vip-forward-Word (arg)
  "Forward word delimited by white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (vip-loop val
	      (progn
		(skip-chars-forward "^ \t\n")
		(vip-skip-separators t)))
    (if com 
	(progn
	  (cond ((or (= com ?c) (= com (- ?c)))
		 (skip-chars-backward " \t\n"))
	  	((or (= com ?y) (= com (- ?y)))
		 (skip-chars-backward "\n"))
		((or (= com ?d) (= com (- ?d)))
		 (skip-chars-backward "\n")))
	  (vip-execute-com 'vip-forward-Word val com)))))
;;;;
;;;; end of ver 4.3 routines
;;;;
 
;; end of word is a mess probably because I tried to reuse
;; forward word -- someday it should be rewritten. 

(defun vip-end-of-word-kernel(val)
  (let ((was-sep (looking-at vip-SEP-B))
	(next-sep (save-excursion (forward-char) 
				  (looking-at vip-SEP-B))))
    (if (vip-end-of-word-p) (forward-char))
    (if (looking-at "[ \t]*\n")
	(skip-chars-forward vip-SEP)
      (if (vip-one-char-word-p) nil
	(vip-forward-word-kernel 1)
	(if (or was-sep (and next-sep (eq val 1)))
	    (vip-forward-word-kernel 1))
	(vip-skip-separators nil)
	(backward-char)))))

(defun vip-end-of-word-p()
  (if (eobp) t
    (save-excursion
      (cond ((looking-at vip-ALPHA-B)
	     (forward-char)
	     (looking-at vip-NONALPHA-B))
	    ((looking-at vip-NONALPHASEP-B)
	     (forward-char)
	     (looking-at vip-ALPHASEP-B))))))

(defun vip-one-char-word-p()
  (let ((step 2))
    (save-excursion
      (cond ((looking-at vip-ALPHA-B)
	     (if (bobp) (setq step 1) (backward-char))
	     (if (or (bobp) (looking-at vip-NONALPHA-B))
		 (progn
		   (forward-char step)
		   (looking-at vip-NONALPHA-B))
	       nil))
	    ((looking-at vip-NONALPHASEP-B)
	     (if (bobp) (setq step 1) (backward-char))
	     (if (or (bobp) (looking-at vip-ALPHASEP-B))
		 (progn
		   (forward-char step)
		   (looking-at vip-ALPHASEP-B))
	       nil))))))

(defun vip-end-of-word (arg)
  "Move point to end of current word."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (vip-loop val (vip-end-of-word-kernel 1))
    (if com 
	(progn
	  (forward-char)
	  (vip-execute-com 'vip-end-of-word val com)))))

(defun vip-end-of-Word (arg)
  "Forward to end of word delimited by white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (vip-loop val
	(progn
	  (vip-end-of-word-kernel 1)
	  (if (not (re-search-forward 
		    vip-SEP-B nil t 1))
	      (goto-char (point-max)))
	  (skip-chars-backward vip-SEP)
	  (backward-char)))
    (if com 
	(progn
	  (forward-char)
	  (vip-execute-com 'vip-end-of-Word val com)))))

(defun vip-backward-word-kernel(val)
  (while (> val 0)
    (backward-char)
    (cond ((looking-at vip-ALPHA-B)
	   (skip-chars-backward vip-ALPHA))
	  ((looking-at vip-SEP-B)
	   (forward-char)
	   (vip-skip-separators nil)
	   (backward-char)
	   (cond ((looking-at vip-ALPHA-B)
		  (skip-chars-backward vip-ALPHA))
		 ((looking-at vip-NONALPHASEP-B)
		  (skip-chars-backward vip-NONALPHASEP))
		 (t (forward-char))))
	  ((looking-at vip-NONALPHASEP-B)
	   (skip-chars-backward vip-NONALPHASEP)))
    (setq val (1- val))))

(defun vip-backward-word (arg)
  "Backward word."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com
	(let (i)
	  (if (setq i (save-excursion (backward-char) (looking-at "\n")))
	      (backward-char))
	  (move-marker vip-com-point (point))
	  (if i (forward-char))))
    (vip-backward-word-kernel val)
    (if com (vip-execute-com 'vip-backward-word val com))))

(defun vip-backward-Word (arg)
  "Backward word delimited by white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com
	(let (i)
	  (if (setq i (save-excursion (backward-char) (looking-at "\n")))
	      (backward-char))
	  (move-marker vip-com-point (point))
	  (if i (forward-char))))
    (vip-loop val
	      (progn 
		(vip-skip-separators nil)
		(skip-chars-backward vip-NONSEP)))
    (if com (vip-execute-com 'vip-backward-Word val com))))


;; line commands

(defun vip-beginning-of-line (arg)
  "Go to beginning of line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (beginning-of-line val)
    (if com (vip-execute-com 'vip-beginning-of-line val com))))

(defun vip-bol-and-skip-white (arg)
  "Beginning of line at first non-white character."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (forward-to-indentation (1- val))
    (if com (vip-execute-com 'vip-bol-and-skip-white val com))))

(defun vip-goto-eol (arg)
  "Go to end of line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (end-of-line val)
    (if com (vip-execute-com 'vip-goto-eol val com)
      (if vip-ex-style-motion
	  (if (and (eolp) (not (bolp))) (backward-char 1))))))

(defun vip-next-line (arg)
  "Go to next line."
  (interactive "P")
  (save-excursion
    (end-of-line)
    (if (eobp) (error "Last line in buffer")))
  (let ((val (vip-p-val arg)) (com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (next-line val)
    (if vip-ex-style-motion
	(if (and (eolp) (not (bolp))) (backward-char 1)))
    (setq this-command 'next-line)
    (if com (vip-execute-com 'vip-next-line val com))))

(defun vip-next-line-at-bol (arg)
  "Next line at beginning of line."
  (interactive "P")
  (save-excursion
    (end-of-line)
    (if (eobp) (error "Last line in buffer")))
  (let ((val (vip-p-val arg)) (com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (next-line val)
    (back-to-indentation)
    (if com (vip-execute-com 'vip-next-line-at-bol val com))))

(defun vip-previous-line (arg)
  "Go to previous line."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (if (bobp) (error "First line in buffer")))
  (let ((val (vip-p-val arg)) (com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (next-line (- val))
    (if vip-ex-style-motion
	(if (and (eolp) (not (bolp))) (backward-char 1)))
    (setq this-command 'previous-line)
    (if com (vip-execute-com 'vip-previous-line val com))))

(defun vip-previous-line-at-bol (arg)
  "Previous line at beginning of line."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (if (bobp) (error "First line in buffer")))
  (let ((val (vip-p-val arg)) (com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (next-line (- val))
    (back-to-indentation)
    (if com (vip-execute-com 'vip-previous-line val com))))

(defun vip-change-to-eol (arg)
  "Change to end of line."
  (interactive "P")
  (vip-goto-eol (cons arg ?c)))

(defun vip-kill-line (arg)
  "Delete line."
  (interactive "P")
  (let ((val (vip-p-val arg)))
    (vip-goto-eol (cons val ?d))))

(defun vip-erase-line (arg)
  "Erase line."
  (interactive "P")
  (vip-beginning-of-line (cons arg ?d)))


;; moving around

(defun vip-goto-line (arg)
  "Go to ARG's line.  Without ARG go to end of buffer."
  (interactive "P")
  (let ((val (vip-P-val arg)) (com (vip-getCom arg)))
    (move-marker vip-com-point (point))
    (set-mark (point))
    (if (null val)
	(goto-char (point-max))
      (goto-char (point-min))
      (forward-line (1- val)))
    (back-to-indentation)
    (if com (vip-execute-com 'vip-goto-line val com))))

(defun vip-find-char (arg char forward offset)
  "Find ARG's occurence of CHAR on the current line.  If FORWARD then
search is forward, otherwise backward.  OFFSET is used to adjust point
after search."
  (let ((arg (if forward arg (- arg))) point)
    (save-excursion
      (save-restriction
	(if (> arg 0)
	    (narrow-to-region
	     ;; forward search begins here
	     (if (eolp) (error "") (point))
	     ;; forward search ends here
	     (progn (next-line 1) (beginning-of-line) (point)))
	  (narrow-to-region
	   ;; backward search begins from here
	   (if (bolp) (error "") (point))
	   ;; backward search ends here
	   (progn (beginning-of-line) (point))))
	;; if arg > 0, point is forwarded before search.
	(if (> arg 0) (goto-char (1+ (point-min)))
	  (goto-char (point-max)))
	(let ((case-fold-search nil))
	  (if (not (search-forward (char-to-string char) nil 0 arg))
	      (if (or (and (> arg 0) (eobp))
		      (and (< arg 0) (bobp)))
		  (error ""))))
	(setq point (point))))
    (goto-char (+ point (if (> arg 0) (if offset -2 -1) (if offset 1 0))))))

(defun vip-find-char-forward (arg)
  "Find char on the line.  If called interactively read the char to find
from the terminal, and if called from vip-repeat, the char last used is
used.  This behaviour is controlled by the sign of prefix numeric value."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq vip-f-char (read-char)
	      vip-f-forward t
	      vip-f-offset nil)
      (setq val (- val)))
    (if com (move-marker vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) t nil)
    (setq val (- val))
    (if com
	(progn
	  (setq vip-F-char vip-f-char);; set new vip-F-char
	  (forward-char)
	  (vip-execute-com 'vip-find-char-forward val com)))))

(defun vip-goto-char-forward (arg)
  "Go up to char ARG forward on line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq vip-f-char (read-char)
	      vip-f-forward t
	      vip-f-offset t)
      (setq val (- val)))
    (if com (move-marker vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) t t)
    (setq val (- val))
    (if com
	(progn
	  (setq vip-F-char vip-f-char);; set new vip-F-char
	  (forward-char)
	  (vip-execute-com 'vip-goto-char-forward val com)))))

(defun vip-find-char-backward (arg)
  "Find char ARG on line backward."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq vip-f-char (read-char)
	      vip-f-forward nil
	      vip-f-offset nil)
      (setq val (- val)))
    (if com (move-marker vip-com-point (point)))
    (vip-find-char
     val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) nil nil)
    (setq val (- val))
    (if com
	(progn
	  (setq vip-F-char vip-f-char);; set new vip-F-char
	  (vip-execute-com 'vip-find-char-backward val com)))))

(defun vip-goto-char-backward (arg)
  "Go up to char ARG backward on line."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq vip-f-char (read-char)
	      vip-f-forward nil
	      vip-f-offset t)
      (setq val (- val)))
    (if com (move-marker vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) nil t)
    (setq val (- val))
    (if com
	(progn
	  (setq vip-F-char vip-f-char);; set new vip-F-char
	  (vip-execute-com 'vip-goto-char-backward val com)))))

(defun vip-repeat-find (arg)
  "Repeat previous find command."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (vip-find-char val vip-f-char vip-f-forward vip-f-offset)
    (if com
	(progn
	  (if vip-f-forward (forward-char))
	  (vip-execute-com 'vip-repeat-find val com)))))

(defun vip-repeat-find-opposite (arg)
  "Repeat previous find command in the opposite direction."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (vip-find-char val vip-f-char (not vip-f-forward) vip-f-offset)
    (if com
	(progn
	  (if vip-f-forward (forward-char))
	  (vip-execute-com 'vip-repeat-find-opposite val com)))))


;; window scrolling etc.

(defun vip-other-window (arg)
  "Switch to other window."
  (interactive "p")
  (other-window arg)
  (or (not (eq vip-current-mode 'emacs-mode))
      (string= (buffer-name (current-buffer)) " *Minibuf-1*")
      (vip-change-mode-to-vi)))

(defun vip-window-top (arg)
  "Go to home window line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    ;(if com (move-marker vip-com-point (poinp-com-point (point)))
    (if com (move-marker vip-com-point (point))
    (move-to-window-line (1- val))
    (if (not com) (back-to-indentation))
    (if com (vip-execute-com 'vip-window-top val com)))))

(defun vip-window-middle (arg)
  "Go to middle window line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (move-to-window-line (+ (/ (1- (window-height)) 2) (1- val)))
    (if (not com) (back-to-indentation))
    (if com (vip-execute-com 'vip-window-middle val com))))

(defun vip-window-bottom (arg)
  "Go to last window line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (move-to-window-line (- val))
    (if (not com) (back-to-indentation))
    (if com (vip-execute-com 'vip-window-bottom val com))))

(defun vip-line-to-top (arg)
  "Put current line on the home line."
  (interactive "p")
  (recenter (1- arg)))

(defun vip-line-to-middle (arg)
  "Put current line on the middle line."
  (interactive "p")
  (recenter (+ (1- arg) (/ (1- (window-height)) 2))))

(defun vip-line-to-bottom (arg)
  "Put current line on the last line."
  (interactive "p")
  (recenter (- (window-height) (1+ arg))))


;; paren match
;; must correct this to only match ( to ) etc. On the other hand
;; it is good that paren match gets confused, because that way you
;; catch _all_ imbalances. - Aamod

(defun vip-paren-match (arg)
  "Go to the matching parenthesis."
  (interactive "P")
  (let ((com (vip-getcom arg)))
    (if (numberp arg)
	(if (or (> arg 99) (< arg 1))
	    (error "Prefix must be between 1 and 99.")
	  (goto-char
	   (if (> (point-max) 80000)
	       (* (/ (point-max) 100) arg)
	     (/ (* (point-max) arg) 100)))
	  (back-to-indentation))
      (let (lim)
	(if (and (eolp) (not (bolp))) (forward-char -1))
	(save-excursion
	  (end-of-line)
	  (setq lim (point)))
	(if (re-search-forward "[][(){}]" lim t) 
	    (backward-char) 
	  (error "No matchable character on line")))
      (cond ((looking-at "[\(\[{]")
	     (if com (move-marker vip-com-point (point)))
	     (forward-sexp 1)
	     (if com
		 (vip-execute-com 'vip-paren-match nil com)
	       (backward-char)))
	    ((looking-at "[])}]")
	     (forward-char)
	     (if com (move-marker vip-com-point (point)))
	     (backward-sexp 1)
	     (if com (vip-execute-com 'vip-paren-match nil com)))
	    (t (error ""))))))


;; sentence ,paragraph and heading

(defun vip-forward-sentence (arg)
  "Forward sentence."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (forward-sentence val)
    (if com (vip-execute-com 'vip-forward-sentence nil com))))

(defun vip-backward-sentence (arg)
  "Backward sentence."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (backward-sentence val)
    (if com (vip-execute-com 'vip-backward-sentence nil com))))

(defun vip-forward-paragraph (arg)
  "Forward paragraph."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (forward-paragraph val)
    (if com (vip-execute-com 'vip-forward-paragraph nil com))))

(defun vip-backward-paragraph (arg)
  "Backward paragraph."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (backward-paragraph val)
    (if com (vip-execute-com 'vip-backward-paragraph nil com))))

;; should be mode-specific etc.

(defun vip-prev-heading(arg)
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (move-marker vip-com-point (point)))
    (re-search-backward vip-heading-start nil t val)
    (goto-char (match-beginning 0))
    (if com (vip-execute-com 'vip-prev-heading nil com))))

(defun vip-heading-end(arg)
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (move-marker vip-com-point (point)))
    (re-search-forward vip-heading-end nil t val)
    (goto-char (match-beginning 0))
    (if com (vip-execute-com 'vip-heading-end nil com))))

(defun vip-next-heading(arg)
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com
	(progn				; do heading end like VI
	  (if (eq com ?d) (setq com ?D)); for deletion enlarge region
	  (move-marker vip-com-point (point))
	  (re-search-forward vip-heading-end nil t val)
	  (vip-execute-com 'vip-next-heading nil com))
      (end-of-line)
      (re-search-forward vip-heading-start nil t val)
      (goto-char (match-beginning 0)))))


;; scrolling

(defun vip-scroll (arg)
  "Scroll to next screen."
  (interactive "p")
  (if (> arg 0)
      (while (> arg 0)
	(scroll-up)
	(setq arg (1- arg)))
    (while (> 0 arg)
      (scroll-down)
      (setq arg (1+ arg)))))

(defun vip-scroll-back (arg)
  "Scroll to previous screen."
  (interactive "p")
  (vip-scroll (- arg)))

(defun vip-scroll-down (arg)
  "Scroll up half screen."
  (interactive "P")
  (if (null arg) (scroll-down (/ (window-height) 2))
    (scroll-down arg)))

(defun vip-scroll-down-one (arg)
  "Scroll up one line."
  (interactive "p")
  (scroll-down arg))

(defun vip-scroll-up (arg)
  "Scroll down half screen."
  (interactive "P")
  (if (null arg) (scroll-up (/ (window-height) 2))
    (scroll-up arg)))

(defun vip-scroll-up-one (arg)
  "Scroll down one line."
  (interactive "p")
  (scroll-up arg))

;; actually, emacs usually scrolls too slowly for this, but I still remember
;; my surprise when emacs scrolled by half a page...

(setq scroll-step 1)


;; splitting window

(defun vip-buffer-in-two-windows ()
  "Show current buffer in two windows."
  (interactive)
  (delete-other-windows)
  (split-window-vertically nil))


;; searching

(defun vip-if-string(prompt)
  (let ((s (vip-read-string prompt "" 'vip-search-history)))
    (if (not (string= s ""))
	(setq vip-s-string s))))

(defun vip-search-forward (arg)
  "Search a string forward.  ARG is used to find the ARG's occurence
of the string.  Null string will repeat previous search"
  (interactive "P")
  (let ((val (vip-P-val arg)) (com (vip-getcom arg)))
    (setq vip-s-forward t)
    (vip-if-string "/")
    (vip-search vip-s-string t val)
    (if com
	(progn
	  (move-marker vip-com-point (mark t))
	  (vip-execute-com 'vip-search-next val com)))))

(defun vip-search-backward (arg)
  "Search a string backward.  ARG is used to find the ARG's occurence
of the string.  Null string will repeat previous search"
  (interactive "P")
  (let ((val (vip-P-val arg)) (com (vip-getcom arg)))
    (setq vip-s-forward nil)
    (vip-if-string "?")
    (vip-search vip-s-string nil val)
    (if com
	(progn
	  (move-marker vip-com-point (mark t))
	  (vip-execute-com 'vip-search-next val com)))))

(defun vip-search (string forward arg &optional no-offset init-point)
  "(STRING FORWARD COUNT &optional NO-OFFSET) Search COUNT's occurrence of
STRING.  Search will be forward if FORWARD, otherwise backward."
  (let ((val (vip-p-val arg)) (com (vip-getcom arg))
	(null-arg (null (vip-P-val arg))) (offset (not no-offset))
	(case-fold-search vip-case-fold-search)
	(start-point (or init-point (point))))
    (if forward
	(condition-case conditions
	    (progn
	      (if (and offset (not (eobp))) (forward-char))
	      (if vip-re-search
		  (progn
		    (re-search-forward string nil nil val)
		    (re-search-backward string))
		(search-forward string nil nil val)
		(search-backward string))
	      (push-mark start-point))
	  (search-failed
	   (if (and null-arg vip-search-wrap-around-t)
	       (progn
		 (goto-char (point-min))
		 (vip-search string forward (cons 1 com) t start-point))
	     (goto-char start-point)
	     (signal 'search-failed (cdr conditions)))))
      (condition-case conditions
	  (progn
	    (if vip-re-search
		(re-search-backward string nil nil val)
	      (search-backward string nil nil val))
	    (push-mark start-point))
	(search-failed
	 (if (and null-arg vip-search-wrap-around-t)
	     (progn
	       (goto-char (point-max))
	       (vip-search string forward (cons 1 com) t start-point))
	   (goto-char start-point)
	   (signal 'search-failed (cdr conditions))))))))

(defun vip-search-next (arg)
  "Repeat previous search."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if (null vip-s-string) (error "No previous search string."))
    (vip-search vip-s-string vip-s-forward arg)
    (if com
	(progn
	  (move-marker vip-com-point (mark t))
	  (vip-execute-com 'vip-search-next val com)))))

(defun vip-search-Next (arg)
  "Repeat previous search in the reverse direction."
  (interactive "P")
  (let ((val (vip-p-val arg)) (com (vip-getcom arg)))
    (if (null vip-s-string) (error "No previous search string."))
    (vip-search vip-s-string (not vip-s-forward) arg)
    (if com
	(progn
	  (move-marker vip-com-point (mark t))
	  (vip-execute-com 'vip-search-Next val com)))))

;; search contents of buffer defined by one of vip's motion commands
;; repeatable via n and N

(setq vip-buffer-search-char nil)

(defun vip-buffer-search-enable(&optional c)
  (if (null vip-buffer-search-char)
      (progn
	(if (null c) (setq c ?g))
	(setq vip-buffer-search-char c)
	(define-key vip-mode-map (char-to-string vip-buffer-search-char)
	  'vip-command-argument)
	(aset vip-exec-array c 'vip-exec-buffer-search)
	(setq vip-prefix-commands (cons c vip-prefix-commands)))))


;; visiting and killing files, buffers

(defun vip-switch-to-buffer ()
  "Switch to buffer in the current window."
  (interactive)
  (let (buffer)
    (setq buffer
	  (read-buffer
	   (format "switch to buffer \(%s\): "
		   (buffer-name (other-buffer (current-buffer))))))
    (switch-to-buffer buffer)
    (vip-change-mode-to-vi)))

(defun vip-switch-to-buffer-other-window ()
  "Switch to buffer in another window."
  (interactive)
  (let (buffer)
    (setq buffer
	  (read-buffer
	   (format "Switch to buffer \(%s\): "
		   (buffer-name (other-buffer (current-buffer))))))
    (switch-to-buffer-other-window buffer)
    (vip-change-mode-to-vi)))

(defun vip-kill-buffer ()
  "Kill a buffer."
  (interactive)
  (let (buffer buffer-name)
    (setq buffer-name
	  (read-buffer
	   (format "Kill buffer \(%s\): "
		   (buffer-name (current-buffer)))))
    (setq buffer
	  (if (null buffer-name)
	      (current-buffer)
	    (get-buffer buffer-name)))
    (if (null buffer) (error "Buffer %s nonexistent." buffer-name))
    (if (or (not (buffer-modified-p buffer))
	    (y-or-n-p "Buffer is modified, are you sure? "))
	(kill-buffer buffer)
      (error "Buffer not killed."))))

(defun vip-find-file ()
  "Visit file in the current window."
  (interactive)
  (let (file)
    (setq file (read-file-name "visit file: "))
    (switch-to-buffer (find-file-noselect file))
    (if (not (equal major-mode 'dired-mode))
	(vip-change-mode-to-vi))))

(defun vip-find-file-other-window ()
  "Visit file in another window."
  (interactive)
  (let (file)
    (setq file (read-file-name "Visit file: "))
    (switch-to-buffer-other-window (find-file-noselect file))
    (if (not (equal major-mode 'dired-mode))
	(vip-change-mode-to-vi))))

(defun vip-find-file-other-screen ()
  "Visit file in another screen."
  (interactive)
  (let (file)
    (setq file (read-file-name "Visit file other screen: "))
    (switch-to-buffer-new-screen (find-file-noselect file))
    (if (not (equal major-mode 'dired-mode))
	(vip-change-mode-to-vi))))

(defun vip-info-on-file ()
  "Give information of the file associated to the current buffer."
  (interactive)
  (message "\"%s\"%s line %d of %d"
	   (if (buffer-file-name) (buffer-file-name) "")
	   (if (buffer-modified-p) " [Modified]" "")
	   (let ((l (count-lines (point-min) (point)))) 
	     (if (bolp) (1+ l) l))
	   (1+ (count-lines (point-min) (point-max)))))


;; yank and pop

(defun vip-yank (text)
  "yank TEXT silently."
  (save-excursion
    (vip-push-mark-silent (point))
    (insert text)
    (exchange-point-and-mark))
  (skip-chars-forward " \t"))

(defun vip-put-back (arg)
  "Put back after point/below line."
  (interactive "P")
  (setq zmacs-regions nil)
  (let ((val (vip-p-val arg))
	(text (if vip-use-register
		  (if (and (<= ?1 vip-use-register) (<= vip-use-register ?9))
		      (nth (- vip-use-register 49) kill-ring-yank-pointer)
		    (get-register (downcase vip-use-register)))
		(car kill-ring-yank-pointer))))
    (if (null text)
	(if vip-use-register
	    (let ((reg vip-use-register))
	      (setq vip-use-register nil)
	      (error "Nothing in register %c" reg))
	  (error "")))
    (setq vip-use-register nil)
    (if (vip-end-with-a-newline-p text)
	(progn
	  (next-line 1)
	  (beginning-of-line))
      (if (and (not (eolp)) (not (eobp))) (forward-char)))
    (setq vip-d-com (list 'vip-put-back val nil vip-use-register))
    (vip-loop val (vip-yank text)))
  (setq zmacs-regions t))

(defun vip-Put-back (arg)
  "Put back at point/above line."
  (interactive "P")
  (setq zmacs-regions nil)
  (let ((val (vip-p-val arg))
	(text (if vip-use-register
		  (if (and (<= ?1 vip-use-register) (<= vip-use-register ?9))
		      (nth (- vip-use-register 49) kill-ring-yank-pointer)
		    (get-register (downcase vip-use-register)))
		(car kill-ring-yank-pointer))))
    (if (null text)
	(if vip-use-register
	    (let ((reg vip-use-register))
	      (setq vip-use-register nil)
	      (error "Nothing in register %c" reg))
	  (error "")))
    (setq vip-use-register nil)
    (if (vip-end-with-a-newline-p text) (beginning-of-line))
    (setq vip-d-com (list 'vip-Put-back val nil vip-use-register))
    (vip-loop val (vip-yank text)))
  (setq zmacs-regions t))

(defun vip-delete-char (arg)
  "Delete character."
  (interactive "P")
  (let ((val (vip-p-val arg)))
    (setq vip-d-com (list 'vip-delete-char val nil))
    (save-excursion
      (let ((here (point)))
	(end-of-line)
	(if (> val (- (point) here))
	    (setq val (- (point) here)))))
    (if (eolp) (setq val -1))
    (if vip-use-register
	(progn
	  (if (and (<= ?A vip-use-register) (<= vip-use-register ?Z))
	      (vip-append-to-register
	       (+ vip-use-register 32) (point) (+ (point) val))
	    (copy-to-register vip-use-register (point) (+ (point) val) nil))
	  (setq vip-use-register nil)))
    (if vip-ex-style-motion
	(progn
	  (delete-char val t)
	  (if (and (eolp) (not (bolp))) (backward-char 1)))
      (delete-char val t))))

(defun vip-delete-backward-char (arg)
  "Delete previous character."
  (interactive "P")
  (let ((val (vip-p-val arg)))
    (setq vip-d-com (list 'vip-delete-backward-char val nil))
    (if (bolp) (ding)
      (save-excursion
	(let ((here (point)))
	  (beginning-of-line)
	  (if (> val (- here (point)))
	      (setq val (- here (point))))))
      (if vip-use-register
	  (progn
	    (if (and (<= ?A vip-use-register) (<= vip-use-register ?Z))
		(vip-append-to-register
		 (+ vip-use-register 32) (- (point) val) (point))
	      (copy-to-register vip-use-register (- (point) val) (point) nil))
	    (setq vip-use-register nil)))
      (delete-backward-char val t))))


;; join lines.

(defun vip-join-lines (arg)
  "Join this line to next, if ARG is nil.  Otherwise, join ARG lines"
  (interactive "*P")
  (let ((val (vip-P-val arg)))
    (setq vip-d-com (list 'vip-join-lines val nil))
    (vip-loop (if (null val) 1 (1- val))
	      (progn
		(end-of-line)
		(if (not (eobp))
		    (progn
		      (forward-line 1)
		      (delete-region (point) (1- (point)))
		      (fixup-whitespace)))))))


;; making small changes

(defun vip-finish-change ()
  (setq vip-c-string (buffer-substring vip-change-beg-point (point)))
  (if (and (= (count-lines vip-change-beg-point vip-change-end-point) 1)
	   (>= (point) vip-change-beg-point)
	   (< (point) vip-change-end-point))
      (kill-region (point) vip-change-end-point))
  (setq vip-need-to-finish-change nil))

(defun vip-change (beg end)
  (if vip-use-register
      (progn
        (copy-to-register vip-use-register beg end nil)
        (setq vip-use-register nil)))
  (if (<= beg end)
      (progn
	(move-marker vip-change-beg-point beg)
	(move-marker vip-change-end-point end))
    (move-marker vip-change-beg-point end)
    (move-marker vip-change-end-point beg))
  (setq vip-need-to-finish-change t)
  (if (= (count-lines vip-change-beg-point vip-change-end-point) 1)
      (progn
	(goto-char vip-change-end-point)
	(delete-backward-char 1)
	(insert "$")
	(setq vip-change-end-point (point-marker))
	(goto-char vip-change-beg-point)
	(vip-change-mode-to-overwrite nil nil vip-change-end-point))
    (kill-region vip-change-beg-point vip-change-end-point)
    (vip-change-mode-to-insert)))

(defun vip-change-subr (beg end)
  (if vip-use-register
      (progn
	(copy-to-register vip-use-register beg end nil)
	(setq vip-use-register nil)))
  (kill-region beg end)
  (setq this-command 'vip-change)
  (insert vip-c-string))

(defun vip-toggle-case (arg)
  "toggle character case."
  (interactive "P")
  (let ((val (vip-p-val arg)) (c))
    (setq vip-d-com (list 'vip-toggle-case val nil))
    (while (> val 0)
      (setq c (following-char))
      (delete-char 1 nil)
      (if (eq c (upcase c))
	  (insert-char (downcase c) 1)
	(insert-char (upcase c) 1))
      (setq val (1- val)))))


;; query replace

(defun vip-query-replace ()
  "Query replace.  If you supply null string as the string to be replaced,
the query replace mode will toggle between string replace and regexp replace."
  (interactive)
  (let (str)
    (setq str (vip-read-string
	       (if vip-re-query-replace "Query replace regexp: "
		 "Query replace: ")))
    (if (string= str "")
	(progn
	  (setq vip-re-query-replace (not vip-re-query-replace))
	  (message "Query replace mode changed to %s."
		   (if vip-re-query-replace "regexp replace"
		     "string replace")))
      (if vip-re-query-replace
	  (query-replace-regexp
	   str
	   (vip-read-string (format "Query replace regexp \"%s\" with: " str)))
	(query-replace
	 str
	 (vip-read-string (format "Query replace \"%s\" with: " str)))))))


;; marking

(defun vip-mark-beginning-of-buffer ()
  (interactive)
  (set-mark (point))
  (goto-char (point-min))
  (exchange-point-and-mark)
  (message "mark set at the beginning of buffer"))

(defun vip-mark-end-of-buffer ()
  (interactive)
  (set-mark (point))
  (goto-char (point-max))
  (exchange-point-and-mark)
  (message "mark set at the end of buffer"))

(defun vip-mark-point (char)
  (interactive "c")
  (cond ((and (<= ?a char) (<= char ?z))
	 (point-to-register (- char (- ?a ?\C-a))))
	((= char ?<) (vip-mark-beginning-of-buffer))
	((= char ?>) (vip-mark-end-of-buffer))
	((= char ?.) (push-mark))
	((= char ?,) (set-mark-command 1))
	((= char ?D) (mark-defun))
	(t (error ""))))

(defun vip-goto-mark (arg)
  "Go to mark."
  (interactive "P")
  (let ((char (read-char)) (com (vip-getcom arg)))
    (vip-goto-mark-subr char com nil)))

(defun vip-goto-mark-and-skip-white (arg)
  "Go to mark and skip to first non-white on line."
  (interactive "P")
  (let ((char (read-char)) (com (vip-getCom arg)))
    (vip-goto-mark-subr char com t)))

(defun vip-goto-mark-subr (char com skip-white)
  (cond ((and (<= ?a char) (<= char ?z))
	 (let ((buff (current-buffer)))
	   (if com (move-marker vip-com-point (point)))
	   (goto-char (register-to-point (- char (- ?a ?\C-a))))
	   (if skip-white (back-to-indentation))
	   (vip-change-mode-to-vi)
	   (if com
	       (if (equal buff (current-buffer))
		   (vip-execute-com (if skip-white
					'vip-goto-mark-and-skip-white
				      'vip-goto-mark)
				    nil com)
		 (switch-to-buffer buff)
		 (goto-char vip-com-point)
		 (vip-change-mode-to-vi)
		 (error "")))))
	((and (not skip-white) (= char ?`))
	 (if com (move-marker vip-com-point (point)))
	 (exchange-point-and-mark)
	 (if com (vip-execute-com 'vip-goto-mark nil com)))
	((and skip-white (= char ?'))
	 (if com (move-marker vip-com-point (point)))
	 (exchange-point-and-mark)
	 (back-to-indentation)
	 (if com (vip-execute-com 'vip-goto-mark-and-skip-white nil com)))
	(t (error ""))))

(defun vip-exchange-point-and-mark ()
  (interactive)
  (exchange-point-and-mark)
  (back-to-indentation))

;; Input Mode Indentation

(defun vip-forward-indent()
  "Indent forward - C-d in VI"
  (interactive)
  (setq vip-cted t)
  (indent-to (+ (current-column) vip-shift-width)))

(defun vip-backward-indent()
  "Backtab, C-t in VI"
  (interactive)
  (if vip-cted
      (let ((p (point)) (c (current-column)) bol (indent t))
	(if (vip-looking-back "[0^]")
	    (progn
	      (if (= ?^ (preceding-char)) (setq vip-preserve-indent t))
	      (delete-backward-char 1)
	      (setq p (point))
	      (setq indent nil)))
	(save-excursion
	  (beginning-of-line)
	  (setq bol (point)))
	(if (re-search-backward "[^ \t]" bol 1) (forward-char))
	(delete-region (point) p)
	(if indent
	    (indent-to (- c vip-shift-width)))
	(if (or (bolp) (vip-looking-back "[^ \t]"))
	    (setq vip-cted nil)))))

(defun vip-autoindent()
  "Auto Indentation, VI style"
  (interactive)
  (let ((col (current-indentation)))
    (if (not vip-preserve-indent)
	(setq vip-current-indent col)
      (setq vip-preserve-indent nil))
    (newline 1)
    (if vip-auto-indent
	(progn
	  (setq vip-cted t)
	  (indent-to vip-current-indent)))))

;; Keyboard macros in registers

(defun vip-register-macro(count)
  "Keyboard macros in registers - a modified \@ command"
  (interactive "P")
  (let ((reg (downcase (read-char))))
    (cond ((or (and (<= ?a reg) (<= reg ?z)))
	   (setq vip-last-macro-reg reg)
	   (if defining-kbd-macro
	       (progn
		 (end-kbd-macro)
		 (if (get-register reg)
		     (if (not (y-or-n-p "Register contains data. Overwrite?"))
			 (error "Keyboard macro defined but not written")))
		 (set-register reg last-kbd-macro))
	     (execute-kbd-macro (get-register reg) count)))
	  ((or (= ?@ reg) (= ?\^j reg) (= ?\^m reg))
	   (execute-kbd-macro (get-register vip-last-macro-reg) count))
	  ((= ?\# reg)
	   (start-kbd-macro count))
	  ((= ?! reg)
	   (setq reg (downcase (read-char)))
	   (if (or (and (<= ?a reg) (<= reg ?z)))
	       (progn
	       (setq vip-last-macro-reg reg)
		 (if (get-register reg)
		     (if (not (y-or-n-p "Register contains data. Overwrite?"))
			 (error "Keyboard macro defined but not written")))
	       (set-register reg last-kbd-macro))))
	  (t
	   (error (format "Unknown register %c" reg))))))
	   
;; Viewing registers

(defun vip-ket-function(arg)
  "Function called by \], the ket. View registers and call \]\]"
  (interactive "P")
  (let ((reg (read-char)))
    (cond ((and (<= ?a reg) (<= reg ?z))
	   (view-register reg))
	  ((and (<= ?1 reg) (<= reg ?9))
	   (let ((text (nth (- reg 49) kill-ring-yank-pointer)))
	     (if (null text)
		 (message "Register %c is Empty" reg)
	       (save-excursion 
		 (set-buffer (get-buffer-create "*Output*"))
		 (delete-region (point-min) (point-max))
		 (insert (format "Register %c contains the string:\n" reg))
		 (insert text)
		 (goto-char (point-min)))
	       (display-buffer "*Output*"))))
	  ((= ?\] reg)
	   (vip-next-heading arg))
	  (t (error "Invalid Command %c%c" last-command-char reg)))))

(defun vip-brac-function(arg)
  "Function called by \[, the brac. View textmarkers and call \[\["
  (interactive "P")
  (let ((reg (read-char)))
    (cond ((= ?\[ reg)
	   (vip-prev-heading arg))
	  ((= ?\] reg)
	   (vip-heading-end arg))
	  ((and (<= ?a reg) (<= reg ?z))
	   (let* ((val (get-register (- reg (- ?a ?\C-a))))
		  (buf 
		   (if (not val) 
		       (error 
			(format "Textmarker %c does not point anywhere" reg)) 
		     (marker-buffer val))) 
		  (pos (marker-position val))
		  line-no text (s pos) (e pos))
	     (save-excursion 
	       (set-buffer (get-buffer-create "*Output*"))
	       (delete-region (point-min) (point-max))
	       (if (and buf pos)
		   (progn
		     (save-excursion 
		       (set-buffer buf)
		       (setq line-no (1+ (count-lines (point-min) val)))
		       (goto-char pos)
		       (beginning-of-line)
		       (if (re-search-backward "[^ \t]" nil t)
			   (progn
			     (beginning-of-line)
			     (setq s (point))))
		       (goto-char pos)
		       (forward-line 1)
		       (if (re-search-forward "[^ \t]" nil t)
			   (progn
			     (end-of-line)
			     (setq e (point))))
		       (setq text (buffer-substring s e))
		       (setq text (format "%s<%c>%s" 
					  (substring text 0 (- pos s)) 
					  reg (substring text (- pos s)))))
		     (insert (format "Textmarker %c is in buffer %s at line %d.\n"
				     reg (buffer-name buf) line-no))
		     (insert (format "Here is some text around %c:\n\n %s" 
				     reg text)))
		 (insert (format "Textmarker %c not set anywhere" reg)))
	       (goto-char (point-min)))
	     (display-buffer "*Output*")))
	  (t (error (format "Invalid Command %c%c" last-command-char reg))))))
  

;; Misc

(defun vip-keyboard-quit ()
  "Abort partially formed or running command."
  (interactive)
  (setq vip-use-register nil)
  (keyboard-quit))

(defun vip-ctl-c-equivalent (arg)
  "Emulate C-c in Emacs mode."
  (interactive "P")
  (vip-ctl-key-equivalent "\C-c" arg))

(defun vip-ctl-x-equivalent (arg)
  "Emulate C-x in Emacs mode."
  (interactive "P")
  (vip-ctl-key-equivalent "\C-x" arg))

(defun vip-ctl-key-equivalent (key arg)
  (let ((char (read-char)))
    (if (and (<= ?A char) (<= char ?Z))
	(setq char (- char (- ?A ?\C-a))))
    (setq prefix-arg arg)
    (command-execute
     (vip-get-editor-command
      vip-emacs-local-map global-map
      (format "%s%s" key (char-to-string char))))))


;; commands in insertion mode

(defun vip-delete-backward-word (arg)
  "Delete previous word."
  (interactive "p")
  (save-excursion
    (set-mark (point))
    (backward-word arg)
    (delete-region (point) (mark t))))


;; key bindings

(setq vip-mode-map (make-keymap))

(define-key vip-mode-map "\C-^" 
  (function (lambda () (interactive) (vip-ex "e#"))))
(define-key vip-mode-map "\C-b" 'vip-scroll-back)
(define-key vip-mode-map "\C-d" 'vip-scroll-up)
(define-key vip-mode-map "\C-e" 'vip-scroll-up-one)
(define-key vip-mode-map "\C-f" 'vip-scroll)
(define-key vip-mode-map "\C-h" 'help-command)
(define-key vip-mode-map "\C-m" 'vip-next-line-at-bol)
(define-key vip-mode-map "\C-u" 'vip-scroll-down)
(define-key vip-mode-map "\C-x" 'vip-ctl-x)
(define-key vip-mode-map "\C-y" 'vip-scroll-down-one)
;; vip-toggle-key is C-z by default
(define-key vip-mode-map vip-toggle-key 'vip-change-mode-to-emacs)
(define-key vip-mode-map vip-ESC-key 'vip-ESC)

;; Suppress emacs keys that are not bound above.

(defun vip-make-emacs-keys-invisible(&optional map)
  (if (not map) (setq map vip-mode-map))
  (define-key map "\C-a" 'vip-nil)
  (define-key map "\C-c" 'vip-keyboard-quit)
  (define-key map "\C-g" 'vip-info-on-file)
  (define-key map "\C-i" 'vip-nil)
  (define-key map "\C-k" 'vip-nil)
  (define-key map "\C-l" 'redraw-display)
  (define-key map "\C-n" 'vip-next-line)
  (define-key map "\C-o" 'vip-nil)
  (define-key map "\C-p" 'vip-previous-line)
  (define-key map "\C-q" 'vip-nil)
  (define-key map "\C-r" 'redraw-display)
  (define-key map "\C-s" 'vip-nil)
  (define-key map "\C-t" 'vip-nil)
  (define-key map "\C-v" 'vip-nil)
  (define-key map "\C-w" 'vip-nil)
  )
(vip-make-emacs-keys-invisible)

;; (defun vip-swap-cc-cg()
;;   (define-key vip-mode-map "\C-c" 'vip-info-on-file)
;;   (setq keyboard-translate-table (make-string 128 0))
;;   (let ((i 0))
;;     (while (< i 128)
;;       (aset keyboard-translate-table i i)
;;       (setq i (1+ i))))
;;   (aset keyboard-translate-table ?\^c ?\^g)
;;   (aset keyboard-translate-table ?\^g ?\^c))

;; Unsuppress the emacs  key bindings

(defun vip-rebind-emacs-keys(&optional map fun)
  "Unsuppress emacs key bindings that were hidden for strict vi compatitibilty"
  (if (not map) (setq map vip-mode-map))
  (define-key map "\C-a" fun)
  (define-key map "\C-c" fun)
  (define-key map "\C-g" fun)
  (define-key map "\C-i" fun)
  (define-key map "\C-k" fun)
  (define-key map "\C-l" fun)
  (define-key map "\C-n" fun)
  (define-key map "\C-o" fun)
  (define-key map "\C-p" fun)
  (define-key map "\C-q" fun)
  (define-key map "\C-r" fun)
  (define-key map "\C-s" fun)
  (define-key map "\C-t" fun)
  (define-key map "\C-v" fun)
  (define-key map "\C-w" fun))

;; For diddling with control keys not handled in the suppressor
;; and unsuppressor

(defun vip-rebind-other-keys(&optional map fun)
  "Rebind"
  (if (not map) (setq map vip-mode-map))
  (define-key map "\C-b" fun)
  (define-key map "\C-e" fun)
  (define-key map "\C-f" fun)
  (define-key map "\C-g" fun)
  (define-key map "\C-u" fun)
  (define-key map "\C-y" fun)
  (define-key map "\C-z" fun)
  (define-key map "\C-]" fun)
  (define-key map "\C-_" fun)
  )

;; unsuppress emacs keys in insert mode and vi-mode

(defun vip-make-emacs-keys-visible()
  (setq-default vip-insert-mode-vi-map nil)
  (setq vip-insert-mode-vi-map nil)
  (vip-rebind-emacs-keys)
  (define-key vip-mode-map "\C-c" 'vip-ctl-c)
  (define-key vip-mode-map "\C-g" 'vip-keyboard-quit)
  )


;; Just for the fun of it....
;; Can the user now distinguish using keystrokes?? :-)

(defun vip-become-vi()
  (define-key vip-mode-map "@" 'vip-nil)
  (define-key vip-mode-map "*" 'vip-nil)
  (define-key vip-mode-map "#" 'vip-nil)
  (define-key vip-mode-map "\C-x" 'vip-nil)
  (define-key vip-mode-map "\C-z" 'suspend-emacs)
  (define-key vip-mode-map "\C-h" 'vip-backward-char)
  (define-key vip-mode-map "\C-_" 'vip-nil)
  (define-key vip-mode-map "\C-]" 'vip-nil);; This is actually tags.
  (define-key vip-insert-mode-map "\C-x" 'self-insert-command)
  (setq vip-is-vi t)
  (global-set-key "\C-c" 'keyboard-quit))

(define-key vip-mode-map " " 'vip-forward-char)
(define-key vip-mode-map "!" 'vip-command-argument)
(define-key vip-mode-map "\"" 'vip-command-argument)
(define-key vip-mode-map "#" 'vip-command-argument)
(define-key vip-mode-map "$" 'vip-goto-eol)
(define-key vip-mode-map "%" 'vip-paren-match)
(define-key vip-mode-map "&" (function (lambda () (interactive) (vip-ex "&"))))
(define-key vip-mode-map "'" 'vip-goto-mark-and-skip-white)
(define-key vip-mode-map "(" 'vip-backward-sentence)
(define-key vip-mode-map ")" 'vip-forward-sentence)
(define-key vip-mode-map "*" 'call-last-kbd-macro)
(define-key vip-mode-map "+" 'vip-next-line-at-bol)
(define-key vip-mode-map "," 'vip-repeat-find-opposite)
(define-key vip-mode-map "-" 'vip-previous-line-at-bol)
(define-key vip-mode-map "." 'vip-repeat)
(define-key vip-mode-map "/" 'vip-search-forward)

(define-key vip-mode-map "0" 'vip-beginning-of-line)
(define-key vip-mode-map "1" 'vip-digit-argument)
(define-key vip-mode-map "2" 'vip-digit-argument)
(define-key vip-mode-map "3" 'vip-digit-argument)
(define-key vip-mode-map "4" 'vip-digit-argument)
(define-key vip-mode-map "5" 'vip-digit-argument)
(define-key vip-mode-map "6" 'vip-digit-argument)
(define-key vip-mode-map "7" 'vip-digit-argument)
(define-key vip-mode-map "8" 'vip-digit-argument)
(define-key vip-mode-map "9" 'vip-digit-argument)

(define-key vip-mode-map ":" 'vip-ex)
(define-key vip-mode-map ";" 'vip-repeat-find)
(define-key vip-mode-map "<" 'vip-command-argument)
(define-key vip-mode-map "=" 'vip-command-argument)
(define-key vip-mode-map ">" 'vip-command-argument)
(define-key vip-mode-map "?" 'vip-search-backward)
(define-key vip-mode-map "@" 'vip-register-macro)

(define-key vip-mode-map "A" 'vip-Append)
(define-key vip-mode-map "B" 'vip-backward-Word)
(define-key vip-mode-map "C" 'vip-change-to-eol)
(define-key vip-mode-map "D" 'vip-kill-line)
(define-key vip-mode-map "E" 'vip-end-of-Word)
(define-key vip-mode-map "F" 'vip-find-char-backward)
(define-key vip-mode-map "G" 'vip-goto-line)
(define-key vip-mode-map "H" 'vip-window-top)
(define-key vip-mode-map "I" 'vip-Insert)
(define-key vip-mode-map "J" 'vip-join-lines)
(define-key vip-mode-map "K" 'vip-nil)
(define-key vip-mode-map "L" 'vip-window-bottom)
(define-key vip-mode-map "M" 'vip-window-middle)
(define-key vip-mode-map "N" 'vip-search-Next)
(define-key vip-mode-map "O" 'vip-Open-line)
(define-key vip-mode-map "P" 'vip-Put-back)
(define-key vip-mode-map "Q" 'vip-query-replace)
(define-key vip-mode-map "R" 'vip-overwrite)
(define-key vip-mode-map "S" 'vip-substitute-line)
(define-key vip-mode-map "T" 'vip-goto-char-backward)
(define-key vip-mode-map "U" 'vip-undo)
(define-key vip-mode-map "V" 'vip-find-file-other-screen)
(define-key vip-mode-map "W" 'vip-forward-Word)
(define-key vip-mode-map "X" 'vip-delete-backward-char)
(define-key vip-mode-map "Y" 'vip-yank-line)
(define-key vip-mode-map "ZZ" 'save-buffers-kill-emacs)

;;(define-key vip-mode-map "\\" 'vip-escape-to-emacs)
(define-key vip-mode-map "\\" 'vip-nil)
(define-key vip-mode-map "[" 'vip-brac-function)
(define-key vip-mode-map "]" 'vip-ket-function)
(define-key vip-mode-map "_" 'vip-alternate-ESC)
(define-key vip-mode-map "^" 'vip-bol-and-skip-white)
(define-key vip-mode-map "`" 'vip-goto-mark)

(define-key vip-mode-map "a" 'vip-append)
(define-key vip-mode-map "b" 'vip-backward-word)
(define-key vip-mode-map "c" 'vip-command-argument)
(define-key vip-mode-map "d" 'vip-command-argument)
(define-key vip-mode-map "e" 'vip-end-of-word)
(define-key vip-mode-map "f" 'vip-find-char-forward)
(define-key vip-mode-map "g" 'vip-nil)
(define-key vip-mode-map "h" 'vip-backward-char)
(define-key vip-mode-map "i" 'vip-insert)
(define-key vip-mode-map "j" 'vip-next-line)
(define-key vip-mode-map "k" 'vip-previous-line)
(define-key vip-mode-map "l" 'vip-forward-char)
(define-key vip-mode-map "m" 'vip-mark-point)
(define-key vip-mode-map "n" 'vip-search-next)
(define-key vip-mode-map "o" 'vip-open-line)
(define-key vip-mode-map "p" 'vip-put-back)
(define-key vip-mode-map "q" 'vip-nil)
(define-key vip-mode-map "r" 'vip-replace-char)
(define-key vip-mode-map "s" 'vip-substitute)
(define-key vip-mode-map "t" 'vip-goto-char-forward)
(define-key vip-mode-map "u" 'vip-undo)
(define-key vip-mode-map "v" 'vip-find-file)
(define-key vip-mode-map "w" 'vip-forward-word)
(define-key vip-mode-map "x" 'vip-delete-char)
(define-key vip-mode-map "y" 'vip-command-argument)
(define-key vip-mode-map "zH" 'vip-line-to-top)
(define-key vip-mode-map "zM" 'vip-line-to-middle)
(define-key vip-mode-map "zL" 'vip-line-to-bottom)
(define-key vip-mode-map "z\C-m" 'vip-line-to-top)
(define-key vip-mode-map "z." 'vip-line-to-middle)
(define-key vip-mode-map "z-" 'vip-line-to-bottom)

(define-key vip-mode-map "{" 'vip-backward-paragraph)
(define-key vip-mode-map "|" 'vip-goto-col)
(define-key vip-mode-map "}" 'vip-forward-paragraph)
(define-key vip-mode-map "~" 'vip-toggle-case)
(define-key vip-mode-map "\177" 'vip-delete-backward-char)

;;(define-key ctl-x-map "3" 'vip-buffer-in-two-windows)
;;(define-key ctl-x-map "\C-i" 'insert-file)

(defun vip-version ()
  (interactive)
  (message "VIP version 4.4.2 of September 5, 1993"))


;; implement ex commands

(defvar ex-token-type nil
  "type of token.  if non-nil, gives type of address.  if nil, it
is a command.")

(defvar ex-token nil
  "value of token.")

(defvar ex-addresses nil
  "list of ex addresses")

(defvar ex-flag nil
  "flag for ex flag")

(defvar ex-buffer nil
  "name of ex buffer")

(defvar ex-count nil
  "value of ex count")

(defvar ex-g-flag nil
  "flag for global command")

(defvar ex-g-variant nil
  "if t global command is executed on lines not matching ex-g-pat")

(defvar ex-reg-exp nil
  "save reg-exp used in substitute")


(defvar ex-repl nil
  "replace pattern for substitute")

(defvar ex-g-pat nil
  "pattern for global command")

(defvar ex-map (make-sparse-keymap)
  "save commands for mapped keys")

(defvar ex-ins-map (make-sparse-keymap)
  "save commands for map!ped keys")

(defconst ex-find-file-shell "csh"
  "Shell in which to interpret wildcards")

(defvar ex-tag nil
  "save ex tag")

(defvar ex-file nil)

(defvar ex-variant nil)

(defvar ex-offset nil)

(defvar ex-append nil)

(defvar ex-cmdfile nil)

(defvar ex-cycle-other-window t
  "* :n cycles through files in other window")

(defvar ex-cycle-through-non-files nil
  "* cycle through *scratch* etc.")

(defun vip-nil ()
  (interactive)
  (error ""))

(defun vip-looking-back (str)
  "returns t if looking back reg-exp STR before point."
  (and (save-excursion (re-search-backward str nil t))
       (= (point) (match-end 0))))

(defun vip-check-sub (str)
  "check if ex-token is an initial segment of STR"
  (let ((length (length ex-token)))
    (if (and (<= length (length str))
	     (string= ex-token (substring str 0 length)))
	(setq ex-token str)
      (setq ex-token-type "non-command"))))

(defun vip-get-ex-com-subr ()
  "get a complete ex command"
  (set-mark (point))
  (re-search-forward "[a-z][a-z]*")
  (setq ex-token-type "command")
  (setq ex-token (buffer-substring (point) (mark t)))
  (exchange-point-and-mark)
  (cond ((looking-at "a")
	 (cond ((looking-at "ab") (vip-check-sub "abbreviate"))
	       ((looking-at "ar") (vip-check-sub "args"))
	       (t (vip-check-sub "append"))))
	((looking-at "[bh]") (setq ex-token-type "non-command"))
	((looking-at "c")
	 (cond ((looking-at "cd") (vip-check-sub "cd"))
	       ((looking-at "ch") (vip-check-sub "chdir"))
	       ((looking-at "co") (vip-check-sub "copy"))
	       (t (vip-check-sub "change"))))
	((looking-at "d") (vip-check-sub "delete"))
	((looking-at "e")
	 (if (looking-at "ex") (vip-check-sub "ex")
	   (vip-check-sub "edit")))
	((looking-at "f") (vip-check-sub "file"))
	((looking-at "g") (vip-check-sub "global"))
	((looking-at "i") (vip-check-sub "insert"))
	((looking-at "j") (vip-check-sub "join"))
	((looking-at "l") (vip-check-sub "list"))
	((looking-at "m")
	 (cond ((looking-at "map") (vip-check-sub "map"))
	       ((looking-at "mar") (vip-check-sub "mark"))
	       (t (vip-check-sub "move"))))
	((looking-at "n")
	 (if (looking-at "nu") (vip-check-sub "number")
	   (vip-check-sub "next")))
	((looking-at "o") (vip-check-sub "open"))
	((looking-at "p")
	 (cond ((looking-at "pre") (vip-check-sub "preserve"))
	       ((looking-at "pu") (vip-check-sub "put"))
	       ((looking-at "pw") (vip-check-sub "pwd"))
	       (t (vip-check-sub "print"))))
	((looking-at "q") (vip-check-sub "quit"))
	((looking-at "r")
	 (cond ((looking-at "rec") (vip-check-sub "recover"))
	       ((looking-at "rew") (vip-check-sub "rewind"))
	       (t (vip-check-sub "read"))))
	((looking-at "s")
	 (cond ((looking-at "se") (vip-check-sub "set"))
	       ((looking-at "sh") (vip-check-sub "shell"))
	       ((looking-at "so") (vip-check-sub "source"))
	       ((looking-at "sr") (vip-check-sub "sr"))
	       ((looking-at "st") (vip-check-sub "stop"))
	       ((looking-at "sus") (vip-check-sub "suspend"))
	       (t (vip-check-sub "substitute"))))
	((looking-at "t")
	 (if (looking-at "ta") (vip-check-sub "tag")
	   (vip-check-sub "t")))
	((looking-at "u")
	 (cond ((looking-at "una") (vip-check-sub "unabbreviate"))
	       ((looking-at "unm") (vip-check-sub "unmap"))
	       (t (vip-check-sub "undo"))))
	((looking-at "v")
	 (cond ((looking-at "ve") (vip-check-sub "version"))
	       ((looking-at "vi") (vip-check-sub "visual"))
	       (t (vip-check-sub "v"))))
	((looking-at "w")
	 (if (looking-at "wq") (vip-check-sub "wq")
	   (vip-check-sub "write")))
	((looking-at "x") (vip-check-sub "xit"))
	((looking-at "y") (vip-check-sub "yank"))
	((looking-at "z") (vip-check-sub "z")))
  (exchange-point-and-mark))

(defun vip-get-ex-token ()
  "get an ex-token which is either an address or a command.
a token has type \(command, address, end-mark\) and value."
  (save-window-excursion
    (set-buffer " *ex-working-space*")
    (skip-chars-forward " \t|")
    (cond ((looking-at "[k#]")
	   (setq ex-token-type "command")
	   (setq ex-token (char-to-string (following-char)))
	   (forward-char 1))
	  ((looking-at "[a-z]") (vip-get-ex-com-subr))
	  ((looking-at "\\.")
	   (forward-char 1)
	   (setq ex-token-type "dot"))
	  ((looking-at "[0-9]")
	   (set-mark (point))
	   (re-search-forward "[0-9]*")
	   (setq ex-token-type
		 (cond ((string= ex-token-type "plus") "add-number")
		       ((string= ex-token-type "minus") "sub-number")
		       (t "abs-number")))
	   (setq ex-token (string-to-int (buffer-substring (point) (mark t)))))
	  ((looking-at "\\$")
	   (forward-char 1)
	   (setq ex-token-type "end"))
	  ((looking-at "%")
	   (forward-char 1)
	   (setq ex-token-type "whole"))
	  ((looking-at "+")
	   (cond ((or (looking-at "+[-+]") (looking-at "+[\n|]"))
		  (forward-char 1)
		  (insert "1")
		  (backward-char 1)
		  (setq ex-token-type "plus"))
		 ((looking-at "+[0-9]")
		  (forward-char 1)
		  (setq ex-token-type "plus"))
		 (t
		  (error "Badly formed address"))))
	  ((looking-at "-")
	   (cond ((or (looking-at "-[-+]") (looking-at "-[\n|]"))
		  (forward-char 1)
		  (insert "1")
		  (backward-char 1)
		  (setq ex-token-type "minus"))
		 ((looking-at "-[0-9]")
		  (forward-char 1)
		  (setq ex-token-type "minus"))
		 (t
		  (error "Badly formed address"))))
	  ((looking-at "/")
	   (forward-char 1)
	   (set-mark (point))
	   (let ((cont t))
	     (while (and (not (eolp)) cont)
	       ;;(re-search-forward "[^/]*/")
	       ;;(re-search-forward "[^/]*/")
	       (re-search-forward "[^/]*\\(/\\|\n\\)")
	       (if (not (vip-looking-back "[^\\\\]\\(\\\\\\\\\\)*\\\\/"))
		   (setq cont nil))))
	   (backward-char 1)
	   (setq ex-token (buffer-substring (point) (mark t)))
	   (if (looking-at "/") (forward-char 1))
	   (setq ex-token-type "search-forward"))
	  ((looking-at "\\?")
	   (forward-char 1)
	   (set-mark (point))
	   (let ((cont t))
	     (while (and (not (eolp)) cont)
	       ;;(re-search-forward "[^\\?]*\\?")
	       (re-search-forward "[^\\?]*\\(\\?\\|\n\\)")
	       (if (not (vip-looking-back "[^\\\\]\\(\\\\\\\\\\)*\\\\\\?"))
		   (setq cont nil))
	       (backward-char 1)
	       (if (not (looking-at "\n")) (forward-char 1))))
	   (setq ex-token-type "search-backward")
	   (setq ex-token (buffer-substring (1- (point)) (mark t))))
	  ((looking-at ",")
	   (forward-char 1)
	   (setq ex-token-type "comma"))
	  ((looking-at ";")
	   (forward-char 1)
	   (setq ex-token-type "semi-colon"))
	  ((looking-at "[!=><&~]")
	   (setq ex-token-type "command")
	   (setq ex-token (char-to-string (following-char)))
	   (forward-char 1))
	  ((looking-at "'")
	   (setq ex-token-type "goto-mark")
	   (forward-char 1)
	   (cond ((looking-at "'") (setq ex-token nil))
		 ((looking-at "[a-z]") (setq ex-token (following-char)))
		 (t (error "Marks are ' and a-z")))
	   (forward-char 1))
	  ((looking-at "\n")
	   (setq ex-token-type "end-mark")
	   (setq ex-token "goto"))
	  (t
	   (error "illegal token")))))

(defun vip-ex (&optional string)
  "ex commands within VIP."
  (interactive)
  (setq zmacs-regions nil)
  (or string
      (setq ex-g-flag nil
	    ex-g-variant nil))
  (let ((com-str (or string (vip-read-string ":" "" 'vip-ex-history)))
	(address nil) (cont t) (dot (point)))
    (save-window-excursion
      (set-buffer (get-buffer-create " *ex-working-space*"))
      (delete-region (point-min) (point-max))
      (insert com-str "\n")
      (goto-char (point-min)))
    (setq ex-token-type "")
    (setq ex-addresses nil)
    (while cont
      (vip-get-ex-token)
      (cond ((or (string= ex-token-type "command")
		 (string= ex-token-type "end-mark"))
	     (if address (setq ex-addresses (cons address ex-addresses)))
	     (cond ((string= ex-token "global")
		    (ex-global nil)
		    (setq cont nil))
		   ((string= ex-token "v")
		    (ex-global t)
		    (setq cont nil))
		   (t
		    (vip-execute-ex-command)
		    (save-window-excursion
		      (set-buffer " *ex-working-space*")
		      (skip-chars-forward " \t")
		      (cond ((looking-at "|") (forward-char 1))
			    ((looking-at "\n") (setq cont nil))
			    (t (error "Extra character end of command")))))))
	    ((string= ex-token-type "non-command")
	     (error "%s: %s" ex-token vip-ENOCMD))
	    ((string= ex-token-type "whole")
	     (setq ex-addresses
		   (cons (point-max) (cons (point-min) ex-addresses))))
	    ((string= ex-token-type "comma")
	     (setq ex-addresses
		   (cons (if (null address) (point) address) ex-addresses)))
	    ((string= ex-token-type "semi-colon")
	     (if address (setq dot address))
	     (setq ex-addresses
		   (cons (if (null address) (point) address) ex-addresses)))
	    (t (let ((ans (vip-get-ex-address-subr address dot)))
		 (if ans (setq address ans)))))))
  (setq zmacs-regions t)
  )

(defun vip-get-ex-pat ()
  "get a regular expression and set ex-variant if found"
  (save-window-excursion
    (set-buffer " *ex-working-space*")
    (skip-chars-forward " \t")
    (if (looking-at "!")
	(progn
	  (setq ex-g-variant (not ex-g-variant)
		ex-g-flag (not ex-g-flag))
	  (forward-char 1)
	  (skip-chars-forward " \t")))
    (let ((c (following-char)))
      (if (looking-at "[^\\\\\n]")
	  (progn
	    (forward-char 1)
	    (set-mark (point))
	    (let ((cont t))
	      (while (and (not (eolp)) cont)
		(re-search-forward (format "[^%c]*\\(%c\\|\n\\)" c c))
		;;(re-search-forward "[^/]*/")
		(if (not (vip-looking-back
			  (format "[^\\\\]\\(\\\\\\\\\\)*\\\\%c" c)))
		    (setq cont nil))))
	    (setq ex-token
		  (if (= (mark t) (point)) ""
		    (buffer-substring (1- (point)) (mark t))))
	    (backward-char 1))
	(setq ex-token nil))
      c)))

(defun vip-get-ex-command ()
  "get an ex command"
  (save-window-excursion
    (set-buffer " *ex-working-space*")
    (if (looking-at "/") (forward-char 1))
    (skip-chars-forward " \t")
    (cond ((looking-at "[a-z]")
	   (vip-get-ex-com-subr)
	   (if (string= ex-token-type "non-command")
	       (error "%s: not an editor command" ex-token)))
	  ((looking-at "[!=><&~]")
	   (setq ex-token (char-to-string (following-char)))
	   (forward-char 1))
	  (t (error "Could not find an ex command")))))

(defun vip-get-ex-opt-gc (c)
  "get an ex option g or c"
  (save-window-excursion
    (set-buffer " *ex-working-space*")
    (if (looking-at (format "%c" c)) (forward-char 1))
    (skip-chars-forward " \t")
    (cond ((looking-at "g")
	   (setq ex-token "g")
	   (forward-char 1)
	   t)
	  ((looking-at "c")
	   (setq ex-token "c")
	   (forward-char 1)
	   t)
	  (t nil))))

(defun vip-default-ex-addresses (&optional whole-flag)
  "compute default addresses.  whole-flag means whole buffer."
  (cond ((null ex-addresses)
	 (setq ex-addresses
	       (if whole-flag
		   (cons (point-max) (cons (point-min) nil))
		 (cons (point) (cons (point) nil)))))
	((null (cdr ex-addresses))
	 (setq ex-addresses
	       (cons (car ex-addresses) ex-addresses)))))

(defun vip-get-ex-address ()
  "get an ex-address as a marker and set ex-flag if a flag is found"
  (let ((address (point-marker)) (cont t))
    (setq ex-token "")
    (setq ex-flag nil)
    (while cont
      (vip-get-ex-token)
      (cond ((string= ex-token-type "command")
	     (if (or (string= ex-token "print") (string= ex-token "list")
		     (string= ex-token "#"))
		 (progn
		   (setq ex-flag t)
		   (setq cont nil))
	       (error "address expected")))
	    ((string= ex-token-type "end-mark")
	     (setq cont nil))
	    ((string= ex-token-type "whole")
	     (error "a trailing address is expected"))
	    ((string= ex-token-type "comma")
	     (error "Extra characters after an address"))
	    (t (let ((ans (vip-get-ex-address-subr address (point-marker))))
		 (if ans (setq address ans))))))
    address))

(defun vip-get-ex-address-subr (old-address dot)
  "returns an address as a point"
  (let ((address nil) (relative (eq old-address dot)))
    (if (null old-address) (setq old-address dot))
    (cond ((string= ex-token-type "dot")
	   (setq address dot))
	  ((string= ex-token-type "add-number")
	   (save-excursion
	     (goto-char old-address)
	     (forward-line (if (= old-address 0) (1- ex-token) ex-token))
	     (setq address (point-marker))))
	  ((string= ex-token-type "sub-number")
	   (save-excursion
	     (goto-char old-address)
	     (forward-line (- ex-token))
	     (setq address (point-marker))))
	  ((string= ex-token-type "abs-number")
	   (save-excursion
	     (goto-char (point-min))
	     (if (= ex-token 0) (setq address 0)
	       (forward-line (1- ex-token))
	       (setq address (point-marker)))))
	  ((string= ex-token-type "end")
	   (setq address (point-max-marker)))
	  ((string= ex-token-type "plus") t);; do nothing
	  ((string= ex-token-type "minus") t);; do nothing
	  ((string= ex-token-type "search-forward")
	   (save-excursion
	     (if relative (goto-char dot))
	     (ex-search-address t)
	     (setq address (point-marker))))
	  ((string= ex-token-type "search-backward")
	   (save-excursion
	     (if relative (goto-char dot))
	     (ex-search-address nil)
	     (setq address (point-marker))))
	  ((string= ex-token-type "goto-mark")
	   (save-excursion
	     (if (null ex-token)
		 (exchange-point-and-mark)
	       (goto-char (register-to-point (- ex-token (- ?a ?\C-a)))))
	          (goto-char (register-to-point (- ex-token (- ?a ?\C-a))))
	     (setq address (point-marker)))))
    address))

(defun ex-search-address (forward)
  "search pattern and set address"
  (if (string= ex-token "")
      (if (null vip-s-string) (error "No previous search string")
	(setq ex-token vip-s-string))
    (setq vip-s-string ex-token))
  (if forward
      (progn
	(forward-line 1)
	(re-search-forward ex-token))
    (forward-line -1)
    (re-search-backward ex-token)))

(defun vip-get-ex-buffer ()
  "get a buffer name and set ex-count and ex-flag if found"
  (setq ex-buffer nil)
  (setq ex-count nil)
  (setq ex-flag nil)
  (save-window-excursion
    (set-buffer " *ex-working-space*")
    (skip-chars-forward " \t")
    (if (looking-at "[a-zA-Z]")
	(progn
	  (setq ex-buffer (following-char))
	  (forward-char 1)
	  (skip-chars-forward " \t")))
    (if (looking-at "[0-9]")
	(progn
	  (set-mark (point))
	  (re-search-forward "[0-9][0-9]*")
	  (setq ex-count (string-to-int (buffer-substring (point) (mark t))))
	  (skip-chars-forward " \t")))
    (if (looking-at "[pl#]")
	(progn
	  (setq ex-flag t)
	  (forward-char 1)))
    (if (not (looking-at "[\n|]"))
	(error "Illegal extra characters"))))

(defun vip-get-ex-count ()
  (setq ex-variant nil
	ex-count nil
	ex-flag nil)
  (save-window-excursion
    (set-buffer " *ex-working-space*")
    (skip-chars-forward " \t")
    (if (looking-at "!")
	(progn
	  (setq ex-variant t)
	  (forward-char 1)))
    (skip-chars-forward " \t")
    (if (looking-at "[0-9]")
	(progn
	  (set-mark (point))
	  (re-search-forward "[0-9][0-9]*")
	  (setq ex-count (string-to-int (buffer-substring (point) (mark t))))
	  (skip-chars-forward " \t")))
    (if (looking-at "[pl#]")
	(progn
	  (setq ex-flag t)
	  (forward-char 1)))
    (if (not (looking-at "[\n|]"))
	(error "Illegal extra characters"))))

(defun ex-expand-filsyms (cmd buf)
  "expand \% and \# in ex command"
  (let (cf pf ret)
    (save-excursion 
      (set-buffer buf)
      (setq cf buffer-file-name)
      (setq pf (ex-next t)))
    (if (and (null cf) (string-match "[^\\]%\\|\\`%" cmd))
	(error "No current file to substitute for \%"))
    (if (and (null pf) (string-match "[^\\]#\\|\\`#" cmd))
	(error "No alternate file to substitute for #"))
    (save-excursion
      (set-buffer (get-buffer-create " ex-tmp"))
      (insert cmd)
      (goto-char (point-min))
      (while (re-search-forward "%\\|#" nil t)
	(let ((data (match-data)) 
	      (char (buffer-substring (match-beginning 0) (match-end 0))))
	  (if (vip-looking-back (concat "\\\\" char))
	      (replace-match char)
	    (store-match-data data)
	    (if (string= char "%")
		(replace-match cf)
	      (replace-match pf)))))
      (end-of-line)
      (setq ret (buffer-substring (point-min) (point)))
      (kill-buffer (current-buffer))
      (message "%s" ret))
    ret))

(defun vip-get-ex-file ()
  "get a file name and set ex-variant, ex-append and ex-offset if found"
  (let ((file-buf (current-buffer)))
    (setq ex-file nil
	  ex-variant nil
	  ex-append nil
	  ex-offset nil
	  ex-cmdfile nil)
    (save-window-excursion
      (set-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (if (looking-at "!")
	  (progn
	    (if (vip-looking-back "[ \t]")
		(setq ex-cmdfile t)
	      (setq ex-variant t))
	    (forward-char 1)
	    (skip-chars-forward " \t")))
      (if (looking-at ">>")
	  (progn
	    (setq ex-append t
		  ex-variant t)
	    (forward-char 2)
	    (skip-chars-forward " \t")))
      (if (looking-at "+")
	  (progn
	    (forward-char 1)
	    (set-mark (point))
	    (re-search-forward "[ \t\n]")
	    (backward-char 1)
	    (setq ex-offset (buffer-substring (point) (mark t)))
	    (forward-char 1)
	    (skip-chars-forward " \t")))
      (set-mark (point))
;;      (re-search-forward "[ \t\n|]")
;;      (backward-char 1)
      (if ex-cmdfile
	  (progn
	    (goto-char (point-max))
	    (re-search-backward "[^ \t\n]")
	    (forward-char 1))
	(re-search-forward "[ \t\n|]")
	(backward-char 1))
      (setq ex-file 
	    (ex-expand-filsyms (buffer-substring (point) (mark t)) file-buf)))))

(defun vip-execute-ex-command ()
  "execute ex command using the value of addresses."
  (cond ((string= ex-token "args") (ex-args))
	((string= ex-token "copy") (ex-copy nil))
	((string= ex-token "cd") (ex-cd))
	((string= ex-token "chdir") (ex-cd))
	((string= ex-token "delete") (ex-delete))
	((string= ex-token "edit") (ex-edit))
	((string= ex-token "file") (vip-info-on-file))
	((string= ex-token "goto") (ex-goto))
	;;((string= ex-token "global") (ex-global nil))
	((string= ex-token "join") (ex-line "join"))
	;((string= ex-token "k") (ex-mark))
	((string= ex-token "k") (kill-this-buffer))
	((string= ex-token "mark") (ex-mark))
	((string= ex-token "map") (ex-map))
	((string= ex-token "move") (ex-copy t))
	((string= ex-token "next") (ex-next))
	((string= ex-token "put") (ex-put))
	((string= ex-token "pwd") (ex-pwd))
	((string= ex-token "preserve") (ex-preserve))
	((string= ex-token "quit") (ex-quit))
	((string= ex-token "read") (ex-read))
	((string= ex-token "recover") (ex-recover))
	((string= ex-token "rewind") (ex-rewind))
	((string= ex-token "set") (ex-set))
	((string= ex-token "shell") (ex-shell))
	((string= ex-token "source") (ex-source))
	((string= ex-token "sr") (ex-substitute t t))
	((string= ex-token "substitute") (ex-substitute))
	((string= ex-token "suspend") (suspend-emacs))
	((string= ex-token "stop") (suspend-emacs))
	((string= ex-token "t") (ex-copy nil))
	((string= ex-token "tag") (ex-tag))
	((string= ex-token "undo") (vip-undo))
	((string= ex-token "unmap") (ex-unmap))
	;;((string= ex-token "v") (ex-global t))
	((string= ex-token "version") (vip-version))
	((string= ex-token "visual") (ex-edit))
	((string= ex-token "write") (ex-write nil))
	((string= ex-token "wq") (ex-write t))
	((string= ex-token "xit") (ex-write t))
	((string= ex-token "yank") (ex-yank))
	((string= ex-token "!") (ex-command))
	((string= ex-token "=") (ex-line-no))
	((string= ex-token ">") (ex-line "right"))
	((string= ex-token "<") (ex-line "left"))
	((string= ex-token "&") (ex-substitute t))
	((string= ex-token "~") (ex-substitute t t))
	((or (string= ex-token "append")
	     (string= ex-token "change")
	     (string= ex-token "insert")
	     (string= ex-token "open")
	     )
	 (error "%s: %s" ex-token vip-ENOCMD))
	((or (string= ex-token "abbreviate")
	     (string= ex-token "list")
	     (string= ex-token "print")
	     (string= ex-token "unabbreviate")
	     (string= ex-token "z")
	     )
	 (error "%s: %s" ex-token vip-ENOCMD))
	(t (error "%s: %s" ex-token vip-ENOCMD))))

(defun vip-undisplayed-files()
  (mapcar
   (function 
    (lambda (b) 
      (if (null (get-buffer-window b))
	  (let ((f (buffer-file-name b)))
	    (if f f
	      (if ex-cycle-through-non-files 
		  (let ((s (buffer-name b)))
		    (if (string= " " (substring s 0 1))
			nil
		      s))
		nil)))
	nil)))
   (buffer-list)))

(defun ex-args()
  (let ((l (vip-undisplayed-files))
	(args "") (insert-point (point)) end-point 
	(file-count 1) 
	(modified (buffer-modified-p)))
    (while (not (null l))
      (if (car l) 
	  (setq args (format "%s %d) %s" args file-count (car l))
		file-count (1+ file-count)))
      (setq l (cdr l)))
    (if (string= args "")
	(message "All files are displayed")
      (unwind-protect
	  (save-excursion
	    (insert "\n\nThese files are not displayed in any window.")
	    (insert "\n=============\n" args "\n=============\n")
	    (insert "The numbers can be given as counts to :next. ")
	    (insert "Press any key to continue.\n\n")
	    (message "Undisplayed files. Press any key to continue")
	    (setq end-point (point))
	    (read-char))
	(delete-region insert-point end-point)
	(set-buffer-modified-p modified)))))

(defun ex-cd ()
  "ex cd. Default directory of this buffer changes"
  (vip-get-ex-file)
  (if (string= ex-file "")
      (setq ex-file "~"))
  (setq default-directory (file-name-as-directory (expand-file-name ex-file))))

(defun ex-copy (del-flag)
  "ex copy and move command.  DEL-FLAG means delete."
  (vip-default-ex-addresses)
  (let ((address (vip-get-ex-address))
	(end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (goto-char end)
    (save-excursion
      (set-mark beg)
      (vip-enlarge-region (mark t) (point))
      (if del-flag (kill-region (point) (mark t))
	(copy-region-as-kill (point) (mark t)))
      (if ex-flag
	  (progn
	    (with-output-to-temp-buffer "*copy text*"
	      (princ
	       (if (or del-flag ex-g-flag ex-g-variant)
		   (car kill-ring-yank-pointer)
		 (buffer-substring (point) (mark t)))))
	    (condition-case nil
		(progn
		  (vip-read-string "[Hit return to continue] ")
		  (save-excursion (kill-buffer "*copy text*")))
	      (quit
	       (save-excursion (kill-buffer "*copy text*"))
	       (signal 'quit nil))))))
    (if (= address 0)
	(goto-char (point-min))
      (goto-char address)
      (forward-line 1))
    (insert (car kill-ring-yank-pointer))))

(defun ex-delete ()
  "ex delete"
  (vip-default-ex-addresses)
  (vip-get-ex-buffer)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (vip-enlarge-region beg end)
      (exchange-point-and-mark)
      (if ex-count
	  (progn
	    (set-mark (point))
	    (forward-line (1- ex-count)))
	(set-mark end))
      (vip-enlarge-region (point) (mark t))
      (if ex-flag
	  ;; show text to be deleted and ask for confirmation
	  (progn
	    (with-output-to-temp-buffer " *delete text*"
	      (princ (buffer-substring (point) (mark t))))
	    (condition-case conditions
		(vip-read-string "[Hit return to continue] ")
	      (quit
	       (save-excursion (kill-buffer " *delete text*"))
	       (error "")))
	    (save-excursion (kill-buffer " *delete text*")))
	(if ex-buffer
	    (if (and (<= ?A ex-buffer) (<= ex-buffer ?Z))
		(vip-append-to-register
		 (+ ex-buffer 32) (point) (mark t))
	      (copy-to-register ex-buffer (point) (mark t) nil)))
	(delete-region (point) (mark t))))))



(defun ex-edit (&optional file)
  "ex-edit"
  (if (not file)
      (vip-get-ex-file))
  (if (and (not ex-variant) (buffer-modified-p) buffer-file-name)
      (error "No write since last change \(:e! overrides\)"))
  (if (string= ex-file "")
      (if ex-variant
	  (progn
	    (setq ex-file (buffer-file-name))
	    (set-buffer-modified-p nil)
	    (vip-ex-kill-buf (current-buffer)))
	(if (y-or-n-p
	     (format "No file specified. Dired %s ?" default-directory))
	    (setq ex-file default-directory)
	  (error ""))))
  (if (null (setq file (get-file-buffer ex-file)))
      (ex-find-file ex-file)
    (vip-ex-find-buf file))
  (vip-change-mode-to-vi)
  (goto-char (point-min))
  (if ex-offset
      (progn
	(save-window-excursion
	  (set-buffer " *ex-working-space*")
	  (delete-region (point-min) (point-max))
	  (insert ex-offset "\n")
	  (goto-char (point-min)))
	(goto-char (vip-get-ex-address))
	(beginning-of-line))))

(defun ex-find-file(filespec)
  (let (s f)
    (if (string-match "[^a-zA-Z0-9_.-/]" filespec)
	(progn
	  (save-excursion 
	    (set-buffer (get-buffer-create " ex-tmp"))
	    (call-process ex-find-file-shell nil t nil "-c" 
			  (format "echo %s | tr ' ' '\\012'" filespec))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (setq s (point))
	      (end-of-line)
	      (setq f (buffer-substring s (point)))
	      (find-file-noselect f)
	      (forward-to-indentation 1))
	    (kill-buffer (current-buffer))))
      (find-file-noselect (setq f filespec)))
    (vip-ex-find-buf (get-file-buffer f)))) 

(defun ex-global (variant)
  "ex global command"
  (if (or ex-g-flag ex-g-variant)
      (error "Global within global not allowed")
    (if variant
	(setq ex-g-flag nil
	      ex-g-variant t)
      (setq ex-g-flag t
	    ex-g-variant nil)))
  (vip-get-ex-pat)
  (if (null ex-token)
      (error "Missing regular expression for global command"))
  (if (string= ex-token "")
      (if (null vip-s-string) (error "No previous search string")
	(setq ex-g-pat vip-s-string))
    (setq ex-g-pat ex-token
	  vip-s-string ex-token))
  (if (null ex-addresses)
      (setq ex-addresses (list (point-max) (point-min))))
  (let ((marks nil) (mark-count 0)
	com-str (end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (vip-enlarge-region beg end)
      (exchange-point-and-mark)
      (let ((cont t) (limit (point-marker)))
	(exchange-point-and-mark)
	;; skip the last line if empty
	(beginning-of-line)
	(if (and (eobp) (not (bobp))) (backward-char 1))
	(while (and cont (not (bobp)) (>= (point) limit))
	  (beginning-of-line)
	  (set-mark (point))
	  (end-of-line)
	  (let ((found (re-search-backward ex-g-pat (mark t) t)))
	    (if (or (and ex-g-flag found)
		    (and ex-g-variant (not found)))
		(progn
		  (end-of-line)
		  (setq mark-count (1+ mark-count))
		  (setq marks (cons (point-marker) marks)))))
	  (beginning-of-line)
	  (if (bobp) (setq cont nil)
	    (forward-line -1)
	    (end-of-line)))))
    (save-window-excursion
      (set-buffer " *ex-working-space*")
      (setq com-str (buffer-substring (1+ (point)) (1- (point-max)))))
    (while marks
      (goto-char (car marks))
; report progress of execution on a slow machine.
;(message "Executing global command...")
;(if (zerop (% mark-count 10))
;(message "Executing global command...%d" mark-count))
      (vip-ex com-str)
      (setq mark-count (1- mark-count))
      (setq marks (cdr marks)))))
;(message "Executing global command...done")))

(defun ex-goto ()
  "ex goto command"
  (if (null ex-addresses)
      (setq ex-addresses (cons (point) nil)))
  (push-mark (point))
  (goto-char (car ex-addresses))
  (beginning-of-line))

(defun ex-line (com)
  "ex line commands.  COM is join, shift-right or shift-left."
  (vip-default-ex-addresses)
  (vip-get-ex-count)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))) point)
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (vip-enlarge-region beg end)
      (exchange-point-and-mark)
      (if ex-count
	  (progn
	    (set-mark (point))
	    (forward-line ex-count)))
      (if ex-flag
	  ;; show text to be joined and ask for confirmation
	  (progn
	    (with-output-to-temp-buffer " *text*"
	      (princ (buffer-substring (point) (mark t))))
	    (condition-case conditions
		(progn
		  (vip-read-string "[Hit return to continue] ")
		  (ex-line-subr com (point) (mark t)))
	      (quit
	       (ding)))
	    (save-excursion (kill-buffer " *text*")))
	(ex-line-subr com (point) (mark t)))
      (setq point (point)))
    (goto-char (1- point))
    (beginning-of-line)))

(defun ex-line-subr (com beg end)
  (cond ((string= com "join")
	 (goto-char (min beg end))
	 (while (and (not (eobp)) (< (point) (max beg end)))
	   (end-of-line)
	   (if (and (<= (point) (max beg end)) (not (eobp)))
	       (progn
		 (forward-line 1)
		 (delete-region (point) (1- (point)))
		 (if (not ex-variant) (fixup-whitespace))))))
	((or (string= com "right") (string= com "left"))
	 (indent-rigidly
	  (min beg end) (max beg end)
	  (if (string= com "right") vip-shift-width (- vip-shift-width)))
	 (goto-char (max beg end))
	 (end-of-line)
	 (forward-char 1))))

(defun ex-mark ()
  "ex mark"
  (let (char)
    (if (null ex-addresses)
	(setq ex-addresses (cons (point) nil)))
    (save-window-excursion
      (set-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (if (looking-at "[a-z]")
	  (progn
	    (setq char (following-char))
	    (forward-char 1)
	    (skip-chars-forward " \t")
	    (if (not (looking-at "[\n|]"))
		(error "Extra characters at end of \"k\" command")))
	(if (looking-at "[\n|]")
	    (error "\"k\" requires a following letter")
	  (error "Mark must specify a letter"))))
    (save-excursion
      (goto-char (car ex-addresses))
      (point-to-register (- char (- ?a ?\C-a))))))

(defun ex-map ()
  "ex map"
  (let (char string ins)
    (save-window-excursion
      (set-buffer " *ex-working-space*")
      (if (looking-at "!") (progn (setq ins t) (forward-char 1)))
      (skip-chars-forward " \t")
      (setq char (char-to-string (following-char)))
      (forward-char 1)
      (if (not (looking-at "[ \t]")) (error "Usage: :map char string"))
      (skip-chars-forward " \t")
      (if (looking-at "[\n|]") (error "Missing rhs"))
      (set-mark (point))
      (end-of-buffer)
      (backward-char 1)
      (setq string (buffer-substring (mark t) (point))))
    (if ins
	(progn
	  (if (not (lookup-key ex-ins-map char))
	      (define-key ex-ins-map char
		(or (lookup-key vip-insert-mode-map char) 'vip-nil)))
	  (define-key vip-insert-mode-map char
	    (` (lambda (count) (interactive "p")
		 (execute-kbd-macro (, string) count)))))
      (if (not (lookup-key ex-map char))
	  (define-key ex-map char
	    (or (lookup-key vip-mode-map char) 'vip-nil)))
      (define-key vip-mode-map char
	(` (lambda (count) (interactive "p")
	     (execute-kbd-macro (, string) count)))))))

(defun ex-unmap ()
  "ex unmap"
  (let (char ins)
    (save-window-excursion
      (set-buffer " *ex-working-space*")
      (if (looking-at "!") (progn (setq ins t) (forward-char 1)))
      (skip-chars-forward " \t")
      (setq char (char-to-string (following-char)))
      (forward-char 1)
      (skip-chars-forward " \t")
      (if (not (looking-at "[\n|]")) (error "Only single character macros")))
    (if ins
	(progn
	 (if (not (lookup-key ex-ins-map char))
	     (error "That macro wasn't mapped"))
	 (define-key vip-insert-mode-map char (lookup-key ex-map char))
	 (define-key ex-ins-map char nil))
      (if (not (lookup-key ex-map char))
	  (error "That macro wasn't mapped"))
      (define-key vip-mode-map char (lookup-key ex-map char))
      (define-key ex-map char nil))))

(defun ex-next (&optional previous)
  (catch 'ex-edit
    (let (count l)
      (if (not previous) 
	  (progn
	    (vip-get-ex-file)
	    (if (or (char-or-string-p ex-offset)
		    (and (not (string= "" ex-file)) 
			 (not (string-match "[0-9]+" ex-file))))
		(progn
		  (ex-edit t)
		  (throw 'ex-edit nil))
	      (setq count (string-to-int ex-file))
	      (if (= count 0) (setq count 1))
	      (if (< count 0) (error "Usage: next (count >= 0)"))))
	(setq count 1))
      (setq l (vip-undisplayed-files))
      (while (> count 0)
	(while (and (not (null l)) (null (car l)))
	  (setq l (cdr l)))
	(setq count (1- count))
	(if (> count 0)
	    (setq l (cdr l))))
      (if previous (car l)
	(progn
	  (if (car l)
	      (let* ((w (if ex-cycle-other-window
			    (get-lru-window) (selected-window)))
		     k   (b (window-buffer w)))
		(set-window-buffer w (get-file-buffer (car l)))
		(bury-buffer b))
	    (error "Not that many undisplayed files")))))))

(defun ex-preserve ()
  "Force auto save"
  (message "Autosaving all buffers that need to be saved...")
  (do-auto-save t))

(defun ex-put ()
  "ex put"
  (let ((point (if (null ex-addresses) (point) (car ex-addresses))))
    (vip-get-ex-buffer)
    (setq vip-use-register ex-buffer)
    (goto-char point)
    (if (= point 0) (vip-Put-back 1) (vip-put-back 1))))

(defun ex-pwd ()
  "ex print working directory"
  (message default-directory))

(defun ex-quit ()
  "ex quit"
  (let (char)
    (save-window-excursion
      (set-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (setq char (following-char)))
    (if (= char ?!) (set-buffer-modified-p nil))
    (if vip-is-vi (save-buffers-kill-emacs)
      (vip-ex-kill-buf (current-buffer)))))


(defun ex-read ()
  "ex read"
  (vip-get-ex-file)
  (let ((point (if (null ex-addresses) (point) (car ex-addresses))))
    (goto-char point)
    (if (not (= point 0)) (next-line 1))
    (beginning-of-line)
    (if (and (not ex-variant) (string= ex-file ""))
	(progn
	  (if (null buffer-file-name)
	      (error "No file specified"))
	  (setq ex-file buffer-file-name)))
    (if ex-cmdfile
	(shell-command ex-file t)
      (insert-file ex-file))))

(defun ex-recover ()
  "ex recover from emacs \#file\#"
  (vip-get-ex-file)
  (if (or ex-append ex-offset)
      (error "Illegal extra characters"))
  (if (string= ex-file "")
      (progn
	(if (null buffer-file-name)
	    (error "No file associated with this buffer"))
	(setq ex-file buffer-file-name))
    (setq ex-file (expand-file-name ex-file)))
  (if (and (not (string= ex-file (buffer-file-name)))
	   (buffer-modified-p)
	   (not ex-variant))
      (error "No write since last change \(:rec! overrides\)"))
  (recover-file ex-file))

(defun ex-rewind ()
  "No rewind really. Instead tell about :next count"
  (message ":n can count the :args list now. :rewind is obsolete"))

(defun ex-set ()
  (let (var (val 0) (b (buffer-name (current-buffer))) (s "setq"))
    (save-window-excursion
      (set-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (if (looking-at "[\n|]") (error "Usage: set variable[= \t]value"))
      (set-mark (point))
      (skip-chars-forward "^ \t=\n|")
      (setq var (buffer-substring (mark t) (point)))
      (cond ((or (string= var "ai") (string= var "autoindent"))
	     (setq var "vip-auto-indent") (setq val "t"))
	    ((or (string= var "noai") (string= var "noautoindent"))
	     (setq var "vip-auto-indent") (setq val "nil"))
	    ((or (string= var "ic") (string= var "ignorecase"))
	     (setq var "vip-case-fold-search") (setq val "t"))
	    ((or (string= var "noic") (string= var "noignorecase"))
	     (setq var "vip-case-fold-search") (setq val "nil"))
	    ((or (string= var "ma") (string= var "magic"))
	     (setq var "vip-re-search") (setq val "t"))
	    ((or (string= var "noma") (string= var "nomagic"))
	     (setq var "vip-re-search") (setq val "nil"))
	    ((or (string= var "ro") (string= var "readonly"))
	     (setq var "buffer-read-only") (setq val "t"))
	    ((or (string= var "noro") (string= var "noreadonly"))
	     (setq var "buffer-read-only") (setq val "nil"))
	    ((or (string= var "sm") (string= var "showmatch"))
	     (setq var "blink-matching-paren") (setq val "t"))
	    ((or (string= var "nosm") (string= var "noshowmatch"))
	     (setq var "blink-matching-paren") (setq val "nil"))
	    ((or (string= var "ws") (string= var "wrapscan"))
	     (setq var "vip-search-wrap-around-t") (setq val "t"))
	    ((or (string= var "nows") (string= var "nowrapscan"))
	     (setq var "vip-search-wrap-around-t") (setq val "nil")))
      (if (eq val 0)
	  (progn
	    (if (looking-at "[\n|]") (error "Usage: set variable[= \t]value"))
	    (forward-char 1)
	    (skip-chars-forward " \t")
	    (if (looking-at "[\n|]") (error "Missing rhs"))
	    (set-mark (point))
	    (end-of-buffer)
	    (backward-char 1)
	    (setq val (buffer-substring (mark t) (point)))
	    (cond
	     ((or (string= var "sw") (string= var "shiftwidth"))
	      (setq var "vip-shift-width"))
	     ((or (string= var "ts") (string= var "tabstop"))
	      (setq var "tab-width") (setq s "setq-default"))
	     ((or (string= var "wm") (string= var "wrapmargin"))
	      (setq var "fill-column") 
	      (setq val (format "(- (window-width) %s)" val)) 
	      (setq s "setq-default"))
	     ((or (string= var "sh") (string= var "shell"))
	      (setq var "explicit-shell-file-name") 
	      (setq val (format "\"%s\"" val))))))
      (message (format "%s %s %s" s var val))
      (eval (car (read-from-string (format "(%s %s %s)" s var val)))))))


;;   (eval (list 'setq
;; 	      (read-variable "Variable: ")
;; 	      (eval (read-minibuffer "Value: ")))))

(defun ex-shell ()
  "ex shell"
  (shell))

(defun ex-source ()
  "ex-source - just load the file or ~/.vip"
  (vip-get-ex-file)
  (if (string= ex-file "")
      (load vip-custom-file-name)
    (load ex-file)))

(defun ex-substitute (&optional repeat r-flag) 
  "ex substitute. if REPEAT use previous reg-exp which is ex-reg-exp or
vip-s-string"
  (let (delim pat repl (opt-g nil) (opt-c nil) (matched-pos nil))
    (if repeat (setq ex-token nil) (setq delim (vip-get-ex-pat)))
    (if (null ex-token)
	(setq pat (if r-flag vip-s-string ex-reg-exp)
	      repl ex-repl)
      (setq pat (if (string= ex-token "") vip-s-string ex-token))
      (setq vip-s-string pat
	    ex-reg-exp pat)
      (setq delim (vip-get-ex-pat))
      (if (null ex-token)
	  (setq ex-token ""
		ex-repl "")
	(setq repl ex-token
	      ex-repl ex-token)))
    (while (vip-get-ex-opt-gc delim)
      (if (string= ex-token "g") (setq opt-g t) (setq opt-c t)))
    (vip-get-ex-count)
    (if ex-count
	(save-excursion
	  (if ex-addresses (goto-char (car ex-addresses)))
	  (set-mark (point))
	  (forward-line (1- ex-count))
	  (setq ex-addresses (cons (point) (cons (mark t) nil))))
      (if (null ex-addresses)
	  (setq ex-addresses (cons (point) (cons (point) nil)))
	(if (null (cdr ex-addresses))
	    (setq ex-addresses (cons (car ex-addresses) ex-addresses)))))
					;(setq G opt-g)
    (let ((beg (car ex-addresses)) (end (car (cdr ex-addresses)))
	  (cont t) eol-mark)
      (save-excursion
	(vip-enlarge-region beg end)
	(let ((limit (save-excursion
		       (goto-char (max (point) (mark t)))
		       (point-marker))))
	  (goto-char (min (point) (mark t)))
	  (while (< (point) limit)
	    (end-of-line)
	    (setq eol-mark (point-marker))
	    (beginning-of-line)
	    (if opt-g
		(progn
		  (while (and (not (eolp))
			      (re-search-forward pat eol-mark t))
		    (if (or (not opt-c) (y-or-n-p "Replace? "))
			(progn
			  (setq matched-pos (point))
			  (replace-match repl))))
		  (end-of-line)
		  (forward-char))
	      (if (and (re-search-forward pat eol-mark t)
		       (or (not opt-c) (y-or-n-p "Replace? ")))
		  (progn
		    (setq matched-pos (point))
		    (replace-match repl)))
	      (end-of-line)
	      (forward-char))))))
    (if matched-pos (goto-char matched-pos))
    (beginning-of-line)
    (if opt-c (message "done"))))

(defun ex-tag ()
  "ex tag"
  (let (tag)
    (save-window-excursion
      (set-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (set-mark (point))
      (skip-chars-forward "^ |\t\n")
      (setq tag (buffer-substring (mark t) (point))))
    (if (not (string= tag "")) (setq ex-tag tag))
    (vip-change-mode-to-emacs)
    (condition-case conditions
	(progn
	  (if (string= tag "")
	      (find-tag ex-tag t)
	    (find-tag-other-window ex-tag))
	  (vip-change-mode-to-vi))
      (error
       (vip-change-mode-to-vi)
       (vip-message-conditions conditions)))))

(defun ex-write (q-flag)
  "ex write"
  (vip-default-ex-addresses t)
  (vip-get-ex-file)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))) 
	size old-point ret ex-write-cmd)
    (if (> beg end) (error "First address exceeds second"))
    (if ex-cmdfile
	(progn
	  (vip-enlarge-region beg end)
	  (shell-command-on-region (point) (mark t) ex-file))
      (if (string= ex-file "")
	  (progn
	    (if (null buffer-file-name)
		(error "No file associated with this buffer"))
	    (setq ex-file buffer-file-name))
	(setq ex-file (expand-file-name ex-file)))
      (if (and (not (string= ex-file (buffer-file-name)))
	       (file-exists-p ex-file)
	       (not ex-variant))
	  (error (format "\"%s\" File exists - use w! to override" ex-file)))
      (save-excursion
	(vip-enlarge-region beg end)
	(write-region (point) (mark t) ex-file ex-append 1)
	(setq size (- (point) (mark t)))
	(if (string= buffer-file-name ex-file) 
	    (progn
	      (set-buffer-modified-p nil)
	      (clear-visited-file-modtime)
	      (delete-auto-save-file-if-necessary))))
;;      (ex-write-info (file-exists-p ex-file) ex-file size)
      (message "\"%s\"%s %d lines, %d characters"
	       ex-file
	       (if (file-exists-p ex-file) "" " [New file]")
	       (count-lines beg end) size)
      (if (null buffer-file-name) (setq buffer-file-name ex-file))
      (if q-flag
	  (if vip-is-vi
	      (save-buffers-kill-emacs)
	    (vip-ex-kill-buf (current-buffer)))))))

;; (defun ex-write-info(exists file-name size)
;;   (message "\"%s\"%s %d lines, %d characters"
;; 	   file-name
;; 	   (if exists "" " [New file]")
;; 	   (count-lines beg end) size))

(defun ex-yank ()
  "ex yank"
  (vip-default-ex-addresses)
  (vip-get-ex-buffer)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error "First address exceeds second"))
    (save-excursion
      (vip-enlarge-region beg end)
      (exchange-point-and-mark)
      (if (or ex-g-flag ex-g-variant) (error "Can't yank within global"))
      (if ex-count
	  (progn
	    (set-mark (point))
	    (forward-line (1- ex-count)))
	(set-mark end))
      (vip-enlarge-region (point) (mark t))
      (if ex-flag (error "Extra chacters at end of command"))
      (if ex-buffer
	  (copy-to-register ex-buffer (point) (mark t) nil))
      (copy-region-as-kill (point) (mark t)))))

(defun ex-command ()
  "execute shell command"
  (let (command)
    (save-window-excursion
      (set-buffer " *ex-working-space*")
      (skip-chars-forward " \t")
      (setq command (buffer-substring (point) (point-max)))
      (end-of-line))
    (setq command (ex-expand-filsyms command (current-buffer)))
    (if (string= "!" (substring command 0 1))
	(if vip-ex-last-shell-com
	    (setq command (concat vip-ex-last-shell-com (substring command 1)))
	  (error "No previous shell command")))
    (setq vip-ex-last-shell-com command)
    (if (null ex-addresses)
	(shell-command command)
      (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
	(if (null beg) (setq beg end))
	(save-excursion
	  (goto-char beg)
	  (set-mark end)
	  (vip-enlarge-region (point) (mark t))
	  (shell-command-on-region (point) (mark t) command t))
	(goto-char beg)))))

(defun ex-line-no ()
  "print line number"
  (message "%d" (save-excursion
		  (beginning-of-line)
		  (1+ (count-lines (point-min) (point))))))

;; 	   (1+ (count-lines
;; 		(point-min)
;; 		(if (null ex-addresses) (point-max) (car ex-addresses))))))

(if (file-exists-p vip-custom-file-name) (load vip-custom-file-name))

;; Emacs customisations for vip usage - runs after (load .vip)

;; (if vip-make-cc-quit
;;     (set-input-mode nil nil ?\^c))

(if vip-want-history (vip-get-history))

(if vip-always
    (progn
      (setq term-setup-hook 'vip-mode)
      (setq default-major-mode 'vip-mode)
      
      (defun vip-tmp-hook() (vip-mode))
      (defun vip-set-hook(h)
	(if (or (not (listp h)) (and h (equal (car h) 'lambda)))
	    (setq h (list h)))
	(or (memq 'vip-tmp-hook h)
	    (setq h (cons 'vip-tmp-hook h))))
      
      (defvar emacs-lisp-mode-hook nil)
      (setq emacs-lisp-mode-hook (vip-set-hook emacs-lisp-mode-hook))
      
      (defvar TeX-mode-hook nil)
      (setq TeX-mode-hook (vip-set-hook TeX-mode-hook))
      
      (defvar c-mode-hook nil)
      (setq c-mode-hook (vip-set-hook c-mode-hook))
      
      (defvar c++-mode-hook nil)
      (setq c++-mode-hook (vip-set-hook c++-mode-hook))
      
      (defvar lisp-interaction-mode-hook nil)
      (setq lisp-interaction-mode-hook
	    (vip-set-hook lisp-interaction-mode-hook))
      
      (defvar text-mode-hook nil)
      (setq text-mode-hook (vip-set-hook text-mode-hook))
      
      ))

;;; vip.el ends here
