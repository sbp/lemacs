;;; -*- Mode: Emacs-Lisp -*-

;;; This is a sample .emacs file.
;;;
;;; The .emacs file, which should reside in your home directory, allows you to
;;; customize the behavior of Emacs.  In general, changes to your .emacs file
;;; will not take effect until the next time you start up Emacs.  You can load
;;; it explicitly with `M-x load-file RET ~/.emacs RET'.
;;;
;;; There is a great deal of documentation on customization in the Emacs
;;; manual.  You can read this manual with the online Info browser: type
;;; `C-h i' or select "Emacs Info" from the "Help" menu.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			Basic Customization			    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable the commands `narrow-to-region' ("C-x n n") and 
;; `eval-expression' ("M-ESC", or "ESC ESC").  Both are useful
;; commands, but they can be confusing for a new user, so they're
;; disabled by default.
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)


;; Make the sequence "C-x C-j" execute the `goto-line' command, 
;; which prompts for a line number to jump to.
(global-set-key "\C-x\C-j" 'goto-line)


(cond ((string-match "Lucid" emacs-version)
       ;;
       ;; Code for any version of Lucid Emacs goes here
       ;;

       ;; Change the values of some variables.
       ;; (t means true; nil means false.)
       ;;
       ;; Use the "Describe Variable..." option on the "Help" menu
       ;; to find out what these variables mean.
       (setq find-file-use-truenames nil
	     find-file-compare-truenames t
	     minibuffer-confirm-incomplete t
	     complex-buffers-menu-p t
	     next-line-add-newlines nil
	     mail-yank-prefix "> "
	     )

       ;; When running ispell, consider all 1-3 character words as correct.
       (setq ispell-extra-args '("-W" "3"))

       (if (featurep 'sunpro)
	   (progn
	     (setq-default mode-line-buffer-identification '("lemacs: %17b"))
	     (setq mode-line-buffer-identification '("lemacs: %17b")))
	   (progn
	     (setq-default mode-line-buffer-identification '("Xemacs: %17b"))
	     (setq mode-line-buffer-identification '("Xemacs: %17b"))))

       (cond ((eq window-system 'x)
	      ;;
	      ;; Code which applies only when running emacs under X goes here.
	      ;; (Currently, this is always the case in lemacs, but it will
	      ;; not be in the future.)
	      ;;

	      ;; Remove the binding of C-x C-c, which normally exits emacs.
	      ;; It's easy to hit this by mistake, and that can be annoying.
	      ;; Under X, you can always quit with the "Exit Emacs" option on
	      ;; the File menu.
	      (global-set-key "\C-x\C-c" nil)

	      ;; This changes the variable which controls the text that goes
	      ;; in the top window title bar.  (However, it is not changed
	      ;; unless it currently has the default value, to avoid
	      ;; interfering with a -wn command line argument I may have
	      ;; started emacs with.)
	      (if (equal screen-title-format "%S: %b")
		  (setq screen-title-format
			(concat "%S: " execution-path " [" emacs-version "]"
				(if nil ; (getenv "NCD")
				    ""
				  "   %b"))))

	      ;; If we're running on display 0, load some nifty sounds that
	      ;; will replace the default beep.  But if we're running on a
	      ;; display other than 0, which probably means my NCD X terminal,
	      ;; which can't play digitized sounds, do two things: reduce the
	      ;; beep volume a bit, and change the pitch of the sound that is
	      ;; made for "no completions."
	      ;;
	      ;; (Note that sampled sounds only work if lemacs was compiled
	      ;; with sound support, and we're running on the console of a
	      ;; Sparc, HP, or SGI machine, or on a machine which has a
	      ;; NetAudio server; otherwise, you just get the standard beep.)
	      ;;
	      ;; (Note further that changing the pitch and duration of the
	      ;; standard beep only works with some X servers; many servers
	      ;; completely ignore those parameters.)
	      ;;
	      (cond ((string-match ":0" (getenv "DISPLAY"))
		     (load-default-sounds))
		    (t
		     (setq bell-volume 40)
		     (setq sound-alist
			   (append sound-alist '((no-completion :pitch 500))))
		     ))
	      ))

       ;;
       ;; (The following code applies whether or not we're running X.)
       ;;

       ;; Change the binding of mouse button 2, so that it inserts the
       ;; selection at point (where the text cursor is), instead of at
       ;; the position clicked.
       ;;
       ;; Note that you can find out what a particular key sequence or
       ;; mouse button does by using the "Describe Key..." option on
       ;; the Help menu.
       (define-key global-map 'button2 'x-insert-selection)

       ;; LISPM bindings of Control-Shift-C and Control-Shift-E.
       ;; Note that "\C-C" means Control-C, not Control-Shift-C.
       ;; To specify shifted control characters, you must use the
       ;; more verbose syntax used here.
       (define-key emacs-lisp-mode-map '(control C) 'compile-defun)
       (define-key emacs-lisp-mode-map '(control E) 'eval-defun)

       ;; Make backspace and delete be the same.  This doesn't work in all
       ;; cases; a better way would be to use xmodmap.
       (global-set-key 'backspace [delete])
       (global-set-key '(meta backspace) [(meta delete)])
       (global-set-key '(control backspace) [(control delete)])
       (global-set-key '(meta control backspace) [(meta control delete)])

       ;; Make F5 be `undo'
       (global-set-key 'f5 'undo)

       ;; Make F6 be `save-file' followed by `delete-window'.
       (global-set-key 'f6 "\C-x\C-s\C-x0")

       ;; Make `C-x C-m' and `C-x RET' be different (since I tend to type
       ;; the latter by accident sometimes.)
       (define-key global-map [(control x) return] nil)

       ;; Change the cursor used when the mouse is over a mode line
       (setq x-mode-pointer-shape "leftbutton")

       ;; Change the cursor used during garbage collection.
       ;;
       ;; Note that this cursor image is rather large as cursors go, and so it
       ;; won't work on some X servers (such as the MIT R5 Sun server) because
       ;; servers may have lamentably small upper limits on cursor size.
       ;(if (featurep 'xpm)
       ;   (setq x-gc-pointer-shape
       ;	 (expand-file-name "trash.xpm" data-directory)))

       ;; Here's another way to do that: it first tries to load the cursor
       ;; once and traps the error, just to see if it's possible to load that
       ;; cursor on this system; if it is, then it sets x-gc-pointer-shape,
       ;; because we knows that will work.  Otherwise, it doesn't change that
       ;; variable because we know it will just cause some error messages.
       (if (featurep 'xpm)
	   (let ((file (expand-file-name "recycle.xpm" data-directory)))
	     (if (condition-case error
		     (make-cursor file) ; returns a cursor if successful.
		   (error nil))	    ; returns nil if an error occurred.
		 (setq x-gc-pointer-shape file))))
	 
       ;; Add `dired' to the File menu
       (add-menu-item '("File") "Edit Directory" 'dired t)

       ;; Here's a way to add scrollbar-like buttons to the menubar
       (add-menu-item nil "Top" 'beginning-of-buffer t)
       (add-menu-item nil "<<<" 'scroll-down t)
       (add-menu-item nil " . " 'recenter t)
       (add-menu-item nil ">>>" 'scroll-up t)
       (add-menu-item nil "Bot" 'end-of-buffer t)

       ))

;;; Older versions of emacs did not have these variables
;;; (emacs-major-version and emacs-minor-version.)
;;; Let's define them if they're not around, since they make
;;; it much easier to conditionalize on the emacs version.

(if (and (not (boundp 'emacs-major-version))
	 (string-match "^[0-9]+" emacs-version))
    (setq emacs-major-version
	  (string-to-int (substring emacs-version
				    (match-beginning 0) (match-end 0)))))
(if (and (not (boundp 'emacs-minor-version))
	 (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version))
    (setq emacs-minor-version
	  (string-to-int (substring emacs-version
				    (match-beginning 1) (match-end 1)))))


(cond ((and (string-match "Lucid" emacs-version)
	    (>= emacs-major-version 19)
	    (>= emacs-minor-version 6))
       ;;
       ;; Code which requires Lucid Emacs version 19.6 or newer goes here
       ;;
       ))

(cond ((>= emacs-major-version 19)
       ;;
       ;; Code for any vintage-19 emacs goes here
       ;;
       ))

(cond ((and (not (string-match "Lucid" emacs-version))
	    (= emacs-major-version 19))
       ;;
       ;; Code specific to FSF Emacs 19 (not Lucid Emacs) goes here
       ;;
       ))

(cond ((< emacs-major-version 19)
       ;;
       ;; Code specific to emacs 18 goes here
       ;;
       ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		Customization of Specific Packages		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; ********************
;;; Load ange-ftp, which uses the FTP protocol as a pseudo-filesystem.
;;; When this is loaded, the pathname syntax /user@host:/remote/path
;;; refers to files accessible through ftp.
;;;
(require 'dired)
(require 'ange-ftp)
(setq ange-ftp-default-user "anonymous"      ; id to use for /host:/remote/path
      ange-ftp-generate-anonymous-password t ; use $USER@`hostname`
      ange-ftp-binary-file-name-regexp "."   ; always transfer in binary mode
      )


;;; ********************
;;; Load the auto-save.el package, which lets you put all of your autosave
;;; files in one place, instead of scattering them around the file system.
;;;
(setq auto-save-directory (expand-file-name "~/autosaves/")
      auto-save-directory-fallback auto-save-directory
      auto-save-hash-p nil
      ange-ftp-auto-save t
      ange-ftp-auto-save-remotely nil
      ;; now that we have auto-save-timeout, let's crank this up
      ;; for better interactive response.
      auto-save-interval 2000
      )
(require 'auto-save)


;;; ********************
;;; Load much improved versions of the C and C++ modes.  At some point this
;;; will become the default in Lucid Emacs.
;;;
(fmakunbound 'c-mode)
(fmakunbound 'c++-mode)
(makunbound 'c-mode-map)
(makunbound 'c++-mode-map)
(makunbound 'c-style-alist)

(autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode   "cc-mode" "C Editing Mode" t)

(setq auto-mode-alist
      (append '(("\\.C$"  . c++-mode)
		("\\.cc$" . c++-mode)
		("\\.hh$" . c++-mode)
		("\\.c$"  . c-mode)
		("\\.h$"  . c-mode))
	      auto-mode-alist))

;; This controls indentation.  The default is 4 spaces but the
;; Emacs source code uses 2.
(setq c-basic-offset 2)


;;; ********************
;;; Load a partial-completion mechanism, which makes minibuffer completion
;;; search multiple words instead of just prefixes; for example, the command
;;; `M-x byte-compile-and-load-file RET' can be abbreviated as `M-x b-c-a RET'
;;; because there are no other commands whose first three words begin with
;;; the letters `b', `c', and `a' respectively.
;;;
(load-library "completer")


;;; ********************
;;; Load crypt, which is a package for automatically decoding and reencoding
;;; files by various methods - for example, you can visit a .Z or .gz file,
;;; edit it, and have it automatically re-compressed when you save it again.
;;; 
(setq crypt-encryption-type 'pgp   ; default encryption mechanism
      crypt-confirm-password t	   ; make sure new passwords are correct
      ;crypt-never-ever-decrypt t  ; if you don't encrypt anything, set this to
				   ; tell it not to assume that "binary" files
				   ; are encrypted and require a password.
      )
(require 'crypt)


;;; ********************
;;; Edebug is a source-level debugger for emacs-lisp programs.
;;;
(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)


;;; ********************
;;; Font-Lock is a syntax-highlighting package.  When it is enabled and you
;;; are editing a program, different parts of your program will appear in
;;; different fonts or colors.  For example, with the code below, comments
;;; appear in red italics, function names in function definitions appear in
;;; blue bold, etc.  The code below will cause font-lock to automatically be
;;; enabled when you edit C, C++, Emacs-Lisp, and many other kinds of
;;; programs.
;;;
;;; The "Options" menu has some commands for controlling this as well.
;;;
(cond ((string-match "Lucid" emacs-version)
       (require 'font-lock)
       (set-face-foreground 'font-lock-function-name-face "blue")
       (set-face-foreground 'font-lock-comment-face "red")
       (set-face-foreground 'font-lock-string-face "forest green")
       (set-face-underline-p 'font-lock-string-face nil)
       (make-face-unitalic 'font-lock-string-face)
       (make-face-unitalic 'font-lock-function-name-face)
       (add-hook 'emacs-lisp-mode-hook	'turn-on-font-lock)
       (add-hook 'lisp-mode-hook	'turn-on-font-lock)
       (add-hook 'c-mode-hook		'turn-on-font-lock)
       (add-hook 'c++-mode-hook		'turn-on-font-lock)
       (add-hook 'perl-mode-hook	'turn-on-font-lock)
       (add-hook 'tex-mode-hook		'turn-on-font-lock)
       (add-hook 'texinfo-mode-hook	'turn-on-font-lock)
       (add-hook 'postscript-mode-hook	'turn-on-font-lock)
       (add-hook 'dired-mode-hook	'turn-on-font-lock)
       (add-hook 'ada-mode-hook		'turn-on-font-lock)
       ))


;;; ********************

;;; func-menu is a package that scans your source file for function definitions
;;; and makes a menubar entry that lets you jump to any particular function
;;; definition by selecting it from the menu.  The following code turns this on
;;; for all of the recognized languages.  Scanning the buffer takes some time,
;;; but not much.
;;;
(cond ((string-match "Lucid" emacs-version)
       (require 'func-menu)
       (define-key global-map 'f8 'function-menu)
       (add-hook 'find-file-hooks 'fume-add-menubar-entry)
       (define-key global-map "\C-cg" 'fume-prompt-function-goto)
       (define-key global-map '(shift button3) 'mouse-function-menu)
       ))


;;; ********************
;;; MH is a mail-reading system from the Rand Corporation that relies on a
;;; number of external filter programs (which do not come with emacs.)
;;; Emacs provides a nice front-end onto MH, called "mh-e".
;;;
;; Bindings that let you send or read mail using MH
;(global-set-key "\C-xm" 'mh-smail)
;(global-set-key "\C-x4m" 'mh-smail-other-window)
;(global-set-key "\C-cr" 'mh-rmail)

;; Customization of MH behavior.
(setq mh-delete-yanked-msg-window t)
(setq mh-yank-from-start-of-msg 'body)
(setq mh-summary-height 11)

;; Use lines like the following if your version of MH
;; is in a special place.
;(setq mh-progs "/usr/dist/pkgs/mh/bin.svr4/")
;(setq mh-lib "/usr/dist/pkgs/mh/lib.svr4/")


;;; ********************
;;; W3 is a browser for the World Wide Web, and takes advantage of the very
;;; latest redisplay features in Lucid Emacs.  You can access it simply by
;;; typing 'M-x w3'; however, if you're unlucky enough to be on a machine
;;; that is behind a firewall, you will have to do something like this first:

;(setq w3-use-telnet t
;      ;;
;      ;; If the Telnet program you use to access the outside world is
;      ;; not called "telnet", specify its name like this.
;      w3-telnet-prog "itelnet"
;      ;;
;      ;; If your Telnet program adds lines of junk at the beginning
;      ;; of the session, specify the number of lines here.
;      w3-telnet-header-length 4
;      )
