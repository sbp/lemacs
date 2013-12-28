;; Load up standardly loaded Lisp files for Emacs.
;;
;; It is not a good idea to edit this file.  Use site-init.el or site-load.el
;; instead.
;;
;; This is loaded into a bare Emacs to make a dumpable one.
;; Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

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

(if (fboundp 'error)
    (error (gettext "loadup.el already loaded!")))

(setq debug-on-error t)
(setq debugger (function (lambda (&rest x)
                 (setq debugger nil debug-on-error nil)
                 (princ (gettext "*** Error in Emacs initialisation") 
                        'external-debugging-output)
                 (print x 'external-debugging-output)
                 (princ (gettext "*** Backtrace\n") 'external-debugging-output)
                 (backtrace 'external-debugging-output t)
                 (princ (gettext "*** Killing Emacs\n")
			'external-debugging-output)
                 (kill-emacs -1))))

;; We don't want to have any undo records in the dumped Emacs.
(buffer-disable-undo (get-buffer (gettext "*scratch*")))

;; lread.c (or src/Makefile.in.in) has prepended "${srcdir}/../lisp/prim" to
;; load-path, which is how this file has been found.  At this point, enough
;; of emacs has been initialized that we can call directory-files and get the
;; rest of the dirs (so that we can dump stuff from modes/ and packages/.)
;;
(let ((temp-path (expand-file-name ".." (car load-path))))
  (setq load-path (nconc (directory-files temp-path t "^[^-.]" nil 'dirs-only)
			 (cons temp-path load-path))))

(setq load-warn-when-source-newer t ; set to nil at the end
      load-warn-when-source-only t)

(load "version.el")  ;Don't get confused if someone compiled version.el by mistake.
(load "bytecomp-runtime")		; define defsubst
(garbage-collect)
(load "subr")
(garbage-collect)
(load "cmdloop")
(or (fboundp 'recursive-edit) (load "cmdloop1"))
(garbage-collect)
(load "keymap")
(load "syntax")
(garbage-collect)
(load "minibuf")
(load "faces")		; must be loaded before any file that makes faces
(garbage-collect)
(load "keydefs.el")	; Before loaddefs, so that the keymap vars exist.
(load "loaddefs.el")	; don't get confused if loaddefs mistakenly compiled.
(garbage-collect)
(load "simple")
(garbage-collect)
(load "help")
(garbage-collect)
(load "files")
(garbage-collect)
(load "indent")
(load "window")
(load "paths.el")  ;Don't get confused if someone compiled paths by mistake.
(garbage-collect)
(load "startup")
(garbage-collect)
(load "lisp")
(garbage-collect)
(load "page")
(load "register")
(garbage-collect)
(load "iso8859-1") ;This must be before any modes (sets standard syntax table)
(load "paragraphs")
(load "lisp-mode")
(garbage-collect)
(load "text-mode")
(load "fill")
(garbage-collect)
(load "c-mode")
(garbage-collect)
(load "isearch-mode")
(garbage-collect)
(load "replace")
(if (eq system-type 'vax-vms)
    (progn
      (garbage-collect)
      (load "vmsproc")))
(garbage-collect)
(load "abbrev")
(garbage-collect)
(load "buff-menu")
(if (eq system-type 'vax-vms)
    (progn
      (garbage-collect)
      (load "vms-patch")))
(if (fboundp 'atan)	; preload some constants and 
    (progn		; floating pt. functions if 
      (garbage-collect)	; we have float support.
      (load "float-sup")))

(load "itimer")	; so that vars auto-save-timeout and auto-gc-threshold work
(garbage-collect)

(if (fboundp 'x-create-screen)	; preload the X code, for faster startup.
    (progn
      (require 'screen)
      (garbage-collect)
      (require 'menubar)
      (garbage-collect)
      (load "x11/x-font-menu")
      (garbage-collect)
      (if (fboundp 'popup-dialog-box)
	  (require 'dialog))
      (require 'x-faces)
      (garbage-collect)
      (require 'x-iso8859-1)
      (garbage-collect)
      (require 'x-mouse)
      (garbage-collect)
      (require 'xselect)
      (garbage-collect)
      ))

(if (fboundp (intern-soft "create-tooltalk-message"))	; #ifdef TOOLTALK
    (progn
      (load "tooltalk/tooltalk-load")
      (garbage-collect)))

(if (fboundp (intern-soft "handle-energize-request"))	; #ifdef ENERGIZE
    (progn
      (load "energize/energize-load.el")
      (garbage-collect)))

(if (fboundp (intern-soft "has-usage-tracking-p"))	; #ifdef SUNPRO
    (progn
      (load "sunpro/sunpro-load.el")
      (garbage-collect)))


(setq load-warn-when-source-newer nil ; set to t at top of file
      load-warn-when-source-only nil)

(setq debugger 'debug)
(setq debug-on-error nil)

(if (or (equal (nth 4 command-line-args) "no-site-file")
	(equal (nth 5 command-line-args) "no-site-file"))
    (setq site-start-file nil))

;If you want additional libraries to be preloaded and their
;doc strings kept in the DOC file rather than in core,
;you may load them with a "site-load.el" file.
;But you must also cause them to be scanned when the DOC file
;is generated.  For VMS, you must edit ../../vms/makedoc.com.
;For other systems, you must edit ../../src/Makefile.in.in.
(if (load "site-load" t)
    (garbage-collect))

;; Note: all compiled Lisp files loaded above this point
;; must be among the ones parsed by make-docfile
;; to construct DOC.  Any that are not processed
;; for DOC will not have doc strings in the dumped Emacs.

;; Don't bother with these if we're running temacs, i.e. if we're
;; just debugging don't waste time finding doc strings.

(if (or (equal (nth 3 command-line-args) "dump")
	(equal (nth 4 command-line-args) "dump"))
    (progn
      (message (gettext "Finding pointers to doc strings..."))
      (if (fboundp 'dump-emacs)
	  (let ((name emacs-version))
	    (while (string-match "[^-+_.a-zA-Z0-9]+" name)
	      (setq name (concat
			  (downcase (substring name 0 (match-beginning 0)))
			  "-"
			  (substring name (match-end 0)))))
	    (if (string-match "-+\\'" name)
		(setq name (substring name 0 (match-beginning 0))))
	    (copy-file (expand-file-name "DOC" "../lib-src")
		       (expand-file-name (concat "DOC-" name) "../lib-src")
		       t)
	    (Snarf-documentation (concat "DOC-" name)))
	(Snarf-documentation "DOC"))
      (message (gettext "Finding pointers to doc strings...done"))
      (Verify-documentation)
      ))

;Note: You can cause additional libraries to be preloaded
;by writing a site-init.el that loads them.
;See also "site-load" above.
(if (stringp site-start-file)
    (load "site-init" t))

;; At this point, we're ready to resume undo recording for scratch.
(buffer-enable-undo (gettext "*scratch*"))

(if (and (eq system-type 'vax-vms)
         (or (equal (nth 3 command-line-args) "dump")
             (equal (nth 4 command-line-args) "dump")))
    (progn
      (setq command-line-args nil)
      (message (gettext "Dumping data as file temacs.dump"))
      (dump-emacs "temacs.dump" "temacs")
      (kill-emacs)))

(if (or (equal (nth 3 command-line-args) "dump")
        (equal (nth 4 command-line-args) "dump"))
    (progn
      (let ((name (concat "emacs-" emacs-version)))
	(while (string-match "[^-+_.a-zA-Z0-9]+" name)
	  (setq name (concat (downcase (substring name 0 (match-beginning 0)))
			     "-"
			     (substring name (match-end 0)))))
	(if (string-match "-+\\'" name)
	    (setq name (substring name 0 (match-beginning 0))))
	(message (gettext "Dumping under names xemacs and %s") name))
      (condition-case ()
	  (delete-file "xemacs")
	(file-error nil))
      (if (fboundp 'really-free)
	  (really-free))
      (dump-emacs "xemacs" "temacs")
      ;; Recompute NAME now, so that it isn't set when we dump.
      (let ((name (concat "emacs-" emacs-version)))
	(while (string-match "[^-+_.a-zA-Z0-9]+" name)
	  (setq name (concat (downcase (substring name 0 (match-beginning 0)))
			     "-"
			     (substring name (match-end 0)))))
	(if (string-match "-+\\'" name)
	    (setq name (substring name 0 (match-beginning 0))))
	(add-name-to-file "xemacs" name t))
      (kill-emacs)))

(if (or (equal (nth 3 command-line-args) "run-temacs")
	(equal (nth 4 command-line-args) "run-temacs"))
    (progn
      ;; purify-flag is nil if called from loadup-el.el.
      (if purify-flag
	  (progn
	    (princ (gettext "\nSnarfing doc...\n") #'external-debugging-output)
	    (Snarf-documentation "DOC")
	    (Verify-documentation)))
      (princ (gettext "\nBootstrapping from temacs...\n")
	     #'external-debugging-output)
      (setq purify-flag nil)
      (apply #'run-emacs-from-temacs
             (nthcdr (if (equal (nth 3 command-line-args) "run-temacs")
                         4 5)
                     command-line-args))
      ;; run-emacs-from-temacs doesn't actually return anyway.
      (kill-emacs)))

;; Avoid error if user loads some more libraries now.
(setq purify-flag nil)

;; If you are using 'recompile', then you should have used -l loadup-el.el
;; so that the .el files always get loaded (the .elc files may be out-of-
;; date or bad).
(if (or (equal (nth 3 command-line-args) "recompile")
	(equal (nth 4 command-line-args) "recompile"))
    (progn
      (let ((command-line-args-left
             (nthcdr (if (equal (nth 3 command-line-args) "recompile")
                         4 5)
                     command-line-args)))
	(batch-byte-recompile-directory)
	(kill-emacs))))


;; For machines with CANNOT_DUMP defined in config.h,
;; this file must be loaded each time Emacs is run.
;; So run the startup code now.

(or (fboundp 'dump-emacs)
    (eval top-level))
