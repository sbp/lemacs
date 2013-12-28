;; Load up standardly loaded Lisp files for Emacs.
;;
;; It is not a good idea to edit this file.  Use site-init.el or site-load.el
;; instead.
;;
;; This is loaded into a bare Emacs to make a dumpable one.
;; Copyright (C) 1985, 1986, 1992, 1993 Free Software Foundation, Inc.

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


;; We don't want to have any undo records in the dumped Emacs.
(buffer-disable-undo (get-buffer "*scratch*"))

;; lread.c has prepended "../lisp/prim" to load-path, which is how this file
;; has been found.  At this point, enough of emacs has been initialized that 
;; we can call directory-files and get the rest of the dirs (so that we can
;; dump stuff from modes/ and packages/.)
;;
(setq load-path (nconc (directory-files "../lisp" t "^[^-.]" nil 'dirs-only)
		       (cons "../lisp" load-path)))

(setq load-warn-when-source-newer t ; set to nil at the end
      load-warn-when-source-only t)

(load "subr")
(garbage-collect)
(load "minibuf")
(load "faces")		; must be loaded before any file that makes faces
(garbage-collect)
(load "loaddefs.el")  ;Don't get confused if someone compiled loaddefs by mistake.
(load "keydefs.el")
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

(if (fboundp 'x-create-screen)	; preload the X code, for faster startup.
    (progn
      (require 'screen)
      (require 'menubar)
      (require 'x-faces)
      (require 'x-iso8859-1)
      (require 'x-mouse)
      (require 'xselect)
      ))

(load "version.el")  ;Don't get confused if someone compiled version.el by mistake.

(load "bytecomp-runtime")  ; needs version.el to know what emacs it's in.

(if (fboundp (intern-soft "handle-energize-request"))
    (load "energize/energize-load.el"))

(garbage-collect)


(setq load-warn-when-source-newer nil ; set to t at top of file
      load-warn-when-source-only nil)


;If you want additional libraries to be preloaded and their
;doc strings kept in the DOC file rather than in core,
;you may load them with a "site-load.el" file.
;But you must also cause them to be scanned when the DOC file
;is generated.  For VMS, you must edit ../etc/makedoc.com.
;For other systems, you must edit ../src/ymakefile.
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
      (message "Finding pointers to doc strings...")
      (if (fboundp 'dump-emacs)
	  (let ((name emacs-version))
	    (while (string-match "[^-+_.a-zA-Z0-9]+" name)
	      (setq name (concat
			  (downcase (substring name 0 (match-beginning 0)))
			  "-"
			  (substring name (match-end 0)))))
	    (copy-file (expand-file-name "../etc/DOC")
		       (concat (expand-file-name "../etc/DOC-") name)
		       t)
	    (Snarf-documentation (concat "DOC-" name)))
	(Snarf-documentation "DOC"))
      (message "Finding pointers to doc strings...done")
;;      (Verify-documentation)
      ))

;Note: You can cause additional libraries to be preloaded
;by writing a site-init.el that loads them.
;See also "site-load" above.
(load "site-init" t)

(garbage-collect)

;; At this point, we're ready to resume undo recording for scratch.
(buffer-enable-undo "*scratch*")

(if (or (equal (nth 3 command-line-args) "dump")
	(equal (nth 4 command-line-args) "dump"))
    (if (eq system-type 'vax-vms)
	(progn 
	  (message "Dumping data as file temacs.dump")
	  (dump-emacs "temacs.dump" "temacs")
	  (kill-emacs))
      (let ((name (concat "emacs-" emacs-version)))
	(while (string-match "[^-+_.a-zA-Z0-9]+" name)
	  (setq name (concat (downcase (substring name 0 (match-beginning 0)))
			     "-"
			     (substring name (match-end 0)))))
	(message "Dumping under names xemacs and %s" name))
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
	(add-name-to-file "xemacs" name t))
      (kill-emacs)))

(if (or (equal (elt command-line-args 3) "run-temacs")
	(equal (elt command-line-args 4) "run-temacs"))
    (progn
      (princ "\nSnarfing doc...\n" (function external-debugging-output))
      (Snarf-documentation "DOC")
;;      (Verify-documentation)
      (princ "\nBootstrapping from temacs...\n"
	     (function external-debugging-output))
      (setq purify-flag nil)
      (apply (function run-emacs-from-temacs)
             (nthcdr (if (equal (elt command-line-args 3) "run-temacs")
                         4 5)
                     command-line-args))
      ;; run-emacs-from-temacs doesn't actually return anyway.
      (kill-emacs)))

;; Avoid error if user loads some more libraries now.
(setq purify-flag nil)

;; For machines with CANNOT_DUMP defined in config.h,
;; this file must be loaded each time Emacs is run.
;; So run the startup code now.

(or (fboundp 'dump-emacs)
    (eval top-level))
