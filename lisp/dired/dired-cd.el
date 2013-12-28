;;; -*- Mode: Emacs-lisp -*- ;;;
;;; dired-cd.el - Adjust Working Directory for Tree Dired Shell Commands 
;;; Id: dired-cd.el,v 1.14 1991/11/01 14:28:27 sk RelBeta 
;;; Copyright (C) 1991 Hugh Secker-Walker
;;;
;;; Author:  Hugh Secker-Walker   hugh@ear-ache.mit.edu
;;;
;;; Modified by Sebastian Kremer <sk@thp.uni-koeln.de>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to the above address) or from
;;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LISPDIR ENTRY for the Elisp Archive ===============================
;;    LCD Archive Entry:
;;    dired-cd|Hugh Secker-Walker|hugh@ear-ache.mit.edu
;;    |Adjust Working Directory for Tree Dired Shell Commands 
;;    |Date: 1991/11/01 14:28:27 |Revision: 1.14 |

;;; SUMMARY

;;; This extension to Sebastian Kremer's (sk@thp.Uni-Koeln.DE) Tree-Dired
;;; permits the working directory of the dired shell commands
;;; dired-do-shell-command and dired-do-background-shell-command
;;; to be the files' subdirectory under certain circumstances.
;;; Loading this extension does not change the behavior of dired until
;;; the variables dired-cd-same-subdir and/or dired-cd-on-each are
;;; non-nil.  


;;; FUNCTIONALITY PROVIDED

;;; If dired-cd-same-subdir is non-nil and if all the selected files
;;; (marked, non-zero numeric ARG, etc.) are in the same directory, then
;;; dired-do-shell-command and dired-do-background-shell-command will
;;; cause the shell to perform a cd into that directory before the
;;; commands are executed.  Also, the selected filenames will be provided
;;; to the command without any directory components.

;;; If dired-cd-on-each is non-nil and if the on-each option is specified
;;; (numeric arg of zero), then dired-do-shell-command and
;;; dired-do-background-shell-command will perform a cd into the
;;; directory of each file before the commands on that file are executed.
;;; Also, each filename will be provided to the command without any
;;; directory components.  Note that this on-each behavior occurs
;;; regardless of whether the files are all in the same directory or not.

;;; After the above "cd wrapping" has occured, the existing
;;; dired-shell-stuff-it is used to do file-name substitution and
;;; quoting, so custom versions of this procedure should work, e.g.
;;; dired-trans will transform commands correctly.  However, since
;;; filenames lack any directory components, features that use the
;;; directory components will fail, e.g. the dired-trans [d] transform
;;; specifier will be empty.

;;; New variables (user options):
;;;    dired-cd-same-subdir
;;;    dired-cd-on-each
;;;
;;; Replaces procedures:
;;;    dired-do-shell-command  (new doc and prompt, calls dired-cd-wrap-it)
;;;
;;; Adds procedures:
;;;    dired-cd-wrap-it  (wraps calls to dired-shell-stuff-it with "cd <dir>")
;;;    dired-files-same-directory


;; INSTALLATION
;;
;; Put this file into your load-path and add (load "dired-cd") to
;; your dired-load-hook, e.g.
;;
;; (setq dired-load-hook '(lambda ()
;; 			  ;; possibly more statements here
;;			  (load "dired-cd")))
;;
;; Do (setq dired-cd-same-subdir t) and perhaps (setq dired-cd-on-each t)
;; in your .emacs.  By default, dired-cd doesn't change the behavior of 
;; dired when it is loaded. 
;;
;; If dired-cd-same-subdir is non-nil, then the shell commands cd to
;; the appropriate directory if all the selected files (marked,
;; numeric ARG, etc.) are in that directory; however, on-each behavior
;; is not changed.
;;
;; If dired-cd-on-each is non-nil, then each instance of the command
;; for an on-each shell command runs in the file's directory
;; regardless of whether the files are all in the same directory.


(defvar dired-cd-same-subdir nil
  "*If non-nil, and selected file(s) (by marks, numeric arg, \\[universal-argument]) are in same
subdir, causes dired shell command to run in that subdir.  Filenames provided
to shell commands are stripped of their directory components.  Does not
affect behavior of on-each, for that see variable dired-cd-on-each.")

(defvar dired-cd-on-each nil
  "*If non-nil, on-each causes each dired shell command to run in the 
file's directory.  Filenames provided to shell commands are stripped of 
their directory components.  Also see variable dired-cd-same-subdir.")

;; Redefines dired.el's version.
;; Changes to documentation and prompt, and uses dired-cd-wrap-it.
(defun dired-do-shell-command (&optional arg in-background)
  "Run a shell command on the marked files.
If there is output, it goes to a separate buffer.
The list of marked files is appended to the command string unless asterisks
  `*' indicate the place(s) where the list should go.
If no files are marked or a specific numeric prefix arg is given, uses
  next ARG files.  With a zero argument, run command on each marked file
  separately: `cmd * foo' results in `cmd F1 foo; ...; cmd Fn foo'.
  As always, a raw arg (\\[universal-argument]) means the current file.
The option variables dired-cd-same-subdir and dired-cd-on-each
  permit the command\(s\) to run in the files' directories if appropriate,
  and thus determine where output files are created.  Default is top
  directory.  The prompt mentions the file(s) or the marker, the cd subdir,
  and the on-each flags when they apply.
No automatic redisplay is attempted, as the file names may have
  changed.  Type \\[dired-do-redisplay] to redisplay the marked files."
  ;; Function dired-shell-stuff-it (called by dired-cd-wrap-it) does the
  ;; actual file-name substitution and can be redefined for customization.
  (interactive "P")
  (let* ((on-each (equal arg 0))
	 (file-list (dired-mark-get-files t (if on-each nil arg)))
	 (prompt (concat (if in-background "& " "! ")
			 (if (or (and on-each dired-cd-on-each)
				 (and dired-cd-same-subdir
				      (not on-each)
				      (dired-files-same-directory file-list)))
			     "cd <dir>; " "")
			 "on "
			 (if on-each "each " "")
			 "%s: "))
	 ;; Give feedback on file(s) and working directory status
	 (command (dired-read-shell-command
		   prompt (if on-each nil arg) file-list))
	 (result (dired-cd-wrap-it command file-list on-each arg)))
    ;; execute the shell command
    (dired-run-shell-command result in-background)))

(defun dired-cd-wrap-it (command files on-each &optional raw)
  "Args COMMAND FILES ON-EACH &optional RAW-ARG, like dired-shell-stuff-it.
Calls dired-shell-stuff-it, but wraps the resulting command\(s\)
with \"cd <dir>\" commands when appropriate.  Note: when ON-EACH is non-nil, 
dired-shell-stuff-it is called once for each file in FILES.
See documentation of variables dired-cd-same-subdir and dired-cd-on-each 
for wrap conditions." 
  (if on-each;; command applied to each file separately
      ;; cd's are done in subshells since all shells I know of have subshells
      (let* ((cwd "");; current working directory
	     (in-subshell nil)
	     (cmd (mapconcat;; files over command, fuss with "cd <dir>"
		   (function
		    (lambda (file)
		      (let ((cd "") d);; cd command and file's directory
			(if (not dired-cd-on-each) nil;; poor man's (when ...)
			  (setq d;; directory, relative to default-directory
				(directory-file-name 
				 (or (file-name-directory file) ""))
				file (file-name-nondirectory file))
			  (if (not (string= d cwd));; new subdir, new subshell
			      (setq cwd d
				    ;; close existing subshell, 
				    ;; open a new one
				    cd (concat (if in-subshell "); " "") 
					       "(cd " (shell-quote cwd) "; ")
				    in-subshell t))
			  )
			;; existing dired-shell-stuff-it does 
			;; actual command substitution
			(concat cd (dired-shell-stuff-it command (list file) 
							 on-each raw)))))
		   files "; ")))
	(if in-subshell (concat cmd ")") cmd));; close an open subshell
    
    ;; not on-each, all files are args to single command instance
    (let ((same-dir (and dired-cd-same-subdir
			 (dired-files-same-directory files nil)))
	  (cd ""))
      ;; Let the prepended cd command be relative to default-directory,
      ;; and only give it if necessary.  This way, after ange-ftp
      ;; prepends its own cd command, it will still work.
      ;; sk  3-Sep-1991 14:23
      ;; hsw 31-Oct-1991 -- filenames relative to default-directory
      (if (and same-dir (not (equal same-dir "")))
	  (setq files (mapcar (function file-name-nondirectory) files)
		cd  (concat "cd " (shell-quote same-dir) "; ")))
      ;; existing dired-shell-stuff-it does the command substitution
      (concat cd (dired-shell-stuff-it command files on-each raw)))))

(defun dired-files-same-directory (file-list &optional absolute)
  "If all files in LIST are in the same directory return it, otherwise nil.
Returned name has no trailing slash.  \"Same\" means file-name-directory of
the files are string=.  File names in LIST must all be absolute or all be
relative.  Implicitly, relative file names are in default-directory.  If
optional ABS is non-nil, the returned name will be absolute, otherwise the
returned name will be absolute or relative as per the files in LIST."
  (let ((dir (file-name-directory (car file-list))))
    (if (memq nil (mapcar (function
			   (lambda (file)
			     (string= dir (file-name-directory file))))
			  file-list))
	nil
      (directory-file-name
       (if (or (not absolute) (and dir (file-name-absolute-p dir)))
	   (or dir "")
	 (concat default-directory dir))))))

(provide 'dired-cd)
