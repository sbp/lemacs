;;; -*- Mode: Emacs-lisp -*- ;;;
;;; dired-guess.el - In Dired, guess what shell command to apply.

;;; Copyright (C) 1991, 1992 Gregory N. Shapiro
;;;
;;; Author:  Gregory N. Shapiro   gshapiro@wpi.wpi.edu
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
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


;;; This extension to Sebastian Kremer's (sk@thp.Uni-Koeln.DE) Tree-Dired
;;; permits dired to guess a shell command to use when the user performs
;;; a shell command on a single file.
;;;
;;; New variables (user options):
;;;    dired-auto-shell-command-alist
;;;    dired-auto-shell-use-last-extension
;;;    dired-guess-have-gnutar
;;;
;;; Replaces procedures:
;;;    dired-read-shell-command  (new doc, calls dired-guess-shell-command)
;;;
;;; Adds procedures:
;;;    dired-guess-shell-command  (guesses command by comparing file extensions
;;;                                to dired-auto-shell-command-alist)

;; LISPDIR ENTRY for the Elisp Archive ===============================
;;    LCD Archive Entry:
;;    dired-guess|Gregory N. Shapiro|gshapiro@wpi.wpi.edu
;;    |Guess a Dired shell command from the filename.

;; INSTALLATION
;;
;; Put this file into your load-path and add (load "dired-guess") to
;; your dired-load-hook, e.g.
;;
;; (setq dired-load-hook '(lambda ()
;; 			  ;; possibly more statements here
;;			  (load "dired-guess")))
;;
;; Note: dired-guess must be loaded after dired-extra.
;;
;; If dired-auto-shell-use-last-extension is nil, all file extensions will
;; be used to determine the command to use.  If nil, use all the
;; extensions.  For example, foo.tar.Z would guess for the .tar.Z extension.
;; If non-nil, uses only the last extension of the filename. For example,
;; foo.tar.Z would use the guess for the .Z extension.
;;
;; Set dired-guess-have-gnutar to the name of the GNU tar file (defaults to 
;; "gnutar").  Set to nil if you don't have GNU tar installed on your system.
;; GNU tar is available for anonymous ftp at prep.ai.mit.edu.

(defvar dired-guess-have-gnutar "gnutar"
  "*If non-nil, name of GNU tar (e.g. \"tar\" or \"gnutar\").
GNU tar's `z' switch is used for compressed tar files.
If you don't have GNU tar, set this to nil: a pipe is then used.")

(defvar dired-guess-tar (or dired-guess-have-gnutar "tar"))

(defvar dired-auto-shell-command-alist
  (list
   '(".Z"     . "uncompress")
   '(".Z.uu" . "uudecode * | uncompress")
   '(".uu"    . "uudecode")
   '(".hqx"   . "mcvert")
   '(".sh"    . "sh")
   '(".shar"  . "unshar")
   (cons ".tar" (concat dired-guess-tar " xvf"))
   (cons ".tar.Z" (if dired-guess-have-gnutar
		      (concat dired-guess-tar " xvfz")
		    (concat "zcat * | " dired-guess-tar " xvf -")))
   (cons ".tar.Z.uu" (if dired-guess-have-gnutar
			 (concat "uudecode * | " dired-guess-tar " xvfz -")
		       "uudecode * | zcat | tar xvf -")))

  "*Alist of file extensions and their suggested commands.
See also variable `dired-auto-shell-use-last-extension'.")

(defvar dired-auto-shell-use-last-extension nil
  "*If non-nil, uses only the last extension of the filename.
  For example, foo.tar.Z would use the guess for the .Z extension.
If nil, use all the extensions.  For example, foo.tar.Z would guess
  for the .tar.Z extension.")

(defun dired-read-shell-command (prompt arg files)
  "Read a dired shell command using generic minibuffer history.
This command tries to guess a command from the filename(s)
from the variable `dired-auto-shell-command-alist' (which see)."
  (dired-mark-pop-up
   nil 'shell files			; bufname type files
   'dired-guess-shell-command		; function &rest args
   (format prompt (dired-mark-prompt arg files)) files))


(defun dired-guess-shell-command (prompt files)
  ;;"Ask user with PROMPT for a shell command, guessing a default from FILES."
  (let ((defalt (if (cdr files)
		    nil                 ; If more than one file, don't guess
		  (cdr (assoc
			(substring (car files) ; Separate extension & lookup
				   (if dired-auto-shell-use-last-extension
				       (string-match "\.[^.]*$" (car files))
				     (string-match "\\." (car files))))
			dired-auto-shell-command-alist)))))
    (if (not (featurep 'gmhist))
	(read-string prompt defalt)
      (if defalt
	  (put 'dired-shell-command-history 'default defalt)))
    (read-with-history-in 'dired-shell-command-history prompt)))
