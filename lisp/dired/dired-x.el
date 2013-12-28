;; dired-x.el - Extra DIRED commands for Emacs.

(defconst dired-extra-version (substring "!Revision: 1.191 !" 11 -2)
  "Id: dired-x.el,v 1.191 1992/05/14 11:41:54 sk RelBeta ")
  
;; Copyright (C) 1991 Sebastian Kremer.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; LISPDIR ENTRY for the Elisp Archive ===============================
;;    LCD Archive Entry:
;;    dired-x|Sebastian Kremer|sk@thp.uni-koeln.de
;;    |Extra Features for Tree Dired
;;    |Date: 1992/05/14 11:41:54 |Revision: 1.191 |

;; INSTALLATION ======================================================

;; In your ~/.emacs, say
;;
;;       (setq dired-load-hook '(lambda () (load "dired-x")))
;;
;; At load time dired-x will install itself using the various other
;; dired hooks.  It will redefine some functions and bind dired keys.
;; If gmhist is present, dired-x will take advantage of it.

(require 'dired)			; we will redefine some functions
					; and also need some macros

(provide 'dired-extra)			; but this file is "dired-x"

;; Customization (see also defvars in other sections below)

(defvar dired-mark-keys '("Z")
  "*List of keys (strings) that insert themselves as file markers.")

(defvar dired-dangerous-shell-command "^rm" ; e.g. "rm" or "rmdir"
  "*Regexp for dangerous shell commands that should never be the default.")

;; Add key bindings.  This file is supposed to be loaded immediately
;; after dired, inside dired-load-hook.

(define-key dired-mode-map "V" 'dired-vm)
(define-key dired-mode-map "\(" 'dired-set-marker-char)
(define-key dired-mode-map "\)" 'dired-restore-marker-char)
(define-key dired-mode-map "I" 'dired-do-insert-subdir)
;;(define-key dired-mode-map "\M-f" 'dired-flag-extension)
(define-key dired-mode-map "\M-M" 'dired-do-unmark)
(define-key dired-mode-map "\M-o" 'dired-omit-toggle)
(define-key dired-mode-map "\M-(" 'dired-mark-sexp)
(define-key dired-mode-map "," 'dired-mark-rcs-files)
(define-key dired-mode-map "\M-!" 'dired-smart-shell-command)
(define-key dired-mode-map "\M-&" 'dired-smart-background-shell-command)
(define-key dired-mode-map "T" 'dired-do-toggle)
(define-key dired-mode-map "w" 'dired-copy-filename-as-kill)
(define-key dired-mode-map "\M-g" 'dired-goto-file)
(define-key dired-mode-map "\M-G" 'dired-goto-subdir)
(define-key dired-mode-map "&" 'dired-do-background-shell-command)
(define-key dired-mode-map "A" 'dired-do-byte-compile-and-load)
(define-key dired-mode-map "F" 'dired-do-find-file)
(define-key dired-mode-map "S" 'dired-do-relsymlink)
(define-key dired-mode-map "%S" 'dired-do-relsymlink-regexp)

(mapcar (function;; do this last to override bindings above
	 (lambda (x)
	   (define-key dired-mode-map x 'dired-mark-with-this-char)))
	dired-mark-keys)

;; Install ourselves into the appropriate hooks

(defun dired-add-hook (hook-var function)
  "Add a function to a hook.
First argument HOOK-VAR (a symbol) is the name of a hook, second
argument FUNCTION is the function to add.
Returns nil if FUNCTION was already present in HOOK-VAR, else new
value of HOOK-VAR."
  (interactive "SAdd to hook-var (symbol): \naAdd which function to %s? ")
  (if (not (boundp hook-var)) (set hook-var nil))
  (if (or (not (listp (symbol-value hook-var)))
	  (eq (car (symbol-value hook-var)) 'lambda))
      (set hook-var (list (symbol-value hook-var))))
  (if (memq function (symbol-value hook-var))
      nil
    (set hook-var (cons function (symbol-value hook-var)))))

(dired-add-hook 'dired-mode-hook 'dired-extra-startup)
(dired-add-hook 'dired-after-readin-hook 'dired-omit-expunge)

(defvar dired-default-marker dired-marker-char
  "*The value of `dired-marker-char' in effect before dired-x was
loaded and the value which is restored if the marker stack underflows.
This is usually the asterisk `*'.")

(defun dired-extra-startup ()
  "Automatically put on dired-mode-hook to get extra dired features:
\\<dired-mode-map>
  \\[dired-vm]\t-- VM on folder
  \\[dired-rmail]\t-- Rmail on folder
  \\[dired-do-insert-subdir]\t-- insert all marked subdirs
  \\[dired-do-find-file]\t-- visit all marked files simultaneously
  \\[dired-set-marker-char], \\[dired-restore-marker-char]\t-- change and display dired-marker-char dynamically.
  \\[dired-omit-toggle]\t-- toggle omitting of files
  \\[dired-mark-sexp]\t-- mark by lisp expression
  \\[dired-do-unmark]\t-- replace existing marker with another.
  \\[dired-mark-rcs-files]\t-- mark all RCS controlled files
  \\[dired-mark-files-compilation-buffer]\t-- mark compilation files
  \\[dired-copy-filename-as-kill]\t-- copy the file or subdir names into the kill ring.
  \t   You can feed it to other commands using \\[yank].

For more features, see variables

  dired-omit-files
  dired-omit-extenstions
  dired-dangerous-shell-command
  dired-mark-keys
  dired-local-variables-file
  dired-find-subdir
  dired-guess-have-gnutar
  dired-auto-shell-command-alist

See also functions

  dired-sort-on-size
  dired-do-relsymlink
  dired-flag-extension
  dired-virtual
  dired-jump-back
  dired-jump-back-other-window
"
  (interactive)
  ;; This must be done in each new dired buffer:
  (dired-hack-local-variables)
  (dired-omit-startup)
  (dired-marker-stack-startup))

;;; Handle customization

(or (fboundp 'read-with-history-in)
    ;; try to load gmhist
    (load "gmhist" t))

(if (not (fboundp 'read-with-history-in))

    nil					; Gmhist is not available

  ;; Else use generic minibuffer history
  (put 'dired-shell-command-history 'dangerous dired-dangerous-shell-command)

  ;; Redefinition - when this is loaded, dired.el has alreay been loaded.

  (defun dired-read-regexp (prompt &optional initial)
    (setq dired-flagging-regexp
	  (if (null initial)
	      (read-with-history-in 'regexp-history prompt initial)
	    (put 'regexp-history 'default
		 nil)
	    (put 'regexp-history 'default
		 (read-with-history-in 'regexp-history prompt initial)))))

  (defun dired-read-dir-and-switches (str)
    (nreverse
     (list
      (if current-prefix-arg
	  (read-string "Dired listing switches: " dired-listing-switches))
      (read-file-name-with-history-in
       'file-history			; or 'dired-history?
       (format "Dired %s(directory): " str) nil default-directory nil))))
)



;;; Dynamic Markers

(defun dired-mark-with-this-char (arg)
  "Mark the current file or subdir with the last key you pressed to invoke
this command.  Else like \\[dired-mark-subdir-or-file] command."
  (interactive "p")
  (let ((dired-marker-char;; use last character, in case of prefix cmd
	 last-command-char))
    (dired-mark-subdir-or-file arg)))

(defvar dired-marker-stack nil
  "List of previously used dired marker characters.")

(defvar dired-marker-string ""
  "String version of `dired-marker-stack'.")

(defun dired-current-marker-string ()
  "Computes and returns `dired-marker-string'."
  (setq dired-marker-string
	(concat " "
		(mapconcat (function char-to-string)
			   (reverse dired-marker-stack)
			   ""))))

(defun dired-marker-stack-startup ()
  (make-local-variable 'dired-marker-char)
  (make-local-variable 'dired-del-marker)
  (make-local-variable 'dired-marker-stack)
  (or (assq 'dired-marker-stack minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(dired-marker-stack dired-marker-string)
		  minor-mode-alist))))

(defun dired-set-marker-char (c)
  "Set the marker character to something else.
Use \\[dired-restore-marker-char] to restore the previous value."
  (interactive "cNew marker character: ")
  (setq dired-marker-stack (cons c dired-marker-stack))
  (dired-current-marker-string)
  (setq dired-marker-char c)
  (set-buffer-modified-p (buffer-modified-p)) ; update mode line
  (message "New marker is %c" dired-marker-char))

(defun dired-restore-marker-char ()
  "Restore the marker character to its previous value.
Uses `dired-default-marker' if the marker stack is empty."
  (interactive)
  (setq dired-marker-stack (cdr dired-marker-stack)
	dired-marker-char (car dired-marker-stack))
  (dired-current-marker-string)
  (set-buffer-modified-p (buffer-modified-p)) ; update mode line
  (or dired-marker-char (setq dired-marker-char dired-default-marker))
  (message "Marker is %c" dired-marker-char))

;;; Sort on Size kludge if your ls can't do it

(defun dired-sort-on-size ()
  "Sorts a dired listing on file size.
If your ls cannot sort on size, this is useful as `dired-after-readin-hook':
    \(setq dired-after-readin-hook 'dired-sort-on-size\)"
  (require 'sort)
  (goto-char (point-min))
  (dired-goto-next-file)		; skip `total' line
  (beginning-of-line)
  (sort-subr t				; biggest file first
	     'forward-line 'end-of-line 'dired-get-file-size))

(defun dired-get-file-size ()
  (re-search-forward "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)")
  (goto-char (match-beginning 1))
  (forward-char -1)
  (string-to-int (buffer-substring (save-excursion
				     (backward-word 1)
				     (point))
				   (point))))


;;; Misc. (mostly featurismic) commands

;; Mail folders

(defvar dired-vm-read-only-folders nil
  "*If t, \\[dired-vm] will visit all folders read-only.
If neither nil nor t, e.g. the symbol `if-file-read-only', only
files not writable by you are visited read-only.

Read-only folders only work in VM 5, not in VM 4.")

(defun dired-vm (&optional read-only)
  "Run VM on this file.
With prefix arg, visit folder read-only (this requires at least VM 5).
See also variable `dired-vm-read-only-folders'."
  (interactive "P")
  (let ((dir (dired-current-directory))
	(fil (dired-get-filename)))
    ;; take care to supply 2nd arg only if requested - may still run VM 4!
    (cond (read-only (vm-visit-folder fil t))
	  ((eq t dired-vm-read-only-folders) (vm-visit-folder fil t))
	  ((null dired-vm-read-only-folders) (vm-visit-folder fil))
	  (t (vm-visit-folder fil (not (file-writable-p fil)))))
    ;; so that pressing `v' inside VM does prompt within current directory:
    (set (make-local-variable 'vm-folder-directory) dir)))

(defun dired-rmail ()
  "Run RMAIL on this file."
  (interactive)
  (rmail (dired-get-filename)))

;; More subdir operations

(defun dired-do-insert-subdir ()
  "Insert all marked subdirectories in situ that are not yet inserted.
Non-directories are silently ignored."
  (interactive)
  (let ((files (or (dired-mark-get-files)
		   (error "No files marked."))))
    (while files
      (if (file-directory-p (car files))
	  (save-excursion (dired-maybe-insert-subdir (car files))))
      (setq files (cdr files)))))

(defun dired-mark-extension (extension &optional marker-char)
  "Mark all files with a certain extension for use in later commands.
A `.' is not automatically prepended to the string entered."
  ;; EXTENSION may also be a list of extensions instead of a single one.
  ;; Optional MARKER-CHAR is marker to use.
  (interactive "sMarking extension: \nP")
  (or (listp extension)
      (setq extension (list extension)))
  (dired-mark-files-regexp
   (concat ".";; don't match names with nothing but an extension
	   "\\("
	   (mapconcat 'regexp-quote extension "\\|")
	   "\\)$")
   marker-char))

(defun dired-flag-extension (extension)
  "In dired, flag all files with a certain extension for deletion.
A `.' is *not* automatically prepended to the string entered."
  (interactive "sFlagging extension: ")
  (dired-mark-extension extension dired-del-marker))

(defvar patch-unclean-extensions
  '(".rej" ".orig")
  "List of extensions of dispensable files created by the `patch' program.")

(defvar tex-unclean-extensions
  '(".toc" ".log" ".aux");; these are already in completion-ignored-extensions
  "List of extensions of dispensable files created by TeX.")

(defvar latex-unclean-extensions
  '(".idx" ".lof" ".lot" ".glo")
  "List of extensions of dispensable files created by LaTeX.")

(defvar bibtex-unclean-extensions
  '(".blg" ".bbl")
  "List of extensions of dispensable files created by BibTeX.")

(defvar texinfo-unclean-extensions
  '(".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs"
    ".tp" ".tps" ".vr" ".vrs")
  "List of extensions of dispensable files created by texinfo.")

(defun dired-clean-patch ()
  "Flag dispensable files created by patch for deletion.
See variable `patch-unclean-extensions'."
  (interactive)
  (dired-flag-extension patch-unclean-extensions))

(defun dired-clean-tex ()
  "Flag dispensable files created by tex etc. for deletion.
See variable `texinfo-unclean-extensions', `latex-unclean-extensions',
`bibtex-unclean-extensions' and `texinfo-unclean-extensions'."
  (interactive)
  (dired-flag-extension (append texinfo-unclean-extensions
				latex-unclean-extensions
				bibtex-unclean-extensions
				tex-unclean-extensions)))

(defun dired-do-unmark (unmarker)
  "Unmark marked files by replacing the marker with another character.
The new character defaults to a space, effectively unmarking them."
  (interactive "sChange marker to: ")
  (if (string= unmarker "")
      (setq unmarker " "))
  (setq unmarker (substring unmarker 0 1))
  (let ((regexp (dired-marker-regexp))
	(buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(replace-match unmarker)))))

;; This is unused but might come in handy sometime
;(defun dired-directories-of (files)
;  ;; Return unique list of parent directories of FILES.
;  (let (dirs dir file)
;    (while files
;      (setq file (car files)
;	    files (cdr files)
;	    dir (file-name-directory file))
;      (or (member dir dirs)
;	  (setq dirs (cons dir dirs))))
;    dirs))

;; Adapted from code by wurgler@zippysun.math.uakron.edu (Tom Wurgler).
;; Suggest you bind it to a key.  I use C-x C-j.
(defun dired-jump-back (&optional other-window)
  "Jump back to dired:
If in a file, dired the current directory and move to file's line.
If in dired already, pop up a level and goto old directory's line.
In case the proper dired file line cannot be found, refresh the dired
  buffer and try again."
  (interactive)
  (let* ((file buffer-file-name)
	 (dir (if file (file-name-directory file) default-directory)))
    (if (eq major-mode 'dired-mode)
	(progn
	  (setq dir (dired-current-directory))
	  (if other-window
	      (dired-up-directory-other-window)
	    (dired-up-directory))
	  (dired-really-goto-file dir))
      (if other-window
	  (dired-other-window dir)
	(dired dir))
      (if file (dired-really-goto-file file)))))

(defun dired-jump-back-other-window ()
  "Like \\[dired-jump-back], but to other window."
  (interactive)
  (dired-jump-back t))

(defun dired-really-goto-file (file)
  (or (dired-goto-file file)
      (progn				; refresh and try again
	(dired-insert-subdir (file-name-directory file))
	(dired-goto-file file))))

(defun dired-up-directory-other-window ()
  "Like `dired-up-directory', but in other window."
  (interactive)
  (let* ((dir (dired-current-directory))
	 (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
	(dired-goto-subdir up)
	;; Only in this case it really uses another window:
	(progn
	  (dired-other-window up)
	  (dired-goto-file dir)))))

(defun dired-mark-rcs-files (&optional unflag-p)
  "Mark all files that are under RCS control.
With prefix argument, unflag all those files.
Mentions RCS files for which a working file was not found in this buffer.
Type \\[dired-why] to see them again."
  ;; Returns failures, or nil on success.
  ;; Finding those with locks would require to peek into the ,v file,
  ;; depends slightly on the RCS version used and should be done
  ;; together with the Emacs RCS interface.
  ;; Unfortunately, there is no definitive RCS interface yet.
  (interactive "P")
  (message "%sarking RCS controlled files..." (if unflag-p "Unm" "M"))
  (let ((dired-marker-char (if unflag-p ?\  dired-marker-char))
	rcs-files wf failures count total)
    (mapcar				; loop over subdirs
     (function
      (lambda (dir)
	(or (equal (file-name-nondirectory (directory-file-name dir))
		   "RCS")
	    ;; skip inserted RCS subdirs
	    (setq rcs-files
		  (append (directory-files dir t ",v$") ; *,v and RCS/*,v
			  (let ((rcs-dir (expand-file-name "RCS" dir)))
			    (if (file-directory-p rcs-dir)
				(mapcar	; working files from ./RCS are in ./
				 (function
				  (lambda (x)
				    (expand-file-name x dir)))
				 (directory-files
				  (file-name-as-directory rcs-dir) nil ",v$"))))
			  rcs-files)))))
     (mapcar (function car) dired-subdir-alist))
    (setq total (length rcs-files))
    (while rcs-files
      (setq wf (substring (car rcs-files) 0 -2)
	    rcs-files (cdr rcs-files))
      (save-excursion (if (dired-goto-file wf)
			  (dired-mark-file 1)
			(setq failures (cons wf failures)))))
    (if (null failures)
	(message "%d RCS file%s %smarked."
		 total (dired-plural-s total) (if unflag-p "un" ""))
      (setq count (length failures))
      (dired-log-summary "RCS working file not found %s" failures)
      (message "%d RCS file%s: %d %smarked - %d not found %s."
	       total (dired-plural-s total) (- total count)
	       (if unflag-p "un" "") count failures))
    failures))

(defun dired-do-toggle ()
  "Toggle marks.
That is, currently marked files become unmarked and vice versa.
Files marked with other flags (such as `D') are not affected.
`.' and `..' are never toggled.
As always, hidden subdirs are not affected."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only)
      (while (not (eobp))
	(or (dired-between-files)
	    (looking-at dired-re-dot)
	    ;; use subst instead of insdel because it does not move
	    ;; the gap and thus should be faster and because
	    ;; other characters are left alone automatically
	    (apply 'subst-char-in-region
		   (point) (1+ (point))
		   (if (eq ?\040 (following-char)) ; SPC
		       (list ?\040 dired-marker-char)
		     (list dired-marker-char ?\040))))
	(forward-line 1)))))

;; This function is missing in simple.el
(defun copy-string-as-kill (string)
  "Save STRING as if killed in a buffer."
  (setq kill-ring (cons string kill-ring))
  (if (> (length kill-ring) kill-ring-max)
	(setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))
  (setq kill-ring-yank-pointer kill-ring))

(defvar dired-marked-files nil
  "List of filenames from last `dired-copy-filename-as-kill' call.")

(defun dired-copy-filename-as-kill (&optional arg)
  "Copy names of marked (or next ARG) files into the kill ring.
The names are separated by a space.
With a zero prefix arg, use the complete pathname of each marked file.
With a raw (just \\[universal-argument]) prefix arg, use the relative pathname of each marked file.

If on a subdir headerline and no prefix arg given, use subdirname instead.

You can then feed the file name to other commands with \\[yank].

The list of names is also stored onto the variable
`dired-marked-files' for use, e.g., in an `\\[eval-expression]' command."
  (interactive "P")
  (copy-string-as-kill
   (or (and (not arg)
	    (dired-get-subdir))
       (mapconcat (function identity)
		  (setq dired-marked-files
			(if arg
			    (cond ((zerop (prefix-numeric-value arg))
				   (dired-mark-get-files))
				  ((integerp arg)
				   (dired-mark-get-files 'no-dir arg))
				  (t	; else a raw arg
				   (dired-mark-get-files t)))
			  (dired-mark-get-files 'no-dir)))
		  " ")))
  (message "%s" (car kill-ring)))

(defun dired-do-background-shell-command (&optional arg)
  "Like \\[dired-do-shell-command], but starts command in background.
Note that you can type input to the command in its buffer.
This requires background.el from the comint package to work."
  ;; With the version in emacs-19.el, you can alternatively just
  ;; append an `&' to any shell command to make it run in the
  ;; background, but you can't type input to it.
  (interactive "P")
  (dired-do-shell-command arg t))

;; redefines dired.el to put back in the dired-offer-kill-buffer
;; feature which rms didn't like.
(defun dired-clean-up-after-deletion (fn)
  ;; Clean up after a deleted file or directory FN.
  ;; Remove expanded subdir of deleted dir, if any
  (save-excursion (and (dired-goto-subdir fn)
		       (dired-kill-subdir)))
  ;; Offer to kill buffer of deleted file FN.
  (let ((buf (get-file-buffer fn)))
    (and buf
	 (funcall (function y-or-n-p)
		  (format "Kill buffer of %s, too? "
			  (file-name-nondirectory fn)))
	 (save-excursion;; you never know where kill-buffer leaves you
	   (kill-buffer buf))))
  (let ((buf-list (dired-buffers-for-top-dir fn))
	(buf nil))
    (and buf-list
	 (y-or-n-p (format "Kill dired buffer%s of %s, too? "
			   (dired-plural-s (length buf-list))
			   (file-name-nondirectory fn)))
	 (while buf-list
	   (save-excursion (kill-buffer (car buf-list)))
	   (setq buf-list (cdr buf-list)))))
  ;; Anything else?
  )

;;; Omitting

;;; Enhanced omitting of lines from directory listings.
;;; Marked files are never omitted.
;;; Adapted from code submitted by:
;;; Michael D. Ernst, mernst@theory.lcs.mit.edu, 1/11/91

(defvar dired-omit-files-p nil
  "*If non-nil, \"uninteresting\" files are not listed (buffer-local).
Use \\[dired-omit-toggle] to toggle its value.
Uninteresting files are those whose filenames match regexp `dired-omit-files',
plus those ending with extensions in `dired-omit-extensions'.")

(defvar dired-omit-files "^#\\|\\.$"
  "*Filenames matching this regexp will not be displayed (buffer-local).
This only has effect when `dired-omit-files-p' is t.
See also `dired-omit-extensions'.")

(defvar dired-omit-extensions
  (append completion-ignored-extensions
	  latex-unclean-extensions
	  bibtex-unclean-extensions
	  texinfo-unclean-extensions)
  "*If non-nil, a list of extensions (strings) to omit from Dired
listings.  Defaults to the elements of
`completion-ignored-extensions', `latex-unclean-extensions',
`bibtex-unclean-extensions' and `texinfo-unclean-extensions'.")

;; should probably get rid of this and always use 'no-dir.
;; sk 28-Aug-1991 09:37
(defvar dired-omit-localp 'no-dir
  "The LOCALP argument dired-omit-expunge passes to dired-get-filename.
If it is 'no-dir, omitting is much faster, but you can only match
against the basename of the file.  Set it to nil if you need to match the
whole pathname.")

;; \017=^O for Omit - other packages can chose other control characters.
(defvar dired-omit-marker-char ?\017
  "Temporary marker used by by dired-omit.
Should never be used as marker by the user or other packages.")

(defun dired-omit-startup ()
  (make-local-variable 'dired-omit-files-p)
  (or (assq 'dired-omit-files-p minor-mode-alist)
      ;; Append at end so that it doesn't get between "Dired" and "by name".
      (setq minor-mode-alist
	    (append minor-mode-alist '((dired-omit-files-p " Omit"))))))

(defun dired-omit-toggle (&optional flag)
  "Toggle between displaying and omitting files matching `dired-omit-files'.
With an arg, and if omitting was off, don't toggle and just mark the
  files but don't actually omit them.
With an arg, and if omitting was on, turn it off but don't refresh the buffer."
  (interactive "P")
  (if flag
      (if dired-omit-files-p
	  (setq dired-omit-files-p (not dired-omit-files-p))
	(dired-mark-unmarked-files (dired-omit-regexp) nil nil
				   dired-omit-localp))
    ;; no FLAG
    (setq dired-omit-files-p (not dired-omit-files-p))
    (if (not dired-omit-files-p)
	(revert-buffer)
      ;; this will mention how many were omitted:
      (dired-omit-expunge))))

;; This is sometimes let-bound to t if messages would be annoying,
;; e.g., in dired-awrh.el.
(defvar dired-omit-silent nil)

(defun dired-omit-expunge (&optional regexp)
  "Erases all unmarked files matching REGEXP.
Does nothing if global variable `dired-omit-files-p' is nil.
If REGEXP is nil or not specified, uses `dired-omit-files', and also omits
  filenames ending in `dired-omit-extensions'.
If REGEXP is the empty string, this function is a no-op.

This functions works by temporarily binding `dired-marker-char' to
`dired-omit-marker-char' and calling `dired-do-kill'."
  (interactive "sOmit files (regexp): ")
  (if dired-omit-files-p
     (let ((omit-re (or regexp (dired-omit-regexp)))
	   count)
       (or (string= omit-re "")
	   (let ((dired-marker-char dired-omit-marker-char))
	     (or dired-omit-silent (message "Omitting..."))
	     (if (dired-mark-unmarked-files
		  omit-re nil nil dired-omit-localp)
		 (setq count (dired-do-kill nil (if dired-omit-silent
						    ""
						  "Omitted %d line%s.")))
	       (or dired-omit-silent
		   (message "(Nothing to omit)")))))
       count)))

(defun dired-omit-regexp ()
  (concat (if dired-omit-files (concat "\\(" dired-omit-files "\\)") "")
	  (if (and dired-omit-files dired-omit-extensions) "\\|" "")
	  (if dired-omit-extensions
	      (concat ".";; a non-extension part should exist
		      "\\("
		      (mapconcat 'regexp-quote dired-omit-extensions "\\|")
		      "\\)$")
	    "")))

;; Returns t if any work was done, nil otherwise.
(defun dired-mark-unmarked-files (regexp msg &optional unflag-p localp)
  "Marks unmarked files matching REGEXP, displaying MSG.
REGEXP is matched against the complete pathname.
Does not re-mark files which already have a mark.
With prefix argument, unflag all those files.
Second optional argument LOCALP is as in `dired-get-filename'."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\  dired-marker-char)))
    (dired-mark-if
     (and
      ;; not already marked
      (looking-at " ")
      ;; uninteresting
      (let ((fn (dired-get-filename localp t)))
	(and fn (string-match regexp fn))))
     msg)))

(defun dired-omit-new-add-entry (filename &optional marker-char)
  ;; This redefines dired.el's dired-add-entry to avoid calling ls for
  ;; files that are going to be omitted anyway.
  (if dired-omit-files-p
      ;; perhaps return t without calling ls
      (let ((omit-re (dired-omit-regexp)))
	(if (or (string= omit-re "")
		(not
		 (string-match omit-re
			       (cond
				((eq 'no-dir dired-omit-localp)
				 filename)
				((eq t dired-omit-localp)
				 (dired-make-relative filename))
				(t
				 (dired-make-absolute filename directory))))))
	    ;; if it didn't match, go ahead and add the entry
	    (dired-omit-old-add-entry filename marker-char)
	  ;; dired-add-entry returns t for success, perhaps we should
	  ;; return file-exists-p
	  t))
    ;; omitting is not turned on at all
    (dired-omit-old-add-entry filename marker-char)))

;; Save old defun if not already done:
(or (fboundp 'dired-omit-old-add-entry)
    (fset 'dired-omit-old-add-entry (symbol-function 'dired-add-entry)))
;; Redefine dired.el
(fset 'dired-add-entry 'dired-omit-new-add-entry)


;;
(defun dired-mark-sexp (predicate &optional unflag-p)
  "Mark files for which PREDICATE returns non-nil.
With a prefix arg, unflag those files instead.

PREDICATE is a lisp expression that can refer to the following symbols:

    inode  [integer] the inode of the file (only for ls -i output)
    s      [integer] the size of the file for ls -s output
	             (ususally in blocks or, with -k, in KByte)
    mode   [string]  file permission bits, e.g. \"-rw-r--r--\"
    nlink  [integer] number of links to file
    uid    [string]  owner
    gid    [string]  group  (If the gid is not displayed by ls,
	             this will still be set (to the same as uid))
    size   [integer] file size in bytes
    time   [string]  the time that ls displays, e.g. \"Feb 12 14:17\"
    name   [string]  the name of the file
    sym    [string]  if file is a symbolic link, the linked-to name, else \"\"

For example, use

        (equal 0 size)

to mark all zero length files."
  ;; Using sym="" instead of nil avoids the trap of
  ;; (string-match "foo" sym) into which a user would soon fall.
  ;; Give `equal' instead of `=' in the example, as this works on
  ;; integers and strings.
  (interactive "xMark if (lisp expr): \nP")
  (message "%s" predicate)
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char))
	inode s mode nlink uid gid size time name sym)
    (dired-mark-if
     (save-excursion (and (dired-parse-ls)
			  (eval predicate)))
     (format "'%s file" predicate))
    ;; With Jamie's compiler we could do the following instead:
;    (eval (byte-compile-sexp
;	   (macroexpand
;	    (` (dired-mark-if
;		(save-excursion (and (dired-parse-ls)
;				     (, predicate)))
;		(format "'%s file" (quote (, predicate))))))))
    ;; This isn't measurably faster, though, at least for simple predicates.
    ;; Caching compiled predicates might be interesting if you use
    ;; this command a lot or with complicated predicates.
    ;; Alternatively compiling PREDICATE by hand should not be too
    ;; hard - e.g., if it uses just one variable, not all of the ls
    ;; line needs to be parsed.
    ))

(if (fboundp 'gmhist-make-magic)
    (gmhist-make-magic 'dired-mark-sexp 'eval-expression-history))

(defun dired-parse-ls ()
  ;; Sets vars
  ;;                inode s mode nlink uid gid size time name sym
  ;; (probably let-bound in caller) according to current file line.
  ;; Returns t for succes, nil if this is no file line.
  ;; Upon success, all variables are set, either to nil or the
  ;; appropriate value, so they need not be initialized.
  ;; Moves point within the current line.
  (if (dired-move-to-filename)
      (let (pos
	    (mode-len 10)		; length of mode string
	    ;; like in dired.el, but with subexpressions \1=inode, \2=s:
	    (dired-re-inode-size "\\s *\\([0-9]*\\)\\s *\\([0-9]*\\) ?"))
	(beginning-of-line)
	(forward-char 2)
	(if (looking-at dired-re-inode-size)
	    (progn
	      (goto-char (match-end 0))
	      (setq inode (string-to-int (buffer-substring (match-beginning 1)
							   (match-end 1)))
		    s (string-to-int (buffer-substring (match-beginning 2)
						       (match-end 2)))))
	  (setq inode nil
		s nil))
	(setq mode (buffer-substring (point) (+ mode-len (point))))
	(forward-char mode-len)
	(setq nlink (read (current-buffer)))
	(setq uid (buffer-substring (point) (progn (forward-word 1) (point))))
	(re-search-forward "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)")
	(goto-char (match-beginning 1))
	(forward-char -1)
	(setq size (string-to-int (buffer-substring (save-excursion
						      (backward-word 1)
						      (setq pos (point)))
						    (point))))
	(goto-char pos)
	(backward-word 1)
	;; if no gid is displayed, gid will be set to uid
	;; but user will then not reference it anyway in PREDICATE.
	(setq gid (buffer-substring (save-excursion (forward-word 1) (point))
				    (point))
	      time (buffer-substring (match-beginning 1)
				     (1- (dired-move-to-filename)))
	      name (buffer-substring (point)
				     (or (dired-move-to-end-of-filename t)
					 (point)))
	      sym  (progn
		     (if (looking-at " -> ")
			 (buffer-substring (progn (forward-char 4) (point))
					   (progn (end-of-line) (point)))
		       "")))
	t)
    nil))


;; tester
;;(defun dired-parse-ls-show ()
;;  (interactive)
;;   (let (inode s mode size uid gid nlink time name sym)
;;     (if (dired-parse-ls)
;;	 (message "%s" (list inode s mode nlink uid gid size time name sym))
;;       (message "Not on a file line."))))


;; Mark files whose names appear in another buffer.

(defun dired-mark-these-files (file-list from)
  ;; Mark the files in FILE-LIST.  Relative filenames are taken to be
  ;; in the current dired directory.
  ;; FROM is a string (used for logging) describing where FILE-LIST
  ;; came from.
  ;; Logs files that were not found and displays a success or failure
  ;; message.
  (message "Marking files %s..." from)
  (let ((total (length file-list))
	(cur-dir (dired-current-directory))
	file failures)
    (while file-list
      (setq file (dired-make-absolute (car file-list) cur-dir)
	    file-list (cdr file-list))
      ;;(message "Marking file `%s'" file)
      (save-excursion
	(if (dired-goto-file file)
	    (dired-mark-file 1)
	  (setq failures (cons (dired-make-relative file) failures))
	  (dired-log "Cannot mark this file (not found): %s\n" file))))
    (if failures
	(dired-log-summary (message "Failed to mark %d of %d files %s %s"
				    (length failures) total from failures))
      (message "Marked %d file%s %s." total (dired-plural-s total) from))))

(defun dired-mark-files-from-other-dired-buffer (buf)
  "Mark files that are marked in the other Dired buffer.
I.e, mark those files in this Dired buffer that have the same
non-directory part as the marked files in the Dired buffer in the other window."
  (interactive (list (window-buffer (next-window))))
  (if (eq (get-buffer buf) (current-buffer))
      (error "Other dired buffer is the same"))
  (or (stringp buf) (setq buf (buffer-name buf)))
  (let ((other-files (save-excursion
		       (set-buffer buf)
		       (or (eq major-mode 'dired-mode)
			   (error "%s is not a dired buffer" buf))
		       (dired-mark-get-files 'no-dir))))
    (dired-mark-these-files other-files (concat "from buffer " buf))))

(defun dired-mark-files-compilation-buffer (&optional regexp buf)
  "Mark the files mentioned in the `*compilation*' buffer.
With an arg, you may specify the other buffer and your own regexp
instead of `compilation-error-regexp'.
Use `^.+$' (the default with a prefix arg) to match complete lines or
an empty string for `compilation-error-regexp'.
In conjunction with narrowing the other buffer you can mark an
arbitrary list of files, one per line, with this command."
  (interactive
   (if current-prefix-arg
       (list
	(read-string "Use compilation regexp: " "^.+$")
	(read-buffer "Use buffer: "
		     (let ((next-buffer (window-buffer (next-window))))
		       (if (eq next-buffer (current-buffer))
			   (other-buffer)
			 next-buffer))))))
  (let (other-files user-regexp-p)
    (if (zerop (length regexp))		; nil or ""
	(setq regexp compilation-error-regexp)
      (setq user-regexp-p t))
    (or buf (setq buf "*compilation*"))
    (or (stringp buf) (setq buf (buffer-name buf)))
    (save-excursion
      (set-buffer (or (get-buffer buf)
		      (error "No %s buffer!" buf)))
      (goto-char (point-min))
      (let (file new-file)
	(while (re-search-forward regexp nil t)
	  (setq new-file
		(buffer-substring
		 ;; If user specified a regexp with subexpr 1, and it
		 ;; matched, take that one for the file name, else
		 ;; take whole match.
		 ;; Else take the match from the compile regexp
		 (if user-regexp-p
		     (or (match-beginning 1)
			 (match-beginning 0))
		   (match-beginning 1))
		 (if user-regexp-p
		     (or (match-end 1)
			 (match-end 0))
		   (match-beginning 2))))
	  (or (equal file new-file)
	      ;; Avoid marking files twice as this is slow.  Multiple
	      ;; lines for the same file are common when compiling.
	      (setq other-files (cons new-file other-files)
		    file new-file)))))
    (dired-mark-these-files other-files (concat "from buffer " buf))))


;; make-symbolic-link always expand-file-name's its args, so relative
;; symlinks (e.g. "foo" -> "../bar/foo") are impossible to create.
;; Following code uses ln -s for a workaround.

(defvar dired-keep-marker-relsymlink ?S
  "See variable `dired-keep-marker-move'.")

(defun dired-make-symbolic-link (name1 name2 &optional ok-if-already-exists)
  ;; Args NAME1 NAME2 &optional OK-IF-ALREADY-EXISTS.
  ;; Create file NAME2, a symbolic link pointing to NAME1 (which may
  ;; be any string whatsoever and is passed untouched to ln -s).
  ;; OK-IF-ALREADY-EXISTS means that NAME2 will be overwritten if it
  ;; already exists.  If it is an integer, user will be asked about this.
  ;; On error, signals a file-error.
  (interactive "FSymlink to (string): \nFMake symbolic link to `%s': \np")
  (setq name2 (expand-file-name name2))
  (let* ((file-symlink-p (file-symlink-p name2))
	 (file-exists-p (file-exists-p name2)) ; dereferences symlinks
	 (file-or-symlink-exists (or file-symlink-p file-exists-p)))
    (if (and file-symlink-p (not file-exists-p))
	;; We do something dirty here as dired.el never checks
	;; file-symlink-p in addition to file-exists-p.
	;; This way me make sure we never silently overwrite even
	;; symlinks to non-existing files (what an achievement! ;-)
	(setq ok-if-already-exists 1))
    (if (or (null ok-if-already-exists)
	    (integerp ok-if-already-exists))
	(if (and file-or-symlink-exists
		 (not (and (integerp ok-if-already-exists)
			   (yes-or-no-p
			    (format
			     "File %s already exists; symlink anyway? "
			     name2)))))
	    (signal 'file-error (cons "File already exists" name2))))
    ;; Bombs if NAME1 starts with "-", but not all ln programs may
    ;; understand "--"  to mean end of options...sigh
    (let (err)
      (if file-or-symlink-exists (delete-file name2))
      (setq err (dired-check-process "SymLink" "ln" "-s" name1 name2))
      (if err
	  (signal 'file-error (cons "ln" err))))))

(defun dired-make-relative-symlink (file1 file2 &optional ok-if-already-exists)
  "Three arguments: FILE1 FILE2 &optional OK-IF-ALREADY-EXISTS
Make a symbolic link (pointing to FILE1) in FILE2.
The link is relative (if possible), for example

    \"/vol/tex/bin/foo\" \"/vol/local/bin/foo\"

results in

    \"../../tex/bin/foo\" \"/vol/local/bin/foo\"
"
  (interactive "FRelSymLink: \nFRelSymLink %s: \np")
  (let (name1 name2 len1 len2 (index 0) sub)
    (setq file1 (expand-file-name file1)
	  file2 (expand-file-name file2)
	  len1 (length file1)
	  len2 (length file2))
    ;; Find common initial pathname components:
    (let (next)
      (while (and (setq next (string-match "/" file1 index))
		  (setq next (1+ next))
		  (< next (min len1 len2))
		  ;; For the comparison, both substrings must end in
		  ;; `/', so NEXT is *one plus* the result of the
		  ;; string-match.
		  ;; E.g., consider the case of linking "/tmp/a/abc"
		  ;; to "/tmp/abc" erronously giving "/tmp/a" instead
		  ;; of "/tmp/" as common initial component
		  (string-equal (substring file1 0 next)
				(substring file2 0 next)))
	(setq index next))
      (setq name2 file2
	    sub (substring file1 0 index)
	    name1 (substring file1 index)))
    (if (string-equal sub "/")
	;; No common initial pathname found
	(setq name1 file1)
      ;; Else they have a common parent directory
      (let ((tem (substring file2 index))
	    (start 0)
	    (count 0))
	;; Count number of slashes we must compensate for ...
	(while (setq start (string-match "/" tem start))
	  (setq count (1+ count)
		start (1+ start)))
	;; ... and prepend a "../" for each slash found:
	(while (> count 0)
	  (setq count (1- count)
		name1 (concat "../" name1)))))
    (dired-make-symbolic-link
     (directory-file-name name1)	; must not link to foo/
					; (trailing slash!)
     name2 ok-if-already-exists)))

(defun dired-do-relsymlink (&optional arg)
   "Symlink all marked (or next ARG) files into a directory,
or make a symbolic link to the current file.
This creates relative symbolic links like

    foo -> ../bar/foo

not absolute ones like

    foo -> /ugly/path/that/may/change/any/day/bar/foo"
  (interactive "P")
  (dired-do-create-files 'relsymlink (function dired-make-relative-symlink)
			   "RelSymLink" arg dired-keep-marker-relsymlink))

(defun dired-do-relsymlink-regexp (regexp newname &optional whole-path)
  "RelSymlink all marked files containing REGEXP to NEWNAME.
See functions `dired-rename-regexp' and `dired-do-relsymlink'
  for more info."
  (interactive (dired-mark-read-regexp "RelSymLink"))
  (dired-do-create-files-regexp
   (function dired-make-relative-symlink)
   "RelSymLink" nil regexp newname whole-path dired-keep-marker-relsymlink))

;; Virtual dired mode to browse ls -lR listings
;; sk@sun5  7-Mar-1991 16:00

(fset 'virtual-dired 'dired-virtual)
(defun dired-virtual (dirname &optional switches)
  "Put this buffer into Virtual Dired mode.

In Virtual Dired mode, all commands that do not actually consult the
filesystem will work.

This is useful if you want to peruse and move around in an ls -lR
output file, for example one you got from an ftp server.  With
ange-ftp, you can even dired a directory containing an ls-lR file,
visit that file and turn on virtual dired mode.  But don't try to save
this file, as dired-virtual indents the listing and thus changes the
buffer.

If you have save a Dired buffer in a file you can use \\[dired-virtual] to
resume it in a later session.

Type \\<dired-mode-map>\\[revert-buffer] in the
Virtual Dired buffer and answer `y' to convert the virtual to a real
dired buffer again.  You don't have to do this, though: you can relist
single subdirs using \\[dired-do-redisplay].
"

  ;; DIRNAME is the top level directory of the buffer.  It will become
  ;; its `default-directory'.  If nil, the old value of
  ;; default-directory is used.

  ;; Optional SWITCHES are the ls switches to use.

  ;; Shell wildcards will be used if there already is a `wildcard'
  ;; line in the buffer (thus it is a saved Dired buffer), but there
  ;; is no other way to get wildcards.  Insert a `wildcard' line by
  ;; hand if you want them.

  (interactive
   (list (read-string "Virtual Dired directory: " (dired-virtual-guess-dir))))
  (goto-char (point-min))
  (or (looking-at "  ")
      ;; if not already indented, do it now:
      (indent-region (point-min) (point-max) 2))
  (or dirname (setq dirname default-directory))
  (setq dirname (expand-file-name (file-name-as-directory dirname)))
  (setq default-directory dirname)	; contains no wildcards
  (let ((wildcard (save-excursion
		    (goto-char (point-min))
		    (forward-line 1)
		    (and (looking-at "^  wildcard ")
			 (buffer-substring (match-end 0)
					   (progn (end-of-line) (point)))))))
  (if wildcard
	(setq dirname (expand-file-name wildcard default-directory))))
  ;; If raw ls listing (not a saved old dired buffer), give it a
  ;; decent subdir headerline:
  (goto-char (point-min))
  (or (looking-at dired-subdir-regexp)
      (dired-insert-headerline default-directory))
  (dired-mode dirname (or switches dired-listing-switches))
  (setq mode-name "Virtual Dired"
	revert-buffer-function 'dired-virtual-revert)
  (set (make-local-variable 'dired-subdir-alist) nil)
  (dired-build-subdir-alist)
  (goto-char (point-min))
  (dired-initial-position dirname))

(defun dired-virtual-guess-dir ()

  ;; Guess and return appropriate working directory of this buffer,
  ;; assumed to be in Dired or ls -lR format.
  ;; The guess is based upon buffer contents.
  ;; If nothing could be guessed, returns nil.

  (let ((regexp "^\\(  \\)?\\([^ \n\r]*\\)\\(:\\)[\n\r]")
	(subexpr 2))
    (goto-char (point-min))
    (cond ((looking-at regexp)
	   ;; If a saved dired buffer, look to which dir and
	   ;; perhaps wildcard it belongs:
	   (let ((dir (buffer-substring (match-beginning subexpr)
					(match-end subexpr))))
	     (file-name-as-directory dir)))
	  ;; Else no match for headerline found.  It's a raw ls listing.
	  ;; In raw ls listings the directory does not have a headerline
	  ;; try parent of first subdir, if any
	  ((re-search-forward regexp nil t)
	   (file-name-directory
	    (directory-file-name
	     (file-name-as-directory
	      (buffer-substring (match-beginning subexpr)
				(match-end subexpr))))))
	  (t				; if all else fails
	   nil))))


(defun dired-virtual-revert (&optional arg noconfirm)
  (if (not
       (y-or-n-p "Cannot revert a Virtual Dired buffer - switch to Real Dired mode? "))
      (error "Cannot revert a Virtual Dired buffer.")
    (setq mode-name "Dired"
	  revert-buffer-function 'dired-revert)
    (revert-buffer)))

;; A zero-arg version of dired-virtual.
;; You need my modified version of set-auto-mode for the
;; `buffer-contents-mode-alist'.
;; Or you use infer-mode.el and infer-mode-alist, same syntax.
(defun dired-virtual-mode ()
  "Put current buffer into virtual dired mode (see `dired-virtual').
Useful on `buffer-contents-mode-alist' (which see) with the regexp

    \"^  \\(/[^ /]+\\)/?+:$\"

to put saved dired buffers automatically into virtual dired mode.

Also useful for `auto-mode-alist' (which see) like this:

  \(setq auto-mode-alist (cons '(\"[^/]\\.dired$\" . dired-virtual-mode)
			      auto-mode-alist)\)
"
  (interactive)
  (dired-virtual (dired-virtual-guess-dir)))


(defvar dired-find-subdir nil		; t is pretty near to DWIM...
  "*If non-nil, Dired does not make a new buffer for a directory if it
can be found (perhaps as subdir) in some existing Dired buffer.

If there are several Dired buffers for a directory, the most recently
used is chosen.

Dired avoids switching to the current buffer, so that if you have
a normal and a wildcard buffer for the same directory, C-x d RET will
toggle between those two.")

(or (fboundp 'dired-old-find-buffer-nocreate)
    (fset 'dired-old-find-buffer-nocreate
	  (symbol-function 'dired-find-buffer-nocreate)))

(defun dired-find-buffer-nocreate (dirname) ; redefine dired.el
  (if dired-find-subdir
      (let* ((cur-buf (current-buffer))
	     (buffers (nreverse (dired-buffers-for-dir-exact dirname)))
	     (cur-buf-matches (and (memq cur-buf buffers)
				   ;; wildcards must match, too:
				   (equal dired-directory dirname))))
	;; We don't want to switch to the same buffer---
	(setq buffers (delq cur-buf buffers));;need setq with delq
	(or (car (sort buffers (function dired-x-buffer-more-recently-used-p)))
	    ;; ---unless it's the only possibility:
	    (and cur-buf-matches cur-buf)))
    (dired-old-find-buffer-nocreate dirname)))

;; this should be a builtin
(defun dired-x-buffer-more-recently-used-p (buffer1 buffer2)
  "Return t if BUFFER1 is more recently used than BUFFER2."
  (if (equal buffer1 buffer2)
      nil
    (let ((more-recent nil)
	  (list (buffer-list)))
      (while (and list
		  (not (setq more-recent (equal buffer1 (car list))))
		  (not (equal buffer2 (car list))))
	(setq list (cdr list)))
      more-recent)))

(defun dired-buffers-for-dir-exact (dir)
;; Return a list of buffers that dired DIR (a directory or wildcard)
;; at top level, or as subdirectory.
;; Top level matches must match the wildcard part too, if any.
;; The list is in reverse order of buffer creation, most recent last.
;; As a side effect, killed dired buffers for DIR are removed from
;; dired-buffers.
  (let ((alist dired-buffers) result elt)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist))
      (let ((buf (cdr elt)))
	(if (buffer-name buf)
	    ;; Top level must match exactly against dired-directory in
	    ;; case one of them is a wildcard.
	    (if (or (equal dir (save-excursion (set-buffer buf)
					       dired-directory))
		    (assoc dir (save-excursion (set-buffer buf)
					       dired-subdir-alist)))
		(setq result (cons buf result)))
	  ;; else buffer is killed - clean up:
	  (setq dired-buffers (delq elt dired-buffers)))))
    result))

(defun dired-buffers-for-top-dir (dir)
;; Return a list of buffers that dired DIR (a directory, not a wildcard)
;; at top level, with or without wildcards.
;; As a side effect, killed dired buffers for DIR are removed from
;; dired-buffers.
  (setq dir (file-name-as-directory dir))
  (let ((alist dired-buffers) result elt)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist))
      (let ((buf (cdr elt)))
	(if (buffer-name buf)
	    (if (equal dir (save-excursion (set-buffer buf) default-directory))
		(setq result (cons buf result)))
	  ;; else buffer is killed - clean up:
	  (setq dired-buffers (delq elt dired-buffers)))))
    result))

(defun dired-initial-position (dirname)	; redefine dired.el
  (end-of-line)
  (if dired-find-subdir (dired-goto-subdir dirname)) ; new
  (if dired-trivial-filenames (dired-goto-next-nontrivial-file)))

;;; Let `C-x f' and `C-x 4 f' know about Tree Dired's multiple directories.
;;; As a bonus, you get filename-at-point as default with a prefix arg.

;; It's easier to add to this alist than redefine function
;; default-directory while keeping the old information.
(defconst default-directory-alist
  '((dired-mode . (if (fboundp 'dired-current-directory)
		      (dired-current-directory)
		    default-directory)))
  "Alist of major modes and their opinion on default-directory, as a
lisp expression to evaluate.  A resulting value of nil is ignored in
favor of default-directory.")

(defun default-directory ()
  "Usage like variable `default-directory', but knows about the special
cases in variable `default-directory-alist' (which see)."
  (or (eval (cdr (assq major-mode default-directory-alist)))
      default-directory))

(defun find-file-read-filename-at-point (prompt)
  (if (fboundp 'gmhist-read-file-name)
      (if current-prefix-arg
	  (let ((fn (filename-at-point)))
	    (gmhist-read-file-name
	     prompt (default-directory) fn nil
	     ;; the INITIAL arg is only accepted in Emacs 19 or with gmhist:
	     fn))
	(gmhist-read-file-name prompt (default-directory)))
    ;; Else gmhist is not available, thus no initial input possible.
    ;; Could use filename-at-point as default and mung prompt...ugh.
    ;; Nah, get gmhist, folks!
    (read-file-name prompt (default-directory))))

(defun filename-at-point ()
  "Get the filename closest to point, but don't change your position.
Has a preference for looking backward when not directly on a symbol."
  ;; Not at all perfect - point must be right in the name.
  (let ((filename-chars ".a-zA-Z0-9---_/:$") start end filename
	(bol (save-excursion (beginning-of-line) (point)))
	(eol (save-excursion (end-of-line) (point))))
    (save-excursion
      ;; first see if you're just past a filename
      (if (not (eobp))
	  (if (looking-at "[] \t\n[{}()]") ; whitespace or some parens
	      (progn
		(skip-chars-backward " \n\t\r({[]})")
		(if (not (bobp))
		    (backward-char 1)))))
      (if (string-match (concat "[" filename-chars "]")
			(char-to-string (following-char)))
	  (progn
	    (skip-chars-backward filename-chars)
	    (setq start (point))
	    (if (string-match "[/~]" (char-to-string (preceding-char)))
		(setq start (1- start)))
	    (skip-chars-forward filename-chars))
	(error "No file found around point!"))
      (expand-file-name (buffer-substring start (point))))))

(defun find-this-file (fn)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME, creating one if none already exists.

Interactively, with a prefix arg, calls `filename-at-point'.
Useful to edit the file mentioned in the buffer you are editing, or to
test if that file exists: use minibuffer completion after snatching the
name or part of it."
  (interactive (list (find-file-read-filename-at-point "Find file: ")))
  (find-file (expand-file-name fn)))

(defun find-this-file-other-window (fn)
  "Edit file FILENAME in other window.
Switch to a buffer visiting file FILENAME, creating one if none already exists.

Interactively, with a prefix arg, call `filename-at-point'.
Useful to edit the file mentioned in the buffer you are editing, or to
test if that file exists: use minibuffer completion after snatching the
name or part of it."
  (interactive (list (find-file-read-filename-at-point "Find file: ")))
  (find-file-other-window (expand-file-name fn)))

(defun dired-smart-shell-command (cmd &optional insert)
  "Like function `shell-command', but in the current Tree Dired directory."
  (interactive "sShell command: \nP")
  (let ((default-directory (default-directory)))
    (shell-command cmd insert)))

(if (fboundp 'gmhist-make-magic)
    (gmhist-make-magic 'dired-smart-shell-command 'shell-history))

(defun dired-smart-background-shell-command (cmd)
  "Run a shell command in the background.
Like function `background' but in the current Tree Dired directory."
  (interactive "s%% ")
  (shell-command (concat "cd " (default-directory) "; " cmd " &")))

(if (fboundp 'gmhist-make-magic)
    (gmhist-make-magic 'dired-smart-background-shell-command 'shell-history))


;; Local variables for Dired buffers

(defvar dired-local-variables-file ".dired"
  "If non-nil, filename for local variables for Dired.
If Dired finds a file with that name in the current directory, it will
temporarily insert it into the dired buffer and run `hack-local-variables'.

Type \\[info] and and `g' `(emacs)File Variables' `RET' for more info on
local variables.")

(defun dired-hack-local-variables ()
  "Parse, and bind or evaluate as appropriate, any local variables
for current dired buffer.
See variable `dired-local-variables-file'."
  (if (and dired-local-variables-file
	   (file-exists-p dired-local-variables-file))
      (let (buffer-read-only opoint )
	(save-excursion
	  (goto-char (point-max))
	  (setq opoint (point-marker))
	  (insert "\^L\n")
	  (insert-file-contents dired-local-variables-file))
	(let ((buffer-file-name dired-local-variables-file))
	  (hack-local-variables))
	;; Must delete it as (eobp) is often used as test for last
	;; subdir in dired.el.
	(delete-region opoint (point-max))
	(set-marker opoint nil))))

;; Guess what shell command to apply to a file.

(defvar dired-guess-have-gnutar nil
  "*If non-nil, name of the GNU tar executable (e.g. \"tar\" or \"gnutar\").
GNU tar's `z' switch is used for compressed tar files.
If you don't have GNU tar, set this to nil: a pipe using `zcat' is then used.")

(defvar dired-auto-shell-command-alist-default
  (list
   (list "\\.tar$" (if dired-guess-have-gnutar
		    (concat dired-guess-have-gnutar " xvf")
		  "tar xvf"))
   ;; regexps for compressed archives must come before the .Z rule to
   ;; be recognized:
   (list "\\.tar\\.Z$" (if dired-guess-have-gnutar
		      (concat dired-guess-have-gnutar " zxvf")
		    (concat "zcat * | tar xvf -")))
   '("\\.shar.Z$" "zcat * | unshar")
   '("\\.uu$" "uudecode")
   '("\\.hqx$" "mcvert")
   '("\\.sh$" "sh")			; execute shell scripts
   '("\\.xbm$" "bitmap")		; view X11 bitmaps
   '("\\.gp$" "gnuplot")
   '("\\.gif$" "xv")			; view gif pictures
   '("\\.fig$" "xfig")			; edit fig pictures
   '("\.tex$" "latex" "tex")
   '("\\.texi\\(nfo\\)?$" "makeinfo" "texi2dvi")
   (if (eq window-system 'x)		; under X, offer both...
       '("\\.dvi$"  "xtex" "dvips")	; ...preview and printing
     '("\\.dvi$" "dvips"))
   '("\\.Z$" "uncompress")
   ;; some popular archivers:
   '("\\.zoo$" "zoo x//")
   '("\\.zip$" "unzip")
   '("\\.lzh$" "lharc x")
   '("\\.arc$" "arc x")
   '("\\.shar$" "unshar")		; use "sh" if you don't have unshar
   )

  "Default for variable `dired-auto-shell-command-alist' (which see).
Set this to nil to turn off shell command guessing.")

(defvar dired-auto-shell-command-alist nil
  "*If non-nil, an alist of file regexps and their suggested commands.
Dired shell commands will look up the name of a file in this list
and suggest the matching command as default.

Each element of this list looks like

    \(REGEXP COMMAND...\)

where each COMMAND can either be a string or a lisp expression that
evaluates to a string.  If several COMMANDs are given, the first one
will be the default and minibuffer completion will use the given set.

These rules take precedence over the predefined rules in the variable
`dired-auto-shell-command-alist-default' (to which they are prepended).

You can set this variable in your ~/.emacs.  For example, to add
rules for `.foo' and `.bar' files, write

\(setq dired-auto-shell-command-alist
      (list (list \"\\\\.foo$\" \"FOO-COMMAND\");; fixed rule
             ;; possibly more rules ...
	     (list \"\\\\.bar$\";; rule with condition test
		   '(if condition
			 \"BAR-COMMAND-1\"
		       \"BAR-COMMAND-2\")))\)
")

(setq dired-auto-shell-command-alist
      (if dired-auto-shell-command-alist;; join user and default value:
	  (append dired-auto-shell-command-alist
	      dired-auto-shell-command-alist-default)
	;; else just copy the default value:
	dired-auto-shell-command-alist-default))

(defun dired-guess-default (files)
  ;; Guess a shell command for FILES.
  ;; Returns a command or a list of commands.
  ;; You may want to redefine this to try something smarter.
  (if (or (cdr files)
	  (null dired-auto-shell-command-alist))
      nil				; If more than one file, don't guess
    (let* ((file (car files))
	   (alist dired-auto-shell-command-alist)
	   elt re cmds)
      (while alist
	(setq elt (car alist)
	      re (car elt)
	      alist (cdr alist))
	(if (string-match re file)
	    (setq cmds (cdr elt)
		  alist nil)))
      (cond ((not (cdr cmds)) (eval (car cmds))) ; single command
	    (t (mapcar (function eval) cmds))))))

(defun dired-guess-shell-command (prompt files)
  ;;"Ask user with PROMPT for a shell command, guessing a default from FILES."
  (let ((default (dired-guess-default files))
	default-list old-history val (failed t))
    (if (not (featurep 'gmhist))
	(read-string prompt (if (listp default) (car default) default))
      ;; else we have gmhist
      (if (null default)
	  (read-with-history-in 'dired-shell-command-history prompt)
	(or (boundp 'dired-shell-command-history)
	    (setq dired-shell-command-history nil))
	(setq old-history dired-shell-command-history)
	(if (listp default)
	    ;; more than one guess
	    (setq default-list default
		  default (car default)
		  prompt (concat
			  prompt
			  (format "{%d guesses} " (length default-list))))
	  ;; just one guess
	  (setq default-list (list default)))
	(put 'dired-shell-command-history 'default default)
	;; push guesses onto history so that they can be retrieved with M-p
	(setq dired-shell-command-history
	      (append default-list dired-shell-command-history))
	;; the unwind-protect returns VAL, and we too.
	(unwind-protect
	    (progn
	      (setq val (read-with-history-in
			 'dired-shell-command-history prompt)
		    failed nil)
	      val)
	  (progn
	    ;; Undo pushing onto the history list so that an aborted
	    ;; command doesn't get the default in the next command.
	    (setq dired-shell-command-history old-history)
	    (if (not failed)
		(or (equal val (car-safe dired-shell-command-history))
		    (setq dired-shell-command-history
			  (cons val dired-shell-command-history))))))))))

;; redefine dired.el's version:
(defun dired-read-shell-command (prompt arg files)
  "Read a dired shell command using generic minibuffer history.
This command tries to guess a command from the filename(s)
from the variable `dired-auto-shell-command-alist' (which see)."
  (dired-mark-pop-up
   nil 'shell files			; bufname type files
   'dired-guess-shell-command		; function &rest args
   (format prompt (dired-mark-prompt arg files)) files))


;; Byte-compile-and-load (requires jwz@lucid.com's new byte compiler)
(defun dired-do-byte-compile-and-load (&optional arg)
  "Byte compile marked and load (or next ARG) Emacs lisp files.
This requires jwz@lucid.com's new optimizing byte compiler."
  (interactive "P")
  (dired-mark-map-check (function dired-byte-compile-and-load) arg
			'byte-compile-and-load t))

(defun dired-byte-compile-and-load ()
  ;; Return nil for success, offending file name else.
  (let* (buffer-read-only
	 (from-file (dired-get-filename))
	 (new-file (byte-compile-dest-file from-file)))
    (if (not (string-match elisp-source-extention-re from-file))
	(progn
	  (dired-log "Attempt to compile non-elisp file %s\n" from-file)
	  ;; return a non-nil value as error indication
	  (dired-make-relative from-file))
      (save-excursion;; Jamie's compiler may switch buffer
	(byte-compile-and-load-file from-file))
      (dired-remove-file new-file)
      (forward-line)			; insert .elc after its .el file
      (dired-add-file new-file)
      nil)))

;; Visit all marked files simultaneously.
;; After an idea by wurgler@zippysun.math.uakron.edu (Tom Wurgler).

(defun dired-do-find-file (&optional arg)
  "Visit all marked files at once, and display them simultaneously.
See also function `simultaneous-find-file'.
If you want to keep the dired buffer displayed, type \\[split-window-vertically] first.
If you want just the marked files displayed and nothing else, type \\[delete-other-windows] first."
  (interactive "P")
  (simultaneous-find-file (dired-mark-get-files nil arg)))

(defun simultaneous-find-file (file-list)
  "Visit all files in FILE-LIST and display them simultaneously.

The current window is split across all files in FILE-LIST, as evenly
as possible.  Remaining lines go to the bottommost window.

The number of files that can be displayed this way is restricted by
the height of the current window and the variable `window-min-height'."
  ;; It is usually too clumsy to specify FILE-LIST interactively
  ;; unless via dired (dired-do-find-file).
  (let ((size (/ (window-height) (length file-list))))
    (or (<= window-min-height size)
	(error "Too many files to visit simultaneously"))
    (find-file (car file-list))
    (setq file-list (cdr file-list))
    (while file-list
      ;; Split off vertically a window of the desired size
      ;; The upper window will have SIZE lines.  We select the lower
      ;; (larger) window because we want to split that again.
      (select-window (split-window nil size))
      (find-file (car file-list))
      (setq file-list (cdr file-list)))))
