;; DIRED commands for Emacs.  $Revision: 6.1 $
;; Copyright (C) 1985, 1986, 1991 Free Software Foundation, Inc.

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

;; Rewritten in 1990/1991 to add tree features, file marking and
;; sorting by Sebastian Kremer <sk@thp.uni-koeln.de>.

(provide 'dired)

(defconst dired-version (substring "$Revision: 6.1 $" 11 -2)
  "The revision number of Tree Dired (as string).  The complete RCS id is:

  $Id: dired.el,v 6.0 1992/05/15 14:25:45 sk RelBeta $

Don't forget to mention this when reporting bugs to:

  Sebastian Kremer <sk@thp.uni-koeln.de>

Tree dired is available for anonymous ftp in USA in:

  ftp.cs.buffalo.edu:pub/Emacs/diredall.tar.Z

and in Europe at my own site in Germany:

  ftp.uni-koeln.de:/pub/gnu/emacs/diredall.tar.Z")
;; Should perhaps later give bug-gnu-emacs@prep.gnu.ai.mit.edu instead.

;; compatibility package when using Emacs 18.55
(defvar dired-emacs-19-p (equal (substring emacs-version 0 2) "19"))
;;;>>> install (is there a better way to test for Emacs 19?)
(or dired-emacs-19-p
    (require 'emacs-19))

;;; Customizable variables

;;; The funny comments are for autoload.el, to automagically update
;;; loaddefs.

;;;###autoload
(defvar dired-listing-switches "-al"
  "*Switches passed to ls for dired. MUST contain the `l' option.
Can contain even `F', `b', `i' and `s'.")

; Don't use absolute paths as /bin should be in any PATH and people
; may prefer /usr/local/gnu/bin or whatever.  However, chown is
; usually not in PATH.

;;;###autoload
(defvar dired-chown-program
  (if (memq system-type '(hpux dgux usg-unix-v)) "chown" "/etc/chown")
  "*Name of chown command (usully `chown' or `/etc/chown').")

;;;###autoload
(defvar dired-ls-program "ls"
  "*Absolute or relative name of the ls program used by dired.")

;;;###autoload
(defvar dired-ls-F-marks-symlinks t
  "*Informs dired about how ls -lF marks symbolic links.
Set this to t if `dired-ls-program' with -lF marks the symbolic link
itself with a trailing @ (usually the case under Ultrix).

Example: if `ln -s foo bar; ls -F bar' gives `bar -> foo', set it to
nil, if it gives `bar@ -> foo', set it to t.

Dired checks if there is really a @ appended.  Thus, if you have a
marking ls program on one host and a non-marking on another host, and
don't care about symbolic links which really end in a @, you can
always set this variable to t.")

;;;###autoload
(defvar dired-trivial-filenames "^\\.\\.?$\\|^#"
  "*Regexp of files to skip when moving point to the first file of a new directory listing.
Nil means move to the subdir line, t means move to first file.")

;;;###autoload
(defvar dired-keep-marker-move t
  ;; Use t as default so that moved files `take their markers with them'
  "If t, moved marked files are marked if their originals were.
If a character, those files (marked or not) are marked with that character.")

;;;###autoload
(defvar dired-keep-marker-copy ?C
    "If t, copied files are marked if their source files were.
If a character, those files are always marked with that character.")

;;;###autoload
(defvar dired-keep-marker-hardlink ?H
    "If t, hard-linked files are marked if the linked-to files were.
If a character, those files are always marked with that character.")

;;;###autoload
(defvar dired-keep-marker-symlink ?Y
    "If t, symlinked marked files are marked if the linked-to files were.
If a character, those files are always marked with that character.")

;;;###autoload
(defvar dired-dwim-target nil
  "*If non-nil, dired tries to guess a default target directory:
If there is a dired buffer displayed in the next window, use
its current subdir, instead of the current subdir of this dired
buffer.

The target is used in the prompt for file copy, move etc.")

;;;###autoload
(defvar dired-copy-preserve-time nil
  "*If non-nil, Dired preserves the last-modified time in a file copy.
\(This works on only some systems.)\\<dired-mode-map>
Use `\\[dired-do-copy]' with a zero prefix argument to toggle its value.")

;;; Hook variables

(defvar dired-load-hook nil
  "Run after loading dired.
You can customize key bindings or load extensions with this.")

(defvar dired-mode-hook nil
  "Run at the very end of dired-mode.")

(defvar dired-before-readin-hook nil
  "This hook is run before a dired buffer is newly read in (created or reverted).")

(defvar dired-after-readin-hook nil
  "After each listing of a file or directory, this hook is run
with the buffer narrowed to the listing.")
;; Note this can't simply be run inside function dired-ls as the hook
;; functions probably depend on the dired-subdir-alist to be OK.

;;; Internal variables

(defvar dired-marker-char ?*		; the answer is 42
  ;; so that you can write things like
  ;; (let ((dired-marker-char ?X))
  ;;    ;; great code using X markers ...
  ;;    )
  ;; For example, commands operating on two sets of files, A and B.
  ;; Or marking files with digits 0-9.  This could implicate
  ;; concentric sets or an order for the marked files.
  ;; The code depends on dynamic scoping on the marker char.
  "In dired, character used to mark files for later commands.")

(defvar dired-del-marker ?D
  "Character used to flag files for deletion.")

(defvar dired-shrink-to-fit
  (if (fboundp 'baud-rate) (> (baud-rate) search-slow-speed) t)
  "Whether dired shrinks the display buffer to fit the marked files.")

(defvar dired-flagging-regexp nil);; Last regexp used to flag files.

(defvar dired-directory nil
  "The directory name or shell wildcard passed as argument to ls.
Local to each dired buffer.")

(defvar dired-actual-switches nil
  "The actual (buffer-local) value of `dired-listing-switches'.")

(defvar dired-re-inode-size "[0-9 \t]*"
  "Regexp for optional initial inode and file size as produced by ls' -i and -s flags.")

;; These regexps must be tested at beginning-of-line, but are also
;; used to search for next matches, so neither omitting "^" nor
;; replacing "^" by "\n" (to make it slightly faster) will work.

(defvar dired-re-mark "^[^ \n]")
;; "Regexp matching a marked line.
;; Important: the match ends just after the marker."
(defvar dired-re-maybe-mark "^. ")
;; Note that dired-re-inode-size allows for an arbitray amount of
;; whitespace, making nested indentation in dired-nstd.el work.
(defvar dired-re-dir (concat dired-re-maybe-mark dired-re-inode-size "d"))
(defvar dired-re-sym (concat dired-re-maybe-mark dired-re-inode-size "l"))
(defvar dired-re-exe;; match ls permission string of an executable file
  (mapconcat (function
	      (lambda (x)
		(concat dired-re-maybe-mark dired-re-inode-size x)))
	     '("-[-r][-w][xs][-r][-w].[-r][-w]."
	       "-[-r][-w].[-r][-w][xs][-r][-w]."
	       "-[-r][-w].[-r][-w].[-r][-w][xst]")
	     "\\|"))
(defvar dired-re-dot "^.* \\.\\.?/?$")	; with -F, might end in `/'

(defvar dired-subdir-alist nil
  "Association list of subdirectories and their buffer positions:

  \((LASTDIR . LASTMARKER) ... (DEFAULT-DIRECTORY . FIRSTMARKER)).")

(defvar dired-subdir-regexp "^. \\([^ \n\r]+\\)\\(:\\)[\n\r]"
  "Regexp matching a maybe hidden subdirectory line in ls -lR output.
Subexpression 1 is the subdirectory proper, no trailing colon.
The match starts at the beginning of the line and ends after the end
of the line (\\n or \\r).
Subexpression 2 must end right before the \\n or \\r.")


;;; Macros must be defined before they are used - for the byte compiler.

;; Returns the count if any work was done, nil otherwise.
(defmacro dired-mark-if (predicate msg)
  (` (let (buffer-read-only count)
       (save-excursion
	 (setq count 0)
	 (if (, msg) (message "Marking %ss..." (, msg)))
	 (goto-char (point-min))
	 (while (not (eobp))
	   (if (, predicate)
	       (progn
		 (delete-char 1)
		 (insert dired-marker-char)
		 (setq count (1+ count))))
	   (forward-line 1))
	 (if (, msg) (message "%s %s%s %s%s."
			  count
			  (, msg)
			  (dired-plural-s count)
			  (if (eq dired-marker-char ?\040) "un" "")
			  (if (eq dired-marker-char dired-del-marker)
			      "flagged" "marked"))))
       (and (> count 0) count))))

(defmacro dired-mark-map (body arg &optional show-progress)
;;  "Macro: Perform BODY with point somewhere on each marked line
;;and return a list of BODY's results.
;;If no marked file could be found, execute BODY on the current line.
;;  If ARG is an integer, use the next ARG (or previous -ARG, if ARG<0)
;;  files instead of the marked files.
;;  In that case point is dragged along.  This is so that commands on
;;  the next ARG (instead of the marked) files can be chained easily.
;;  If ARG is otherwise non-nil, use current file instead.
;;If optional third arg SHOW-PROGRESS evaluates to non-nil,
;;   redisplay the dired buffer after each file is processed.
;;No guarantee is made about the position on the marked line.
;;  BODY must ensure this itself if it depends on this.
;;Search starts at the beginning of the buffer, thus the car of the list
;;  corresponds to the line nearest to the buffer's bottom.  This
;;  is also true for (positive and negative) integer values of ARG.
;;BODY should not be too long as it is expanded four times."
;;
;;Warning: BODY must not add new lines before point - this may cause an
;;endless loop.
;;This warning should not apply any longer, sk  2-Sep-1991 14:10.
  (` (prog1
	 (let (buffer-read-only case-fold-search found results)
	   (if (, arg)
	       (if (integerp (, arg))
		   (progn;; no save-excursion, want to move point.
		     (dired-repeat-over-lines
		      (, arg)
		      (function (lambda ()
				  (if (, show-progress) (sit-for 0))
				  (setq results (cons (, body) results)))))
		     (if (< (, arg) 0)
			 (nreverse results)
		       results))
		 ;; non-nil, non-integer ARG means use current file:
		 (list (, body)))
	     (let ((regexp (dired-marker-regexp)) next-position)
	       (save-excursion
		 (goto-char (point-min))
		 ;; remember position of next marked file before BODY
		 ;; can insert lines before the just found file,
		 ;; confusing us by finding the same marked file again
		 ;; and again and...
		 (setq next-position (and (re-search-forward regexp nil t)
					  (point-marker))
		       found (not (null next-position)))
		 (while next-position
		   (goto-char next-position)
		   (if (, show-progress) (sit-for 0))
		   (setq results (cons (, body) results))
		   ;; move after last match
		   (goto-char next-position)
		   (forward-line 1)
		   (set-marker next-position nil)
		   (setq next-position (and (re-search-forward regexp nil t)
					    (point-marker)))))
	       (if found
		   results
		 (list (, body))))))
       ;; save-excursion loses, again
       (dired-move-to-filename))))

(defun dired-mark-get-files (&optional localp arg)
  "Return the marked files as list of strings.
The list is in the same order as the buffer, that is, the car is the
  first marked file.
Values returned are normally absolute pathnames.
Optional arg LOCALP as in `dired-get-filename'.
Optional second argument ARG forces to use other files.  If ARG is an
  integer, use the next ARG files.  If ARG is otherwise non-nil, use
  current file.  Usually ARG comes from the current prefix arg."
  (nreverse (save-excursion (dired-mark-map (dired-get-filename localp) arg))))


;; Function dired-ls is redefinable for VMS, ange-ftp, Prospero or
;; other special applications.

;; dired-ls
;; - must insert _exactly_one_line_ describing FILE if WILDCARD and
;;   FULL-DIRECTORY-P is nil.
;;   The single line of output must display FILE's name as it was
;;   given, namely, an absolute path name.
;; - must insert exactly one line for each file if WILDCARD or
;;   FULL-DIRECTORY-P is t, plus one optional "total" line
;;   before the file lines, plus optional text after the file lines.
;;   Lines are delimited by "\n", so filenames containing "\n" are not
;;   allowed.
;;   File lines should display the basename, not a path name.
;; - must drag point after inserted text
;; - must be consistent with
;;   - functions dired-move-to-filename, (these two define what a file line is)
;;   		 dired-move-to-end-of-filename,
;;		 dired-between-files, (shortcut for (not (dired-move-to-filename)))
;;   		 dired-insert-headerline
;;   		 dired-after-subdir-garbage (defines what a "total" line is)
;;   - variables dired-subdir-regexp
(defun dired-ls (file switches &optional wildcard full-directory-p)
;  "Insert ls output of FILE, formatted according to SWITCHES.
;Optional third arg WILDCARD means treat FILE as shell wildcard.
;Optional fourth arg FULL-DIRECTORY-P means file is a directory and
;switches do not contain `d', so that a full listing is expected.
;
;Uses dired-ls-program (and shell-file-name if WILDCARD) to do the work."
  (if wildcard
      (let ((default-directory (file-name-directory file)))
	(call-process shell-file-name nil t nil
		      "-c" (concat dired-ls-program " -d " switches " "
				   (file-name-nondirectory file))))
    (call-process dired-ls-program nil t nil switches file)))

;; The dired command

(defun dired-read-dir-and-switches (str)
  ;; For use in interactive.
  (reverse (list
	    (if current-prefix-arg
		(read-string "Dired listing switches: "
			     dired-listing-switches))
	    (read-file-name (format "Dired %s(directory): " str)
			    nil default-directory nil))))

;;;###autoload (define-key ctl-x-map "d" 'dired)
;;;###autoload
(defun dired (dirname &optional switches)
  "\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
With an optional prefix argument you can specify the ls SWITCHES that are used.
Dired displays a list of files in DIRNAME (which may also have
  shell wildcards appended to select certain files).
You can move around in it with the usual commands.
You can flag files for deletion with \\<dired-mode-map>\\[dired-flag-file-deleted] and then delete them by
  typing \\[dired-do-deletions].
Type \\[describe-mode] after entering dired for more info.

If DIRNAME is already in a dired buffer, that buffer is used without refresh."
  ;; Cannot use (interactive "D") because of wildcards.
  (interactive (dired-read-dir-and-switches ""))
  (switch-to-buffer (dired-noselect dirname switches)))

;;;###autoload (define-key ctl-x-4-map "d" 'dired-other-window)
;;;###autoload
(defun dired-other-window (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but selects in another window."
  (interactive (dired-read-dir-and-switches "in other window "))
  (switch-to-buffer-other-window (dired-noselect dirname switches)))

;;;###autoload
(defun dired-noselect (dirname &optional switches)
  "Like `dired' but returns the dired buffer as value, does not select it."
  (or dirname (setq dirname default-directory))
  ;; This loses the distinction between "/foo/*/" and "/foo/*" that
  ;; some shells make:
  (setq dirname (expand-file-name (directory-file-name dirname)))
  (if (file-directory-p dirname)
      (setq dirname (file-name-as-directory dirname)))
  (dired-internal-noselect dirname switches))

;; Separate function from dired-noselect for the sake of dired-vms.el.
(defun dired-internal-noselect (dirname &optional switches)
  ;; If there is an existing dired buffer for DIRNAME, just leave
  ;; buffer as it is (don't even call dired-revert).
  ;; This saves time especially for deep trees or with ange-ftp.
  ;; The user can type `g'easily, and it is more consistent with find-file.
  ;; But if SWITCHES are given they are probably different from the
  ;; buffer's old value, so call dired-sort-other, which does
  ;; revert the buffer.
  ;; A pity we can't possibly do "Directory has changed - refresh? "
  ;; like find-file does...maybe in the GNU OS.
  (let* ((buffer (dired-find-buffer-nocreate dirname))
	 ;; note that buffer already is in dired-mode, if found
	 (new-buffer-p (not buffer))
	 (old-buf (current-buffer)))
    (or buffer
	(let ((default-major-mode 'fundamental-mode))
	  ;; We don't want default-major-mode to run hooks and set auto-fill
	  ;; or whatever, now that dired-mode does not
	  ;; kill-all-local-variables any longer.
	  (setq buffer (create-file-buffer (directory-file-name dirname)))))
    (set-buffer buffer)
    (if (not new-buffer-p)		; existing buffer ...
	(if switches			; ... but new switches
	    (dired-sort-other switches))	; this calls dired-revert
      ;; Else a new buffer
      (setq default-directory (if (file-directory-p dirname)
				  dirname
				(file-name-directory dirname)))
      (or switches (setq switches dired-listing-switches))
      (dired-mode dirname switches)
      ;; default-directory and dired-actual-switches are set now
      ;; (buffer-local), so we can call dired-readin:
      (let ((failed t))
	(unwind-protect
	    (progn (dired-readin dirname buffer)
		   (setq failed nil))
	  ;; dired-readin can fail if parent directories are inaccessible.
	  ;; Don't leave an empty buffer around in that case.
	  (if failed (kill-buffer buffer))))
      ;; No need to narrow since the whole buffer contains just
      ;; dired-readin's output, nothing else.  The hook can
      ;; successfully use dired functions (e.g. dired-get-filename)
      ;; as the subdir-alist has been built in dired-readin.
      (run-hooks 'dired-after-readin-hook)
      (goto-char (point-min))
      (dired-initial-position dirname))
    (set-buffer old-buf)
    buffer))

;; This differs from dired-buffers-for-dir in that it does not consider
;; subdirs of default-directory and searches for the first match only
(defun dired-find-buffer-nocreate (dirname)
  (let (found (blist (buffer-list)))
    (while blist
      (save-excursion
        (set-buffer (car blist))
	(if (and (eq major-mode 'dired-mode)
		 (equal dired-directory dirname))
	    (setq found (car blist)
		  blist nil)
	  (setq blist (cdr blist)))))
    found))


;; Read in a new dired buffer

;; dired-readin differs from dired-insert-subdir in that it accepts
;; wildcards, erases the buffer, and builds the subdir-alist anew
;; (including making it buffer-local and clearing it first).
(defun dired-readin (dirname buffer)
  ;; default-directory and dired-actual-switches must be buffer-local
  ;; and initialized by now.
  ;; Thus we can test (equal default-directory dirname) instead of
  ;; (file-directory-p dirname) and save a filesystem transaction.
  ;; Also, we can run this hook which may want to modify the switches
  ;; based on default-directory, e.g. with ange-ftp to a SysV host
  ;; where ls won't understand -Al switches.
  (setq dirname (expand-file-name dirname))
  (run-hooks 'dired-before-readin-hook)
  (save-excursion
    (message "Reading directory %s..." dirname)
    (set-buffer buffer)
    (let (buffer-read-only)
      (widen)
      (erase-buffer)
      (dired-readin-insert dirname)
      (indent-rigidly (point-min) (point-max) 2)
      ;; We need this to make the root dir have a header line as all
      ;; other subdirs have:
      (goto-char (point-min))
      (dired-insert-headerline default-directory)
      ;; can't run dired-after-readin-hook here, it may depend on the subdir
      ;; alist to be OK.
      )
    (message "Reading directory %s...done" dirname)
    (set-buffer-modified-p nil)
    ;; Must first make alist buffer local and set it to nil because
    ;; dired-build-subdir-alist will call dired-clear-alist first
    (set (make-local-variable 'dired-subdir-alist) nil)
    (let (case-fold-search)
      (if (string-match "R" dired-actual-switches)
	  (dired-build-subdir-alist)
	;; no need to parse the buffer if listing is not recursive
	(dired-simple-subdir-alist)))))

;; Subroutines of dired-readin

(defun dired-readin-insert (dirname)
  ;; Just insert listing for DIRNAME, assuming a clean buffer.
  (if (equal default-directory dirname);; i.e., (file-directory-p dirname)
      (dired-ls (if (or (let (case-fold-search)
			  (string-match "R" dired-actual-switches))
			(eq system-type 'vax-vms))
		    dirname
		  ;; On SysV derived system, symbolic links to
		  ;; directories are not resolved, while on BSD
		  ;; derived it suffices to let DIRNAME end in slash.
		  ;; We always let it end in "/." since it does no
		  ;; harm on BSD and makes Dired work on such links on
		  ;; SysV.
		  ;; Cannot do this with -R since "dir/./subdir"
		  ;; headerlines would result, utterly confusing dired.
		  (concat dirname "."))
		dired-actual-switches nil t)
    (if (not (file-readable-p
	      (directory-file-name (file-name-directory dirname))))
	(error "Directory %s inaccessible or nonexistent" dirname)
      ;; else assume it contains wildcards:
      (dired-ls dirname dired-actual-switches t)
      (save-excursion;; insert wildcard instead of total line:
	(goto-char (point-min))
	(insert "wildcard " (file-name-nondirectory dirname) "\n")))))

(defun dired-insert-headerline (dir);; also used by dired-insert-subdir
  ;; Insert DIR's headerline with no trailing slash, exactly like ls
  ;; would, and put cursor where dired-build-subdir-alist puts subdir
  ;; boundaries.
  (save-excursion (insert "  " (directory-file-name dir) ":\n")))


;; Reverting a dired buffer

(defun dired-revert (&optional arg noconfirm)
  ;; Reread the dired buffer.  Must also be called after
  ;; dired-actual-switches have changed.
  ;; Should not fail even on completely garbaged buffers.
  ;; Preserves old cursor, marks/flags, hidden-p.
  (widen)				; just in case user narrowed
  (let ((opoint (point))
	(ofile (dired-get-filename nil t))
	(mark-alist nil)		; save marked files
	(hidden-subdirs (dired-remember-hidden))
	(old-subdir-alist (cdr (reverse dired-subdir-alist))) ; except pwd
	case-fold-search		; we check for upper case ls flags
	buffer-read-only)
    (goto-char (point-min))
    (setq mark-alist;; only after dired-remember-hidden since this unhides:
	  (dired-remember-marks (point-min) (point-max)))
    ;; treat top level dir extra (it may contain wildcards)
    (dired-readin dired-directory (current-buffer))
    (let ((dired-after-readin-hook nil))
      ;; don't run that hook for each subdir...
      (dired-insert-old-subdirs old-subdir-alist))
    (dired-mark-remembered mark-alist)	; mark files that were marked
    ;; ... run the hook for the whole buffer, and only after markers
    ;; have been reinserted (else omitting in dired-x would omit marked files)
    (run-hooks 'dired-after-readin-hook)	; no need to narrow
    (or (and ofile (dired-goto-file ofile)) ; move cursor to where it
	(goto-char opoint))		; was before
    (dired-move-to-filename)
    (save-excursion			; hide subdirs that were hidden
      (mapcar (function (lambda (dir)
			  (if (dired-goto-subdir dir)
			      (dired-hide-subdir 1))))
	      hidden-subdirs)))
  ;; outside of the let scope
  (setq buffer-read-only t))

;; Subroutines of dired-revert
;; Some of these are also used when inserting subdirs.

(defun dired-remember-marks (beg end)
  ;; Return alist of files and their marks, from BEG to END.
  (if selective-display			; must unhide to make this work.
      (let (buffer-read-only)
	(subst-char-in-region beg end ?\r ?\n)))
  (let (fil chr alist)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward dired-re-mark end t)
	(if (setq fil (dired-get-filename nil t))
	    (setq chr (preceding-char)
		  alist (cons (cons fil chr) alist)))))
    alist))

(defun dired-mark-remembered (alist)
  ;; Mark all files remembered in ALIST.
  (let (elt fil chr)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist)
	    fil (car elt)
	    chr (cdr elt))
      (if (dired-goto-file fil)
	  (save-excursion
	    (beginning-of-line)
	    (delete-char 1)
	    (insert chr))))))

(defun dired-remember-hidden ()
  (let ((l dired-subdir-alist) dir result)
    (while l
      (setq dir (car (car l))
	    l (cdr l))
      (if (dired-subdir-hidden-p dir)
	  (setq result (cons dir result))))
    result))

(defun dired-insert-old-subdirs (old-subdir-alist)
  ;; Try to insert all subdirs that were displayed before
  (or (string-match "R" dired-actual-switches)
      (let (elt dir)
	(while old-subdir-alist
	  (setq elt (car old-subdir-alist)
		old-subdir-alist (cdr old-subdir-alist)
		dir (car elt))
	  (condition-case ()
	      (dired-insert-subdir dir)
	    (error nil))))))


;; dired mode key bindings and initialization

(defvar dired-mode-map nil "Local keymap for dired-mode buffers.")
(if dired-mode-map
    nil
  ;; Force `f' rather than `e' in the mode doc:
  (fset 'dired-advertised-find-file 'dired-find-file)
  ;; This looks ugly when substitute-command-keys uses C-d instead d:
  ;;  (define-key dired-mode-map "\C-d" 'dired-flag-file-deleted)

  (setq dired-mode-map (make-keymap))
  (suppress-keymap dired-mode-map)
  ;; Commands to mark certain categories of files
  (define-key dired-mode-map "#" 'dired-flag-auto-save-files)
  (define-key dired-mode-map "*" 'dired-mark-executables)
  (define-key dired-mode-map "." 'dired-clean-directory)
  (define-key dired-mode-map "/" 'dired-mark-directories)
  (define-key dired-mode-map "@" 'dired-mark-symlinks)
  ;; Upper case keys (except !, c, r) for operating on the marked files
  (define-key dired-mode-map "c" 'dired-do-copy)
  (define-key dired-mode-map "r" 'dired-do-move)
  (define-key dired-mode-map "!" 'dired-do-shell-command)
  (define-key dired-mode-map "B" 'dired-do-byte-compile)
  (define-key dired-mode-map "C" 'dired-do-compress)
  (define-key dired-mode-map "G" 'dired-do-chgrp)
  (define-key dired-mode-map "H" 'dired-do-hardlink)
  (define-key dired-mode-map "L" 'dired-do-load)
  (define-key dired-mode-map "M" 'dired-do-chmod)
  (define-key dired-mode-map "O" 'dired-do-chown)
  (define-key dired-mode-map "P" 'dired-do-print)
  (define-key dired-mode-map "U" 'dired-do-uncompress)
  (define-key dired-mode-map "X" 'dired-do-delete)
  (define-key dired-mode-map "Y" 'dired-do-symlink)
  ;; exceptions to the upper key rule
  (define-key dired-mode-map "D" 'dired-diff)
  (define-key dired-mode-map "W" 'dired-why)
  ;; Tree Dired commands
  (define-key dired-mode-map "\M-\C-?" 'dired-unflag-all-files)
  (define-key dired-mode-map "\M-\C-d" 'dired-tree-down)
  (define-key dired-mode-map "\M-\C-u" 'dired-tree-up)
  (define-key dired-mode-map "\M-\C-n" 'dired-next-subdir)
  (define-key dired-mode-map "\M-\C-p" 'dired-prev-subdir)
  ;; move to marked files
  (define-key dired-mode-map "\M-{" 'dired-prev-marked-file)
  (define-key dired-mode-map "\M-}" 'dired-next-marked-file)
  ;; kill marked files
  (define-key dired-mode-map "\M-k" 'dired-do-kill)
  ;; Make all regexp commands share a `%' prefix:
  (fset 'dired-regexp-prefix (make-sparse-keymap))
  (define-key dired-mode-map "%" 'dired-regexp-prefix)
  (define-key dired-mode-map "%u" 'dired-upcase)
  (define-key dired-mode-map "%l" 'dired-downcase)
  (define-key dired-mode-map "%d" 'dired-flag-regexp-files)
  (define-key dired-mode-map "%m" 'dired-mark-files-regexp)
  (define-key dired-mode-map "%r" 'dired-do-rename-regexp)
  (define-key dired-mode-map "%c" 'dired-do-copy-regexp)
  (define-key dired-mode-map "%H" 'dired-do-hardlink-regexp)
  (define-key dired-mode-map "%Y" 'dired-do-symlink-regexp)
  ;; Lower keys for commands not operating on all the marked files
  (define-key dired-mode-map "d" 'dired-flag-file-deleted)
  (define-key dired-mode-map "e" 'dired-find-file)
  (define-key dired-mode-map "f" 'dired-advertised-find-file)
  (define-key dired-mode-map "g" 'revert-buffer)
  (define-key dired-mode-map "h" 'describe-mode)
  (define-key dired-mode-map "i" 'dired-maybe-insert-subdir)
  (define-key dired-mode-map "k" 'dired-kill-line-or-subdir)
  (define-key dired-mode-map "l" 'dired-do-redisplay)
  (define-key dired-mode-map "m" 'dired-mark-subdir-or-file)
  (define-key dired-mode-map "n" 'dired-next-line)
  (define-key dired-mode-map "o" 'dired-find-file-other-window)
  (define-key dired-mode-map "p" 'dired-previous-line)
  (define-key dired-mode-map "q" 'dired-quit)
  (define-key dired-mode-map "s" 'dired-sort-toggle-or-edit)
  (define-key dired-mode-map "u" 'dired-unmark-subdir-or-file)
  (define-key dired-mode-map "v" 'dired-view-file)
  (define-key dired-mode-map "x" 'dired-do-deletions)
  (define-key dired-mode-map "~" 'dired-flag-backup-files)
  (define-key dired-mode-map "\M-~" 'dired-backup-diff)
  (define-key dired-mode-map "+" 'dired-create-directory)
  ;; moving
  (define-key dired-mode-map "<" 'dired-prev-dirline)
  (define-key dired-mode-map ">" 'dired-next-dirline)
  (define-key dired-mode-map "^" 'dired-up-directory)
  (define-key dired-mode-map " "  'dired-next-line)
  (define-key dired-mode-map "\C-n" 'dired-next-line)
  (define-key dired-mode-map "\C-p" 'dired-previous-line)
  ;; hiding
  (define-key dired-mode-map "$" 'dired-hide-subdir)
  (define-key dired-mode-map "=" 'dired-hide-all)
  ;; misc
  (define-key dired-mode-map "?" 'dired-summary)
  (define-key dired-mode-map "\177" 'dired-backup-unflag)
  (define-key dired-mode-map "\C-_" 'dired-undo)
  (define-key dired-mode-map "\C-xu" 'dired-undo)
  )

(or (equal (assq 'dired-sort-mode minor-mode-alist)
	   '(dired-sort-mode dired-sort-mode))
    ;; Test whether this has already been done in case dired is reloaded
    ;; There may be several elements with dired-sort-mode as car.
    (setq minor-mode-alist
	  (cons '(dired-sort-mode dired-sort-mode)
		;; dired-sort-mode is nil outside dired
		minor-mode-alist)))

;; Dired mode is suitable only for specially formatted data.
(put 'dired-mode 'mode-class 'special)

(defun dired-mode (&optional dirname switches)
  "\
Mode for \"editing\" directory listings.
In dired, you are \"editing\" a list of the files in a directory and
  \(optionally) its subdirectories, in the format of `ls -lR'.
  Each directory is a page: use \\[backward-page] and \\[forward-page] to move pagewise.
\"Editing\" means that you can run shell commands on files, visit,
  compress, load or byte-compile them, change their file attributes
  and insert subdirectories into the same buffer.  You can \"mark\"
  files for later commands or \"flag\" them for deletion, either file
  by file or all files matching certain criteria.
You can move using the usual cursor motion commands.\\<dired-mode-map>
Letters no longer insert themselves.  Digits are prefix arguments.
Instead, type \\[dired-flag-file-deleted] to flag a file for Deletion.
Type \\[dired-mark-subdir-or-file] to Mark a file or subdirectory for later commands.
  Most commands operate on the marked files and use the current file
  if no files are marked.  Use a numeric prefix argument to operate on
  the next ARG (or previous -ARG if ARG<0) files, or just `1'
  to operate on the current file only.  Prefix arguments override marks.
  Mark-using commands display a list of failures afterwards.  Type \\[dired-why] to see
  why something went wrong.
Type \\[dired-unmark-subdir-or-file] to Unmark a file or all files of a subdirectory.
Type \\[dired-backup-unflag] to back up one line and unflag.
Type \\[dired-do-deletions] to eXecute the deletions requested.
Type \\[dired-advertised-find-file] to Find the current line's file
  (or dired it in another buffer, if it is a directory).
Type \\[dired-find-file-other-window] to find file or dired directory in Other window.
Type \\[dired-maybe-insert-subdir] to Insert a subdirectory in this buffer.
Type \\[dired-do-move] to Rename a file or move the marked files to another directory.
Type \\[dired-do-copy] to Copy files.
Type \\[dired-sort-toggle-or-edit] to toggle sorting by name/date or change the ls switches.
Type \\[revert-buffer] to read all currently expanded directories again.
  This retains all marks and hides subdirs again that were hidden before.
SPC and DEL can be used to move down and up by lines.

If dired ever gets confused, you can either type \\[revert-buffer] \
to read the
directories again, type \\[dired-do-redisplay] \
to relist a single or the marked files or a
subdirectory, or type \\[dired-build-subdir-alist] to parse the buffer
again for the directory tree.

Customization variables (rename this buffer and type \\[describe-variable] on each line
for more info):

  dired-listing-switches
  dired-trivial-filenames
  dired-shrink-to-fit
  dired-marker-char
  dired-del-marker
  dired-keep-marker-move
  dired-keep-marker-copy
  dired-keep-marker-hardlink
  dired-keep-marker-symlink

Hooks (use \\[describe-variable] to see their documentation):

  dired-before-readin-hook
  dired-after-readin-hook
  dired-mode-hook
  dired-load-hook

Keybindings:
\\{dired-mode-map}"
  ;; Not to be called interactively (e.g. dired-directory will be set
  ;; to default-directory, which is wrong with wildcards).
  (kill-all-local-variables)
  (use-local-map dired-mode-map)
  (dired-advertise)			; default-directory is already set
  (setq major-mode 'dired-mode
	mode-name "Dired"
	case-fold-search nil
	buffer-read-only t
	selective-display t		; for subdirectory hiding
	mode-line-buffer-identification '("Dired: %17b"))
  (set (make-local-variable 'revert-buffer-function)
       (function dired-revert))
  (set (make-local-variable 'page-delimiter)
       "\n\n")
  (set (make-local-variable 'dired-directory)
       (or dirname default-directory))
  (set (make-local-variable 'list-buffers-directory)
       dired-directory)
  (set (make-local-variable 'dired-actual-switches)
       (or switches dired-listing-switches))
  (make-local-variable 'dired-sort-mode)
  (dired-sort-other dired-actual-switches t)
  (run-hooks 'dired-mode-hook))


(defun dired-check-ls-l ()
  (let (case-fold-search)
    (or (string-match "l" dired-actual-switches)
	(error "Dired needs -l in ls switches"))))

(defun dired-repeat-over-lines (arg function)
  ;; This version skips non-file lines.
  (beginning-of-line)
  (while (and (> arg 0) (not (eobp)))
    (setq arg (1- arg))
    (beginning-of-line)
    (while (and (not (eobp)) (dired-between-files)) (forward-line 1))
    (save-excursion (funcall function))
    (forward-line 1))
  (while (and (< arg 0) (not (bobp)))
    (setq arg (1+ arg))
    (forward-line -1)
    (while (and (not (bobp)) (dired-between-files)) (forward-line -1))
    (beginning-of-line)
    (save-excursion (funcall function))
    (dired-move-to-filename))
  (dired-move-to-filename))

(defun dired-flag-file-deleted (arg)
  "In dired, flag the current line's file for deletion.
With prefix arg, repeat over several lines.

If on a subdir headerline, mark all its files except `.' and `..'."
  (interactive "P")
  (let ((dired-marker-char dired-del-marker))
    (dired-mark-subdir-or-file arg)))

(defun dired-quit ()
  "Bury the current dired buffer."
  (interactive)
  (bury-buffer))

(defun dired-summary ()
  (interactive)
  ;>> this should check the key-bindings and use substitute-command-keys if non-standard
  (message
   "d-elete, u-ndelete, x-punge, f-ind, o-ther window, r-ename, c-opy, h-elp"))

(defun dired-create-directory (directory)
  "Create a directory called DIRECTORY."
  (interactive
   (list (read-file-name "Create directory: " (dired-current-directory))))
  (let ((expanded (directory-file-name (expand-file-name directory))))
    (make-directory expanded)
    (dired-add-file expanded)
    (dired-move-to-filename)))

(defun dired-undo ()
  "Undo in a dired buffer.
This doesn't recover lost files, it is just normal undo with temporarily
writeable buffer.  You can use it to recover marks, killed lines or subdirs.
In the latter case, you have to do \\[dired-build-subdir-alist] to
parse the buffer again."
  (interactive)
  (let (buffer-read-only)
    (undo)))

(defun dired-unflag (arg)
  "In dired, remove the current line's delete flag then move to next line.
Optional prefix ARG says how many lines to unflag."
  (interactive "p")
  (dired-repeat-over-lines arg
    '(lambda ()
       (let (buffer-read-only)
	 (delete-char 1)
	 (insert " ")
	 (forward-char -1)
	 nil))))

(defun dired-backup-unflag (arg)
  "In dired, move up lines and remove deletion flag there.
Optional prefix ARG says how many lines to unflag; default is one line."
  (interactive "p")
  (dired-unflag (- arg)))

(defun dired-next-line (arg)
  "Move down lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (next-line arg)
  (dired-move-to-filename))

(defun dired-previous-line (arg)
  "Move up lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (previous-line arg)
  (dired-move-to-filename))

(defun dired-up-directory ()
  "Run dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary."
  (interactive)
  (let* ((dir (dired-current-directory))
	 (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
	(dired-goto-subdir up)
	(progn
	  (dired up)
	  (dired-goto-file dir)))))

(defun dired-find-file ()
  "In dired, visit the file or directory named on this line."
  (interactive)
  (find-file (dired-get-filename)))

(defun dired-view-file ()
  "In dired, examine a file in view mode, returning to dired when done.
When file is a directory, show it in this buffer if it is inserted;
otherwise, display it in another buffer."
  (interactive)
  (if (file-directory-p (dired-get-filename))
      (or (dired-goto-subdir (dired-get-filename))
	  (dired (dired-get-filename)))
    (view-file (dired-get-filename))))

(defun dired-find-file-other-window ()
  "In dired, visit this file or directory in another window."
  (interactive)
  (find-file-other-window (dired-get-filename)))

(defun dired-get-filename (&optional localp no-error-if-not-filep)
  "In dired, return name of file mentioned on this line.
Value returned normally includes the directory name.
Optional arg LOCALP with value `no-dir' means don't include directory
  name in result.  A value of t means use path name relative to
  `default-directory', which still may contain slashes if in a subdirectory.
Optional arg NO-ERROR-IF-NOT-FILEP means return nil if no filename on
  this line, otherwise an error occurs."
  (let (case-fold-search file p1 p2)
    (save-excursion
      (if (setq p1 (dired-move-to-filename (not no-error-if-not-filep)))
	  (setq p2 (dired-move-to-end-of-filename no-error-if-not-filep))))
    ;; nil if no file on this line, but no-error-if-not-filep is t:
    (if (setq file (and p1 p2 (buffer-substring p1 p2)))
	;; Check if ls quoted the names, and unquote them.
	;; Using read to unquote is much faster than substituting
	;; \007 (4 chars) -> ^G  (1 char) etc. in a lisp loop.
	(cond ((string-match "b" dired-actual-switches) ; System V ls
	       ;; This case is about 20% slower than without -b.
	       (setq file
		     (read
		      (concat "\""
			      ;; some ls -b don't escape quotes, argh!
			      ;; This is not needed for GNU ls, though.
			      (or (dired-string-replace-match
				   "\\([^\\]\\)\"" file "\\1\\\\\"")
				  file)
			      "\""))))
	      ;; If you do this, update dired-insert-subdir-validate too
	      ;; ((string-match "Q" dired-actual-switches) ; GNU ls
	      ;;  (setq file (read file)))
	      ))
    (if (eq localp 'no-dir)
	file
      (and file (concat (dired-current-directory localp) file)))))

(defun dired-move-to-filename (&optional raise-error eol)
  "In dired, move to first char of filename on this line.
Returns position (point) or nil if no filename on this line."
  ;; This is the UNIX version.
  (or eol (setq eol (progn (end-of-line) (point))))
  (beginning-of-line)
  (if (string-match "l" dired-actual-switches)
      (if (re-search-forward
	   "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[ ]+[0-9]+"
	   eol t)
	  (progn
	    (skip-chars-forward " ")	; there is one SPC after day of month
	    (skip-chars-forward "^ " eol) ; move after time of day (or year)
	    (skip-chars-forward " " eol) ; there is space before the file name
	    ;; Actually, if the year instead of clock time is displayed,
	    ;; there are (only for some ls programs?) two spaces instead
	    ;; of one before the name.
	    ;; If we could depend on ls inserting exactly one SPC we
	    ;; would not bomb on names _starting_ with SPC.
	    (point))
	(if raise-error
	    (error "No file on this line")
	  nil))
    ;; else ls switches don't contain -l.
    ;; Note that even if we make dired-move-to-filename and
    ;; dired-move-to-end-of-filename (and thus dired-get-filename)
    ;; work, all commands that gleaned information from the permission
    ;; bits (like dired-mark-directories) will cease to work properly.
    (if (eolp)
	(if raise-error
	    (error "No file on this line")
	  nil)
      ;; skip marker, if any
      (forward-char))
    (skip-chars-forward " ")
    (point)))

(defun dired-move-to-end-of-filename (&optional no-error)
  ;; Assumes point is at beginning of filename,
  ;; thus the rwx bit re-search-backward below will succeed in *this*
  ;; line if at all.  So, it should be called only after
  ;; (dired-move-to-filename t).
  ;; On failure, signals an error (with non-nil NO-ERROR just returns nil).
  ;; This is the UNIX version.
  (let (opoint file-type executable symlink hidden case-fold-search used-F eol)
    ;; case-fold-search is nil now, so we can test for capital F:
    (setq used-F (string-match "F" dired-actual-switches)
	  opoint (point)
          eol (save-excursion (end-of-line) (point))
	  hidden (and selective-display
		      (save-excursion (search-forward "\r" eol t))))
    (if hidden
	nil
      (save-excursion;; Find out what kind of file this is:
	;; Restrict perm bits to be non-blank,
	;; otherwise this matches one char to early (looking backward):
	;; "l---------" (some systems make symlinks that way)
	;; "----------" (plain file with zero perms)
	(if (re-search-backward
	     "\\([^ ]\\)[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)"
	     nil t)
	    (setq file-type (char-after (match-beginning 1))
		  symlink (eq file-type ?l)
		  ;; Only with -F we need to know whether it's an executable
		  executable (and
			      used-F
			      (string-match
			       "[xst]";; execute bit set anywhere?
			       (concat
				(buffer-substring (match-beginning 2)
						  (match-end 2))
				(buffer-substring (match-beginning 3)
						  (match-end 3))
				(buffer-substring (match-beginning 4)
						  (match-end 4))))))
	  (or no-error
	      (not (string-match "l" dired-actual-switches))
	      (error "No file on this line"))))
      ;; Move point to end of name:
      (if symlink
	  (if (search-forward " ->" eol t)
	      (progn
		(forward-char -3)
		(and used-F
		     dired-ls-F-marks-symlinks
		     (eq (preceding-char) ?@);; did ls really mark the link?
		     (forward-char -1))))
	(goto-char eol);; else not a symbolic link
	;; ls -lF marks dirs, sockets and executables with exactly one
	;; trailing character. (Executable bits on symlinks ain't mean
	;; a thing, even to ls, but we know it's not a symlink.)
	(and used-F
	     ;; -F may not actually be honored, e.g. by an FTP ls in ange-ftp
	     (let ((char (preceding-char)))
	       (or (and (eq file-type ?d) (eq char ?/))
		   (and executable (eq char ?*))
		   (and (eq file-type ?s) (eq char ?=))))
	     (forward-char -1))))
    (or no-error
	(not (eq opoint (point)))
	(error (if hidden
		   (substitute-command-keys
		    "File line is hidden, type \\[dired-hide-subdir] to unhide")
		 "No file on this line")))
    (if (eq opoint (point))
	nil
      (point))))


;; Perhaps something could be done to handle VMS' own backups.

(defun dired-clean-directory (keep)
  "Flag numerical backups for deletion.
Spares `dired-kept-versions' latest versions, and `kept-old-versions' oldest.
Positive prefix arg KEEP overrides `dired-kept-versions';
Negative prefix arg KEEP overrides `kept-old-versions' with KEEP made positive.

To clear the flags on these files, you can use \\[dired-flag-backup-files]
with a prefix argument."
  (interactive "P")
  (setq keep (if keep (prefix-numeric-value keep) dired-kept-versions))
  (let ((early-retention (if (< keep 0) (- keep) kept-old-versions))
	(late-retention (if (<= keep 0) dired-kept-versions keep))
	(file-version-assoc-list ()))
    (message "Cleaning numerical backups (keeping %d late, %d old)..."
	     late-retention early-retention)
    ;; Look at each file.
    ;; If the file has numeric backup versions,
    ;; put on file-version-assoc-list an element of the form
    ;; (FILENAME . VERSION-NUMBER-LIST)
    (dired-map-dired-file-lines (function dired-collect-file-versions))
    ;; Sort each VERSION-NUMBER-LIST,
    ;; and remove the versions not to be deleted.
    (let ((fval file-version-assoc-list))
      (while fval
	(let* ((sorted-v-list (cons 'q (sort (cdr (car fval)) '<)))
	       (v-count (length sorted-v-list)))
	  (if (> v-count (+ early-retention late-retention))
	      (rplacd (nthcdr early-retention sorted-v-list)
		      (nthcdr (- v-count late-retention)
			      sorted-v-list)))
	  (rplacd (car fval)
		  (cdr sorted-v-list)))
	(setq fval (cdr fval))))
    ;; Look at each file.  If it is a numeric backup file,
    ;; find it in a VERSION-NUMBER-LIST and maybe flag it for deletion.
    (dired-map-dired-file-lines (function dired-trample-file-versions))
    (message "Cleaning numerical backups...done")))

;;; Subroutines of dired-clean-directory.

(defun dired-map-dired-file-lines (fun)
  ;; Perform FUN with point at the end of each non-directory line.
  ;; FUN takes one argument, the filename (complete pathname).
  (dired-check-ls-l)
  (save-excursion
    (let (file buffer-read-only)
      (goto-char (point-min))
      (while (not (eobp))
	(save-excursion
	  (and (not (looking-at dired-re-dir))
	       (not (eolp))
	       (setq file (dired-get-filename nil t)) ; nil on non-file
	       (progn (end-of-line)
		      (funcall fun file))))
	(forward-line 1)))))

(defun dired-collect-file-versions (fn)
  ;;  "If it looks like file FN has versions, return a list of the versions.
  ;;That is a list of strings which are file names.
  ;;The caller may want to flag some of these files for deletion."
    (let* ((base-versions
	    (concat (file-name-nondirectory fn) ".~"))
	   (bv-length (length base-versions))
	   (possibilities (file-name-all-completions
			   base-versions
			   (file-name-directory fn)))
	   (versions (mapcar 'backup-extract-version possibilities)))
      (if versions
	  (setq file-version-assoc-list (cons (cons fn versions)
					      file-version-assoc-list)))))

(defun dired-trample-file-versions (fn)
  (let* ((start-vn (string-match "\\.~[0-9]+~$" fn))
	 base-version-list)
    (and start-vn
	 (setq base-version-list	; there was a base version to which
	       (assoc (substring fn 0 start-vn)	; this looks like a
		      file-version-assoc-list))	; subversion
	 (not (memq (string-to-int (substring fn (+ 2 start-vn)))
		    base-version-list))	; this one doesn't make the cut
	 (progn (beginning-of-line)
		(delete-char 1)
		(insert dired-del-marker)))))


;; Keeping Dired buffers in sync with the filesystem and with each other

(defvar dired-buffers nil
  ;; Enlarged by dired-advertise
  ;; Queried by function dired-buffers-for-dir. When this detects a
  ;; killed buffer, it is removed from this list.
  "Alist of directories and their associated dired buffers.")

(defun dired-buffers-for-dir (dir)
;; Return a list of buffers that dired DIR (top level or in-situ subdir).
;; The list is in reverse order of buffer creation, most recent last.
;; As a side effect, killed dired buffers for DIR are removed from
;; dired-buffers.
  (setq dir (file-name-as-directory dir))
  (let ((alist dired-buffers) result elt)
    (while alist
      (setq elt (car alist))
      (if (dired-in-this-tree dir (car elt))
	  (let ((buf (cdr elt)))
	    (if (buffer-name buf)
		(if (assoc dir (save-excursion
				 (set-buffer buf)
				 dired-subdir-alist))
		    (setq result (cons buf result)))
	      ;; else buffer is killed - clean up:
	      (setq dired-buffers (delq elt dired-buffers)))))
      (setq alist (cdr alist)))
    result))

(defun dired-advertise ()
  ;;"Advertise in variable `dired-buffers' that we dired `default-directory'."
  ;; With wildcards we actually advertise too much.
  (if (memq (current-buffer) (dired-buffers-for-dir default-directory))
      t					; we have already advertised ourselves
    (setq dired-buffers
	  (cons (cons default-directory (current-buffer))
		dired-buffers))))

(defun dired-unadvertise (dir)
  ;; Remove DIR from the buffer alist in variable dired-buffers.
  ;; This has the effect of removing any buffer whose main directory is DIR.
  ;; It does not affect buffers in which DIR is a subdir.
  ;; Removing is also done as a side-effect in dired-buffer-for-dir.
  (setq dired-buffers
      (delq (assoc dir dired-buffers) dired-buffers)))

(defun dired-fun-in-all-buffers (directory fun &rest args)
  ;; In all buffers dired'ing DIRECTORY, run FUN with ARGS.
  ;; Return list of buffers where FUN succeeded (i.e., returned non-nil).
  (let ((buf-list (dired-buffers-for-dir directory))
	(obuf (current-buffer))
	buf success-list)
    (while buf-list
      (setq buf (car buf-list)
	    buf-list (cdr buf-list))
      (unwind-protect
	  (progn
	    (set-buffer buf)
	    (if (apply fun args)
		(setq success-list (cons (buffer-name buf) success-list))))
	(set-buffer obuf)))
    success-list))

(defun dired-add-file (filename &optional marker-char)
  (dired-fun-in-all-buffers
   (file-name-directory filename)
   (function dired-add-entry) filename marker-char))

(defun dired-add-entry (filename &optional marker-char)
  ;; Add a new entry for FILENAME, optionally marking it
  ;; with MARKER-CHAR (a character, else dired-marker-char is used).
  ;; Note that this adds the entry `out of order' if files sorted by
  ;; time, etc.
  ;; At least this version inserts in the right subdirectory (if present).
  ;; And it skips "." or ".." (see `dired-trivial-filenames').
  ;; Hidden subdirs are exposed if a file is added there.
  (setq filename (directory-file-name filename))
  ;; Entry is always for files, even if they happen to also be directories
  (let ((opoint (point))
	(cur-dir (dired-current-directory))
	(directory (file-name-directory filename))
	reason)
    (setq filename (file-name-nondirectory filename)
	  reason
	  (catch 'not-found
	    (if (string= directory cur-dir)
		(progn
		  (if (dired-subdir-hidden-p cur-dir)
		      (dired-unhide-subdir))
		  ;; We are already where we should be, except when
		  ;; point is before the subdir line or its total line.
		  (let ((p (dired-after-subdir-garbage cur-dir)))
		    (if (< (point) p)
			(goto-char p))))
	      ;; else try to find correct place to insert
	      (if (dired-goto-subdir directory)
		  (progn;; unhide if necessary
		    (if (looking-at "\r");; point is at end of subdir line
			(dired-unhide-subdir))
		    ;; found - skip subdir and `total' line
		    ;; and uninteresting files like . and ..
		    ;; This better not moves into the next subdir!
		    (dired-goto-next-nontrivial-file))
		;; not found
		(throw 'not-found "Subdir not found")))
	    ;; found and point is at The Right Place:
	    (let (buffer-read-only)
	      (beginning-of-line)
	      (dired-add-entry-do-indentation marker-char)
	      (dired-ls (dired-make-absolute filename directory);; don't expand `.' !
			(concat dired-actual-switches "d"))
	      (forward-line -1)
	      ;; We want to have the non-directory part, only:
	      (let* ((beg (dired-move-to-filename t)) ; error for strange output
		     (end (dired-move-to-end-of-filename)))
		(setq filename (buffer-substring beg end))
		(delete-region beg end)
		(insert (file-name-nondirectory filename)))
	      (if dired-after-readin-hook;; the subdir-alist is not affected...
		  (save-excursion;; ...so we can run it right now:
		    (save-restriction
		      (beginning-of-line)
		      (narrow-to-region (point) (save-excursion
						  (forward-line 1) (point)))
		      (run-hooks 'dired-after-readin-hook))))
	      (dired-move-to-filename))
	    ;; return nil if all went well
	    nil))
    (if reason				; don't move away on failure
	(goto-char opoint))
    (not reason)))			; return t on succes, nil else

;; This is a separate function for the sake of nested dired format.
(defun dired-add-entry-do-indentation (marker-char)
  ;; two spaces or a marker plus a space:
  (insert (if marker-char
	      (if (integerp marker-char) marker-char dired-marker-char)
	    ?\040)
	  ?\040))

(defun dired-after-subdir-garbage (dir)
  ;; Return pos of first file line of DIR, skipping header and total
  ;; or wildcard lines.
  ;; Important: never moves into the next subdir.
  ;; DIR is assumed to be unhidden.
  ;; Will probably be redefined for VMS etc.
  (save-excursion
    (or (dired-goto-subdir dir) (error "This cannot happen"))
    (forward-line 1)
    (while (and (not (eolp))		; don't cross subdir boundary
		(not (dired-move-to-filename)))
	(forward-line 1))
    (point)))

(defun dired-remove-file (file)
  (dired-fun-in-all-buffers
   (file-name-directory file) (function dired-remove-entry) file))

(defun dired-remove-entry (file)
  (save-excursion
    (and (dired-goto-file file)
	 (let (buffer-read-only)
	   (delete-region (progn (beginning-of-line) (point))
			  (save-excursion (forward-line 1) (point)))))))

(defun dired-relist-file (file)
  (dired-fun-in-all-buffers (file-name-directory file)
			    (function dired-relist-entry) file))

(defun dired-relist-entry (file)
  ;; Relist the line for FILE, or just add it if it did not exist.
  ;; FILE must be an absolute pathname.
  (let (buffer-read-only marker)
    ;; If cursor is already on FILE's line delete-region will cause
    ;; save-excursion to fail because of floating makers,
    ;; moving point to beginning of line.  Sigh.
    (save-excursion
      (and (dired-goto-file file)
	   (delete-region (progn (beginning-of-line)
				 (setq marker (following-char))
				 (point))
			  (save-excursion (forward-line 1) (point))))
      (setq file (directory-file-name file))
      (dired-add-entry file (if (eq ?\040 marker) nil marker)))))

(defun dired-update-file-line (file)
  ;; Delete the current line, and insert an entry for FILE.
  ;; If FILE is nil, then just delete the current line.
  ;; Keeps any marks that may be present in column one (doing this
  ;; here is faster than with dired-add-entry's optional arg).
  ;; Does not update other dired buffers.  Use dired-relist-entry for that.
  (beginning-of-line)
  (let ((char (following-char)) (opoint (point)))
    (delete-region (point) (progn (forward-line 1) (point)))
    (if file
	(progn
	  (dired-add-entry file)
	  ;; Replace space by old marker without moving point.
	  ;; Faster than goto+insdel inside a save-excursion?
	  (subst-char-in-region opoint (1+ opoint) ?\040 char))))
  (dired-move-to-filename))


;; Running subprocesses, checking and logging of their errors.

(defvar dired-log-buf "*Dired log*")

(defun dired-why ()
  "Pop up a buffer with error log output from Dired.
A group of errors from a single command ends with a formfeed.
Thus, use \\[backward-page] to find the beginning of a group of errors."
  (interactive)
  (let ((obuf (current-buffer)))
    (pop-to-buffer dired-log-buf)
    (goto-char (point-max))
    (recenter -1)
    (switch-to-buffer-other-window obuf)))

(defun dired-log (log &rest args)
  ;; Log a message or the contents of a buffer.
  ;; If LOG is a string and there are more args, it is formatted with
  ;; those ARGS.  Usually the LOG string ends with a \n.
  ;; End each bunch of errors with (dired-log t): this inserts
  ;; current time and buffer, and a \f (formfeed).
  (let ((obuf (current-buffer)))
    (unwind-protect			; want to move point
	(progn
	  (set-buffer (get-buffer-create dired-log-buf))
	  (goto-char (point-max))
	  (let (buffer-read-only)
	    (cond ((stringp log)
		   (insert (if args
			       (apply (function format) log args)
			     log)))
		  ((bufferp log)
		   (insert-buffer log))
		  ((eq t log)
		   (insert "\n\t" (current-time-string)
			   "\tBuffer `" (buffer-name obuf) "'\n\f\n")))))
      (set-buffer obuf))))

(defun dired-log-summary (log &rest args)
  ;; Log a summary describing a bunch of errors.
  (apply (function dired-log) (concat "\n" log) args)
  (dired-log t))

;; In Emacs 19 this will return program's exit status.
;; This is a separate function so that ange-ftp can redefine it.
(defun dired-call-process (program discard &rest arguments)
;  "Run PROGRAM with output to current buffer unless DISCARD is t.
;Remaining arguments are strings passed as command arguments to PROGRAM."
  (apply 'call-process program nil (not discard) nil arguments))

(defun dired-check-process-checker (exit-status)
  ;; In Emacs 19, EXIT-STATUS comes from (dired-)call-process
  ;; Then this function should return (/= 0 exit-status)
  ;; In Emacs 18 the exit status is not accessible, so we
  ;; do the following which is not always correct as some compress
  ;; programs are verbose by default or otherwise braindamaged
  (if dired-emacs-19-p
      (/= 0 exit-status);; >>> install (does it work in Emacs 19?)
    (/= 0 (buffer-size)))		; run in program's output buffer
  ;; If have you one of those compress programs, you might
  ;; want to redefine this function to look closer at compress' output.
  ;; This is why it is a separate function.
  )

(defun dired-check-process (msg program &rest arguments)
;  "Display MSG while running PROGRAM, and check for output.
;Remaining arguments are strings passed as command arguments to PROGRAM.
; On error as determined by dired-check-process-checker, insert output
; in a log buffer and return the offending ARGUMENTS or PROGRAM.
; Caller can cons up a list of failed args.
;Else returns nil for success."
  (let (err-buffer err (dir default-directory))
    (message "%s..." msg)
    (save-excursion
      ;; Get a clean buffer for error output:
      (setq err-buffer (get-buffer-create " *dired-check-process output*"))
      (set-buffer err-buffer)
      (erase-buffer)
      (setq default-directory dir	; caller's default-directory
	    err (dired-check-process-checker
		 (apply (function dired-call-process) program nil arguments)))
      (if err
	  (progn
	    (dired-log (concat program " " (prin1-to-string arguments) "\n"))
	    (dired-log err-buffer)
	    (or arguments program t))
	(kill-buffer err-buffer)
	(message "%s...done" msg)
	nil))))

;;; 7K
;;;###begin dired-cmd.el
;; Diffing and compressing

(defun dired-diff (file &optional switches)
  "Compare file at point with file FILE using `diff'.
FILE defaults to the file at the mark.
The prompted-for file is the first file given to `diff'.
Prefix arg lets you edit the diff switches.  See the command `diff'."
  (interactive
   (let ((default (if (mark)
		      (save-excursion (goto-char (mark))
				      (dired-get-filename t t)))))
     (list (read-file-name (format "Diff %s with: %s"
				   (dired-get-filename t)
				   (if default
				       (concat "(default " default ") ")
				     ""))
			   (dired-current-directory) default t)
	   (if (fboundp 'diff-read-switches)
	       (diff-read-switches "Options for diff: ")))))
  (if switches				; Emacs 19's diff has but two
      (diff file (dired-get-filename t) switches) ; args (yet ;-)
    (diff file (dired-get-filename t))))

(defun dired-backup-diff (&optional switches)
  "Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
Prefix arg lets you edit the diff switches.  See the command `diff'."
  (interactive (list (if (fboundp 'diff-read-switches)
			 (diff-read-switches "Diff with switches: "))))
  (let (bak ori (file (dired-get-filename)))
    (if (backup-file-name-p file)
	(setq bak file
	      ori (file-name-sans-versions file))
      (setq bak (or (latest-backup-file file)
		    (error "No backup found for %s" file))
	    ori file))
    (if switches
	(diff bak ori switches)
      (diff bak ori))))

;;>>> install (move this function into files.el)
(defun latest-backup-file (fn)	; actually belongs into files.el
  "Return the latest existing backup of FILE, or nil."
  ;; First try simple backup, then the highest numbered of the
  ;; numbered backups.
  ;; Ignore the value of version-control because we look for existing
  ;; backups, which maybe were made earlier or by another user with
  ;; a different value of version-control.
  (setq fn (expand-file-name fn))
  (or
   (let ((bak (make-backup-file-name fn)))
     (if (file-exists-p bak) bak))
   (let* ((dir (file-name-directory fn))
	  (base-versions (concat (file-name-nondirectory fn) ".~"))
	  (bv-length (length base-versions)))
     (concat dir
	     (car (sort
		   (file-name-all-completions base-versions dir)
		   ;; bv-length is a fluid var for backup-extract-version:
		   (function
		    (lambda (fn1 fn2)
		      (> (backup-extract-version fn1)
			 (backup-extract-version fn2))))))))))

;; This is a separate function for the sake of ange-ftp.el
(defun dired-compress-make-compressed-filename (from-file &optional reverse)
;;  "Converts a filename FROM-FILE to the filename of the associated
;;  compressed file.  With an optional argument REVERSE, the reverse
;;  conversion is done."
  (if reverse
      (substring from-file 0 -2)
    (concat from-file ".Z")))

(defun dired-compress ()
  ;; Compress current file.  Return nil for success, offending filename else.
  (dired-check-ls-l)
  (let* (buffer-read-only
	 (from-file (dired-get-filename))
	 (to-file (dired-compress-make-compressed-filename from-file)))
    (cond ((save-excursion (beginning-of-line)
			   (looking-at dired-re-sym))
	   (dired-log (concat "Attempt to compress a symbolic link:\n"
			      from-file))
	   (dired-make-relative from-file))
	  ((dired-check-process (concat "Compressing " from-file)
				"compress" "-f" from-file)
	   ;; errors from the process are already logged by dired-check-process
	   (dired-make-relative from-file))
	(t
	 (dired-update-file-line to-file)
	 nil))))

(defun dired-uncompress ()
  ;; Uncompress current file.  Return nil for success, offending filename else.
  (let* (buffer-read-only
	 (from-file (dired-get-filename))
	 (to-file (dired-compress-make-compressed-filename from-file t)))
    (if (dired-check-process (concat "Uncompressing " from-file)
			     "uncompress" from-file)
	(dired-make-relative from-file)
      (dired-update-file-line to-file)
      nil)))

(defun dired-mark-map-check (fun arg op-symbol &optional show-progress)
;  "Map FUN over marked files (with second ARG like in dired-mark-map)
; and display failures.

; FUN takes zero args.  It returns non-nil (the offending object, e.g.
; the short form of the filename) for a failure and probably logs a
; detailed error explanation using function `dired-log'.

; OP-SYMBOL is a symbol describing the operation performed (e.g.
; `compress').  It is used with `dired-mark-pop-up' to prompt the user
; (e.g. with `Compress * [2 files]? ') and to display errors (e.g.
; `Failed to compress 1 of 2 files - type W to see why ("foo")')

; SHOW-PROGRESS if non-nil means redisplay dired after each file."
  (if (dired-mark-confirm op-symbol arg)
      (let* ((total-list;; all of FUN's return values
	      (dired-mark-map (funcall fun) arg show-progress))
	     (total (length total-list))
	     (failures (delq nil total-list))
	     (count (length failures)))
	(if (not failures)
	    (message "%s: %d file%s."
		     (capitalize (symbol-name op-symbol))
		     total (dired-plural-s total))
	  (message "Failed to %s %d of %d file%s - type W to see why %s"
		   (symbol-name op-symbol) count total (dired-plural-s total)
		   ;; this gives a short list of failed files in parens
		   ;; which may be sufficient for the user even
		   ;; without typing `W' for the process' diagnostics
		   failures)
	  ;; end this bunch of errors:
	  (dired-log-summary
	   "Failed to %s %d of %d file%s"
	   (symbol-name op-symbol) count total (dired-plural-s total))))))

(defun dired-do-compress (&optional arg)
  "Compress marked (or next ARG) files.
Type \\[dired-do-uncompress] to uncompress again."
  (interactive "P")
  (dired-mark-map-check (function dired-compress) arg 'compress t))

(defun dired-do-uncompress (&optional arg)
  "Uncompress marked (or next ARG) files."
  (interactive "P")
  (dired-mark-map-check (function dired-uncompress) arg 'uncompress t))

;; Commands for Emacs Lisp files - load and byte compile

(defun dired-byte-compile ()
  ;; Return nil for success, offending file name else.
  (let* ((filename (dired-get-filename))
	 (elc-file
	  (if (eq system-type 'vax-vms)
	      (concat (substring filename 0 (string-match ";" filename)) "c")
	    (concat filename "c")))
	 buffer-read-only failure)
    (condition-case err
	(save-excursion (byte-compile-file filename))
      (error
       (setq failure err)))
    (if failure
	(progn
	  (dired-log "Byte compile error for %s:\n%s\n" filename failure)
	  (dired-make-relative filename))
      (dired-remove-file elc-file)
      (forward-line)			; insert .elc after its .el file
      (dired-add-file elc-file)
      nil)))

(defun dired-do-byte-compile (&optional arg)
  "Byte compile marked (or next ARG) Emacs lisp files."
  (interactive "P")
  (dired-mark-map-check (function dired-byte-compile) arg 'byte-compile t))

(defun dired-load ()
  ;; Return nil for success, offending file name else.
  (let ((file (dired-get-filename)) failure)
    (condition-case err
      (load file nil nil t)
      (error (setq failure err)))
    (if (not failure)
	nil
      (dired-log "Load error for %s:\n%s\n" file failure)
      (dired-make-relative file))))

(defun dired-do-load (&optional arg)
  "Load the marked (or next ARG) Emacs lisp files."
  (interactive "P")
  (dired-mark-map-check (function dired-load) arg 'load t))

(defun dired-do-chxxx (attribute-name program op-symbol arg)
  ;; Change file attributes (mode, group, owner) of marked files and
  ;; refresh their file lines.
  ;; ATTRIBUTE-NAME is a string describing the attribute to the user.
  ;; PROGRAM is the program used to change the attribute.
  ;; OP-SYMBOL is the type of operation (for use in dired-mark-pop-up).
  ;; ARG describes which files to use, like in dired-mark-get-files.
  (let* ((files (dired-mark-get-files t arg))
	 (new-attribute
	  (dired-mark-read-string
	   (concat "Change " attribute-name " of %s to: ")
	   nil op-symbol arg files))
	 (operation (concat program " " new-attribute))
	 (failure (apply (function dired-check-process)
			 operation program new-attribute
			 files)))
    (dired-do-redisplay arg);; moves point if ARG is an integer
    (if failure
	(dired-log-summary
	 (message "%s: error - type W to see why." operation)))))

(defun dired-do-chmod (&optional arg)
  "Change the mode of the marked (or next ARG) files.
This calls chmod, thus symbolic modes like `g+w' are allowed."
  (interactive "P")
  (dired-do-chxxx "Mode" "chmod" 'chmod arg))

(defun dired-do-chgrp (&optional arg)
  "Change the group of the marked (or next ARG) files."
  (interactive "P")
  (dired-do-chxxx "Group" "chgrp" 'chgrp arg))

(defun dired-do-chown (&optional arg)
  "Change the owner of the marked (or next ARG) files."
  (interactive "P")
  (dired-do-chxxx "Owner" dired-chown-program 'chown arg))

;;;###end dired-cmd.el


;; Deleting files

(defun dired-do-deletions ()
  "In dired, delete the files flagged for deletion."
  (interactive)
  (let* ((dired-marker-char dired-del-marker)
	 (regexp (dired-marker-regexp))
	 case-fold-search)
    (if (save-excursion (goto-char (point-min))
			(re-search-forward regexp nil t))
	(dired-internal-do-deletions
	 ;; this can't move point since ARG is nil
	 (dired-mark-map (cons (dired-get-filename) (point))
			 nil)
	 nil)
      (message "(No deletions requested)"))))

(defun dired-do-delete (&optional arg)
  "Delete all marked (or next ARG) files."
  ;; This is more consistent with the file marking feature than
  ;; dired-do-deletions.
  (interactive "P")
  (dired-internal-do-deletions
   ;; this may move point if ARG is an integer
   (dired-mark-map (cons (dired-get-filename) (point))
		   arg)
   arg))

(defvar dired-deletion-confirmer 'yes-or-no-p) ; or y-or-n-p?

(defun dired-internal-do-deletions (l arg)
  ;; L is an alist of files to delete, with their buffer positions.
  ;; ARG is the prefix arg.
  ;; Filenames are absolute (VMS needs this for logical search paths).
  ;; (car L) *must* be the *last* (bottommost) file in the dired buffer.
  ;; That way as changes are made in the buffer they do not shift the
  ;; lines still to be changed, so the (point) values in L stay valid.
  ;; Also, for subdirs in natural order, a subdir's files are deleted
  ;; before the subdir itself - the other way around would not work.
  (let ((files (mapcar (function car) l))
	(count (length l))
	(succ 0))
    ;; canonicalize file list for pop up
    (setq files (nreverse (mapcar (function dired-make-relative) files)))
    (if (dired-mark-pop-up
	 " *Deletions*" 'delete files dired-deletion-confirmer
	 (format "Delete %s " (dired-mark-prompt arg files)))
	(save-excursion
	  (let (failures);; files better be in reverse order for this loop!
	    (while l
	      (goto-char (cdr (car l)))
	      (let (buffer-read-only)
		(condition-case err
		    (let ((fn (car (car l))))
		      ;; This test is equivalent to
		      ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
		      ;; but more efficient
		      (if (eq t (car (file-attributes fn)))
			  (remove-directory fn)
			(delete-file fn))
		      ;; if we get here, removing worked
		      (setq succ (1+ succ))
		      (message "%s of %s deletions" succ count)
		      (delete-region (progn (beginning-of-line) (point))
				     (progn (forward-line 1) (point)))
		      (dired-clean-up-after-deletion fn))
		  (error;; catch errors from failed deletions
		   (dired-log "%s\n" err)
		   (setq failures (cons (car (car l)) failures)))))
	      (setq l (cdr l)))
	    (if (not failures)
		(message "%d deletion%s done" count (dired-plural-s count))
	      (dired-log-summary
	       (message "%d of %d deletion%s failed: %s"
			(length failures) count
			(dired-plural-s count)
			(prin1-to-string failures))))))
      (message "(No deletions performed)")))
  (dired-move-to-filename))

;; This is a separate function for the sake of dired-x.el.
(defun dired-clean-up-after-deletion (fn)
  ;; Clean up after a deleted file or directory FN.
  (save-excursion (and (dired-goto-subdir fn)
		       (dired-kill-subdir))))


(defun dired-replace-in-string (regexp newtext string)
  ;; Replace REGEXP with NEWTEXT everywhere in STRING and return result.
  ;; NEWTEXT is taken literally---no \\DIGIT escapes will be recognized.
  (let ((result "") (start 0) mb me)
    (while (string-match regexp string start)
      (setq mb (match-beginning 0)
	    me (match-end 0)
	    result (concat result (substring string start mb) newtext)
	    start me))
    (concat result (substring string start))))

(defun dired-next-dirline (arg &optional opoint)
  "Goto ARG'th next directory file line."
  (interactive "p")
  (dired-check-ls-l)
  (or opoint (setq opoint (point)))
  (if (if (> arg 0)
	  (re-search-forward dired-re-dir nil t arg)
	(beginning-of-line)
	(re-search-backward dired-re-dir nil t (- arg)))
      (dired-move-to-filename)		; user may type `i' or `f'
    (goto-char opoint)
    (error "No more subdirectories")))

(defun dired-prev-dirline (arg)
  "Goto ARG'th previous directory file line."
  (interactive "p")
  (dired-next-dirline (- arg)))

(defun dired-unflag-all-files (flag &optional arg)
  "Remove a specific or all flags from every file.
With an arg, queries for each marked file.
Type \\[help-command] at that time for help."
  (interactive "sRemove flag: (default: all flags) \nP")
  (let ((count 0)
	(re (if (zerop (length flag)) dired-re-mark
	      (concat "^" (regexp-quote flag)))))
    (save-excursion
      (let (buffer-read-only case-fold-search query
			     (help-form "\
Type SPC or `y' to unflag one file, DEL or `n' to skip to next,
`!' to unflag all remaining files with no more questions."))
	(goto-char (point-min))
	(while (re-search-forward re nil t)
	  (if (or (not arg)
		  (dired-query 'query "Unflag file `%s' ? "
			       (dired-get-filename t)))
	      (progn (delete-char -1) (insert " ") (setq count (1+ count))))
	  (forward-line 1))))
    (message "%s" (format "Flags removed: %d %s" count flag) )))

;; pop ups and user input for file marking

(defun dired-marker-regexp ()
  (concat "^" (regexp-quote (char-to-string dired-marker-char))))

(defun dired-plural-s (count)
  (if (= 1 count) "" "s"))

(defun dired-mark-prompt (arg files)
  ;; Return a string for use in a prompt, either the current file
  ;; name, or the marker and a count of marked files.
  (let ((count (length files)))
    (if (= count 1)
	(car files)
      ;; more than 1 file:
      (if (integerp arg)
	  ;; abs(arg) = count
	  ;; Perhaps this is nicer, but it also takes more screen space:
	  ;;(format "[%s %d files]" (if (> arg 0) "next" "previous")
	  ;;                        count)
	  (format "[next %d files]" arg)
	(format "%c [%d files]" dired-marker-char count)))))

(defvar dired-query-alist
  '((?\y . y) (?\040 . y)		; `y' or SPC means accept once
    (?n . n) (?\177 . n)		; `n' or DEL skips once
    (?! . yes)				; `!' accepts rest
    (?q. no) (?\e . no)			; `q' or ESC skips rest
    ;; None of these keys quit - use C-g for that.
    ))

(defun dired-query (qs-var qs-prompt &rest qs-args)
  ;; Query user and return nil or t.
  ;; Store answer in symbol VAR (which must initially be bound to nil).
  ;; Format PROMPT with ARGS.
  ;; Binding variable help-form will help the user who types C-h.
  (let* ((char (symbol-value qs-var))
	 (action (cdr (assoc char dired-query-alist))))
    (cond ((eq 'yes action)
	   t)				; accept, and don't ask again
	  ((eq 'no action)
	   nil)				; skip, and don't ask again
	  (t;; no lasting effects from last time we asked - ask now
	   (let ((qprompt (concat qs-prompt
				  (if help-form
				      (format " [Type yn!q or %s] "
					      (key-description
					       (char-to-string help-char)))
				    " [Type y, n, q or !] ")))
		 result elt)
	     ;; Actually it looks nicer without cursor-in-echo-area - you can
	     ;; look at the dired buffer instead of at the prompt to decide.
	     (apply 'message qprompt qs-args)
	     (setq char (set qs-var (read-char)))
	     (while (not (setq elt (assoc char dired-query-alist)))
	       (message "Invalid char - type %c for help." help-char)
	       (ding)
	       (sit-for 1)
	       (apply 'message qprompt qs-args)
	       (setq char (set qs-var (read-char))))
	     (memq (cdr elt) '(t y yes)))))))

(defun dired-pop-to-buffer (buf)
  ;; Pop up buffer BUF.
  ;; If dired-shrink-to-fit is t, make its window fit its contents.
  (if (not dired-shrink-to-fit)
      (pop-to-buffer (get-buffer-create buf))
    ;; let window shrink to fit:
    (let ((window (selected-window))
	  target-lines w2)
      (cond ;; if split-window-threshold is enabled, use the largest window
            ((and (> (window-height (setq w2 (get-largest-window)))
		     split-height-threshold)
		  (= (screen-width) (window-width w2)))
	     (setq window w2))
	    ;; if the least-recently-used window is big enough, use it
	    ((and (> (window-height (setq w2 (get-lru-window)))
		     (* 2 window-min-height))
		  (= (screen-width) (window-width w2)))
	     (setq window w2)))
      (save-excursion
	(set-buffer buf)
	(goto-char (point-max))
	(skip-chars-backward "\n\r\t ")
	(setq target-lines (count-lines (point-min) (point))))
      (if (<= (window-height window) (* 2 window-min-height))
	  ;; At this point, every window on the screen is too small to split.
	  (setq w2 (display-buffer buf))
	(setq w2 (split-window window
		  (max window-min-height
		       (- (window-height window)
			  (1+ (max window-min-height target-lines)))))))
      (set-window-buffer w2 buf)
      (if (< (1- (window-height w2)) target-lines)
	  (progn
	    (select-window w2)
	    (enlarge-window (- target-lines (1- (window-height w2))))))
      (set-window-start w2 1)
      )))

(defvar dired-no-confirm nil
;;  "If non-nil, list of symbols for commands dired should not confirm.
;;It can be a sublist of
;;
;;  '(byte-compile chgrp chmod chown compress copy delete hardlink load
;;    move print shell symlink uncompress)"
  )

(defun dired-mark-confirm (op-symbol arg)
  ;; Request confirmation from the user that the operation described
  ;; by OP-SYMBOL is to be performed on the marked files.
  ;; Confirmation consists in a y-or-n question with a file list
  ;; pop-up unless OP-SYMBOL is a member of `dired-no-confirm'.
  ;; The files used are determined by ARG (like in dired-mark-get-files).
  (or (memq op-symbol dired-no-confirm)
      (let ((files (dired-mark-get-files t arg)))
	(dired-mark-pop-up nil op-symbol files (function y-or-n-p)
			   (concat (capitalize (symbol-name op-symbol)) " "
				   (dired-mark-prompt arg files) "? ")))))

(defun dired-mark-pop-up (bufname op-symbol files function &rest args)
  ;;"Args BUFNAME OP-SYMBOL FILES FUNCTION &rest ARGS.
  ;;Return FUNCTION's result on ARGS after popping up a window (in a buffer
  ;;named BUFNAME, nil gives \" *Marked Files*\") showing the marked
  ;;files.  Uses function `dired-pop-to-buffer' to do that.
  ;; FUNCTION should not manipulate files.
  ;; It should only read input (an argument or confirmation).
  ;;The window is not shown if there is just one file or
  ;; OP-SYMBOL is a member of the list in `dired-no-confirm'.
  ;;FILES is the list of marked files."
  (or bufname (setq bufname  " *Marked Files*"))
  (if (or (memq op-symbol dired-no-confirm)
	  (= (length files) 1))
      (apply function args)
    (save-excursion
      (set-buffer (get-buffer-create bufname))
      (erase-buffer)
      (dired-format-columns-of-files files))
    (save-window-excursion
      (dired-pop-to-buffer bufname)
      (apply function args))))

(defun dired-format-columns-of-files (files)
  ;; Files should be in forward order for this loop.
  ;; i.e., (car files) = first file in buffer.
  ;; Returns the number of lines used.
  (let* ((maxlen (+ 2 (apply 'max (mapcar 'length files))))
	 (width (- (window-width (selected-window)) 2))
	 (columns (max 1 (/ width maxlen)))
	 (nfiles (length files))
	 (rows (+ (/ nfiles columns)
		  (if (zerop (% nfiles columns)) 0 1)))
	 (i 0)
	 (j 0))
    (setq files (nconc (copy-sequence files) ; fill up with empty fns
		       (make-list (- (* columns rows) nfiles) "")))
    (setcdr (nthcdr (1- (length files)) files) files) ; make circular
    (while (< j rows)
      (while (< i columns)
	(indent-to (* i maxlen))
	(insert (car files))
	(setq files (nthcdr rows files)
	      i (1+ i)))
      (insert "\n")
      (setq i 0
	    j (1+ j)
	    files (cdr files)))
    rows))

;; Read arguments for a mark command of type OP-SYMBOL,
;; perhaps popping up the list of marked files.
;; ARG is the prefix arg and indicates whether the files came from
;; marks (ARG=nil) or a repeat factor (integerp ARG).
;; If the current file was used, the list has but one element and ARG
;; does not matter. (It is non-nil, non-integer in that case, namely '(4)).

(defun dired-mark-read-string (prompt initial op-symbol arg files)
  ;; PROMPT for a string, with INITIAL input.
  ;; Other args are used to give user feedback and pop-up:
  ;; OP-SYMBOL of command, prefix ARG, marked FILES.
  (dired-mark-pop-up
   nil op-symbol files
   (function read-string)
   (format prompt (dired-mark-prompt arg files)) initial))

(defun dired-mark-read-file-name (prompt dir op-symbol arg files)
  (dired-mark-pop-up
   nil op-symbol files
   (function read-file-name)
   (format prompt (dired-mark-prompt arg files)) dir))

(defun dired-mark-file (arg)
  "In dired, mark the current line's file for later commands.
With arg, repeat over several lines.
Use \\[dired-unflag-all-files] to remove all flags."
  (interactive "p")
  (let (buffer-read-only)
    (dired-repeat-over-lines
     arg
     (function (lambda () (delete-char 1) (insert dired-marker-char))))))

(defun dired-next-marked-file (arg &optional wrap opoint)
  "Move to the next marked file, wrapping around the end of the buffer."
  (interactive "p\np")
  (or opoint (setq opoint (point)));; return to where interactively started
  (if (if (> arg 0)
	  (re-search-forward dired-re-mark nil t arg)
	(beginning-of-line)
	(re-search-backward dired-re-mark nil t (- arg)))
      (dired-move-to-filename)
    (if (null wrap)
	(progn
	  (goto-char opoint)
	  (error "No next marked file"))
      (message "(Wraparound for next marked file)")
      (goto-char (if (> arg 0) (point-min) (point-max)))
      (dired-next-marked-file arg nil opoint))))

(defun dired-prev-marked-file (arg &optional wrap)
  "Move to the previous marked file, wrapping around the end of the buffer."
  (interactive "p\np")
  (dired-next-marked-file (- arg) wrap))

(defun dired-file-marker (file)
  ;; Return FILE's marker, or nil if unmarked.
  (save-excursion
    (and (dired-goto-file file)
	 (progn
	   (beginning-of-line)
	   (if (not (equal ?\040 (following-char)))
	       (following-char))))))

(defun dired-read-regexp (prompt &optional initial)
;; This is an extra function so that gmhist can redefine it.
  (setq dired-flagging-regexp
	(read-string prompt (or initial dired-flagging-regexp))))

(defun dired-mark-files-regexp (regexp &optional marker-char)
  "Mark all files matching REGEXP for use in later commands.
A prefix argument means to unmark them instead.
`.' and `..' are never marked.

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\\.o$' for
object files--just `.o' will mark more than you might think."
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
				    " files (regexp): "))
	 (if current-prefix-arg ?\040)))
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (dired-mark-if
     (and (not (looking-at dired-re-dot))
	  (not (eolp))			; empty line
	  (let ((fn (dired-get-filename nil t)))
	    (and fn (string-match regexp (file-name-nondirectory fn)))))
     "matching file")))

(defun dired-flag-regexp-files (regexp)
  "In dired, flag all files containing the specified REGEXP for deletion.
The match is against the non-directory part of the filename.  Use `^'
  and `$' to anchor matches.  Exclude subdirs by hiding them.
`.' and `..' are never flagged."
  (interactive (list (dired-read-regexp "Flag for deletion (regexp): ")))
  (dired-mark-files-regexp regexp dired-del-marker))

(defun dired-mark-symlinks (unflag-p)
  "Mark all symbolic links.
With prefix argument, unflag all those files."
  (interactive "P")
  (dired-check-ls-l)
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char)))
    (dired-mark-if (looking-at dired-re-sym) "symbolic link")))

(defun dired-mark-directories (unflag-p)
  "Mark all directory file lines except `.' and `..'.
With prefix argument, unflag all those files."
  (interactive "P")
  (dired-check-ls-l)
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char)))
    (dired-mark-if (and (looking-at dired-re-dir)
			(not (looking-at dired-re-dot)))
		   "directory file")))

(defun dired-mark-executables (unflag-p)
  "Mark all executable files.
With prefix argument, unflag all those files."
  (interactive "P")
  (dired-check-ls-l)
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char)))
    (dired-mark-if (looking-at dired-re-exe) "executable file")))

;; dired-x.el has a dired-mark-sexp interactive command: mark
;; files for which PREDICATE returns non-nil.

(defun dired-flag-auto-save-files (&optional unflag-p)
  "Flag for deletion files whose names suggest they are auto save files.
A prefix argument says to unflag those files instead."
  (interactive "P")
  (dired-check-ls-l)
  (let ((dired-marker-char (if unflag-p ?\040 dired-del-marker)))
    (dired-mark-if
       (and (not (looking-at dired-re-dir))
	    (let ((fn (dired-get-filename t t)))
	      (if fn (auto-save-file-name-p
		      (file-name-nondirectory fn)))))
       "auto save file")))

(defun dired-flag-backup-files (&optional unflag-p)
  "Flag all backup files (names ending with `~') for deletion.
With prefix argument, unflag these files."
  (interactive "P")
  (dired-check-ls-l)
  (let ((dired-marker-char (if unflag-p ?\040 dired-del-marker)))
    (dired-mark-if
     (and (not (looking-at dired-re-dir))
	  (let ((fn (dired-get-filename t t)))
	    (if fn (backup-file-name-p fn))))
     "backup file")))


;;; Shell commands
;;>>> install (move this function into simple.el)
(defun shell-quote (filename)		; actually belongs into simple.el
  "Quote a file name for inferior shell (see variable shell-file-name)."
  ;; Quote everything except POSIX filename characters.
  ;; This should be safe enough even for really wierd shells.
  (let ((result "") (start 0) end)
    (while (string-match "[^---0-9a-zA-Z_./]" filename start)
      (setq end (match-beginning 0)
	    result (concat result (substring filename start end)
			   "\\" (substring filename end (1+ end)))
	    start (1+ end)))
    (concat result (substring filename start))))

(defun dired-read-shell-command (prompt arg files)
;;  "Read a dired shell command prompting with PROMPT (using read-string).
;;ARG is the prefix arg and may be used to indicate in the prompt which
;;  files are affected.
;;This is an extra function so that you can redefine it, e.g., to use gmhist."
  (dired-mark-pop-up
   nil 'shell files
   (function read-string) (format prompt (dired-mark-prompt arg files))))

;; The in-background argument is only needed in Emacs 18 where
;; shell-command doesn't understand an appended ampersand `&'.
(defun dired-do-shell-command (&optional arg in-background)
  "Run a shell command on the marked files.
If there is output, it goes to a separate buffer.
The list of marked files is appended to the command string unless asterisks
  `*' indicate the place(s) where the list should go.
If no files are marked or a specific numeric prefix arg is given, uses
  next ARG files.  As always, a raw arg (\\[universal-argument]) means the current file.
  The prompt mentions the file(s) or the marker, as appropriate.
With a zero argument, run command on each marked file separately: `cmd *
  foo' results in `cmd F1 foo; ...; cmd Fn foo'.
No automatic redisplay is attempted, as the file names may have
  changed.  Type \\[dired-do-redisplay] to redisplay the marked files.
The shell command has the top level directory as working directory, so
  output files usually are created there instead of in a subdir."
;;Functions dired-run-shell-command and dired-shell-stuff-it do the
;;actual work and can be redefined for customization.
  (interactive "P")
  (let* ((on-each (equal arg 0))
	 (prompt (concat (if in-background "& on " "! on ")
			 (if on-each "each " "")
			 "%s: "))
	 (file-list (dired-mark-get-files t (if on-each nil arg)))
	 ;; Want to give feedback whether this file or marked files are used:
	 (command (dired-read-shell-command
		   prompt (if on-each nil arg) file-list))
	 (result
	  (dired-shell-stuff-it command file-list on-each arg)))
    ;; execute the shell command
    (dired-run-shell-command result in-background)))

;; Might use {,} for bash or csh:
(defvar dired-mark-prefix ""
  "Prepended to marked files in dired shell commands.")
(defvar dired-mark-postfix ""
  "Appended to marked files in dired shell commands.")
(defvar dired-mark-separator " "
  "Separates marked files in dired shell commands.")

(defun dired-shell-stuff-it (command file-list on-each &optional raw-arg)
;; "Make up a shell command line from COMMAND and FILE-LIST.
;; If ON-EACH is t, COMMAND should be applied to each file, else
;; simply concat all files and apply COMMAND to this.
;; FILE-LIST's elements will be quoted for the shell."
;; Might be redefined for smarter things and could then use RAW-ARG
;; (coming from interactive P and currently ignored) to decide what to do.
;; Smart would be a way to access basename or extension of file names.
;; See dired-trns.el for an approach to this.
  ;; Bug: There is no way to quote a *
  ;; On the other hand, you can never accidentally get a * into your cmd.
  (let ((stuff-it
	 (if (string-match "\\*" command)
	     (function (lambda (x)
			 (dired-replace-in-string "\\*" x command)))
	   (function (lambda (x) (concat command " " x))))))
    (if on-each
	(mapconcat stuff-it (mapcar (function shell-quote) file-list) ";")
      (let ((fns (mapconcat (function shell-quote)
			    file-list dired-mark-separator)))
	(if (> (length file-list) 1)
	    (setq fns (concat dired-mark-prefix fns dired-mark-postfix)))
	(funcall stuff-it fns)))))

;; This is an extra function so that it can be redefined by ange-ftp.
(defun dired-run-shell-command (command &optional in-background)
  (if (and in-background (not (string-match "&[ \t]*$" command)))
      (setq command (concat command " &")))
  (shell-command command))

(defun dired-do-print (&optional arg)
  "Print the marked (or next ARG) files.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default."
  (interactive "P")
  (or (listp lpr-switches)
      (error "lpr-switches must be a *list* of strings"))
  (let* ((file-list (dired-mark-get-files t arg))
	 (switches (mapconcat (function identity) lpr-switches " "))
	 (command (dired-mark-read-string
		   "Print %s with: "
		   (concat lpr-command " " switches)
		   'print arg file-list)))
    (dired-run-shell-command (dired-shell-stuff-it command file-list nil))))


;;; 10K
;;;###begin dired-cp.el
;;; Copy, move/rename, making hard and symbolic links

(defvar dired-backup-if-overwrite nil
  "*Non-nil if Dired should ask about making backups before overwriting files.
Special value 'always suppresses confirmation.")

(defun dired-handle-overwrite (to)
  ;; Save old version of a to be overwritten file TO.
  ;; `overwrite-confirmed' and `overwrite-backup-query' are fluid vars
  ;; from dired-create-files.
  (if (and dired-backup-if-overwrite
	   overwrite-confirmed
	   (or (eq 'always dired-backup-if-overwrite)
	       (dired-query 'overwrite-backup-query
			(format "Make backup for existing file `%s'? " to))))
      (let ((backup (car (find-backup-file-name to))))
	(rename-file to backup 0)	; confirm overwrite of old backup
	(dired-relist-entry backup))))

(defun dired-copy-file (from to ok-flag)
  (dired-handle-overwrite to)
  (copy-file from to ok-flag dired-copy-preserve-time))

(defun dired-rename-file (from to ok-flag)
  (dired-handle-overwrite to)
  (rename-file from to ok-flag)		; error is caught in -create-files
  ;; Silently rename the visited file of any buffer visiting this file.
  (and (get-file-buffer from)
       (save-excursion
	 (set-buffer (get-file-buffer from))
	 (let ((modflag (buffer-modified-p)))
	   (set-visited-file-name to)	; kills write-file-hooks
	   (set-buffer-modified-p modflag))))
  (dired-remove-file from)
  ;; See if it's an inserted subdir, and rename that, too.
  (dired-rename-subdir from to))

(defun dired-rename-subdir (from-dir to-dir)
  (setq from-dir (file-name-as-directory from-dir)
	to-dir (file-name-as-directory to-dir))
  (dired-fun-in-all-buffers from-dir
			    (function dired-rename-subdir-1) from-dir to-dir)
  ;; Update visited file name of all affected buffers
  (let ((blist (buffer-list)))
    (while blist
      (save-excursion
        (set-buffer (car blist))
	(if (and buffer-file-name
		 (dired-in-this-tree buffer-file-name from-dir))
	    (let ((modflag (buffer-modified-p))
		  (to-file (dired-replace-in-string
			    (concat "^" (regexp-quote from-dir))
			    to-dir
			    buffer-file-name)))
	      (set-visited-file-name to-file)
	      (set-buffer-modified-p modflag))))
      (setq blist (cdr blist)))))

(defun dired-rename-subdir-1 (dir to)
  ;; Rename DIR to TO in headerlines and dired-subdir-alist, if DIR or
  ;; one of its subdirectories is expanded in this buffer.
  (let ((alist dired-subdir-alist)
	(elt nil))
    (while alist
      (setq elt (car alist)
	    alist (cdr alist))
      (if (dired-in-this-tree (car elt) dir)
	  ;; ELT's subdir is affected by the rename
	  (dired-rename-subdir-2 elt dir to)))
    (if (equal dir default-directory)
	;; if top level directory was renamed, lots of things have to be
	;; updated:
	(progn
	  (dired-unadvertise dir)	; we no longer dired DIR...
	  (setq default-directory to
		dired-directory (expand-file-name;; this is correct
				 ;; with and without wildcards
				 (file-name-nondirectory dired-directory)
				 to))
	  (let ((new-name (file-name-nondirectory
			   (directory-file-name dired-directory))))
	    ;; try to rename buffer, but just leave old name if new
	    ;; name would already exist (don't try appending "<%d>")
	    (or (get-buffer new-name)
		(rename-buffer new-name)))
	  ;; ... we dired TO now:
	  (dired-advertise)))))

(defun dired-rename-subdir-2 (elt dir to)
  ;; Update the headerline and dired-subdir-alist element of directory
  ;; described by alist-element ELT to reflect the moving of DIR to TO.
  ;; Thus, ELT describes either DIR itself or a subdir of DIR.

  ;; Bug: If TO is not longer part of the same dired tree as DIR was,
  ;; updating the headerline is actually not the right thing---it
  ;; should be removed in that case and a completely new entry be
  ;; added for TO.  Actually, removing and adding anew would always be
  ;; the right (but slow) way of doing it.

  ;; The consequences are pretty harmless though (no updates since
  ;; dired-buffers-for-dir will not suspect it to be in this dired
  ;; buffer).

  (save-excursion
    (let ((regexp (regexp-quote (directory-file-name dir)))
	  (newtext (directory-file-name to))
	  buffer-read-only)
      (goto-char (dired-get-subdir-min elt))
      ;; Update subdir headerline in buffer
      (if (not (looking-at dired-subdir-regexp))
	  (error "%s not found where expected - dired-subdir-alist broken?"
		 dir)
	(goto-char (match-beginning 1))
	(if (re-search-forward regexp (match-end 1) t)
	    (replace-match newtext t t)
	  (error "Expected to find `%s' in headerline of %s" dir (car elt))))
      ;; Update buffer-local dired-subdir-alist
      (setcar elt
	      (dired-normalize-subdir
	       (dired-replace-in-string regexp newtext (car elt)))))))

;; Cloning replace-match to work on strings instead of in buffer:
;; The FIXEDCASE parameter of replace-match is not implemented.
(defun dired-string-replace-match (regexp string newtext
					  &optional literal global)
  "Replace first match of REGEXP in STRING with NEWTEXT.
If it does not match, nil is returned instead of the new string.
Optional arg LITERAL means to take NEWTEXT literally.
Optional arg GLOBAL means to replace all matches."
  (if global
        (let ((result "") (start 0) mb me)
	  (while (string-match regexp string start)
	    (setq mb (match-beginning 0)
		  me (match-end 0)
		  result (concat result
				 (substring string start mb)
				 (if literal
				     newtext
				   (dired-expand-newtext string newtext)))
		  start me))
	  (if mb			; matched at least once
	      (concat result (substring string start))
	    nil))
    ;; not GLOBAL
    (if (not (string-match regexp string 0))
	nil
      (concat (substring string 0 (match-beginning 0))
	      (if literal newtext (dired-expand-newtext string newtext))
	      (substring string (match-end 0))))))

(defun dired-expand-newtext (string newtext)
  ;; Expand \& and \1..\9 (referring to STRING) in NEWTEXT, using match data.
  ;; Note that in Emacs 18 match data are clipped to current buffer
  ;; size...so the buffer should better not be smaller than STRING.
  (let ((pos 0)
	(len (length newtext))
	(expanded-newtext ""))
    (while (< pos len)
      (setq expanded-newtext
	    (concat expanded-newtext
		    (let ((c (aref newtext pos)))
		      (if (= ?\\ c)
			  (cond ((= ?\& (setq c
					      (aref newtext
						    (setq pos (1+ pos)))))
				 (substring string
					    (match-beginning 0)
					    (match-end 0)))
				((and (>= c ?1) (<= c ?9))
				 ;; return empty string if N'th
				 ;; sub-regexp did not match:
				 (let ((n (- c ?0)))
				   (if (match-beginning n)
				       (substring string
						  (match-beginning n)
						  (match-end n))
				     "")))
				(t
				 (char-to-string c)))
			(char-to-string c)))))
      (setq pos (1+ pos)))
    expanded-newtext))

;; The basic function for half a dozen variations on cp/mv/ln/ln -s.
(defun dired-create-files (file-creator operation fn-list name-constructor
					&optional marker-char)

;; Create a new file for each from a list of existing files.  The user
;; is queried, dired buffers are updated, and at the end a success or
;; failure message is displayed

;; FILE-CREATOR must accept three args: oldfile newfile ok-if-already-exists

;; It is called for each file and must create newfile, the entry of
;; which will be added.  The user will be queried if the file already
;; exists.  If oldfile is removed by FILE-CREATOR (i.e, it is a
;; rename), it is FILE-CREATOR's responsibility to update dired
;; buffers.  FILE-CREATOR must abort by signalling a file-error if it
;; could not create newfile.  The error is caught and logged.

;; OPERATION (a capitalized string, e.g. `Copy') describes the
;; operation performed.  It is used for error logging.

;; FN-LIST is the list of files to copy (full absolute pathnames).

;; NAME-CONSTRUCTOR returns a newfile for every oldfile, or nil to
;; skip.  If it skips files for other reasons than a direct user
;; query, it is supposed to tell why (using dired-log).

;; Optional MARKER-CHAR is a character with which to mark every
;; newfile's entry, or t to use the current marker character if the
;; oldfile was marked.

  (let (failures skipped (success-count 0) (total (length fn-list)))
    (let (to overwrite-query
	     overwrite-backup-query)	; for dired-handle-overwrite
      (mapcar
       (function
	(lambda (from)
	  (setq to (funcall name-constructor from))
	  (if (equal to from)
	      (progn
		(setq to nil)
		(dired-log "Cannot %s to same file: %s\n"
			   (downcase operation) from)))
	  (if (not to)
	      (setq skipped (cons (dired-make-relative from) skipped))
	    (let* ((overwrite (file-exists-p to))
		   (overwrite-confirmed	; for dired-handle-overwrite
		    (and overwrite
			 (let ((help-form '(format "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." to)))
			   (dired-query 'overwrite-query
					"Overwrite `%s'?" to))))
		   ;; must determine if FROM is marked before file-creator
		   ;; gets a chance to delete it (in case of a move).
		   (actual-marker-char
		    (cond  ((integerp marker-char) marker-char)
			   (marker-char (dired-file-marker from)) ; slow
			   (t nil))))
	      (condition-case err
		  (progn
		    (funcall file-creator from to overwrite-confirmed)
		    (if overwrite
			;; If we get here, file-creator hasn't been aborted
			;; and the old entry (if any) has to be deleted
			;; before adding the new entry.
			(dired-remove-file to))
		    (setq success-count (1+ success-count))
		    (message "%s: %d of %d" operation success-count total)
		    (dired-add-file to actual-marker-char))
		(file-error		; FILE-CREATOR aborted
		 (progn
		   (setq failures (cons (dired-make-relative from) failures))
		   (dired-log "%s `%s' to `%s' failed:\n%s\n"
			      operation from to err))))))))
       fn-list))
    (cond
     (failures
      (dired-log-summary
       (message "%s failed for %d of %d file%s %s"
		operation (length failures) total
		(dired-plural-s total) failures)))
     (skipped
      (dired-log-summary
       (message "%s: %d of %d file%s skipped %s"
		operation (length skipped) total
		(dired-plural-s total) skipped)))
     (t
      (message "%s: %s file%s."
	       operation success-count (dired-plural-s success-count)))))
  (dired-move-to-filename))

(defun dired-do-create-files (op-symbol file-creator operation arg
					     &optional marker-char op1
					     how-to)
  ;; Create a new file for each marked file.
  ;; Prompts user for target, which is a directory in which to create
  ;;   the new files.  Target may be a plain file if only one marked
  ;;   file exists.
  ;; OP-SYMBOL is the symbol for the operation.  Function `dired-mark-pop-up'
  ;;   will determine wether pop-ups are appropriate for this OP-SYMBOL.
  ;; FILE-CREATOR and OPERATION as in dired-create-files.
  ;; ARG as in dired-mark-get-files.
  ;; Optional arg OP1 is an alternate form for OPERATION if there is
  ;;   only one file.
  ;; Optional arg MARKER-CHAR as in dired-create-files.
  ;; Optional arg HOW-TO determines how to treat target:
  ;;   If HOW-TO is not given (or nil), and target is a directory, the
  ;;     file(s) are created inside the target directory.  If target
  ;;     is not a directory, there must be exactly one marked file,
  ;;     else error.
  ;;   If HOW-TO is t, then target is not modified.  There must be
  ;;     exactly one marked file, else error.
  ;; Else HOW-TO is assumed to be a function of one argument, target,
  ;;     that looks at target and returns a value for the into-dir
  ;;     variable.  The function dired-into-dir-with-symlinks is provided
  ;;     for the case (common when creating symlinks) that symbolic
  ;;     links to directories are not to be considered as directories
  ;;     (as file-directory-p would if HOW-TO had been nil).
  (or op1 (setq op1 operation))
  (let* ((fn-list (dired-mark-get-files nil arg))
	 (fn-count (length fn-list))
	 (target (expand-file-name
		   (dired-mark-read-file-name
		    (concat (if (= 1 fn-count) op1 operation) " %s to: ")
		    (dired-dwim-target-directory)
		    op-symbol arg (mapcar (function dired-make-relative) fn-list))))
	 (into-dir (cond ((null how-to) (file-directory-p target))
			 ((eq how-to t) nil)
			 (t (funcall how-to target)))))
    (if (and (> fn-count 1)
	     (not into-dir))
	(error "Marked %s: target must be a directory: %s" operation target))
    ;; rename-file bombs when moving directories unless we do this:
    (or into-dir (setq target (directory-file-name target)))
    (dired-create-files
     file-creator operation fn-list
     (if into-dir			; target is a directory
	 ;; This function uses fluid vars into-dir and target when called
	 ;; inside dired-create-files:
	 (function (lambda (from)
		     (expand-file-name (file-name-nondirectory from) target)))
       (function (lambda (from) target)))
     marker-char)))

(defun dired-dwim-target-directory ()
  ;; Try to guess which target directory the user may want.
  ;; If there is a dired buffer displayed in the next window, use
  ;; its current subdir, else use current subdir of this dired buffer.
  (let ((this-dir (and (eq major-mode 'dired-mode)
		       (dired-current-directory))))
    ;; non-dired buffer may want to profit from this function, e.g. vm-uudecode
    (if dired-dwim-target
	(let* ((other-buf (window-buffer (next-window)))
	       (other-dir (save-excursion
			    (set-buffer other-buf)
			    (and (eq major-mode 'dired-mode)
				 (dired-current-directory)))))
	  (or other-dir this-dir))
      this-dir)))

(defun dired-into-dir-with-symlinks (target)
  (and (file-directory-p target)
       (not (file-symlink-p target))))
;; This may not always be what you want, especially if target is your
;; home directory and it happens to be a symbolic link, as is often the
;; case with NFS and automounters.  Or if you want to make symlinks
;; into directories that themselves are only symlinks, also quite
;; common.

;; So we don't use this function as value for HOW-TO in
;; dired-do-symlink, which has the minor disadvantage of
;; making links *into* a symlinked-dir, when you really wanted to
;; *overwrite* that symlink.  In that (rare, I guess) case, you'll
;; just have to remove that symlink by hand before making your marked
;; symlinks.

(defun dired-do-copy (&optional arg)
  "Copy all marked (or next ARG) files, or copy the current file.
Thus, a zero prefix argument copies nothing.  But it toggles the
variable `dired-copy-preserve-time' (which see)."
  (interactive "P")
  (if (not (zerop (prefix-numeric-value arg)))
      (dired-do-create-files 'copy (function dired-copy-file)
			       (if dired-copy-preserve-time "Copy [-p]" "Copy")
			       arg dired-keep-marker-copy)
    (setq dired-copy-preserve-time (not dired-copy-preserve-time))
    (if dired-copy-preserve-time
	(message "Copy will preserve time.")
      (message "Copied files will get current date."))))

(defun dired-do-symlink (&optional arg)
   "Symlink all marked (or next ARG) files into a directory,
or make a symbolic link to the current file."
  (interactive "P")
  (dired-do-create-files 'symlink (function make-symbolic-link)
			   "SymLink" arg dired-keep-marker-symlink))

(defun dired-do-hardlink (&optional arg)
   "Hard-link all marked (or next ARG) files into a directory,
or make a hard link to the current file."
  (interactive "P")
  (dired-do-create-files 'hardlink (function add-name-to-file)
			   "HardLink" arg dired-keep-marker-hardlink))

(defun dired-do-move (&optional arg)
  "Move all marked (or next ARG) files into a directory,
or rename the current file.
A zero ARG moves no files but toggles `dired-dwim-target' (which see)."
  (interactive "P")
  (if (not (zerop (prefix-numeric-value arg)))
      (dired-do-create-files 'move (function dired-rename-file)
			       "Move" arg dired-keep-marker-move "Rename")
    (setq dired-dwim-target (not dired-dwim-target))
    (message "dired-dwim-target is %s." (if dired-dwim-target "ON" "OFF"))))

;;;###end dired-cp.el

;;; 5K
;;;###begin dired-re.el
(defun dired-do-create-files-regexp
  (file-creator operation arg regexp newname &optional whole-path marker-char)
  ;; Create a new file for each marked file using regexps.
  ;; FILE-CREATOR and OPERATION as in dired-create-files.
  ;; ARG as in dired-mark-get-files.
  ;; Matches each marked file against REGEXP and constructs the new
  ;;   filename from NEWNAME (like in function replace-match).
  ;; Optional arg WHOLE-PATH means match/replace the whole pathname
  ;;   instead of only the non-directory part of the file.
  ;; Optional arg MARKER-CHAR as in dired-create-files.
  (let* ((fn-list (dired-mark-get-files nil arg))
	 (fn-count (length fn-list))
	 (operation-prompt (concat operation " `%s' to `%s'?"))
	 (rename-regexp-help-form (format "\
Type SPC or `y' to %s one match, DEL or `n' to skip to next,
`!' to %s all remaining matches with no more questions."
					  (downcase operation)
					  (downcase operation)))
	 (regexp-name-constructor
	  ;; Function to construct new filename using REGEXP and NEWNAME:
	  (if whole-path		; easy (but rare) case
	      (function
	       (lambda (from)
		 (let ((to (dired-string-replace-match regexp from newname))
		       ;; must bind help-form directly around call to
		       ;; dired-query
		       (help-form rename-regexp-help-form))
		   (if to
		       (and (dired-query 'rename-regexp-query
					 operation-prompt
					 from
					 to)
			    to)
		     (dired-log "%s: %s did not match regexp %s\n"
				operation from regexp)))))
	    ;; not whole-path, replace non-directory part only
	    (function
	     (lambda (from)
	       (let* ((new (dired-string-replace-match
			    regexp (file-name-nondirectory from) newname))
		      (to (and new	; nil means there was no match
			       (expand-file-name new
						 (file-name-directory from))))
		      (help-form rename-regexp-help-form))
		 (if to
		     (and (dired-query 'rename-regexp-query
				       operation-prompt
				       (dired-make-relative from)
				       (dired-make-relative to))
			  to)
		   (dired-log "%s: %s did not match regexp %s\n"
			      operation (file-name-nondirectory from) regexp)))))))
	 rename-regexp-query)
    (dired-create-files
     file-creator operation fn-list regexp-name-constructor marker-char)))

(defun dired-mark-read-regexp (operation)
  ;; Prompt user about performing OPERATION.
  ;; Read and return list of: regexp newname arg whole-path.
  (let* ((whole-path
	  (equal 0 (prefix-numeric-value current-prefix-arg)))
	 (arg
	  (if whole-path nil current-prefix-arg))
	 (regexp
	  (dired-read-regexp
	   (concat (if whole-path "Path " "") operation " from (regexp): ")
	   dired-flagging-regexp))
	 (newname
	  (read-string
	   (concat (if whole-path "Path " "") operation " " regexp " to: "))))
    (list regexp newname arg whole-path)))

(defun dired-do-rename-regexp (regexp newname &optional arg whole-path)
  "Rename marked files containing REGEXP to NEWNAME.
As each match is found, the user must type a character saying
  what to do with it.  For directions, type \\[help-command] at that time.
NEWNAME may contain \\=\\<n> or \\& as in `query-replace-regexp'.
REGEXP defaults to the last regexp used.
With a zero prefix arg, renaming by regexp affects the complete
  pathname - usually only the non-directory part of file names is used
  and changed."
  (interactive (dired-mark-read-regexp "Rename"))
  (dired-do-create-files-regexp
   (function dired-rename-file)
   "Rename" arg regexp newname whole-path dired-keep-marker-move))

(defun dired-do-copy-regexp (regexp newname &optional arg whole-path)
  "Copy all marked files containing REGEXP to NEWNAME.
See function `dired-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "Copy"))
  (dired-do-create-files-regexp
   (function dired-copy-file)
   (if dired-copy-preserve-time "Copy [-p]" "Copy")
   arg regexp newname whole-path dired-keep-marker-copy))

(defun dired-do-hardlink-regexp (regexp newname &optional arg whole-path)
  "Hardlink all marked files containing REGEXP to NEWNAME.
See function `dired-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "HardLink"))
  (dired-do-create-files-regexp
   (function add-name-to-file)
   "HardLink" arg regexp newname whole-path dired-keep-marker-hardlink))

(defun dired-do-symlink-regexp (regexp newname &optional arg whole-path)
  "Symlink all marked files containing REGEXP to NEWNAME.
See function `dired-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "SymLink"))
  (dired-do-create-files-regexp
   (function make-symbolic-link)
   "SymLink" arg regexp newname whole-path dired-keep-marker-symlink))

(defun dired-create-files-non-directory
  (file-creator basename-constructor operation arg)
  ;; Perform FILE-CREATOR on the non-directory part of marked files
  ;; using function BASENAME-CONSTRUCTOR, with query for each file.
  ;; OPERATION like in dired-create-files, ARG like in dired-mark-get-files.
  (let (rename-non-directory-query)
    (dired-create-files
     file-creator
     operation
     (dired-mark-get-files nil arg)
     (function
      (lambda (from)
	(let ((to (concat (file-name-directory from)
			  (funcall basename-constructor
				   (file-name-nondirectory from)))))
	  (and (let ((help-form (format "\
Type SPC or `y' to %s one file, DEL or `n' to skip to next,
`!' to %s all remaining matches with no more questions."
					(downcase operation)
					(downcase operation))))
		 (dired-query 'rename-non-directory-query
			      (concat operation " `%s' to `%s'")
			      (dired-make-relative from)
			      (dired-make-relative to)))
	       to))))
     dired-keep-marker-move)))

(defun dired-rename-non-directory (basename-constructor operation arg)
  (dired-create-files-non-directory
   (function dired-rename-file)
   basename-constructor operation arg))

(defun dired-upcase (&optional arg)
  "Rename all marked (or next ARG) files to upper case."
  (interactive "P")
  (dired-rename-non-directory (function upcase) "Rename upcase" arg))

(defun dired-downcase (&optional arg)
  "Rename all marked (or next ARG) files to lower case."
  (interactive "P")
  (dired-rename-non-directory (function downcase) "Rename downcase" arg))

;;;###end dired-re.el


;; Tree Dired

;;; utility functions

(defun dired-in-this-tree (file dir)
  ;;"Is FILE part of the directory tree starting at DIR?"
  (let (case-fold-search)
    (string-match (concat "^" (regexp-quote dir)) file)))

(defun dired-make-absolute (file &optional dir)
  ;;"Convert FILE (a pathname relative to DIR) to an absolute pathname."
  ;; We can't always use expand-file-name as this would get rid of `.'
  ;; or expand in / instead default-directory if DIR=="".
  ;; This should be good enough for ange-ftp, but might easily be
  ;; redefined (for VMS?).
  ;; It should be reasonably fast, though, as it is called in
  ;; dired-get-filename.
  (concat (or dir default-directory) file))

(defun dired-make-relative (file &optional dir no-error)
  ;;"Convert FILE (an absolute pathname) to a pathname relative to DIR.
  ;; Else error (unless NO-ERROR is non-nil, then FILE is returned unchanged)
  ;;DIR defaults to default-directory."
  ;; DIR must be file-name-as-directory, as with all directory args in
  ;; elisp code.
  (or dir (setq dir default-directory))
  (if (string-match (concat "^" (regexp-quote dir)) file)
      (substring file (match-end 0))
    (if no-error
	file
      (error  "%s: not in directory tree growing at %s" file dir))))

(defun dired-normalize-subdir (dir)
  ;; Prepend default-directory to DIR if relative path name.
  ;; dired-get-filename must be able to make a valid filename from a
  ;; file and its directory DIR.
  (file-name-as-directory
   (if (file-name-absolute-p dir)
       dir
     (expand-file-name dir default-directory))))

(defun dired-between-files ()
  ;; Point must be at beginning of line
  ;; Should be equivalent to (save-excursion (not (dired-move-to-filename)))
  ;; but is about 1.5..2.0 times as fast. (Actually that's not worth it)
  (or (looking-at "^$\\|^. *$\\|^. total\\|^. wildcard")
      (looking-at dired-subdir-regexp)))

(defun dired-get-subdir ()
  ;;"Return the subdir name on this line, or nil if not on a headerline."
  ;; Look up in the alist whether this is a headerline.
  (save-excursion
    (let ((cur-dir (dired-current-directory)))
      (beginning-of-line)		; alist stores b-o-l positions
      (and (zerop (- (point)
		     (dired-get-subdir-min (assoc cur-dir
						  dired-subdir-alist))))
	   cur-dir))))

;(defun dired-get-subdir-min (elt)
;  (cdr elt))
;; can't use macro,  must be redefinable for other alist format in dired-nstd.
(fset 'dired-get-subdir-min 'cdr)

(defun dired-get-subdir-max (elt)
  (save-excursion
    (goto-char (dired-get-subdir-min elt))
    (dired-subdir-max)))

(defun dired-clear-alist ()
  (while dired-subdir-alist
    (set-marker (dired-get-subdir-min (car dired-subdir-alist)) nil)
    (setq dired-subdir-alist (cdr dired-subdir-alist))))

(defun dired-simple-subdir-alist ()
  ;; Build and return `dired-subdir-alist' assuming just the top level
  ;; directory to be inserted.  Don't parse the buffer.
  (set (make-local-variable 'dired-subdir-alist)
       (list (cons default-directory (point-min-marker)))))

(defun dired-build-subdir-alist ()
  "Build `dired-subdir-alist' by parsing the buffer and return it's new value."
  (interactive)
  (dired-clear-alist)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (setq dired-subdir-alist nil)
      (while (re-search-forward dired-subdir-regexp nil t)
	(setq count (1+ count))
	(dired-alist-add-1 (buffer-substring (match-beginning 1)
					     (match-end 1))
			 ;; Put subdir boundary between lines:
			 (save-excursion
			   (goto-char (match-beginning 0))
			   (beginning-of-line)
			   (point-marker)))
	(message "%d" count))
      (message "%d director%s." count (if (= 1 count) "y" "ies"))
      ;; We don't need to sort it because it is in buffer order per
      ;; constructionem.  Return new alist:
      dired-subdir-alist)))

(defun dired-alist-add (dir new-marker)
  ;; Add new DIR at NEW-MARKER.  Sort alist.
  (dired-alist-add-1 dir new-marker)
  (dired-alist-sort))

(defun dired-alist-add-1 (dir new-marker)
  ;; Add new DIR at NEW-MARKER.  Don't sort.
  (setq dired-subdir-alist
	(cons (cons (dired-normalize-subdir dir) new-marker)
	      dired-subdir-alist)))

(defun dired-alist-sort ()
  ;; Keep the alist sorted on buffer position.
  (setq dired-subdir-alist
	(sort dired-subdir-alist
	      (function (lambda (elt1 elt2)
			  (> (dired-get-subdir-min elt1)
			     (dired-get-subdir-min elt2)))))))

(defun dired-unsubdir (dir)
  ;; Remove DIR from the alist
  (setq dired-subdir-alist
	(delq (assoc dir dired-subdir-alist) dired-subdir-alist)))

(defun dired-goto-next-nontrivial-file ()
  ;; Position point on first nontrivial file after point.
  (dired-goto-next-file);; so there is a file to compare with
  (if (stringp dired-trivial-filenames)
      (while (and (not (eobp))
		  (string-match dired-trivial-filenames
				(file-name-nondirectory
				 (or (dired-get-filename nil t) ""))))
	(forward-line 1)
	(dired-move-to-filename))))

(defun dired-goto-next-file ()
  (let ((max (1- (dired-subdir-max))))
    (while (and (not (dired-move-to-filename)) (< (point) max))
      (forward-line 1))))

(defun dired-goto-subdir (dir)
  "Goto end of header line of DIR in this dired buffer.
Return value of point on success, otherwise return nil.
The next char is either \\n, or \\r if DIR is hidden."
  (interactive
   (prog1				; let push-mark display its message
       (list (expand-file-name
	      (completing-read "Goto in situ directory: " ; prompt
			       dired-subdir-alist ; table
			       nil	; predicate
			       t	; require-match
			       (dired-current-directory))))
     (push-mark)))
  (setq dir (file-name-as-directory dir))
  (let ((elt (assoc dir dired-subdir-alist)))
    (and elt
	 (goto-char (dired-get-subdir-min elt))
	 ;; dired-subdir-hidden-p and dired-add-entry depend on point being
	 ;; at either \r or \n after this function succeeds.
	 (progn (skip-chars-forward "^\r\n")
		(point)))))

(defun dired-goto-file (file)
  "Goto file line of FILE in this dired buffer."
  ;; Return value of point on success, else nil.
  ;; FILE must be an absolute pathname.
  ;; Loses if FILE contains control chars like "\007" for which ls
  ;; either inserts "?" or "\\007" into the buffer, so we won't find
  ;; it in the buffer.
  (interactive
   (prog1				; let push-mark display its message
       (list (expand-file-name
	      (read-file-name "Goto file: "
			      (dired-current-directory))))
     (push-mark)))
  (setq file (directory-file-name file)) ; does no harm if no directory
  (let (found case-fold-search)
    (save-excursion
      (if (dired-goto-subdir (or (file-name-directory file)
				 (error "Need absolute pathname for %s" file)))
	  (let ((base (file-name-nondirectory file))
		(boundary (dired-subdir-max)))
	    (while (and (not found)
			;; filenames are preceded by SPC, this makes
			;; the search faster (e.g. for the filename "-"!).
			(search-forward (concat " " base) boundary 'move))
	      ;; Match could have BASE just as initial substring or
	      ;; or in permission bits or date or
	      ;; not be a proper filename at all:
	      (if (equal base (dired-get-filename 'no-dir t))
		    ;; Must move to filename since an (actually
		    ;; correct) match could have been elsewhere on the
		    ;; ;; line (e.g. "-" would match somewhere in the
		    ;; permission bits).
		  (setq found (dired-move-to-filename)))))))
    (and found
	 ;; return value of point (i.e., FOUND):
	 (goto-char found))))

(defun dired-initial-position (dirname)
  ;; Where point should go in a new listing of DIRNAME.
  ;; Point assumed at beginning of new subdir line.
  ;; You may redefine this function as you wish, e.g. like in dired-x.el.
  (end-of-line)
  (if dired-trivial-filenames (dired-goto-next-nontrivial-file)))

;;; moving by subdirectories

(defun dired-subdir-index (dir)
  ;; Return an index into alist for use with nth
  ;; for the sake of subdir moving commands.
  (let (found (index 0) (alist dired-subdir-alist))
    (while alist
      (if (string= dir (car (car alist)))
	  (setq alist nil found t)
	(setq alist (cdr alist) index (1+ index))))
    (if found index nil)))

(defun dired-next-subdir (arg &optional no-error-if-not-found no-skip)
  "Go to next subdirectory, regardless of level."
  ;; Use 0 arg to go to this directory's header line.
  ;; NO-SKIP prevents moving to end of header line, returning whatever
  ;; position was found in dired-subdir-alist.
  (interactive "p")
  (let ((this-dir (dired-current-directory))
	pos index)
    ;; nth with negative arg does not return nil but the first element
    (setq index (- (dired-subdir-index this-dir) arg))
    (setq pos (if (>= index 0)
		  (dired-get-subdir-min (nth index dired-subdir-alist))))
    (if pos
	(progn
	  (goto-char pos)
	  (or no-skip (skip-chars-forward "^\n\r"))
	  (point))
      (if no-error-if-not-found
	  nil				; return nil if not found
	(error "%s directory" (if (> arg 0) "Last" "First"))))))

(defun dired-prev-subdir (arg &optional no-error-if-not-found no-skip)
  "Go to previous subdirectory, regardless of level.
When called interactively and not on a subdir line, go to this subdir's line."
  ;;(interactive "p")
  (interactive
   (list (if current-prefix-arg
	     (prefix-numeric-value current-prefix-arg)
	   ;; if on subdir start already, don't stay there!
	   (if (dired-get-subdir) 1 0))))
  (dired-next-subdir (- arg) no-error-if-not-found no-skip))

(defun dired-tree-up (arg)
  "Go up ARG levels in the dired tree."
  (interactive "p")
  (let ((dir (dired-current-directory)))
    (while (>= arg 1)
      (setq arg (1- arg)
	    dir (file-name-directory (directory-file-name dir))))
    ;;(setq dir (expand-file-name dir))
    (or (dired-goto-subdir dir)
	(error "Cannot go up to %s - not in this tree." dir))))

(defun dired-tree-down ()
  "Go down in the dired tree."
  (interactive)
  (let ((dir (dired-current-directory)) ; has slash
	pos case-fold-search)		; filenames are case sensitive
    (let ((rest (reverse dired-subdir-alist)) elt)
      (while rest
	(setq elt (car rest)
	      rest (cdr rest))
	(if (dired-in-this-tree (directory-file-name (car elt)) dir)
	    (setq rest nil
		  pos (dired-goto-subdir (car elt))))))
    (if pos
	(goto-char pos)
      (error "At the bottom"))))

;;; hiding

(defun dired-subdir-hidden-p (dir)
  (and selective-display
       (save-excursion
	 (dired-goto-subdir dir)
	 (looking-at "\r"))))

(defun dired-unhide-subdir ()
  (let (buffer-read-only)
    (subst-char-in-region (dired-subdir-min) (dired-subdir-max) ?\r ?\n)))

(defun dired-hide-check ()
  (or selective-display
      (error "selective-display must be t for subdir hiding to work!")))

(defun dired-hide-subdir (arg)
  "Hide or unhide the current subdirectory and move to next directory.
Optional prefix arg is a repeat factor.
Use \\[dired-hide-all] to (un)hide all directories."
  (interactive "p")
  (dired-hide-check)
  (while (>=  (setq arg (1- arg)) 0)
    (let* ((cur-dir (dired-current-directory))
	   (hidden-p (dired-subdir-hidden-p cur-dir))
	   (elt (assoc cur-dir dired-subdir-alist))
	   (end-pos (1- (dired-get-subdir-max elt)))
	   buffer-read-only)
      ;; keep header line visible, hide rest
      (goto-char (dired-get-subdir-min elt))
      (skip-chars-forward "^\n\r")
      (if hidden-p
	  (subst-char-in-region (point) end-pos ?\r ?\n)
	(subst-char-in-region (point) end-pos ?\n ?\r)))
    (dired-next-subdir 1 t)))

(defun dired-hide-all (arg)
  "Hide all subdirectories, leaving only their header lines.
If there is already something hidden, make everything visible again.
Use \\[dired-hide-subdir] to (un)hide a particular subdirectory."
  (interactive "P")
  (dired-hide-check)
  (let (buffer-read-only)
    (if (save-excursion
	  (goto-char (point-min))
	  (search-forward "\r" nil t))
	;; unhide - bombs on \r in filenames
	(subst-char-in-region (point-min) (point-max) ?\r ?\n)
      ;; hide
      (let ((pos (point-max))		; pos of end of last directory
	    (alist dired-subdir-alist))
	(while alist			; while there are dirs before pos
	  (subst-char-in-region (dired-get-subdir-min (car alist)) ; pos of prev dir
				(save-excursion
				  (goto-char pos) ; current dir
				  ;; we're somewhere on current dir's line
				  (forward-line -1)
				  (point))
				?\n ?\r)
	  (setq pos (dired-get-subdir-min (car alist)))	; prev dir gets current dir
	  (setq alist (cdr alist)))))))


;; This function is the heart of tree dired.
;; It is called for each retrieved filename.
;; It could stand to be faster, though it's mostly function call
;; overhead.  Avoiding to funcall seems to save about 10% in
;; dired-get-filename.  Make it a defsubst?
(defun dired-current-directory (&optional localp)
  "Return the name of the subdirectory to which this line belongs.
This returns a string with trailing slash, like `default-directory'.
Optional argument means return a file name relative to `default-directory'."
  (let ((here (point))
	(alist (or dired-subdir-alist
		   ;; probably because called in a non-dired buffer
		   (error "No subdir-alist in %s" (current-buffer))))
	elt dir)
    (while alist
      (setq elt (car alist)
	    dir (car elt)
	    ;; use `<=' (not `<') as subdir line is part of subdir
	    alist (if (<= (dired-get-subdir-min elt) here)
		      nil		; found
		    (cdr alist))))
    (if localp
	(dired-make-relative dir default-directory)
      dir)))

;; Subdirs start at the beginning of their header lines and end just
;; before the beginning of the next header line (or end of buffer).

(defun dired-subdir-min ()
  (save-excursion
    (if (not (dired-prev-subdir 0 t t))
	(error "Not in a subdir!")
      (point))))

(defun dired-subdir-max ()
  (save-excursion
    (if (not (dired-next-subdir 1 t t))
	(point-max)
      (point))))

(defun dired-kill-line-or-subdir (&optional arg)
  "Kill this line (but not this file).
Optional prefix argument is a repeat factor.
If file is displayed as in situ subdir, kill that as well.
If on a subdir headerline, kill whole subdir."
  (interactive "p")
  (if (dired-get-subdir)
      (dired-kill-subdir)
    (dired-kill-line arg)))

(defun dired-kill-line (&optional arg)
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (let (buffer-read-only file)
    (while (/= 0 arg)
      (setq file (dired-get-filename nil t))
      (if (not file)
	  (error "Can only kill file lines.")
	(save-excursion (and file
			     (dired-goto-subdir file)
			     (dired-kill-subdir)))
	(delete-region (progn (beginning-of-line) (point))
		       (progn (forward-line 1) (point)))
	(if (> arg 0)
	    (setq arg (1- arg))
	  (setq arg (1+ arg))
	  (forward-line -1))))
    (dired-move-to-filename)))

(defun dired-kill-subdir (&optional remember-marks)
  "Remove all lines of current subdirectory.
Lower levels are unaffected."
  ;; With optional REMEMBER-MARKS, return a mark-alist.
  (interactive)
  (let ((beg (dired-subdir-min))
	(end (dired-subdir-max))
	buffer-read-only cur-dir)
    (setq cur-dir (dired-current-directory))
    (if (equal cur-dir default-directory)
	(error "Attempt to kill top level directory"))
    (prog1
	(if remember-marks (dired-remember-marks beg end))
      (delete-region beg end)
      (if (eobp)			; don't leave final blank line
	  (delete-char -1))
      (dired-unsubdir cur-dir))))

(defun dired-do-kill (&optional arg fmt)
  "Kill all marked lines (not files).
With a prefix arg, kill all lines not marked or flagged."
  ;; Returns count of killed lines.  FMT="" suppresses message.
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only (count 0))
      (if (not arg)			; kill marked lines
	  (let ((regexp (dired-marker-regexp)))
	    (while (and (not (eobp))
			(re-search-forward regexp nil t))
	      (setq count (1+ count))
	      (delete-region (progn (beginning-of-line) (point))
			     (progn (forward-line 1) (point)))))
	;; else kill unmarked lines
	(while (not (eobp))
	  (if (or (dired-between-files)
		  (not (looking-at "^  ")))
	      (forward-line 1)
	    (setq count (1+ count))
	    (delete-region (point) (save-excursion
				     (forward-line 1)
				     (point))))))
      (or (equal "" fmt)
	  (message (or fmt "Killed %d line%s.") count (dired-plural-s count)))
      count)))

(defun dired-do-redisplay (&optional arg test-for-subdir)
  "Redisplay all marked (or next ARG) files.

If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the ls switches used for the new listing."
  ;; Moves point if the next ARG files are redisplayed.
  (interactive "P\np")
  (if (and test-for-subdir (dired-get-subdir))
      (dired-insert-subdir
       (dired-get-subdir)
       (if arg (read-string "Switches for listing: " dired-actual-switches)))
    (message "Redisplaying...")
    ;; message instead of making dired-mark-map show-progress is much faster
    (dired-mark-map (let ((fname (dired-get-filename)))
		      (message "Redisplaying... %s" fname)
		      (dired-update-file-line fname))
		    arg)
    (dired-move-to-filename)
    (message "Redisplaying...done")))

(defun dired-mark-files-in-region (start end)
  (let (buffer-read-only)
    (if (> start end)
	(error "start > end"))
    (goto-char start)			; assumed at beginning of line
    (while (< (point) end)
      ;; Skip subdir line and following garbage like the `total' line:
      (while (and (< (point) end) (dired-between-files))
	(forward-line 1))
      (if (and (not (looking-at dired-re-dot))
	       (dired-get-filename nil t))
	  (progn
	    (delete-char 1)
	    (insert dired-marker-char)))
      (forward-line 1))))

(defun dired-mark-subdir-files ()
  "Mark all files except `.' and `..'."
  (interactive "P")
  (let ((p-min (dired-subdir-min)))
    (dired-mark-files-in-region p-min (dired-subdir-max))))

(defun dired-mark-subdir-or-file (arg)
  "Mark the current (or next ARG) files.
If on a subdir headerline, mark all its files except `.' and `..'.

Use \\[dired-unflag-all-files] to remove all marks
and \\[dired-unmark-subdir-or-file] on a subdir to remove the marks in
this subdir."
  (interactive "P")
  (if (dired-get-subdir)
      (save-excursion (dired-mark-subdir-files))
    (dired-mark-file (prefix-numeric-value arg))))

(defun dired-unmark-subdir-or-file (arg)
  "Unmark the current (or next ARG) files.
If looking at a subdir, unmark all its files except `.' and `..'."
  (interactive "P")
  (let ((dired-marker-char ?\040))
    (dired-mark-subdir-or-file arg)))

;;; 5K
;;;###begin dired-ins.el

(defun dired-maybe-insert-subdir (dirname &optional
					  switches no-error-if-not-dir-p)
  "Insert this subdirectory into the same dired buffer.
If it is already present, just move to it (type \\[dired-do-redisplay] to refresh),
  else inserts it at its natural place (as ls -lR would have done).
With a prefix arg, you may edit the ls switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to ls -lR output."
  (interactive
   (list (dired-get-filename)
	 (if current-prefix-arg
	     (read-string "Switches for listing: " dired-actual-switches))))
  (let ((opoint (point)))
    ;; We don't need a marker for opoint as the subdir is always
    ;; inserted *after* opoint.
    (setq dirname (file-name-as-directory dirname))
    (or (and (not switches)
	     (dired-goto-subdir dirname))
	(dired-insert-subdir dirname switches no-error-if-not-dir-p))
    ;; Push mark so that it's easy to find back.  Do this after the
    ;; insert message so that the user sees the `Mark set' message.
    (push-mark opoint)))

(defun dired-insert-subdir (dirname &optional switches no-error-if-not-dir-p)
  "Insert this subdirectory into the same dired buffer.
If it is already present, overwrites previous entry,
  else inserts it at its natural place (as ls -lR would have done).
With a prefix arg, you may edit the ls switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to ls -lR output."
  ;; NO-ERROR-IF-NOT-DIR-P needed for special filesystems like
  ;; Prospero where dired-ls does the right thing, but
  ;; file-directory-p has not been redefined.
  (interactive
   (list (dired-get-filename)
	 (if current-prefix-arg
	     (read-string "Switches for listing: " dired-actual-switches))))
  (setq dirname (file-name-as-directory (expand-file-name dirname)))
  (dired-insert-subdir-validate dirname switches)
  (or no-error-if-not-dir-p
      (file-directory-p dirname)
      (error  "Attempt to insert a non-directory: %s" dirname))
  (let ((elt (assoc dirname dired-subdir-alist))
	 switches-have-R mark-alist case-fold-search buffer-read-only)
    ;; case-fold-search is nil now, so we can test for capital `R':
    (if (setq switches-have-R (and switches (string-match "R" switches)))
	;; avoid duplicated subdirs
	(setq mark-alist (dired-kill-tree dirname t)))
    (if elt
	;; If subdir is already present, remove it and remember its marks
	(setq mark-alist (nconc (dired-insert-subdir-del elt) mark-alist))
      (dired-insert-subdir-newpos dirname)) ; else compute new position
    (dired-insert-subdir-doupdate
     dirname elt (dired-insert-subdir-doinsert dirname switches))
    (if switches-have-R (dired-build-subdir-alist))
    (dired-initial-position dirname)
    (save-excursion (dired-mark-remembered mark-alist))))

;; This is a separate function for dired-vms.
(defun dired-insert-subdir-validate (dirname &optional switches)
  ;; Check that it is valid to insert DIRNAME with SWITCHES.
  ;; Signal an error if invalid (e.g. user typed `i' on `..').
  (or (dired-in-this-tree dirname default-directory)
      (error  "%s: not in this directory tree" dirname))
  (if switches
      (let (case-fold-search)
	(mapcar
	 (function
	  (lambda (x)
	    (or (eq (null (string-match x switches))
		    (null (string-match x dired-actual-switches)))
		(error "Can't have dirs with and without -%s switches together"
		       x))))
	 ;; all switches that make a difference to dired-get-filename:
	 '("F" "b")))))

(defun dired-kill-tree (dirname &optional remember-marks)
  ;;"Kill all proper subdirs of DIRNAME, excluding DIRNAME itself.
  ;; With optional arg REMEMBER-MARKS, return an alist of marked files."
  (interactive "DKill tree below directory: ")
  (let ((s-alist dired-subdir-alist) dir m-alist)
    (while s-alist
      (setq dir (car (car s-alist))
	    s-alist (cdr s-alist))
      (if (and (not (string-equal dir dirname))
	       (dired-in-this-tree dir dirname)
	       (dired-goto-subdir dir))
	  (setq m-alist (nconc (dired-kill-subdir remember-marks) m-alist))))
    m-alist))

(defun dired-insert-subdir-newpos (new-dir)
  ;; Find pos for new subdir, according to tree order.
  (let ((alist dired-subdir-alist) elt dir pos new-pos)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist)
	    dir (car elt)
	    pos (dired-get-subdir-min elt))
      (if (dired-tree-lessp dir new-dir)
	  ;; Insert NEW-DIR after DIR
	  (setq new-pos (dired-get-subdir-max elt)
		alist nil)))
    (goto-char new-pos))
  ;; want a separating newline between subdirs
  (or (eobp)
      (forward-line -1))
  (insert "\n")
  (point))

(defun dired-insert-subdir-del (element)
  ;; Erase an already present subdir (given by ELEMENT) from buffer.
  ;; Move to that buffer position.  Return a mark-alist.
  (let ((begin-marker (dired-get-subdir-min element)))
    (goto-char begin-marker)
    ;; Are at beginning of subdir (and inside it!).  Now determine its end:
    (goto-char (dired-subdir-max))
    (or (eobp);; want a separating newline _between_ subdirs:
	(forward-char -1))
    (prog1
	(dired-remember-marks begin-marker (point))
      (delete-region begin-marker (point)))))

(defun dired-insert-subdir-doinsert (dirname switches)
  ;; Insert ls output after point and put point on the correct
  ;; position for the subdir alist.
  ;; Return the boundary of the inserted text (as list of BEG and END).
  (let ((begin (point)) end)
    (message "Reading directory %s..." dirname)
    (let ((dired-actual-switches
	   (or switches
	       (dired-replace-in-string "R" "" dired-actual-switches))))
      (if (equal dirname (car (car (reverse dired-subdir-alist))))
	  ;; top level directory may contain wildcards:
	  (dired-readin-insert dired-directory)
	(dired-ls dirname dired-actual-switches nil t)))
    (message "Reading directory %s...done" dirname)
    (setq end (point-marker))
    (indent-rigidly begin end 2)
    ;;  call dired-insert-headerline afterwards, as under VMS dired-ls
    ;;  does insert the headerline itself and the insert function just
    ;;  moves point.
    ;;  Need a marker for END as this inserts text.
    (goto-char begin)
    (dired-insert-headerline dirname)
    ;; point is now like in dired-build-subdir-alist
    (prog1
	(list begin (marker-position end))
      (set-marker end nil))))

(defun dired-insert-subdir-doupdate (dirname elt beg-end)
  ;; Point is at the correct subdir alist position for ELT,
  ;; BEG-END is the subdir-region (as list of begin and end).
  (if elt				; subdir was already present
      ;; update its position (should actually be unchanged)
      (set-marker (dired-get-subdir-min elt) (point-marker))
    (dired-alist-add dirname (point-marker)))
  ;; The hook may depend on the subdir-alist containing the just
  ;; inserted subdir, so run it after dired-alist-add:
  (if dired-after-readin-hook
      (save-excursion
	(let ((begin (nth 0 beg-end))
	      (end (nth 1 beg-end)))
	  (goto-char begin)
	  (save-restriction
	    (narrow-to-region begin end)
	    ;; hook may add or delete lines, but the subdir boundary
	    ;; marker floats
	    (run-hooks 'dired-after-readin-hook))))))

(defun dired-tree-lessp (dir1 dir2)
  ;; Lexicographic order on pathname components, like `ls -lR':
  ;; DIR1 < DIR2 iff DIR1 comes *before* DIR2 in an `ls -lR' listing,
  ;;   i.e., iff DIR1 is a (grand)parent dir of DIR2,
  ;;   or DIR1 and DIR2 are in the same parentdir and their last
  ;;   components are string-lessp.
  ;; Thus ("/usr/" "/usr/bin") and ("/usr/a/" "/usr/b/") are tree-lessp.
  ;; string-lessp could arguably be replaced by file-newer-than-file-p
  ;;   if dired-actual-switches contained `t'.
  (setq dir1 (file-name-as-directory dir1)
	dir2 (file-name-as-directory dir2))
  (let ((components-1 (dired-split "/" dir1))
	(components-2 (dired-split "/" dir2)))
    (while (and components-1
		components-2
		(equal (car components-1) (car components-2)))
      (setq components-1 (cdr components-1)
	    components-2 (cdr components-2)))
    (let ((c1 (car components-1))
	  (c2 (car components-2)))

      (cond ((and c1 c2)
	     (string-lessp c1 c2))
	    ((and (null c1) (null c2))
	     nil)			; they are equal, not lessp
	    ((null c1)			; c2 is a subdir of c1: c1<c2
	     t)
	    ((null c2)			; c1 is a subdir of c2: c1>c2
	     nil)
	    (t (error "This can't happen"))))))

;; There should be a builtin split function - inverse to mapconcat.
(defun dired-split (pat str &optional limit)
  "Splitting on regexp PAT, turn string STR into a list of substrings.
Optional third arg LIMIT (>= 1) is a limit to the length of the
resulting list.
Thus, if SEP is a regexp that only matches itself,

   (mapconcat 'identity (dired-split SEP STRING) SEP)

is always equal to STRING."
  (let* ((start (string-match pat str))
	 (result (list (substring str 0 start)))
	 (count 1)
	 (end (if start (match-end 0))))
    (if end				; else nothing left
	(while (and (or (not (integerp limit))
			(< count limit))
		    (string-match pat str end))
	  (setq start (match-beginning 0)
		count (1+ count)
		result (cons (substring str end start) result)
		end (match-end 0)
		start end)
	  ))
    (if (and (or (not (integerp limit))
		 (< count limit))
	     end)			; else nothing left
	(setq result
	      (cons (substring str end) result)))
    (nreverse result)))

;;;###end dired-ins.el


;;; Sorting

;; Most ls can only sort by name or by date (with -t), nothing else.
;; GNU ls sorts on size with -S, on extension with -X, and unsorted with -U.
;; So anything that does not contain these is sort "by name".

(defvar dired-ls-sorting-switches "SXU"
  "String of ls switches (single letters) except `t' that influence sorting.")

(defvar dired-sort-by-date-regexp
  (concat "^-[^" dired-ls-sorting-switches
	  "]*t[^" dired-ls-sorting-switches "]*$")
  "Regexp recognized by dired to set `by date' mode.")

(defvar dired-sort-by-name-regexp
  (concat "^-[^t" dired-ls-sorting-switches "]+$")
  "Regexp recognized by dired to set `by name' mode.")

(defvar dired-sort-mode nil
  "Whether Dired sorts by name, date etc. (buffer-local).")
;; This is nil outside dired buffers so it can be used in the modeline

(defun dired-sort-set-modeline ()
  ;; Set modeline display according to dired-actual-switches.
  ;; Modeline display of "by name" or "by date" guarantees the user a
  ;; match with the corresponding regexps.  Non-matching switches are
  ;; shown literally.
  (setq dired-sort-mode
	(let (case-fold-search)
	  (cond ((string-match dired-sort-by-name-regexp dired-actual-switches)
		 " by name")
		((string-match dired-sort-by-date-regexp dired-actual-switches)
		 " by date")
		(t
		 (concat " " dired-actual-switches)))))
  ;; update mode line:
  (set-buffer-modified-p (buffer-modified-p)))

(defun dired-sort-toggle-or-edit (&optional arg)
  "Toggle between sort by date/name and refresh the dired buffer.
With a prefix argument you can edit the current listing switches instead."
  (interactive "P")
  (if arg
      (dired-sort-other
       (read-string "ls switches (must contain -l): " dired-actual-switches))
    (dired-sort-toggle)))

(defun dired-sort-toggle ()
  ;; Toggle between sort by date/name.  Reverts the buffer.
  (setq dired-actual-switches
	(let (case-fold-search)
	  (concat
	   "-l"
	   (dired-replace-in-string (concat "[---lt"
					    dired-ls-sorting-switches "]")
				    ""
				    dired-actual-switches)
	   (if (string-match (concat "[t" dired-ls-sorting-switches "]")
			     dired-actual-switches)
	       ""
	     "t"))))
  (dired-sort-set-modeline)
  (revert-buffer))

(defun dired-sort-other (switches &optional no-revert)
  ;; Specify new ls SWITCHES for current dired buffer.  Values matching
  ;; `dired-sort-by-date-regexp' or `dired-sort-by-name-regexp' set the
  ;; minor mode accordingly, others appear literally in the mode line.
  ;; With optional second arg NO-REVERT, don't refresh the listing afterwards.
  (setq dired-actual-switches switches)
  (dired-sort-set-modeline)
  (or no-revert (revert-buffer)))

(if (eq system-type 'vax-vms)
    (load "dired-vms"))

(run-hooks 'dired-load-hook)		; for your customizations
