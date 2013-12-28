;;; -*- Mode:Emacs-Lisp -*-
;;; Jamie Zawinski <jwz@lucid.com> 7-may-91
;;;
;;; This makes dired buffers which display multiple directories display
;;; them in a tree rather than in an "ls -R"-like format.  Which, as every
;;; Lisp Machine hacker knows, is the Right Thing!
;;;
;;;   -rw-r--r--  1 jwz         31543 Mar 26 03:20 reportmail.el
;;;   -rw-r--r--  1 jwz         14919 Mar 26 03:20 reportmail.elc
;;;   drwxr-xr-x  2 jwz          1024 Apr  5 13:08 sk-dired/
;;;     -rw-r--r--  1 jwz          3258 Mar  6 06:33 ange-ftp-dired.el
;;;     -rw-r--r--  1 jwz          1750 Mar 12 15:04 ange-ftp-dired.elc
;;;   -rw-r--r--  1 jwz          3151 Mar 29 00:01 symbol-syntax.el
;;;   -rw-r--r--  1 jwz          1504 Mar 29 01:01 symbol-syntax.elc

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

(defconst dired-subdir-alist nil
  "Association list of subdirectories and their buffer positions:

  ((LASTDIR STARTMARKER ENDMARKER NESTING-DEPTH)
   ...
   (DEFAULT-DIRECTORY POINTMIN POINTMAX 0)).
"
;;The markers point right at the end of the line, so that the cursor
;;looks at either \\n or \\r, the latter for a hidden subdir.
;; The directories must be file-name-as-directory, of course.
)

(defun dired-simple-subdir-alist ()
  ;; Build and return `dired-subdir-alist' assuming just the top level
  ;; directory to be inserted.  Don't parse the buffer.
  (set (make-local-variable 'dired-subdir-alist)
       (list (list default-directory
		   (point-min-marker) (point-max-marker) 0))))

(define-key dired-mode-map "i" 'dired-insert-subdir-inline)
(define-key dired-mode-map "j" 'dired-maybe-insert-subdir)

;;; ## these should be macros when this is integrated with the distribution.
(defun dired-get-subdir-min (elt) (nth 1 elt))
(defun dired-get-subdir-max (elt) (nth 2 elt))

(defun dired-subdir-min ()
  (let ((d (dired-current-directory))
	c)
    (if (setq c (assoc d dired-subdir-alist))
	(marker-position (dired-get-subdir-min c))
      (error "not in a subdir!"))))

(defun dired-subdir-max ()
  (let ((d (dired-current-directory))
	c)
    (if (setq c (assoc d dired-subdir-alist))
	(marker-position (dired-get-subdir-max c))
	(point-max))))

(defun dired-clear-alist ()
  (while dired-subdir-alist
    (let ((elt (car dired-subdir-alist)))
      (set-marker (nth 1 elt) nil)
      (set-marker (nth 2 elt) nil))
    (setq dired-subdir-alist (cdr dired-subdir-alist))))

(defun dired-unsubdir (dir)
  ;; Remove DIR from the alist.
  ;; also remove any directories which are inside of it.
  (let* ((elt (assoc dir dired-subdir-alist))
	 (min (nth 1 elt))
	 (max (nth 2 elt))
	 other-elt
	 (rest dired-subdir-alist))
    (while rest
      (setq other-elt (car rest))
      (if (and (<= min (nth 1 other-elt))
	       (>= max (nth 2 other-elt)))
	  (setq dired-subdir-alist (delq other-elt dired-subdir-alist)))
      (setq rest (cdr rest)))))

;;; this needs to be changed to grok indentation.  Or not. -jwz
;;; Probably not, as dired-revert either starts with one dir and inserting
;;; then enlarges the alist automatically, or it inserts all dirs with
;;; one "ls -lR". -sk
(defun dired-build-subdir-alist ()
  "Build dired-subdir-alist by parsing the buffer and return it's new value."
  (interactive)
  (dired-clear-alist)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (setq dired-subdir-alist nil)
      (while (re-search-forward dired-subdir-regexp nil t)
	(setq count (1+ count))
	(dired-alist-add (buffer-substring (match-beginning 1)
					   (match-end 1))
			 ;; Put subdir boundary between lines:
			 (save-excursion
			   (goto-char (match-beginning 0))
			   (beginning-of-line)
			   (point-marker))
			 ;; isn't this wrong when already more than one
			 ;; subdir is present with -lR?
			 ;; maybe.  I don't know.   But we can't call
			 ;; dired-subdir-max here, it loops.  -jwz.
			 (point-max-marker)
			 0)
	(message "%d" count))
      (message "%d director%s." count (if (= 1 count) "y" "ies"))
      ;; return new alist:
      dired-subdir-alist)))

(defun dired-alist-add (dir start-marker end-marker indentation-depth)
  ;; indentation-depth may be 0 for more than one directory -- this happens
  ;; when "ls -R" format is used.
  ;; ## debugging code
  (or start-marker (error "start marker nil"))
  (or end-marker (error "end marker nil"))
  ;;(or (/= start-marker end-marker) (error "markers are the same"))
  (let ((old (assoc dir dired-subdir-alist)))
    (setq dired-subdir-alist
	  (cons (list (dired-normalize-subdir dir)
		      start-marker end-marker
		      (or indentation-depth 0))
		(delq old dired-subdir-alist)))
    (dired-alist-sort)))

;; can't see at the moment how this could work with a mixed format
;; alist -sk
(defun dired-current-directory (&optional relative)
  "Get the subdirectory to which this line belongs.
This returns a string with trailing slash, like default-directory.
Optional argument means return a name relative to default-directory."
  (let (elt
	(here (point))
	(alist (or dired-subdir-alist (dired-build-subdir-alist)))
	best-so-far)
    (while alist
      (setq elt (car alist))
      (if (or (< here (nth 1 elt))
	      (> here (nth 2 elt)))
	  nil ;; the subdir is disjoint
	;; otherwise it's on the path between the current file and the root.
	;; decide if it's deeper than what we've already got.
	(if (or (null best-so-far)
		(< (- (nth 2 elt) (nth 1 elt))
		   (- (nth 2 best-so-far) (nth 1 best-so-far))))
	    (setq best-so-far elt)))
      (setq alist (cdr alist)))
    (if best-so-far
	(if relative
	    (dired-make-relative (car best-so-far) default-directory)
	  (car best-so-far))
      (progn
	(dired-build-subdir-alist)
	(dired-current-directory relative)))))

(defun dired-insert-subdir-del (element)
  ;; Erase an already present subdir (given by ELEMENT) from buffer.
  ;; Move to that buffer position.  Return a mark-alist.
  (let ((begin-marker (dired-get-subdir-min element))
	(end-marker (dired-get-subdir-max element)))
    (goto-char end-marker)
    (or (eobp)
	(not (= 0 (nth 3 element)))
	;; for -R style, want a separating newline _between_ subdirs.
	(forward-char -1))
    (if (= 0 (nth 3 element))
	(insert "\n\n"))
    (prog1
	(dired-remember-marks begin-marker (point))
      (delete-region begin-marker (point)))))


(defun dired-insert-subdir-doupdate (dirname elt beg-end)
  (let ((beg (nth 0 beg-end))
	(end (nth 1 beg-end))
	(indent (or (nth 2 beg-end) 0)))
    (if (and elt
	     (not (eq indent (nth 2 elt))))
	(setq elt nil
	      dired-subdir-alist (delq elt dired-subdir-alist)))
    (if elt
	(let ((old-start (nth 1 elt))
	      (old-end   (nth 2 elt)))
	  (set-marker old-start beg)
	  (set-marker old-end end)
	  (setcar (nthcdr 3 elt) indent))
      (dired-alist-add dirname
		       (set-marker (make-marker) beg)
		       (set-marker (make-marker) end)
		       indent))))

(defun dired-insert-subdir-inline (dirname &optional switches no-error-if-not-dir-p)
  "Insert this subdirectory into the same dired buffer.
If it is already present, overwrites previous entry,
  else inserts it, indented, within its parent's listing.
With a prefix arg, you may edit the ls switches used for this listing.
  This command ignores the `R' switch."
  ;; NO-ERROR-IF-NOT-DIR-P needed for special filesystems like
  ;; Prospero where dired-ls does the right thing, but
  ;; file-directory-p has not been redefined.
  (interactive
   (list (dired-get-filename)
	 (if current-prefix-arg
	     (read-string "Switches for listing: " dired-actual-switches))))
  (setq dirname (file-name-as-directory (expand-file-name dirname)))
  (if (let ((case-fold-search nil))
	(string-match "R" (or switches "")))
      (setq switches (concat (substring switches 0 (match-beginning 0))
			     (substring switches (match-end 0)))))
  (dired-make-relative dirname default-directory) ; error on failure
  (or no-error-if-not-dir-p
      (file-directory-p dirname)
      (error  "Attempt to insert a non-directory: %s" dirname))
  (let ((elt (assoc dirname dired-subdir-alist))
	(parentdir (file-name-directory (directory-file-name dirname)))
	beg end old-start old-end new-start new-end
	mark-alist
	tail-adjascents
	buffer-read-only case-fold-search)
    (if elt
	;; subdir is already present - must first erase it from buffer.
	;; if it's already in -R format, pretend it wasn't there, but
	;; remember its file marks.
	(progn
	  (setq mark-alist
		(append (dired-insert-subdir-del elt) mark-alist))
	  (setq dired-subdir-alist
		(delq elt dired-subdir-alist))))
    ;;(dired-insert-subdir-newpos) ;##
    (dired-goto-file dirname)
    (forward-line 1)
    (dired-insert-subdir-doupdate
     dirname elt (dired-insert-subdir-inline-doinsert dirname switches parentdir))
    (dired-initial-position dirname)
    (save-excursion (dired-mark-remembered mark-alist)))
  (dired-nuke-extra-newlines)
  )


(defun dired-insert-subdir (dirname &optional switches no-error-if-not-dir-p)
  "Insert this subdirectory into the same dired buffer.
If it is already present, overwrites previous entry,
  else appends at end of buffer.
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
  (dired-make-relative dirname default-directory) ; error on failure
  (or no-error-if-not-dir-p
      (file-directory-p dirname)
      (error  "Attempt to insert a non-directory: %s" dirname))
  (let ((elt (assoc dirname dired-subdir-alist))
	(switches-have-R (and switches (string-match "R" switches)))
	mark-alist
	buffer-read-only case-fold-search)
    (if switches-have-R			; avoid double subdirs
	(setq mark-alist (dired-kill-tree dirname t)))
    (let ((was-nested (and (nth 3 elt) (not (eq 0 (nth 3 elt))))))
      (if elt				; subdir is already present
	  (setq mark-alist		; remove it, remembering marks
		(append (dired-insert-subdir-del elt) mark-alist)))
      (if (or was-nested (null elt))
	  (dired-insert-subdir-newpos dirname))
      (if was-nested (setcar (nthcdr 3 elt) 0)))
    (dired-insert-subdir-doupdate
     dirname elt (dired-insert-subdir-doinsert dirname switches))
    (if switches-have-R (dired-build-subdir-alist))
    (dired-initial-position dirname)
    (save-excursion (dired-mark-remembered mark-alist)))
  (dired-nuke-extra-newlines))

(defun dired-nuke-extra-newlines ()
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\n\n\n+" nil t)
	(goto-char (+ 2 (match-beginning 0)))
	(delete-region (point) (match-end 0))))))


(defun dired-insert-subdir-newpos (new-dir)
  ;; Find pos for new subdir, according to tree order.
  ;;(goto-char (point-max))
  (let ((alist dired-subdir-alist) elt dir pos new-pos)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist)
	    dir (car elt)
	    pos (dired-get-subdir-min elt))
      (if (and (= 0 (nth 3 elt)) ; nested ones don't count.
	       (dired-tree-lessp dir new-dir))
	  ;; Insert NEW-DIR after DIR
	  (setq new-pos (dired-get-subdir-max elt)
		alist nil)))
    (goto-char new-pos))
  ;; want a separating newline between subdirs
  (insert "\n\n")
  (point))


(defvar dired-no-inline-headerlines t
  "*set this to t to suppress the directory header and `total' line.")


(defun dired-insert-subdir-inline-doinsert (dirname switches parentdir)
  ;; Insert ls output after point and put point on the correct
  ;; position for the subdir alist.
  ;; returns the dired-subdir-alist entry.
  (let ((begin (point)) end
	indent
	tail-adjascents)
    (message "Reading directory %s..." dirname)
    (dired-ls dirname
	      (or switches
		  (dired-replace-in-string "R" "" dired-actual-switches))
	      nil t)
    (message "Reading directory %s...done" dirname)
    (setq end (point))
    (setq indent (1+ (nth 3 (assoc parentdir dired-subdir-alist))))

    (save-excursion
      (goto-char begin)
      (or dired-no-inline-headerlines
	  (progn
	    (dired-insert-headerline dirname)
	    (save-excursion (delete-horizontal-space)))
	  (goto-char begin)
	  (delete-horizontal-space))
      (if (and dired-no-inline-headerlines
	       (looking-at "^ *total [0-9]"))
	  (progn
	    (delete-region (point) (progn (forward-line 1) (point)))
	    (setq begin (point)))))
    ;;
    ;; If there are other directories whose end-point is right here,
    ;; then they are the directories such that X is the last directory
    ;; in the listing of Y.  We need to grab them and update their
    ;; last-point to be the same as ours will be (goofy margin-case).
    ;;
    (let ((rest dired-subdir-alist))
      (while rest
	(if (= (point) (nth 2 (car rest)))
	    (setq tail-adjascents (cons (car rest) tail-adjascents)))
	(setq rest (cdr rest))))
    (let ((indent-tabs-mode nil))
      (indent-rigidly begin (point) (* 2 (1+ indent))))
    (setq end (point-marker))
    (goto-char begin)
    (while tail-adjascents
      (set-marker (nth 2 (car tail-adjascents)) end)
      (setq tail-adjascents (cdr tail-adjascents)))
    (if dired-after-readin-hook
	(save-restriction
	  (narrow-to-region begin end)
	  (run-hooks 'dired-after-readin-hook)))
    ;;  call dired-insert-headerline afterwards, as under VMS dired-ls
    ;;  does insert the headerline itself and the insert function just
    ;;  moves point.
    (setq end (prog1 (marker-position end) (set-marker end nil)))
    (goto-char begin)
    (list begin end indent)))


(defun dired-insert-subdir-doinsert (dirname switches)
  ;; Insert ls output after point and put point on the correct
  ;; position for the subdir alist.
  ;; Return the boundary of the inserted text (as list of BEG and END).
  (let ((begin (point)) end)
    (message "Reading directory %s..." dirname)
    (dired-ls dirname
	      (or switches
		  (dired-replace-in-string "R" "" dired-actual-switches))
	      nil t)
    (message "Reading directory %s...done" dirname)
    (insert "\n\n")
    (setq end (point-marker))
    (indent-rigidly begin (point) 2)
    (if dired-after-readin-hook
	(save-restriction
	  (narrow-to-region begin (point))
	  (run-hooks 'dired-after-readin-hook)))
    ;;  call dired-insert-headerline afterwards, as under VMS dired-ls
    ;;  does insert the headerline itself and the insert function just
    ;;  moves point.
    (goto-char begin)
    (dired-insert-headerline dirname)
    ;; point is now like in dired-build-subdir-alist
    (setq end (prog1 (marker-position end) (set-marker end nil)))
    (list begin end)))


(defun dired-insert-old-subdirs (old-subdir-alist)
  ;; Try to insert all subdirs that were displayed before
  (or (string-match "R" dired-actual-switches)
      (let (elt dir)
	(setq old-subdir-alist (sort old-subdir-alist
				     (function (lambda (x y)
				       (< (nth 3 x) (nth 3 y))))))
	(while old-subdir-alist
	  (setq elt (car old-subdir-alist)
		old-subdir-alist (cdr old-subdir-alist)
		dir (car elt))
	  (condition-case ()
	      (if (= 0 (nth 3 elt))
		  (dired-insert-subdir dir)
		  (dired-insert-subdir-inline dir))
	    (error nil))))))

(defun dired-add-entry-do-indentation (marker-char)
  ;; two spaces or a marker plus a space, plus nesting indentation.
  ;; Uses fluid vars `directory', `marker-char' from dired-add-entry
  (insert (if marker-char
	      (if (integerp marker-char) marker-char dired-marker-char)
	    ?\040)
	  ?\040)
  (let ((indent (nth 3 (assoc directory dired-subdir-alist))))
    (insert (make-string (* 2 indent) ?\040))))
