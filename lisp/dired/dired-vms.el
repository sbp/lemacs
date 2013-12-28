;; dired-vms.el - VMS support for dired. $Revision: 1.17 $
;; Copyright (C) 1990 Free Software Foundation, Inc.

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

;; $Id: dired-vms.el,v 1.17 1992/05/20 05:24:07 jwz Exp $

;; You'll need vmsproc.el for this function:
(autoload 'subprocess-command-to-buffer "vmsproc")

(setq dired-subdir-regexp "^ *Directory \\([][:.A-Z-0-9_$;<>]+\\)\\(\\)[\n\r]")

(defconst dired-vms-filename-regexp
"\\(\\([_A-Z0-9$]?\\|[_A-Z0-9$][_A-Z0-9$---]*\\)\\.[_A-Z0-9$---]*;+[0-9]*\\)"
  "Regular expression to match for a valid VMS file name in Dired buffer.
Stupid freaking bug! Position of _ and $ shouldn't matter but they do.
Having [A-Z0-9$_] bombs on filename _$$CHANGE_LOG$.TXT$ and $CHANGE_LOG$.TX
Other orders of $ and _ seem to all work just fine.")

(setq dired-re-mark "^[^ \n\t]")

(defvar dired-directory-command
  "DIRECTORY/SIZE/DATE/PROT"
  "Directory command for dired under VMS.")

;; requires vmsproc.el to work
(defun dired-ls (file switches &optional wildcard full-directory-p)
  "Insert ls output of FILE,formatted according to SWITCHES.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d'.

SWITCHES default to dired-listing-switches.

This is the VMS version of this UNIX command.
The SWITCHES and WILDCARD arguments are ignored.
Uses dired-directory-command."
  (save-restriction;; Must drag point along:
    (narrow-to-region (point) (point))
    (subprocess-command-to-buffer
     (concat dired-directory-command " " file)
     (current-buffer))
    (if full-directory-p
	(goto-char (point-max))
      ;; Just the file line if no full directory required:
      (goto-char (point-min))  
      (let ((case-fold-search nil))
	(re-search-forward dired-subdir-regexp)
	(re-search-forward (concat "^" dired-vms-filename-regexp)))
      (beginning-of-line)
      (delete-region (point-min) (point))
      (forward-line 1)
      (delete-region (point) (point-max)))))

(defun dired-insert-headerline (dir)	; redefinition
  ;; VMS dired-ls makes its own headerline, but we must position the
  ;; cursor where dired-insert-subdir expects it.
  ;; This does not check whether the headerline matches DIR.
  (re-search-forward dired-subdir-regexp)
  (goto-char (match-end 1)))


(defun dired-make-absolute (file &optional dir)
  ;;"Convert FILE (a pathname relative to DIR) to an absolute pathname."
  ;; This should be good enough for ange-ftp, but might easily be
  ;; redefined (for VMS?).
  ;; It should be reasonably fast, though, as it is called in
  ;; dired-get-filename.
  (concat (or dir
	      (dired-current-directory)
	      default-directory)
	  file))

(defun dired-make-relative (file &optional dir)
  ;; In VMS we don't want relative names at all because of search path
  ;; logical names.  Also, we never need to raise an error when a file
  ;; `doesn't belong' in this buffer (like in the Unix case). 
  file)

(defun dired-in-this-tree (file dir)
  ;;"Is FILE part of the directory tree starting at DIR?"
  ;; Under VMS, file="DEV:[foo.bar]zod", dir="DEV:[foo]"
  (or (string= (substring dir -1) "\]")
      (string= (substring dir -1) "\:")
      (error "Not a directory: %s" dir))
  (string-match (concat "^" (regexp-quote (substring dir 0 -1)))
		 file))

(defun dired-vms-split-filename (file)
  (if (string-match;; "DEV:[DIR]FIL" \1=DEV \2=DIR \3=FIL
       "^\\([.A-Z-0-9_$;]*\\):?[[<]\\([.A-Z-0-9_$;]*\\)[]>]\\([.A-Z-0-9_$;]*\\)$"
       file)
      (mapcar '(lambda (x)
		 (substring file (match-beginning x) (match-end x)))
	      '(1 2 3))))

;; Must use this in dired-noselect instead of expand-file-name and
;; file-name-as-directory
;; Taken from the VMS dired version by
;;Roland Roberts                      BITNET: roberts@uornsrl
;;  Nuclear Structure Research Lab  INTERNET: rbr4@uhura.cc.rochester.edu
;;  271 East River Road                 UUCP: rochester!ur-cc!uhura!rbr4
;;  Rochester, NY  14267                AT&T: (716) 275-8962


(defun dired-noselect (dirname &optional switches)
  "Like M-x dired but returns the dired buffer as value, does not select it."
  (setq dirname (dired-fix-directory dirname))
  (dired-internal-noselect dirname switches))

(defun dired-fix-directory (dirname)
  "Fix up dirname to be a valid directory name and return it"
  (setq dirname
	(expand-file-name (or dirname (setq dirname default-directory))))
  (let ((end (1- (length dirname)))
	bracket colon)
    (if (or (char-equal ?\] (elt dirname end))
	    (char-equal ?\: (elt dirname end)))
	dirname
      (setq bracket (string-match "\\]" dirname))
      (setq colon (string-match "\\:" dirname))
      (setq end (string-match "\\.DIR" dirname (or bracket colon)))
      (if end
	  (let ((newdir
		 (if bracket (concat (substring dirname 0 bracket)
				     ".")
		   (if colon (concat (substring dirname 0 (1+ colon))
				     "[")
		     "["))))
	    (concat newdir (substring dirname
				      (1+ (or bracket colon)) end)
		    "]"))
	(if bracket (substring dirname 0 (1+ bracket))
	  (if colon (substring dirname 0 (1+ colon))
	    default-directory))))))

;; Versions are not yet supported in dired.el (as of version 4.53):
;;(setq dired-file-version-regexp "[.;][0-9]+$")

(defun dired-move-to-filename (&optional raise-error eol)
  "In dired, move to first char of filename on this line.
Returns position (point) or nil if no filename on this line."
  ;; This is the VMS version.
  (or eol (setq eol (progn (end-of-line) (point))))
  (beginning-of-line)
  (if (re-search-forward (concat " " dired-vms-filename-regexp) eol t)
      (goto-char (match-beginning 1))
    (if raise-error
	(error "No file on this line")
      nil)))

(defun dired-move-to-end-of-filename (&optional no-error eol)
  ;; Assumes point is at beginning of filename,
  ;; thus the rwx bit re-search-backward below will succeed in *this* line.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; case-fold-search must be nil, at least for VMS.
  ;; On failure, signals an error or returns nil.
  ;; This is the VMS version.
  (let (opoint flag ex sym hidden case-fold-search)
    (setq opoint (point))
    (or eol (setq eol (save-excursion (end-of-line) (point))))
    (setq hidden (and selective-display
		      (save-excursion (search-forward "\r" eol t))))
    (if hidden
	nil
      (re-search-forward dired-vms-filename-regexp eol t))
    (or no-error
	(not (eq opoint (point)))
	(error (if hidden
		   (substitute-command-keys
		    "File line is hidden, type \\[dired-hide-subdir] to unhide")
		 "No file on this line")))
    (if (eq opoint (point))
	nil
      (point))))

(defun dired-tree-lessp (dir1 dir2)
  (setq dir1 (substring (file-name-as-directory dir1) 0 -1)
	dir2 (substring (file-name-as-directory dir2) 0 -1))
  (let ((components-1 (dired-split "[:.]" dir1))
	(components-2 (dired-split "[:.]" dir2)))
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

(defun dired-insert-subdir-validate (dirname)
  (let ((alist dired-subdir-alist)
	(found nil)
	item)
    (while (and alist (not found))
      (setq item (car alist)
	    alist (cdr alist))
      (setq found (dired-in-this-tree dirname (car item))))
    (if (not found)
	(error  "%s: directory not in this buffer" dirname))))

(defun dired-insert-subdir-newpos (new-dir)
  ;; Find pos for new subdir, according to tree order.
  (let ((alist (reverse dired-subdir-alist)) elt dir pos new-pos found)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist)
	    dir (car elt)
	    pos (dired-get-subdir-min elt))
      (if (or (and found
		   (or (dired-in-this-tree dir found)
		       (setq alist nil)))
	      (and (dired-in-this-tree new-dir dir)
		   (setq found dir)))
	  (if (dired-tree-lessp dir new-dir)
	      ;; Insert NEW-DIR after DIR
	      (setq new-pos (dired-get-subdir-max elt)))))
    (goto-char new-pos))
  ;; want a separating newline between subdirs
  (or (eobp)
      (forward-line -1))
  (insert "\n")
  (point))

(defun dired-between-files ()
  (save-excursion
    (beginning-of-line)
    (or (equal (following-char) 9)
	(progn (forward-char 2)
	       (or (looking-at "Total of")
		   (equal (following-char) 32))))))

(defun dired-buffers-for-dir (dir)
  ;; Return a list of buffers that dired DIR (top level or in-situ subdir).
  ;; The list is in reverse order of buffer creation, most recent last.
  ;; As a side effect, killed dired buffers for DIR are removed from
  ;; dired-buffers.
  (setq dir (file-name-as-directory dir))
  (let ((alist dired-buffers) result elt)
    (while alist
      (setq elt (car alist))
      ;; In Unix we only looked into the buffer when
      ;; (dired-in-this-tree dir (car elt)) returned non-nil.
      ;; In VMS we have to look into each buffer because it doesn't
      ;; necessarily contain only the tree starting at the top level directory
      (let ((buf (cdr elt)))
	(if (buffer-name buf)
	    (if (assoc dir (save-excursion
			     (set-buffer buf)
			     dired-subdir-alist))
		(setq result (cons buf result)))
	  ;; else buffer is killed - clean up:
	  (setq dired-buffers (delq elt dired-buffers))))
      (setq alist (cdr alist)))
    result))
