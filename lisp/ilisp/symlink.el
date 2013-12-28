;; symlink: Fix to remove symbolic links from file pathnames.
;; Copyright (C) 1989 Free Software Foundation, Inc.

;; This file is not officially part of GNU Emacs, but is being donated
;; to the Free Software Foundation.

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

;; Created by: Joe Wells, joew@uswest.com
;; Created on: summer 1988
;; Last modified by: Joe Wells, jbw@bucsf
;; Last modified on: Fri May 11 12:56:08 1990
;; Filename: symlink.el

;; Modified by Chris McConnell and Todd Kaufmann for ilisp and CMU.
(or (fboundp 'original-expand-file-name)
    (fset 'original-expand-file-name
	  (symbol-function 'expand-file-name)))

;;;
(defvar expand-symlinks nil "*Set this to T to expand symbolic links.")

;;;; Added by Todd.Kaufmann@cs.cmu.edu, 20-Jul-90:  for CMU RFS.
;;;
;;; Because of weird cmu rfs filesystem, the /../machine must be preserved in
;;; links, but toss it when a link goes to another machine or afs.
(defvar expand-symlinks-rfs-exists nil
  "T at cmu, nil otherwise.  This is to preserve machine name context like
/../foo in the file names during expansion of symlinks.")

(defun expand-symlinks-rfs-machine-context (old-dir new-dir)
  "If new-dir is afs or rfs to another machine, forget old-dir.
Otherwise leave /../machine in the old-dir and return it, else the empty
string."
  (if (string-match "\\(/\\.\\.\\|/afs\\)" new-dir)    ; new: afs or rfs?
      ""
      (if (not (string-match "^/\\.\\./[^/]+/" (setq *ld old-dir))) ; old: rfs?
	  ""
	  (substring old-dir 0 (1- (match-end 0))))
      ))
  
;;; Concatenates LEFT and RIGHT, preserving at most one slash between them.
;;; This horrible hack is necessary to work around the fact that
;;; expand-file-name treats // specially.
(defun join-file-name (left right)
  (let* ((llen (length left))
	 (rlen (length right))
	 (lend llen)
	 (rstart 0)
	 slash-found)
    (while (and (> lend 0)
		(eq ?/ (aref left (1- lend))))
      (setq slash-found t
	    lend (1- lend)))
    (while (and (< rstart rlen)
		(eq ?/ (aref right rstart)))
      (setq slash-found t
	    rstart (1+ rstart)))
    (concat (if (eq lend llen) left (substring left 0 lend))
	    (if slash-found "/" "")
	    (if (eq rstart 0) right (substring right rstart rlen)))))

;;; Splits FILENAME into two strings, and returns a list of the two
;;; strings.  The first string will be the first filename component in
;;; FILENAME, plus any leading slashes, and the second string will be the
;;; rest of FILENAME, possibly a string of length 0.
(defun split-file-name (file-name)
  (if (string-match "\\`\\(/*[^/]+\\)\\(/.*\\)\\'" file-name)
      (cons (substring file-name (match-beginning 1) (match-end 1))
	    (substring file-name (match-beginning 2) (match-end 2)))
    (cons file-name "")))

;;; Takes FILENAME and LINK and returns a string which substitutes LINK for
;;; the last component of FILENAME.
(defun hack-local-link (file-name link)
  (or (string-match "\\`\\(.*/\\)[^/]+\\'" file-name)
      (error "hack-local-link bad argument: %s" file-name))
  (concat (substring file-name (match-beginning 1) (match-end 1)) link))

;;;
(defun expand-file-name (file-name &optional directory)
  "Convert FILENAME to absolute, and canonicalize it.
Second arg DEFAULT is directory to start with if FILENAME is relative
\(does not start with slash)		; if DEFAULT is nil or missing,
the current buffer's value of default-directory is used.
Filenames containing . or .. as components are simplified ;
initial ~ is expanded.  See also the function  substitute-in-file-name.
This has been modified to resolve all symbolic links from FILENAME.
The original function definition is stored on original-expand-file-name."
  (let (left-slash left right split link)
    (setq right (original-expand-file-name file-name directory))
    (if expand-symlinks
	(progn
	  (setq left-slash (string-match "//" right)
		left-slash (and left-slash (zerop left-slash)))
	  (setq left "")
	  (while (not (equal right ""))
	    (setq split (split-file-name right))
	    (setq left (join-file-name left (car split)))
	    (if left-slash
		(setq left (concat "/" left)
		      *l2 left
		      left-slash nil))
	    (setq right (cdr split))
	    (setq link (file-symlink-p left))
	    (if (null link)
		nil
		(if (eq 0 (length link)) (setq link "."))
		(cond ((not (eq (aref link 0) ?/))
		       (setq split (split-file-name link))
		       (setq left (join-file-name (file-name-directory left)
						  (car split)))
		       (setq right (join-file-name (cdr split) right)))
		      (t
		       (setq right (join-file-name link right))
		       ;; mod by tk, 20-Jul-90 for cmu csd w/rfs.
		       ;; maybe keep rfs part of file name?
		       (if expand-symlinks-rfs-exists
			   (setq left (expand-symlinks-rfs-machine-context
				       left right))
			   (setq left "")
			   )))))
	  left)
    	right)))

(provide 'symlink)
