;;!emacs
;;
;; FILE:         dired-link.el
;; SUMMARY:      Properly resolves UNIX (and Apollo variant) links under dired.
;;               Works for both classic dired (V18) and tree dired (V19).
;;
;; AUTHOR:       Bob Weiner
;;
;; ORIG-DATE:    09-May-89
;; LAST-MOD:     15-Apr-92
;;
;; Copyright (C) 1989, 1991, Brown University, Providence, RI
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; This file is not part of GNU Emacs.
;;
;; DESCRIPTION:  
;;
;;   This library is used in conjunction with the GNU Emacs dired facility.
;;   To install it, simply load this file or create a
;;   dired hook which loads this file.  Then use {M-x dired <directory> RTN}
;;   or {C-x C-f <directory> RTN} as one normally would.
;;
;;   The changes below to 'dired-noselect' assume UNIX shell file
;;   abbreviation and UNIX file name conventions.
;;
;;   This modified version of the 'dired-noselect' function automatically
;;   resolves all recursive links properly and edits the final directory that
;;   a link points to, called the link referent.  It handles Apollo-isms such
;;   as /usr/local -> $(SERVER_LOCAL)/usr/local, /usr/bin ->
;;   ../$(SYSTYPE)/usr/bin and /tmp -> `node_data/tmp.  It also handles
;;   relative links properly as in /usr/local/emacs -> gnu/emacs which must
;;   be resolved relative to the '/usr/local' directory.
;;
;; DESCRIP-END.

(require 'dired)

;; ************************************************************************
;; Internal functions
;; ************************************************************************

;; Normally, if one performs a dired multiple times on a directory which is a
;; link, a new buffer will be created each time.  This is due to the fact
;; that 'dired-find-buffer' is called in 'dired-noselect' before the link is
;; resolved.  The following code solves this problem by checking for a
;; previously existing buffer that is performing dired on the directory that
;; the link resolves to.  This is also done recursively.  If one is found,
;; the dired buffer that shows the link is killed and the previously existing
;; one is used and re-read in.

(defun dired-link-noselect-classic (dirname)
  "Like M-x dired but returns the dired buffer as value, does not select it."
  (or dirname (setq dirname default-directory))
  (setq dirname (dired-link-referent (directory-file-name dirname)))
  (if (equal dirname "")
      nil
    (if (= (aref dirname 0) ?~) (setq dirname (expand-file-name dirname)))
    (if (file-directory-p dirname)
	(setq dirname (file-name-as-directory dirname)))
    (let ((buffer (dired-find-buffer dirname)))
      (set-buffer buffer)
      (dired-readin dirname buffer)
      (dired-move-to-filename)
      (dired-mode dirname)
      buffer)))

(defun dired-link-noselect-tree (dirname &optional switches)
  "Like `dired' but returns the dired buffer as value, does not select it."
  (or dirname (setq dirname default-directory))
  (setq dirname (expand-file-name
		 (dired-link-referent (directory-file-name dirname))))
  (if (file-directory-p dirname)
      (setq dirname (file-name-as-directory dirname)))
  (dired-internal-noselect dirname switches))

;; Overload as appropriate for Classic (V18) or Tree Dired
(fset 'dired-noselect (if (fboundp 'dired-internal-noselect)
			  'dired-link-noselect-tree
			'dired-link-noselect-classic))

;;
;; Resolves all UNIX links.
;; Works with Apollo's variant and other strange links.  Will fail on
;; Apollos if the '../' notation is used to move just above the '/'
;; directory level.  This is fairly uncommon and so the problem has not been
;; fixed.
;;;
(defun dired-link-referent (linkname)
  "Returns expanded file or directory referent of LINKNAME.
LINKNAME should not end with a directory delimiter.
If LINKNAME is not a string, returns nil.
If LINKNAME is not a link, it is simply expanded and returned."
  (if (not (stringp linkname))
      nil
    (let ((referent))
      (while (setq referent (file-symlink-p linkname))
	(setq linkname (dired-link-expand
			referent (file-name-directory linkname)))))
    (dired-link-expand linkname (file-name-directory linkname))))

(defun dired-link-expand (referent dirname)
  "Expands REFERENT relative to DIRNAME and returns."
  (let ((var-link)
	(dir dirname))
    (while (string-match "\\$(\\([^\)]*\\))" referent)
      (setq var-link (getenv (substring referent (match-beginning 1)
					(match-end 1)))
	    referent (concat (substring referent 0 (match-beginning 0))
			     var-link
			     (substring referent (match-end 0)))))
    ;; If referent is not an absolute path
    (let ((nd-abbrev (string-match "`node_data" referent)))
      (if (and nd-abbrev (= nd-abbrev 0))
	  (setq referent (concat
			   ;; Prepend node name given in dirname, if any
			   (and (string-match "^//[^/]+" dirname)
				(substring dirname 0 (match-end 0)))
			   "/sys/" (substring referent 1)))))
    (while (string-match "\\.\\." referent)
      ;; Match to "//.." or "/.." at the start of link referent
      (while (string-match "^\\(//\\.\\.\\|/\\.\\.\\)\\(/\\|$\\)" referent)
	(setq referent (substring referent (match-end 1))))
      ;; Match to "../" or ".." at the start of link referent
      (while (string-match "^\\.\\.\\(/\\|$\\)" referent)
	(setq dir (file-name-directory (directory-file-name dir))
	      referent (concat dir (substring referent (match-end 0)))))
      ;; Match to rest of "../" in link referent
      (while (string-match "[^/]+/\\.\\./" referent)
	(setq referent (concat (substring referent 0 (match-beginning 0))
			       (substring referent (match-end 0))))))
    (and (/= (aref referent 0) ?~)
	 (/= (aref referent 0) ?/)
	 (setq referent (concat dirname referent))))
  referent)

(provide 'dir-links)			; the old name of this package
(provide 'dired-link)
