;;;; dired-rcs.el - RCS support for Tree Dired

(defconst dired-rcs-version (substring "$Revision: 1.5 $" 11 -2)
  "$Id: dired-rcs.el,v 1.5 1992/05/20 05:27:55 jwz Exp $")

;; Copyright (C) 1991 by Sebastian Kremer <sk@thp.uni-koeln.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LISPDIR ENTRY for the Elisp Archive ===============================
;;    LCD Archive Entry:
;;    dired-rcs|Sebastian Kremer|sk@thp.uni-koeln.de
;;    |RCS support for Tree Dired 
;;    |$Date: 1992/05/20 05:27:55 $|$Revision: 1.5 $|

;; INSTALLATION ======================================================
;; 
;; You need to have my rcs.el loaded for this to work.  It is
;; available via anonymous ftp from
;; 
;;     ftp.thp.Uni-Koeln.DE[134.95.64.1]:/pub/gnu/emacs/rcs.tar.Z
;;
;; This will not work with classic (18.xx) Dired, you'll need Tree Dired,
;; available via anonymous ftp from
;; 
;;     ftp.thp.Uni-Koeln.DE[134.95.64.1]:/pub/gnu/emacs/diredall.tar.Z
;;
;; Put this file into your load-path and the following in your ~/.emacs:
;; 
;;   (autoload 'dired-rcs-mark-rcs-locked-files "dired-rcs")
;;   (autoload 'dired-rcs-mark-rcs-files "dired-rcs")
;;
;; Put this inside your dired-load-hook:
;; 
;;   (define-key dired-mode-map "," 'dired-rcs-mark-rcs-files)
;;   (define-key dired-mode-map "\M-," 'dired-rcs-mark-rcs-locked-files)
;;

(require 'dired)

;;;###autoload
(defun dired-rcs-mark-rcs-locked-files (&optional unflag-p)
  "Mark all files that are under RCS control and RCS-locked.
With prefix argument, unflag all those files.
Mentions RCS files for which a working file was not found in this buffer.
Type \\[dired-why] to see them again."
  (interactive "P")
  (dired-rcs-mark-rcs-files unflag-p t))

;;;###autoload
(defun dired-rcs-mark-rcs-files (&optional unflag-p locked)
  "Mark all files that are under RCS control.
With prefix argument, unflag all those files.
Mentions RCS files for which a working file was not found in this buffer.
Type \\[dired-why] to see them again."
  ;; Returns list of failures, or nil on success.
  ;; Optional arg LOCKED means just mark RCS-locked files.
  (interactive "P")
  (message "%s %sRCS controlled files..."
	   (if unflag-p "Unmarking" "Marking")
	   (if locked "locked " ""))
  (let ((dired-marker-char (if unflag-p ?\  dired-marker-char))
	rcs-files wf failures count total)
    ;; Loop over subdirs to set `rcs-files'
    (mapcar
     (function
      (lambda (dir)
	(or (equal (file-name-nondirectory (directory-file-name dir))
		   "RCS")
	    ;; skip inserted RCS subdirs
	    (setq rcs-files
		  (append (if locked
			      ;; these two functions from sk's rcs.el
			      (rcs-locked-files dir)
			    (rcs-files dir))
			  rcs-files)))))
     (mapcar (function car) dired-subdir-alist))
    (setq total (length rcs-files))
    (while rcs-files
      (setq wf (rcs-working-file (car rcs-files))
	    rcs-files (cdr rcs-files))
      (save-excursion (if (dired-goto-file wf)
			  (dired-mark-file 1)
			(dired-log "RCS working file not found: %s\n" wf)
			(setq failures (cons (dired-make-relative wf)
					     failures)))))
    (if (null failures)
	(message "%d %sRCS file%s %smarked."
		 total
		 (if locked "locked " "")
		 (dired-plural-s total)
		 (if unflag-p "un" ""))
      (setq count (length failures))
      (dired-log-summary "RCS working file not found %s" failures)
      (message "%d %sRCS file%s: %d %smarked - %d not found %s."
	       total
	       (if locked "locked " "")
	       (dired-plural-s total) (- total count)
	       (if unflag-p "un" "") count failures))
    failures))
