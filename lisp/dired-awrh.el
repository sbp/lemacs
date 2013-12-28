;;;; dired-awrh.el - an after-write-region-hook for Dired to
;;;; 		     automagically update Dired after saving.
;;;; $Id: dired-awrh.el,v 1.14 1992/02/10 14:51:54 sk RelBeta $

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
;;    dired-awrh|Sebastian Kremer|sk@thp.uni-koeln.de
;;    |Automagically update Dired after saving buffers.
;;    |$Date: 92/03/01 14:19:24 $|$Revision: 1.14 $|

;; INSTALLATION ======================================================
;; 
;; You need gmhist-mh.el (this implements the after-write-region-hook)
;; and after-save.el (this implements after-save-buffer-hooks).
;; 
;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;
;;    (load "gmhist-mh")
;;    (autoload 'dired-after-write-region-hook "dired-awrh")
;;    (setq after-write-region-hook 'dired-after-write-region-hook)
;;
;;    (load "after-save")
;;    (autoload 'dired-after-save-buffer-hook "dired-awrh")
;;    (setq after-save-buffer-hooks 'dired-after-save-buffer-hook)
;;
;; to always update the Dired line of a just saved file.

;; PROBLEMS ==========================================================
;;
;; Sometimes Dired will relist files twice (e.g., for byte-compiling).

;; The after-write-region-hook is run immediately after write-region,
;; but basic-save-buffer sometime does set-file-modes afterwards, so
;; the Dired listing may show the wrong permission bits.
;;
;; Thus we also need after-save-buffer-hooks.  This hook alone would
;; not catch all opportunities where files are written (e.g. in byte
;; compiling, autosave files)

;; ange-ftp calls write-region for a *local* temp file, so only that
;; gets relisted, which is actually a bug, but prevents slowness over
;; ftp connections.
;;
;; But if you load ange-ftp *before* gmhist-app, gmhist-app will wrap
;; itself around ange-ftp-write-region, running the hook for the
;; *remote* file, which is the right thing, but may be slow.  Then
;; the default value of this variable takes effect:

(defvar dired-after-write-region-hook-ignored-files
  (if (boundp 'ange-ftp-path-format)
      (car ange-ftp-path-format))
  "*If non-nil, regexp of filenames not to be automagically redisplayed in Dired.
See also function dired-after-write-region-hook.")

;;;###autoload
(defun dired-after-write-region-hook ()
  "Useful on after-write-region-hook to update Dired after saving a buffer.
This might be slow: see variable dired-after-write-region-hook-ignored-files.

If after-write-region-hook is not defined for you, you lack gmhist-mh.el."
  ;; Uses fluid vars: start end filename append visit.
  ;; Returns list of updated buffers (or nil).
  (let ((filename (expand-file-name filename)))
    ;; Sometimes FILENAME is relative, e.g., in TeX-mode's TeX-buffer
    ;; command, but we need to pass absolute pathnames to dired-relist-file.
    (cond ((or (not (fboundp 'dired-relist-file))
	       ;; If Dired is not loaded yet there is nothing to do anyway
	       (and dired-after-write-region-hook-ignored-files
		    (string-match dired-after-write-region-hook-ignored-files
				  filename)))
	   nil)
	  ((and (boundp 'after-save-buffer-hooks)
		(or (eq after-save-buffer-hooks 'dired-after-save-buffer-hook)
		    (and (listp after-save-buffer-hooks)
			 (memq 'dired-after-save-buffer-hook
			       after-save-buffer-hooks)))
		(get-file-buffer filename))
	   ;; If write-region is called because a buffer is being saved,
	   ;; try to avoid to relist it too early (before save-buffer had
	   ;; a change to fix up the permission bits, such as chmod +x etc.)
	   ;; and unnecessarily.
	   nil)
	  (t
	   (let ((dired-omit-silent t))
	     (dired-relist-file filename))))))

;;;###autoload
(defun dired-after-save-buffer-hook ()
  "Useful on after-save-buffer-hooks to update Dired after saving a buffer.

If after-write-region-hook is not defined for you, you lack after-save.el."
  (and (fboundp 'dired-relist-file)
       ;; If dired isn't loaded yet, there is nothing to do
       (let ((dired-omit-silent t))
	 (dired-relist-file (expand-file-name buffer-file-name)))))

(provide 'dired-awrh)
