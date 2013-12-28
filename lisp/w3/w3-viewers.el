;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to pass files off to external viewers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-start-viewer (fname cmd &optional view)
  "Start a subprocess, named FNAME, executing CMD
If third arg VIEW is non-nil, show the output in a buffer when
the subprocess exits."
  (if view (save-excursion
	     (set-buffer (get-buffer-create view))
	     (erase-buffer)))
  (let ((proc
	 (start-process fname view (or (getenv "ESHELL")
				       (getenv "SHELL")
				       "/bin/sh") "-c" cmd)))
    proc))

(defun w3-pass-to-viewer ()
  "Pass a w3 buffer to a viewer based on file extension."
  (set-buffer w3-working-buffer)
  (let* ((view w3-current-mime-viewer))
    (if (null view)
	(setq view 'indented-text-mode))
    (if (symbolp view)
	(if (not (memq view '(w3-prepare-buffer w3-print w3-source)))
	    (progn
	      (rename-buffer (w3-generate-new-buffer-name
			      (file-name-nondirectory w3-current-file)))
	      (set-buffer-modified-p nil)
	      (if w3-mutable-windows
		    (pop-to-buffer (file-name-nondirectory w3-current-file))
		(switch-to-buffer (file-name-nondirectory w3-current-file)))
	      (buffer-enable-undo)
	      (funcall view))
	  (funcall view))
      (if (or (eq window-system 'x)
	      (funcall w3-confirmation-func
		       (format "Xwindows not detected, still use %s?" view)))
	  (let ((fname (w3-generate-unique-filename))
		(show (cond
		       ((null w3-always-show-output) nil)
		       ((eq w3-always-show-output t) t)
		       (t (funcall w3-confirmation-func
				   "View process output?")))))
	    (write-region (point-min) (point-max) fname)
	    (kill-buffer w3-working-buffer)
	    (message "Passing to viewer %s" view)
	    (set-process-sentinel
	     (w3-start-viewer fname (format view fname)
			      (if show
				  (w3-generate-new-buffer-name
				   (prin1-to-string (read view)))
				nil))
			      'w3-viewer-sentinel))
	(let ((x (read-file-name "Filename to save as: "
				 (expand-file-name "~/") "")))
	  (write-region (point-min) (point-max) x))))))

(defun w3-save-binary-file ()
  (interactive)
  (let ((x (read-file-name "Filename to save as: "
			   (expand-file-name "~/") "")))
    (save-excursion
      (set-buffer w3-working-buffer)
      (write-region (point-min) (point-max) x)
      (kill-buffer w3-working-buffer))))

(defun w3-viewer-sentinel (proc string)
  "Delete any temp files left from a viewer process."
  (let ((fname (process-name proc))
	(buffr (process-buffer proc)))
    (if (and (file-exists-p fname)
	     (file-writable-p fname))
	(delete-file fname))
    (if buffr
	(if w3-mutable-windows
	    (pop-to-buffer buffr)
	  (switch-to-buffer buffr)))))

(defun w3-generate-new-buffer-name (start)
  "Create a new buffer name based on START."
  (let ((x 1)
	name)
    (if (not (get-buffer start))
	start
      (progn
	(setq name (format "%s<%d>" start x))
	(while (get-buffer name)
	  (setq x (1+ x)
		name (format "%s<%d>" start x)))
	name))))

(defun w3-generate-unique-filename ()
  "Generate a unique filename in /tmp"
  (let ((base (format "/tmp/w3-tmp.%d" (user-real-uid)))
	(fname "")
	(x 0))
    (setq fname (format "%s%d" base x))
    (while (file-exists-p fname)
      (setq x (1+ x)
	    fname (format "%s%d" base x)))
    fname))

(defun w3-extension-to-mime (extn)
  "This will convert a file extensions (EXTN) to a mime-type, using
the variable w3-mime-extensions"
  (cdr (assoc (downcase extn) w3-mime-extensions)))

(provide 'w3-viewers)
