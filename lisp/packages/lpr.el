;; Print Emacs buffer on line printer.
;; Copyright (C) 1985, 1988 Free Software Foundation, Inc.

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


;(defconst lpr-switches nil
;  "*List of strings to pass as extra switch args to lpr when it is invoked.")

(defvar lpr-command (if (eq system-type 'usg-unix-v)
			"lp" "lpr")
  "Shell command for printing a file")

(defun lpr-buffer ()
  "Print buffer contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive)
  (print-region-1 (point-min) (point-max) lpr-switches nil))

(defun print-buffer ()
  "Print buffer contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive)
  (print-region-1 (point-min) (point-max) lpr-switches t))

(defun lpr-region (start end)
  "Print region contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive "r")
  (print-region-1 start end lpr-switches nil))

(defun print-region (start end)
  "Print region contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive "r")
  (print-region-1 start end lpr-switches t))

(defun print-region-1 (start end switches page-headers)
  (let ((name (concat (buffer-name) " Emacs buffer"))
	(width tab-width))
    (save-excursion
      (message "Spooling...")
      (if (/= tab-width 8)
	  (progn
	    (print-region-new-buffer) ; dynaimcally accesses start and end
	    (setq tab-width width)
	    (untabify (point-min) (point-max))))
      (if page-headers
	  (if (eq system-type 'usg-unix-v)
	      (progn
		(print-region-new-buffer)
		(call-process-region start end "pr" t t nil))
	    ;; On BSD, use an option to get page headers.
	    (setq switches (cons "-p" switches))))
      (apply 'call-process-region
	     (nconc (list start end lpr-command
			  nil nil nil)
		    (nconc (and (eq system-type 'berkeley-unix)
				(list "-J" name "-T" name))
			   switches)))
      (message "Spooling...done"))))

;; This function copies the text between start and end
;; into a new buffer, makes that buffer current,
;; and sets start and end to the buffer bounds.
;; start and end are used free.
(defun print-region-new-buffer ()
  (or (string= (buffer-name) " *spool temp*")
      (let ((oldbuf (current-buffer)))
	(set-buffer (get-buffer-create " *spool temp*"))
	(widen) (erase-buffer)
	(insert-buffer-substring oldbuf start end)
	(setq start (point-min) end (point-max)))))
