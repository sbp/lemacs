;;; Metamail interface for GNU Emacs
;; Copyright (C) 1993 Masanobu UMEDA

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Version: $Header: /cadillac-inferno-5/cvs-master/lemacs/lisp/packages/metamail.el,v 1.1 1993/12/01 08:54:51 jwz Exp $
;; Keywords: mail, news, mime, multimedia

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; LCD Archive Entry:
;; metamail|Masanobu UMEDA|umerin@mse.kyutech.ac.jp|
;; Metamail interface for GNU Emacs|
;; $Date: 1993/12/01 08:54:51 $|$Revision: 1.1 $|~/misc/metamail.el.Z|

;; Note: Metamail does not have all options which is compatible with
;; the environment variables.  For that reason, matamail.el have to
;; hack the environment variables.  In addition, there is no way to
;; display all header fields without extra informative body messages
;; which is suppressed by "-q" option.

;; The idea of using metamail to process MIME messages is from
;; gnus-mime.el by Spike <Spike@world.std.com>.

;;; Code:

(defvar metamail-program-name "metamail"
  "*Metamail program name.")

(defvar metamail-environment "KEYHEADS='*';export KEYHEADS;"
  "*Environment variables for Metamail.
It must be an emtpy string or a string terminated with ';'.")

(defvar metamail-switches '("-m" "emacs" "-x" "-d" "-z")
  "*Switches for Metamail program.
-z is required to remove zap file.")

(defun metamail-buffer (&optional buffer)
  "Process current buffer through 'metamail'.
Optional argument BUFFER specifies a buffer to be filled (nil means current)."
  (interactive)
  (metamail-region (point-min) (point-max) buffer))

(defun metamail-region (beg end &optional buffer)
  "Process current region through 'metamail'.
Optional argument BUFFER specifies a buffer to be filled (nil means current)."
  (interactive "r")
  (let ((curbuf (current-buffer))
	(buffer-read-only nil)
	(metafile (make-temp-name "/tmp/metamail")))
    (save-excursion
      ;; Gee!  Metamail does not ouput to stdout if input comes from
      ;; stdin.
      (write-region beg end metafile nil 'nomessage)
      (if buffer
	  (set-buffer buffer))
      (setq buffer-read-only nil)
      ;; Clear destination buffer.
      (if (eq curbuf (current-buffer))
	  (delete-region beg end)
	(delete-region (point-min) (point-max)))
      ;; We have to pass the environment variable KEYHEADS to /bin/sh
      ;; to display all header fields.  Metamail should have an
      ;; optional argument to pass such information directly.
      (apply (function call-process)
	     "/bin/sh"
	     nil
	     t				;Output to current buffer
	     t				;Force redisplay
	     (list "-c"
		   ;; Construct environment and the command.
		   (concat
		    metamail-environment
		    metamail-program-name
		    " "
		    (mapconcat (function identity) metamail-switches " ")
		    " "
		    metafile
		    )))
      )))

;(defun metamail-region (beg end &optional buffer)
;  "Process current region through 'metamail'.
;Optional argument BUFFER specifies a buffer to be filled (nil means current)."
;  (interactive "r")
;  (let ((curbuf (current-buffer))
;	(buffer-read-only nil)
;	(metafile (make-temp-name "/tmp/metamail")))
;    (save-excursion
;      (write-region (point-min) (point-max) metafile nil 'nomessage)
;      (if (eq curbuf
;	      (if buffer (get-buffer buffer) (current-buffer)))
;	  (delete-region (point-min) (point-max)))
;      (apply (function call-process)
;	     metamail-program-name
;	     nil
;	     (or buffer t)		;BUFFER or current buffer
;	     nil			;don't redisplay
;	     (append metamail-switches (list metafile)))
;      )))

(provide 'metamail)

;;; metamail.el ends here
