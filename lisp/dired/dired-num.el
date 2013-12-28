;;;; dired-num.el - Renaming with numbers in Tree Dired.

(defconst dired-num-version (substring "!Revision: 1.2 !" 11 -2)
  "Id: dired-num.el,v 1.2 1991/10/15 13:24:10 sk RelBeta ")
  
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
;;    dired-num|Sebastian Kremer|sk@thp.uni-koeln.de
;;    |Renaming with numbers in Tree Dired. 
;;    |Date: 1991/10/15 13:24:10 |Revision: 1.2 |

;; INSTALLATION ======================================================
;; 
;; Put this file into your load-path and the following in your ~/.emacs:
;; 
;;   (autoload 'dired-do-rename-numeric "dired-num")
;;   (autoload 'dired-do-rename-list "dired-num")
;;
;; Do
;; 
;;   (define-key dired-mode-map "%#" 'dired-do-rename-numeric)
;;   (define-key dired-mode-map "%(" 'dired-do-rename-list)
;;
;; inside your dired-load-hook.

(require 'dired);; we need its macros when being compiled

(defun dired-number-of-marked-files (&optional arg)
  ;; Return the number of marked files in a dired buffer.
  ;; Optional ARG as in dired-mark-map.
  (length
   (save-excursion
     ;; this returns a list of ``results'' (i.e. nil's):
     (dired-mark-map nil arg))))

(defun dired-do-create-files-numeric (file-creator operation arg format start
						   &optional arg)
  ;; Create a new file for each marked file using numbers.
  ;; FILE-CREATOR and OPERATION as in dired-create-files.
  ;; ARG as in dired-mark-get-files.
  ;; FORMAT is a format string for use with an integer, assuming
  ;; values starting from START, incremented for each marked file.
  (let ((i (1- start)));; signals an error if START is not a number
    (dired-create-files-non-directory
     file-creator
     (function (lambda (x)
		 (format format (setq i (1+ i)))))
     operation arg)))

;;;###autoload
(defun dired-do-rename-numeric (format start &optional arg)
  "Rename all marked (or next ARG) files using numbers.
You are prompted for a format string, e.g \"part_%d_of_8\", and a starting
number, e.g. 1.  If there are 8 marked files, this example will rename them to

    part_1_of_8
    part_2_of_8
    ...
    part_8_of_8"
  (interactive
   (list
    (read-string
     (format "Rename numeric [%d files] (format string using %%d): "
	     (dired-number-of-marked-files current-prefix-arg)))
    (read-minibuffer "Numbers start at: " "1")
    current-prefix-arg))
  (dired-do-create-files-numeric
   (function dired-rename-file)
   "Rename-numeric" arg format start))

;; Copy etc. would be similar to implement.


(defun dired-do-create-files-list (file-creator operation arg format list
						   &optional arg)
  ;; Create a new file for each marked file by subsituting elements
  ;; from LIST in the format string FORMAT.
  ;; FILE-CREATOR and OPERATION as in dired-create-files.
  ;; ARG as in dired-mark-get-files.
  (let ((rest list))
    (dired-create-files-non-directory
     file-creator
     (function (lambda (x)
		 (format format (prog1
				    (car rest)
				  (setq rest (cdr rest))))))
     operation arg)))

;;;###autoload
(defun dired-do-rename-list (format list &optional arg)
  "Rename all marked (or next ARG) files using elements from LIST.
You are prompted for a format string, e.g \"x_%s\", and the list,
e.g. '(foo bar zod).  This example will rename the marked files to

    x_foo
    x_bar
    x_zod

It is an error if LIST has not as many elements as there are files."
  (interactive "sRename list (format using %%s): \nxList: \nP")
  (or (= (dired-number-of-marked-files arg)
	 (length list))
      (error "Must have as many elements as there are files to rename"))
  (dired-do-create-files-list
   (function dired-rename-file)
   "Rename-list" arg format list))

