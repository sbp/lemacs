;;; find-dired.el -- Run a `find' command and dired the output
;;; Copyright (C) 1991 Roland McGrath

(defconst find-dired-version (substring "$Revision: 1.14 $" 11 -2)
  "$Id: find-dired.el,v 1.14 1992/05/20 05:28:46 jwz Exp $")

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to roland@ai.mit.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;; LISPDIR ENTRY for the Elisp Archive ===============================
;;    LCD Archive Entry:
;;    find-dired|Roland McGrath, Sebastian Kremer
;;    |roland@gnu.ai.mit.edu, sk@thp.uni-koeln.de
;;    |Run a `find' command and dired the output
;;    |$Date: 1992/05/20 05:28:46 $|$Revision: 1.14 $|

;; INSTALLATION ======================================================

;; To use this file, byte-compile it, install it somewhere in your
;; load-path, and put:

;;   (autoload 'find-dired "find-dired" nil t)
;;   (autoload 'find-name-dired "find-dired" nil t)
;;   (autoload 'find-grep-dired "find-dired" nil t)

;; in your ~/.emacs, or site-init.el, etc.

;; To bind it to a key, put, e.g.:
;;
;;   (global-set-key "\C-cf" 'find-dired)
;;   (global-set-key "\C-cn" 'find-name-dired)
;;   (global-set-key "\C-cl" 'find-grep-dired)
;;
;; in your ~/.emacs.

(require 'dired)
(provide 'find-dired)

;;;###autoload
(defvar find-ls-option (if (eq system-type 'berkeley-unix) "-ls"
			 "-exec ls -ldi {} \\;")
  "*Option to `find' to produce an `ls -l'-type listing.")

;;;###autoload
(defvar find-grep-options (if (eq system-type 'berkeley-unix) "-s" "-l")
  "*Option to grep to be as silent as possible.
On Berkeley systems, this is `-s', for others it seems impossible to
suppress all output, so `-l' is used to print nothing more than the
file name.")

(defvar find-args nil
  "Last arguments given to `find' by \\[find-dired].")

;;;###autoload
(defun find-dired (dir args)
  "Run `find' and go into dired-mode on a buffer of the output.
The command run (after changing into DIR) is

    find . \\( ARGS \\) -ls"
  (interactive (list (read-file-name "Run find in directory: " nil "" t)
		     (if (featurep 'gmhist)
			 (read-with-history-in 'find-args-history
					       "Run find (with args): ")
		       (read-string "Run find (with args): " find-args))))
  ;; Expand DIR ("" means default-directory), and make sure it has a
  ;; trailing slash.
  (setq dir (file-name-as-directory (expand-file-name dir)))
  ;; Check that it's really a directory.
  (or (file-directory-p dir)
      (error "find-dired needs a directory: %s" dir))
  (switch-to-buffer (get-buffer-create "*Find*"))
  (widen)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq default-directory dir
	find-args args			; save for next interactive call
	args (concat "find . "
		     (if (string= args "")
			 ""
		       (concat "\\( " args " \\) "))
		     find-ls-option))
  ;; The next statement will bomb in classic dired (no optional arg allowed)
  ;; find(1)'s -ls corresponds to these switches.
  ;; Note -b, at least GNU find quotes spaces etc. in filenames
  (dired-mode dir "-gilsb")
  ;; Set subdir-alist so that Tree Dired will work:
  (if (fboundp 'dired-simple-subdir-alist)
      ;; will work even with nested dired format (dired-nstd.el,v 1.15
      ;; and later)
      (dired-simple-subdir-alist)
    ;; else we have an ancient tree dired (or classic dired, where
    ;; this does no harm) 
    (set (make-local-variable 'dired-subdir-alist)
	 (list (cons default-directory (point-min-marker)))))
  (setq buffer-read-only nil)
  ;; Subdir headlerline must come first because the first marker in
  ;; subdir-alist points there.
  (insert "  " dir ":\n")
  ;; Make second line a ``find'' line in analogy to the ``total'' or
  ;; ``wildcard'' line. 
  (insert "  " args "\n")
  ;; Start the find process
  (set-process-filter (start-process-shell-command "find"
						   (current-buffer) args)
		      (function find-dired-filter))
  (set-process-sentinel (get-buffer-process (current-buffer))
			(function find-dired-sentinel))
  (setq mode-line-process '(": %s")))

;;;###autoload
(defun find-name-dired (dir pattern)
  "Search DIR recursively for files matching the globbing pattern PATTERN,
and run dired on those files.
PATTERN is a shell wildcard (not an Emacs regexp) and need not be quoted.
The command run (after changing into DIR) is

    find . -name 'PATTERN' -ls"
  (interactive
   "DFind-name (directory): \nsFind-name (filename wildcard): ")
  (find-dired dir (concat "-name '" pattern "'")))

;; This functionality suggested by
;; From: oblanc@watcgl.waterloo.edu (Olivier Blanc)
;; Subject: find-dired, lookfor-dired
;; Date: 10 May 91 17:50:00 GMT
;; Organization: University of Waterloo

(fset 'lookfor-dired 'find-grep-dired)
;;;###autoload
(defun find-grep-dired (dir args)
  "Find files in DIR containing a regexp ARG and start Dired on output.
The command run (after changing into DIR) is

    find . -exec grep -s ARG {} \\\; -ls

Thus ARG can also contain additional grep options."
  (interactive "DFind-grep (directory): \nsFind-grep (grep args): ")
  ;; find -exec doesn't allow shell i/o redirections in the command,
  ;; or we could use `grep -l >/dev/null'
  (find-dired dir
	      (concat "-exec grep " find-grep-options " " args " {} \\\; ")))

(defun find-dired-filter (proc string)
  ;; Filter for \\[find-dired] processes.
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)		; not killed?
	(save-excursion
	  (set-buffer buf)
	  (save-restriction
	    (widen)
	    (save-excursion
	      (let ((buffer-read-only nil)
		    (end (point-max)))
		(goto-char end)
		(insert string)
		(goto-char end)
		(or (looking-at "^")
		    (forward-line 1))
		(while (looking-at "^")
		  (insert "  ")
		  (forward-line 1))
		;; Convert ` ./FILE' to ` FILE'
		;; This would lose if the current chunk of output
		;; starts or ends within the ` ./', so backup up a bit:
		(goto-char (- end 3))	; no error if < 0
		(while (search-forward " ./" nil t)
		  (delete-region (point) (- (point) 2)))))))
      ;; The buffer has been killed.
      (delete-process proc))))

(defun find-dired-sentinel (proc state)
  ;; Sentinel for \\[find-dired] processes.
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)
	(save-excursion
	  (set-buffer buf)
	  (setq mode-line-process nil)
	  (message "find-dired %s finished." (current-buffer))))))

(or (fboundp 'start-process-shell-command)
    ;; From version 19 subr.el.
(defun start-process-shell-command (name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it.
Args are NAME BUFFER COMMAND &rest COMMAND-ARGS.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is command name, the name of a shell command.
Remaining arguments are the arguments for the command.
Wildcards and redirection are handle as usual in the shell."
  (if (eq system-type 'vax-vms)
      (apply 'start-process name buffer args)
    (start-process name buffer shell-file-name "-c"
		   (concat "exec " (mapconcat 'identity args " "))))))

