;; Id: diff.el,v 1.3 1992/08/26 21:06:27 tfb Exp 
;; "DIFF" mode for handling output from unix diff utility.
;; Copyright (C) 1990 Free Software Foundation, Inc.
;; Written sunpitt!wpmstr!fbresz@Sun.COM 1/27/89
;; hacked on by tfb@aisb.ed.ac.uk (Tim Bradshaw)

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

;; todo: diff-switches flexibility:
;; (defconst diff-switches-function
;;   '(lambda (file)
;;     (if (string-match "\\.el$" file)
;; 	 "-c -F\"^(\""
;;       "-p"))
;;  "Function to return switches to pass to the `diff' utility, in \\[diff].
;; This function is called with one arg, a file name, and returns a string
;; containing 0 or more arguments which are passed on to `diff'.
;; NOTE: This is not an ordinary hook; it may not be a list of functions.")

;;; moved to loaddefs.el
;(defvar diff-switches nil
;  "*A list of switches to pass to the diff program.")

(defvar diff-search-pattern "^\\([0-9]\\|\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\)"
  "Regular expression that delineates difference regions in diffs.")

(defvar diff-mode-map nil)
(defvar diff-total-differences)
(defvar diff-current-difference)

;; Initialize the keymap if it isn't already
(if diff-mode-map
    nil
  (setq diff-mode-map (make-keymap))
  (suppress-keymap diff-mode-map)
  (define-key diff-mode-map "?" 'describe-mode)
  (define-key diff-mode-map "." 'diff-beginning-of-diff)
  (define-key diff-mode-map " " 'scroll-up)
  (define-key diff-mode-map "\177" 'scroll-down)
  (define-key diff-mode-map "n" 'diff-next-difference)
  (define-key diff-mode-map "p" 'diff-previous-difference)
  (define-key diff-mode-map "j" 'diff-show-difference))

(defun diff (old new &optional switches)
  "Find and display the differences between OLD and NEW files.
Interactively you are prompted with the current buffer's file name for NEW
and what appears to be its backup for OLD."
  (interactive (diff-read-args "Diff original file (%s) "
			       "Diff new file (%s) "
			       "Switches for diff (%s) " 
			       (buffer-file-name)))
  (setq switches (diff-fix-switches (or switches diff-switches)))
  (message "Comparing files %s %s..." new old)
  (setq new (expand-file-name new)
	old (expand-file-name old))
  (let ((buffer-read-only nil))
    (with-output-to-temp-buffer "*Diff Output*"
      (buffer-disable-undo standard-output)
      (save-excursion
	(set-buffer standard-output)
	(erase-buffer)
	(apply 'call-process "diff" nil t nil
	       (append switches (list old new)))))
    (set-buffer "*Diff Output*")
    (goto-char (point-min))
    (while switches
      (if (string= (car switches) "-c")
	  ;; strip leading filenames from context diffs
	  (progn (forward-line 2) (delete-region (point-min) (point))))
      (setq switches (cdr switches))))
  (diff-mode)
  (if (string= "0" diff-total-differences)
      (message "There are no differences.")
    (narrow-to-region (point) (progn
				(forward-line 1)
				(if 
				    (re-search-forward diff-search-pattern nil 'move)
				    (goto-char (match-beginning 0))
				  (point))))
				  
    (setq diff-current-difference "1")))

;;; arg reading from Dired originally
(defun diff-read-args (oldprompt newprompt switchprompt 
				 &optional file-for-backup)
  ;; Grab the args for diff.  OLDPROMPT and NEWPROMPT are the prompts
  ;; for the old & new filenames, SWITCHPROMPT for the list of
  ;; switches.  If FILE_FOR_BACKUP is provided (it must be a string if
  ;; so), then it will be used to try & work out a file & backup to
  ;; diff, & in this case the prompting order is backwards.  %s in a
  ;; prompt has a guess substituted into it.  This is nasty.
  (let (oldf newf)
    (if file-for-backup
	(setq newf file-for-backup
	      newf (if (and newf (file-exists-p newf))
		       (read-file-name 
			(format newprompt (file-name-nondirectory newf))
			nil newf t)
		     (read-file-name (format newprompt "") nil nil t))
	      oldf (file-newest-backup newf)
	      oldf (if (and oldf (file-exists-p oldf))
		       (read-file-name 
			(format oldprompt (file-name-nondirectory oldf)) 
			nil oldf t)
		     (read-file-name (format oldprompt "") 
				     (file-name-directory newf) nil t)))
      ;; Else we aren't trying to be bright...
      (setq oldf (read-file-name (format oldprompt "") nil nil t)
	    newf (read-file-name 
		  (format newprompt (file-name-nondirectory oldf))
		  nil (file-name-directory oldf) t)))
	(list oldf newf (diff-read-switches switchprompt))))

(defun diff-read-switches (switchprompt)
  ;; Read and return a list of switches
  (if current-prefix-arg
      (let ((default (mapconcat 'identity diff-switches " ")))
	(diff-fix-switches
	 (read-string (format switchprompt default) default)))))

(defun diff-fix-switches (switch-spec)
  ;; Parse a string into a list of switches or leave it be if it's 
  ;; not a string
  (if (stringp switch-spec)
      (let (result (start 0))
	(while (string-match "\\(\\S-+\\)" switch-spec start)
	  (setq result (cons (substring switch-spec (match-beginning 1)
					(match-end 1))
			     result)
		start (match-end 0)))
	(nreverse result))
    switch-spec))


;; Take a buffer full of Unix diff output and go into a mode to easily 
;; see the next and previous difference
(defun diff-mode ()
  "Diff Mode is used by \\[diff] for perusing the output from the diff program.
All normal editing commands are turned off.  Instead, these are available:
\\<diff-mode-map>
\\[diff-beginning-of-diff]	Move point to start of this difference.
\\[scroll-up]	Scroll to next screen of this difference.
\\[scroll-down]	Scroll to previous screen of this difference.
\\[diff-next-difference]	Move to Next Difference.
\\[diff-previous-difference]	Move to Previous Difference.
\\[diff-show-difference]	Jump to difference specified by numeric position.
"
  (interactive)
  (use-local-map diff-mode-map)
  (setq buffer-read-only t
	major-mode 'diff-mode
	mode-name "Diff"
	mode-line-modified "--- "
	mode-line-process
	'(" " diff-current-difference "/" diff-total-differences))
  (make-local-variable 'diff-current-difference)
  (set (make-local-variable 'diff-total-differences)
       (int-to-string (diff-count-differences))))

(defun diff-next-difference (n)
  "In diff-mode go the the beginning of the next difference as delimited
by diff-search-pattern."
  (interactive "p")
  (if (< n 0) (diff-previous-difference (- n))
    (if (zerop n) ()
      (goto-char (point-min))
      (forward-line 1) ; to get past the match for the start of this diff
      (widen)
      (if (re-search-forward diff-search-pattern nil 'move n)
	  (let ((start (goto-char (match-beginning 0))))
	    (forward-line 1)
	    (if (re-search-forward diff-search-pattern nil 'move)
		(goto-char (match-beginning 0)))
	    (narrow-to-region start (point))
	    (setq diff-current-difference
		  (int-to-string (+ n (string-to-int
				       diff-current-difference)))))
	(re-search-backward diff-search-pattern nil)
	(narrow-to-region (point) (point-max))
	(message "No following differences.")
	(setq diff-current-difference diff-total-differences))
      (goto-char (point-min)))))
      
(defun diff-previous-difference (n)
  "In diff-mode go the the beginning of the previous difference as delimited
by diff-search-pattern."
  (interactive "p")
  (if (< n 0) (diff-next-difference (- n))
    (if (zerop n) ()
      (goto-char (point-min))
      (widen)
      (if (re-search-backward diff-search-pattern nil 'move n)
	  (setq diff-current-difference
		(int-to-string (- (string-to-int diff-current-difference) n)))
	(message "No previous differences.")
	(setq diff-current-difference "1"))
      (narrow-to-region (point) (progn
				  (forward-line 1)
				  (if
				      (re-search-forward diff-search-pattern nil 'move)
				      (goto-char (match-beginning 0))
				    (point))))
      (goto-char (point-min)))))

(defun diff-show-difference (n)
  "Show difference number N (prefix argument)."
  (interactive "p")
  (let ((cur (string-to-int diff-current-difference)))
    (cond ((or (= n cur)
	       (zerop n)
	       (not (natnump n))) ; should signal an error perhaps.
	   ;; just redisplay.
	   (goto-char (point-min)))
	  ((< n cur)
	   (diff-previous-difference (- cur n)))
	  ((> n cur)
	   (diff-next-difference (- n cur))))))

(defun diff-beginning-of-diff ()
  "Goto beginning of current difference."
  (interactive)
  (goto-char (point-min)))

;; This function counts up the number of differences in the buffer.
(defun diff-count-differences ()
  "Count number of differences in the current buffer."
  (message "Counting differences...")
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((cnt 0))
	(while (re-search-forward diff-search-pattern nil t)
	  (setq cnt (1+ cnt)))
	(message "Counting differences...done (%d)" cnt)
	cnt))))
