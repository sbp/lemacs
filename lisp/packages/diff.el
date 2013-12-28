;; "DIFF" mode for handling output from unix diff utility.
;; Copyright (C) 1990 Free Software Foundation, Inc.
;; Written sunpitt!wpmstr!fbresz@Sun.COM 1/27/89

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

;; Initialize the keymap if it isn't already
(if (boundp 'diff-mode-map)
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

(defun diff (old new)
  "Find and display the differences between OLD and NEW files.
Interactively you are prompted with the current buffer's file name for NEW
and what appears to be it's backup for OLD."
  (interactive
   (let (oldf newf)
     (reverse
      (list
       (setq newf (buffer-file-name)
	     newf (if (and newf (file-exists-p newf))
			  (read-file-name
			   (concat "Diff new file: ("
				   (file-name-nondirectory newf) ") ")
			   nil newf t)
			(read-file-name "Diff new file: " nil nil t)))
       (setq oldf (file-newest-backup newf)
	     oldf (if (and oldf (file-exists-p oldf))
			  (read-file-name
			   (concat "Diff original file: ("
				   (file-name-nondirectory oldf) ") ")
			   (file-name-directory oldf) oldf t)
			(read-file-name "Diff original file: "
					(file-name-directory newf) nil t)))))))
  (message "Comparing files %s %s..." new old)
  (setq new (expand-file-name new)
	old (expand-file-name old))
  (let ((buffer-read-only nil)
	(sw diff-switches))
    (with-output-to-temp-buffer "*Diff Output*"
      (buffer-disable-undo standard-output)
      (save-excursion
	(set-buffer standard-output)
	(erase-buffer)
	(apply 'call-process "diff" nil t nil
	       (append diff-switches (list old new)))))
    (set-buffer "*Diff Output*")
    (goto-char (point-min))
    (while sw
      (if (string= (car sw) "-c")
	  ;; strip leading filenames from context diffs
	  (progn (forward-line 2) (delete-region (point-min) (point))))
      (setq sw (cdr sw))))
  (diff-mode)
  (if (string= "0" diff-total-differences)
      (insert (message "There are no differences."))
    (narrow-to-region (point) (progn
				(forward-line 1)
				(re-search-forward diff-search-pattern nil)
				(goto-char (match-beginning 0))))
    (setq diff-current-difference "1")))

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
				  (re-search-forward diff-search-pattern nil)
				  (goto-char (match-beginning 0))))
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
