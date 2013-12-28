;;; Partial completion mechanism for GNU Emacs.  Version 2.00
;;; Copyright (C) 1990 Chris McConnell, ccm@cs.cmu.edu.

;;; This file is part of GNU Emacs.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; When loaded, this file extends the standard minibuffer completion
;;; mechanism so that it performs pattern matching completions.  The
;;; completion rules are: 
;;;
;;; 1) If what has been typed matches any possibility, do normal
;;; completion. 
;;;
;;; 2) Otherwise, generate a regular expression such that
;;; completer-delimiters delimit words and generate all possible
;;; matches.  Unless completer-exact is T, the match at any point will
;;; be the shortest string with the number of words closest to the
;;; number of words in the pattern.
;;;
;;; SPACE, TAB, LFD, RET, and ? do normal completion if possible
;;; otherwise they do partial completion.  In addition, C-DEL will
;;; undo the last partial expansion or contraction.  M-RET will always
;;; complete to the current match before returning.  This is useful
;;; when any string is possible, but you want to complete to a string
;;; as when calling find-file.  The bindings can be changed by using
;;; completer-load-hook.
;;;
;;; Examples:
;;; a-f     auto-fill-mode
;;; b--d    *beginning-of-defun or byte-recompile-directory
;;; ~/i.e   *~/ilisp.el or ~/il-el.el or ~/ilisp.elc
;;;
;;; Initially completer-delimiters are "-" and "."
;;;
(defvar completer-load-hook nil
  "Hook called when minibuffer partial completion is loaded.")

(defvar completer-disable nil
  "*If T, turn off partial completion.  Use the command
\\[completer-toggle] to set this.")

(defvar completer-delimiters "-." 
  "*Delimiters used in partial completions.")

(defvar completer-exact nil
  "*If T, then you must have an exact match.  Otherwise, the shortest
string that matches the pattern will be used.")

(defvar completer-last-pattern ""
  "The last pattern expanded.")

;;;
(defun temp-minibuffer-message (m)
  "A Lisp version of temp_minibuffer_message from minibuf.c."
  (let ((savemax (point-max)))
    (save-excursion
      (goto-char (point-max))
      (insert m))
    (let ((inhibit-quit t))
      (sit-for 2)
      (delete-region savemax (point-max))
      (if quit-flag
	  (setq quit-flag nil
		unread-command-char 7)))))

;;;
(defun completer-regexp (string delimiters)
  "Convert STRING into a regexp with words delimited by characters in
DELIMITERS." 
  (let* ((delimiters (concat "[" delimiters "]"))
	 (limit (length string))
	 (pos 0)
	 (regexp "^"))
    (while (and (< pos limit) (string-match delimiters string pos))
      (let* ((begin (match-beginning 0))
	     (end (match-end 0))
	     (delimiter (substring string begin end)))
	(setq regexp 
	      (format "%s%s[^%s]*\\%s" 
		      regexp
		      (regexp-quote (substring string pos begin))
		      delimiter
		      delimiter))
	(setq pos end)))
    (if (<= pos limit)
	(setq regexp (concat regexp 
			     (regexp-quote (substring string pos limit)))))))

;;;
(defun completer-words (regexp string &optional limit)
  "Return the number of words matching REGEXP in STRING up to LIMIT."
  (setq limit (or limit 1000))
  (let ((count 0)
	(pos 0))
    (while (and (string-match regexp string pos) (<= count limit))
      (setq count (1+ count)
	    pos (match-end 0)))
    count))

;;;
(defun completer-match (string choices delimiters)
  "Return the best match of STRING in CHOICES and T if the best match
is the only one with the right number of words.  The best match has
the same number of words by DELIMITERS as string if possible and is
the shortest possibility within the rest."
  (or (if (null (cdr choices)) (cons (car choices) t))
      (let* ((regexp (concat "[^" delimiters "]*[" delimiters "]"))
	     (words (completer-words regexp string))
	     (choice choices)
	     (unique-p nil)
	     (match nil)
	     (match-count nil)
	     (match-len 1000))
	(while choice
	  (let* ((current (car choice))
		 (length (length current)))
	    (if match-count
		(if (= (completer-words regexp current words) words)
		    (setq unique-p nil choice nil)
		    (if (< length match-len)
			(setq match current
			      match-len length)))
		(if (= (completer-words regexp current words) words)
		    (setq match current
			  match-len length
			  match-count t
			  unique-p t)
		    (if (< length match-len)
			(setq match current
			      match-len length)))))
	  (setq choice (cdr choice)))
	(cons match unique-p))))

;;;
(defun completer-completions (string table pred)
  "Return all possible completions for STRING in the elements of TABLE
that pass PRED.  Also handle filename completion magic."
  (let ((choices (all-completions string table pred)))
    (if (eq table 'read-file-name-internal)
	(let ((dir (file-name-directory string)))
	  (setq choices (mapcar (function (lambda (n) (concat dir n)))
				choices))))
    choices))

;;;
(defun completer (string table pred delimiters)
  "Return (match common-substring matches unique-p) for STRING in TABLE
for choices that pass PRED using DELIMITERS to delimit words."
  (let* ((case-fold-search completion-ignore-case)
	 (has-delimiters (string-match 
			  (concat "[" completer-delimiters "]")
			  string)))
    (if has-delimiters
	(let* ((choices
		(completer-completions 
		 (substring string 0 (match-beginning 0))
		 table pred))
	       (regexp (completer-regexp string delimiters))
	       (choicep choices)
	       (matches nil))
	  (while choicep
	    (let ((choice (car choicep)))
	      (if (string-match regexp choice)
		  (setq matches (cons choice matches)))
	      (setq choicep (cdr choicep))))
	  (if (cdr matches)
	      (let* ((match (if (not completer-exact)
				(completer-match string matches delimiters))))
		(list (if match (car match))
		      (try-completion "" (mapcar 'list matches)) 
		      matches (if match (cdr match))))
	      (if matches 
		  (list (car matches) nil matches t)
		  '(nil nil nil nil))))
	'(nil nil nil nil))))

;;;
(defun completer-display-choices (choices &optional match message)
  "Display the list of possible CHOICES.  If MATCH is non-nil, that
string will be marked with a *.  If there are no choices, display
MESSAGE."
  (if choices
      (with-output-to-temp-buffer " *Completions*"
	(display-completion-list
	 (if match
	     (sort (cons (concat "*" match) (delq match choices))
		   'string-lessp)
	     choices)))
      (beep)
      (temp-minibuffer-message (or message " [No completions]"))))

;;;
(defun completer-insert (match)
  "Insert MATCH into the buffer saving the current contents so that it
can be undone with completer-undo."
  (setq completer-last-pattern (buffer-substring (point-min) (point-max)))
  (delete-region (point-min) (point-max))
  (insert match))

;;;
(defun completer-undo ()
  "Swap the last expansion and the last partial regular expression."
  (interactive)
  (completer-insert completer-last-pattern))

;;;
(defun completer-toggle ()
  "Turn partial completion on or off."
  (interactive)
  (setq completer-disable (not completer-disable))
  (message (if completer-disable 
	       "Partial completion OFF"
	       "Partial completion ON")))

;;;
;;; Minibuffer specific code
;;;
(defun completer-new-cmd (cmd)
  "Return T if we can't execute the old minibuffer version of CMD."
  (if (or completer-disable
	  (not (string-match
		(concat "[" (regexp-quote completer-delimiters) "]")
		(buffer-substring (point-min) (point-max))))
	  (try-completion (buffer-substring (point-min) (point-max))
			  minibuffer-completion-table
			  minibuffer-completion-predicate))
      (progn
	(funcall cmd)
	nil)
      t))

;;;
(defun completer-minibuf ()
  "Partial completion of minibuffer expressions.

If what has been typed so far matches any possibility normal
completion will be done.  Otherwise, the string is considered to be a
pattern with words delimited by the characters in
completer-delimiters.  If completer-exact is T, the best match will be
the shortest one with the same number of words as the pattern if
possible and otherwise the shortest matching expression.  If
completions are displayed, the best match will be displayed with a *.

Examples:
a-f     auto-fill-mode
r-e     rmail-expunge
b--d    *begining-of-defun or byte-recompile-directory
~/i.e   *~/ilisp.el or ~/il-el.el or ~/ilisp.elc"
  (interactive)
  (completer (buffer-substring (point-min) (point-max))
	     minibuffer-completion-table
	     minibuffer-completion-predicate
	     (regexp-quote completer-delimiters)))

;;;
(defun completer-goto (match lcs choices unique &optional no-insert)
  "MATCH is the best match, LCS is the longest common substring of all
of the matches.  CHOICES is a list of the possibilities, UNIQUE
indicates if MATCH is unique.  Insert the match if unique and the lcs
otherwise unless optional NO-INSERT is T.  Then go to the part of the
string in the minibuffer that disambiguates choices and display the
possibilities if the string was not extended."
  (let* ((string (buffer-substring (point-min) (point-max)))
	 (new (not (string= string lcs))))
    (if unique
	(if no-insert
	    (completer-display-choices choices match)
	    (if match
		(if (string= string match)
		    (temp-minibuffer-message " [Sole completion]")
		    (completer-insert match))
		(beep)
		(temp-minibuffer-message " [No match]")))
	(if (and choices new)
	    (let* ((regexp 
		    (concat "[" (regexp-quote completer-delimiters) "]"))
		   (words (1+ (completer-words regexp lcs)))
		   point)
	      ;; Go to where its ambiguous
	      (goto-char (point-min))
	      (insert lcs)
	      (setq start (point))
	      (re-search-forward regexp nil 'move words)
	      (if (not (eobp)) (forward-char -1))
	      (delete-region start (point))))
	(if choices
	    (if (or no-insert (not new))
		(completer-display-choices choices match))
	    (beep)
	    (temp-minibuffer-message " [No match]")))))
;;;
(defvar completer-old-help
  (lookup-key minibuffer-local-must-match-map "?")
  "Old binding of ? in minibuffer completion map.")
(defun completer-help ()
  "Partial completion minibuffer-completion-help.  
See completer-minibuf for more information."
  (interactive)
  (if (completer-new-cmd completer-old-help)
      (apply 'completer-goto (append (completer-minibuf) (list t)))))

;;;
(defvar completer-old-completer
  (lookup-key minibuffer-local-must-match-map "\t")
  "Old binding of TAB in minibuffer completion map.")
(defun completer-complete ()
  "Partial completion minibuffer-complete.
See completer-minibuf for more information."
  (interactive)
  (if (completer-new-cmd completer-old-completer)
      (apply 'completer-goto (completer-minibuf))))

;;;
(defvar completer-old-word
  (lookup-key minibuffer-local-must-match-map " ")
  "Old binding of SPACE in minibuffer completion map.")
(defun completer-word ()
  "Partial completion minibuffer-complete.
See completer-minibuf for more information."
  (interactive)
  (if (completer-new-cmd completer-old-word)
      (apply 'completer-goto (completer-minibuf))))

;;; 
(defvar completer-old-exit
  (lookup-key minibuffer-local-must-match-map "\n")
  "Old binding of RET in minibuffer completion map.")
(defun completer-exit (&optional short)
  "Partial completion minibuffer-complete-and-exit.
See completer-minibuf for more information."
  (interactive)
  (if (or short (completer-new-cmd completer-old-exit))
      (let* ((completions (completer-minibuf))
	     (match (car completions))
	     (unique-p (car (cdr (cdr (cdr completions))))))
	(apply 'completer-goto completions)
	(if unique-p 
	    (exit-minibuffer)
	    (if (and match short)
		(progn (completer-insert match)
		       (if minibuffer-completion-confirm
			   (temp-minibuffer-message " [Confirm]")
			   (exit-minibuffer))))))))

;;;
(defun completer-match-exit ()
  "Exit the minibuffer with the current best match."
  (interactive)
  (completer-exit t))

;;; Keymaps
(define-key minibuffer-local-completion-map "\C-_"  'completer-undo)
(define-key minibuffer-local-completion-map "\t"    'completer-complete)
(define-key minibuffer-local-completion-map " "     'completer-word)
(define-key minibuffer-local-completion-map "?"     'completer-help)
(define-key minibuffer-local-completion-map "\M-\n" 'completer-match-exit)
(define-key minibuffer-local-completion-map "\M-\r" 'completer-match-exit)

(define-key minibuffer-local-must-match-map "\C-_"  'completer-undo)
(define-key minibuffer-local-must-match-map "\t"    'completer-complete)
(define-key minibuffer-local-must-match-map " "     'completer-word)
(define-key minibuffer-local-must-match-map "\n"    'completer-exit)
(define-key minibuffer-local-must-match-map "\r"    'completer-exit)
(define-key minibuffer-local-must-match-map "?"     'completer-help)
(define-key minibuffer-local-must-match-map "\M-\n" 'completer-match-exit)
(define-key minibuffer-local-must-match-map "\M-\r" 'completer-match-exit)

(provide 'completer)
(run-hooks 'completer-load-hook)
