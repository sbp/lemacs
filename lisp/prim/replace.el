;;; replace.el --- replace commands for Emacs.

;; Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.

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

;; This package supplies the string and regular-expression replace functions
;; documented in the Emacs user's manual.

;;; Code:

(defvar case-replace t "\
*Non-nil means query-replace should preserve case in replacements.")

(defvar query-replace-history nil)

(defun query-replace-read-args (string)
  (let (from to)
    (setq from (read-from-minibuffer (format (gettext "%s: ") string)
				     nil nil nil
				     'query-replace-history))
    (setq to (read-from-minibuffer (format (gettext "%s %s with: ") string from)
				   nil nil nil
				   'query-replace-history))
    (list from to current-prefix-arg)))

(defun query-replace (from-string to-string &optional arg)
  "Replace some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

Preserves case in each replacement if  case-replace  and  case-fold-search
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries.

To customize possible responses, change the \"bindings\" in `query-replace-map'."
  (interactive (query-replace-read-args (gettext "Query replace")))
  (perform-replace from-string to-string t nil arg)
  (or unread-command-event (message (gettext "Done"))))

(defun query-replace-regexp (regexp to-string &optional arg)
  "Replace some things after point matching REGEXP with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

Preserves case in each replacement if  case-replace  and  case-fold-search
are non-nil and REGEXP has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries.
In TO-STRING, \\& means insert what matched REGEXP,
and \\=\\<n> means insert what matched <n>th \\(...\\) in REGEXP."
  (interactive (query-replace-read-args (gettext "Query replace regexp")))
  (perform-replace regexp to-string t t arg)
  (or unread-command-event (message (gettext "Done"))))

;;>>> Not patently useful
(defun map-query-replace-regexp (regexp to-strings &optional arg)
  "Replace some matches for REGEXP with various strings, in rotation.
The second argument TO-STRINGS contains the replacement strings, separated
by spaces.  This command works like `query-replace-regexp' except
that each successive replacement uses the next successive replacement string,
wrapping around from the last such string to the first.

Non-interactively, TO-STRINGS may be a list of replacement strings.

A prefix argument N says to use each replacement string N times
before rotating to the next."
  (interactive
   (let (from to)
     (setq from (read-from-minibuffer (gettext "Map query replace (regexp): ")
				      nil nil nil
				      'query-replace-history))
     (setq to (read-from-minibuffer
	       (format (gettext "Query replace %s with (space-separated strings): ")
		       from)
	       nil nil nil
	       'query-replace-history))
     (list from to current-prefix-arg)))
  (let (replacements)
    (if (listp to-strings)
	(setq replacements to-strings)
      (while (/= (length to-strings) 0)
	(if (string-match " " to-strings)
	    (setq replacements
		  (append replacements
			  (list (substring to-strings 0
					   (string-match " " to-strings))))
		  to-strings (substring to-strings
				       (1+ (string-match " " to-strings))))
	  (setq replacements (append replacements (list to-strings))
		to-strings ""))))
   (perform-replace regexp replacements t t nil arg))
  (or unread-command-event (message (gettext "Done"))))

(defun replace-string (from-string to-string &optional delimited)
  "Replace occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (search-forward OLD-STRING nil t)
    (replace-match REPLACEMENT nil t))
which will run faster and will not set the mark or print anything."
  (interactive (query-replace-read-args (gettext "Replace string")))
  (perform-replace from-string to-string nil nil delimited)
  (or unread-command-event (message (gettext "Done"))))

(defun replace-regexp (regexp to-string &optional delimited)
  "Replace things after point matching REGEXP with TO-STRING.
Preserve case in each match if case-replace and case-fold-search
are non-nil and REGEXP has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries.
In TO-STRING, \\& means insert what matched REGEXP,
and \\=\\<n> means insert what matched <n>th \\(...\\) in REGEXP.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (re-search-forward REGEXP nil t)
    (replace-match REPLACEMENT nil nil))
which will run faster and will not set the mark or print anything."
  (interactive (query-replace-read-args (gettext "Replace regexp")))
  (perform-replace regexp to-string nil t delimited)
  (or unread-command-event (message (gettext "Done"))))


(defvar regexp-history nil
  "History list for some commands that read regular expressions.")

(define-function 'keep-lines 'delete-non-matching-lines)
(defun delete-non-matching-lines (regexp)
  "Delete all lines except those containing matches for REGEXP.
A match split across lines preserves all the lines it lies in.
Applies to all lines after point."
  (interactive (list (read-from-minibuffer
		      (gettext "Keep lines (containing match for regexp): ")
		      nil nil nil 'regexp-history)))
  (save-excursion
    (or (bolp) (forward-line 1))
    (let ((start (point)))
      (while (not (eobp))
	;; Start is first char not preserved by previous match.
	(if (not (re-search-forward regexp nil 'move))
	    (delete-region start (point-max))
	  (let ((end (save-excursion (goto-char (match-beginning 0))
				     (beginning-of-line)
				     (point))))
	    ;; Now end is first char preserved by the new match.
	    (if (< start end)
		(delete-region start end))))
	(setq start (save-excursion (forward-line 1)
				    (point)))
	;; If the match was empty, avoid matching again at same place.
	(and (not (eobp)) (= (match-beginning 0) (match-end 0))
	     (forward-char 1))))))

(define-function 'flush-lines 'delete-matching-lines)
(defun delete-matching-lines (regexp)
  "Delete lines containing matches for REGEXP.
If a match is split across lines, all the lines it lies in are deleted.
Applies to lines after point."
  (interactive (list (read-from-minibuffer
		      (gettext "Flush lines (containing match for regexp): ")
		      nil nil nil 'regexp-history)))
  (save-excursion
    (while (and (not (eobp))
		(re-search-forward regexp nil t))
      (delete-region (save-excursion (goto-char (match-beginning 0))
				     (beginning-of-line)
				     (point))
		     (progn (forward-line 1) (point))))))

(define-function 'how-many 'count-matches)
(defun count-matches (regexp)
  "Print number of matches for REGEXP following point."
  (interactive (list (read-from-minibuffer
		      (gettext "How many matches for (regexp): ")
		      nil nil nil 'regexp-history)))
  (let ((count 0) opoint)
    (save-excursion
     (while (and (not (eobp))
		 (progn (setq opoint (point))
			(re-search-forward regexp nil t)))
       (if (= opoint (point))
	   (forward-char 1)
	 (setq count (1+ count))))
     (message (gettext "%d occurrences") count))))


(defvar occur-mode-map ())
(if occur-mode-map
    ()
  (setq occur-mode-map (make-sparse-keymap))
  (set-keymap-name occur-mode-map 'occur-mode-map)
  (define-key occur-mode-map "\C-c\C-c" 'occur-mode-goto-occurrence)
  (define-key occur-mode-map 'button2 'occur-mouse-goto))

(defvar occur-buffer nil)
(defvar occur-nlines nil)
(defvar occur-pos-list nil)

(defun occur-mode ()
  "Major mode for output from \\[occur].
Move point to one of the occurrences in this buffer,
then use \\[occur-mode-goto-occurrence] to go to the same occurrence
in the buffer that the occurrences were found in.
\\{occur-mode-map}"
  (kill-all-local-variables)
  (use-local-map occur-mode-map)
  (setq major-mode 'occur-mode)
  (setq mode-name (gettext "Occur"))
  (make-local-variable 'occur-buffer)
  (make-local-variable 'occur-nlines)
  (make-local-variable 'occur-pos-list)
  (require 'mode-motion)
  (setq mode-motion-hook 'mode-motion-highlight-line)
  (run-hooks 'occur-mode-hook))

(defun occur-mouse-goto (e)
  (interactive "e")
  (mouse-set-point e)
  (occur-mode-goto-occurrence))

(defun occur-mode-goto-occurrence ()
  "Go to the line this occurrence was found in, in the buffer it was found in."
  (interactive)
  (if (or (null occur-buffer)
	  (null (buffer-name occur-buffer)))
      (progn
	(setq occur-buffer nil
	      occur-pos-list nil)
	(error (gettext "Buffer in which occurrences were found is deleted"))))
  (let* ((occur-number (save-excursion
			 (beginning-of-line)
			 (/ (1- (count-lines (point-min)
					     (save-excursion
					       (beginning-of-line)
					       (point))))
			    (cond ((< occur-nlines 0)
				   (- 2 occur-nlines))
				  ((> occur-nlines 0)
				   (+ 2 (* 2 occur-nlines)))
				  (t 1)))))
	 (pos (nth occur-number occur-pos-list)))
    (or pos
	(error (gettext "No occurrence on this line")))
    (pop-to-buffer occur-buffer)
    (goto-char (marker-position pos))))


(defvar list-matching-lines-default-context-lines 0
  "*Default number of context lines to include around a `list-matching-lines'
match.  A negative number means to include that many lines before the match.
A positive number means to include that many lines both before and after.")

;;>>> Should be named list-matching-lines-whole-buffer
;; lemacs addition
(defvar occur-whole-buffer nil
  "If t, occur operates on whole buffer, otherwise occur starts from point.
default is nil.")

(define-function 'occur 'list-matching-lines)
(defun list-matching-lines (regexp &optional nlines)
  "Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

If variable `occur-whole-buffer' is non-nil, the entire buffer is
searched, otherwise search begins at point.

Each line is displayed with NLINES lines before and after,
or -NLINES before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*Occur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  (interactive (list (let* ((default (car regexp-history))
			    (input 
			     (read-from-minibuffer
			      (if default
				  (format (gettext "List lines matching regexp (default `%s'): ") default)
                                  (gettext "List lines matching regexp: "))
			      nil nil nil
			      'regexp-history)))
		       (if (> (length input) 0) 
                           input
                           (setcar regexp-history default)))
		     current-prefix-arg))
  (setq nlines (if nlines (prefix-numeric-value nlines)
		 list-matching-lines-default-context-lines))
  (let ((first t)
	(buffer (current-buffer))
	(linenum 1)
	(prevpos (point-min))
        (final-context-start (make-marker)))
    (if (not occur-whole-buffer)
	(save-excursion
	  (beginning-of-line)
	  (setq linenum (1+ (count-lines (point-min) (point))))
	  (setq prevpos (point))))
    (with-output-to-temp-buffer (gettext "*Occur*")
      (save-excursion
	(set-buffer standard-output)
	(insert (format (gettext "Lines matching %s in buffer %s.\n")
			regexp (buffer-name buffer)))
	(occur-mode)
	(setq occur-buffer buffer)
	(setq occur-nlines nlines)
	(setq occur-pos-list ()))
      (if (eq buffer standard-output)
	  (goto-char (point-max)))
      (save-excursion
	(if occur-whole-buffer
	    (beginning-of-buffer))
	;; Find next match, but give up if prev match was at end of buffer.
	(while (and (not (= prevpos (point-max)))
		    (re-search-forward regexp nil t))
	  (goto-char (match-beginning 0))
	  (beginning-of-line)
	  (save-match-data
            (setq linenum (+ linenum (count-lines prevpos (point)))))
	  (setq prevpos (point))
	  (goto-char (match-end 0))
	  (let* ((start (save-excursion
			  (goto-char (match-beginning 0))
			  (forward-line (if (< nlines 0) nlines (- nlines)))
			  (point)))
		 (end (save-excursion
			(goto-char (match-end 0))
			(if (> nlines 0)
			    (forward-line (1+ nlines))
			    (forward-line 1))
			(point)))
		 (tag (format "%3d" linenum))
		 (empty (make-string (length tag) ?\ ))
		 tem)
	    (save-excursion
	      (setq tem (make-marker))
	      (set-marker tem (point))
	      (set-buffer standard-output)
	      (setq occur-pos-list (cons tem occur-pos-list))
	      (or first (zerop nlines)
		  (insert "--------\n"))
	      (setq first nil)
	      (insert-buffer-substring buffer start end)
	      (backward-char (- end start))
	      (setq tem (if (< nlines 0) (- nlines) nlines))
	      (while (> tem 0)
		(insert empty ?:)
		(forward-line 1)
		(setq tem (1- tem)))
	      (let ((this-linenum linenum))
		(set-marker final-context-start
			    (+ (point) (- (match-end 0) (match-beginning 0))))
		(while (< (point) final-context-start)
		  (if (null tag)
		      (setq tag (format "%3d" this-linenum)))
		  (insert tag ?:)
		  (setq tag nil)
		  (forward-line 1)
		  (setq this-linenum (1+ this-linenum))))
	      (while (< tem nlines)
		(insert empty ?:)
		(forward-line 1)
		(setq tem (1+ tem))))
	    (forward-line 1)))
	(set-buffer standard-output)
	;; Put positions in increasing order to go with buffer.
	(setq occur-pos-list (nreverse occur-pos-list))
	(if (interactive-p)
	    (message (gettext "%d matching lines.") (length occur-pos-list)))))))

;; It would be nice to use \\[...], but there is no reasonable way
;; to make that display both SPC and Y.
(defvar query-replace-help (purecopy
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
RET or `q' to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r to enter recursive edit (\\[exit-recursive-edit] to get out again),
C-w to delete match and recursive edit,
C-l to clear the screen, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions,
^ to move point back to previous match.")
  "Help message while in query-replace")

(defvar	query-replace-map nil
  "Keymap that defines the responses to questions in `query-replace'.
The \"bindings\" in this map are not commands; they are answers.
The valid answers include `act', `skip', `act-and-show',
`exit', `act-and-exit', `edit', `delete-and-edit', `recenter',
`automatic', `backup', and `help'.")

(if query-replace-map
    nil
    (let ((map (make-sparse-keymap)))
      (set-keymap-name map 'query-replace-map)
      (define-key map " " 'act)
      (define-key map "\d" 'skip)
      (define-key map [delete] 'skip)
      (define-key map [backspace] 'skip)
      (define-key map "y" 'act)
      (define-key map "n" 'skip)
      (define-key map "," 'act-and-show)
      (define-key map [escape] 'exit)
      (define-key map "q" 'exit)
      (define-key map [return] 'exit)
      (define-key map "." 'act-and-exit)
      (define-key map "\C-r" 'edit)
      (define-key map "\C-w" 'delete-and-edit)
      (define-key map "\C-l" 'recenter)
      (define-key map "!" 'automatic)
      (define-key map "^" 'backup)
      (define-key map "\C-h" 'help)
      (define-key map "?" 'help)
      (define-key map "\C-g" 'quit)
      (define-key map "\C-]" 'quit)
      
      (setq query-replace-map map)))


(autoload 'isearch-highlight "isearch")

(defun perform-replace-next-event (event)
  (if isearch-highlight
      (let ((aborted t))
	(unwind-protect
	    (progn
	      (isearch-highlight (match-beginning 0) (match-end 0))
	      (next-command-event event)
	      (setq aborted nil))
	  (isearch-dehighlight aborted)))
    (next-command-event event)))

(defun perform-replace (from-string replacements
		        query-flag regexp-flag delimited-flag
			&optional repeat-count map)
  "Subroutine of `query-replace'.  Its complexity handles interactive queries.
Don't use this in your own program unless you want to query and set the mark
just as `query-replace' does.  Instead, write a simple loop like this:
  (while (re-search-forward \"foo[ \t]+bar\" nil t)
    (replace-match \"foobar\" nil nil))
which will run faster and probably do exactly what you want."
  (or map (setq map query-replace-map))
  (let ((event (allocate-event))
	(nocasify (not (and case-fold-search case-replace
			    (string-equal from-string
					  (downcase from-string)))))
	(literal (not regexp-flag))
	(search-function (if regexp-flag 're-search-forward 'search-forward))
	(search-string from-string)
	(real-match-data nil)		; the match data for the current match
	(next-replacement nil)
	(replacement-index 0)
	(keep-going t)
	(stack nil)
	(next-rotate-count 0)
	(replace-count 0)
	(lastrepl nil)			;Position after last match considered.
	(match-again t))
    (if (stringp replacements)
	(setq next-replacement replacements)
      (or repeat-count (setq repeat-count 1)))
    (if delimited-flag
	(setq search-function 're-search-forward
	      search-string (concat "\\b"
				    (if regexp-flag from-string
				      (regexp-quote from-string))
				    "\\b")))
    (push-mark)
    (undo-boundary)
    ;; Loop finding occurrences that perhaps should be replaced.
    (while (and keep-going
		(not (eobp))
		(funcall search-function search-string nil t)
		;; If the search string matches immediately after
		;; the previous match, but it did not match there
		;; before the replacement was done, ignore the match.
		(if (or (eq lastrepl (point))
			(and regexp-flag
			     (eq lastrepl (match-beginning 0))
			     (not match-again)))

		    (if (eobp)
			nil
		      ;; Don't replace the null string 
		      ;; right after end of previous replacement.
		      (forward-char 1)
		      (funcall search-function search-string nil t))
		  t))
      ;; Save the data associated with the real match.
      (setq real-match-data (match-data))

      ;; Before we make the replacement, decide whether the search string
      ;; can match again just after this match.
      (if regexp-flag
	  (progn 
            (setq match-again (looking-at search-string))
            (store-match-data real-match-data)))

      ;; If time for a change, advance to next replacement string.
      (if (and (listp replacements)
	       (= next-rotate-count replace-count))
	  (progn
	    (setq next-rotate-count
		  (+ next-rotate-count repeat-count))
	    (setq next-replacement (nth replacement-index replacements))
	    (setq replacement-index (% (1+ replacement-index) (length replacements)))))
      (if (not query-flag)
	  (progn
	    (store-match-data real-match-data)
	    (replace-match next-replacement nocasify literal)
	    (setq replace-count (1+ replace-count)))
	(undo-boundary)
	(let ((help-form
	       '(concat (format (gettext "Query replacing %s%s with %s.\n\n")
				(if regexp-flag (gettext "regexp ") "")
				from-string next-replacement)
                           (substitute-command-keys query-replace-help)))
              (done nil)
              (replaced nil)
              def)
          ;; Loop reading commands until one of them sets done,
	  ;; which means it has finished handling this occurrence.
	  (while (not done)
            (message (gettext "Query replacing %s with %s: ")
                     from-string next-replacement)
            (perform-replace-next-event event)
            (setq def (lookup-key map (vector event)))
	    ;; Restore the match data while we process the command.
            (store-match-data real-match-data)
	    (cond ((eq def 'help)
		   (with-output-to-temp-buffer (gettext "*Help*")
		     (princ (concat
			     (format "Query replacing %s%s with %s.\n\n"
				     (if regexp-flag "regexp " "")
				     from-string next-replacement)
			      (substitute-command-keys
			       query-replace-help)))))
		  ((eq def 'exit)
		   (setq keep-going nil)
		   (setq done t))
		  ((eq def 'backup)
                   (if stack
                       (let ((elt (car stack)))
                         (goto-char (car elt))
                         (setq replaced (eq t (cdr elt)))
                         (or replaced
                             (store-match-data (cdr elt)))
                         (setq stack (cdr stack)))
                       (progn
			 (message "No previous match")
			 (ding 'no-terminate)
			 (sit-for 1))))
		  ((eq def 'act)
		   (or replaced
		       (replace-match next-replacement nocasify literal))
		   (setq done t replaced t))
		  ((eq def 'act-and-exit)
		   (or replaced
		       (replace-match next-replacement nocasify literal))
		   (setq keep-going nil)
		   (setq done t replaced t))
		  ((eq def 'act-and-show)
		   (if (not replaced)
		       (progn
			 (replace-match next-replacement nocasify literal)
			 (setq replaced t))))
		  ((eq def 'automatic)
		   (or replaced
		       (replace-match next-replacement nocasify literal))
		   (setq done t query-flag nil replaced t))
		  ((eq def 'skip)
		   (setq done t))
		  ((eq def 'recenter)
		   (recenter nil))
		  ((eq def 'edit)
		   (store-match-data
		    (prog1 (match-data)
		      (save-excursion (recursive-edit))))
		   ;; Before we make the replacement,
		   ;; decide whether the search string
		   ;; can match again just after this match.
		   (if regexp-flag
		       (setq match-again (looking-at search-string))))
		  ((eq def 'delete-and-edit)
		   (delete-region (match-beginning 0) (match-end 0))
		   (store-match-data (prog1 (match-data)
		      (save-excursion (recursive-edit))))
		   (setq replaced t))
		  (t
		   (setq keep-going nil)
		   (setq unread-command-event event)
		   (setq done t))))
	  ;; Record previous position for ^ when we move on.
	  ;; Change markers to numbers in the match data
	  ;; since lots of markers slow down editing.
	  (setq stack
		(cons (cons (point)
			    (or replaced
				(mapcar
				 #'(lambda (elt)
				     (if (markerp elt)
					 (marker-position elt)
				        elt))
				 (match-data))))
		      stack))
	  (if replaced (setq replace-count (1+ replace-count)))))
      (setq lastrepl (point)))
    (and keep-going stack)))

;;; replace.el ends here
