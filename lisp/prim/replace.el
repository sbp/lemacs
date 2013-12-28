;; Replace commands for Emacs.
;; Copyright (C) 1985-1993 Free Software Foundation, Inc.

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


(defvar minibuffer-regexp-history nil)

(fset 'delete-non-matching-lines 'keep-lines)
(defun keep-lines (regexp)
  "Delete all lines except those containing matches for REGEXP.
A match split across lines preserves all the lines it lies in.
Applies to all lines after point."
;;  (interactive "sKeep lines (containing match for regexp): ")
  (interactive (list (read-string "Keep lines (containing match for regexp): "
				  nil 'minibuffer-regexp-history)))
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

(fset 'delete-matching-lines 'flush-lines)
(defun flush-lines (regexp)
  "Delete lines containing matches for REGEXP.
If a match is split across lines, all the lines it lies in are deleted.
Applies to lines after point."
;;  (interactive "sFlush lines (containing match for regexp): ")
  (interactive (list (read-string "Flush lines (containing match for regexp): "
				  nil 'minibuffer-regexp-history)))
  (save-excursion
    (while (and (not (eobp))
		(re-search-forward regexp nil t))
      (delete-region (save-excursion (goto-char (match-beginning 0))
				     (beginning-of-line)
				     (point))
		     (progn (forward-line 1) (point))))))

(fset 'count-matches 'how-many)
(defun how-many (regexp)
  "Print number of matches for REGEXP following point."
;;  (interactive "sHow many matches for (regexp): ")
  (interactive (read-string "How many matches for (regexp): "
			    nil 'minibuffer-regexp-history))
  (let ((count 0) opoint)
    (save-excursion
     (while (and (not (eobp))
		 (progn (setq opoint (point))
			(re-search-forward regexp nil t)))
       (if (= opoint (point))
	   (forward-char 1)
	 (setq count (1+ count))))
     (message "%d occurrences" count))))

(defvar occur-mode-map ())
(if occur-mode-map
    ()
  (setq occur-mode-map (make-sparse-keymap))
  (define-key occur-mode-map "\C-c\C-c" 'occur-mode-goto-occurrence))

(defvar occur-buffer nil)
(defvar occur-nlines nil)
(defvar occur-pos-list nil)
(defvar occur-last-string "")

(defun occur-mode ()
  "Major mode for output from \\[occur].
Move point to one of the occurrences in this buffer,
then use \\[occur-mode-goto-occurrence] to go to the same occurrence
in the buffer that the occurrences were found in.
\\{occur-mode-map}"
  (kill-all-local-variables)
  (use-local-map occur-mode-map)
  (setq major-mode 'occur-mode)
  (setq mode-name "Occur")
  (make-local-variable 'occur-buffer)
  (make-local-variable 'occur-nlines)
  (make-local-variable 'occur-pos-list))

(defun occur-mode-goto-occurrence ()
  "Go to the line this occurrence was found in, in the buffer it was found in."
  (interactive)
  (if (or (null occur-buffer)
	  (null (buffer-name occur-buffer)))
      (progn
	(setq occur-buffer nil
	      occur-pos-list nil)
	(error "Buffer in which occurrences were found is deleted")))
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
    (pop-to-buffer occur-buffer)
    (goto-char (marker-position pos))))

(defvar list-matching-lines-default-context-lines 0
  "*Default number of context lines to include around a list-matching-lines
match.  A negative number means to include that many lines before the match.
A positive number means to include that many lines both before and after.")

(defvar occur-whole-buffer nil
  "If t, occur operates on whole buffer, otherwise occur starts from point.
default is nil.")

(fset 'list-matching-lines 'occur)

(defun occur (regexp &optional nlines)
  "Show lines containing a match for REGEXP.  If the global variable
occur-whole-buffer is non-nil, the entire buffer is searched, otherwise
search begins at point.  Interactively, REGEXP defaults to the last REGEXP
used interactively.

Each line is displayed with NLINES lines before and after,
or -NLINES before if NLINES is negative.
NLINES defaults to list-matching-lines-default-context-lines.
Interactively it is the prefix arg.

The lines are shown in a buffer named *Occur*.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  (interactive (list (setq occur-last-string
			   (read-string "List lines matching regexp: "
					occur-last-string
					'minibuffer-regexp-history))
		     current-prefix-arg))
  (setq nlines (if nlines (prefix-numeric-value nlines)
		 list-matching-lines-default-context-lines))
  (let ((first t)
	(buffer (current-buffer))
	(linenum 1)
	(prevpos (point-min)))
    (if (not occur-whole-buffer)
	(save-excursion
	  (beginning-of-line)
	  (setq linenum (1+ (count-lines (point-min) (point))))
	  (setq prevpos (point))))
    (with-output-to-temp-buffer "*Occur*"
      (save-excursion
	(set-buffer standard-output)
	(insert "Lines matching ")
	(prin1 regexp)
	(insert " in buffer " (buffer-name buffer) ?. ?\n)
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
	  (beginning-of-line)
	  (setq linenum (+ linenum (count-lines prevpos (point))))
	  (setq prevpos (point))
	  (let* ((start (save-excursion
			  (forward-line (if (< nlines 0) nlines (- nlines)))
			  (point)))
		 (end (save-excursion
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
	      (insert tag ?:)
	      (forward-line 1)
	      (while (< tem nlines)
		(insert empty ?:)
		(forward-line 1)
		(setq tem (1+ tem))))				
	    (forward-line 1)))
	(set-buffer standard-output)
	;; Put positions in increasing order to go with buffer.
	(setq occur-pos-list (nreverse occur-pos-list))
	(if (interactive-p)
	    (message "%d matching lines." (length occur-pos-list)))))))

(defconst query-replace-help
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
ESC or `q' to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r to enter recursive edit (\\[exit-recursive-edit] to get out again),
C-w to delete match and recursive edit,
C-l to clear the screen, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions,
^ to move point back to previous match."
  "Help message while in query-replace")

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
			&optional repeat-count)
  "Subroutine of `query-replace'.  Its complexity handles interactive queries.
Don't use this in your own program unless you want to query and set the mark
just as `query-replace' does.  Instead, write a simple loop like this:
  (while (re-search-forward \"foo[ \t]+bar\" nil t)
    (replace-match \"foobar\" nil nil))
which will run faster and do exactly what you probably want."
  (let ((char nil)
	(event (allocate-event))
	(nocasify (not (and case-fold-search case-replace
			    (string-equal from-string
					  (downcase from-string)))))
	(literal (not regexp-flag))
	(search-function (if regexp-flag 're-search-forward 'search-forward))
	(search-string from-string)
	(next-replacement nil)
	(replacement-index 0)
	(keep-going t)
	(stack nil)
	(next-rotate-count 0)
	(replace-count 0)
	(lastrepl nil))			;Position after last match considered.
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
    (while (and keep-going
		(not (eobp))
		(funcall search-function search-string nil t)
		(if (eq lastrepl (point))
		    (progn 
		      ;; Don't replace the null string 
		      ;; right after end of previous replacement.
		      (forward-char 1)
		      (funcall search-function search-string nil t))
		  t))
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
	    (replace-match next-replacement nocasify literal)
	    (setq replace-count (1+ replace-count)))
	(undo-boundary)
	(let (done replaced)
	  (while (not done)
	    ;; Preserve the match data.  Process filters and sentinels
	    ;; could run inside read-char..
	    (let ((data (match-data))
		  (help-form
		   '(concat "Query replacing "
			    (if regexp-flag "regexp " "")
			    from-string " with " next-replacement ".\n\n"
			    (substitute-command-keys query-replace-help))))
	      (setq char help-char)
	      (while (or (not (numberp char)) (= char help-char))
		(message "Query replacing %s with %s: " from-string next-replacement)
		(perform-replace-next-event event)
		
		(setq char (or (event-to-character event) event))
		(if (and (numberp char) (= char ??))
		    (setq unread-command-event
			   (character-to-event help-char (allocate-event))
			  char help-char)))
	      (store-match-data data))
	    (cond ((or (= char ?\e)
		       (= char ?q))
		   (setq keep-going nil)
		   (setq done t))
		  ((= char ?^)
		   (let ((elt (car stack)))
		     (goto-char (car elt))
		     (setq replaced (eq t (cdr elt)))
		     (or replaced
			 (store-match-data (cdr elt)))
		     (setq stack (cdr stack))))		     
		  ((or (= char ?\ )
		       (= char ?y))
		   (or replaced
		       (replace-match next-replacement nocasify literal))
		   (setq done t replaced t))
		  ((= char ?\.)
		   (or replaced
		       (replace-match next-replacement nocasify literal))
		   (setq keep-going nil)
		   (setq done t replaced t))
		  ((= char ?\,)
		   (if (not replaced)
		       (progn
			 (replace-match next-replacement nocasify literal)
			 (setq replaced t))))
		  ((= char ?!)
		   (or replaced
		       (replace-match next-replacement nocasify literal))
		   (setq done t query-flag nil replaced t))
		  ((or (= char ?\177)
		       (= char ?n))
		   (setq done t))
		  ((= char ?\C-l)
		   (recenter nil))
		  ((= char ?\C-r)
		   (store-match-data
		    (prog1 (match-data)
		      (save-excursion (recursive-edit)))))
		  ((= char ?\C-w)
		   (delete-region (match-beginning 0) (match-end 0))
		   (store-match-data
		    (prog1 (match-data)
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
				 (function (lambda (elt)
					     (if (markerp elt)
						 (marker-position elt)
					       elt)))
				 (match-data))))
		      stack))
	  (if replaced (setq replace-count (1+ replace-count)))))
      (setq lastrepl (point)))
    (and keep-going stack)))

(defun map-query-replace-regexp (regexp to-strings &optional arg)
  "Replace some matches for REGEXP with various strings, in rotation.
The second argument TO-STRINGS contains the replacement strings, separated
by spaces.  This command works like `query-replace-regexp' except
that each successive replacement uses the next successive replacement string,
wrapping around from the last such string to the first.

Non-interactively, TO-STRINGS may be a list of replacement strings.

A prefix argument N says to use each replacement string N times
before rotating to the next."
  (interactive "sMap query replace (regexp): \nsQuery replace %s with (space-separated strings): \nP")
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
  (message "Done"))

