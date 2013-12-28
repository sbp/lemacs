;;; Spelling correction interface for GNU EMACS "ispell"

;;; Walt Buehring
;;; Texas Instruments - Computer Science Center
;;; ARPA:  Buehring%TI-CSL@CSNet-Relay
;;; UUCP:  {smu, texsun, im4u, rice} ! ti-csl ! buehring

;;; ispell-region and associated routines added by
;;; Perry Smith
;;; pedz@bobkat
;;; Tue Jan 13 20:18:02 CST 1987

;;; extensively modified by Mark Davies and Andrew Vignaux
;;; {mark,andrew}@vuwcomp
;;; Sun May 10 11:45:04 NZST 1987

;;; This file has overgone a major overhaul to be compatible with ispell
;;; version 2.1.  Most of the functions have been totally rewritten, and
;;; many user-accessible variables have been added.  The syntax table has
;;; been removed since it didn't work properly anyway, and a filter is
;;; used rather than a buffer.  Regular expressions are used based on
;;; ispell's internal definition of characters (see ispell(4)).
;;; Note that ispell:casechars and ispell:otherchars should contain the same
;;; character set as casechars and otherchars in the language.aff file
;;; (for us Yankees, english.aff).
;;; Only one "otherchar" is allowed per word.  To change this, make the
;;; changed noted in the function ispell-word.
;;; Ken Stevens 	ARPA: stevens@hplabs.hp.com	UUCP: hplabs!stevens
;;; Tue Jan  3 16:59:07 PST 1989
;;; Some new updates:
;;; - Updated to version 3.0 to include terse processing.
;;; - Added a variable for the look command.
;;; - Fixed a bug in ispell-word when cursor is far away from the word
;;;   that is to be checked.
;;; - Ispell places the incorrect word or guess in the minibuffer now.
;;; - fixed a bug with 'l' option when multiple windows are on the screen.
;;; - lookup-words just didn't work with the process filter.  Fixed.
;;; - Rewrote the process filter to make it cleaner and more robust
;;;   in the event of a continued line not being completed.
;;; - Made ispell-init-process more robust in handling errors.
;;; - Fixed bug in continuation location after a region has been modified by
;;;   correcting a misspelling.
;;; Mon 17 Sept 1990

;;; To fully install this, add this file to your GNU lisp directory and 
;;; compile it with M-X byte-compile-file.  Then add the following to the
;;; appropriate init file:

;;;  (autoload 'ispell-word "ispell"
;;;    "Check the spelling of word in buffer." t)
;;;  (global-set-key "\e$" 'ispell-word)
;;;  (autoload 'ispell-region "ispell"
;;;    "Check the spelling of region." t)
;;;  (autoload 'ispell-buffer "ispell"
;;;    "Check the spelling of buffer." t)


;;; **********************************************************************
;;; The following variables should be set according to personal preference
;;; and location of binaries:
;;; **********************************************************************


;;;  ******* THIS FILE IS WRITTEN FOR ISPELL VERSION 3.0


;;; Highlighting can slow down display at slow baud and emacs in
;;; X11 windows cannot take advantage of highlighting (yet).
(defconst ispell:highlight-p t
  "*When not nil, spelling errors will be highlighted.")

(defvar ispell:check-comments nil
  "*When true, the spelling of comments in region is checked.")

(defvar ispell:program-name "ispell"
  "Program invoked by ispell-word and ispell-region commands.")

(defvar ispell:alternate-dictionary "/usr/dict/web2"
  "Alternate dictionary for spelling help.")

(defvar ispell:grep-command "/usr/bin/egrep"
  "Name of the grep command for search processes.")

(defvar ispell:look-command "/usr/bin/look"
  "Name of the look command for search processes.")

;;; These should be the same as in the language.aff file
(defconst ispell:casechars "[A-z]"
  "Regular expression of valid characters that comprise a word.")
;;; We also need a regular expression for all characters that are
;;;  not casechars.
(defconst ispell:not-casechars "[^A-z]"
  "Opposite of ispell:casechars")

;;; otherchars cannot be adjacent to each other in a word, nor
;;;  can they begin or end a word.
;;;  This implies we can't check "Stevens'" as a correct possesive
;;;  and other correct formations.
;;; A bug requires the hyphen to be declared first here.
(defconst ispell:otherchars "[---']"
  "Regular expression of other characters that are valid in word constructs.")


;;; **********************************************************************
;;; The following are used by ispell, and should not be changed.
;;; **********************************************************************


(defvar ispell-process nil
  "Holds the process object for 'ispell'")

(defvar ispell:pdict-modified-p nil
  "T when the personal dictionary has modifications that need to be written.")

(defvar ispell:quit nil
  "Set to t when user want to abort ispell session.")

(defvar ispell:look-p t
  "Use look. Automatically reset if look not available")

(defvar ispell:filter nil
  "Output filter from piped calls to ispell.")

(defvar ispell:filter-continue nil
  "Control variable for ispell filter function.")




(defun ispell-word (&optional preceding quietly)
  "Check spelling of word under or following the cursor.
If word not found in dictionary, display possible corrections in a window 
and let user select.
  Optional argument PRECEDING set for checking preceding word when not
over a word, and QUIETLY suppresses messages when word is correct.
  Word syntax described by ispell:casechars and ispell:otherchars."
  (interactive)
  ;; This code only matches words with one "otherchar".
  ;; Replace final ? with * for words that match with many "otherchars".
  (let ((word-regexp (concat ispell:casechars "+\\(" ispell:otherchars
			     "?" ispell:casechars "+\\)?"))
	start end word poss replace)
    (save-excursion
      ;; find the word
      (if (not (looking-at ispell:casechars))
	  (if preceding
	      (re-search-backward ispell:casechars (point-min) t)
	    (re-search-forward ispell:casechars (point-max) t)))
      ;; move to front of word
      (re-search-backward ispell:not-casechars (point-min) 'start)
      ;; replace "if" with "while" in the following line
      ;; to match words with many "otherchars"
      (if (and (looking-at ispell:otherchars) (not (bobp)))
	  (progn
	    (backward-char 1)
	    (if (looking-at ispell:casechars)
		(re-search-backward ispell:not-casechars (point-min) t)
	      (backward-char -1))))
      ;; Now mark the word and save to string.
      (or (re-search-forward word-regexp (point-max) t)
	  (error "No word found to check!"))
      (setq start (match-beginning 0)
	    end (match-end 0)
	    word (buffer-substring start end)))
    (goto-char start)
    ;; now check spelling of word.
    (or quietly (message "Checking spelling of %s..." (upcase word)))
    (ispell-init-process)			; erases ispell output buffer
    (process-send-string ispell-process "%\n")	;put in verbose mode
    (process-send-string ispell-process (concat word "\n"))
    ;; wait until ispell has processed word
    (while (progn
	     (accept-process-output ispell-process)
	     (not (string= "" (car ispell:filter)))))
    (process-send-string ispell-process "!\n")	;back to terse mode.
    (setq ispell:filter (cdr ispell:filter))
    (if (listp ispell:filter)
	(setq poss (ispell-parse-output (car ispell:filter))))
    (cond ((eq poss t)
	   (or quietly (message "Found %s" (upcase word))))
	  ((stringp poss)
	   (or quietly (message "Found %s because of root %s" (upcase word) (upcase poss))))
	  ((null poss) (message "Error in ispell process"))
	  (t
	   (unwind-protect
	       (progn
		 (if ispell:highlight-p
		     (highlight-spelling-error start end t t)) ; highlight word
		 (setq replace (ispell-choose (car (cdr (cdr poss)))
					      (car (cdr (cdr (cdr poss))))
					      (car poss)))
		 ;; update ispell:pdict-modified-p
		 (if (listp ispell:pdict-modified-p)
		     (setq ispell:pdict-modified-p
			   (car ispell:pdict-modified-p))))
	     ;; protected
	     (if ispell:highlight-p  ; clear highlight
		 (highlight-spelling-error start end nil t)))
	   (cond (replace
		  (goto-char end)
		  (delete-region start end)
		  (if (atom replace)
		      (insert-string replace)
		    (insert-string (car replace)) ; replacement string, recheck spelling.
		    (ispell-word t quietly))))
	   (if (get-buffer "*Choices*")
	       (kill-buffer "*Choices*"))))
    (ispell-pdict-save)
    (if ispell:quit (setq ispell:quit nil))))


(defun ispell-pdict-save ()
  "Check to see if the personal dictionary has been modified.
  If so, ask if it needs to be saved."
  (interactive)
  (if ispell:pdict-modified-p
      (if (y-or-n-p "Personal dictionary modified.  Save? ")
	  (process-send-string ispell-process "#\n")))
  (setq ispell:pdict-modified-p nil))		; unassert variable, even if not saved to avoid questioning.


;;; Global ispell:pdict-modified-p is used to track changes in the dictionary.
;;;   The global becomes a list when we either accept or insert word into the dictionary.
;;;   The value of the only element in the list is the state of whether the dictionary
;;;   needs to be saved.
(defun ispell-choose (miss guess word)
  "Display possible corrections from list MISS.
  GUESS lists possibly valid affix construction of WORD.
  Returns nil to keep word.
          string for new chosen word.
          list for new replacement word (needs rechecking).
  Global ispell:pdict-modified-p becomes a list where the only value indicates
   whether the dictionary has been modified when option a or i is used.  This
   must be returned to an atom by the calling program."
  (unwind-protect 
      (save-window-excursion
	(let ((count 0)
	      (line 2)
	      (choices miss)
	      (window-min-height 2)
	      char num result)
	  (save-excursion
	    (set-buffer (get-buffer-create "*Choices*"))
	    (erase-buffer)
	    (setq mode-line-format "--  %b  --")
	    (if guess
		(progn
		  (insert "\tAffix rules generate and capitalize this word as shown below:\n")
		  (while guess
		    (if (> (+ 4 (current-column) (length (car guess)))
			   (window-width))
			(progn
			  (insert "\n")
			  (setq line (1+ line))))
		    (insert (car guess) "    ")
		    (setq guess (cdr guess)))
		  (insert "\nUse option \"i\" if this is a correct composition from the derivative root.\n\n")
		  (setq line (+ line 4))))
	    (while choices
	      (if (> (+ 7 (current-column) (length (car choices)))
		     (window-width))
		  (progn
		    (insert "\n")
		    (setq line (1+ line))))
	      ;; not so good if there are over 20 or 30 options, but then, if
	      ;; there are that many you don't want to have to scan them all anyway...
	      (insert "(" (+ count ?0) ") " (car choices) "  ")
	      (setq choices (cdr choices)
		    count (if (memq count '(14 48 56 59 64 71))	; skip command characters.
			      (if (= count 64)
				  (+ count 3)
				(+ count 2))
			    (1+ count)))))
	  (overlay-window line)
	  (switch-to-buffer "*Choices*")
	  (select-window (next-window))
	  (while (eq t
		     (setq result
			   (progn
			     (message "^h or ? for more options; Space to leave unchanged, Character to replace word")
			     (setq char (read-char))
			     (setq num (- char ?0))
			     (cond ((< num 15))	; hack to map num to choices, avoiding command characters.
				   ((< num 49) (setq num (- num 1)))
				   ((< num 57) (setq num (- num 2)))
				   ((< num 60) (setq num (- num 3)))
				   ((< num 65) (setq num (- num 4)))
				   ((< num 72) (setq num (- num 6)))
				   (t (setq num (- num 7))))
			     (cond ((= char ? ) nil) ; accept word this time only
				   ((= char ?i)	; accept and insert word into personal dictionary
				    (process-send-string ispell-process (concat "*" word "\n"))	; no return value
				    (setq ispell:pdict-modified-p '(t))
				    nil)
				   ((= char ?a)	; accept word, don't insert in dictionary
				    (process-send-string ispell-process (concat "@" word "\n"))	; no return value
				    (setq ispell:pdict-modified-p (list ispell:pdict-modified-p))
				    nil)
				   ((= char ?r)	; type in replacement
				    (cons (read-string "Replacement: " word) nil))
				   ((or (= char ??) (= char help-char) (= char ?\C-h))
				    (save-window-excursion
				      (select-window (minibuffer-window))
				      (save-excursion
					(enlarge-window 1)
					(erase-buffer)
					(insert "[r]eplace word;  [a]ccept for this session;  [i]nsert into private dictionary;\n")
					(insert "[l]ook a word up in alternate dictionary;  e[x]it;  [q]uit session.")
					(sit-for 5)
					(erase-buffer))) t)
				   ((= char ?x)
				    (setq ispell:quit t) nil)
				   ((= char ?q)
				    (if (y-or-n-p "Really quit ignoring changes? ")
					(progn
					  (setq ispell:quit t)
					  (process-send-eof ispell-process) ; terminate process.
					  (setq ispell:pdict-modified-p nil))))
				   ;; Cannot return to initial state after this....
				   ((= char ?l)
				    (let ((new-word (read-string "Lookup string ('*' is wildcard): " word))
					  (new-line 2))
				      (cond (new-word
					     (save-excursion
					       (setq count 0)
					       (set-buffer (get-buffer-create "*Choices*")) (erase-buffer)
					       (setq mode-line-format "--  %b  --")
					       (setq miss (lookup-words new-word))
					       (setq choices miss)
					       (while choices
						 (if (> (+ 7 (current-column) (length (car choices)))
							(window-width))
						     (progn
						       (insert "\n")
						       (setq new-line (1+ new-line))))
						 (insert "(" (+ count ?0) ") " (car choices) "  ")
						 (setq choices (cdr choices)
						       count (if (memq count '(14 48 56 59 64 71)) ; skip commands
								 (if (= count 64)
								     (+ count 3)
								   (+ count 2))
							       (1+ count)))))
					     (select-window (previous-window))
					     (if (/= new-line line)
						 (if (> new-line line)
						     (enlarge-window (- new-line line))
						   (shrink-window (- line new-line))))
					     (select-window (next-window)))))
				    t)
				   ((and (>= num 0) (< num count))
				    (nth num miss))
				   ((= char ?\C-l)
				    (redraw-display) t)
				   ((= char ?\C-r)
				    (save-excursion (recursive-edit)) t)
				   ((= char ?\C-z)
				    (suspend-emacs) t)
				   (t (ding) t))))))
	  result))
    (bury-buffer "*Choices*")))


(defun lookup-words (word)
  "Look up word in dictionary contained in the
  ispell:algernate-dictionary variable.  A '*' is used for wild cards.
  If no wild cards, LOOK is used if it exists.
  Otherwise the variable ispell:grep-command contains the command used to search
  for the words (usually egrep)."
  ;; We need a new copy of the filter to not overwrite the old copy that may currently be
  ;; utilized for another spelling operation.
  (let ((save-ispell-filter ispell:filter) results)
    (setq ispell:filter nil)			; flush output filter if currently running
    (if (and ispell:look-p
	     (not (string-match "\\*" word)))	; Only use look for an exact match.
	(let (temp-ispell-process)
	  (message "Starting \"look\" process...")
	  (sit-for 0)
	  (setq temp-ispell-process
		(start-process "look" nil
			       ispell:look-command "-df" word ispell:alternate-dictionary))
	  (set-process-filter temp-ispell-process 'ispell-filter)
	  (while (progn
		   (accept-process-output temp-ispell-process)
		   (eq (process-status temp-ispell-process) 'run)))
	  (if (zerop (length ispell:filter))	; assure look worked.
	      (progn
		(sit-for 1)			; Hope this is enough ....
		(accept-process-output temp-ispell-process)
		;; See callproc.c for this error message in function child_setup.
		;; This is passed when the program couldn't be found (no "look" here).
		;; Must recheck using grep if look failed.
		(if (not (string-match "Couldn't exec the program "
				       (car ispell:filter)))
		    (setq ispell:filter nil)	; look failed, and there was no error.  No match!
		  (message "Look failed, starting \"egrep\" process...")
		  (sit-for 0)
		  (setq ispell:look-p nil	; No look, disable it from now on.
			ispell:filter nil
			ispell:filter-continue nil) ; Above message DOESN'T send linefeed!
		  (setq temp-ispell-process	; Search for word using ispell:grep-command
			(start-process "egrep" nil ispell:grep-command
				       "-i" (concat "^" word "$") ispell:alternate-dictionary))
		  (set-process-filter temp-ispell-process 'ispell-filter)
		  (while (progn
			   (accept-process-output temp-ispell-process)
			   (eq (process-status temp-ispell-process) 'run)))))))
      (message "Starting \"egrep\" process...")
      (sit-for 0)
      (let ((start 0)				; Format correctly for egrep search.
	    new-word end)
	(while (progn
		 (if (setq end (string-match "\\*" word start))
		     (progn
		       (setq new-word (concat new-word (substring word start end) ".*"))
		       (setq start (1+ end)))
		   (setq new-word (concat new-word (substring word start)))
		   nil)))
	(setq word (concat "^" new-word "$")))
      (let ((temp-ispell-process (start-process "egrep" nil ispell:grep-command
						"-i" word ispell:alternate-dictionary)))
	(set-process-filter temp-ispell-process 'ispell-filter)
	(while (progn
		 (accept-process-output temp-ispell-process)
		 (eq (process-status temp-ispell-process) 'run)))))
    (setq results ispell:filter ispell:filter save-ispell-filter) ; Restore ispell:filter value.
    results))					; return filtered output.


;;; "ispell:filter" is a list of output lines from the generating function.
;;;   Each full line (ending with \n) is a separate item on the list.
;;; "output" can contain multiple lines, part of a line, or both.
;;; "start" and "end" are used to keep bounds on lines when "output" contains
;;;   multiple lines.
;;; "ispell:filter-continue" is true when we have received only part of
;;;   a line as output from a generating function ("output" did not end with a \n).
;;; NOTE THAT THIS FUNCTION WILL FAIL IF THE PROCESS OUTPUT DOESN'T END WITH A \n!
;;;   This is the case when a process dies or fails -- see lookup-words.
;;;   the default behavior in this case is to treat the next input as fresh input
(defun ispell-filter (process output)
  "Output filter function for ispell, grep, and look."
  (let ((start 0)
	(continue t)
	end)
    (while continue
      (setq end (string-match "\n" output start)) ; get text up to the newline.
      ;; If we get out of sinc and ispell:filter-continue is asserted when we are not
      ;; continuing, treat the next item as a separate list.
      ;; When ispell:filter-continue is asserted, ispell:filter *should* always be a list!
      (if (and ispell:filter-continue ispell:filter (listp ispell:filter)) ; Continue with same line (item)?
	  (setcar ispell:filter (concat (car ispell:filter) ;Add it to the prev item
					(substring output start end)))
	(setq ispell:filter (cons (substring output start end) ; This is a new line and item.
				  ispell:filter)))
      (if (null end)				; We've completed reading the output.
	  (setq ispell:filter-continue t continue nil) ; We didn't finish with the line.
	(setq ispell:filter-continue nil end (1+ end)) ; Get new item next time.
	(if (= end (length output))		; No more lines in output
	    (setq continue nil)			;  so we can exit the filter.
	  (setq start end))))))			; Move start to next line of input.


(defun highlight-spelling-error (start end &optional highlight dont-modify)
  "Highlight a word by toggling inverse-video.
  highlights word from START to END.
  When the optional third arg HIGHLIGHT is set, the word is drawn in inverse
  video, otherwise the word is drawn in normal video mode.
  Optional forth arg DONT-MODIFY keeps buffer modification flag from being set
  from this function."
  (if (string-match "^19\\." emacs-version)
      (if (string-match "Lucid" emacs-version)
	  (highlight-spelling-error-v19-Lucid start end highlight dont-modify)
	(highlight-spelling-error-v19 start end highlight dont-modify))
    ;; else 
  (let ((modified (buffer-modified-p))		; leave buffer unmodified if highlight modifies it.
	(text (buffer-substring start end))	; Save highlight region
	(inhibit-quit t)			; don't process interrupt until this function exits
	(buffer-undo-list nil))			; We're not doing anything permanent here, so dont'
    						;  clutter the undo-list with it.
    (delete-region start end)
    (insert-char ?  (- end start))		; white out region to mimimize amount of redisplay
    (sit-for 0)					; update display
    (if highlight (setq inverse-video (not inverse-video))) ; toggle inverse-video
    (delete-region start end)			; delete whitespace
    (insert text)				; insert text in inverse video.
    (sit-for 0)					; update display showing inverse video.
    (if highlight (setq inverse-video (not inverse-video))) ; toggle inverse-video
    (if dont-modify
	(set-buffer-modified-p modified))
    (message " "))))

(defun highlight-spelling-error-v19 (start end &optional highlight dont-modify)
  (if highlight
      (setq ispell-saved-selection (cons selection-begin selection-end)
	    selection-begin (set-marker (make-marker) start)
	    selection-end (set-marker (make-marker) end))
    (setq selection-begin (car ispell-saved-selection)
	  selection-end (cdr ispell-saved-selection)
	  ispell-saved-selection nil))
  (sit-for 0))

(defun highlight-spelling-error-v19-Lucid (start end &optional highlight
					   dont-modify)
  (if highlight
      (isearch-highlight start end)
    (isearch-dehighlight t))
  (sit-for 0))

(defun overlay-window (height)
  "Create a (usually small) window with HEIGHT lines and avoid
recentering."
  (save-excursion
    (let ((oldot (save-excursion (beginning-of-line) (dot)))
	  (top (save-excursion (move-to-window-line height) (dot)))
	  newin)
      (if (< oldot top) (setq top oldot))
      (setq newin (split-window-vertically height))
      (set-window-start newin top))))


(defun ispell-parse-output (output)
  "Parse the OUTPUT string of 'ispell' and return:
 1) T for an exact match.
 2) A string containing the root word for a match via suffix removal.
 3) A list of possible correct spellings of the format:
    '(\"original-word\" offset miss-list guess-list)
    original-word is a string of the possibly misspelled word.
    offset is an integer of the line offset of the word.
    miss-list and guess-list are possibly null list of guesses and misses."
  (cond
   ((string= output "*") t)			; exact match
   ((string= (substring output 0 1) "+")	; found cuz of rootword
    (substring output 2))			; return root word
   (t						; need to process &,?, and #'s
    (let ((type (substring output 0 1))		; &, ?, or #
	  (original-word (substring output 2 (string-match " " output 2)))
	  (cur-count 0)				; contains current number of misses + guesses
	  count miss-list guess-list)
      (setq output (substring output (match-end 0))) ; skip over original misspelling
      (if (string= type "#")
	  (setq count 0)			; no misses for type #
	(setq count (string-to-int output))	; get number of misses.
	(setq output (substring output (1+ (string-match " " output 1)))))
      (setq offset (string-to-int output))
      (if (string= type "#")			; No miss or guess list.
	  (setq output nil)
	(setq output (substring output (1+ (string-match " " output 1)))))
      (while output
	(let ((end (string-match ",\\|\\($\\)" output))) ; end of next miss/guess.
	  (setq cur-count (1+ cur-count))
	  (if (> cur-count count)
	      (setq guess-list (cons (substring output 0 end) guess-list))
	    (setq miss-list (cons (substring output 0 end) miss-list)))
	  (if (match-end 1)			; True only when at end of line.
	      (setq output nil)			; no more misses or guesses
	    (setq output (substring output (+ end 2))))))
      (list original-word offset miss-list guess-list)))))


(defun ispell-init-process ()
  "Check status of 'ispell' process and start if necessary."
  (if (and ispell-process
	   (eq (process-status ispell-process) 'run))
      (setq ispell:filter nil ispell:filter-continue nil)
    (message "Starting new ispell process...")
    (sit-for 0)
    (setq ispell-process (start-process "ispell" nil ispell:program-name "-a" "-m")
	  ispell:filter nil
	  ispell:filter-continue nil)
    (set-process-filter ispell-process 'ispell-filter)
    (process-send-string ispell-process "!\n")	; Put into terse mode -- save processing & parsing time!
    (process-kill-without-query ispell-process)))


;;; Requires ispell version 2.1.02 or later.
;;; Ispell processes the file and no UNIX filters are used.
;;; This allows tex and nroff files to be processed well (ispell knows about them).
;;; Spelling of comments are checked when ispell:check-comments is non-nil.
(defun ispell-region (reg-start reg-end)
  "Interactively check a region for spelling errors."
  (interactive "*r")
  (ispell-init-process)
  (if (memq major-mode '(plain-TeX-mode plain-tex-mode TeX-mode tex-mode LaTeX-mode latex-mode))
      (process-send-string ispell-process "+\n")	; set ispell mode to tex
    (process-send-string ispell-process "-\n"))		; set ispell mode to normal (nroff)
  (save-excursion
    (message "Spelling %s..."
	     (if (and (= reg-start (point-min)) (= reg-end (point-max)))
		 (buffer-name) "region"))
    (sit-for 0)
    (goto-char reg-start)
    (while (and (not ispell:quit) (< (point) reg-end))
      (let ((start (point))
	    (offset-change 0)
	    (end (save-excursion (end-of-line) (min (point) reg-end)))
	    string)
	(cond ((eolp)				; if at end of line, just go to next.
	       (forward-char 1))
	      ((and (null ispell:check-comments)
		    comment-start		; skip comments that start on the line.
		    (search-forward comment-start end t)) ; a comment is on this line.
	       (if (= (- (point) start) (length comment-start)) ; comments starts line.
		   (if (string= "" comment-end) ; skip to next line over comment
		       (beginning-of-line 2)
		     (search-forward comment-end reg-end 'limit)) ; Skip to end of comment
		 ;; Comment starts later on line.
		 ;; Only send string if it contains "casechars" before comment.
		 (let ((limit (- (point) (length comment-start)))) 
		   (goto-char start)
		   (if (re-search-forward ispell:casechars limit t)
			 (setq string (concat (buffer-substring start limit) "\n")))
		   (goto-char limit))))
	      ((looking-at "[---#@*+!%]")	; looking at the special ispell characters..
	       (forward-char 1))		; skip over it.
	      ((re-search-forward ispell:casechars end t) ; text exists...
	       (setq string (concat (buffer-substring start end) "\n"))
	       (goto-char end))
	      (t (beginning-of-line 2)))	; empty line, skip it.
	(setq end (point))			; use "end" to track end of region to check.
	(if string				; there is something to spell!
	    (let (poss)
	      ;; send string to spell process and get input.
	      (process-send-string ispell-process string)
	      (while (progn
		       (accept-process-output ispell-process)
		       (not (string= "" (car ispell:filter)))))	;Last item of output contains a blank line.
	      ;; parse all inputs from the stream one word at a time.
	      (setq ispell:filter (nreverse (cdr ispell:filter))) ; remove blank item.
	      (while (and (not ispell:quit) ispell:filter)
		(setq poss (ispell-parse-output (car ispell:filter)))
		(if (listp poss)		; spelling error occured.
		    (let* ((word-start (+ start offset-change (car (cdr poss))))
			   (word-end (+ word-start (length (car poss))))
			   replace)
		      ;; debug debug debug
		      (goto-char word-start)
		      (if (/= word-end (progn
					 (re-search-forward (car poss) word-end t)
					 (point)))
			  ;; This usually occurs due to pipe problems with the filter.
			  (error "***ispell misalignment: word \"%s\" point %d; please retry."
				 (car poss) word-start))
		      (unwind-protect
			  (progn
			    (if ispell:highlight-p
				(highlight-spelling-error word-start word-end
							  t t) ; highlight word
			      (sit-for 0))	; otherwise, update screen.
			    (setq replace
				  (ispell-choose (car (cdr (cdr poss)))
						 (car (cdr (cdr (cdr poss))))
						 (car poss))))
			;; protected
			(if ispell:highlight-p
			    (highlight-spelling-error word-start word-end
						      nil t))) ; un-highlight
		      (goto-char word-start)
		      (if replace
			  (if (listp replace)	; re-check all list replacements; otherwise exit.
			      (progn
				;; quit parsing this line, redo rest when re-checking new word.
				(setq ispell:filter nil)
				;; adjust regions
				(let ((change (- (length (car replace)) (length (car poss)))))
				  (setq reg-end (+ reg-end change))
				  (setq offset-change (+ offset-change change)))
				(delete-region word-start word-end)
				(insert (car replace))
				(backward-char (length (car replace)))
				(setq end (point))) ; reposition within region to recheck spelling.
			    (delete-region word-start word-end)
			    (insert replace)
			    (let ((change (- (length replace) (length (car poss)))))
			      (setq reg-end (+ reg-end change)
				    offset-change (+ offset-change change)
				    end (+ end change))))
			;; This prevents us from pointing out the word that was just accepted
			;; (via 'i' or 'a') if it follows on the same line. (The one drawback of
			;; processing an entire line.)  Redo check following the accpeted word.
			(cond ((and (not (null ispell:pdict-modified-p)) (listp ispell:pdict-modified-p))
			       ;; We have accepted or inserted a word.  Re-check line.
			       (setq ispell:pdict-modified-p (car ispell:pdict-modified-p)) ; fix update flag
			       (setq ispell:filter nil) ; don't continue check.
			       (setq end word-end)))) ; reposition to check line following accepted word.
		      (message "continuing spelling check...")
		      (sit-for 0)))
		(setq ispell:filter (cdr ispell:filter))))) ; finished with this check.
	(goto-char end))))
  (ispell-pdict-save)
  (if ispell:quit (setq ispell:quit nil))
  (message "Spell done."))


(defun ispell-buffer () 
  "Check the current buffer for spelling errors interactively."
  (interactive)
  (ispell-region (point-min) (point-max)))
