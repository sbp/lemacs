;;; -*-Emacs-Lisp-*-
;;;%Header
;;;
;;; Send mail to ilisp-bug@darwin.bu.edu if you have problems.
;;;
;;; Send mail to ilisp-request@darwin.bu.edu if you want to be on the
;;; ilisp mailing list.
;;;
;;; Inferior LISP interaction package source submodule.
;;; Copyright (C) 1990, 1991, 1992 Chris McConnell, ccm@cs.cmu.edu.
;;;
;;; Rcs_Info: ilisp-src.el,v 1.29 1993/09/03 02:05:07 ivan Rel $
;;;
;;; Rcs_Info: ilisp-src.el,v $
;;; Revision 1.29  1993/09/03  02:05:07  ivan
;;; "Deposit for Release 5.5"
;;;
;;; Revision 1.28  1993/08/31  09:45:22  ivan
;;; "Deposit for Release 5.5"
;;;
;;; Revision 1.27  1993/08/31  09:07:17  ivan
;;; "Deposit for Release 5.5"
;;;
;;; Revision 1.26  1993/08/31  01:50:03  ivan
;;; "Deposit for Release 5.5"
;;;
;;; Revision 1.25  1993/08/31  01:37:44  ivan
;;; "Deposit for Release 5.5"
;;;
;;; Revision 1.24  1993/08/29  09:35:48  ivan
;;; "Deposit for Release 5.5"
;;;
;;; Revision 1.23  1993/08/25  20:37:50  ivan
;;; "Deposit for release ILISP_FIVE_O_FOUR"
;;;
;;; Revision 1.22  1993/08/18  22:48:20  ivan
;;; Fixed definition regexp in ilisp-locate-ilisp to be able to match
;;;
;;; (def-foreign-function (send-header(:language :c)
;;;                                   ^ no space!
;;;
;;; Revision 1.21  1993/06/29  06:13:12  ivan
;;; "Deposit for release ILISP_FIVE_O_THREE"
;;;
;;; Revision 1.20  1993/06/28  20:30:00  ivan
;;; "Deposit for release ILISP_FIVE_O_2"
;;;
;;; Revision 1.19  1993/06/28  19:35:05  ivan
;;; "Deposit for release ILISP_FIVE_O_2"
;;;
;;; Revision 1.18  1993/06/28  16:44:02  ivan
;;; "Deposit for release ILISP_FIVE_O_1"
;;;
;;; Revision 1.17  1993/06/28  03:26:07  ivan
;;; "Deposit for release ILISP_FIVE_O"
;;;
;;; Revision 1.16  1993/06/28  03:12:44  ivan
;;; "Deposit for release ILISP_FIVE_O"
;;;
;;; Revision 1.15  1993/06/28  01:00:24  ivan
;;; "Deposit for release ILISP_FIVE_O"
;;;
;;; Revision 1.14  1993/06/28  00:59:02  ivan
;;; Deposit
;;;
;;; Revision 1.13  1993/06/28  00:41:52  ivan
;;; Deposit for release ILISP_FIVE_O
;;;
;;; Revision 1.12  1993/06/28  00:39:28  ivan
;;; Deposit for release
;;;
; Revision 1.11  1993/06/26  18:24:58  ivan
; Use ilisp-temp-buffer-show-function.
;
; Revision 1.10  1993/06/10  01:02:29  ivan
; Undid ilisp-send mod in edit-definitions-lisp.  Ugh.
;
; Revision 1.9  1993/06/07  07:57:16  ivan
; Hacked edit-definitions-lisp so that it would deal better with
; ILISP:SOURCE-FILE causing some sort of error.  Really only made the
; ilisp-send call pass along 'call as its AND-GO.
;
; Revision 1.8  1993/05/03  18:11:30  ivan
; Made fix-source-filenames not crap out when
; ilisp-source-directory-fixup-alist is null. Just doesn't do anything.
;
; Revision 1.7  1993/05/03  17:34:13  ivan
; Made edit-definitions-lisp know about lemacs' different find-tag
; invocation.
;
; Revision 1.6  1993/05/03  17:31:18  ivan
; Added error message for when ilisp-source-directory-fixup-alist hasn't been
; set to fix-source-filenames.
;
; Revision 1.5  1993/05/03  17:11:13  ivan
; let'ed result to avoid compilation warning.
; /
;
; Revision 1.4  1993/03/16  10:44:34  ivan
; Removed reference to my-result.
;
; Revision 1.3  1993/03/16  09:29:31  ivan
; Added file doesn't exist message to lisp-locate-definition so user
; will know when lisp can't find a definition and when it can't find a
; file.
;
; Revision 1.2  1993/03/16  09:15:35  ivan
; Moved ilisp-source-directory-fixup-alist to ilisp.el.
;
;;; Revision 1.1  1993/03/16  08:17:35  ivan
;;; Initial revision
;;;
;;;
;;; See ilisp.el for more information.

;;;%Source file operations
(if (not (boundp 'tags-file-name)) (defvar tags-file-name nil))
(defvar lisp-last-definition nil "Last definition (name type) looked for.")
(defvar lisp-last-file nil "Last used source file.")
(defvar lisp-first-point nil "First point found in last source file.")
(defvar lisp-last-point nil "Last point in last source file.")
(defvar lisp-last-locator nil "Last source locator used.")
(defvar lisp-search nil "Set to T when searching for definitions.")
(defvar lisp-using-tags nil "Set to T when using tags.")

;;;%%lisp-directory
(defvar lisp-edit-files t
  "If T, then buffers in one of lisp-source-modes will be searched by
edit-definitions-lisp if the source cannot be found through the
inferior LISP.  It can also be a list of files to edit definitions
from set up by \(\\[lisp-directory]).  If it is set to nil, then no
additional files will be searched.")

;;;
(defun lisp-extensions ()
  "Return a regexp for matching the extensions of files that enter one
of lisp-source-modes according to auto-mode-alist."
  (let ((entries auto-mode-alist)
	(extensions nil))
    (while entries
      (let ((entry (car entries)))
	(if (memq (cdr entry) lisp-source-modes)
	    (setq extensions 
		  (concat "\\|" (car entry) extensions))))
      (setq entries (cdr entries)))
  (substring extensions 2)))

;;;
(defun lisp-directory (directory add)
  "Edit the files in DIRECTORY that have an auto-mode alist entry in
lisp-source-modes.  With a positive prefix, add the files on to the
already existing files.  With a negative prefix, clear the list.  In
either case set tags-file-name to nil so that tags are not used."
  (interactive 
   (list (if (not (eq current-prefix-arg '-))
	     (read-file-name "Lisp Directory: "
			     nil
			     default-directory
			     nil))
	     current-prefix-arg))
  (setq tags-file-name nil)
  (if (eq add '-)
      (progn (setq lisp-edit-files t)
	     (message "No current lisp directory"))
      (if add
	  (message "Added %s as a lisp directory" directory)
	  (message "%s is the lisp directory" directory))
      (setq directory (expand-file-name directory))
      (if (file-directory-p directory)
	  (setq lisp-edit-files
		(append
		 (directory-files directory t (lisp-extensions))
		 (if add (if (eq lisp-edit-files t) nil lisp-edit-files))))
	  (error "%s is not a directory" directory))))

;;;%%Utilities

(defun fix-source-filenames ()
  "Apply the ilisp-source-directory-fixup-alist to the current buffer
   (which will be *Edit-Definitions*) to change any pre-compiled
   source-file locations to point to local source file locations.  
   See ilisp-source-directory-fixup-alist."
  (let ((alist (ilisp-value 'ilisp-source-directory-fixup-alist t))
	cons)
    (if alist
	(save-excursion
	  (while alist
	    (setq cons (car alist))
	    (goto-char (point-min))
	    (if (re-search-forward (car cons) (point-max) t)
		(replace-match (cdr cons)))
	    (setq alist (cdr alist)))))))

(defun lisp-setup-edit-definitions (message edit-files)
  "Set up *Edit-Definitions* with MESSAGE. If EDIT-FILES is T, insert
all buffer filenames that are in one of lisp-source-modes into the
current buffer.  If it is a list of files set up by lisp-directory,
insert those in the buffer.  If it is a string put that in the buffer."
  (setq lisp-using-tags nil
	lisp-search (not (stringp edit-files)))
  (set-buffer (get-buffer-create "*Edit-Definitions*"))
  (erase-buffer)
  (insert message)
  (insert "\n\n")
  (if edit-files
      (progn
	(if (eq edit-files t)
	    (let ((buffers (buffer-list)))
	      (while buffers
		(let ((buffer (car buffers)))
		  (if (save-excursion 
			(set-buffer buffer) 
			(and (memq major-mode lisp-source-modes)
			     (buffer-file-name buffer)))
		      (progn (insert ?\") (insert (buffer-file-name buffer))
			     (insert "\"\n"))))
		(setq buffers (cdr buffers))))
	    (if (stringp edit-files)
		(progn (insert edit-files)
		       	;; Remove garbage collection messages
		       (replace-regexp "^;[^\n]*\n" "")
		       (fix-source-filenames))
		(let ((files edit-files))
		  (while files
		    (insert ?\")
		    (insert (car files))
		    (insert "\"\n")
		    (setq files (cdr files))))))
	(goto-char (point-min))
	(forward-line 2)
	(set-buffer-modified-p nil))
      (error 
       (substitute-command-keys
	"Use \\[lisp-directory] to define source files."))))
	  
;;;
(defun lisp-locate-definition (locator definition file point 
				       &optional
				       back pop)
  "Use LOCATOR to find the next DEFINITION (symbol . type) in FILE
starting at POINT, optionally BACKWARDS and POP to buffer.  Return T
if successful."
  (if file 
      (if (not (file-exists-p file))
	  (progn
	    (message "File %s doesn't exist!" file)
	    (sit-for 1)
	    nil)
	  (let* ((symbol (car definition))
		 (type (cdr definition))
		 (first (not (eq lisp-last-file file)))
		 (buffer (current-buffer))
		 name)
	    (lisp-find-file file pop)
	    (if first (setq lisp-first-point (point)))
	    (if back
		(if first
		    (goto-char (point-max))
		    (goto-char point)
		    (forward-line -1) 
		    (end-of-line))
		(goto-char point)
		(if (not first) 
		    (progn (forward-line 1) (beginning-of-line))))
	    (if (eq type 't)
		(message "Search %s for %s" file symbol)
		(message "Searching %s for %s %s" file type
			 (setq name (lisp-buffer-symbol symbol))))
	    (if (funcall locator symbol type first back)
		(progn
		  (setq lisp-last-file file
			lisp-last-point (point))
		  (if (bolp)
		      (forward-line -1)
		      (beginning-of-line))
		  (recenter 0)
		  (if name 
		      (message "Found %s %s definition" type name)
		      (message "Found %s"))
		  t)
		(if first 
		    (goto-char lisp-first-point)
		    (set-buffer buffer)
		    (goto-char point))
		nil)))))

;;;
(defun lisp-next-file (back)
  "Return the next filename in *Edit-Definitions*, or nil if none."
  (let ((file t) 
	result)
    (set-buffer (get-buffer-create "*Edit-Definitions*"))
    (if back 
	(progn (forward-line -1)
	       (if (looking-at "\n")
		   (progn 
		     (forward-line 1)
		     (end-of-line)
		     (setq file nil)))))
  (if file
      (progn
	(skip-chars-forward "^\"")
	(if (eobp)
	    (progn (bury-buffer (current-buffer))
		   (setq result nil))
	    (let* ((start (progn (forward-char 1) (point))))
	      (skip-chars-forward "^\"") 
	      (setq file
		    (prog1 (buffer-substring start (point))
		      (end-of-line)))
	      (bury-buffer (current-buffer))))))
  (if (not (eq file 't)) file)))

;;;
(defun lisp-next-definition (back pop)
  "Go to the next definition from *Edit-Definitions* going BACK with
prefix and POPPING.  Return 'first if found first time, 'none if no
definition ever, T if another definition is found, and nil if no more
definitions are found."
  (let ((done nil)
	(result nil))
    (while
	(not
	 (or
	  (setq result
		(lisp-locate-definition	;Same file
		 lisp-last-locator
		 lisp-last-definition lisp-last-file lisp-last-point back))
	  (let ((file (lisp-next-file back)))
	    (if file
		(if (lisp-locate-definition 
		     lisp-last-locator lisp-last-definition 
		     file 1 back 
		     (prog1 pop (setq pop nil)))
		    (setq result 'first)
		    (setq result (if (not lisp-search) 'none)))
		t)))))
    (set-buffer (window-buffer (selected-window)))
    result))

;;;%%Next-definition
(defun next-definition-lisp (back &optional pop)
  "Edit the next definition from *Edit-Definitions* going BACK with
prefix and optionally POPPING or call tags-loop-continue if using tags."
  (interactive "P")
  (if lisp-using-tags
      (tags-loop-continue)
      (let* ((result (lisp-next-definition back pop))
	     (symbol (car lisp-last-definition))
	     (type (cdr lisp-last-definition))
	     (name (if (not (eq type 't)) (lisp-buffer-symbol symbol))))
	(cond ((or (eq result 'first) (eq result 't))
	       (if name
		   (message "Found %s %s definition" type name)
		   (message "Found %s" symbol)))
	      ((eq result 'none)
	       (error "Can't find %s %s definition" type name))
	      (t 
	       (if name 
		   (error "No more %s %s definitions" type name)
		   (message "Done")))))))


;;;%%Edit-definitions
(defun edit-definitions-lisp (symbol type &optional stay search locator)
  "Find the source files for the TYPE definitions of SYMBOL.  If STAY,
use the same window.  If SEARCH, do not look for symbol in inferior
LISP.  The definition will be searched for through the inferior LISP
and if not found it will be searched for in the current tags file and
if not found in the files in lisp-edit-files set up by
\(\\[lisp-directory]) or the buffers in one of lisp-source-modes if
lisp-edit-files is T.  If lisp-edit-files is nil, no search will be
done if not found through the inferior LISP.  TYPES are from
ilisp-source-types which is an alist of symbol strings or list
strings.  With a negative prefix, look for the current symbol as the
first type in ilisp-source-types."
  (interactive 
   (let* ((types (ilisp-value 'ilisp-source-types t))
	  (default (if types (car (car types))))
	  (function (lisp-function-name))
	  (symbol (lisp-buffer-symbol function)))
     (if (lisp-minus-prefix)
	 (list function default)
	 (list (ilisp-read-symbol 
		(format "Edit Definition [%s]: " symbol)
		function
		nil
		t)
	       (if types 
		   (ilisp-completing-read
		    (format "Type [%s]: " default)
		    types default))))))
  (let* ((name (lisp-buffer-symbol symbol))
	 (symbol-name (lisp-symbol-name symbol))
	 (command (ilisp-value 'ilisp-find-source-command t))
	 (source
	  (if (and command (not search) (comint-check-proc ilisp-buffer))
	      (ilisp-send
	       (format command symbol-name
		       (lisp-symbol-package symbol)
		       type)
	       (concat "Finding " type " " name " definitions")
	       'source )
	      "nil"))
	 (result (and source (lisp-last-line source)))
	 (source-ok (not (or (ilisp-value 'comint-errorp t)
			     (null result)
			     (string-match "nil" (car result)))))
	 (case-fold-search t)
	 (tagged nil))
    (unwind-protect
       (if (and tags-file-name (not source-ok))
	   (progn (setq lisp-using-tags t)
		  (if (string-match "Lucid" emacs-version)
		      (find-tag symbol-name stay)
		      (find-tag symbol-name nil stay))
		  (setq tagged t)))
       (if (not tagged)
	   (progn
	     (setq lisp-last-definition (cons symbol type)
		   lisp-last-file nil
		   lisp-last-locator (or locator (ilisp-value 'ilisp-locator)))
	     (lisp-setup-edit-definitions
	      (format "%s %s definitions:" type name)
	      (if source-ok (cdr result) lisp-edit-files))
	     (next-definition-lisp nil t))))))

;;;%%Searching
(defun lisp-locate-search (pattern type first back)
  "Find PATTERN in the current buffer."
  (if back
      (search-backward pattern nil t)
      (search-forward pattern nil t)))

;;;
(defun lisp-locate-regexp (regexp type first back)
  "Find REGEXP in the current buffer."
  (if back
      (re-search-backward regexp nil t)
      (re-search-forward regexp nil t)))

;;;
(defvar lisp-last-pattern nil "Last search regexp.")
(defun search-lisp (pattern regexp)
  "Search for PATTERN through the files in lisp-edit-files if it is a
list and the current buffers in one of lisp-source-modes otherwise.
If lisp-edit-files is nil, no search will be done.  If called with a
prefix, search for regexp.  If there is a tags file, call tags-search instead."
  (interactive
   (list (read-string (if current-prefix-arg 
			  "Search for regexp: "
			  "Search for: ") lisp-last-pattern)
	 current-prefix-arg))
  (if tags-file-name
      (progn (setq lisp-using-tags t)
	     (tags-search (if regexp pattern (regexp-quote pattern))))
      (setq lisp-last-pattern pattern
	    lisp-last-definition (cons pattern t)
	    lisp-last-file nil
	    lisp-last-locator (if regexp
				  'lisp-locate-regexp
				  'lisp-locate-search))
      (lisp-setup-edit-definitions (format "Searching for %s:" pattern) 
				   lisp-edit-files)
      (next-definition-lisp nil nil)))

;;;%%Replacing
(defvar lisp-last-replace nil "Last replace regexp.")
(defun replace-lisp (old new regexp)
  "Query replace OLD by NEW through the files in lisp-edit-files if it
is a list and the current buffers in one of lisp-source-modes
otherwise.  If lisp-edit-files is nil, no search will be done.  If
called with a prefix, replace regexps.  If there is a tags file, then
call tags-query-replace instead."
  (interactive
   (let ((old (read-string (if current-prefix-arg
			       "Replace regexp: "
			       "Replace: ") lisp-last-pattern)))
     (list old
	   (read-string (if current-prefix-arg
			    (format "Replace regexp %s by: " old)
			    (format "Replace %s by: " old))
			lisp-last-replace)
	   current-prefix-arg)))
  (if tags-file-name
      (progn (setq lisp-using-tags t)
	     (tags-query-replace (if regexp old (regexp-quote old))
				 new))
      (setq lisp-last-pattern old
	    lisp-last-replace new)
      (lisp-setup-edit-definitions 
       (format "Replacing %s by %s:\n\n" old new)
       lisp-edit-files)
      (let (file)
	(while (setq file (lisp-next-file nil))
	  (lisp-find-file file)
	  (let ((point (point)))
	    (goto-char (point-min))
	    (if (if regexp 
		    (re-search-forward old nil t)
		    (search-forward old nil t))
		(progn (beginning-of-line)
		       (if regexp
			   (query-replace-regexp old new)
			   (query-replace old new)))
		(goto-char point)))))))

;;;%%Edit-callers
(defvar lisp-callers nil 
  "T if we found callers through inferior LISP.")

;;;
(defun who-calls-lisp (function &optional no-show)
  "Put the functions that call FUNCTION into the buffer *All-Callers*
and show it unless NO-SHOW is T.  Return T if successful."
  (interactive 
   (let* ((function (lisp-defun-name))
	  (symbol (lisp-buffer-symbol function)))
     (if (lisp-minus-prefix)
	 (list function)
	 (list (ilisp-read-symbol 
		(format "Who Calls [%s]: " symbol)
		function
		t t)))))
  (let* ((name (lisp-buffer-symbol function))
	 (command (ilisp-value 'ilisp-callers-command t))
	 (callers
	  (if command
	      (ilisp-send
	       (format command
		       (lisp-symbol-name function)
		       (lisp-symbol-package function))
	       (concat "Finding callers of " name)
	       'callers)))
	 (last-line (lisp-last-line callers))
	 (case-fold-search t))
    (set-buffer (get-buffer-create "*All-Callers*"))
    (erase-buffer)
    (insert (format "All callers of function %s:\n\n" name))
    (if (and command (not (ilisp-value 'comint-errorp t)))
	(if (string-match "nil" (car last-line))
	    (error "%s has no callers" name)
	    (message "")
	    (insert (cdr last-line))
	    (goto-char (point-min))
	    ;; Remove garbage collection messages
	    (replace-regexp "^;[^\n]*\n" "")
	    (goto-char (point-min))
	    (forward-line 2)
	    (if (not no-show) 
		(if (ilisp-temp-buffer-show-function)
		    (funcall (ilisp-temp-buffer-show-function)
			     (get-buffer "*All-Callers*"))
		    (view-buffer "*All-Callers*")))
	    t)
	(insert "Using the current source files to find callers.")
	nil)))

;;;
(defun next-caller-lisp (back &optional pop)
  "Edit the next caller from *All-Callers*.  With prefix, edit
the previous caller.  If it can't get caller information from the
inferior LISP, this will search using the current source files.  See
lisp-directory."
  (interactive "P")
  (if (not lisp-callers)
      (next-definition-lisp back pop)
      (set-buffer (get-buffer-create "*All-Callers*"))
      (if back (forward-line -1))
      (skip-chars-forward " \t\n")
      (if (eobp)
	  (progn
	    (bury-buffer (current-buffer))
	    (error "No more callers"))
	  (let* ((start (point))
		 (caller-function
		  (progn
		    (skip-chars-forward "^ \t\n")
		    (buffer-substring start (point)))))
	    (bury-buffer (current-buffer))
	    (edit-definitions-lisp (lisp-string-to-symbol caller-function) 
				  (car (car (ilisp-value 'ilisp-source-types)))
				  (not pop))))))

;;;
(defun edit-callers-lisp (function)
  "Edit the callers of FUNCTION.  With a minus prefix use the symbol
at the start of the current defun."
  (interactive
   (let* ((function (lisp-defun-name)))
     (if (lisp-minus-prefix)
	 (list function)
	 (list (ilisp-read-symbol 
		(format "Edit callers of [%s]: "
			(lisp-buffer-symbol function))
		function
		t)))))
  (if (save-excursion (setq lisp-callers (who-calls-lisp function t)))
      (progn 
	(setq lisp-last-locator (ilisp-value 'ilisp-calls-locator))
	(next-caller-lisp nil t))
      (edit-definitions-lisp function "calls" nil t 
			    (ilisp-value 'ilisp-calls-locator))))

;;;%Locators
(defun lisp-re (back format &rest args)
  "Search BACK if T using FORMAT applied to ARGS."
  (let ((regexp (apply 'format format args)))
    (if back
	(re-search-backward regexp nil t)
	(re-search-forward regexp nil t))))

;;;
(defun lisp-locate-ilisp (symbol type first back)
  "Find SYMBOL's TYPE definition in the current file and return T if
successful.  A definition is of the form
\(def<whitespace>(?name<whitespace>."
  (lisp-re back
	   "^[ \t\n]*(def[^ \t\n]*[ \t\n]+(?%s[ \t\n(]+" 
	   (regexp-quote (lisp-symbol-name symbol))))

;;;
(defun lisp-locate-calls (symbol type first back)
  "Locate calls to SYMBOL."
  (lisp-re back "\\(#'\\|(\\|'\\)%s\\([ \t\n]+\\|)\\)"
	   (regexp-quote (lisp-buffer-symbol symbol))))

;;;%%Common LISP
(defun lisp-locate-clisp (symbol type first back)
  "Try to find SYMBOL's TYPE definition in the current buffer and return
T if sucessful.  FIRST is T if this is the first time in a file.  BACK
is T to go backwards."
  (let* ((name (regexp-quote (lisp-symbol-name symbol)))
	 (prefix 
	  ;; Automatically generated defstruct accessors
	  (if (string-match "-" name)
	      (let ((struct (substring name 0 (1- (match-end 0)))))
		(format 
		 "^\\(.\\)?[ \t\n]*(def[^ \t\n]*\\([ \t\n]+\\(.\\)?\\|\\|[ \t\n]*.[ \t\n]+\\)(?%s[ \t\n)]\\|:conc-name\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)%s-" 
		 struct struct))))
	 ;; Defclass accessors
	 (class
	  "\\(:accessor\\|:writer\\|:reader\\)\\([ \t\n]+\\(.\\)?+[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)%s[ \t\n)]"))
    (or
     (if (equal type "any")
	 (lisp-re 
	  back
	  (concat
	   "^\\(.\\)?[ \t\n]*(def[^ \t\n]*\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)\\((setf\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)\\|(?[ \t\n]*\\(.\\)?[ \t\n]*\\)%s[ \t\n)]"
	   (if prefix (concat "\\|" prefix))
	   "\\|"
	   class)
	  name name))
     ;; (qualifiers* (type1 type2 ...))
     (if (string-match "(\\([^(]*\\)\\(([^)]*)\\)" type)
	 (let* ((quals (substring type (match-beginning 1) (match-end 1)))
		(class
		 (read (substring type (match-beginning 2) (match-end 2))))
		(class-re nil)
		(position 0))
	   (while (setq position (string-match 
				  "\\([ \t\n]+.[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\|[ \t\n]+\\)"
				  quals position))
	     (setq quals
		   (concat (substring quals 0 position)
			   "\\([ \t\n]+.[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\|[ \t\n]+\\)"
			   (substring quals (match-end 0)))))
	   (while class
	     (setq class-re 
		   (concat 
		    class-re 
		    (format
		     "[ \t\n]*\\(.\\)?[ \t\n]*([ \t\n]*\\(.\\)?[ \t\n]*[^ \t\n]*\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)%s[ \t\n]*\\(.\\)?[ \t\n]*"
		     (car class)))
		   class (cdr class)))
	   (lisp-re back 
		    "^\\(.\\)?[ \t\n]*(def[^ \t\n]*\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)%s\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)%s[^ \t\n]*([^ \t\n]*%s"
		    name quals class-re)))
     ;; Setf method
     (if (equal type "setf")
	 (lisp-re back 
		  "^\\(.\\)?[ \t\n]*(def[^ \t\n]*\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)(setf\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)%s[ \t\n]*\\(.\\)?[ \t\n]*)"
		  name))
     ;; Function
     (if (equal type "function")
	 (lisp-re back 
		  "^\\(.\\)?[ \t\n]*(defun\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)%s[ \t\n(]"
		  name))
     ;; Macro
     (if (equal type "macro")
	 (lisp-re back 
		  "^\\(.\\)?[ \t\n]*(defmacro\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)%s[ \t\n(]"
		  name))
     ;; Variable
     (if (equal type "variable")
	 (lisp-re back 
		  "^\\(.\\)?[ \t\n]*(def\\(\\(var\\)\\|\\(parameter\\)\\|constant\\)\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)%s[ \t\n(]"
		  name))
     ;; Structure
     (if (equal type "structure")
	 (lisp-re back 
		  "^\\(.\\)?[ \t\n]*(defstruct\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)(?[ \t\n]*\\(.\\)?[ \t\n]*%s[ \t\n(]"
		  name))
     ;; Type
     (if (equal type "type")
	 (lisp-re back 
		  "^\\(.\\)?[ \t\n]*(deftype\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)%s[ \t\n(]"
		  name))
     ;; Class
     (if (equal type "class")
	 (lisp-re back
		  "^\\(.\\)?[ \t\n]*(defclass\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)%s[ \t\n(]"
		  name))
     ;; Standard def form
     (if first (lisp-locate-ilisp symbol type first back))
     ;; Automatically generated defstruct accessors
     (if (and first prefix) (lisp-re back prefix))
     ;; Defclass accessors
     (lisp-re back class name)
     ;; Give up!
     )))

(provide 'ilisp-src )
