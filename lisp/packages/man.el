;; Find and display Unix manual pages.
;; Copyright (C) 1985-1993 Free Software Foundation, Inc.
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Mostly rewritten by Alan K. Stebbens <aks@hub.ucsb.edu> 11-apr-90.
;;
;;  o  Match multiple man pages using TOPIC as a simple pattern
;;  o  Search unformatted pages, even when formatted matches are found
;;  o  Query the user as to which pages are desired
;;  o  Use of the prefix arg to toggle/bypass the above features
;;  o  Buffers named by the first topic in the buffer
;;  o  Automatic uncompress for compressed man pages (.Z and .z)
;;  o  View the resulting buffer using M-x view mode
;;
;; Modified 16-mar-91 by Jamie Zawinski <jwz@lucid.com> to default the 
;; manual topic to the symbol at point, just like find-tag does.
;;
;; Modified 22-mar-93 by jwz to use multiple fonts and follow xrefs with mouse.
;;
;; Modified 16-apr-93 by Dave Gillespie <daveg@synaptics.com> to make
;; apropos work nicely; work correctly when bold or italic is unavailable; 
;; reuse old buffer if topic is re-selected (in Manual-topic-buffer mode).

;; This file defines "manual-entry", and the remaining definitions all
;; begin with "Manual-".  This makes the autocompletion on "M-x man" work.
;;
;; Variables of interest:
;;
;;	Manual-program
;;	Manual-topic-buffer
;;	Manual-buffer-view-mode
;;	Manual-directory-list
;;	Manual-formatted-directory-list
;;	Manual-match-topic-exactly
;;	Manual-query-multiple-pages
;;	Manual-page-history

(defvar Manual-program "man" "\
*Name of the program to invoke in order to format the source man pages.")

(defvar Manual-topic-buffer t "\
*Non-nil means \\[Manual-entry] should output the manual entry for TOPIC into
a buffer named *TOPIC Manual Entry*, otherwise, it should name the buffer
*Manual Entry*.")

(defvar Manual-buffer-view-mode t "\
*Non-nil means that \\[view-buffer] is used to display the output from
\\[Manual-entry]; nil means that the buffer is left in fundamental-mode
in another window.")

(defvar Manual-match-topic-exactly t "\
*Non-nil means that \\[manual-entry] will match the given TOPIC exactly, rather
apply it as a pattern.  When this is nil, and \"Manual-query-multiple-pages\" is
non-nil, then \\[manual-entry] will query you for all matching TOPICs.
This variable only has affect on the preformatted man pages (the \"cat\" files),
since the \"man\" command always does exact topic matches.")

(defvar Manual-query-multiple-pages nil "\
*Non-nil means that \\[manual-entry] will query the user about multiple man
pages which match the given topic.  The query is done using the function 
\"y-or-n-p\".  If this variable is nil, all man pages with topics matching the
topic given to \\[manual-entry] will be inserted into the temporary buffer.
See the variable \"Manual-match-topic-exactly\" to control the matching.")

(defvar Manual-mode-hook nil
  "Function or functions run on entry to Manual-mode.")

(defvar Manual-directory-list nil "\
*A list of directories used with the \"man\" command, where each directory
contains a set of \"man?\" and \"cat?\" subdirectories.  If this variable is nil,
it is initialized by \\[Manual-directory-list-init].")

(defvar Manual-formatted-directory-list nil "\
A list of directories containing formatted man pages.  Initialized by
\\[Manual-directory-list-init].")

(defvar Manual-unformatted-directory-list nil "\
A list of directories containing the unformatted (source) man pages.  
Initialized by \\[Manual-directory-list-init].")

(defvar Manual-page-history nil "\
A list of names of previously visited man page buffers.")

(make-face 'man-italic)
(or (face-differs-from-default-p 'man-italic)
    (copy-face 'italic 'man-italic))
(or (face-differs-from-default-p 'man-italic)
    (set-face-underline-p 'man-italic t))

(make-face 'man-bold)
(or (face-differs-from-default-p 'man-bold)
    (copy-face 'bold 'man-bold))
(or (face-differs-from-default-p 'man-bold)
    (copy-face 'man-italic 'man-bold))

(make-face 'man-heading)
(or (face-differs-from-default-p 'man-heading)
    (copy-face 'man-bold 'man-heading))


;; Manual-directory-list-init
;; Initialize the directory lists.

(defun Manual-directory-list-init (&optional arg) "\
Unless the variable Manual-directory-list is nil, initialize it using the
MANPATH environment variable.  Once this variable is set,
\\[Manual-directory-list-init] will not reinitialize it unless a prefix
argument is given."
  (interactive "P")
  (if arg (setq Manual-directory-list nil))
  (if (null Manual-directory-list)
      (let ((manpath (or (getenv "MANPATH") ""))
	    (dirlist nil))
	(while (string-match "\\`:*\\([^:]+\\)" manpath)
	  (setq dirlist (cons (substring manpath
					 (match-beginning 1) (match-end 1))
			      dirlist))
	  (setq manpath (substring manpath (match-end 0))))
	(setq dirlist (nreverse dirlist))
	(setq Manual-directory-list dirlist)
	(setq Manual-formatted-directory-list nil)
	(setq Manual-unformatted-directory-list nil)))
  (if (null Manual-formatted-directory-list)
      (setq Manual-formatted-directory-list
	    (Manual-select-subdirectories Manual-directory-list "cat")))
  (if (null Manual-unformatted-directory-list)
      (setq Manual-unformatted-directory-list
	    (Manual-select-subdirectories Manual-directory-list "man"))))

;;
;; manual-entry  -- The "main" user function
;;

(defun manual-entry (topic &optional arg silent)
  "Display the Unix manual entry (or entries) for TOPIC.  If prefix
arg is given, modify the search according to the value:
  2 = toggle exact matching of the TOPIC name
  3 = force a search of the unformatted man directories
  4 = both 2 and 3
The manual entries are searched according to the variable
Manual-directory-list, which should be a list of directories.  If
Manual-directory-list is nil, \\[Manual-directory-list-init] is
invoked to create this list from the MANPATH environment variable.
See the variable Manual-topic-buffer which controls how the buffer
is named.  See also the variables Manual-match-topic-exactly,
Manual-query-multiple-pages, and Manual-buffer-view-mode."
  (interactive
   (list (let* ((fmh "-A-Za-z0-9_.")
		(default (save-excursion
			   (buffer-substring
			    (progn
			      (re-search-backward "\\sw" nil t)
			      (skip-chars-backward fmh) (point))
			    (progn (skip-chars-forward fmh) (point)))))
		(thing (read-string
			(if (equal default "") "Manual entry: "
			  (concat "Manual entry: (default " default ") ")))))
	   (if (equal thing "") default thing))
	 (prefix-numeric-value current-prefix-arg)))
  ;;(interactive "sManual entry (topic): \np")
  (or arg (setq arg 1))
  (Manual-directory-list-init nil)
  (let ((exact (if (or (= arg 2)(= arg 4))
		   (not Manual-match-topic-exactly)
		 Manual-match-topic-exactly))
	(force (>= arg 3))
	(sep (make-string 65 ?-))
	section fmtlist manlist apropos-mode)
    (let ((case-fold-search nil))
      (if (and (null section)
	       (string-match
		"\\`[ \t]*\\([^( \t]+\\)[ \t]*(\\(.+\\))[ \t]*\\'" topic))
	  (setq section (substring topic (match-beginning 2)
				   (match-end 2))
		topic (substring topic (match-beginning 1)
				 (match-end 1)))
	(if (string-match "\\`[ \t]*-k[ \t]+\\([^ \t]+\\)\\'" topic)
	    (setq section "-k"
		  topic (substring topic (match-beginning 1))))))
    (if (equal section "-k")
	(setq apropos-mode t)
      (or silent
	  (message "Looking for formatted entry for %s%s..."
		   topic (if section (concat "(" section ")") "")))
      (setq fmtlist (Manual-select-man-pages
		     (Manual-select-directories
		      Manual-formatted-directory-list section) 
		     topic section exact))
      (if (or force (not fmtlist))
	  (progn
	    (or silent
		(message "%sooking for unformatted entry for %s%s..."
			 (if fmtlist "L" "No formatted entry, l")
			 topic (if section (concat "(" section ")") "")))
	    (setq manlist (Manual-select-man-pages
			   (Manual-select-directories
			    Manual-unformatted-directory-list section)
			   topic section exact)))))
    (if (or fmtlist manlist apropos-mode)
	(let* ((name (car (or fmtlist manlist)))
	       (bufname (concat
			 (if Manual-topic-buffer
			     (if apropos-mode
				 (concat "*" topic " ")
			       (concat "*"
				       (and (string-match "/\\([^/]+\\)$" name)
					    (substring name (match-beginning 1)
						       (match-end 1)))
				       " ")))
			 (if apropos-mode
			     "*Manual Apropos*" "*Manual Entry*")))
	       (temp-buffer-show-function (if Manual-buffer-view-mode
					      'view-buffer
					    temp-buffer-show-function)))
	  ;; Delete duplicate man pages (a file of the same name in multiple
	  ;; directories.)
	  (let ((rest (append fmtlist manlist)))
	    (while rest
	      (let ((rest2 (cdr rest)))
		(while rest2
		  (if (equal (file-name-nondirectory (car rest))
			     (file-name-nondirectory (car rest2)))
		      (setq fmtlist (delq (car rest2) fmtlist)
			    manlist (delq (car rest2) manlist)))
		  (setq rest2 (cdr rest2))))
	      (setq rest (cdr rest))))

	  (if apropos-mode
	      (setq manlist (list (format "%s.%s" topic section))))

	  (cond
	   ((and Manual-topic-buffer (get-buffer bufname))
	    ;; reselect an old man page buffer if it exists already.
	    (save-excursion
	      (set-buffer (get-buffer bufname))
	      (Manual-mode))
	    (if temp-buffer-show-function
		(funcall temp-buffer-show-function (get-buffer bufname))
	      (display-buffer bufname)))
	   (t
	    (with-output-to-temp-buffer bufname
	      (buffer-disable-undo standard-output)
	      (save-excursion
		(set-buffer standard-output)
		(setq buffer-read-only nil)
		(erase-buffer)
		(let (name start end topic section)
		  (while fmtlist	; insert any formatted files
		    (setq name (car fmtlist))
		    (goto-char (point-max))
		    (setq start (point))
		    ;; In case the file can't be read or uncompressed or
		    ;; something like that.
		    (condition-case ()
			(Manual-insert-man-file name)
		      (file-error nil))
		    (goto-char (point-max))
		    (setq end (point))
		    (save-excursion
		      (save-restriction
			(message "Cleaning manual entry for %s..."
				 (file-name-nondirectory name))
			(narrow-to-region start end)
			(Manual-nuke-nroff-bs)))
		    (if (or (cdr fmtlist) manlist)
			(insert "\n\n" sep "\n"))
		    (setq fmtlist (cdr fmtlist)))
		  (while manlist	; process any unformatted files
		    (setq name (car manlist))
		    (string-match "\\([^/]+\\)\\.\\([^./]+\\)$" name)
		    (setq topic (substring name (match-beginning 1)
					   (match-end 1)))
		    (setq section (substring name (match-beginning 2)
					     (match-end 2)))
		    (message "Invoking man %s %s ..." section topic)
		    (setq start (point))
		    ;; kludge kludge
		    (if (string-match "roff\\'" Manual-program)
			(call-process Manual-program nil t nil
				      "-Tman" "-man" name)
		      (call-process Manual-program nil t nil section topic))
		    (setq end (point))
		    (save-excursion
		      (save-restriction
			(message "Cleaning manual entry for %s(%s)..."
				 topic section)
			(narrow-to-region start end)
			(Manual-nuke-nroff-bs apropos-mode)))
		    (if (cdr manlist)
			(insert "\n\n" sep "\n"))
		    (setq manlist (cdr manlist))))
		(if (< (buffer-size) 80)
		    (progn
		      (goto-char (point-min))
		      (end-of-line)
		      (error (buffer-substring 1 (point)))))
		(set-buffer-modified-p nil)
		(Manual-mode)
		))))
 	  (setq Manual-page-history
 		(cons (buffer-name)
		      (delete (buffer-name) Manual-page-history)))
	  (message nil)
	  t)
      ;; else
      (message "No entries found for %s%s" topic
	       (if section (concat "(" section ")") ""))
      nil)))

(defvar Manual-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'Manual-mode-map)
    (define-key m "l" 'Manual-last-page)
    (define-key m 'button2 'Manual-follow-xref)
    (define-key m 'button3 'Manual-popup-menu)
    m))

(defun Manual-mode ()
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (use-local-map Manual-mode-map)
  (setq major-mode 'Manual-mode
	mode-name "Manual")
  ;; man pages with long lines are buggy!
  ;; This looks slightly better if they only
  ;; overran by a couple of chars.
  (setq truncate-lines t)
  (run-hooks 'Manual-mode-hook))

(defun Manual-last-page ()
  (interactive)
  (while (or (not (get-buffer (car (or Manual-page-history
				       (error "No more history.")))))
	     (eq (get-buffer (car Manual-page-history)) (current-buffer)))
    (setq Manual-page-history (cdr Manual-page-history)))
  (switch-to-buffer (car Manual-page-history)))

;; Manual-select-subdirectories
;; Given a DIRLIST and a SUBDIR name, return all subdirectories of the former which
;; match the latter.

(defun Manual-select-subdirectories (dirlist subdir)
  (apply 'append (mapcar '(lambda (dir)
			   (and (file-exists-p dir)
                              (mapcar
			       '(lambda (name) (expand-file-name name dir))
			       (sort (file-name-all-completions subdir dir)
				     'string<))))
			 dirlist)))

;; Manual-select-directories
;;
;; Select from DIRLIST the appropriate directories by SECTION.
;; Return selected directories in a list.  If SECTION is nil, select
;; all SECTION directories.

(defun Manual-select-directories (dirlist section)
  (delq nil
	(mapcar
	 (function (lambda (fmtdir)
		     (if (or (not section)
			     (string-match (concat (substring section 0 1)
						   "/$") fmtdir))
			 fmtdir)))
	 dirlist)))

;; Manual-select-man-pages
;;
;; Given a DIRLIST, discover all filenames which complete given the TOPIC and SECTION.

(defun Manual-select-man-pages-iterator (file)
  ;; If Manual-match-topic-exactly is set, then we must make sure
  ;; the completions are exact, except for trailing weird characters
  ;; after the section.
  (if (or (not exact)
	  (eq 0 (string-match (concat "^" topic "\\." (or section)) file)))
      (concat dir file)))

;; ## Note: BSD man looks for .../man1/foo.1 and .../man1/$MACHINE/foo.1

(defun Manual-select-man-pages (dirlist topic section exact)
  (let ((manlist
	 (apply 'append		; this removes the nulls
	   (mapcar (function
		    (lambda (dir)
		      (if (file-directory-p dir)
			  (delq nil
				(mapcar 'Manual-select-man-pages-iterator
					(file-name-all-completions
					 (concat topic
						 (if section
						     (concat "." section)))
					 dir)))
			(message "warning: %s is not a directory" dir)
			;;(sit-for 1)
			nil)))
		   dirlist))))
    (if (and manlist Manual-query-multiple-pages)
	(apply 'append
	       (mapcar '(lambda (page)
			  (if (and page 
				   (y-or-n-p (format "Read %s? " page)))
			      (list page)))
		       manlist))
      manlist)))

(defun Manual-insert-man-file (name)
  ;; Insert manual file (unpacked as necessary) into buffer
  (cond ((or (equal (substring name -2) ".Z")
	     ;; HPUX uses directory names that end in .Z and compressed
	     ;; files that don't.  How gratuitously random.
	     (string-match "\\.Z/" name))
	 (call-process "zcat" nil t nil name))
	((equal (substring name -2) ".z")
	 (call-process "pcat" nil t nil name)) ; use gzip instead?
	(t
	 (insert-file-contents name))))

(defmacro Manual-delete-char (n)
  ;; in v19, delete-char is compiled as a function call, but delete-region
  ;; is byte-coded, so it's much faster.  (We were spending 40% of our time
  ;; in delete-char alone.)
  (list 'delete-region '(point) (list '+ '(point) n)))

;; Hint: BS stands form more things than "back space"
(defun Manual-nuke-nroff-bs (&optional apropos-mode)
  (interactive "*")
  ;;
  ;; turn underlining into italics
  (goto-char (point-min))
  (while (re-search-forward "\\(_\b[^\n]\\)+" nil t)
    (let ((s (match-beginning 0))
	  (e (match-end 0)))
      (goto-char s)
      (while (< (point) e)
	(setq e (- e 2))
	(Manual-delete-char 2)
	(forward-char 1))
      (set-extent-face (make-extent s (point)) 'man-italic)))
  ;;
  ;; turn overstriking into bold
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (if (save-excursion
	  (forward-char -2)
	  (looking-at "\\(\\([^\n]\\)\b\\2\\)+"))
	(let* ((s (match-beginning 0))
	       (e (match-end 0)))
	  (goto-char s)
	  (while (< (point) e)
	    (setq e (- e 2))
	    (Manual-delete-char 2)
	    (forward-char 1))
	  (set-extent-face (make-extent s e) 'man-bold))))
  ;;
  ;; hack bullets: o^H+ --> +
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (Manual-delete-char -2))

  (Manual-nuke-nroff-bs-footers)
  ;;
  ;; turn subsection header lines into bold
  (goto-char (point-min))
  (if apropos-mode
      (while (re-search-forward "[a-zA-Z0-9] ([0-9]" nil t)
	(backward-char 2)
	(delete-backward-char 1))
    (while (re-search-forward "^[^ \t\n]" nil t)
      (set-extent-face (make-extent (match-beginning 0)
				    (progn (end-of-line) (point)))
		       'man-heading)))

  ;; Zap ESC7,  ESC8, and ESC9
  ;; This is for Sun man pages like "man 1 csh"
;  (goto-char (point-min))
;  (while (re-search-forward "\e[789]" nil t)
;    (replace-match ""))

  ;; Nuke blanks lines at start.
;  (goto-char (point-min))
;  (skip-chars-forward "\n")
;  (delete-region (point-min) (point))

  (Manual-mouseify-xrefs)
  )

(fset 'nuke-nroff-bs 'Manual-nuke-nroff-bs) ; use old name


(defun Manual-nuke-nroff-bs-footers ()
  ;; Nuke headers and footers.
  ;;
  ;; nroff assumes pages are 66 lines high.  We assume that, and that the
  ;; first and last line on each page is expendible.  There is no way to
  ;; tell the difference between a page break in the middle of a paragraph
  ;; and a page break between paragraphs (the amount of extra whitespace
  ;; that nroff inserts is the same in both cases) so this might strip out
  ;; a blank line were one should remain.  I think that's better than
  ;; leaving in a blank line where there shouldn't be one.  (Need I say
  ;; it: FMH.)
  ;;
  ;; Note that if nroff spits out error messages, pages will be more than
  ;; 66 lines high, and we'll lose badly.  That's ok because standard
  ;; nroff doesn't do any diagnostics, and the "gnroff" wrapper for groff
  ;; turns off error messages for compatibility.  (At least, it's supposed
  ;; to.)
  ;; 
  (goto-char (point-min))
  ;; first lose the status output
  (let ((case-fold-search t))
    (if (and (not (looking-at "[^\n]*warning"))
	     (looking-at "Reformatting.*\n"))
	(delete-region (match-beginning 0) (match-end 0))))

  ;; kludge around a groff bug where it won't keep quiet about some
  ;; warnings even with -Wall or -Ww.
  (cond ((looking-at "grotty:")
	 (while (looking-at "grotty:")
	   (delete-region (point) (progn (forward-line 1) (point))))
	 (if (looking-at " *done\n")
	     (delete-region (point) (match-end 0)))))

  (let ((pages '())
	p)
    ;; collect the page boundary markers before we start deleting, to make
    ;; it easier to strip things out without changing the page sizes.
    (while (not (eobp))
      (forward-line 66)
      (setq pages (cons (point-marker) pages)))
    (setq pages (nreverse pages))
    (while pages
      (goto-char (car pages))
      (set-marker (car pages) nil)
      ;;
      ;; The lines are: 3 blank; footer; 6 blank; header; 3 blank.
      ;; We're in between the previous footer and the following header,
      ;;
      ;; First lose 3 blank lines, the header, and then 3 more.
      ;;
      (setq p (point))
      (skip-chars-forward "\n")
      (delete-region p (point))
      (and (looking-at "[^\n]+\n\n?\n?\n?")
	   (delete-region (match-beginning 0) (match-end 0)))
      ;;
      ;; Next lose the footer, and the 3 blank lines after, and before it.
      ;; But don't lose the last footer of the manual entry; that contains
      ;; the "last change" date, so it's not completely uninteresting.
      ;; (Actually lose all blank lines before it; sh(1) needs this.)
      ;;
      (skip-chars-backward "\n")
      (beginning-of-line)
      (if (null (cdr pages))
	  nil
	(and (looking-at "[^\n]+\n\n?\n?\n?")
	     (delete-region (match-beginning 0) (match-end 0))))
      (setq p (point))
      (skip-chars-backward "\n")
      (if (> (- p (point)) 4)
	  (delete-region (+ 2 (point)) p)
	(delete-region (1+ (point)) p))
;      (and (looking-at "\n\n?\n?")
;	   (delete-region (match-beginning 0) (match-end 0)))

      (setq pages (cdr pages)))
    ;;
    ;; Now nuke the extra blank lines at the beginning and end.
    (goto-char (point-min))
    (if (looking-at "\n+")
	(delete-region (match-beginning 0) (match-end 0)))
    (forward-line 1)
    (if (looking-at "\n\n+")
	(delete-region (1+ (match-beginning 0)) (match-end 0)))
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (delete-region (point) (point-max))
    (beginning-of-line)
    (forward-char -1)
    (setq p (point))
    (skip-chars-backward "\n")
    (if (= ?\n (following-char)) (forward-char 1))
    (if (> (point) (1+ p))
	(delete-region (point) p))
    ))

;(defun Manual-nuke-nroff-bs-footers ()
;  ;; Nuke headers: "MORE(1) UNIX Programmer's Manual MORE(1)"
;  (goto-char (point-min))
;  (while (re-search-forward "^ *\\([A-Za-z][-_A-Za-z0-9]*([0-9A-Za-z]+)\\).*\\1$" nil t)
;    (replace-match ""))
;  
;  ;;
;  ;; it would appear that we have a choice between sometimes introducing
;  ;; an extra blank line when a paragraph was broken by a footer, and
;  ;; sometimes not putting in a blank line between two paragraphs when
;  ;; a footer appeared right between them.  FMH; I choose the latter.
;  ;;
;
;  ;; Nuke footers: "Printed 12/3/85	27 April 1981	1"
;  ;;    Sun appear to be on drugz:
;  ;;     "Sun Release 3.0B  Last change: 1 February 1985     1"
;  ;;    HP are even worse!
;  ;;     "     Hewlett-Packard   -1- (printed 12/31/99)"  FMHWA12ID!!
;  ;;    System V (well WICATs anyway):
;  ;;     "Page 1			  (printed 7/24/85)"
;  ;;    Who is administering PCP to these corporate bozos?
;  (goto-char (point-min))
;  (while (re-search-forward
;	   (cond
;	    ((eq system-type 'hpux)
;	     "\n\n?[ \t]*Hewlett-Packard\\(\\| Company\\)[ \t]*- [0-9]* -.*\n")
;	    ((eq system-type 'dgux-unix)
;	     "\n\n?[ \t]*Licensed material--.*Page [0-9]*\n")
;	    ((eq system-type 'usg-unix-v)
;	     "\n\n? *Page [0-9]*.*(printed [0-9/]*)\n")
;	    (t
;	     "\n\n?\\(Printed\\|Sun Release\\) [0-9].*[0-9]\n"))
;	   nil t)
;    (replace-match ""))
;
;  ;;    Also, hack X footers:
;  ;;     "X Version 11         Last change: Release 5         1"
;  (goto-char (point-min))
;  (while (re-search-forward "\n\n?X Version [^\n]+\n" nil t)
;    (replace-match ""))
;
;  ;; Crunch blank lines
;  (goto-char (point-min))
;  (while (re-search-forward "\n\n\n\n*" nil t)
;    (replace-match "\n\n"))
;  )

(defun Manual-mouseify-xrefs ()
  (goto-char (point-min))
  (forward-line 1)
  (let ((case-fold-search nil)
	s e name extent already-fontified)
    ;; possibly it would be faster to rewrite this expression to search for
    ;; a less common sequence first (like "([0-9]") and then back up to see
    ;; if it's really a match.  This function is 15% of the total time, 13%
    ;; of which is this call to re-search-forward.
    (while (re-search-forward "[a-zA-Z_][-a-zA-Z0-9_.]*([0-9][a-zA-Z0-9]*)"
			      nil t)
      (setq s (match-beginning 0)
	    e (match-end 0)
	    name (buffer-substring s e))
      (goto-char s)
      (skip-chars-backward " \t")
      (if (and (bolp)
	       (progn (backward-char 1) (= (preceding-char) ?-)))
	  (progn
	    (setq s (point))
	    (skip-chars-backward "-a-zA-Z0-9_.")
	    (setq name (concat (buffer-substring (point) (1- s)) name))
	    (setq s (point))))
      ;; if there are upper case letters in the section, downcase them.
      (if (string-match "(.*[A-Z]+.*)$" name)
	  (setq name (concat (substring name 0 (match-beginning 0))
			     (downcase (substring name (match-beginning 0))))))
      (setq already-fontified (extent-at s))
      (setq extent (make-extent s e))
      (set-extent-data extent (list 'Manual-follow-xref name))
      (set-extent-attribute extent 'highlight)
      (if (not already-fontified)
	  (set-extent-face extent 'italic))
      (goto-char e))))

(defun Manual-follow-xref (&optional name-or-event)
  "Invoke `manual-entry' on the cross-reference under the mouse.
When invoked noninteractively, the arg may be an xref string to parse instead."
  (interactive "e")
  (if (eventp name-or-event)
      (let* ((p (event-point name-or-event))
	     (extent (and p (extent-at p
			     (window-buffer (event-window name-or-event))
			     'highlight)))
	     (data (and extent (extent-data extent))))
	(if (eq (car-safe data) 'Manual-follow-xref)
	    (eval data)
	  (error "no manual cross-reference there.")))
    (let ((Manual-match-topic-exactly t)
	  (Manual-query-multiple-pages nil))
      (or (manual-entry name-or-event)
	  ;; If that didn't work, maybe it's in a different section than the
	  ;; man page writer expected.  For example, man pages tend assume
	  ;; that all user programs are in section 1, but X tends to generate
	  ;; makefiles that put things in section "n" instead...
	  (and (string-match "[ \t]*([^)]+)\\'" name-or-event)
	       (progn
		 (message "No entries found for %s; checking other sections..."
			  name-or-event)
		 (manual-entry
		  (substring name-or-event 0 (match-beginning 0))
		  nil t)))))))

(defun Manual-popup-menu (&optional event)
  "Pops up a menu of cross-references in this manual page.
If there is a cross-reference under the mouse button which invoked this
command, it will be the first item on the menu.  Otherwise, they are
on the menu in the order in which they appear in the buffer."
  (interactive "e")
  (let ((buffer (current-buffer))
	(sep "---")
	(prefix "Show Manual Page for ")
	xref items)
    (cond (event
	   (setq buffer (window-buffer (event-window event)))
	   (let* ((p (event-point event))
		  (extent (and p (extent-at p buffer 'highlight)))
		  (data (and extent (extent-data extent))))
	     (if (eq (car-safe data) 'Manual-follow-xref)
		 (setq xref (nth 1 data))))))
    (if xref (setq items (list sep xref)))
    (map-extents (function
		  (lambda (extent ignore)
		    (let ((data (extent-data extent)))
		      (if (and (eq (car-safe data) 'Manual-follow-xref)
			       (not (member (nth 1 data) items)))
			  (setq items (cons (nth 1 data) items))))
		    nil))
		 buffer)
    (if (eq sep (car items)) (setq items (cdr items)))
    (popup-menu
     (cons "Manual Entry"
	   (mapcar '(lambda (item)
		      (if (eq item sep)
			  item
			(vector (concat prefix item)
				(list 'Manual-follow-xref item) t)))
		   (nreverse items))))))
