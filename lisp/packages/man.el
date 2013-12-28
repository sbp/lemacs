;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!usc!ucsd!ucbvax!SOMEWHERE.BERKELEY.EDU!aks Fri May 18 20:08:09 EDT 1990
;Article 1962 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!usc!ucsd!ucbvax!SOMEWHERE.BERKELEY.EDU!aks
;>From: aks@SOMEWHERE.BERKELEY.EDU (Alan Stebbens)
;Newsgroups: comp.emacs
;Subject: Fix to man.el to use MANPATH
;Message-ID: <9005180023.AA16456@somewhere>
;Date: 18 May 90 00:23:52 GMT
;Sender: daemon@ucbvax.BERKELEY.EDU
;Lines: 350
;
;After avoiding the use of M-x manual-entry because it didn't know
;about alternate man directories, as given by MANPATH, it finally
;occurred to me that it's silly to not use Emacs to read the man
;pages, and that it shouldn't be that hard to make "manual-entry"
;Do The Right Thing.  Well it wasn't very hard, and it wasn't a
;major rewrite, although it was more than a minor one.
;
;The features of this version are:
;
;  o  Match multiple man pages using TOPIC as a simple pattern
;  o  Search unformatted pages, even when formatted matches are found
;  o  Query the user as to which pages are desired
;  o  Use of the prefix arg to toggle/bypass the above features
;  o  Buffers named by the first topic in the buffer
;  o  Automatic uncompress for compressed man pages (.Z and .z)
;  o  View the resulting buffer using M-x view mode
;
;All the features may be disabled to achieve the (limited)
;features of the original M-x manual-entry.
;
;This is a first cut.  Send improvements, fixes, and comments to me
;via email.  
;
;Alan Stebbens        <aks@hub.ucsb.edu>             (805) 961-3221
;     Center for Computational Sciences and Engineering (CCSE)
;          University of California, Santa Barbara (UCSB)
;           3111 Engineering I, Santa Barbara, CA 93106
;
; Modified 16-mar-91 by Jamie Zawinski <jwz@lucid.com> to default the 
; manual topic to the symbol at point, just like find-tag does.
;
;The following is the complete file; the diffs were so extensive as
;to be almost as large as the file itself.
;======================================================================
;; Read in and display parts of Unix manual.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;
;;
;; Written by Alan K. Stebbens, CCSE, Univ. of CA, Santa Barbara
;;
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
;;
;; Last edited:
;; 
;; Thu May 17 14:35:17 1990 by Alan Stebbens (aks at somewhere)
;; 	 Added force -- allow unformatted search even with
;; 	 formatted finds.
;; 	 Broke out heavily nested mapcars into the functions
;; 	 Manual-select-directories and Manual-select-man-pages.
;; 	 Fixed bugs.
;; 	 Renamed "Manual.el"
;; 	 Added multiple man pages query code.
;; 
;; Wed Apr 11 13:42:31 1990 by Alan Stebbens (aks at somewhere)
;; 	 Initial rewrite

(defconst Manual-program "man" "\
*Name of the program to invoke in order to format the source man pages.")

(defvar Manual-topic-buffer t "\
*Non-nil means \\[Manual-entry] should output the manual entry for TOPIC into
a buffer named *TOPIC Manual Entry*, otherwise, it should name the buffer
*Manual Entry*.")

(defvar Manual-buffer-view-mode t "\
*Non-nil means that \\[view-buffer] is used to display the output from
\\[Manual-entry]; nil means that the buffer is left in fundamental-mode
in another window.")

(defvar Manual-match-topic-exactly nil "\
*Non-nil means that \\[manual-entry] will match the given TOPIC exactly, rather
apply it as a pattern.  When this is nil, and \"Manual-query-multiple-pages\" is
non-nil, then \\[manual-entry] will query you for all matching TOPICs.
This variable only has affect on the preformatted man pages (the \"cat\" files),
since the \"man\" command always does exact topic matches.")

(defvar Manual-query-multiple-pages t "\
*Non-nil means that \\[manual-entry] will query the user about multiple man
pages which match the given topic.  The query is done using the function 
\"y-or-n-p\".  If this variable is nil, all man pages with topics matching the
topic given to \\[manual-entry] will be inserted into the temporary buffer.
See the variable \"Manual-match-topic-exactly\" to control the matching.")

(defvar Manual-directory-list nil "\
*A list of directories used with the \"man\" command, where each directory
contains a set of \"man?\" and \"cat?\" subdirectories.  If this variable is nil,
it is initialized by \\[Manual-directory-list-init].")

(defvar Manual-formatted-directory-list nil "\
A list of directories containing formatted man pages.  Initialized by
\\[Manual-directory-list-init].")

(defvar Manual-unformatted-directory-list nil "\
A list of directories containing the unformatted (source) man pages.  Initialized
by \\[Manual-directory-list-init].")

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
      (let ((manpath (getenv "MANPATH")) dirlist)
	(while (and manpath (not (null (string-match "[^:]+" manpath))))
	  (setq dirlist (nconc dirlist (list (substring manpath 0 (match-end 0)))))
	  (setq manpath (if (< (match-end 0) (length manpath))
			    (substring manpath (1+ (match-end 0))))))
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

(defun manual-entry (topic &optional arg)
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
   (list (let* ((fmh "-A-Za-z0-9_")
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
  (Manual-directory-list-init nil)
  (let ((case-fold-search nil)		; let search be easy
	(hook (and (boundp 'temp-buffer-show-hook) temp-buffer-show-hook))
	(exact (if (or (= arg 2)(= arg 4))
		   (not Manual-match-topic-exactly)
		 Manual-match-topic-exactly))
	(force (>= arg 3))
	section fmtlist manlist apropos-mode)
    (if (and (null section)
	     (string-match "\\`[ \t]*\\([^( \t]+\\)[ \t]*(\\(.+\\))[ \t]*\\'" topic))
	(setq section (substring topic (match-beginning 2)
				 (match-end 2))
	      topic (substring topic (match-beginning 1)
			       (match-end 1))))
    (if (equal section "-k")
	(setq apropos-mode t)
      (message "Looking for formatted entry for %s%s..."
	       topic (if section (concat "(" section ")") ""))
      (setq fmtlist (Manual-select-man-pages
		     (Manual-select-directories Manual-formatted-directory-list section) 
		     topic section exact))
      (if (or force (not fmtlist))
	  (progn
	    (message "%sooking for unformatted entry for %s%s..."
		     (if fmtlist "L" "No formatted entry, l")
		     topic (if section (concat "(" section ")") ""))
	    (setq manlist (Manual-select-man-pages
			   (Manual-select-directories Manual-unformatted-directory-list section)
			   topic section exact)))))
    (if (or fmtlist manlist apropos-mode)
	(let* ((name (car (or fmtlist manlist)))
	       (bufname (concat (if Manual-topic-buffer
				    (if apropos-mode
					(concat "*" topic " ")
				      (concat "*" (and (string-match "/\\([^/]+\\)$" name)
						       (substring name (match-beginning 1) (match-end 1)))
					      " ")))
				(if apropos-mode
				    "*Manual Apropos*" "*Manual Entry*"))))

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

	  (with-output-to-temp-buffer bufname
	    (buffer-flush-undo standard-output)
	    (save-excursion
	      (set-buffer standard-output)
	      (if fmtlist		; insert any formatted files
		  (mapcar (function (lambda (name)
			     (goto-char (point-max))
			     ;; In case the file can't be read or uncompressed or something like that.
			     (condition-case ()
				 (Manual-insert-man-file name)
			       (file-error nil))
			    (goto-char (point-max))
			    (insert "\n\n-----\n")))
			  fmtlist))
	      (if manlist		; process any unformatted files
		  (mapcar (function (lambda (name)
			     (let (topic section)
			       (string-match "\\([^/]+\\)\\.\\([^./]+\\)$" name)
			       (setq topic (substring name (match-beginning 1) (match-end 1)))
			       (setq section (substring name (match-beginning 2) (match-end 2)))
			       (message "Invoking man %s %s ..." section topic)
			       (call-process Manual-program nil t nil section topic))
			    (insert "\n\n-----\n")))
			  manlist))
	      (if (< (buffer-size) 80)
		  (progn
		    (goto-char (point-min))
		    (end-of-line)
		    (error (buffer-substring 1 (point)))))
	      (message "Cleaning manual entr%s..." 
		       (if (> (length (or fmtlist manlist)) 1) "ies" (concat "y for " topic)))
	      (Manual-nuke-nroff-bs)
	      (set-buffer-modified-p nil)
	      (setq hook (and (boundp 'temp-buffer-show-hook) temp-buffer-show-hook));emacs19
	      (if Manual-buffer-view-mode
		  (setq temp-buffer-show-hook 'view-buffer))
	      (message ""))))
      (message "No entries found for %s%s" topic (if section (concat "(" section ")") "")))
    (setq temp-buffer-show-hook hook)))

;; Manual-select-subdirectories
;; Given a DIRLIST and a SUBDIR name, return all subdirectories of the former which
;; match the latter.

(defun Manual-select-subdirectories (dirlist subdir)
  (apply 'append (mapcar '(lambda (dir)
			   (and (file-exists-p dir)
			        (mapcar '(lambda (name) (concat dir "/" name))
				         (file-name-all-completions subdir dir))))
			 dirlist)))

;; Manual-select-directories
;;
;; Select from DIRLIST the appropriate directories by SECTION.
;; Return selected directories in a list.  If SECTION is nil, select
;; all SECTION directories.

(defun Manual-select-directories (dirlist section)
  (mapcar (function (lambda (fmtdir)
	     (if (or (not section)
		     (string-match (concat (substring section 0 1) "/$") fmtdir))
		 fmtdir)))
	  dirlist))

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

;; Hint: BS stands form more things than "back space"
(defun Manual-nuke-nroff-bs ()
  (interactive "*")
  ;; Nuke underlining and overstriking (only by the same letter)
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (let* ((preceding (char-after (- (point) 2)))
	   (following (following-char)))
      (cond ((or (= preceding following)	; x\bx
		 (= preceding ?\_))     	; _\b
	     (delete-char -2))
	    ((or (= following ?\_)		; \b_
		 (= following ?\ ))             ; \b(SPACE)
	     (delete-region (1- (point)) (1+ (point))))
	    (t (delete-char -1)))))		; \b by itself (remove it)
	    

  ;; Nuke headers: "MORE(1) UNIX Programmer's Manual MORE(1)"
  (goto-char (point-min))
  (while (re-search-forward "^ *\\([A-Za-z][-_A-Za-z0-9]*([0-9A-Za-z]+)\\).*\\1$" nil t)
    (replace-match ""))
  
  ;; Nuke footers: "Printed 12/3/85	27 April 1981	1"
  ;;    Sun appear to be on drugz:
  ;;     "Sun Release 3.0B  Last change: 1 February 1985     1"
  ;;    HP are even worse!
  ;;     "     Hewlett-Packard   -1- (printed 12/31/99)"  FMHWA12ID!!
  ;;    System V (well WICATs anyway):
  ;;     "Page 1			  (printed 7/24/85)"
  ;;    Who is administering PCP to these corporate bozos?
  (goto-char (point-min))
  (while (re-search-forward
	   (cond ((eq system-type 'hpux)
		  "^[ \t]*Hewlett-Packard\\(\\| Company\\)[ \t]*- [0-9]* -.*$")
		 ((eq system-type 'usg-unix-v)
		  "^ *Page [0-9]*.*(printed [0-9/]*)$")
		 (t
		  "^\\(Printed\\|Sun Release\\) [0-9].*[0-9]$"))
	   nil t)
    (replace-match ""))

  ;; Zap ESC7,  ESC8, and ESC9
  ;; This is for Sun man pages like "man 1 csh"
  (goto-char (point-min))
  (while (re-search-forward "\e[789]" nil t)
    (replace-match ""))

  ;; Crunch blank lines
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n\n*" nil t)
    (replace-match "\n\n"))

  ;; Nuke blanks lines at start.
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (delete-region (point-min) (point)))

(fset 'nuke-nroff-bs 'Manual-nuke-nroff-bs) ; use old name


(defun Manual-insert-man-file (name)
  ;; Insert manual file (unpacked as necessary) into buffer
  (if (equal (substring name -2) ".Z")
      (call-process "zcat" nil t nil name)
    (if (equal (substring name -2) ".z")
	(call-process "pcat" nil t nil name)
      (insert-file-contents name))))


