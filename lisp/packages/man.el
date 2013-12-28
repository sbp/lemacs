;; Find and display Unix manual pages.
;; Copyright (C) 1985, 1993, 1994 Free Software Foundation, Inc.
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
;;  o  Automatic uncompress for compressed man pages (.Z, .z, and .gz)
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
;;
;; Modified 4-apr-94 by jwz: merged in Tibor Polgar's code for manpath.conf.
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
;;	Manual-page-history

(defvar Manual-program "man" "\
*Name of the program to invoke in order to format the source man pages.")

(defvar Manual-section-switch (if (eq system-type 'usg-unix-v) "-s" nil)
  "SysV needs this to work right.")

(defvar Manual-topic-buffer t "\
*Non-nil means \\[Manual-entry] should output the manual entry for TOPIC into
a buffer named *man TOPIC*, otherwise, it should name the buffer
*Manual Entry*.")

(defvar Manual-buffer-view-mode t "\
*Whether manual buffers should be placed in view-mode.
nil means leave the buffer in fundamental-mode in another window.
t means use `view-buffer' to display the man page in the current window.
Any other value means use `view-buffer-other-window'.")

(defvar Manual-match-topic-exactly t "\
*Non-nil means that \\[manual-entry] will match the given TOPIC exactly, rather
apply it as a pattern.  When this is nil, and \"Manual-query-multiple-pages\"
is non-nil, then \\[manual-entry] will query you for all matching TOPICs.
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

(defvar Manual-manpath-config-file "/usr/lib/manpath.config"
  "*Location of the manpath.config file, if any.")

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

(defun Manual-directory-list-init (&optional arg) 
  "Initialize the Manual-directory-list variable from $MANPATH
if it is not already set, or if a prefix argument is provided."
  (interactive "P")
  (if arg (setq Manual-directory-list nil))
  (if (null Manual-directory-list)
      (let ((manpath (getenv "MANPATH"))
	    (global (Manual-manpath-config-contents))
	    (dirlist nil)
	    dir)
	(cond ((and manpath global)
	       (setq manpath (concat manpath ":" global)))
	      (global
	       (setq manpath global)))
	(while (string-match "\\`:*\\([^:]+\\)" manpath)
	  (setq dir (substring manpath (match-beginning 1) (match-end 1)))
	  (and (not (member dir dirlist))
	       (setq dirlist (cons dir dirlist)))
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

(defun Manual-manpath-config-contents ()
  "Parse the `Manual-manpath-config-file' file, if any.
Returns a string like in $MANPATH."
  (if (and Manual-manpath-config-file
	   (file-readable-p Manual-manpath-config-file))
      (let ((buf (get-buffer-create " *Manual-config*"))
	    path)
	(set-buffer buf)
	(buffer-disable-undo buf)
	(erase-buffer)
	(insert-file-contents Manual-manpath-config-file)
	(while (re-search-forward "^\\(MANDATORY_MANPATH\\|MANPATH_MAP\\)"
				  nil t)
	  (and (re-search-forward "\\(/[^ \t\n]+\\)[ \t]*$")
	       (setq path (concat path (buffer-substring (match-beginning 1)
							 (match-end 1))
				  ":"))))
	(kill-buffer buf)
	path)))
;;
;; manual-entry  -- The "main" user function
;;

;;;###autoload
(defun manual-entry (topic &optional arg silent)
  "Display the Unix manual entry (or entries) for TOPIC.
If prefix arg is given, modify the search according to the value:
  2 = complement default exact matching of the TOPIC name;
      exact matching default is specified by `Manual-match-topic-exactly'
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
  (let ((exact (if (or (= arg 2) (= arg 4))
		   (not Manual-match-topic-exactly)
		 Manual-match-topic-exactly))
	(force (if (>= arg 3)
                   t
                   nil))
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
                      Manual-formatted-directory-list
                      topic section exact '()))
      (if (or force (not section) (null fmtlist))
	  (progn
	    (or silent
		(message "%sooking for unformatted entry for %s%s..."
			 (if fmtlist "L" "No formatted entry, l")
			 topic (if section (concat "(" section ")") "")))
	    (setq manlist (Manual-select-man-pages
                            Manual-unformatted-directory-list
                            topic section exact (if force '() fmtlist))))))

    ;; Delete duplicate man pages (a file of the same name in multiple
    ;; directories.)
    (or nil ;force
        (let ((rest (append fmtlist manlist)))
          (while rest
            (let ((rest2 (cdr rest)))
              (while rest2
                (if (equal (file-name-nondirectory (car rest))
                           (file-name-nondirectory (car rest2)))
                    (setq fmtlist (delq (car rest2) fmtlist)
                          manlist (delq (car rest2) manlist)))
                (setq rest2 (cdr rest2))))
            (setq rest (cdr rest)))))

    (if (not (or fmtlist manlist apropos-mode))
        (progn
          (message "No entries found for %s%s" topic
                   (if section (concat "(" section ")") ""))
          nil)
      (let ((bufname (cond ((not Manual-topic-buffer)
                            ;; What's the point of retaining this?
                            (if apropos-mode
                                "*Manual Apropos*"
                                "*Manual Entry*"))
                           (apropos-mode
                            (concat "*man apropos " topic "*"))
                           (t
                            (concat "*man "
                                    (cond (exact
                                           (if section
                                               (concat topic "." section)
                                               topic))
                                          ((or (cdr fmtlist) (cdr manlist)
                                               (and fmtlist manlist))
                                           ;; more than one entry found
                                           (concat topic "..."))
                                          (t
                                           (file-name-nondirectory
                                            (car (or fmtlist manlist)))))
                                    "*"))))
            (temp-buffer-show-function 
             (cond ((eq 't Manual-buffer-view-mode) 'view-buffer)
                   ((eq 'nil Manual-buffer-view-mode)
                    temp-buffer-show-function)
                   (t 'view-buffer-other-window))))

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
	       (Manual-insert-pages fmtlist manlist apropos-mode)
               (set-buffer-modified-p nil)
               (Manual-mode)
               ))))
        (setq Manual-page-history
              (cons (buffer-name)
                    (delete (buffer-name) Manual-page-history)))
        (message nil)
        t))))

(defun Manual-insert-pages (fmtlist manlist apropos-mode)
  (let ((sep (make-string 65 ?-))
	name start end topic section)
    (while fmtlist			; insert any formatted files
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
	  (Manual-nuke-nroff-bs)
	  (goto-char (point-min))
	  (insert "File: " name "\n")
	  (goto-char (point-max))
	  ))
      (if (or (cdr fmtlist) manlist)
	  (insert "\n\n" sep "\n"))
      (setq fmtlist (cdr fmtlist)))

    (while manlist			; process any unformatted files
      (setq name (car manlist))
      (string-match "\\([^/]+\\)\\.\\([^./]+\\)\\'" name)
      (setq topic (substring name (match-beginning 1) (match-end 1)))
      (setq section (substring name (match-beginning 2) (match-end 2)))
      (message "Invoking man %s%s %s..."
	       (if Manual-section-switch
		   (concat Manual-section-switch " ")
		 "")
	       section topic)
      (setq start (point))
      (Manual-run-formatter name topic section)
      (setq end (point))
      (save-excursion
	(save-restriction
	  (message "Cleaning manual entry for %s(%s)..." topic section)
	  (narrow-to-region start end)
	  (Manual-nuke-nroff-bs apropos-mode)
	  (goto-char (point-min))
	  (insert "File: " name "\n")
	  (goto-char (point-max))
	  ))
      (if (cdr manlist)
	  (insert "\n\n" sep "\n"))
      (setq manlist (cdr manlist))))
  (if (< (buffer-size) 200)
      (progn
	(goto-char (point-min))
	(if (looking-at "^File: ")
	    (forward-line 1))
	(error (buffer-substring (point) (progn (end-of-line) (point))))))
  nil)


(defun Manual-run-formatter (name topic section)
  (cond ((string-match "roff\\'" Manual-program)
	 ;; kludge kludge
	 (call-process Manual-program nil t nil "-Tman" "-man" name))
	(Manual-section-switch
	 (call-process Manual-program nil t nil Manual-section-switch
		       section topic))
	(t
	 (call-process Manual-program nil t nil section topic))))


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
  (let ((dirs '())
        (case-fold-search nil)
        (match (concat "\\`" (regexp-quote subdir)))
        d)
    (setq dirlist (reverse dirlist))
    (while dirlist
      (setq d (car dirlist) dirlist (cdr dirlist))
      (if (file-directory-p d)
          (let ((files (directory-files d t match nil 'dirs-only))
		(dir-temp '()))
            (while files
              (if (file-executable-p (car files))
                  (setq dir-temp (cons (file-name-as-directory (car files))
                                   dir-temp)))
              (setq files (cdr files)))
	    (and dir-temp
		 (setq dirs (append (nreverse dir-temp)
				    dirs))))))
    dirs))


(defvar Manual-bogus-file-pattern "\\.\\(lpr\\|ps\\|PS\\)\\'"
  "Some systems have files in the man/man*/ directories which aren't man pages.
This pattern is used to prune those files.")

;; Manual-select-man-pages
;;
;; Given a DIRLIST, discover all filenames which complete given the TOPIC
;; and SECTION.

;; ## Note: BSD man looks for .../man1/foo.1 and .../man1/$MACHINE/foo.1

(defun Manual-select-man-pages (dirlist topic section exact shadow)
  (let ((case-fold-search nil))
    (if (not section)
        (setq dirlist (reverse dirlist))
        (let ((l '())
              (match (concat (substring section 0 1) "/?\\'"))
              d)
          (setq dirlist (reverse dirlist))
          (while dirlist
            (setq d (car dirlist) dirlist (cdr dirlist))
            (if (string-match match d)
                (setq l (cons d l))))
          (setq dirlist l)))
    (if shadow
        (setq shadow (concat "/\\("
                             (mapconcat #'(lambda (n)
                                            (regexp-quote
                                             (file-name-nondirectory n)))
                                        shadow
                                        "\\|")
                             "\\)\\'")))
    (let ((manlist '())
          (match (concat "\\`"
                           (regexp-quote topic)
                           (cond (section
                                  (concat "\\." (regexp-quote section)))
                                 (exact
                                  ;; If Manual-match-topic-exactly is
                                  ;; set, then we must make sure the
                                  ;; completions are exact, except for
                                  ;; trailing weird characters after
                                  ;; the section.
                                  "\\.")
                                 (t
                                  ""))))
          dir)
      (while dirlist
        (setq dir (car dirlist) dirlist (cdr dirlist))
        (if (not (file-directory-p dir))
            (progn
              (message "warning: %s is not a directory" dir)
              ;;(sit-for 1)
              )
            (let ((files (directory-files dir t match nil t))
                  f)
              (while files
                (setq f (car files) files (cdr files))
                (cond ((string-match Manual-bogus-file-pattern f)
		       ;(message "Bogus fule %s" f) (sit-for 2)
                       )
		      ((and shadow (string-match shadow f))
                       ;(message "Shadowed %s" f) (sit-for 2)
                       )
                      ((not (file-readable-p f))
                       ;(message "Losing with %s" f) (sit-for 2)
                       )
                      (t
                       (setq manlist (cons f manlist))))))))
      (setq manlist (nreverse manlist))
      (if (and manlist Manual-query-multiple-pages)
          (apply #'append
                 (mapcar #'(lambda (page)
                             (and page 
                                  (y-or-n-p (format "Read %s? " page)))
                             (list page))
                         manlist))
          manlist))))

(defun Manual-insert-man-file (name)
  ;; Insert manual file (unpacked as necessary) into buffer
  (cond ((equal (substring name -3) ".gz")
	 (call-process "gunzip" nil t nil "--stdout" name))
        ((or (equal (substring name -2) ".Z")
	     ;; HPUX uses directory names that end in .Z and compressed
	     ;; files that don't.  How gratuitously random.
             (let ((case-fold-search nil))
               (string-match "\\.Z/" name)))
	 (call-process "zcat" nil t nil name))
	((equal (substring name -2) ".z")
	 (call-process "pcat" nil t nil name))
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
  ;;
  (goto-char (point-min))
  (while (search-forward "_\b" nil t)
    ;; searching for underscore-backspace and then comparing the following
    ;; chars until the sequence ends turns out to be much faster than searching
    ;; for a regexp which matches the whole sequence.
    (let ((s (match-beginning 0)))
      (goto-char s)
      (while (and (= (following-char) ?_)
		  (= (char-after (1+ (point))) ?\b))
	(Manual-delete-char 2)
	(forward-char 1))
      (set-extent-face (make-extent s (point)) 'man-italic)))
  ;;
  ;; turn overstriking into bold
  ;;
  (goto-char (point-min))
  (while (re-search-forward "\\([^\n]\\)\\(\b\\1\\)" nil t)
    ;; Surprisingly, searching for the above regexp is faster than searching
    ;; for a backspace and then comparing the preceding and following chars,
    ;; I presume because there are many false matches, meaning more funcalls
    ;; to re-search-forward.
    (let ((s (match-beginning 0)))
      (goto-char s)
      ;; Some systems (SGI) overstrike multiple times, eg, "M\bM\bM\bM".
      (while (looking-at "\\([^\n]\\)\\(\b\\1\\)+")
	(delete-region (+ (point) 1) (match-end 0))
	(forward-char 1))
      (set-extent-face (make-extent s (point)) 'man-bold)))
  ;;
  ;; hack bullets: o^H+ --> +
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (Manual-delete-char -2))

  (if (> (buffer-size) 100) ; minor kludge
      (Manual-nuke-nroff-bs-footers))
  ;;
  ;; turn subsection header lines into bold
  ;;
  (goto-char (point-min))
  (if apropos-mode
      (while (re-search-forward "[a-zA-Z0-9] ([0-9]" nil t)
	(forward-char -2)
	(delete-backward-char 1))

;    (while (re-search-forward "^[^ \t\n]" nil t)
;      (set-extent-face (make-extent (match-beginning 0)
;				    (progn (end-of-line) (point)))
;		       'man-heading))

    ;; boldface the first line
    (if (looking-at "[^ \t\n].*$")
	(set-extent-face (make-extent (match-beginning 0) (match-end 0))
			 'man-bold))

    ;; boldface subsequent title lines
    ;; Regexp to match section headers changed to match a non-indented
    ;; line preceded by a blank line and followed by an indented line. 
    ;; This seems to work ok for manual pages but gives better results
    ;; with other nroff'd files
    (while (re-search-forward "\n\n\\([^ \t\n].*\\)\n[ \t]+[^ \t\n]" nil t)
      (goto-char (match-end 1))
      (set-extent-face (make-extent (match-beginning 1) (match-end 1))
		       'man-bold)
      (forward-line 1))
    )

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
      (set-extent-property extent 'man (list 'Manual-follow-xref name))
      (set-extent-property extent 'highlight t)
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
	     (data (and extent (extent-property extent 'man))))
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
		  (data (and extent (extent-property extent 'man))))
	     (if (eq (car-safe data) 'Manual-follow-xref)
		 (setq xref (nth 1 data))))))
    (if xref (setq items (list sep xref)))
    (map-extents #'(lambda (extent ignore)
		     (let ((data (extent-property extent 'man)))
		       (if (and (eq (car-safe data) 'Manual-follow-xref)
				(not (member (nth 1 data) items)))
			   (setq items (cons (nth 1 data) items)))
		    nil))
		 buffer)
    (if (eq sep (car items)) (setq items (cdr items)))
    (let ((popup-menu-titles nil))
      (popup-menu
       (cons "Manual Entry"
	     (mapcar #'(lambda (item)
			 (if (eq item sep)
			     item
                           (vector (concat prefix item)
                                   (list 'Manual-follow-xref item) t)))
		     (nreverse items)))))))

(provide 'man)
