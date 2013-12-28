;;; BibTeX mode for GNU Emacs
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

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

;;; Marc Shapiro 1-feb-89: integrated changes by Bengt Martensson 88-05-06:
;;;   Added Sun menu support.  Locally bound to right mouse button in 
;;;   bibtex-mode.  Emacs 18.49 allows local mouse bindings!!
;;;   Commented out DEAthesis.

;;; Marc Shapiro 6-oct-88
;;;  * use indent-to-column instead of inserting tabs (changes to 
;;;    bibtex-entry, bibtex-make-entry, bibtex-make-OPT-entry, renamed to
;;;    bibtex-make-optional-entry)
;;;  * C-c C-k deletes the current OPT entry entirely
;;;  * C-c C-d replaces text of field with ""
;;;  * renamed bibtex-find-it to bibtex-find-text.  With arg, now goes to
;;;    start of text.  Fixed bugs in it.

;;; Marc Shapiro 23-sep-88
;;;  * bibtex-clean-entry moves past end of entry.
;;;  * bibtex-clean-entry signals mandatory fields left empty.

;;; Marc Shapiro 18-jul-88
;;;  * Moved all the entry type keystrokes to "C-c C-e something" (instead of
;;;    "C-c something" previously) to make room for more.  C-c C-e is
;;;    supposed to stand for "entry" [idea taken from mail-mode].  Moved
;;;    bibtex-pop-previous to C-c C-p and bibtex-pop-next to C-c C-n.
;;;  * removed binding for "\e[25~"
;;;  * replaced bibtex-clean-optionals by bibtex-clean-entry, bound to
;;;    C-c C-c

;;; Marc Shapiro 13-jul-88 [based on ideas by Sacha Krakowiak of IMAG]
;;;  * bibtex-pop-previous replaces current field with value of
;;;    similar field in previous entry.  May be called n times in a row
;;;    (or with arg n) to pop similar field of n'th previous entry.
;;;    There is also a bibtex-pop-next to get similar field of next
;;;    entry.
;;;  * C-c C-k now kills all empty optional fields of current entry, and
;;;    removes "OPT" for those optional fields which have text. 

;;; Marc Shapiro 14-dec-87
;;;   Cosmetic fixes.  Fixed small bug in bibtex-move-outside-of-entry.
;;; Skip Montanaro <steinmetz!sprite!montanaro> 7-dec-87, Shapiro 10-dec-87
;;;   before inserting an entry, make sure we are outside of a bib entry
;;; Marc Shapiro 3-nov-87
;;;   addition for France: DEAthesis
;;; Marc Shapiro 19-oct-1987
;;;   add X window menu option; bug fixes. TAB, LFD, C-c " and C-c C-o now
;;;   behave consistently; deletion never occurs blindly.
;;; Marc Shapiro <shapiro@inria.inria.fr> 15-oct-1986
;;;    align long lines nicely; C-c C-o checks for the "OPT" string;
;;;    TAB goes to the end of the string; use lower case; use
;;;    run-hooks

;;; Bengt Martensson <ubrinf!mond!bengt> 87-06-28
;;;   Original version

;;; NOTE by Marc Shapiro, 14-dec-87:
;;; (bibtex-x-environment) binds an X menu for bibtex mode to x-button-c-right.
;;; Trouble is, in Emacs 18.44 you can't have a mode-specific mouse binding,
;;; so it will remain active in all windows.  Yuck!

(defvar bibtex-mode-syntax-table nil "")
(defvar bibtex-mode-abbrev-table nil "")
(define-abbrev-table 'bibtex-mode-abbrev-table ())
(defvar bibtex-mode-map (make-sparse-keymap) "")
(defvar bibtex-pop-previous-search-point nil
  "Next point where bibtex-pop-previous should start looking for a similar
entry.")
(defvar bibtex-pop-next-search-point nil
  "Next point where bibtex-pop-next should start looking for a similar
entry.")

;;; A bibtex file is a sequence of entries, either string definitions
;;; or reference entries.  A reference entry has a type part, a
;;; key part, and a comma-separated sequence of fields.  A string
;;; entry has a single field.  A field has a left and right part,
;;; separated by a '='.  The left part is the name, the right part is
;;; the text.  Here come the definitions allowing to create and/or parse
;;; entries and fields:

;;; fields
(defun bibtex-cfield (name text)
  "Create a regexp for a bibtex field of name NAME and text TEXT"
  (concat ",[ \t\n]*\\("
	  name
	  "\\)[ \t\n]*=[ \t\n]*\\("
	  text
	  "\\)"))
(defconst bibtex-name-in-cfield 1
  "The regexp subexpression number of the name part in bibtex-cfield.")
(defconst bibtex-text-in-cfield 2
  "The regexp subexpression number of the text part in bibtex-cfield.")

(defconst bibtex-field-name "[A-Za-z][---A-Za-z0-9:_+]*"
  "Regexp defining the name part of a bibtex field.")
(defconst bibtex-field-text
  "\"[^\"]*[^\\\\]\"\\|\"\"\\|[0-9A-Za-z][---A-Za-z0-9:_+]*"
  "Regexp defining the text part of a bibtex field: either a string, or an empty string, or a constant.")
(defconst bibtex-field
  (bibtex-cfield bibtex-field-name bibtex-field-text)
  "Regexp defining the format of a bibtex field")

(defconst bibtex-name-in-field bibtex-name-in-cfield
  "The regexp subexpression number of the name part in bibtex-field")
(defconst bibtex-text-in-field bibtex-text-in-cfield
  "The regexp subexpression number of the text part in bibtex-field")

;;; references
(defconst bibtex-reference-type
  "@[A-Za-z]+"
  "Regexp defining the type part of a bibtex reference entry")
(defconst bibtex-reference-head
  (concat "^[ \t]*\\("
	  bibtex-reference-type
	  "\\)[ \t]*[({]\\("
	  bibtex-field-name
	  "\\)")
  "Regexp defining format of the header line of a bibtex reference entry")
(defconst bibtex-type-in-head 1
  "The regexp subexpression number of the type part in bibtex-reference-head")
(defconst bibtex-key-in-head 2
  "The regexp subexpression number of the key part in
bibtex-reference-head")

(defconst bibtex-reference
  (concat bibtex-reference-head
	  "\\([ \t\n]*" bibtex-field "\\)*"
	  "[ \t\n]*[})]")
  "Regexp defining the format of a bibtex reference entry")
(defconst bibtex-type-in-reference bibtex-type-in-head
  "The regexp subexpression number of the type part in bibtex-reference")
(defconst bibtex-key-in-reference bibtex-key-in-head
  "The regexp subexpression number of the key part in
bibtex-reference")

;;; strings
(defconst bibtex-string
  (concat "^[ \t]*@[sS][tT][rR][iI][nN][gG][ \t\n]*[({][ \t\n]*\\("
	  bibtex-field-name
	  "\\)[ \t\n]*=[ \t\n]*\\("
	  bibtex-field-text
	  "\\)[ \t\n]*[})]")
  "Regexp defining the format of a bibtex string entry")
(defconst bibtex-name-in-string 1
  "The regexp subexpression of the name part in bibtex-string")
(defconst bibtex-text-in-string 2
  "The regexp subexpression of the text part in bibtex-string")

(defconst bibtex-name-alignement 2
  "Alignment for the name part in BibTeX fields.
Chosen on aesthetic grounds only.")

(defconst bibtex-text-alignment (length "  organization = ")
  "Alignment for the text part in BibTeX fields.
Equal to the space needed for the longest name part.")

;;; bibtex mode:

(defun bibtex-mode () 
  "Major mode for editing bibtex files.

\\{bibtex-mode-map}

A command such as \\[bibtex-Book] will outline the fields for a BibTeX book entry.

The optional fields start with the string OPT, and thus ignored by BibTeX.
The OPT string may be removed from a field with \\[bibtex-remove-OPT].
\\[bibtex-kill-optional-field] kills the current optional field entirely.
\\[bibtex-remove-double-quotes] removes the double-quotes around the text of
the current field.  \\[bibtex-empty-field] replaces the text of the current
field with the default \"\".

The command \\[bibtex-clean-entry] cleans the current entry, i.e. (i) removes
double-quotes from entirely numerical fields, (ii) removes OPT from all
non-empty optional fields, (iii) removes all empty optional fields, and (iv)
checks that no non-optional fields are empty.

Use \\[bibtex-find-text] to position the dot at the end of the current field.
Use \\[bibtex-next-field] to move to end of the next field.

\\[bibtex-x-environment] binds a mode-specific X menu to control+right
mouse button.
\\[bibtex-sun-environment] binds a mode-specific Sun menu to right
mouse button.

Fields:
    address
           Publisher's address
    annote
           Long annotation used for annotated bibliographies (begins sentence)
    author
           Name(s) of author(s), in BibTeX name format
    booktitle
           Book title when the thing being referenced isn't the whole book.
           For book entries, the title field should be used instead.
    chapter
           Chapter number
    edition
           Edition of a book (e.g., \"second\")
    editor
           Name(s) of editor(s), in BibTeX name format.
           If there is also an author field, then the editor field should be
           for the book or collection that the work appears in
    howpublished
            How something strange has been published (begins sentence)
    institution
           Sponsoring institution
    journal
           Journal name (macros are provided for many)
    key
           Alphabetizing and labeling key (needed when no author or editor)
    month
           Month (macros are provided)
    note
           To help the reader find a reference (begins sentence)
    number
           Number of a journal or technical report
    organization
           Organization (sponsoring a conference)
    pages
           Page number or numbers (use `--' to separate a range)
    publisher
           Publisher name
    school
           School name (for theses)
    series
           The name of a series or set of books.
           An individual book will will also have it's own title
    title
           The title of the thing being referenced
    type
           Type of a technical report (e.g., \"Research Note\") to be used
           instead of the default \"Technical Report\"
    volume
           Volume of a journal or multivolume work
    year
           Year---should contain only numerals
---------------------------------------------------------
Entry to this mode calls the value of bibtex-mode-hook if that value is
non-nil."
  (interactive)
  (kill-all-local-variables)
  (if bibtex-mode-syntax-table
      (set-syntax-table bibtex-mode-syntax-table)
     (setq bibtex-mode-syntax-table (make-syntax-table))
     (set-syntax-table bibtex-mode-syntax-table)
     (modify-syntax-entry ?\" ".")
     (modify-syntax-entry ?$ "$$  ")
     (modify-syntax-entry ?% "<   ")
     (modify-syntax-entry ?'  "w   ")
     (modify-syntax-entry ?@  "w   ")
     (modify-syntax-entry ?\\ "\\")
     (modify-syntax-entry ?\f ">   ")
     (modify-syntax-entry ?\n ">   ")
     (modify-syntax-entry ?~ " "))
  (use-local-map bibtex-mode-map)
  (setq major-mode 'bibtex-mode)


  (setq mode-name "BibTeX")
  (set-syntax-table bibtex-mode-syntax-table)
  (setq local-abbrev-table bibtex-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^[ \f\n\t]*$")

  (define-key bibtex-mode-map "\t" 'bibtex-find-text)
  (define-key bibtex-mode-map "\n" 'bibtex-next-field)
  (define-key bibtex-mode-map "\C-c\"" 'bibtex-remove-double-quotes)
  (define-key bibtex-mode-map "\C-c\C-c" 'bibtex-clean-entry)
  (define-key bibtex-mode-map "\C-c?" 'describe-mode)
  (define-key bibtex-mode-map "\C-c\C-p" 'bibtex-pop-previous)
  (define-key bibtex-mode-map "\C-c\C-n" 'bibtex-pop-next)
  (define-key bibtex-mode-map "\C-c\C-k" 'bibtex-kill-optional-field)
  (define-key bibtex-mode-map "\C-c\C-d" 'bibtex-empty-field)

  (define-key bibtex-mode-map "\C-c\C-e\C-a" 'bibtex-Article)
  (define-key bibtex-mode-map "\C-c\C-e\C-b" 'bibtex-Book)
  ;; (define-key bibtex-mode-map "\C-c\C-e\C-d" 'bibtex-DEAthesis)
  (define-key bibtex-mode-map "\C-c\C-e\C-c" 'bibtex-InProceedings)
  (define-key bibtex-mode-map "\C-c\C-e\C-i" 'bibtex-InBook)
  (define-key bibtex-mode-map "\C-c\C-ei" 'bibtex-InCollection)
  (define-key bibtex-mode-map "\C-c\C-eI" 'bibtex-InProceedings)
  (define-key bibtex-mode-map "\C-c\C-e\C-m" 'bibtex-Manual)
  (define-key bibtex-mode-map "\C-c\C-em" 'bibtex-MastersThesis)
  (define-key bibtex-mode-map "\C-c\C-eM" 'bibtex-Misc)
  (define-key bibtex-mode-map "\C-c\C-o" 'bibtex-remove-OPT)
  (define-key bibtex-mode-map "\C-c\C-e\C-p" 'bibtex-PhdThesis)
  (define-key bibtex-mode-map "\C-c\C-ep" 'bibtex-Proceedings)
  (define-key bibtex-mode-map "\C-c\C-e\C-t" 'bibtex-TechReport)
  (define-key bibtex-mode-map "\C-c\C-e\C-s" 'bibtex-string)
  (define-key bibtex-mode-map "\C-c\C-e\C-u" 'bibtex-Unpublished)

  (auto-fill-mode 1)			; nice alignements
  (setq left-margin (+ bibtex-text-alignment 1))

  (run-hooks 'bibtex-mode-hook))

(defun bibtex-move-outside-of-entry ()
  "Make sure we are outside of a bib entry"
  (cond ((or
	  (= (point) (point-max))
	  (= (point) (point-min))
	  (looking-at "[ \n]*@")
	  )
	 t)
	(t
	 (backward-paragraph)
	 (forward-paragraph)))
  (re-search-forward "[ \t\n]*" (point-max) t))

(defun bibtex-entry (entry-type required optional)
  (bibtex-move-outside-of-entry)
  (insert "@" entry-type "{")
  (mapcar 'bibtex-make-entry required)
  (mapcar 'bibtex-make-optional-entry optional)
  (insert "\n}\n\n")
  (forward-char -3)
  (up-list -1)
  (forward-char 1))

(defun bibtex-make-entry (str)
  (interactive "sBibTeX entry type: ")
  (insert ",\n")
  (indent-to-column bibtex-name-alignement)
  (insert str " = ")
  (indent-to-column bibtex-text-alignment)
  (insert "\"\"")
  nil)

(defun bibtex-make-optional-entry (str)
  (interactive "sOptional BibTeX entry type: ")
  (insert ",\n")
  (indent-to-column bibtex-name-alignement)
  (insert "OPT" str " = ")
  (indent-to-column bibtex-text-alignment)
  (insert "\"\"")
  nil)

(defun bibtex-Article ()
  (interactive)
  (bibtex-entry "Article" '("author" "title" "journal" "year")
		'("volume" "number" "pages" "month" "note")))

(defun bibtex-Book ()
  (interactive)
  (bibtex-entry "Book" '("author" "title" "publisher" "year")
		'("editor" "volume" "series" "address"
			   "edition" "month" "note")))

(defun bibtex-Booklet ()
  (interactive)
  (bibtex-entry "Booklet" '("title")
		'("author" "howpublished" "address" "month" "year" "note")))

;; France: Dipl\^{o}me d'Etudes Approfondies (similar to Master's)
(defun bibtex-DEAthesis ()
  (interactive)
  (bibtex-entry "DEAthesis" '("author" "title" "school" "year")
		'("address" "month" "note")))

(defun bibtex-InBook ()
  (interactive)
  (bibtex-entry "InBook" '("author" "title" "chapter" "publisher" "year")
		'("editor" "pages" "volume" "series" "address"
			   "edition" "month" "note")))

(defun bibtex-InCollection ()
  (interactive)
  (bibtex-entry "InCollection" '("author" "title" "booktitle"
					  "publisher" "year")
		'("editor" "chapter" "pages" "address" "month" "note")))


(defun bibtex-InProceedings ()
  (interactive)
  (bibtex-entry "InProceedings" '("author" "title" "booktitle" "year")
		'("editor" "pages" "organization" "publisher"
			   "address" "month" "note")))

(defun bibtex-Manual ()
  (interactive)
  (bibtex-entry "Manual" '("title")
		'("author" "organization" "address" "edition" "year"
			   "month" "note")))

(defun bibtex-MastersThesis ()
  (interactive)
  (bibtex-entry "MastersThesis" '("author" "title" "school" "year")
		'("address" "month" "note")))

(defun bibtex-Misc ()
  (interactive)
  (bibtex-entry "Misc" '()
		'("author" "title" "howpublished" "year" "month" "note")))

(defun bibtex-PhdThesis ()
  (interactive)
  (bibtex-entry "PhdThesis" '("author" "title" "school" "year")
		'("address" "month" "note")))

(defun bibtex-Proceedings ()
  (interactive)
  (bibtex-entry "Proceedings" '("title" "year")
		'("editor" "publisher" "organization"
			   "address" "month" "note")))
(defun bibtex-TechReport ()
  (interactive)
  (bibtex-entry "TechReport" '("author" "title" "institution" "year")
		'("type" "number" "address" "month" "note")))


(defun bibtex-Unpublished ()
  (interactive)
  (bibtex-entry "Unpublished" '("author" "title" "note")
		'("year" "month")))

(defun bibtex-string ()
  (interactive)
  (bibtex-move-outside-of-entry)
  (insert "@string{ = """"}\n")
  (previous-line 1)
  (forward-char 8))


(defun bibtex-next-field (arg)
  "Finds end of text of next BibTeX field; with arg, to its beginning"
  (interactive "P")
  (bibtex-inside-field)
  (let ((start (point)))
    (condition-case ()
	(progn
	  (bibtex-enclosing-field)
	  (goto-char (match-end 0))
	  (forward-char 2))
      (error
       (goto-char start)
       (end-of-line)
       (forward-char 1))))
  (bibtex-find-text arg))

(defun bibtex-find-text (arg)
  "Go to end of text of current field; with arg, go to beginning."
  (interactive "P")
  (bibtex-inside-field)
  (bibtex-enclosing-field)
  (if arg
      (progn
	(goto-char (match-beginning bibtex-text-in-field))
	(if (looking-at "\"")
	    (forward-char 1)))
    (goto-char (match-end bibtex-text-in-field))
    (if (= (preceding-char) ?\")
	(forward-char -1))))

(defun bibtex-remove-OPT ()
  "Removes the 'OPT' starting optional arguments and goes to end of text"
  (interactive)
  (bibtex-inside-field)
  (bibtex-enclosing-field)
  (save-excursion
    (goto-char (match-beginning bibtex-name-in-field))
    (if (looking-at "OPT")
	(delete-char (length "OPT"))))
  (bibtex-inside-field))

(defun bibtex-inside-field ()
  "Try to avoid point being at end of a bibtex field."
  (interactive)
  (end-of-line)
  (skip-chars-backward " \t")
  (cond ((= (preceding-char) ?,)
	 (forward-char -1)))
  (forward-char -1))

(defun bibtex-remove-double-quotes ()
  "Removes \"\" around string."
  (interactive)
  (save-excursion
    (bibtex-inside-field)
    (bibtex-enclosing-field)
    (let ((start (match-beginning bibtex-text-in-field))
	  (stop (match-end  bibtex-text-in-field)))
      (goto-char stop)
      (forward-char -1)
      (if (looking-at "\"")
	  (delete-char 1))
      (goto-char start)
      (if (looking-at "\"")
	  (delete-char 1)))))

(defun bibtex-kill-optional-field ()
  "Kill the entire enclosing optional BibTeX field"
  (interactive)
  (bibtex-inside-field)
  (bibtex-enclosing-field)
  (goto-char (match-beginning bibtex-name-in-field))
  (let ((the-end (match-end 0))
	(the-beginning (match-beginning 0)))
    (if (looking-at "OPT")
	(progn
	  (goto-char the-end)
	  (skip-chars-forward " \t\n,")
	  (kill-region the-beginning the-end))
      (error "Mandatory fields can't be killed"))))

(defun bibtex-empty-field ()
  "Delete the text part of the current field, replace with empty text"
  (interactive)
  (bibtex-inside-field)
  (bibtex-enclosing-field)
  (goto-char (match-beginning bibtex-text-in-field))
  (kill-region (point) (match-end bibtex-text-in-field))
  (insert "\"\"")
  (bibtex-find-text t))


(defun bibtex-pop-previous (arg)
  "Replace text of current field with the text of similar field in previous entry.
With arg, go up ARG entries.  Repeated, goes up so many times.  May be
intermixed with \\[bibtex-pop-next] (bibtex-pop-next)."
  (interactive "p")
  (bibtex-inside-field)
  (save-excursion
    ; parse current field
    (bibtex-enclosing-field)
    (let ((start-old-text (match-beginning bibtex-text-in-field))
	  (stop-old-text  (match-end bibtex-text-in-field))
	  (start-name (match-beginning bibtex-name-in-field))
	  (stop-name (match-end bibtex-name-in-field))
	  (new-text))
      (goto-char start-name)
      ; construct regexp for previous field with same name as this one
      (let ((matching-entry
	     (bibtex-cfield
	      (buffer-substring (if (looking-at "OPT")
				    (+ (point) (length "OPT"))
				  (point))
				stop-name)
	      bibtex-field-text)))
	
	; if executed several times in a row, start each search where the
	; last one finished
	(cond ((or (eq last-command 'bibtex-pop-previous)
		   (eq last-command 'bibtex-pop-next))
	       t
	       )
	      (t
	       (bibtex-enclosing-reference)
	       (setq bibtex-pop-previous-search-point (match-beginning 0))
	       (setq bibtex-pop-next-search-point (match-end 0))))
	(goto-char bibtex-pop-previous-search-point)
	
	; Now search for arg'th previous similar field
	(cond
	 ((re-search-backward matching-entry (point-min) t arg)
	  (setq new-text
		(buffer-substring (match-beginning bibtex-text-in-cfield)
				  (match-end bibtex-text-in-cfield)))
	  ; Found a matching field. Remember boundaries.
	  (setq bibtex-pop-next-search-point (match-end 0))
	  (setq bibtex-pop-previous-search-point (match-beginning 0))
	  (bibtex-flash-head)
	  ; Go back to where we started, delete old text, and pop new.
	  (goto-char stop-old-text)
	  (delete-region start-old-text stop-old-text)
	  (insert new-text))
	 (t				; search failed
	  (error "No previous matching BibTeX field."))))))
  (setq this-command 'bibtex-pop-previous))

(defun bibtex-pop-next (arg)
  "Replace text of current field with the text of similar field in next entry.
With arg, go up ARG entries.  Repeated, goes up so many times.  May be
intermixed with \\[bibtex-pop-previous] (bibtex-pop-previous)."
  (interactive "p")
  (bibtex-inside-field)
  (save-excursion
    ; parse current field
    (bibtex-enclosing-field)
    (let ((start-old-text (match-beginning bibtex-text-in-field))
	  (stop-old-text  (match-end bibtex-text-in-field))
	  (start-name (match-beginning bibtex-name-in-field))
	  (stop-name (match-end bibtex-name-in-field))
	  (new-text))
      (goto-char start-name)
      ; construct regexp for next field with same name as this one,
      ; ignoring possible OPT's
      (let ((matching-entry
	     (bibtex-cfield
	      (buffer-substring (if (looking-at "OPT")
				    (+ (point) (length "OPT"))
				  (point))
				stop-name)
	      bibtex-field-text)))
	
	; if executed several times in a row, start each search where the
	; last one finished
	(cond ((or (eq last-command 'bibtex-pop-next)
		   (eq last-command 'bibtex-pop-previous))
	       t
	       )
	      (t
	       (bibtex-enclosing-reference)
	       (setq bibtex-pop-previous-search-point (match-beginning 0))
	       (setq bibtex-pop-next-search-point (match-end 0))))
	(goto-char bibtex-pop-next-search-point)
	
	; Now search for arg'th next similar field
	(cond
	 ((re-search-forward matching-entry (point-max) t arg)
	  (setq new-text
		(buffer-substring (match-beginning bibtex-text-in-cfield)
				  (match-end bibtex-text-in-cfield)))
	  ; Found a matching field. Remember boundaries.
	  (setq bibtex-pop-next-search-point (match-end 0))
	  (setq bibtex-pop-previous-search-point (match-beginning 0))
	  (bibtex-flash-head)
	  ; Go back to where we started, delete old text, and pop new.
	  (goto-char stop-old-text)
	  (delete-region start-old-text stop-old-text)
	  (insert new-text))
	 (t				; search failed
	  (error "No next matching BibTeX field."))))))
  (setq this-command 'bibtex-pop-next))

(defun bibtex-flash-head ()
  "Flash at BibTeX reference head before point, if exists.  (Moves point)."
  (let ((flash))
    (cond ((re-search-backward bibtex-reference-head (point-min) t)
	   (goto-char (match-beginning bibtex-type-in-head))
	   (setq flash (match-end bibtex-key-in-reference)))
	  (t
	   (end-of-line)
	   (skip-chars-backward " \t")
	   (setq flash (point))
	   (beginning-of-line)
	   (skip-chars-forward " \t")))
    (if (pos-visible-in-window-p (point))
	(sit-for 1)
      (message "From: %s"
	       (buffer-substring (point) flash)))))



(defun bibtex-enclosing-field ()
  "Search for BibTeX field enclosing point.
Point moves to end of field; also, use match-beginning and match-end
to parse the field."
  (condition-case errname
      (bibtex-enclosing-regexp bibtex-field)
    (search-failed
     (error "Can't find enclosing BibTeX field."))))

(defun bibtex-enclosing-reference ()
  "Search for BibTeX reference enclosing point.
Point moves to end of reference; also, use match-beginning and match-end
to parse the reference."
  (condition-case errname
      (bibtex-enclosing-regexp bibtex-reference)
    (search-failed
     (error "Can't find enclosing BibTeX reference."))))

(defun bibtex-enclosing-regexp (regexp)
  "Search for REGEXP enclosing point.
Point moves to end of REGEXP.  See also match-beginning and match-end.
If an enclosing REGEXP is not found, signals search-failed; point is left in
an undefined location.

[Doesn't something like this exist already?]"
  
  (interactive "sRegexp: ")
  ; compute reasonable limits for the loop
  (let* ((initial (point))
	 (right (if (re-search-forward regexp (point-max) t)
		    (match-end 0)
		  (point-max)))
	 (left
	  (progn
	    (goto-char initial)
	    (if (re-search-backward regexp (point-min) t)
		(match-beginning 0)
	      (point-min)))))
    ; within the prescribed limits, loop until a match is found
    (goto-char left)
    (re-search-forward regexp right nil 1)
    (if (> (match-beginning 0) initial)
	(signal 'search-failed (list regexp)))	  
    (while (<= (match-end 0) initial)
      (re-search-forward regexp right nil 1)
      (if (> (match-beginning 0) initial)
	  (signal 'search-failed (list regexp))))
    ))

(defun bibtex-clean-entry ()
  "For all optional fields of current BibTeX entry: if empty, kill the whole field; otherwise, remove the \"OPT\" string in the name; if text numerical, remove double-quotes.  For all mandatory fields: if empty, signal error."
  (interactive)
  (bibtex-enclosing-reference)
  (goto-char (match-beginning 0))
  (let ((start (point)))
    (save-restriction
      (narrow-to-region start (match-end 0))
      (while (re-search-forward bibtex-field (point-max) t 1)
	(let ((begin-field (match-beginning 0))
	      (end-field (match-end 0))
	      (begin-name (match-beginning bibtex-name-in-field))
	      (end-name (match-end  bibtex-name-in-field))
	      (begin-text (match-beginning bibtex-text-in-field))
	      (end-text (match-end bibtex-text-in-field))
	      )
	  (goto-char begin-name)
	  (cond ((looking-at "OPT")
		 (goto-char begin-text)
		 (if (looking-at "\"\"") ; empty: delete whole field
		     (delete-region begin-field end-field)
		   ; otherwise: not empty, delete "OPT"
		   (goto-char begin-name)
		   (delete-char (length "OPT"))
		   (goto-char begin-field) ; and loop to go through next test
		   ))
		(t
		 (goto-char begin-text)
		 (cond ((looking-at "\"[0-9]+\"") ; if numerical,
			(goto-char end-text)
			(delete-char -1) ; delete enclosing double-quotes
			(goto-char begin-text)
			(delete-char 1)
			(goto-char end-field) ; go to end for next search
			(forward-char -2) ; to compensate for the 2 quotes deleted
			)
		       ((looking-at "\"\"") ; if empty quotes, complain
			(forward-char 1)
			(error "Mandatory field ``%s'' is empty"
			       (buffer-substring begin-name end-name)))
		       (t
			(goto-char end-field))))))))
    (goto-char start)
    (skip-chars-forward "@a-zA-Z")
    (bibtex-enclosing-reference)
    (goto-char (match-end 0))
    (skip-chars-forward " \t\n")))



;;; X window menus for bibtex mode

(defun bibtex-x-help (arg)
  "Mouse commands for BibTeX mode"
  
  (let ((selection
	 (x-popup-menu
	  arg
	  '("BibTeX commands"
	    ("BibTeX entry types"
	     (" article in conference Proceedings " . bibtex-InProceedings)
	     ("        Article in journal         " . bibtex-Article)
	     ("               Book                " . bibtex-Book)
	     ("             Booklet               " . bibtex-Booklet)
	     ("         Master's Thesis           " . bibtex-MastersThesis)
	     ;; ("            DEA Thesis             " . bibtex-DEAthesis)
	     ("            Phd. Thesis            " . bibtex-PhdThesis)
	     ("         Technical Report          " . bibtex-TechReport)
	     ("         technical Manual          " . bibtex-Manual)
	     ("      conference Proceedings       " . bibtex-Proceedings)
	     ("        a chapter in a Book        " . bibtex-InBook)
	     ("    an article in a Collection     " . bibtex-InCollection)
	     ("           miscellaneous           " . bibtex-Misc)
	     ("            unpublished            " . bibtex-Unpublished)
	     ("              string               " . bibtex-string)
	     )
	    ("Moving around and editing"
	     ("            next field             " . bibtex-next-field)
	     ("          to end of field          " . bibtex-find-text)
	     ("snatch from similar preceding field" . bibtex-pop-previous)
	     ("snatch from similar following field" . bibtex-pop-next)
	     ("            remove OPT             " . bibtex-remove-OPT)
	     ("           remove quotes           "
	      . bibtex-remove-double-quotes)
	     ("          clean up entry           " . bibtex-clean-entry)
	     )
	    ("help"
	     ("       describe BibTeX mode        " . describe-mode)
	     )))))
    (and selection (call-interactively selection))))

(defun bibtex-x-environment ()
  "Set up X menus for BibTeX mode.  Call it as bibtex-mode-hook, or interactively"
  (interactive)
  (require 'x-mouse)
  (define-key mouse-map x-button-c-right 'bibtex-x-help)
  )



;; Please don't send anything to bug-gnu-emacs about these Sunwindows functions
;; since we aren't interested.  See etc/SUN-SUPPORT for the reasons why
;; we consider this nothing but a distraction from our work.

(if (fboundp 'defmenu)
    ;; ## jwz: added this eval so that this file can be compiled correctly;
    ;; if the defmenu macro isn't around at compile-time, we will compile a
    ;; totally bogus set of function calls instead.
    (eval '(progn

(defmenu bibtex-sun-entry-menu 
  ("Article In Conf. Proc."
   (lambda () (eval-in-window *menu-window* (bibtex-InProceedings))))
  ("Article In Journal"
   (lambda () (eval-in-window *menu-window* (bibtex-Article))))
  ("Book"
   (lambda () (eval-in-window *menu-window* (bibtex-Book))))
  ("Booklet"
   (lambda () (eval-in-window *menu-window* (bibtex-Booklet))))
  ("Master's Thesis"
   (lambda () (eval-in-window *menu-window* (bibtex-MastersThesis))))
  ;;("DEA Thesis" bibtex-DEAthesis)
  ("PhD. Thesis"
   (lambda () (eval-in-window *menu-window* (bibtex-PhdThesis))))
  ("Technical Report"
   (lambda () (eval-in-window *menu-window* (bibtex-TechReport))))
  ("Technical Manual"
   (lambda () (eval-in-window *menu-window* (bibtex-Manual))))
  ("Conference Proceedings"
   (lambda () (eval-in-window *menu-window* (bibtex-Proceedings))))
  ("In A Book"
   (lambda () (eval-in-window *menu-window* (bibtex-InBook))))
  ("In A Collection"
   (lambda () (eval-in-window *menu-window* (bibtex-InCollection))))
  ("Miscellaneous"
   (lambda () (eval-in-window *menu-window* (bibtex-Misc))))
  ("Unpublished"
   (lambda () (eval-in-window *menu-window* (bibtex-Unpublished)))))

(defmenu bibtex-sun-menu
  ("BibTeX menu")
  ("add entry" . bibtex-sun-entry-menu)
  ("add string"
   (lambda () (eval-in-window *menu-window* (bibtex-string))))
  ;("next field" bibtex-next-position)
  ;("to end of field" bibtex-find-it)
;  ("remove OPT"
;   (lambda () (eval-in-window *menu-window* (bibtex-remove-opt))))
;  ("remove quotes"
;   (lambda () (eval-in-window *menu-window* (bibtex-remove-double-quotes))))
;  ("remove this line"
;   (lambda () (eval-in-window *menu-window* (kill-current-line))))
  ("describe BibTeX mode"
   (lambda () (eval-in-window *menu-window* (describe-mode))))
  ("Main Emacs menu" . emacs-menu))
 
(defun bibtex-sun-menu-eval (window x y)
  "Pop-up menu of BibTeX commands."
  (sun-menu-evaluate window (1+ x) (1- y) 'bibtex-sun-menu))

(defun bibtex-sun-environment ()
  "Set up sun menus for BibTeX mode.  Call it as bibtex-mode-hook, or interactively"
  (interactive)
  (local-set-mouse  '(text right) 'bibtex-sun-menu-eval))

)))  ; matches (if...
