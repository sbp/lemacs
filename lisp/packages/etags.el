;; Enhanced tags facility for Emacs.
;; Copyright 1985, 1986, 1988, 1990 Free Software Foundation, Inc.

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

;; Created by: Joe Wells, jbw@bucsf.bu.edu
;; Created on: Thu Mar 22 20:17:40 1990
;; Last modified by: Jamie Zawinski <jwz@lucid.com>
;; Last modified on: Wed Jan  1 15:09:18 1992
;; Filename: tags-fix.el
;; Purpose: enhanced tags functionality
;; Change log: 
;; 
;; Wed Jan  1 15:09:18 1992  Jamie Zawinski <jwz@lucid.com>
;;
;;      * Added Harlan's definition of visit-tags-table.  
;;      Renamed variable tags-always-build-completion-table to
;;      tags-build-completion-table and changed its semantics.
;;      Made the explicit buffer-local tags file be searched 
;;      first instead of last.
;;
;; Sun May 10 15:48:00 1992  Hallvard B Furuseth <h.b.furuseth@usit.uio.no>
;;
;;	Inserted visit-tags-table-buffer from tags.el, handle
;;	tag-file-name=nil, improved some doc strings and variable declarations.
;;
;; Fri Mar 29 01:48:06 1991  Jamie Zawinski <jwz@lucid.com>
;;
;;	* Made link-chasing and invisible-tags-files optional.
;;	Renamed delete and remove-duplicates to avoid possible name conflicts.
;;	Moved "provide" to end.  Added some documentation.
;; 
;; Sat Sep 22 22:28:33 1990  Joseph Wells  (jbw at bucsf.bu.edu)
;; 
;; 	* Added handling for case where tag is typedef name immediately
;; 	following struct definition.
;; 
;; Thu Sep 13 21:09:15 1990  Joseph Wells  (jbw at bucsf.bu.edu)
;; 
;; 	* Fixed behavior not to bomb on missing tag table file.
;; 

;; Sat Aug 11 18:07:01 1990  Joe Wells  (jbw at dodge.uswest.com)
;; 
;; 	* Moved calling find-tag-default-hook into find-tag-default.  Put
;; 	it inside a condition-case.  Use find-tag-default method when
;; 	find-tag-default-hook fails or returns nil.
;; 
;; Wed Jul 25 17:16:43 1990  Joe Wells  (jbw at dodge.uswest.com)
;; 
;; 	* Made it an error for a buffer to have no associated tag tables.
;; 

;; enhancements:
;;  1. default tag tables based on filename
;;  2. multiple tag tables possible per file
;;  3. tag name completion for find-tag
;;  4. find-tag using regexp
;;  5. tag name completion in the buffer
;;  6. find-tag-default now works at beginning of tag
;;  7. buffer-local find-tag hook (used for info enhancement)
;;  8. buffer-local find-tag-default hook (used for info enhancement)
;;  9. show short info on tag match in minibuffer
;; 10. stack for backtracking from find-tag
;; 11. widen buffers for tags-search
;; 12. display message on successful search
;; 13. don't pull all files into memory for tags-search
;; 14. don't leave searched buffers on top of buffer list
;; 15. find-tag can specify exact symbol matches
;; 16. find-tag-default specifies an exact symbol match
;; 17. tags-files can be invisible

;; configuration variables:
;;   tag-table-alist		controls which tables apply to which buffers
;;   tags-file-name		a default tags table
;;   buffer-tag-table		another way of specifying a buffer-local table
;;   make-tags-files-invisible	whether tags tables should be very hidden
;;   tag-mark-stack-max		how many tags-based hops to remember

;; TODO:
;; 1. place cursor in echo area while searching
;; 2. document!
;; 3. determine semantics of interactively setting the tags file for a buffer

;; Comments with **** mean something is left to be done.

;; Derived from the original lisp/tags.el.

;; Ideas and code from the work of the following people:
;; Andy Norman <ange@hplb.hpl.hp.com>, author of ange-tags.el
;; Ramana Rao <rao@arisia.xerox.com>
;; John Sturdy <jcgs@harlqn.co.uk>, author of tags-helper.el
;; Henry Kautz <kautz@allegra.att.com>, author of tag-completion.el
;; Dan LaLiberte <liberte@cs.uiuc.edu>, author of local-tags.el
;; Tom Dietterich <tgd@turing.cs.orst.edu>, author of quest.el
;; The author(s) of lisp/simple.el
;; Duke Briscoe <briscoe@cs.yale.edu>
;; Lynn Slater <lrs@indetech.com>, author of location.el
;; Shinichirou Sugou <shin@sgtp.apple.juice.or.jp>
;; an unidentified anonymous elisp hacker

;; Installation instructions:
;;
;; Name this file tags-fix.el.
;; Put tags-fix.el, symlink-fix.el, symbol-syntax.el in your load path.
;;
;; Put the following code in your .emacs (or lisp/default.el)
;;
;;(fmakunbound 'visit-tags-table) ; obsolete
;;(fmakunbound 'find-tag)
;;(autoload 'find-tag "tags-fix" nil t)
;;(fmakunbound 'find-tag-other-window)
;;(autoload 'find-tag-other-window "tags-fix" nil t)
;;(fmakunbound 'lisp-complete-symbol)
;;(autoload 'lisp-complete-symbol "tags-fix" nil t)
;;(fmakunbound 'tag-complete-symbol)
;;(autoload 'tag-complete-symbol "tags-fix" nil t)
;;(fmakunbound 'next-file)
;;(autoload 'next-file "tags-fix" nil t)
;;(fmakunbound 'tags-loop-continue)
;;(autoload 'tags-loop-continue "tags-fix" nil t)
;;(fmakunbound 'tags-search)
;;(autoload 'tags-search "tags-fix" nil t)
;;(fmakunbound 'tags-query-replace)
;;(autoload 'tags-query-replace "tags-fix" nil t)
;;(fmakunbound 'display-tag-info)
;;(autoload 'display-tag-info "tags-fix" nil t)
;;(fmakunbound 'pop-tag-mark)
;;(autoload 'pop-tag-mark "tags-fix" nil t)
;;
;;(define-key esc-map "?" 'display-tag-info)
;;(define-key esc-map "*" 'pop-tag-mark)
;;
;;;; The following are not really implemented:
;;;;(fmakunbound 'set-buffer-tag-table)
;;;;(autoload 'set-buffer-tag-table "tags-fix" nil t)
;;(fmakunbound 'list-tags)
;;;;(autoload 'list-tags "tags-fix" nil t)
;;(fmakunbound 'tags-apropos)
;;;;(autoload 'tags-apropos "tags-fix" nil t)


;; Auxiliary functions

(defun tags-delete (item list)
  "delete the item from the list, testing with equal.  Copies the list."
  (cond ((null list)
	 nil)
	((equal item (car list))
	 (tags-delete item (cdr list)))
	(t
	 (cons (car list) (tags-delete item (cdr list))))))

(defun tags-remove-duplicates (list)
  "delete equal duplicates from the list; copies the list."
  (cond ((null list)
	 nil)
	(t
	 (cons (car list)
	       (tags-remove-duplicates (tags-delete (car list) (cdr list)))))))

;; derived from generate-new-buffer
(defun generate-new-buffer-name (name)
  "Foo"
  (if (not (get-buffer name))
      name
    (let ((count 1)
	  (template (concat name "<%d>"))
	  tempname)
      (catch 'found
	(while t
	  (setq tempname (format template count))
	  (if (not (get-buffer tempname))
	      (throw 'found tempname))
	  (setq count (1+ count)))))))


;; Tag tables for a buffer

(defvar tags-build-completion-table 'ask
  "*If this variable is nil, then tags completion is disabled.
If this variable is t, then things which prompt for tags will do so with 
 completion across all known tags.
If this variable is the symbol `ask', then you will be asked whether each
 tags table should be added to the completion list as it is read in.
 (With the exception that for very small tags tables, you will not be asked,
 since they can be parsed quickly.)")


(defvar tag-table-alist nil
  "*A list which determines which tags files should be active for a 
given buffer.  This is not really an association list, in that all 
elements are checked.  The CAR of each element of this list is a 
pattern against which the buffer's file name is compared; if it 
matches, then the CDR of the list should be the name of the tags
table to use.  If more than one element of this list matches the
buffer's file name, then all of the associated tags tables will be
used.  Earlier ones will be searched first.

If the CAR of elements of this list are strings, then they are treated
as regular-expressions against which the file is compared (like the
auto-mode-alist).  If they are not strings, then they are evaluated.
If they evaluate to non-nil, then the current buffer is considered to
match.

If the CDR of the elements of this list are strings, then they are
assumed to name a TAGS file.  If they name a directory, then the string
\"TAGS\" is appended to them to get the file name.  If they are not 
strings, then they are evaluated, and must return an appropriate string.

For example:
  (setq tag-table-alist
	'((\"/usr/src/public/perl/\" . \"/usr/src/public/perl/perl-3.0/\")
	 (\"\\\\.el$\" . \"/usr/local/emacs/src/\")
	 (\"/jbw/gnu/\" . \"/usr15/degree/stud/jbw/gnu/\")
	 (\"\" . \"/usr/local/emacs/src/\")
	 ))

This means that anything in the /usr/src/public/perl/ directory should use
the TAGS file /usr/src/public/perl/perl-3.0/TAGS; and file ending in .el should
use the TAGS file /usr/local/emacs/src/TAGS; and anything in or below the
directory /jbw/gnu/ should use the TAGS file /usr15/degree/stud/jbw/gnu/TAGS.
A file called something like \"/usr/jbw/foo.el\" would use both the TAGS files
/usr/local/emacs/src/TAGS and /usr15/degree/stud/jbw/gnu/TAGS (in that order)
because it matches both patterns.

If the buffer-local variable `buffer-tag-table' is set, then it names a tags
table that is searched before all others when find-tag is executed from this
buffer.

If there is a file called \"TAGS\" in the same directory as the file in 
question, then that tags file will always be used as well (after the
`buffer-tag-table' but before the tables specified by this list.)

If the variable tags-file-name is set, then the tags file it names will apply
to all buffers (for backwards compatibility.)  It is searched first.
")

(defvar buffer-tag-table nil
  "*The name of one TAGS table to be used for this buffer in addition to the
TAGS tables that the variable `tag-table-alist' specifies.  You can set this
with meta-x set-buffer-tag-table.  See the documentation for the variable
`tag-table-alist' for more information.")
(make-variable-buffer-local 'buffer-tag-table)

(defvar tags-file-name nil
  "*The name of the tags-table used by all buffers.  This is for backwards
compatibility, and is largely supplanted by the variable tag-table-alist.")
;; (setq tags-file-name nil)  ; nuke previous value.  Is this cool?

;; This will be used if it's loaded; don't force it on those who don't want it.
;;(autoload 'symlink-expand-file-name "symlink-fix")

(defun buffer-tag-table-list ()
  "Returns a list (ordered) of the tags tables which should be used for 
the current buffer."
  (let (result expression)
    (if buffer-tag-table
	(setq result (cons buffer-tag-table result)))
    (if (file-readable-p (concat default-directory "TAGS"))
	(setq result (cons (concat default-directory "TAGS") result)))
    (let ((key (or buffer-file-name
		   (concat default-directory (buffer-name))))
	  (alist tag-table-alist))
      (while alist
	(setq expression (car (car alist)))
	;; If the car of the alist item is a string, apply it as a regexp
	;; to the buffer-file-name.  Otherwise, evaluate it.  If the
	;; regexp matches, or the expression evaluates non-nil, then this
	;; item in tag-table-alist applies to this buffer.
	(if (if (stringp expression)
		(string-match (car (car alist)) key)
	      (condition-case nil
		  (eval expression)
		(error nil)))
	    ;; Now evaluate the cdr of the alist item to get the name of
	    ;; the tag table file.
	    (progn
	      (setq expression 
		    (condition-case nil
			(eval (cdr (car alist)))
		      (error nil)))
	      (if (stringp expression)
		  (setq result (cons expression result))
		(error "Expression in tag-table-alist evaluated to non-string"))))
	(setq alist (cdr alist))))
    (or result tags-file-name
	;; **** I don't know if this is the right place to do this,
	;; **** Maybe it would be better to do this after (delq nil result).
	(call-interactively 'visit-tags-table))
    (if tags-file-name
	(setq result (nconc result (list tags-file-name))))
    (setq result
	  (mapcar
	   (function
	    (lambda (name)
	      (if (file-directory-p name)
		  (setq name (concat name "TAGS")))
	      (if (file-readable-p name)
		  (save-excursion
		    ;; get-tag-table-buffer has side-effects
		    (set-buffer (get-tag-table-buffer name))
		    buffer-file-name))))
	   result))
    (setq result (delq nil result))
    (or result (error "Buffer has no associated tag tables"))
    (tags-remove-duplicates (nreverse result))))

(defun visit-tags-table (file)
  "Tell tags commands to use tags table file FILE first.
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory."
  (interactive (list (read-file-name "Visit tags table: (default TAGS) "
				     default-directory
				     (expand-file-name "TAGS" default-directory)
				     t)))
  (if (string-equal file "") 
      (setq tags-file-name nil)
      (progn
        (setq file (expand-file-name file))
        (if (file-directory-p file)
            (setq file (expand-file-name "TAGS" file)))
        (setq tags-file-name file))))

;; **** What should the semantics of this be?
(defun set-buffer-tag-table (file)
  "In addition to the tags tables specified by the variable `tag-table-alist',
each buffer can have one additional table.  This command sets that.
See the documentation for the variable `tag-table-alist' for more information."
  (interactive
   (list
     (read-file-name "Visit tags table: (directory sufficient) "
		     nil default-directory t)))
  (or file (error "No TAGS file name supplied"))
  (setq file (expand-file-name file))
  (if (file-directory-p file)
      (setq file (concat file "TAGS")))
  (or (file-exists-p file) (error "TAGS file missing: %s" file))
  (setq buffer-tag-table file))


;; Manipulating the tag table buffer

(defconst tag-table-completion-status nil
  "Indicates whether a completion table has been built, or has explicitly not 
been built.  this is nil, t, or 'disabled.")
(make-variable-buffer-local 'tag-table-completion-status)

(defvar make-tags-files-invisible nil
  "*If true, TAGS-files will not show up in buffer-lists or be 
selectable (or deletable.)")

(defconst tag-table-files nil
  "If the current buffer is a TAGS table, this holds a list of the files 
referenced by this file, or nil if that hasn't been computed yet.")
(make-variable-buffer-local 'tag-table-files)

(defun get-tag-table-buffer (tag-table)
  "Returns a buffer visiting the give TAGS table, reverting if appropriate,
and possibly building a completion-table."
  (or (stringp tag-table)
      (error "Bad tags file name supplied: %s" tag-table))
  ;; add support for removing symbolic links from name
  (if (fboundp 'symlink-expand-file-name)
      (setq tag-table (symlink-expand-file-name tag-table)))
  (let (buf build-completion check-name)
    (setq buf (get-file-buffer tag-table))
    (or buf
	(if (file-readable-p tag-table)
	    (setq buf (find-file-noselect tag-table)
		  check-name t)
	  (error "No such tags file: %s" tag-table)))
    (save-excursion
      (set-buffer buf)
      ;; make the TAGS buffer invisible
      (if (and check-name
	       make-tags-files-invisible
	       (string-match "\\`[^ ]" (buffer-name)))
	  (rename-buffer (generate-new-buffer-name
			  (concat " " (buffer-name)))))
      (or (verify-visited-file-modtime buf)
	  (cond ((yes-or-no-p
		  (format "Tags file %s has changed, read new contents? "
			  tag-table))
		 (revert-buffer t t)
		 (if (eq tag-table-completion-status t)
		     (setq tag-table-completion-status nil))
		 (setq tag-table-files nil))))
      (or (eq (char-after 1) ?\f)
	  (error "File %s not a valid tags file" tag-table))
      (or (memq tag-table-completion-status '(t disabled))
	  (setq build-completion t))
      (and build-completion
	   (if (cond
		((eq tags-build-completion-table nil)
		 nil)
		((eq tags-build-completion-table t)
		 t)
		((eq tags-build-completion-table 'ask)
		 ;; don't bother asking for small ones
		 (or (< (buffer-size) 20000)
		     (y-or-n-p
		      (format "Build tag completion table for %s? "
			      tag-table))))
		(t (error
		    "tags-build-completion-table is not t, nil, or ask.")))
	       (condition-case foo
		   (progn
		     (add-to-tag-completion-table)
		     (setq tag-table-completion-status t))
		 ;; Allow user to C-g out correctly
		 (quit
		  (setq tag-table-completion-status nil)
		  (setq quit-flag t)
		  (eval t)))
	     (setq tag-table-completion-status 'disabled))))
    buf))

;; This function is unchanged from lisp/tags.el:
(defun file-of-tag ()
  "Return the file name of the file whose tags point is within.
Assumes the tag table is the current buffer.
File name returned is relative to tag table file's directory."
  (let ((opoint (point))
	prev size)
    (save-excursion
     (goto-char (point-min))
     (while (< (point) opoint)
       (forward-line 1)
       (end-of-line)
       (skip-chars-backward "^,\n")
       (setq prev (point))
       (setq size (read (current-buffer)))
       (goto-char prev)
       (forward-line 1)
       (forward-char size))
     (goto-char (1- prev))
     (buffer-substring (point)
		       (progn (beginning-of-line) (point))))))

(defun tag-table-files (tag-table)
  "Returns a list of the files referenced by the named TAGS table."
  (save-excursion
    (set-buffer (get-tag-table-buffer tag-table))
    (or tag-table-files
	(let (files prev size)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (forward-line 1)
	    (end-of-line)
	    (skip-chars-backward "^,\n")
	    (setq prev (point))
	    (setq size (read (current-buffer)))
	    (goto-char prev)
	    (setq files (cons (expand-file-name
			       (buffer-substring (1- (point))
						 (save-excursion
						   (beginning-of-line)
						   (point)))
			       default-directory)
			      files))
	    (forward-line 1)
	    (forward-char size))
	  (setq tag-table-files (nreverse files))))
    tag-table-files))

;; **** should this be on previous page?
(defun buffer-tag-table-files ()
  "Returns a list of all files referenced by all TAGS tables that 
this buffer uses."
  (apply (function append)
	 (mapcar (function tag-table-files)
		 (buffer-tag-table-list))))


;; Building the completion table

;; Test cases for building completion table; must handle these properly:
;; Lisp_Int, XSETINT, current_column 60,2282
;;	   Lisp_Int, XSETINT, point>NumCharacters ? 0 : CharAt(363,9935
;;	   Lisp_Int, XSETINT, point<=FirstCharacter ? 0 : CharAt(366,10108
;;	 point<=FirstCharacter || CharAt(378,10630
;;	 point>NumCharacters || CharAt(382,10825
;; DEFUN ("x-set-foreground-color", Fx_set_foreground_color,191,4562
;; DEFUN ("x-set-foreground-color", Fx_set_foreground_color,191,4562
;; DEFUN ("*", Ftimes,1172,32079
;; DEFUN ("/=", Fneq,1035,28839
;; defun_internal 4199,101362
;; int pure[PURESIZE / sizeof 53,1564
;; char staticvec1[NSTATICS * sizeof 667,17608
;;  Date: 04 May 87 23:53:11 PDT 26,1077
;; #define anymacroname(324,4344
;; (define-key ctl-x-map 311,11784
;; (define-abbrev-table 'c-mode-abbrev-table 24,1016
;; static char *skip_white(116,3443
;; static foo 348,11643
;; (defun texinfo-insert-@code 91,3358
;; (defvar texinfo-kindex)29,1105
;; (defun texinfo-format-\. 548,18376
;; (defvar sm::menu-kludge-y 621,22726
;; (defvar *mouse-drag-window* 103,3642
;; (defun simula-back-level(317,11263
;; } DPxAC,380,14024
;; } BM_QCB;69,2990
;; #define MTOS_DONE\t

;; "^[^ ]+ +\\([^ ]+\\) "

;; void *find_cactus_segment(116,2444
;; void *find_pdb_segment(162,3688
;; void init_dclpool(410,10739
;; WORD insert_draw_command(342,8881
;; void *req_pdbmem(579,15574

(defvar tag-completion-table (make-vector 511 0))

(defvar tag-symbol)
(defvar tag-table-symbol)
(defvar tag-symbol-tables)
(defvar buffer-tag-table-list)

;; make two versions of this, macro and non-macro,
;; and have the correct one used depending whether it's byte compiled
;; (well I think that's a little silly -- only lusers run interpreted! -jwz)
(defmacro intern-tag-symbol (tag)
  (`(progn
      (setq tag-symbol (intern (, tag) tag-completion-table)
	    tag-symbol-tables (and (boundp tag-symbol)
				   (symbol-value tag-symbol)))
      (or (memq tag-table-symbol tag-symbol-tables)
	  (set tag-symbol (cons tag-table-symbol tag-symbol-tables))))))

(defun intern-tag-symbol2 (tag)
  (setq tag-symbol (intern tag tag-completion-table)
	tag-symbol-tables (and (boundp tag-symbol)
			       (symbol-value tag-symbol)))
  (or (memq tag-table-symbol tag-symbol-tables)
      (set tag-symbol (cons tag-table-symbol tag-symbol-tables))))

;; This won't be evaluated at during byte-compilation, thus ensuring the
;; macro version will be used then.  Since the macro version is too slow
;; to use unless its usages are byte-compiled, we want to make sure we use
;; the non-macro version if we are using the non byte-compiled version of
;; add-to-tag-completion-table.
(fset 'intern-tag-symbol (symbol-function 'intern-tag-symbol2))

;; Can't use "\\s " in these patterns because that will include newline
(defconst tags-DEFUN-pattern
          "DEFUN[ \t]*(\"\\([^\"]+\\)\",[ \t]*\\(\\(\\sw\\|\\s_\\)+\\),\C-?")
(defconst tags-array-pattern ".*[ \t]+\\([^ \[]+\\)\\[")
(defconst tags-def-pattern
          "\\(.*[ \t]+\\)?\\(\\(\\sw\\|\\s_\\)+\\)[ ();,\t]*\C-?"
;; "\\(.*[ \t]+\\)?\\(\\(\\sw\\|\\s_\\)+\\)[ ()]*\C-?"
;; "\\(\\sw\\|\\s_\\)+[ ()]*\C-?"
      )
(defconst tags-file-pattern "^\f\n\\([^,]+\\),[0-9]+\n")

(defun add-to-tag-completion-table ()
  "Sucks the current buffer (a TAGS table) into the completion-table."
  (message "Adding %s to tags completion table..."
	   buffer-file-name)
  (goto-char (point-min))
  (let ((tag-table-symbol (intern buffer-file-name tag-completion-table))
	(original-syntax-table (syntax-table))
	;; tag-table-symbol is used by intern-tag-symbol
	filename file-type name name2 tag-symbol eol-point
	tag-symbol-tables file-type-syntax-table)
    (unwind-protect
	;; loop over the files mentioned in the TAGS file
	;; for each file, try to find its major-mode,
	;; then process tags appropriately
	(while (looking-at tags-file-pattern)
	  (goto-char (match-end 0))
	  (setq filename (buffer-substring (match-beginning 1) (match-end 1)))
	  (setq filename (file-name-sans-versions filename))
	  ;; clear loop variables
	  (setq file-type nil)
	  (setq file-type-syntax-table nil)
	  (setq name nil name2 nil)
	  (let ((alist auto-mode-alist)
		(case-fold-search (eq system-type 'vax-vms)))
	    ;; loop over pairs of regexps and major-modes
	    (while (and (not file-type) alist)
	      (if (string-match (car (car alist)) filename)
		  (setq file-type (cdr (car alist))))
	      (setq alist (cdr alist))))
	  ;; try to find a syntax table whose name begins with the major-mode
	  (if file-type
	      (setq file-type-syntax-table
		    (intern (concat (symbol-name file-type)
				    "-syntax-table"))))
	  ;;	  (message "%s %s" filename file-type-syntax-table)
	  (if (and file-type-syntax-table (boundp file-type-syntax-table))
	      (set-syntax-table (symbol-value file-type-syntax-table))
	    (set-syntax-table (standard-syntax-table)))
	  ;; loop over the individual tag lines
	  (while (not (or (eobp) (eq (following-char) ?\f)))
	    (cond ((and (eq file-type 'c-mode)
			(let ((case-fold-search nil))
			  (looking-at "DEFUN[ \t]")))
		   (or (looking-at tags-DEFUN-pattern)
		       (error "DEFUN doesn't fit pattern"))
		   (setq name (buffer-substring (match-beginning 1)
						(match-end 1)))
		   (setq name2 (buffer-substring (match-beginning 2)
						 (match-end 2))))
		  ((looking-at "\\s ")
		   ;; skip probably bogus entry:
		   )
		  ((and (eq file-type 'c-mode)
			(looking-at ".*\\["))
		   (or (looking-at tags-array-pattern)
		       (error "array definition doesn't fit pattern"))
		   (setq name (buffer-substring (match-beginning 1)
						(match-end 1))))
		  ((looking-at tags-def-pattern)
		   (setq name (buffer-substring (match-beginning 2)
						(match-end 2)))))
	    ;; add the tags we found to the completion table
	    (if name (intern-tag-symbol name))
	    (if name2 (intern-tag-symbol name2))
	    (forward-line 1)))
      (set-syntax-table original-syntax-table))
    (or (eobp) (error "Bad TAGS file")))
  (message "Adding %s to tags completion table...done"
	   buffer-file-name))


;; Interactive find-tag

(defvar find-tag-default-hook nil
  "****Function to call to create a default tag.
Make it buffer-local in a mode hook.
The function is called with no args.")

(defvar find-tag-hook nil
  "****Function to call after a hook is found.
Make it buffer-local in a mode hook.
The function is called with no args.")

;; Return a default tag to search for, based on the text at point.
(defun find-tag-default ()
  (or (and (boundp 'find-tag-default-hook)
	   (not (memq find-tag-default-hook '(nil find-tag-default)))
	   (condition-case data
	       (funcall find-tag-default-hook)
	     (error
	      (message "value of find-tag-default-hook signalled error: %s"
		       data)
	      (sit-for 1)
	      nil)))
      (save-excursion
	(if (not (memq (char-syntax (preceding-char)) '(?w ?_)))
	    (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
	      (forward-char 1)))
	(while (looking-at "\\sw\\|\\s_")
	  (forward-char 1))
	(if (re-search-backward "\\sw\\|\\s_" nil t)
	    (regexp-quote
	     (progn (forward-char 1)
		    (buffer-substring (point)
				      (progn (forward-sexp -1)
					     (while (looking-at "\\s'")
					       (forward-char 1))
					     (point)))))
	  nil))))

;;"\\(\\s \\|\\s.\\|\\s\(\\|\\s\)\\|\\s'\\|\\s\"\\|\\s$\\|\\s/\\|\\s\\\\|\\s<\\|\\s>\\)"
;;"[ \";]"

;;(defun non-symbol-char-regexp ()
;;  (let ((i 0)
;;	(numchars (length (syntax-table)))
;;	symbol-chars)
;;    (while (< i numchars)
;;      (if (memq (char-syntax i) '(?w ?_))
;;	  (setq symbol-chars (cons i symbol-chars)))
;;      (setq i (1+ i)))
;;    (concat symbol-chars)))

;; This function depends on the following symbols being bound properly:
;; buffer-tag-table-list,
;; tag-symbol-tables (value irrelevant, bound outside for efficiency)
(defun tag-completion-predicate (tag-symbol)
  (and (boundp tag-symbol)
       (setq tag-symbol-tables (symbol-value tag-symbol))
       (catch 'found
	 (while tag-symbol-tables
	   (if (memq (car tag-symbol-tables) buffer-tag-table-list)
	       (throw 'found t))
	   (setq tag-symbol-tables (cdr tag-symbol-tables))))))

(defun buffer-tag-table-symbol-list ()
  (mapcar (function
	   (lambda (table-name)
	     (intern table-name tag-completion-table)))
	  (buffer-tag-table-list)))
    
;;(defun strip-regexp-border (pattern)
;;  ;; Avoid displaying ugly regexp borders to the user
;;  (cond (pattern
;;	 (if (or (string-match "\\`\\[[^\]]+\\]" pattern)
;;		 ;;(string-match "\\`\\\\(\\([^\\\\]\\|\\\\[^\)]\\)+\\\\)"
;;		 ;;              pattern)
;;		 ;;(string-match "\\`\\\\[b<>`'WsS]" pattern)
;;		 )
;;	     (setq pattern (substring pattern
;;				      (match-end 0))))
;;	 (if (or (string-match "\\[[^\]]+\\]\\'" pattern)
;;		 ;;(string-match "\\\\(\\([^\\\\]\\|\\\\[^\)]\\)+\\\\)\\'"
;;		 ;;              pattern)
;;		 ;;(string-match "\\\\[b<>`'WsS]\\'" pattern)
;;		 )
;;	     (setq pattern (substring pattern 0
;;				      (match-beginning 0))))))
;;  pattern)

(defvar find-tag-history nil "History list for find-tag-tag")

(defun find-tag-tag (prompt)
  (let* ((default (find-tag-default))
	 (buffer-tag-table-list (buffer-tag-table-symbol-list))
	 tag-symbol-tables tag-name)
    (setq tag-name
	  (completing-read
	   (if default
	       (format "%s(default %s) " prompt default)
	     prompt)
	   tag-completion-table 'tag-completion-predicate nil nil
	   'find-tag-history))
    (if (string-equal tag-name "")
	(list default)			;indicate exact symbol match
      tag-name)))

(defvar last-tag-data nil
"Information for continuing a tag search.
Is of the form (TAG POINT TAG-TABLE TAG-TABLE ...).")

(defvar tags-loop-form nil
  "Form for tags-loop-continue to eval to process one file.
If it returns nil, it is through with one file; move on to next.")

(autoload 'get-symbol-syntax-table "symbol-syntax")

(defun find-tag-internal (tagname)
  "Foo"
  (let ((local-find-tag-hook find-tag-hook)
	(next (null tagname))
	(exact (consp tagname))
	symbol-border tag-target
	tag-tables tag-table-point file linebeg startpos target buf
	offset found pat syn-tab)
    (if exact (setq tagname (car tagname)))
    (cond (next
	   (setq tag-tables (cdr (cdr last-tag-data)))
	   (setq tagname (car last-tag-data))
	   (setq tag-table-point (car (cdr last-tag-data))))
	  (t
	   (setq tag-tables (buffer-tag-table-list))
	   (setq tag-table-point 1)))
    ;; If tagname is a list: (TAGNAME), this indicates requiring an exact
    ;; symbol match.  Similarly, \_ in the tagname is used to indicate a
    ;; symbol boundary.
    (cond ((or exact
	       (string-match "\\\\_" tagname))
	   (setq symbol-border t)
	   (if exact
	       (setq tag-target (concat "\\_" tagname "\\_"))
	     (setq tag-target (copy-sequence tagname)))
	   (while (string-match "\\\\_" tag-target)
	     (aset tag-target (1- (match-end 0)) ?b))
	   (setq syn-tab (get-symbol-syntax-table (syntax-table)))
	   ;;	   (let ((i 0)
	   ;;		 (len (length tag-target))
	   ;;		 j)
	   ;;	     (while (< i len)
	   ;;	       (cond ((eq ?\\ (aref tag-target i))
	   ;;		      (setq j (1+ i))
	   ;;		      (if (eq ?_ (aref tag-target j))
	   ;;			  (aset tag-target j ?b))))
	   ;;	       (setq i (1+ i))))
	   )
	  (t
	   (setq tag-target tagname)
	   (setq syn-tab (syntax-table))))
    (save-excursion
      (catch 'found
	(while tag-tables
	  (set-buffer (get-tag-table-buffer (car tag-tables)))
	  (bury-buffer (current-buffer))
	  (goto-char (or tag-table-point (point-min)))
	  (setq tag-table-point nil)
	  (let ((osyn (syntax-table))
		case-fold-search)
	    (set-syntax-table syn-tab)
	    (unwind-protect
		;; **** should there be support for non-regexp tag searches?
		(while (re-search-forward tag-target nil t)
		  (if (looking-at "[^\n\C-?]*\C-?")
		      (throw 'found t)))
	      (set-syntax-table osyn)))
	  (setq tag-tables (cdr tag-tables)))
	(error "No %sentries %s %s"
	       (if next "more " "")
	       (if exact "matching" "containing")
	       tagname))
      (search-forward "\C-?")
      (setq file (expand-file-name (file-of-tag)))
      (setq linebeg
	    (buffer-substring (1- (point))
			      (save-excursion (beginning-of-line) (point))))
      (search-forward ",")
      (setq startpos (read (current-buffer)))
      (setq last-tag-data (nconc (list tagname (point)) tag-tables)))
    (setq buf (find-file-noselect file))
    (save-excursion
      (set-buffer buf)
      (save-excursion
	(save-restriction
	  (widen)
	  (setq offset 1000)
	  (setq pat (concat "^" (regexp-quote linebeg)))
	  (or startpos (setq startpos (point-min)))
	  (while (and (not found)
		      (progn
			(goto-char (- startpos offset))
			(not (bobp))))
	    (setq found (re-search-forward pat (+ startpos offset) t))
	    (setq offset (* 3 offset)))
	  (or found
	      (re-search-forward pat nil t)
	      (error "%s not found in %s" pat file))
	  (beginning-of-line)
	  (setq startpos (point)))))
    (cons buf startpos)))

(defun find-tag (tagname &optional other-window)
  "*Find tag whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If called interactively with a numeric argument, searches for the next tag
in the tag table that matches the tagname used in the previous find-tag.
 If second arg OTHER-WINDOW is non-nil, uses another window to display
the tag.

This version of this function supports multiple active tags tables,
and completion.

Variables of note:

  tag-table-alist		controls which tables apply to which buffers
  tags-file-name		a default tags table
  tags-build-completion-table   controls completion behavior
  buffer-tag-table		another way of specifying a buffer-local table
  make-tags-files-invisible	whether tags tables should be very hidden
  tag-mark-stack-max		how many tags-based hops to remember"
  (interactive (if current-prefix-arg
		   '(nil nil)
		 (list (find-tag-tag "Find tag: ") nil)))
  (let* ((local-find-tag-hook find-tag-hook)
	 (next (null tagname))
	 (result (find-tag-internal tagname))
	 (tag-buf (car result))
	 (tag-point (cdr result)))
    ;; push old position
    (if (or (not next)
	    (not (memq last-command
		       '(find-tag find-tag-other-window tags-loop-continue))))
	(push-tag-mark))
    (if other-window
	(pop-to-buffer tag-buf)
      (switch-to-buffer tag-buf))
    (widen)
    (push-mark)
    (goto-char tag-point)
    (if find-tag-hook
	(funcall find-tag-hook)
      (if local-find-tag-hook
	  (funcall local-find-tag-hook))))
  (setq tags-loop-form (list 'find-tag nil nil))
  ;; Return t in case used as the tags-loop-form.
  t)

;; This function is unchanged from lisp/tags.el:
(defun find-tag-other-window (tagname &optional next)
  "*Find tag whose name contains TAGNAME.
 Selects the buffer that the tag is contained in in another window
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

This version of this function supports multiple active tags tables,
and completion.

Variables of note:

  tag-table-alist		controls which tables apply to which buffers
  tags-file-name		a default tags table
  tags-build-completion-table   controls completion behavior
  buffer-tag-table		another way of specifying a buffer-local table
  make-tags-files-invisible	whether tags tables should be very hidden
  tag-mark-stack-max		how many tags-based hops to remember"
  (interactive (if current-prefix-arg
		   '(nil t)
		 (list (find-tag-tag "Find tag other window: "))))
  (if next
      (find-tag nil t)
    (find-tag tagname t)))


;; Completion on tags in the buffer

(defun lisp-complete-symbol ()
  "*Perform completion on Lisp symbol preceding point.
That symbol is compared against the symbols that exist
and any additional characters determined by what is there
are inserted.
If the symbol starts just after an open-parenthesis,
only symbols with function definitions are considered.
Otherwise, all symbols with function definitions, values
or properties are considered."
  (interactive)
  (let ((buffer-syntax (syntax-table)))
    (unwind-protect
	(progn
	  (if lisp-mode-syntax-table
	      (set-syntax-table lisp-mode-syntax-table))
	  (let ((fn (save-excursion
		      (backward-sexp 1)
		      (while (= (char-syntax (following-char)) ?\')
			(forward-char 1))
		      (eq (preceding-char) ?\())))
	    (complete-symbol
	     obarray
	     (if fn
		 'fboundp
	       (function
		(lambda (sym)
		  (or (boundp sym)
		      (fboundp sym)
		      (symbol-plist sym)))))
	     (if (not fn)
		 ;; prettify the completion list by marking fns with " <f>"
		 (function
		  (lambda (list)
		    (let (new)
		      (while list
			(setq new (cons (if (fboundp (intern (car list)))
					    (list (car list) " <f>")
					  (car list))
					new))
			(setq list (cdr list)))
		      (nreverse new))))))))
      ;; unwind-protected
      (set-syntax-table buffer-syntax))))

(defun complete-symbol (&optional table predicate prettify)
  (let* ((end (point))
	 (beg (save-excursion
		(backward-sexp 1)
		(while (= (char-syntax (following-char)) ?\')
		  (forward-char 1))
		(point)))
	 (pattern (buffer-substring beg end))
	 (table (or table obarray))
	 (completion (try-completion pattern table predicate)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string-equal pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern table predicate)))
	     (if prettify
		 (setq list (funcall prettify list)))
	     (with-output-to-temp-buffer "*Help*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))

(defun tag-complete-symbol ()
  "The function used to do tags-completion (using 'tag-completion-predicate)."
  (interactive)
  (let* ((buffer-tag-table-list (buffer-tag-table-symbol-list))
	 tag-symbol-tables)
    (complete-symbol tag-completion-table 'tag-completion-predicate)))


;; Applying a command to files mentioned in tag tables

(defvar next-file-list nil
  "List of files for next-file to process.")

(defun next-file (&optional initialize)
  "Select next file among files in current tag table(s).
Non-nil argument (prefix arg, if interactive) initializes to the beginning 
of the list of files in the (first) tag table."
  (interactive "P")
  (if initialize
      (setq next-file-list (buffer-tag-table-files)))
  (or next-file-list
      (error "All files processed."))
  (let* ((file (car next-file-list))
	 (buf (get-file-buffer file))
	 new)
    (setq next-file-list (cdr next-file-list))
    (or buf
	(setq buf (find-file-noselect file)
	      new t))
    (switch-to-buffer buf t)
    (widen)
    (cond ((> (point) (point-min))
	   (push-mark nil t)
	   (goto-char (point-min))))
    new))

(defvar tags-search-nuke-uninteresting-buffers t
  "*If t (the default), tags-search and tags-query-replace will only
keep newly-visited buffers if they contain the search target.")

(defun tags-loop-continue (&optional first-time)
  "Continue last \\[tags-search] or \\[tags-query-replace] command.
Used noninteractively with non-nil argument
to begin such a command.  See variable tags-loop-form."
  (interactive)
  (let (buf-is-new message)
    (cond (first-time
	   (setq buf-is-new (next-file t))
	   (message "Scanning file %s..." buffer-file-name)
	   (setq message t)))
    ;; **** (let ((cursor-in-echo-area t)))
    (while (not (eval tags-loop-form))
      (if (and buf-is-new (not (buffer-modified-p))
	       tags-search-nuke-uninteresting-buffers)
	  (kill-buffer (current-buffer)))
      (setq buf-is-new (next-file))
      (message "Scanning file %s..." buffer-file-name)
      (setq message t))
    (switch-to-buffer (current-buffer))
    (if message
	(message "Scanning file %s...done" buffer-file-name))))

;; This function is unchanged from lisp/tags.el:
(defun tags-search (regexp)
  "Search through all files listed in tag table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

See documentation of variable tag-table-alist."
  (interactive "sTags search (regexp): ")
  (if (and (equal regexp "")
	   (eq (car tags-loop-form) 're-search-forward))
      (tags-loop-continue nil)
    (setq tags-loop-form
	  (list 're-search-forward regexp nil t))
    (tags-loop-continue t)))

;; This function is unchanged from lisp/tags.el:
(defun tags-query-replace (from to &optional delimited)
  "Query-replace-regexp FROM with TO through all files listed in tag table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (C-G or ESC), you can resume the query-replace
with the command \\[tags-loop-continue].

See documentation of variable tag-table-alist."
  (interactive "sTags query replace (regexp): \nsTags query replace %s by: \nP")
  (setq tags-loop-form
	(list 'and (list 'save-excursion
			 (list 're-search-forward from nil t))
	      (list 'not (list 'perform-replace from to t t 
			       (not (null delimited))))))
  (tags-loop-continue t))


;; Miscellaneous

;; **** need to alter
;; This function is unchanged from lisp/tags.el:
(defun list-tags (string)
  "Display list of tags in file FILE.
FILE should not contain a directory spec
unless it has one in the tag table."
  (interactive "sList tags (in file): ")
  (with-output-to-temp-buffer "*Tags List*"
    (princ "Tags in file ")
    (princ string)
    (terpri)
    (save-excursion
     (visit-tags-table-buffer)
     (goto-char 1)
     (search-forward (concat "\f\n" string ","))
     (forward-line 1)
     (while (not (or (eobp) (looking-at "\f")))
       (princ (buffer-substring (point)
				(progn (skip-chars-forward "^\C-?")
				       (point))))
       (terpri)
       (forward-line 1)))))

;; **** need to alter
;; This function is unchanged from lisp/tags.el:
(defun tags-apropos (string)
  "Display list of all tags in tag table REGEXP matches."
  (interactive "sTag apropos (regexp): ")
  (with-output-to-temp-buffer "*Tags List*"
    (princ "Tags matching regexp ")
    (prin1 string)
    (terpri)
    (save-excursion
     (visit-tags-table-buffer)
     (goto-char 1)
     (while (re-search-forward string nil t)
       (beginning-of-line)
       (princ (buffer-substring (point)
				(progn (skip-chars-forward "^\C-?")
				       (point))))
       (terpri)
       (forward-line 1)))))

;; **** copied from tags.el
(defun visit-tags-table-buffer ()
  "Select the buffer containing the current tag table.
This is a file whose name is in the variable tags-file-name."
  (or tags-file-name
      (call-interactively 'visit-tags-table))
  (set-buffer (or (get-file-buffer tags-file-name)
		  (progn
		    (setq tag-table-files nil)
		    (find-file-noselect tags-file-name))))
  (or (verify-visited-file-modtime (get-file-buffer tags-file-name))
      (cond ((yes-or-no-p "Tags file has changed, read new contents? ")
	     (revert-buffer t t)
	     (setq tag-table-files nil))))
  (or (eq (char-after 1) ?\^L)
      (error "File %s not a valid tag table" tags-file-name)))


;; Sample uses of find-tag-hook and find-tag-default-hook

;; Example buffer-local tag finding

(or (boundp 'emacs-lisp-mode-hook)
    (setq emacs-lisp-mode-hook nil))
(if (eq (car-safe emacs-lisp-mode-hook) 'lambda)
    (setq emacs-lisp-mode-hook (list emacs-lisp-mode-hook)))
(or (memq 'setup-emacs-lisp-default-tag-hook emacs-lisp-mode-hook)
    (setq emacs-lisp-mode-hook
	  (cons 'setup-emacs-lisp-default-tag-hook emacs-lisp-mode-hook)))

(defun setup-emacs-lisp-default-tag-hook ()
  (cond ((eq major-mode 'emacs-lisp-mode)
	 (make-variable-buffer-local 'find-tag-default-hook)
	 (setq find-tag-default-hook 'emacs-lisp-default-tag))))
;; Run it once immediately
(setup-emacs-lisp-default-tag-hook)
(if (get-buffer "*scratch*")
    (save-excursion (set-buffer "*scratch*")
		    (setup-emacs-lisp-default-tag-hook)))

(defun emacs-lisp-default-tag ()
  "Function to return a default tag for Emacs-Lisp mode."
  (let ((tag (or (variable-at-point)
		 (function-called-at-point))))
    (if tag (symbol-name tag))))

;;(defun Info-find-tag-hook ()
;;  "Function to call after finding a tag in Info-mode."
;;  (let ((onode Info-current-node)
;;	(ofile Info-current-file)
;;	(opoint (point)))
;;    (if (not (string= "*info*" (buffer-name)))
;;	(progn				; replace current *info* file
;;	  (kill-buffer "*info*")
;;	  (rename-buffer "*info*")))
;;    (or (eq major-mode 'Info-mode)
;;	(Info-mode))
;;    (setq Info-current-file
;;	  (file-name-sans-versions buffer-file-name))
;;    (Info-select-node)
;;    (or (and (equal onode Info-current-node)
;;	     (equal ofile Info-current-file))
;;	(setq Info-history (cons (list ofile onode opoint)
;;				 Info-history)))))
;;
;;;; Info-mode does not have a hook, so patch in the necessary calls.
;;
;;(require 'info)
;;
;;;; Only do this once
;;(fset 'Info-mode
;;      (append (symbol-function 'Info-mode)
;;	      (list '(make-local-variable 'find-tag-hook)
;;		    '(setq find-tag-hook 'Info-find-tag-hook)
;;		    '(modify-syntax-entry ?\' "."))))


;; Display short info on tag in minibuffer

(if (null (lookup-key esc-map "?"))
    (define-key esc-map "?" 'display-tag-info))

(defun display-tag-info (tagname)
  "Prints a description of the first tag matching TAGNAME in the echo area.
If this is an elisp function, prints something like \"(defun foo (x y z)\".
That is, is prints the first line of the definition of the form.
If this is a C-defined elisp function, it does something more clever."
  (interactive (if current-prefix-arg
		   '(nil)
		 (list (find-tag-tag "Display tag info: "))))
  (let* ((results (find-tag-internal tagname))
	 (tag-buf (car results))
	 (tag-point (cdr results))
	 info lname min max fname args)
    (save-excursion
      (set-buffer tag-buf)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char tag-point)
	  (cond ((let ((case-fold-search nil))
		   (looking-at "^DEFUN[ \t]"))
		 (forward-sexp 1)
		 (down-list 1)
		 (setq lname (read (current-buffer))
		       fname (buffer-substring
			      (progn (forward-sexp 1) (point))
			      (progn (backward-sexp 1) (point)))
		       min (buffer-substring
			    (progn (forward-sexp 3) (point))
			    (progn (backward-sexp 1) (point)))
		       max (buffer-substring
			    (progn (forward-sexp 2) (point))
			    (progn (backward-sexp 1) (point))))
		 (backward-up-list 1)
		 (setq args (buffer-substring
			     (progn (forward-sexp 2) (point))
			     (progn (backward-sexp 1) (point))))
		 (setq info (format "Elisp: %s, C: %s %s, #args: %s"
				    lname
				    fname args
				    (if (string-equal min max)
					min
				      (format "from %s to %s" min max)))))
		(t
		 (setq info
		       (buffer-substring
			(progn (beginning-of-line) (point))
			(progn (end-of-line) (point)))))))))
    (message "%s" info))
  (setq tags-loop-form '(display-tag-info nil))
  ;; Always return non-nil
  t)


;; Keep track of old locations before finding tags

(defvar tag-mark-stack1 nil)
(defvar tag-mark-stack2 nil)
(defvar tag-mark-stack-max 16
  "*The maximum number of elements kept on the mark-stack used
by tags-search.  See also the commands push-tag-mark (\\[push-tag-mark])
and pop-tag-mark. (\\[pop-tag-mark]).")

(defun push-mark-on-stack (stack-symbol &optional max-size)
  (let ((stack (symbol-value stack-symbol)))
    (setq stack (cons (point-marker) stack))
    (cond ((and max-size
		(> (length stack) max-size))
	   (set-marker (car (nthcdr max-size stack)) nil)
	   (setcdr (nthcdr (1- max-size) stack) nil)))
    (set stack-symbol stack)))

(defun pop-mark-from-stack (stack-symbol1 stack-symbol2 &optional max-size)
  (let* ((stack (or (symbol-value stack-symbol1)
		    (error "No more tag marks on stack")))
	 (marker (car stack))
	 (m-buf (marker-buffer marker)))
    (set stack-symbol1 (cdr stack))
    (or m-buf
	(error "Marker has no buffer"))
    (if (null (buffer-name m-buf))
	(error "Buffer has been killed"))
    (push-mark-on-stack stack-symbol2 max-size)
    (switch-to-buffer m-buf)
    (widen)
    (goto-char (marker-position marker))))

(defun push-tag-mark ()
  (push-mark-on-stack 'tag-mark-stack1 tag-mark-stack-max))

(if (memq (lookup-key esc-map "*") '(nil undefined))
    (define-key esc-map "*" 'pop-tag-mark))

(defun pop-tag-mark (arg)
  "find-tag maintains a mark-stack seperate from the \\[set-mark-command] mark-stack.
This function pops (and moves to) the tag at the top of this stack."
  (interactive "P")
  (if (not arg)
      (pop-mark-from-stack
       'tag-mark-stack1 'tag-mark-stack2 tag-mark-stack-max)
    (pop-mark-from-stack
     'tag-mark-stack2 'tag-mark-stack1 tag-mark-stack-max)))



;; John Sturdy <jcgs@harlqn.co.uk>
;; (defun lookup-tag (use-rec-edit)
;;   "Show a tag from the current tags name list in the other window for
;; reference, then restore the window layout after a pause. With prefix
;; arg, go into a recursive edit instead of pausing."
;;   (interactive "P")
;;   (save-window-excursion
;;     (save-excursion
;;       (find-tag-other-window (completing-read "Tag name: " tags-name-list))
;;       (if use-rec-edit
;;           (recursive-edit)
;;         (sit-for show-tag-time)))))

;(provide 'tags-fix)
(provide 'tags)
