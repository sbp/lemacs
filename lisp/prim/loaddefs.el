;; Define standard autoloads.
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

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;; Special formatting conventions are used in this file!
;;;
;;; a backslash-newline is used at the beginning of a documentation string
;;; when that string should be stored in the file etc/DOCnnn, not in core.
;;;
;;; Such strings read into Lisp as numbers (during the pure-loading phase).
;;;
;;; But you must obey certain rules to make sure the string is understood
;;; and goes into etc/DOCnnn properly.  Otherwise, the string will not go
;;; anywhere!
;;;
;;; The doc string must appear in the standard place in a call to
;;; defun, autoload, defvar or defconst.  No Lisp macros are recognized.
;;; The open-paren starting the definition must appear in column 0.
;;;
;;; In defvar and defconst, there is an additional rule:
;;; The double-quote that starts the string must be on the same
;;; line as the defvar or defconst.
;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;; Know which function the debugger is!
(setq debugger 'debug)

(defconst mode-line-buffer-identification (purecopy '("Emacs: %17b")) "\
Mode-line control for identifying the buffer being displayed.
Its default value is \"Emacs: %17b\".  Major modes that edit things
other than ordinary files may change this (e.g. Info, Dired,...)")

(make-variable-buffer-local 'mode-line-buffer-identification)

(defconst mode-line-process nil "\
Mode-line control for displaying info on process status.
Normally nil in most modes, since there is no process to display.")

(make-variable-buffer-local 'mode-line-process)

(defconst mode-line-modified (purecopy '("--%1*%1*-")) "\
Mode-line control for displaying whether current buffer is modified.")

(make-variable-buffer-local 'mode-line-modified)

(setq-default mode-line-format
  (list (purecopy "")
   'mode-line-modified
   'mode-line-buffer-identification
   (purecopy "   ")
   'global-mode-string
   (purecopy "   %[(")
   'mode-name 'minor-mode-alist "%n" 'mode-line-process
   (purecopy ")%]----")
   (purecopy '(-3 . "%p"))
   (purecopy "-%-")))

(defvar minor-mode-alist nil "\
Alist saying how to show minor modes in the mode line.
Each element looks like (VARIABLE STRING);
STRING is included in the mode line iff VARIABLE's value is non-nil.")
(setq minor-mode-alist (mapcar 'purecopy
			       '((abbrev-mode " Abbrev")
				 (overwrite-mode " Ovwrt")
				 (auto-fill-function " Fill")
				 ;; not really a minor mode...
				 (defining-kbd-macro " Def"))))

;; These variables are used by autoloadable packages.
;; They are defined here so that they do not get overridden
;; by the loading of those packages.

(defconst paragraph-start "^[ \t\n\f]" "\
*Regexp for beginning of a line that starts OR separates paragraphs.")
(defconst paragraph-separate "^[ \t\f]*$" "\
*Regexp for beginning of a line that separates paragraphs.
If you change this, you may have to change paragraph-start also.")

(defconst sentence-end   (purecopy "[.?!][]\"')}]*\\($\\| $\\|\t\\|  \\)[ \t\n]*") "\
*Regexp describing the end of a sentence.
All paragraph boundaries also end sentences, regardless.")

(defconst page-delimiter "^\014" "\
*Regexp describing line-beginnings that separate pages.")

(defconst case-replace t "\
*Non-nil means query-replace should preserve case in replacements.")

;; indent.el may not be autoloading, but it still loses
;; if lisp-mode is ever called before this defvar is done.
(defvar indent-line-function 'indent-to-left-margin "\
Function to indent current line.")

(defconst only-global-abbrevs nil "\
*t means user plans to use global abbrevs only.
Makes the commands to define mode-specific abbrevs define global ones instead.")

;; Names in directory that end in one of these
;; are ignored in completion,
;; making it more likely you will get a unique match.
(setq completion-ignored-extensions
      (if (eq system-type 'vax-vms)
	  '(".obj" ".elc" ".exe" ".bin" ".lbin" ".sbin"
	    ".dvi" ".toc" ".log" ".aux"
	    ".lof" ".brn" ".rnt" ".mem" ".lni" ".lis"
	    ".olb" ".tlb" ".mlb" ".hlb" ".glo" ".idx" ".lot" ".fmt")
	'(".o" ".elc" "~" ".bin" ".lbin" ".fasl"
	  ".dvi" ".toc" ".log" ".aux" ".a" ".ln"
	  ".lof" ".blg" ".bbl" ".glo" ".idx" ".lot" ".fmt"
	  ".diff" ".oi")))

(defconst grep-command "grep -n" "\
*Name of the command to use to run the grep command;
typically \"grep -n\" or \"egrep -n\".
\(The \"-n\" option tells grep to output line numbers.)")

(defvar dired-listing-switches "-al" "\
*Switches passed to `ls' for Dired.  MUST contain the `l' option.")

(defconst lpr-switches nil "\
*List of strings to pass as extra args to `lpr' when it is invoked.")

(defvar tags-always-build-completion-table nil "
*If t, tags files will always be added to the completion table without 
asking first, regardless of the size of the tags file.")

(defvar tag-table-alist nil "\
*A list which determines which tags files should be active for a 
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

(defvar tags-file-name nil "\
*The name of the tags-table used by all buffers.
This is for backward compatibility, and is largely supplanted by the
variable tag-table-alist.")

(defconst shell-prompt-pattern "^\\(([^() \n]+)\\|[^#$%>\n]*[#$%>]\\) *" "\
*Regexp used by Newline command in shell mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not reexecuted.")

(defconst ledit-save-files t "\
*Non-nil means Ledit should save files before transferring to Lisp.")
(defconst ledit-go-to-lisp-string "%?lisp" "\
*Shell commands to execute to resume Lisp job.")
(defconst ledit-go-to-liszt-string "%?liszt" "\
*Shell commands to execute to resume Lisp compiler job.")

(defconst display-time-day-and-date nil "\
*Non-nil means M-x display-time should display day and date as well as time.")

;;; Determine mode according to filename

(defvar auto-mode-alist nil "\
Alist of filename patterns vs corresponding major mode functions.
Each element looks like (REGEXP . FUNCTION).
Visiting a file whose name matches REGEXP causes FUNCTION to be called.")
(setq auto-mode-alist (mapcar 'purecopy
			      '(("\\.te?xt$" . text-mode)
				("\\.c$" . c-mode)
				("\\.h$" . c-mode)
				("\\.tex$" . TeX-mode)
				("\\.ltx$" . LaTeX-mode)
				("\\.el$" . emacs-lisp-mode)
 				("\\.mm$" . nroff-mode)
 				("\\.me$" . nroff-mode)
 				("\\.[12345678]$" . nroff-mode)
				("\\.scm$" . scheme-mode)
				("\\.l$" . lisp-mode)
				("\\.lisp$" . lisp-mode)
				("\\.f$" . fortran-mode)
				("\\.for$" . fortran-mode)
				("\\.mss$" . scribe-mode)
				("\\.pl$" . prolog-mode)
				("\\.cc$" . c++-mode)
				("\\.hh$" . c++-mode)
				("\\.C$" . c++-mode)
				("\\.H$" . c++-mode)
;;; Less common extensions come here
;;; so more common ones above are found faster.
				("ChangeLog$" . change-log-mode)
				("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode)
				("\\.TeX$" . TeX-mode)
				("\\.sty$" . LaTeX-mode)
				("\\.bbl$" . LaTeX-mode)
				("\\.bib$" . text-mode)
				("\\.article$" . text-mode)
				("\\.letter$" . text-mode)
				("\\.texinfo$" . texinfo-mode)
				("\\.lsp$" . lisp-mode)
				("\\.awk$" . awk-mode)
				("\\.prolog$" . prolog-mode)
				;; Mailer puts message to be edited in
				;; /tmp/Re.... or Message
				("^/tmp/Re" . text-mode)
				("/Message[0-9]*$" . text-mode)
				;; some news reader is reported to use this
				("^/tmp/fol/" . text-mode)
				("\\.y$" . c-mode)
				("\\.oak$" . scheme-mode)
				("\\.scm.[0-9]*$" . scheme-mode)
				("\\.tar$" . tar-mode)
				("\\.c?ps$" . postscript-mode)
				;; .emacs following a directory delimiter
				;; in either Unix or VMS syntax.
				("[]>:/]\\..*emacs" . emacs-lisp-mode)
				("\\.ml$" . lisp-mode))))

(make-variable-buffer-local 'indent-tabs-mode)

;; Reduce total amount of space we must allocate during this function
;; that we will not need to keep permanently.
(garbage-collect)

;; Autoload random libraries.
;; Alphabetical order by library name.

(autoload 'add-change-log-entry "add-log"
  "\
Find change log file and add an entry for today.
First arg (interactive prefix) non-nil means prompt for user name and site.
Second arg is file name of change log.
Optional third arg OTHER-WINDOW non-nil means visit in other window."
  t)

(autoload 'add-change-log-entry-other-window "add-log"
  "\
Find change log file in other window, and add an entry for today."
  t)

(autoload 'apropos "apropos"
  "\
Show all symbols whose names contain matches for REGEXP.
If optional argument DO-ALL is non-nil, does more (time-consuming) work such as
showing key bindings.  Optional argument PRED is called with each symbol, and
if it returns nil, the symbol is not shown.  Returns list of symbols and
documentation found."
  t)

(autoload 'super-apropos "apropos"
  "\
Show symbols whose names/documentation contain matches for REGEXP.
If optional argument DO-ALL is non-nil, does more (time-consuming) work such as
showing key bindings and documentation that is not stored in the documentation
file.  Returns list of symbols and documentation found."
  t)

(autoload 'awk-mode "awk-mode"
  "\
Major mode for editing AWK code.
This is much like C mode except for the syntax of comments.
It uses the same keymap as C mode and has the same variables
for customizing indentation.  It has its own abbrev table
and its own syntax table.

Turning on AWK mode calls the value of the variable `awk-mode-hook'
with no args, if that value is non-nil." t)

;; don't backslash this, make-docfile can't cope
(autoload '` "backquote"
  "\
(` FORM)  is a macro that expands to code to construct FORM.
Note that this is very slow in interpreted code, but fast if you compile.
FORM is one or more nested lists, which are `almost quoted':
They are copied recursively, with non-lists used unchanged in the copy.
 (` a b) == (list 'a 'b)  constructs a new list with two elements, `a' and `b'.
 (` a (b c)) == (list 'a (list 'b 'c))  constructs two nested new lists.

However, certain special lists are not copied.  They specify substitution.
Lists that look like (, EXP) are evaluated and the result is substituted.
 (` a (, (+ x 5))) == (list 'a (+ x 5))

Elements of the form (,@ EXP) are evaluated and then all the elements
of the result are substituted.  This result must be a list; it may
be `nil'.

As an example, a simple macro `push' could be written:
   (defmacro push (v l)
        (` (setq (, l) (cons (,@ (list v l))))))
or as
   (defmacro push (v l)
        (` (setq (, l) (cons (, v) (, l)))))

LIMITATIONS: \"dotted lists\" are not allowed in FORM.
The ultimate cdr of each list scanned by ` must be `nil'.
\(This does not apply to constants inside expressions to be substituted.)

Substitution elements are not allowed as the cdr
of a cons cell.  For example, (` (A . (, B))) does not work.
Instead, write (` (A (,@ B))).

You cannot construct vectors, only lists.  Vectors are treated as
constants.

BEWARE BEWARE BEWARE
Inclusion of (,ATOM) rather than (, ATOM)
or of (,@ATOM) rather than (,@ ATOM)
will result in errors that will show up very late."  nil t)

(autoload 'byte-compile-file "bytecomp"
  "\
Compile a file of Lisp code named FILENAME into a file of byte code.
The output file's name is made by appending \"c\" to the end of FILENAME.
With prefix arg (noninteractively: 2nd arg), load the file after compiling."
  t)

(autoload 'byte-compile-and-load-file "bytecomp"
  "\
Compile a file of Lisp code named FILENAME into a file of byte code,
and then load it.  The output file's name is made by appending \"c\" to 
the end of FILENAME."
  t)

(autoload 'byte-compile-buffer "bytecomp"
  "\
Byte-compile and evaluate contents of BUFFER (default: the current buffer)."
  t)

(autoload 'elisp-compile-defun "bytecomp"
  "\
Compile and evaluate the current top-level form.
Print the result in the minibuffer.
With argument, insert value in current buffer after the form."
  t)

(autoload 'byte-recompile-directory "bytecomp"
  "\
Recompile every `.el' file in DIRECTORY that needs recompilation.
This is if a `.elc' file exists but is older than the `.el' file.

If the `.elc' file does not exist, normally the `.el' file is *not* compiled.
But a prefix argument (optional second arg) means ask user,
for each such `.el' file, whether to compile it."
  t)

(autoload 'batch-byte-compile "bytecomp"
  "\
Runs byte-compile-file on the files remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke \"emacs -batch -f batch-byte-compile $emacs/ ~/*.el\""
  nil)

(autoload 'batch-byte-recompile-directory "bytecomp"
  "\
Runs `byte-recompile-directory' on the dirs remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
For example, invoke \"emacs -batch -f batch-byte-recompile-directory .\""
  nil)

(autoload 'set-c-style "c-style"
  "\
Specify a style of indentation for C code for the current buffer.
The argument STYLE should be a standard style name defined in
`c-style-alist', such as GNU, BSD, K&R, etc.  (These are Lisp symbols.)
An omitted arg, or nil, means to use the value of `default-c-style'.

Setting the style sets various C-mode customization parameters accordingly,
all local to the current buffer." t)

(autoload 'c++-mode "c++-mode"
  "\
Major mode for editing C++ code.  Very much like editing C code.
Expression and list commands understand all C++ brackets.
Tab at left margin indents for C++ code.
Comments are delimited with /* ... */ {or with // ... <newline>}
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{c++-mode-map}
Variables controlling indentation style:
 c-tab-always-indent
    Non-nil means TAB in C mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
    Default is t.
 c-auto-newline
    Non-nil means automatically newline before and after braces,
    and after colons and semicolons, inserted in C code.
 c-indent-level
    Indentation of C statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 c-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 c-brace-offset
    Extra indentation for line if it starts with an open brace.
 c-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 c-argdecl-indent
    Indentation level of declarations of C function arguments.
 c-label-offset
    Extra indentation for line that is a label, or case or default.
 c++-electric-colon
    If non-nil at invocation of c++-mode (t is the default) colon electricly
    indents.
 c++-empty-arglist-indent
    If non-nil, a function declaration or invocation which ends a line with a
    left paren is indented this many extra spaces, instead of flush with the
    left paren.
 c++-friend-offset
    Offset of C++ friend class declarations relative to member declarations.
 c++-member-init-indent
    Indentation level of member initializations in function declarations,
    if they are on a separate line beginning with a colon.
 c++-continued-member-init-offset
    Extra indentation for continuation lines of member initializations; NIL
    means to align with previous initializations rather than with the colon.

Turning on C++ mode calls the value of the variable c++-mode-hook with
no args, if that value is non-nil."
  t)

(autoload 'font-lock-mode "font-lock"
  "\
Toggle Font Lock Mode.
With arg, turn font-lock mode on if and only if arg is positive.
In the font-lock minor mode, text is fontified as you type it:

 - comments are displayed in font-lock-comment-face;
 - strings are displayed in font-lock-string-face;
 - documentation strings are displayed in font-lock-doc-string-face;
 - function and variable names in their defining forms are displayed
   in font-lock-function-name-face;
 - and certain other expressions are displayed in other faces
   according to the value of the variable `font-lock-keywords'.

When font-lock mode is turned on/off, the buffer is fontified/defontified.
To fontify a buffer without having newly typed text become fontified, you
can use \\[font-lock-fontify-buffer]."
  t)

(autoload 'font-lock-fontify-buffer "font-lock"
  "\
Fontify the current buffer the way `font-lock-mode' would:

 - comments are displayed in font-lock-comment-face;
 - strings are displayed in font-lock-string-face;
 - documentation strings are displayed in font-lock-doc-string-face;
 - function and variable names in their defining forms are displayed
   in font-lock-function-name-face;
 - and certain other expressions are displayed in other faces
   according to the value of the variable `font-lock-keywords'.

This can take a while for large buffers."
  t)

(defvar font-lock-keywords nil "\
*The keywords to highlight.
If this is a list, then elements may be of the forms:

  \"string\"			; a regexp to highlight in the 
				;  `font-lock-keyword-face'.
  (\"string\" . integer)  	; match N of the regexp will be highlighted
  (\"string\" . face-name)	; use the named face
  (\"string\" integer face-name)  ; both of the above

These regular expressions should not match text which spans lines.  Multi-line
patterns will be correctly fontified when \\[font-lock-fontify-buffer] is used,
but will not be matched by the auto-fontification that font-lock-mode does,
since it looks at only one line at a time.

The more patterns there are in this list, the slower the initial fontification
of the buffer will be.")

(autoload 'calendar "calendar"
  "\
Display a three-month calendar in another window.
The three months appear side by side, with the current month in the middle
surrounded by the previous and next months.  The cursor is put on today's date.

This function is suitable for execution in a .emacs file; appropriate setting
of the variable view-diary-entries-initially will cause the diary entries for
the current date to be displayed in another window.  The value of the variable
number-of-diary-entries controls the number of days of diary entries displayed.

An optional prefix argument ARG causes the calendar displayed to be ARG
months in the future if ARG is positive or in the past if ARG is negative;
in this case the cursor goes on the first day of the month.

Once in the calendar window, future or past months can be moved into view.
Arbitrary months can be displayed, or the calendar can be scrolled forward
or backward.

The cursor can be moved forward or backward by one day, one week, one month,
or one year.  All of these commands take prefix arguments which, when negative,
cause movement in the opposite direction.  For convenience, the digit keys
and the minus sign are automatically prefixes.  The window is replotted as
necessary to display the desired date.

Diary entries can be marked on the calendar or displayed in another window.

Use describe-mode for details of the key bindings in the calendar window.

The Gregorian calendar is assumed.

After preparing the calendar window initially, the hooks
initial-calendar-window-hook are run.

The hooks today-visible-calendar-hook are run everytime the calendar window
gets shifted, if the current date is visible in the window.  If it is not
visible, the hooks today-invisible-calendar-hook are run.  Thus, for
example, setting today-visible-calendar-hook to 'calendar-star-date will
cause today's date to be replaced by asterisks to highlight it whenever it
is in the window."
  t)

(autoload 'diary "calendar"
  "\
Generate the diary window for the current date.
The number of days of diary entries is governed by number-of-diary-entries.
This function is suitable for execution in a .emacs file."
  t)

(autoload 'diff "diff"
  "\
Find and display the differences between OLD and NEW files.
Interactively you are prompted with the current buffer's file name for NEW
and what appears to be it's backup for OLD."
  t)

(defvar diff-switches nil "\
*A list of switches to pass to the diff program.")

(defvar kill-emacs-hook nil "\
A list of functions (of no args) for `kill-emacs' to call before emacs is
actually killed.")

(autoload 'holidays "calendar"
  "\
Display the holidays for last month, this month, and next month.
This function is suitable for execution in a .emacs file."
  t)

(autoload 'list-command-history "chistory"
  "\
List history of commands typed to minibuffer.
The number of commands listed is controlled by  list-command-history-max.
Calls value of  list-command-history-filter  (if non-nil) on each history
element to judge if that element should be excluded from the list.

The buffer is left in Command History mode."
  t)

(autoload 'command-history-mode "chistory"
  "\
Major mode for examining commands from  command-history.
The number of commands listed is controlled by  list-command-history-max.
The command history is filtered by  list-command-history-filter  if non-nil.

Like Emacs-Lisp Mode except that characters do not insert themselves and
Digits provide prefix arguments.  Tab does not indent.
\\{command-history-map}
Calls the value of  command-history-hook  if that is non-nil
The Command History listing is recomputed each time this mode is
invoked."
  t)

(autoload 'repeat-matching-complex-command "chistory"
  "\
Edit and re-evaluate complex command with name matching PATTERN.
Matching occurrences are displayed, most recent first, until you
select a form for evaluation.  If PATTERN is empty (or nil), every form
in the command history is offered.  The form is placed in the minibuffer
for editing and the result is evaluated."
  t)

(autoload 'common-lisp-indent-function "cl-indent"
  "\
")

(autoload 'compare-windows "compare-w"
  "\
Compare text in current window with text in next window.
Compares the text starting at point in each window,
moving over text in each one as far as they match."
  t)

(autoload 'compile "compile"
  "\
Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*.
You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it."
  t)

(autoload 'grep "compile"
  "\
Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to."
  t)

(autoload 'next-error "compile"
  "\
Visit next compilation error message and corresponding source code.
This operates on the output from the \\[compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.
A non-nil argument (prefix arg, if interactive)
means reparse the error message buffer and start at the first error."
  t)

(autoload 'previous-error "compile"
  "\
See \\[next-error]."
  t)

(autoload 'dabbrev-expand "dabbrev"
  "\
Expand previous word \"dynamically\".
Expands to the most recent, preceding word for which this is a prefix.
If no suitable preceding word is found, words following point are considered.

A positive prefix argument, N, says to take the Nth backward DISTINCT
possibility.  A negative argument says search forward.  The variable
dabbrev-backward-only may be used to limit the direction of search to
backward if set non-nil.

If the cursor has not moved from the end of the previous expansion and
no argument is given, replace the previously-made expansion
with the next possible expansion not yet tried."
  t)

(autoload 'debug "debug"
  "\
Enter debugger.  Returns if user says \"continue\".
Arguments are mainly for use when this is called
 from the internals of the evaluator.
You may call with no args, or you may
 pass nil as the first arg and any other args you like.
 In that case, the list of args after the first will 
 be printed into the backtrace buffer.")

(autoload 'cancel-debug-on-entry "debug"
  "\
Undo effect of debug-on-entry on FUNCTION."
  t)

(autoload 'debug-on-entry "debug"
  "\
Request FUNCTION to invoke debugger each time it is called.
If the user continues, FUNCTION's execution proceeds.
Works by modifying the definition of FUNCTION,
which must be written in Lisp, not predefined.
Use `cancel-debug-on-entry' to cancel the effect of this command.
Redefining FUNCTION also does that."
  t)

(autoload 'dired "dired"
  "\
\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Dired displays a list of files in DIRNAME.
You can move around in it with the usual commands.
You can flag files for deletion with C-d
and then delete them by typing `x'.
Type `h' after entering dired for more info."
  t)

(autoload 'dired-other-window "dired"
  "\
\"Edit\" directory DIRNAME.  Like \\[dired] but selects in another window."
  t)

(autoload 'dired-noselect "dired"
  "\
Like M-x dired but returns the dired buffer as value, does not select it.")

(autoload 'dissociated-press "dissociate"
  "\
Dissociate the text of the current buffer.
Output goes in buffer named *Dissociation*,
which is redisplayed each time text is added to it.
Every so often the user must say whether to continue.
If ARG is positive, require ARG chars of continuity.
If ARG is negative, require -ARG words of continuity.
Default is 2."
  t)

(autoload 'doctor "doctor"
  "\
Switch to *doctor* buffer and start giving psychotherapy."
  t)

(autoload 'disassemble "disass"
  "\
Print disassembled code for OBJECT on (optional) STREAM.
OBJECT can be a symbol defined as a function, or a function itself
(a lambda expression or a compiled-function object).
If OBJECT is not already compiled, we compile it, but do not
redefine OBJECT if it is a symbol."
  t)

(autoload 'electric-buffer-list "ebuff-menu"
  "\
Vaguely like ITS lunar select buffer;
combining typeoutoid buffer listing with menuoid buffer selection.

This pops up a buffer describing the set of emacs buffers.
If the very next character typed is a space then the buffer list
 window disappears.

Otherwise, one may move around in the buffer list window, marking
 buffers to be selected, saved or deleted.

To exit and select a new buffer, type Space when the cursor is on the
 appropriate line of the buffer-list window.

Other commands are much like those of buffer-menu-mode.

Calls value of  electric-buffer-menu-mode-hook  on entry if non-nil.

\\{electric-buffer-menu-mode-map}"
  t)


(autoload 'electric-command-history "echistory"
  "\
Major mode for examining and redoing commands from  command-history.
The number of command listed is controlled by  list-command-history-max.
The command history is filtered by  list-command-history-filter  if non-nil.
Combines typeout Command History list window with menu like selection
of an expression from the history for re-evaluation in the *original* buffer.

The history displayed is filtered by  list-command-history-filter  if non-nil.

This pops up a window with the Command History listing.  If the very
next character typed is Space, the listing is killed and the previous
window configuration is restored.  Otherwise, you can browse in the
Command History with  Return  moving down and  Delete  moving up, possibly
selecting an expression to be redone with Space or quitting with `Q'.

Like Emacs-Lisp Mode except that characters do not insert themselves and
Tab and linefeed do not indent.  Instead these commands are provided:
Space or !	edit then evaluate current line in history inside
		   the ORIGINAL buffer which invoked this mode.
		   The previous window configuration is restored
		   unless the invoked command changes it.
C-c C-c, C-], Q	Quit and restore previous window configuration.
LFD, RET	Move to the next line in the history.
DEL		Move to the previous line in the history.
?		Provides a complete list of commands.

Calls the value of  electric-command-history-hook  if that is non-nil
The Command History listing is recomputed each time this mode is invoked."
  t)

(autoload 'edt-emulation-on "edt"
  "\
Begin emulating DEC's EDT editor.
Certain keys are rebound; including nearly all keypad keys.
Use \\[edt-emulation-off] to undo all rebindings except the keypad keys.
Note that this function does not work if called directly from the .emacs file.
Instead, the .emacs file should do (setq term-setup-hook 'edt-emulation-on)
Then this function will be called at the time when it will work."
  t)

(autoload 'fortran-mode "fortran"
  "\
Major mode for editing fortran code.
Tab indents the current fortran line correctly. 
`do' statements must not share a common `continue'.

Type `;?' or `;\\[help-command]' to display a list of built-in abbrevs for Fortran keywords.

Variables controlling indentation style and extra features:

 comment-start
    Normally nil in Fortran mode.  If you want to use comments
    starting with `!', set this to the string \"!\".
 fortran-do-indent
    Extra indentation within do blocks.  (default 3)
 fortran-if-indent
    Extra indentation within if blocks.  (default 3)
 fortran-continuation-indent
    Extra indentation appled to continuation statements.  (default 5)
 fortran-comment-line-column
    Amount of indentation for text within full-line comments. (default 6)
 fortran-comment-indent-style
    nil    means don't change indentation of text in full-line comments,
    fixed  means indent that text at column fortran-comment-line-column
    relative  means indent at fortran-comment-line-column beyond the
 	      indentation for a line of code.
    Default value is fixed.
 fortran-comment-indent-char
    Character to be inserted instead of space for full-line comment
    indentation.  (default is a space)
 fortran-minimum-statement-indent
    Minimum indentation for fortran statements. (default 6)
 fortran-line-number-indent
    Maximum indentation for line numbers.  A line number will get
    less than this much indentation if necessary to avoid reaching
    column 5.  (default 1)
 fortran-check-all-num-for-matching-do
    Non-nil causes all numbered lines to be treated as possible 'continue'
    statements.  (default nil)
 fortran-continuation-char
    character to be inserted in column 5 of a continuation line.
    (default $)
 fortran-comment-region
    String inserted by \\[fortran-comment-region] at start of each line in 
    region.  (default \"c$$$\")
 fortran-electric-line-number
    Non-nil causes line number digits to be moved to the correct column 
    as typed.  (default t)
 fortran-startup-message
    Set to nil to inhibit message first time fortran-mode is used.

Turning on Fortran mode calls the value of the variable fortran-mode-hook 
with no args, if that value is non-nil.
\\{fortran-mode-map}"
  t)


(autoload 'gdb "gdb"
  "\
Run gdb on program FILE in buffer *gdb-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for GDB.  If you wish to change this, use
the GDB commands `cd DIR' and `directory'."
  t)

(autoload 'set-gosmacs-bindings "gosmacs"
  "\
Rebind some keys globally to make GNU Emacs resemble Gosling Emacs.
Use \\[set-gnu-bindings] to restore previous global bindings."
  t)

(autoload 'hanoi "hanoi"
  "\
Towers of Hanoi diversion.  Argument is number of rings."
  t)

(autoload 'Helper-help "helper"
  "\
Provide help for current mode."
  t)

(autoload 'Helper-describe-bindings "helper"
  "\
Describe local key bindings of current mode."
  t)

(autoload 'info "info"
  "\
Enter Info, the documentation browser."
  t)

(autoload 'Info-tagify "informat"
  "\
Create or update Info-file tag table in current buffer."
  t)

(autoload 'Info-validate "informat"
  "\
Check current buffer for validity as an Info file.
Check that every node pointer points to an existing node."
  t)

(autoload 'Info-split "informat"
  "\
Split an info file into an indirect file plus bounded-size subfiles.
Each subfile will be up to 50000 characters plus one node.

To use this command, first visit a large Info file that has a tag table.
The buffer is modified into a (small) indirect info file
which should be saved in place of the original visited file.

The subfiles are written in the same directory the original file is in,
with names generated by appending `-' and a number to the original file name.

The indirect file still functions as an Info file, but it contains
just the tag table and a directory of subfiles."
  t)

(autoload 'batch-info-validate "informat"
  "\
Runs  Info-validate  on the files remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke \"emacs -batch -f batch-info-validate $info/ ~/*.info\""
  nil)

(autoload 'ispell "ispell" "\
Run ispell over a buffer.  (Actually over the buffer's file.)
First the file is scanned for misspelled words, then ispell
enters a loop with the following commands for every misspelled word:

DIGIT	Near miss selector.  If the misspelled word is 'close' to
	some words in the dictionary, they are offered as near misses.
r	Replace.  Replace the word with a string you type.  Each word
	of your new string is also checked.
i	Insert.  Insert this word in your private dictonary (kept in
	$HOME/ispell.words)
a	Accept.  Accept this word for the rest of this editing session,
 	but don't put it in your private dictonary.
l	Lookup.  Look for a word in the dictionary by fast binary
	search, or search for a regular expression in the dictionary
	using grep.
SPACE	Accept the word this time, but complain if it is seen again.
q, C-G	Leave the command loop.  You can come back later with \\[ispell-next]."
	  t)

(autoload 'ispell-region "ispell" "\
Check the spelling for all of the words in the region."
	  t)

(autoload 'ispell-word "ispell" "\
Check the spelling of the word under the cursor.
See `ispell' for more documentation."
	  t)

(autoload 'ledit-mode "ledit"
  "\
Major mode for editing text and stuffing it to a Lisp job.
Like Lisp mode, plus these special commands:
  M-C-d	-- record defun at or after point
	   for later transmission to Lisp job.
  M-C-r -- record region for later transmission to Lisp job.
  C-x z -- transfer to Lisp job and transmit saved text.
  M-C-c -- transfer to Liszt (Lisp compiler) job
	   and transmit saved text.
\\{ledit-mode-map}
To make Lisp mode automatically change to Ledit mode,
do (setq lisp-mode-hook 'ledit-from-lisp-mode)"
  t)

(autoload 'ledit-from-lisp-mode "ledit" 
  "\
")

(autoload 'lpr-buffer "lpr"
  "\
Print buffer contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  t)

(autoload 'print-buffer "lpr"
  "\
Print buffer contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  t)

(autoload 'lpr-region "lpr"
  "\
Print region contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  t)

(autoload 'print-region "lpr"
  "\
Print region contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  t)

(autoload 'insert-kbd-macro "macros"
  "\
Insert in buffer the definition of kbd macro NAME, as Lisp code.
Second argument KEYS non-nil means also record the keys it is on.
 (This is the prefix argument, when calling interactively.)

This Lisp code will, when executed, define the kbd macro with the
same definition it has now.  If you say to record the keys,
the Lisp code will also rebind those keys to the macro.
Only global key bindings are recorded since executing this Lisp code
always makes global bindings.

To save a kbd macro, visit a file of Lisp code such as your ~/.emacs,
use this command, and then save the file."
  t)

(autoload 'kbd-macro-query "macros"
  "\
Query user during kbd macro execution.
With prefix argument, enters recursive edit,
 reading keyboard commands even within a kbd macro.
 You can give different commands each time the macro executes.
Without prefix argument, reads a character.  Your options are:
 Space -- execute the rest of the macro.
 DEL -- skip the rest of the macro; start next repetition.
 C-d -- skip rest of the macro and don't repeat it any more.
 C-r -- enter a recursive edit, then on exit ask again for a character
 C-l -- redisplay screen and ask again."
  t)

(autoload 'name-last-kbd-macro "macros"
  "\
Assign a name to the last keyboard macro defined.
One arg, a symbol, which is the name to define.
The symbol's function definition becomes the keyboard macro string.
Such a \"function\" cannot be called from Lisp, but it is a valid command
definition for the editor command loop."
  t)

(autoload 'make-command-summary "makesum"
  "\
Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first."
  t)

(autoload 'define-mail-alias "mail-abbrevs"
  "\
Define NAME as a mail-alias that translates to DEFINITION.
If DEFINITION contains multiple addresses, seperate them with commas."
  t)

(autoload 'manual-entry "man"
  "\
Display the Unix manual entry for TOPIC.
TOPIC is either the title of the entry, or has the form TITLE(SECTION)
where SECTION is the desired section of the manual, as in `tty(4)'."
  t)

(autoload 'mh-rmail "mh-e"
  "\
Inc(orporate) new mail (no arg) or scan a MH mail box (arg given).
This front end uses the MH mail system, which uses different conventions
from the usual mail system."
  t)

(autoload 'mh-smail "mh-e"
  "\
Send mail using the MH mail system."
  t)

(autoload 'convert-mocklisp-buffer "mlconvert"
  "\
Convert buffer of Mocklisp code to real Lisp that GNU Emacs can run."
  t)

(autoload 'modula-2-mode "modula2"
  "\
This is a mode intended to support program development in Modula-2.
All control constructs of Modula-2 can be reached by typing
Control-C followed by the first character of the construct.
\\{m2-mode-map}
  Control-c b begin         Control-c c case
  Control-c d definition    Control-c e else
  Control-c f for           Control-c h header
  Control-c i if            Control-c m module
  Control-c l loop          Control-c o or
  Control-c p procedure     Control-c Control-w with
  Control-c r record        Control-c s stdio
  Control-c t type          Control-c u until
  Control-c v var           Control-c w while
  Control-c x export        Control-c y import
  Control-c { begin-comment Control-c } end-comment
  Control-c Control-z suspend-emacs     Control-c Control-t toggle
  Control-c Control-c compile           Control-x ` next-error
  Control-c Control-l link

   m2-indent controls the number of spaces for each indentation.
   m2-compile-command holds the command to compile a Modula-2 program.
   m2-link-command holds the command to link a Modula-2 program."
  t)

(setq disabled-command-hook 'disabled-command-hook)

(autoload 'disabled-command-hook "novice"
  "\
")

(autoload 'enable-command "novice"
  "\
Allow COMMAND to be executed without special confirmation from now on.
The user's .emacs file is altered so that this will apply
to future sessions." t)

(autoload 'disable-command "novice"
  "\
Require special confirmation to execute COMMAND from now on.
The user's .emacs file is altered so that this will apply
to future sessions." t)

(autoload 'nroff-mode "nroff-mode"
  "\
Major mode for editing text intended for nroff to format.
\\{nroff-mode-map}
Turning on Nroff mode runs text-mode-hook, then nroff-mode-hook.
Also, try nroff-electric-mode, for automatically inserting
closing requests for requests that are used in matched pairs."
  t)

(autoload 'list-options "options"
  "\
Display a list of Emacs user options, with values and documentation."
  t)

(autoload 'edit-options "options"
  "\
Edit a list of Emacs user option values.
Selects a buffer containing such a list,
in which there are commands to set the option values.
Type \\[describe-mode] in that buffer for a list of commands."
  t)

(autoload 'outline-mode "outline"
  "\
Set major mode for editing outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines. 

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end 
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

Commands:
C-c C-n   outline-next-visible-heading      move by visible headings
C-c C-p   outline-previous-visible-heading
C-c C-f   outline-forward-same-level        similar but skip subheadings
C-c C-b   outline-backward-same-level
C-c C-u   outline-up-heading		    move from subheading to heading

Meta-x hide-body	make all text invisible (not headings).
Meta-x show-all		make everything in buffer visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
C-c C-h   hide-subtree	make body and subheadings invisible.
C-c C-s   show-subtree	make body and subheadings visible.
C-c C-i   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
M-x hide-entry	   make immediately following body invisible.
M-x show-entry	   make it visible.
M-x hide-leaves	   make body under heading and under its subheadings invisible.
		     The subheadings remain visible.
M-x show-branches  make all subheadings at all levels visible.

The variable outline-regexp can be changed to control what is a heading.
A line is a heading if outline-regexp matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on outline mode calls the value of text-mode-hook and then of
outline-mode-hook, if they are non-nil."
  t)

(autoload 'cvs-update "pcl-cvs"
	  "\
Run a 'cvs update' in the current working directory. Feed the
output to a *cvs* buffer and run cvs-mode on it.
If optional prefix argument LOCAL is non-nil, 'cvs update -l' is run."
	  t)

(autoload 'cvs-update-other-window "pcl-cvs"
	  "\
Run a 'cvs update' in the current working directory. Feed the
output to a *cvs* buffer, display it in the other window, and run
cvs-mode on it.

If optional prefix argument LOCAL is non-nil, 'cvs update -l' is run.")

(autoload 'edit-picture "picture"
  "\
Switch to Picture mode, in which a quarter-plane screen model is used.
Printing characters replace instead of inserting themselves with motion
afterwards settable by these commands:
  C-c <	  Move left after insertion.
  C-c >	  Move right after insertion.
  C-c ^	  Move up after insertion.
  C-c .	  Move down after insertion.
  C-c `	  Move northwest (nw) after insertion.
  C-c '	  Move northeast (ne) after insertion.
  C-c /	  Move southwest (sw) after insertion.
  C-c \\   Move southeast (se) after insertion.
The current direction is displayed in the mode line.  The initial
direction is right.  Whitespace is inserted and tabs are changed to
spaces when required by movement.  You can move around in the buffer
with these commands:
  C-p	  Move vertically to SAME column in previous line.
  C-n	  Move vertically to SAME column in next line.
  C-e	  Move to column following last non-whitespace character.
  C-f	  Move right inserting spaces if required.
  C-b	  Move left changing tabs to spaces if required.
  C-c C-f Move in direction of current picture motion.
  C-c C-b Move in opposite direction of current picture motion.
  Return  Move to beginning of next line.
You can edit tabular text with these commands:
  M-Tab	  Move to column beneath (or at) next interesting charecter.
	    `Indents' relative to a previous line.
  Tab	  Move to next stop in tab stop list.
  C-c Tab Set tab stops according to context of this line.
	    With ARG resets tab stops to default (global) value.
	    See also documentation of variable	picture-tab-chars
	    which defines \"interesting character\".  You can manually
	    change the tab stop list with command \\[edit-tab-stops].
You can manipulate text with these commands:
  C-d	  Clear (replace) ARG columns after point without moving.
  C-c C-d Delete char at point - the command normally assigned to C-d.
  Delete  Clear (replace) ARG columns before point, moving back over them.
  C-k	  Clear ARG lines, advancing over them.	 The cleared
	    text is saved in the kill ring.
  C-o	  Open blank line(s) beneath current line.
You can manipulate rectangles with these commands:
  C-c C-k Clear (or kill) a rectangle and save it.
  C-c C-w Like C-c C-k except rectangle is saved in named register.
  C-c C-y Overlay (or insert) currently saved rectangle at point.
  C-c C-x Like C-c C-y except rectangle is taken from named register.
  \\[copy-rectangle-to-register]   Copies a rectangle to a register.
  \\[advertised-undo]   Can undo effects of rectangle overlay commands
	    commands if invoked soon enough.
You can return to the previous mode with:
  C-c C-c Which also strips trailing whitespace from every line.
	    Stripping is suppressed by supplying an argument.

Entry to this mode calls the value of  edit-picture-hook  if non-nil.

Note that Picture mode commands will work outside of Picture mode, but
they are not defaultly assigned to keys."
  t)

(fset 'picture-mode 'edit-picture)

(autoload 'prolog-mode "prolog"
  "\
Major mode for editing Prolog code for Prologs.
Blank lines and `%%...' separate paragraphs.  `%'s start comments.
Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of prolog-mode-hook
if that value is non-nil."
  t)

(autoload 'run-prolog "prolog"
  "\
Run an inferior Prolog process, input and output via buffer *prolog*."
  t)


(autoload 'clear-rectangle "rect"
  "\
Blank out rectangle with corners at point and mark.
The text previously in the region is overwritten by the blanks."
  t)

(autoload 'delete-rectangle "rect"
  "\
Delete (don't save) text in rectangle with point and mark as corners.
The same range of columns is deleted in each line
starting with the line where the region begins
and ending with the line where the region ends."
  t)

(autoload 'delete-extract-rectangle "rect"
  "\
Return and delete contents of rectangle with corners at START and END.
Value is list of strings, one for each line of the rectangle.")

(autoload 'extract-rectangle "rect"
  "\
Return contents of rectangle with corners at START and END.
Value is list of strings, one for each line of the rectangle.")

(autoload 'insert-rectangle "rect"
  "\
Insert text of RECTANGLE with upper left corner at point.
RECTANGLE's first line is inserted at point,
its second line is inserted at a point vertically under point, etc.
RECTANGLE should be a list of strings.")

(autoload 'kill-rectangle "rect"
  "\
Delete rectangle with corners at point and mark; save as last killed one.
Calling from program, supply two args START and END, buffer positions.
But in programs you might prefer to use delete-extract-rectangle."
  t)

(autoload 'open-rectangle "rect"
  "\
Blank out rectangle with corners at point and mark, shifting text right.
The text previously in the region is not overwritten by the blanks,
but insted winds up to the right of the rectangle."
  t)

(autoload 'yank-rectangle "rect"
  "\
Yank the last killed rectangle with upper left corner at point."
  t)

(autoload 'rnews "rnews"
  "\
Read USENET news for groups for which you are a member and add or
delete groups.
You can reply to articles posted and send articles to any group.

Type \\[describe-mode] once reading news to get a list of rnews commands."
  t)

(autoload 'news-post-news "rnewspost"
  "\
Begin editing a new USENET news article to be posted.
Type \\[describe-mode] once editing the article to get a list of commands."
  t)
(fset 'sendnews 'news-post-news)
(fset 'postnews 'news-post-news)

(autoload 'rmail "rmail"
  "\
Read and edit incoming mail.
Moves messages into file named by  rmail-file-name  (a babyl format file)
 and edits that file in RMAIL Mode.
Type \\[describe-mode] once editing that file, for a list of RMAIL commands.

May be called with filename as argument;
then performs rmail editing on that file,
but does not copy any new mail into the file."
  t)

(autoload 'rmail-input "rmail"
  "\
Run RMAIL on file FILENAME."
  t)

(defconst rmail-dont-reply-to-names nil "\
*A regular expression specifying names to prune in replying to messages.
nil means don't reply to yourself.")

(defvar rmail-default-dont-reply-to-names "info-" "\
A regular expression specifying part of the value of the default value of
the variable `rmail-dont-reply-to-names', for when the user does not set
`rmail-dont-reply-to-names' explicitly.  (The other part of the default
value is the user's name.)
It is useful to set this variable in the site customisation file.")

(defconst rmail-primary-inbox-list  nil "\
*List of files which are inboxes for user's primary mail file ~/RMAIL.
`nil' means the default, which is (\"/usr/spool/mail/$USER\" \"~/mbox\")
\(the first name varies depending on the operating system,
and the value of the environment variable MAIL overrides it).")

(defconst rmail-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^received:\\|^[a-z-]*message-id:\\|^summary-line:\\|^errors-to:" "\
*Gubbish header fields one would rather not see.")

(defvar rmail-delete-after-output nil "\
*Non-nil means automatically delete a message that is copied to a file.")

;;; Others are in paths.el.


(autoload 'start-timer "timer"
  "\
Start a timer.
Args are NAME, FUNCTION, VALUE &optional RESTART.
NAME is an identifier for the timer.  It must be a string.  If a timer
  already exists with this name, NAME will be modified slightly to until
  it is unique.
FUNCTION should be a function (or symbol naming one) of no arguments.  It
  will be called each time the timer expires.  The function can access
  timer that invoked it through the variable `current-timer'.
VALUE is the number of seconds until this timer expires.
Optional fourth arg RESTART non-nil means that this timer should be
  restarted automatically after its function is called.  Normally a timer
  is deleted at expiration after its function has returned. 
  If non-nil RESTART should be a number indicating the value at which the
  timer should be set at restart time.
Returns the newly created timer."
  t)

(defvar auto-save-timeout 30 "\
*Number of seconds idle time before auto-save.
Zero or nil means disable auto-saving due to idleness.

The actual amount of idle time between auto-saves is logarithmically related
to the size of the current buffer.  This variable is the number of seconds
after which an auto-save will happen when the current buffer is 50k or less;
the timeout will be 2 1/4 times this in a 200k buffer, 3 3/4 times this in a
1000k buffer, and 4 1/2 times this in a 2000k buffer.

For this variable to have any effect, you must do (require 'timer).

See also the variable `auto-save-interval', which controls auto-saving based
on the number of characters typed.")


(autoload 'run-scheme "xscheme"
  "\
Run an inferior Scheme process.
Output goes to the buffer `*scheme*'.
With argument, asks for a command line."
  t)

(autoload 'scheme-mode "scheme"
  "\
Major mode for editing Scheme code.
Editing commands are similar to those of lisp-mode.

In addition, if an inferior Scheme process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
modeline of all Scheme buffers.  The names of commands that interact
with the Scheme process start with \"xscheme-\".  For more information
see the documentation for xscheme-interaction-mode.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{scheme-mode-map}
Entry to this mode calls the value of scheme-mode-hook
if that value is non-nil."
  t)

(autoload 'scribe-mode "scribe"
  "\
Major mode for editing files of Scribe (a text formatter) source.
Scribe-mode is similar text-mode, with a few extra commands added.
\\{scribe-mode-map}

Interesting variables:

scribe-fancy-paragraphs
  Non-nil makes Scribe mode use a different style of paragraph separation.

scribe-electric-quote
  Non-nil makes insert of double quote use `` or '' depending on context.

scribe-electric-parenthesis
  Non-nil makes an open-parenthesis char (one of `([<{')
  automatically insert its close if typed after an @Command form."
  t)

;; Useful to set in site-init.el
(defconst send-mail-function 'sendmail-send-it "\
Function to call to send the current buffer as mail.
The headers are delimited by a string found in mail-header-separator.")

(defconst mail-self-blind nil "\
*Non-nil means insert BCC to self in messages to be sent.
This is done when the message is initialized,
so you can remove or alter the BCC field to override the default.")

(defconst mail-interactive nil "\
*Non-nil means when sending a message wait for and display errors.
nil means let mailer mail back a message to report errors.")

(defconst mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^[a-z-]*message-id:\\|^summary-line:\\|^to:\\|^cc:\\|^subject:\\|^in-reply-to:\\|^return-path:" "\
Delete these headers from old message when it's inserted in a reply.")

(defconst mail-header-separator "--text follows this line--" "\
*Line used to separate headers from text in messages being composed.")

(defconst mail-archive-file-name nil "\
*Name of file to write all outgoing messages in, or nil for none.
Do not use an rmail file here!  Instead, use its inbox file.")

(defvar mail-signature-file "~/.signature" "\
File to be inserted at the end of a message. Usually, this file is called
\"~/.signature\".")

(defvar mail-aliases nil "\
Word-abbrev table of mail address aliases.
If this is nil, it means the aliases have not yet been initialized and
should be read from the .mailrc file.  (This is distinct from there being
no aliases, which is represented by this being a table with no entries.)")

(defvar mail-abbrev-mailrc-file nil "\
Name of file with mail aliases.   If nil, ~/.mailrc is used.")

(autoload 'mail-other-window "sendmail"
  "\
Like `mail' command, but display mail buffer in another window."
  t)

(autoload 'mail "sendmail"
  "\
Edit a message to be sent.  Argument means resume editing (don't erase).
Returns with message buffer selected; value t if message freshly initialized.
While editing message, type C-c C-c to send the message and exit.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields:
\\{mail-mode-map}

If mail-self-blind is non-nil, a BCC to yourself is inserted
when the message is initialized.

If mail-default-reply-to is non-nil, it should be an address (a string);
a Reply-to: field with that address is inserted.

If mail-archive-file-name is non-nil, an FCC field with that file name
is inserted.

If mail-setup-hook is bound, its value is called with no arguments
after the message is initialized.  It can add more default fields.

When calling from a program, the second through fifth arguments
 TO, SUBJECT, IN-REPLY-TO and CC specify if non-nil
 the initial contents of those header fields.
 These arguments should not have final newlines.
The sixth argument REPLYBUFFER is a buffer whose contents
 should be yanked if the user types C-c C-y."
  t)

(autoload 'mail-mode "sendmail"
  "\
Major mode for editing mail to be sent.
Like Text Mode but with these additional commands:
C-c C-s  mail-send (send the message)    C-c C-c  mail-send-and-exit
C-c C-f  move to a header field (and create it if there isn't):
	 C-c C-f C-t  move to To:	C-c C-f C-s  move to Subj:
	 C-c C-f C-b  move to BCC:	C-c C-f C-c  move to CC:
C-c C-t  move to message text.
C-c C-w  mail-signature (insert ~/.signature at end).
C-c C-y  mail-yank-original (insert current message, in Rmail).
C-c C-q  mail-fill-yanked-message (fill what was yanked)."
  t)

;; used in mail-utils
(defvar mail-use-rfc822 t "\
*If non-nil, use a full, hairy RFC822 parser on mail addresses.
Otherwise, (the default) use a smaller, somewhat faster and
often-correct parser.")

(autoload 'feedmail-send-it "feedmail" "\
Replacement for sendmail-send-it.  Do this to use it:
\(setq send-mail-function 'feedmail-send-it)")


(autoload 'server-start "server"
  "\
Allow this Emacs process to be a server for client processes.
This starts a server communications subprocess through which
client \"editors\" can send your editing commands to this Emacs job.
To use the server, set up the program `etc/emacsclient' in the
Emacs distribution as your standard \"editor\".

Prefix arg means just kill any existing server communications subprocess."
  t)

(autoload 'run-lisp "inf-lisp"
  "\
Run an inferior Lisp process, input and output via buffer *lisp*."
  t)

(autoload 'shell "shell"
  "\
Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
Program used comes from variable explicit-shell-file-name,
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file ~/.emacs_SHELLNAME exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in shell-mode, giving commands for sending input
and controlling the subjobs of the shell.  See shell-mode.
See also variable shell-prompt-pattern.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-arguments'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

Note that many people's .cshrc files unconditionally clear the prompt.
If yours does, you will probably want to change it."
  t)

;; these are in the minibuffer keymap used by `shell-command'
(autoload 'comint-dynamic-complete "comint"
  "\
Dynamically complete the filename at point.
This function is similar to comint-replace-by-expanded-filename, except
that it won't change parts of the filename already entered in the buffer; 
it just adds completion characters to the end of the filename."
  t)
(autoload 'comint-dynamic-list-completions "comint"
  "\
List in help buffer all possible completions of the filename at point."
  t)

(autoload 'sort-lines "sort"
  "\
Sort lines in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort)."
  t)

(autoload 'sort-paragraphs "sort"
  "\
Sort paragraphs in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort)."
  t)

(autoload 'sort-pages "sort"
  "\
Sort pages in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort)."
  t)

(autoload 'sort-numeric-fields "sort"
  "\
Sort lines in region numerically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
Specified field must contain a number in each line of the region.
With a negative arg, sorts by the -ARG'th field, in reverse order.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort."
  t)

(autoload 'sort-fields "sort"
  "\
Sort lines in region lexicographically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
With a negative arg, sorts by the -ARG'th field, in reverse order.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort."
  t)

(autoload 'sort-columns "sort"
  "\
Sort lines in region alphabetically by a certain range of columns.
For the purpose of this command, the region includes
the entire line that point is in and the entire line the mark is in.
The column positions of point and mark bound the range of columns to sort on.
A prefix argument means sort into reverse order.

Note that sort-columns uses the sort utility program and therefore
cannot work on text containing TAB characters.  Use M-x untabify
to convert tabs to spaces before sorting."
  t)

(autoload 'sort-regexp-fields "sort"
  "\
Sort the region lexicographically as specifed by RECORD-REGEXP and KEY.
RECORD-REGEXP specifies the textual units which should be sorted.
  For example, to sort lines RECORD-REGEXP would be \"^.*$\"
KEY specifies the part of each record (ie each match for RECORD-REGEXP)
  is to be used for sorting.
  If it is \"\\digit\" then the digit'th \"\\(...\\)\" match field from
  RECORD-REGEXP is used.
  If it is \"\\&\" then the whole record is used.
  Otherwise, it is a regular-expression for which to search within the record.
If a match for KEY is not found within a record then that record is ignored.

With a negative prefix arg sorts in reverse order.

For example: to sort lines in the region by the first word on each line
 starting with the letter \"f\",
 RECORD-REGEXP would be \"^.*$\" and KEY would be \"\\=\\<f\\w*\\>\""
  t)


(autoload 'spell-buffer "spell"
  "\
Check spelling of every word in the buffer.
For each incorrect word, you are asked for the correct spelling
and then put into a query-replace to fix some or all occurrences.
If you do not want to change a word, just give the same word
as its \"correct\" spelling; then the query replace is skipped."
  t)

(autoload 'spell-region "spell"
  "\
Like spell-buffer but applies only to region.
From program, applies from START to END."
  t)

(autoload 'spell-word "spell"
  "\
Check spelling of word at or before point.
If it is not correct, ask user for the correct spelling
and query-replace the entire buffer to substitute it."
  t)

(autoload 'spell-string "spell"
  "\
Check spelling of string supplied as argument."
  t)

(autoload 'untabify "tabify"
  "\
Convert all tabs in region to multiple spaces, preserving columns.
The variable tab-width controls the action."
  t)

(autoload 'tabify "tabify"
  "\
Convert multiple spaces in region to tabs when possible.
A group of spaces is partially replaced by tabs
when this can be done without changing the column they end at.
The variable tab-width controls the action."
  t)

(autoload 'find-tag "etags"
  "\
*Find tag whose name contains TAGNAME.
Selects the buffer that the tag is contained in and puts
 point at its definition.
If TAGNAME is a null string, the expression in the buffer
 around or before point is used as the tag name.
If second arg NEXT is non-nil (interactively, with prefix arg),
 searches for the next tag in the tag table that matches the
 tagname used in the previous find-tag.

Multiple active tags tables and completion are supported.

Variables of note:

  tag-table-alist		controls which tables apply to which buffers
  tags-file-name		a default tags table
  tags-build-completion-table   controls completion behavior
  buffer-tag-table		another way of specifying a buffer-local table
  make-tags-files-invisible	whether tags tables should be very hidden
  tag-mark-stack-max		how many tags-based hops to remember"
  t)

(autoload 'find-tag-other-window "etags"
  "\
*Find tag whose name contains TAGNAME.
Selects the buffer that the tag is contained in in another window
 and puts point at its definition.
If TAGNAME is a null string, the expression in the buffer
 around or before point is used as the tag name.
If second arg NEXT is non-nil (interactively, with prefix arg),
 searches for the next tag in the tag table that matches the
 tagname used in the previous find-tag.

Multiple active tags tables and completion are supported.

Variables of note:

  tag-table-alist		controls which tables apply to which buffers
  tags-file-name		a default tags table
  tags-build-completion-table   controls completion behavior
  buffer-tag-table		another way of specifying a buffer-local table
  make-tags-files-invisible	whether tags tables should be very hidden
  tag-mark-stack-max		how many tags-based hops to remember"
  t)

(autoload 'list-tags "etags"
  "\
Display list of tags in file FILE.
FILE should not contain a directory spec
unless it has one in the tag table."
  t)

(autoload 'next-file "etags"
  "\
Select next file among files in current tag table(s).
Non-nil argument (prefix arg, if interactive)
initializes to the beginning of the list of files in the (first) tag table."
  t)

(autoload 'tags-apropos "etags"
  "\
Display list of all tags in tag table REGEXP matches."
  t)

(autoload 'tags-loop-continue "etags"
  "\
Continue last \\[tags-search] or \\[tags-query-replace] command.
Used noninteractively with non-nil argument
to begin such a command.  See variable tags-loop-form."
  t)

(autoload 'tag-table-files "etags"
  "\
Returns a list of the files referenced by the named TAGS table.")

(autoload 'tags-query-replace "etags"
  "\
Query-replace-regexp FROM with TO through all files listed in tag table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (C-G or ESC), you can resume the query-replace
with the command \\[tags-loop-continue].

See documentation of variable tag-table-alist."
  t)

(autoload 'tags-search "etags"
  "\
Search through all files listed in tag table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

See documentation of variable tag-table-alist."
  t)

(autoload 'visit-tags-table "etags"
  "\
Tell tags commands to use tags table file FILE first.
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory.
This function is largely obsoleted by the variable tag-table-alist."
  t)

(autoload 'telnet "telnet"
  "\
Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer *HOST-telnet*.
Normally input is edited in Emacs and sent a line at a time.
See also `\\[rsh]'."
  t)

(autoload 'rsh "telnet"
  "\
Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer *HOST-rsh*.
Normally input is edited in Emacs and sent a line at a time.
See also `\\[telnet]'."
  t)

(autoload 'terminal-emulator "terminal"
  "\
Under a display-terminal emulator in BUFFER, run PROGRAM on arguments ARGS.
ARGS is a list of argument-strings.  Remaining arguments are WIDTH and HEIGHT.
BUFFER's contents are made an image of the display generated by that program,
and any input typed when BUFFER is the current Emacs buffer is sent to that
program an keyboard input.

Interactively, BUFFER defaults to \"*terminal*\" and PROGRAM and ARGS
are parsed from an input-string using your usual shell.
WIDTH and HEIGHT are determined from the size of the current window
-- WIDTH will be one less than the window's width, HEIGHT will be its height.

To switch buffers and leave the emulator, or to give commands
to the emulator itself (as opposed to the program running under it),
type Control-^.  The following character is an emulator command.
Type Control-^ twice to send it to the subprogram.
This escape character may be changed using the variable `terminal-escape-char'.

`Meta' characters may not currently be sent through the terminal emulator.

Here is a list of some of the variables which control the behaviour
of the emulator -- see their documentation for more information:
terminal-escape-char, terminal-scrolling, terminal-more-processing,
terminal-redisplay-interval.

This function calls the value of terminal-mode-hook if that exists
and is non-nil after the terminal buffer has been set up and the
subprocess started.

Presently with `termcap' only; if somebody sends us code to make this
work with `terminfo' we will try to use it."
  t)

(autoload 'latex-mode "tex-mode"
  "\
Major mode for editing files of input for LaTeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[TeX-region] to run LaTeX on the current region, plus the preamble
copied from the top of the file (containing \\documentstyle, etc.),
running LaTeX under a special subshell.  \\[TeX-buffer] does the whole buffer.
\\[TeX-print] prints the .dvi file made by either of these.

Use \\[validate-TeX-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{TeX-mode-map}

Mode variables:
TeX-directory
	Directory in which to create temporary files for TeX jobs
	run by \\[TeX-region] or \\[TeX-buffer].
TeX-dvi-print-command
	Command string used by \\[TeX-print] to print a .dvi file.
TeX-show-queue-command
	Command string used by \\[TeX-show-print-queue] to show the print
	queue that \\[TeX-print] put your job on.

Entering LaTeX mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of LaTeX-mode-hook."
  t)

(autoload 'plain-tex-mode "tex-mode"
  "\
Major mode for editing files of input for plain TeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[TeX-region] to run TeX on the current region, plus a \"header\"
copied from the top of the file (containing macro definitions, etc.),
running TeX under a special subshell.  \\[TeX-buffer] does the whole buffer.
\\[TeX-print] prints the .dvi file made by either of these.

Use \\[validate-TeX-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{TeX-mode-map}

Mode variables:
TeX-directory
	Directory in which to create temporary files for TeX jobs
	run by \\[TeX-region] or \\[TeX-buffer].
TeX-dvi-print-command
	Command string used by \\[TeX-print] to print a .dvi file.
TeX-show-queue-command
	Command string used by \\[TeX-show-print-queue] to show the print
	queue that \\[TeX-print] put your job on.

Entering plain-TeX mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of plain-TeX-mode-hook."
  t)

(autoload 'tex-mode "tex-mode"
  "\
Major mode for editing files of input for TeX or LaTeX.
Trys to intuit whether this file is for plain TeX or LaTeX and
calls plain-tex-mode or latex-mode.  If it cannot be determined
(e.g., there are no commands in the file), the value of
TeX-default-mode is used."
  t)

(fset 'TeX-mode 'tex-mode)
(fset 'plain-TeX-mode 'plain-tex-mode)
(fset 'LaTeX-mode 'latex-mode)

(autoload 'texinfo-mode "texinfo"
  "\
Major mode for editing texinfo files.
These are files that are input for TEX and also to be turned
into Info files by \\[texinfo-format-buffer].
These files must be written in a very restricted and
modified version of TEX input format.

As for editing commands, like text-mode except for syntax table,
which is set up so expression commands skip texinfo bracket groups."
  t)

(autoload 'texinfo-format-buffer "texinfmt"
  "\
Process the current buffer as texinfo code, into an Info file.
The Info file output is generated in a buffer
visiting the Info file names specified in the @setfilename command.

Non-nil argument (prefix, if interactive) means don't make tag table
and don't split the file if large.  You can use Info-tagify and
Info-split to do these manually."
  t)

(autoload 'texinfo-format-region "texinfmt"
  "\
Convert the the current region of the Texinfo file to Info format.
This lets you see what that part of the file will look like in Info.
The command is bound to \\[texinfo-format-region].  The text that is
converted to Info is stored in a temporary buffer."
  t)

(autoload 'batch-texinfo-format "texinfmt"
  "\
Runs  texinfo-format-buffer  on the files remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke
  \"emacs -batch -funcall batch-texinfo-format $docs/ ~/*.texinfo\"."
  nil)

(autoload 'display-time "time"
  "\
Display current time and load level in mode line of each buffer.
Updates automatically every minute.
If display-time-day-and-date is non-nil, the current day and date
are displayed as well."
  t)

(autoload 'underline-region "underline"
  "\
Underline all nonblank characters in the region.
Works by overstriking underscores.
Called from program, takes two arguments START and END
which specify the range to operate on."
  t)

(autoload 'ununderline-region "underline"
  "\
Remove all underlining (overstruck underscores) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on."
  t)

(autoload 'ask-user-about-lock "userlock"
  "\
Ask user what to do when he wants to edit FILE but it is locked by USER.
This function has a choice of three things to do:
  do (signal 'buffer-file-locked (list FILE USER))
    to refrain from editing the file
  return t (grab the lock on the file)
  return nil (edit the file even though it is locked).
You can rewrite it to use any criterion you like to choose which one to do."
  nil)

(autoload 'ask-user-about-supersession-threat "userlock"
  "\
Ask a user who is about to modify an obsolete buffer what to do.
This function has two choices: it can return, in which case the modification
of the buffer will proceed, or it can (signal 'file-supersession (file)),
in which case the proposed buffer modification will not be made.

You can rewrite this to use any criterion you like to choose which one to do.
The buffer in question is current when this function is called."
  nil)

(autoload 'vi-mode "vi"
  "\
Major mode that acts like the `vi' editor.
The purpose of this mode is to provide you the combined power of vi (namely,
the \"cross product\" effect of commands and repeat last changes) and Emacs.

This command redefines nearly all keys to look like vi commands.
It records the previous major mode, and any vi command for input
\(`i', `a', `s', etc.) switches back to that mode.
Thus, ordinary Emacs (in whatever major mode you had been using)
is \"input\" mode as far as vi is concerned.

To get back into vi from \"input\" mode, you must issue this command again.
Therefore, it is recommended that you assign it to a key.

Major differences between this mode and real vi :

* Limitations and unsupported features
  - Search patterns with line offset (e.g. /pat/+3 or /pat/z.) are
    not supported.
  - Ex commands are not implemented; try ':' to get some hints.
  - No line undo (i.e. the 'U' command), but multi-undo is a standard feature.

* Modifications
  - The stopping positions for some point motion commands (word boundary,
    pattern search) are slightly different from standard 'vi'.
    Also, no automatic wrap around at end of buffer for pattern searching.
  - Since changes are done in two steps (deletion then insertion), you need
    to undo twice to completely undo a change command.  But this is not needed
    for undoing a repeated change command.
  - No need to set/unset 'magic', to search for a string with regular expr
    in it just put a prefix arg for the search commands.  Replace cmds too.
  - ^R is bound to incremental backward search, so use ^L to redraw screen.

* Extensions
  - Some standard (or modified) Emacs commands were integrated, such as
    incremental search, query replace, transpose objects, and keyboard macros.
  - In command state, ^X links to the 'ctl-x-map', and ESC can be linked to
    esc-map or set undefined.  These can give you the full power of Emacs.
  - See vi-com-map for those keys that are extensions to standard vi, e.g.
    `vi-name-last-change-or-macro', `vi-verify-spelling', `vi-locate-def',
    `vi-mark-region', and 'vi-quote-words'.  Some of them are quite handy.
  - Use \\[vi-switch-mode] to switch among different modes quickly.
  
Syntax table and abbrevs while in vi mode remain as they were in Emacs."
  t)

(autoload 'view-file "view-less"
  "\
Find FILE, enter view mode.  With prefix arg use other window."
  t)

(autoload 'view-buffer "view-less"
  "\
Switch to BUF, enter view mode.  With prefix arg use other window."
  t)

(autoload 'view-mode "view-less"
  "\
Mode for viewing text, with bindings like `less'.")

(autoload 'vip-mode "vip"
  "\
Begin emulating the vi editor.  This is distinct from `vi-mode'.
This emulator has different capabilities from the `vi-mode' emulator.
See the text at the beginning of the source file .../lisp/vip.el
in the Emacs distribution."
  t)

(autoload 'evi "evi"
  "\
Start vi emulation in this buffer.  Of all the those available, this
emulator most closely resembles vi.
See the text at the beginning of the source file .../lisp/evi.el
in the Emacs distribution."
  t)

(autoload 'yow "yow"
  "\
Return or display a Zippy quotation" t)
(autoload 'psychoanalyze-pinhead "yow"
  "\
Zippy goes to the analyst." t)



(defun query-replace (from-string to-string &optional arg)
  "\
Replace some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

Preserves case in each replacement if  case-replace  and  case-fold-search
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries."
  (interactive "sQuery replace: \nsQuery replace %s with: \nP")
  (perform-replace from-string to-string t nil arg)
  (message "Done"))

(defun query-replace-regexp (regexp to-string &optional arg)
  "\
Replace some things after point matching REGEXP with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

Preserves case in each replacement if  case-replace  and  case-fold-search
are non-nil and REGEXP has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries.
In TO-STRING, \\& means insert what matched REGEXP,
and \\=\\<n> means insert what matched <n>th \\(...\\) in REGEXP."
  (interactive "sQuery replace regexp: \nsQuery replace regexp %s with: \nP")
  (perform-replace regexp to-string t t arg)
  (message "Done"))

(defun map-query-replace-regexp (regexp to-strings &optional arg)
  "\
Replace some matches for REGEXP with various strings, in rotation.
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

(defun replace-string (from-string to-string &optional delimited)
  "\
Replace occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (search-forward OLD-STRING nil t)
    (replace-match REPLACEMENT nil t))
which will run faster and will not set the mark or print anything."
  (interactive "sReplace string: \nsReplace string %s with: \nP")
  (perform-replace from-string to-string nil nil delimited)
  (message "Done"))

(defun replace-regexp (regexp to-string &optional delimited)
  "\
Replace things after point matching REGEXP with TO-STRING.
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
  (interactive "sReplace regexp: \nsReplace regexp %s with: \nP")
  (perform-replace regexp to-string nil t delimited)
  (message "Done"))

(autoload 'perform-replace "replace")

(autoload 'load-sound-file "sound"
  "\
Read in an audio-file and add it to the sound-alist."
  t)
(autoload 'load-default-sounds "sound"
  "\
Load and install some sound files as beep-types.
This only works if you're on display 0 of a Sun SparcStation."
  t)

(autoload 'vm "vm"
   "\
View Mail: an alternate mail reader for emacs.
Optional first arg FOLDER specifies the folder to visit.  It defaults
to the value of vm-primary-inbox.  The folder buffer is put into VM
mode, a major mode for reading mail.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder.

Visiting the primary inbox causes any contents of the system mailbox to
be moved and appended to the resulting buffer.

All the messages can be read by repeatedly pressing SPC.  Use `n'ext and
`p'revious to move about in the folder.  Messages are marked for
deletion with `d', and saved to another folder with `s'.  Quitting VM
with `q' expunges deleted messages and saves the buffered folder to
disk.

See the documentation for vm-mode for more information."
 t)

(autoload 'vm-mode "vm" 
  "\
View Mail: an alternate mail reader for emacs.

Commands:
   h - summarize folder contents
   j - discard cached information about the current message

   n - go to next message
   p - go to previous message
   N - like `n' but ignores skip-variable settings
   P - like `p' but ignores skip-variable settings
 M-n - go to next unread message
 M-p - go to previous unread message
 RET - go to numbered message (uses prefix arg or prompts in minibuffer)
 TAB - go to last message seen
 M-s - incremental search through the folder

   t - display hidden headers
 SPC - scroll forward a page (if at end of message, then display next message)
   b - scroll backward a page
   < - go to beginning of current message
   > - go to end of current message

   d - delete message, prefix arg deletes messages forward (flag as deleted)
 C-d - delete message, prefix arg deletes messages backward (flag as deleted)
   u - undelete
   k - flag for deletion all messages with same subject as the current message

   r - reply (only to the sender of the message)
   R - reply with included text for current message
 M-r - extract and resend bounced message
   f - followup (reply to all recipients of message)
   F - followup with included text from the current message
   z - forward the current message
   m - send a message
   B - resend the current message to another user.
   c - continue composing the most recent message you were composing

   @ - digestify and mail entire folder contents (the folder is not modified)
   * - burst a digest into individual messages, and append and assimilate these
       message into the current folder.

   G - group messages according to some criteria

   g - get any new mail that has arrived in the system mailbox
       (new mail is appended to the disk and buffer copies of the
       primary inbox.)
   v - visit another mail folder
   V - visit a virtual folder

   e - edit the current message

   s - save current message in a folder (appends if folder already exists)
   w - write current message to a file without its headers (appends if exists)
   S - save entire folder to disk, expunging deleted messages
   A - save unfiled messages to their vm-auto-folder-alist specified folders
   # - expunge deleted messages (without saving folder)
   q - quit VM, deleted messages are expunged, folder saved to disk
   x - exit VM with no change to the folder

 M N - use marks; the next vm command will affect only marked messages
       if it makes sense for the command to do so

       M m - mark the current message
       M u - unmark the current message
       M M - mark all messsages
       M U - unmark all messsages

 C-_ - undo, special undo that retracts the most recent
             changes in message attributes.  Expunges and saves
             cannot be undone.  C-x u is also bound to this
             command.

   L - reload your VM init file, ~/.vm

   ? - help

   ! - run a shell command
   | - run a shell command with the current message as input

 M-C - view conditions under which you may redistribute VM
 M-W - view the details of VM's lack of a warranty

Variables:
   vm-auto-center-summary
   vm-auto-folder-alist
   vm-auto-folder-case-fold-search
   vm-auto-next-message
   vm-berkeley-mail-compatibility
   vm-circular-folders
   vm-confirm-new-folders
   vm-confirm-quit
   vm-crash-box
   vm-delete-after-archiving
   vm-delete-after-bursting
   vm-delete-after-saving
   vm-delete-empty-folders
   vm-digest-center-preamble
   vm-digest-preamble-format
   vm-folder-directory
   vm-folder-read-only
   vm-follow-summary-cursor
   vm-forwarding-subject-format
   vm-gargle-uucp
   vm-group-by
   vm-highlighted-header-regexp
   vm-honor-page-delimiters
   vm-in-reply-to-format
   vm-included-text-attribution-format
   vm-included-text-prefix
   vm-inhibit-startup-message
   vm-invisible-header-regexp
   vm-keep-sent-messages
   vm-mail-window-percentage
   vm-mode-hooks
   vm-move-after-deleting
   vm-move-after-undeleting
   vm-mutable-windows
   vm-preview-lines
   vm-preview-read-messages
   vm-primary-inbox
   vm-retain-message-order
   vm-reply-ignored-addresses
   vm-reply-subject-prefix
   vm-rfc934-forwarding
   vm-search-using-regexps
   vm-skip-deleted-messages
   vm-skip-read-messages
   vm-spool-files
   vm-startup-with-summary
   vm-strip-reply-headers
   vm-summary-format
   vm-virtual-folder-alist
   vm-virtual-mirror
   vm-visible-headers
   vm-visit-when-saving"
 t)

(autoload 'vm-visit-folder "vm" 
  "\
Visit a mail file with View Mail, an alternate mail reader for emacs.
See the description of the `vm' and `vm-mode' functions.

VM will parse and present its messages to you in the usual way.

First arg FOLDER specifies the mail file to visit.  When this
command is called interactively the file name is read from the
minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder."
  t)
(autoload 'vm-mail "vm-reply" 
  "\
Send a mail message from within View Mail, or from without."
  t)

(autoload 'webster "webster"
   "\
Look up a word in Webster's 7th edition"
   t)
(autoload 'webster-spell "webster"
   "\
Search for an approximately-spelled word in Webster's 7th edition"
   t)
(autoload 'webster-endings "webster"
   "\
Search for a word in Webster's 7th edition that begins with a certain prefix"
   t)

(autoload 'postscript-mode "postscript"
  "\
Major mode for editing PostScript programs."
  t)

(autoload 'gnus "gnus"
   "\
Read network news."
   t)
(autoload 'gnus-post-news "gnuspost"
   "\
Post a netnews article."
   t)

(autoload 'tar-mode "tar-mode"
  "\
Major mode for editing `tar' (tape archive) files in a dired-like manner."
  nil)

(autoload 'conx "conx"
  "\
Generate some random sentences in the *conx* buffer.
Output will be continuously generated until you type ^G.
Before running this function, you must snarf some text into the CONX 
database with the M-x conx-buffer or M-x conx-region commands."
  t)
(autoload 'conx-load "conx"
  "\
Load a CONX database that has been previously saved with M-x conx-save."
  t)
(autoload 'conx-buffer "conx"
  "\
Absorb the text in the current buffer into the CONX database;
use the M-x conx command to generate random sentences based on the
word frequency in this buffer.  Multiple uses of this function are
additive; to clear the database and start over, use M-x conx-init."
  t)
(autoload 'conx-region "conx"
  "\
Absorb the text in the current buffer into the CONX database;
use the M-x conx command to generate random sentences based on the
word frequency in this region.  Multiple uses of this function are
additive; to clear the database and start over, use M-x conx-init."
  t)

(autoload 'with-timeout "with-timeout" "\
Usage: (with-timeout (seconds &rest timeout-forms) &rest body)
This is just like progn, but if the given number of seconds expires before
the body returns, then timeout-forms are evaluated and returned instead.
The body won't be interrupted in the middle of a computation: the check for 
the timer expiration only occurs when body does a redisplay, or prompts the
user for input, or calls accept-process-output."
	  nil t)
(autoload 'yes-or-no-p-with-timeout "with-timeout" "\
Just like yes-or-no-p, but will time out after TIMEOUT seconds
if the user has not yes answered, returning DEFAULT-VALUE.")
(autoload 'y-or-n-p-with-timeout "with-timeout" "\
Just like y-or-n-p, but will time out after TIMEOUT seconds
if the user has not yes answered, returning DEFAULT-VALUE.")

(autoload 'highlight-headers "highlight-headers" "\
Highlight message headers between start and end.
Faces used:
  message-headers			the part before the colon
  message-header-contents		the part after the colon
  message-highlighted-header-contents	contents of \"special\" headers
  message-cited-text			quoted text from other messages

Variables used:

  highlight-headers-regexp			what makes a \"special\" header
  highlight-headers-citation-regexp		matches lines of quoted text
  highlight-headers-citation-header-regexp	matches headers for quoted text

If HACK-SIG is true,then we search backward from END for something that
looks like the beginning of a signature block, and don't consider that a
part of the message (this is because signatures are often incorrectly
interpreted as cited text.)")

(autoload 'run-ilisp "ilisp" "\
Select a new inferior LISP." t)
(autoload 'clisp     "ilisp" "\
Run an inferior generic Common LISP." t)
(autoload 'allegro   "ilisp" "\
Run an inferior Allegro Common LISP." t)
(autoload 'lucid     "ilisp" "\
Run an inferior Lucid Common LISP." t)
(autoload 'cmulisp   "ilisp" "\
Run an inferior CMU Common LISP." t)
(autoload 'kcl       "ilisp" "\
Run an inferior Kyoto Common LISP." t)
(autoload 'scheme    "ilisp" "\
Run an inferior generic Scheme." t)
(autoload 'oaklisp   "ilisp" "\
Run an inferior Oaklisp Scheme." t)
