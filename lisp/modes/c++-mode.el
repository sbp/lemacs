;;; c++-mode.el --- major mode for editing C++ (and C) code

;; Author: 1992 Barry A. Warsaw, Century Computing Inc. <bwarsaw@cen.com>
;;         1987 Dave Detlefs and Stewart Clamen
;;         1985 Richard M. Stallman
;; Maintainer: c++-mode-help@anthem.nlm.nih.gov
;; Created: a long, long, time ago. adapted from the original c-mode.el
;; Version:         Revision: 2.302 
;; Last Modified:   Date: 1993/03/10 18:50:29 
;; Keywords: C++ C editing major-mode

;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.

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

;; Introduction
;; ============
;; Do a "C-h m" in a c++-mode buffer for more information on
;; customizing c++-mode. To submit bug reports hit "C-c C-b" in a
;; c++-mode buffer. This runs the command c++-submit-bug-report and
;; automatically sets up the mail buffer with all the necessary
;; information.  If you have other questions contact me at the
;; following address: c++-mode-help@anthem.nlm.nih.gov. Please don't
;; send bug reports to my personal account, I may not get it for a
;; long time.

;; Notes for Novice Users
;; ======================
;; c++-mode facilitates editing of C++ code by automatically handling
;; the indentation of lines of code in a manner very similar to c-mode
;; as distributed with GNU emacs. Refer to the GNU Emacs manual,
;; chapter 21 for more information on "Editing Programs".  In fact,
;; c++-mode (through its companion mode entry point c++-c-mode) can
;; also be used to edit both K&R and ANSI C code!
;;
;; To use c++-mode, add the following to your .emacs file. This
;; assumes you will use .cc or .C extensions for your C++ source:
;;
;; (autoload 'c++-mode   "c++-mode" "C++ Editing Mode" t)
;; (autoload 'c++-c-mode "c++-mode" "C Editing Mode" t)
;; (setq auto-mode-alist
;;   (append '(("\\.C$"  . c++-mode)
;;             ("\\.cc$" . c++-mode)
;;             ("\\.c$"  . c++-c-mode)   ; to edit C code
;;             ("\\.h$"  . c++-c-mode)   ; to edit C code
;;            ) auto-mode-alist))
;;
;; If you want to use the default c-mode for editing C code, then just
;; omit the lines marked "to edit C code".
;;
;; Finally, you may want to customize certain c++-mode variables.  The
;; best place to do this is in the mode hook variable called
;; c++-mode-hook.  Again, see the Emacs manual, chapter 21 for more
;; information.

;; Important Note about Escapes in Comments, and Performance
;; =========================================================

;; You may notice that certain characters, when typed in comment
;; regions, get escaped with a backslash.  This is a workaround for
;; bugs in emacs' syntax parsing algorithms. In brief, syntax parsing
;; in emacs 18 and derivatives is broken because syntax tables are not
;; rich enough to support more than 1 comment style per mode (as C++
;; requires).  The result is that emacs will sometimes choke on
;; unbalanced parentheses and single quotes in comments.  Please do a
;; "C-h v c++-untame-characters" for more information.
;;
;; This problem affect both the accuracy and performance of c++-mode
;; because some parsing must be performed in elisp instead of relying
;; on the C primitives. In general, I've chosen accuracy over
;; performance, but have worked hard to give moderately acceptable
;; speed in all but the most uncommon situations. You will most likely
;; notice c++-mode slowing when you're editing a file of preprocessor
;; commands, or inside long functions or class definitions.
;; Optimization is an ongoing concern, but the real solution is to fix
;; emacs.
;;
;; Patches to Lucid Emacs 19 have been submitted and may possibly show
;; up in a future release of that editor.  Some patches for GNU emacs
;; 18 have been released on the beta site, but they are not up-to-date
;; with the proposed Lemacs patches. I probably won't re-engineer the
;; v18 patches, but if you do, please send them to me so I can make
;; them available to other users via the beta site. If you have the
;; patches installed, c++-mode will automatically recognize this and
;; use the faster built-in primitives instead, and no characters will
;; be tamed (ie no backslashes in comment regions).

;; Beta Testers Mailing List
;; =========================
;; Want to be a c++-mode victim, er, beta-tester?  Send add/drop
;; requests to c++-mode-victims-request@anthem.nlm.nih.gov.
;; Discussions go to c++-mode-victims@anthem.nlm.nih.gov, but bug
;; reports and such should still be sent to c++-mode-help only.
;;
;; Many, many thanks go out to all the folks on the beta test list.
;; Without their patience, testing, insight, and code contribution,
;; c++-mode.el would be a far inferior package.

;; Getting c++-mode.el
;; ===================
;; The latest public release version of this file should always be
;; available for anon-ftp on ftp.cme.nist.gov:pub/gnu/c++-mode.el. It
;; will also most likely be available on the elisp archive machine:
;; archive.cis.ohio-state.edu.  Look around.
;; 
;; For those of you without aftp access, try sending a message to the
;; mail-server at library@cme.nist.gov.  Put this message in the body
;; of your email: "send pub/gnu/c++-mode.el" (without the quotes) to
;; get the file in uuencoded format, or send the message "help" to get
;; more information about using the mail-server.  Please don't email
;; me asking for the latest version, I may not have it readily
;; available to send to you. The mail-server should get it to you
;; pretty quickly.  Remember that if you want advanced access to beta
;; releases, get on the victims list -- but be forewarned, you should
;; be elisp and C++ fluent, and should have anon-ftp access.

;; LCD Archive Entry:
;; c++-mode|Barry A. Warsaw|c++-mode-help@anthem.nlm.nih.gov
;; |Mode for editing C++, and ANSI/K&R C code (was Detlefs' c++-mode.el)
;; |Date: 1993/03/10 18:50:29 |Revision: 2.302 |

;;; Code:

;; some people may not have c-mode loaded in by default. c++-mode.el
;; unfortunately still depends on distrib c-mode. c-mode doesn't
;; provide itself so this hack is best known way to ensure its loaded
(or (fboundp 'c-mode)
    (load "c-mode" nil t))


;; ======================================================================
;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defconst c++-emacs-is-fixed-p
  (= 8 (length (parse-partial-sexp (point) (point))))
  "True if you've patched your emacs to handle 2 orthogonal comment
styles in a single mode.")
(defconst c++-emacs-is-really-fixed-p
  (fboundp 'backward-syntactic-ws)
  "True if you've patched emacs to add the really fast back-parser.")

(defvar c++-mode-abbrev-table nil
  "Abbrev table in use in C++-mode buffers.")
(define-abbrev-table 'c++-mode-abbrev-table ())

(defvar c++-mode-map ()
  "Keymap used in C++ mode.")
(if c++-mode-map
    ()
  (setq c++-mode-map (make-sparse-keymap))
  (define-key c++-mode-map "\C-j"      'reindent-then-newline-and-indent)
  (define-key c++-mode-map "{"         'c++-electric-brace)
  (define-key c++-mode-map "}"         'c++-electric-brace)
  (define-key c++-mode-map ";"         'c++-electric-semi)
  (define-key c++-mode-map "#"         'c++-electric-pound)
  (define-key c++-mode-map "\e\C-h"    'mark-c-function)
  (define-key c++-mode-map "\e\C-q"    'c++-indent-exp)
  (define-key c++-mode-map "\t"        'c++-indent-command)
  (define-key c++-mode-map "\C-c\C-i"  'c++-insert-header)
  (define-key c++-mode-map "\C-c\C-\\" 'c++-macroize-region)
  (define-key c++-mode-map "\C-c\C-c"  'c++-comment-region)
  (define-key c++-mode-map "\C-c\C-u"  'c++-uncomment-region)
  (define-key c++-mode-map "\C-c\C-x"  'c++-match-paren)
  (define-key c++-mode-map "\e\C-a"    'c++-beginning-of-defun)
  (define-key c++-mode-map "\e\C-e"    'c++-end-of-defun)
  (define-key c++-mode-map "\e\C-x"    'c++-indent-defun)
  (define-key c++-mode-map "/"         'c++-electric-slash)
  (define-key c++-mode-map "*"         'c++-electric-star)
  (define-key c++-mode-map ":"         'c++-electric-colon)
  (define-key c++-mode-map "\177"      'c++-electric-delete)
  (define-key c++-mode-map "\C-c\C-t"  'c++-toggle-auto-hungry-state)
  (define-key c++-mode-map "\C-c\C-h"  'c++-toggle-hungry-state)
  (define-key c++-mode-map "\C-c\C-a"  'c++-toggle-auto-state)
  (if c++-emacs-is-fixed-p nil
    (define-key c++-mode-map "\C-c'"     'c++-tame-comments)
    (define-key c++-mode-map "'"         'c++-tame-insert)
    (define-key c++-mode-map "["         'c++-tame-insert)
    (define-key c++-mode-map "]"         'c++-tame-insert)
    (define-key c++-mode-map "("         'c++-tame-insert)
    (define-key c++-mode-map ")"         'c++-tame-insert))
  (define-key c++-mode-map "\C-c\C-b"  'c++-submit-bug-report)
  (define-key c++-mode-map "\C-c\C-v"  'c++-version)
  ;; these are necessary because default forward-sexp and
  ;; backward-sexp don't automatically let-bind
  ;; parse-sexp-ignore-comments, which is needed for them to work
  ;; properly in a C++ buffer.
  (define-key c++-mode-map "\e\C-f"    'c++-forward-sexp)
  (define-key c++-mode-map "\e\C-b"    'c++-backward-sexp)
  )

(defvar c++-mode-syntax-table nil
  "Syntax table used in c++-mode buffers.")
(defvar c++-c-mode-syntax-table nil
  "Syntax table used in c++-c-mode buffers.")

(if c++-mode-syntax-table
    ()
  (setq c++-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\"    c++-mode-syntax-table)
  (modify-syntax-entry ?+  "."     c++-mode-syntax-table)
  (modify-syntax-entry ?-  "."     c++-mode-syntax-table)
  (modify-syntax-entry ?=  "."     c++-mode-syntax-table)
  (modify-syntax-entry ?%  "."     c++-mode-syntax-table)
  (modify-syntax-entry ?<  "."     c++-mode-syntax-table)
  (modify-syntax-entry ?>  "."     c++-mode-syntax-table)
  (modify-syntax-entry ?&  "."     c++-mode-syntax-table)
  (modify-syntax-entry ?|  "."     c++-mode-syntax-table)
  (modify-syntax-entry ?\' "\""    c++-mode-syntax-table)
  ;; comment syntax
  (if c++-emacs-is-really-fixed-p
      ;; these entries will only work with the latest patches to lemacs
      (progn
	(modify-syntax-entry ?/  ". 1456" c++-mode-syntax-table)
	(modify-syntax-entry ?*  ". 23"   c++-mode-syntax-table)
	(modify-syntax-entry ?\n "> b"    c++-mode-syntax-table)
	)
    ;; though its not optimal, these will work for older, broken
    ;; emacses. some strange behavior may be encountered. PATCH YOUR EMACS!
    (modify-syntax-entry ?/  ". 124" c++-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23b" c++-mode-syntax-table)
    (modify-syntax-entry ?\n ">"     c++-mode-syntax-table)
    ))

(if c++-c-mode-syntax-table
    ()
  (setq c++-c-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\"    c++-c-mode-syntax-table)
  (modify-syntax-entry ?+  "."     c++-c-mode-syntax-table)
  (modify-syntax-entry ?-  "."     c++-c-mode-syntax-table)
  (modify-syntax-entry ?=  "."     c++-c-mode-syntax-table)
  (modify-syntax-entry ?%  "."     c++-c-mode-syntax-table)
  (modify-syntax-entry ?<  "."     c++-c-mode-syntax-table)
  (modify-syntax-entry ?>  "."     c++-c-mode-syntax-table)
  (modify-syntax-entry ?&  "."     c++-c-mode-syntax-table)
  (modify-syntax-entry ?|  "."     c++-c-mode-syntax-table)
  (modify-syntax-entry ?\' "\""    c++-c-mode-syntax-table)
  ;; comment syntax
  (if c++-emacs-is-really-fixed-p
      ;; these entries will only work with the latest patches to lemacs
      (progn
	(modify-syntax-entry ?\n "> b"  c++-c-mode-syntax-table)
	(modify-syntax-entry ?/  ". 14" c++-c-mode-syntax-table)
	(modify-syntax-entry ?*  ". 23" c++-c-mode-syntax-table)
	)
    ;; though its not optimal, these will work for older, broken
    ;; emacses. some strange behavior may be encountered. PATCH YOUR EMACS!
    (modify-syntax-entry ?/  ". 14"  c++-c-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"  c++-c-mode-syntax-table)
    ))

(defvar c++-tab-always-indent
  (if (boundp 'c-tab-always-indent) c-tab-always-indent t)
  "*Controls the operation of the TAB key.
If t (the default), always just indent the current line.  If nil,
indent the current line only if point is at the left margin or in the
line's indentation; otherwise insert a tab.  If not-nil-or-t, then tab
is inserted only within literals (comments and strings) and inside
preprocessor directives, but line is always reindented.")
(defvar c++-always-arglist-indent-p nil
  "*Control indentation of continued arglists.
When non-nil, arglists continued on subsequent lines will always
indent c++-empty-arglist-indent spaces, otherwise, they will indent to
just under previous line's argument indentation.")
(defvar c++-block-close-brace-offset 0
  "*Extra indentation given to close braces which close a block. This
does not affect braces which close a top-level construct (e.g. function).")
(defvar c++-paren-as-block-close-p nil
  "*Treat a parenthesis which is the first non-whitespace on a line as
a paren which closes a block.  When non-nil, c-indent-level is
subtracted, and c++-block-close-brace-offset is added to the line's
offset.")
(defvar c++-continued-member-init-offset nil
  "*Extra indent for continuation lines of member inits; nil means to align
with previous initializations rather than with the colon on the first line.")
(defvar c++-member-init-indent 0
  "*Indentation level of member initializations in function declarations.")
(defvar c++-friend-offset -4
  "*Offset of C++ friend class declarations relative to member declarations.")
(defvar c++-access-specifier-offset c-label-offset
  "*Extra indentation given to public, protected, and private labels.")
(defvar c++-empty-arglist-indent nil
  "*Indicates how far to indent a line following an empty argument list.
Nil means indent to just after the paren.")
(defvar c++-comment-only-line-offset 0
  "*Indentation offset for line which contains only C or C++ style comments.
This variable can take either a single integer or a list of integers.
If a single integer this is the extra indentation offset to apply to
all comment-only lines, except those which start in column zero. If a
list is used, the first integer is for all non-column-zero
comment-only lines and the second integer is for all column-zero
lines. You can also use a list containing only 1 integer, in which
case, this value is used for all comment-only lines.  For example:

value     meaning
=====     =======
  0       comment-only lines do not indent
  4       non-col0 lines indent 4 spaces, col0 lines don't indent
'(4)      all comment-only lines indent 4 spaces
'(4 1)    non-col0 lines indent 4 spaces, col0 lines indent 1 space")

(defvar c++-C-block-comments-indent-p nil
  "*4 styles of C block comments are supported. If this variable is nil,
then styles 1-3 are supported. If this variable is non-nil, style 4 is
supported.
style 1:       style 2:       style 3:       style 4:
/*             /*             /*             /*
   blah         * blah        ** blah        blah
   blah         * blah        ** blah        blah
   */           */            */             */
")
(defvar c++-cleanup-list nil
  "*List of various C++ constructs to \"clean up\".
These cleanups only take place when auto-newline minor mode is on.
Current legal values are:
   brace-else-brace   -- clean up \"} else {\" constructs by placing entire
                         construct on a single line.  This cleanup only
                         takes place when there is nothing but white
                         space between the braces and the else.  
   empty-defun-braces -- cleans up empty C++ function braces by
                         placing them on the same line.
   defun-close-semi   -- cleans up the terminating semi-colon on class
                         definitions and functions by placing the semi
                         on the same line as the closing brace.")
(defvar c++-hanging-braces t
  "*Controls the insertion of newlines before open (left) braces.
This variable only has effect when auto-newline is on.  If nil, open
braces do not hang (i.e. a newline is inserted before all open
braces).  If t, all open braces hang -- no newline is inserted before
open braces.  If not nil or t, newlines are only inserted before
top-level open braces; all other braces hang.")
(defvar c++-hanging-member-init-colon 'before
  "*Defines how colons which introduce member initializations are formatted.
Legal values are:
     t       -- no newlines inserted before or after colon
     nil     -- newlines inserted before and after colon
     'after  -- newlines inserted only after colon
     'before -- newlines inserted only before colon")
(defvar c++-auto-hungry-initial-state 'none
  "*Initial state of auto/hungry mode when buffer is first visited.
Legal values are:
     'none         -- no auto-newline and no hungry-delete-key.
     'auto-only    -- auto-newline, but no hungry-delete-key.
     'hungry-only  -- no auto-newline, but hungry-delete-key.
     'auto-hungry  -- both auto-newline and hungry-delete-key enabled.
Nil is synonymous for 'none and t is synonymous for 'auto-hungry.")

(defvar c++-auto-hungry-toggle t
  "*Enable/disable toggling of auto/hungry states.
Legal values are:
     'none         -- auto-newline and hungry-delete-key cannot be enabled.
     'auto-only    -- only auto-newline state can be toggled.
     'hungry-only  -- only hungry-delete-key state can be toggled.
     'auto-hungry  -- both auto-newline and hungry-delete-key can be toggled.
Nil is synonymous for 'none and t is synonymous for 'auto-hungry.")

(defvar c++-mailer 'mail
  "*Mail package to use to generate bug report mail buffer.")
(defconst c++-mode-help-address "c++-mode-help@anthem.nlm.nih.gov"
  "Address accepting submission of bug reports.")

(defvar c++-relative-offset-p t
  "*Control the calculation for indentation.
When non-nil (the default), indentation is calculated relative to the
first statement in the block.  When nil, the indentation is calculated
without regard to how the first statement is indented.")

(defvar c++-untame-characters (and (not c++-emacs-is-fixed-p) '(?\'))
  "*Utilize a backslashing workaround of an emacs syntax parsing bug.
If non-nil, this variable should contain a list of characters which
will be prepended by a backslash in comment regions.  By default, the
list contains only the most troublesome character, the single quote.
To be completely safe, set this variable to:

    '(?\( ?\) ?\' ?\{ ?\} ?\[ ?\])

This is the full list of characters which can potentially cause
problems if they exist unbalanced within comments. Setting this
variable to nil will defeat this feature, but be forewarned!  Such
un-escaped characters in comment regions can potentially break many
things such as some indenting and blinking of parenthesis.

Note further that only the default set of characters will be escaped
automatically as they are typed. But, executing c++-tame-comments
(\\[c++-tame-comments]) will escape all characters which are members
of this set, and which are found in comments throughout the file.

Finally, c++-mode can tell if you're running a patched emacs. If so,
taming characters isn't necessary and this variable is automatically
set to nil.")

(defvar c++-default-macroize-column 78
  "*Column to insert backslashes.")
(defvar c++-special-indent-hook nil
  "*Hook for user defined special indentation adjustments.
This hook gets called after a line is indented by the mode. By
supplying a hook, you can make adjustments to the line's standard
indentation.  If you do use this hook, you will likely need to also
set c++-relative-offset-p to nil.  The call to this hook is wrapped in
a save-excursion so you don't need to worry about restoring point and
mark inside the hook function.")
(defvar c++-delete-function 'backward-delete-char-untabify
  "*Function called by c++-electric-delete when deleting a single char.")
(defvar c++-electric-pound-behavior nil
  "*List of behaviors for electric pound insertion.
Only currently supported behavior is '(alignleft).")
(defvar c++-backscan-limit 2000
  "*Limit in characters for looking back while skipping syntactic ws.
If you typically write really big methods, and start noticing
incorrect indentations, try cranking this value up.  The larger this
value is, though, the slower parts of c++-mode can become. Setting
this variable to nil defeats backscan limits.")

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT
;; 
(defvar c++-hungry-delete-key nil
  "Internal state of hungry delete key.")
(defvar c++-auto-newline nil
  "Internal state of auto newline feature.")

(make-variable-buffer-local 'c++-auto-newline)
(make-variable-buffer-local 'c++-hungry-delete-key)

(defconst c++-access-key "\\<\\(public\\|protected\\|private\\)\\>:"
  "Regexp which describes access specification keywords.")
(defconst c++-class-key
  (concat
   "\\(extern\\s +\\)?"
   "\\(template\\s *<[^>]*>\\s *\\)?"
   "\\<\\(class\\|struct\\|union\\)\\>")
  "Regexp which describes a class declaration, including templates.")
(defconst c++-inher-key
  (concat "\\(\\<static\\>\\s +\\)?"
	  c++-class-key
	  "[ \t]+\\(\\(\\w\\|_\\)+[ \t]*:[ \t]*\\)?")
  "Regexp which describes a class inheritance declaration.")


;; ======================================================================
;; c++-mode main entry point
;; ======================================================================
(defun c++-mode ()
  "Major mode for editing C++ code.  Revision: 2.302 
To submit a bug report, enter \"\\[c++-submit-bug-report]\"
from a c++-mode buffer.

1. Very much like editing C code,
2. Expression and list commands understand all C++ brackets,
3. Tab at left margin indents for C++ code,
4. Both C++ and C style block comments are recognized,
5. Paragraphs are separated by blank lines only,
6. Hungry delete and auto newline behaviors are optional.

IMPORTANT NOTE: You may notice that some characters (by default, only
single quote) will get escaped with a backslash when typed in a
comment region.  This is a necessary workaround of a bug present in
GNU emacs 18 and derivatives.  Enter \"\\[describe-variable] c++-untame-characters RET\"
for more information. If you are running a patched emacs, no
characters will be escaped in comment regions, and many functions will
run much faster.

Key bindings:
\\{c++-mode-map}

These variables control indentation style. Those with names like
c-<thing> are inherited from c-mode.  Those with names like
c++-<thing> are unique for this mode, or have extended functionality
from their c-mode cousins.

 c-argdecl-indent
    Indentation level of declarations of C function arguments.
 c-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 c-brace-offset
    Extra indentation for line if it starts with an open brace.
 c-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to c-continued-statement-offset.
 c-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 c-indent-level
    Indentation of C statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 c-label-offset
    Extra indentation for line that is a label, or case or ``default:''

 c++-C-block-comments-indent-p
    Style of C block comments to support.
 c++-access-specifier-offset
    Extra indentation given to public, protected, and private keyword lines.
 c++-always-arglist-indent-p
    Control indentation of continued arglists. When non-nil, arglists
    continued on subsequent lines will always indent
    c++-empty-arglist-indent spaces, otherwise, they will indent to
    just under previous line's argument indentation.
 c++-auto-hungry-initial-state
    Initial state of auto/hungry mode when a C++ buffer is first visited.
 c++-auto-hungry-toggle
    Enable/disable toggling of auto/hungry states.
 c++-backscan-limit
    Limit in characters for looking back while skipping syntactic
    whitespace. This variable is only used in an un-patched emacs to
    help improve performance at the expense of some accuracy. Patched
    emacses are both fast and accurate.
 c++-block-close-brace-offset
    Extra indentation give to braces which close a block. This does
    not affect braces which close top-level constructs (e.g. functions).
 c++-cleanup-list
    A list of construct \"clean ups\" which c++-mode will perform when
    auto-newline mode is on.  Current legal values are:
    brace-else-brace, empty-defun-braces, defun-close-semi.
 c++-comment-only-line-offset
    Extra indentation for a line containing only a C or C++ style
    comment. Can be an integer or list, specifying the various styles
    of comment-only line special indentations.
 c++-continued-member-init-offset
    Extra indentation for continuation lines of member initializations; nil
    means to align with previous initializations rather than with the colon.
 c++-default-macroize-column
    Column to insert backslashes when macroizing a region.
 c++-delete-function
    Function called by c++-electric-delete when deleting a single char.
 c++-electric-pound-behavior
    List of behaviors for electric pound insertion.
 c++-empty-arglist-indent
    Extra indentation to apply to a line following an empty argument
    list. nil means to line it up with the left paren.
 c++-friend-offset
    Offset of C++ friend class declarations relative to member declarations.
 c++-hanging-braces
    Controls open brace hanging behavior when using auto-newline. nil
    says no braces hang, t says all open braces hang. non-nil-or-t
    means top-level open braces don't hang, all others do.
 c++-hanging-member-init-colon
    Defines how colons which introduce member initialization lists are
    formatted. t means no newlines are inserted either before or after
    the colon. nil means newlines are inserted both before and after
    the colon.  'before inserts newlines only before the colon, and
    'after inserts newlines only after colon.
 c++-mailer
    Mailer to use when sending bug reports.
 c++-member-init-indent
    Indentation level of member initializations in function declarations,
    if they are on a separate line beginning with a colon.
 c++-paren-as-block-close-p
    If non-nil, treat a parenthesis which is the first non-whitespace
    on a line as a paren whcih closes a block (i.e. treat it similar
    to right curly brace)
 c++-relative-offset-p
    Control the calculation for indentation. When non-nil (the
    default), indentation is calculated relative to the first
    statement in the block.  When nil, the indentation is calculated
    without regard to how the first statement is indented. Useful when
    using a c++-special-indent-hook.
 c++-special-indent-hook
    Hook for user defined special indentation adjustments. You can use
    this hook, which gets called after a line is indented by the mode,
    to customize indentations of the line.
 c++-tab-always-indent
    Controls the operation of the TAB key.  t means always just indent
    the current line.  nil means indent the current line only if point
    is at the left margin or in the line's indentation; otherwise
    insert a tab. If not-nil-or-t, then tab is inserted only within
    literals (comments and strings) and inside preprocessor
    directives, but the line is always reindented. Default is value
    for c-tab-always-indent.
 c++-untame-characters
    When non-nil, inserts backslash escapes before certain untamed
    characters in comment regions. It is recommended that you keep the
    default setting to workaround a nasty emacs bug, unless you are
    running a patched emacs.

Auto-newlining is no longer an all or nothing proposition. To be
specific I don't believe it is possible to implement a perfect
auto-newline algorithm. Sometimes you want it and sometimes you don't.
So now auto-newline (and its companion, hungry-delete) can be toggled
on and off on the fly.  Hungry-delete is the optional behavior of the
delete key so that, when enabled, hitting the delete key once consumes
all preceeding whitespace, unless point is within a literal (defined
as a C or C++ comment, or string).  Inside literals, and with
hungry-delete disabled, the delete key just calls the function in
c++-delete-function.

Behavior is controlled by c++-auto-hungry-initial-state and
c++-auto-hungry-toggle.  Legal values for both variables are:

   'none (or nil)      -- no auto-newline or hungry-delete.
   'auto-only          -- function affects only auto-newline state.
   'hungry-only        -- function affects only hungry-delete state.
   'auto-hungry (or t) -- function affects both states.

Thus if c++-auto-hungry-initial-state is 'hungry-only, then only
hungry state is turned on when the buffer is first visited.  If
c++-auto-hungry-toggle is 'auto-hungry, and both auto-newline and
hungry-delete state are on, then hitting \"\\[c++-toggle-auto-hungry-state]\"
will toggle both states.  Hitting \"\\[c++-toggle-hungry-state]\" will
always toggle hungry-delete state and hitting \"\\[c++-toggle-auto-state]\"
will always toggle auto-newline state, regardless of the value of
c++-auto-hungry-toggle.

Settings for K&R, BSD, and Stroustrup indentation styles are
  c-indent-level                5    8    4
  c-continued-statement-offset  5    8    4
  c-continued-brace-offset                0
  c-brace-offset               -5   -8    0
  c-brace-imaginary-offset                0
  c-argdecl-indent              0    8    4
  c-label-offset               -5   -8   -4
  c++-access-specifier-offset  -5   -8   -4
  c++-empty-arglist-indent                4
  c++-friend-offset                       0

Turning on C++ mode calls the value of the variable c++-mode-hook with
no args, if that value is non-nil.

Report bugs by entering \"\\[c++-submit-bug-report]\". This
automatically sets up a mail buffer with version information already
added. You just need to add a description of the problem and send the
message."
  (interactive)
  (kill-all-local-variables)
  (use-local-map c++-mode-map)
  (set-syntax-table c++-mode-syntax-table)
  (setq major-mode 'c++-mode
	mode-name "C++"
	local-abbrev-table c++-mode-abbrev-table)
  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  ;; 
  (set (make-local-variable 'indent-line-function) 'c++-indent-line)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|// *")
  (set (make-local-variable 'comment-indent-hook) 'c++-comment-indent)
  ;; hack auto-hungry designators into mode-line-format
  (if (listp mode-line-format)
      (setq mode-line-format
	    (let ((modeline nil))
	      (mapcar
	       (function
		(lambda (element)
		  (setq modeline
			(append modeline
				(if (eq element 'mode-name)
				    '(mode-name (c++-hungry-delete-key
						 (c++-auto-newline "/ah" "/h")
						 (c++-auto-newline "/a")))
				  (list element))))))
	       mode-line-format)
	      modeline)))
  (run-hooks 'c++-mode-hook)
  (c++-set-auto-hungry-state
   (memq c++-auto-hungry-initial-state '(auto-only   auto-hungry t))
   (memq c++-auto-hungry-initial-state '(hungry-only auto-hungry t))))

(defun c++-c-mode ()
  "Major mode for editing K&R and ANSI C code. Revision: 2.302 
This mode is based on c++-mode. Documentation for this mode is
available by doing a \"\\[describe-function] c++-mode\"."
  (interactive)
  (c++-mode)
  (setq major-mode 'c++-c-mode
	mode-name "C"
	local-abbrev-table c-mode-abbrev-table)
  (setq comment-start "/* "
	comment-end   " */")
  ;; some syntax differences are necessary for C vs. C++
  (set-syntax-table c++-c-mode-syntax-table)
  (run-hooks 'c++-c-mode-hook))

(defun c++-comment-indent ()
  "Used by indent-for-comment to decide how much to indent a comment
in C++ code based on its context."
  (if (looking-at "^\\(/\\*\\|//\\)")
      0					; Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max
       ;; leave at least one space on non-empty lines.
       (if (zerop (current-column))
	   0
	 (1+ (current-column)))
       ;; use comment-column if previous line is comment only line
       ;; indented to the left of comment-column
       (save-excursion
	 (beginning-of-line)
	 (if (not (bobp)) (forward-line -1))
	 (skip-chars-forward " \t")
	 (if (looking-at "/\\*\\|//")
	     (if (< (current-column) comment-column)
		 comment-column
	       (current-column))
	   0))
       (let ((cur-pt (point)))
	 (beginning-of-line 0)
	 ;; If previous line had a comment, use it's indent
	 (if (re-search-forward comment-start-skip cur-pt t)
	     (progn
	       (goto-char (match-beginning 0))
	       (current-column))
	   comment-column))))))		; otherwise indent at comment column.


;; ======================================================================
;; most command level (interactive) and related
;; ======================================================================
(defun c++-set-auto-hungry-state (auto-p hungry-p)
  "Set auto/hungry to state indicated by AUTO-P and HUNGRY-P.
Update mode line to indicate state to user."
  (setq c++-auto-newline auto-p
	c++-hungry-delete-key hungry-p)
  (set-buffer-modified-p (buffer-modified-p)))

(defun c++-toggle-auto-state (arg)
  "Toggle auto-newline state.
This function ignores c++-auto-hungry-toggle variable.  Optional
numeric ARG, if supplied turns on auto-newline when positive, turns
off auto-newline when negative and toggles when zero."
  (interactive "P")
  (let ((auto (cond
	       ((not arg)
		(not c++-auto-newline))
	       ((zerop (setq arg (prefix-numeric-value arg)))
		(not c++-auto-newline))
	       ((< arg 0) nil)
	       (t t))))
    (c++-set-auto-hungry-state auto c++-hungry-delete-key)))

(defun c++-toggle-hungry-state (arg)
  "Toggle hungry-delete-key state.
This function ignores c++-auto-hungry-toggle variable.  Optional
numeric ARG, if supplied turns on hungry-delete-key when positive,
turns off hungry-delete-key when negative and toggles when zero."
  (interactive "P")
  (let ((hungry (cond
		 ((not arg)
		  (not c++-hungry-delete-key))
		 ((zerop (setq arg (prefix-numeric-value arg)))
		  (not c++-hungry-delete-key))
		 ((< arg 0) nil)
		 (t t))))
    (c++-set-auto-hungry-state c++-auto-newline hungry)))

(defun c++-toggle-auto-hungry-state (arg)
  "Toggle auto-newline and hungry-delete-key state.
Actual toggling of these states is controlled by
c++-auto-hungry-toggle variable.

Optional argument has the following meanings when supplied:
     Universal argument \\[universal-argument]
          resets state to c++-auto-hungry-initial-state.
     negative number
          turn off both auto-newline and hungry-delete-key.
     positive number
          turn on both auto-newline and hungry-delete-key.
     zero
          toggle both states regardless of c++-auto-hungry-toggle-p."
  (interactive "P")
  (let* ((numarg (prefix-numeric-value arg))
	 (apl (list 'auto-only   'auto-hungry t))
	 (hpl (list 'hungry-only 'auto-hungry t))
	 (auto (cond
		((not arg)
		 (if (memq c++-auto-hungry-toggle apl)
		     (not c++-auto-newline)
		   c++-auto-newline))
		((listp arg)
		 (memq c++-auto-hungry-initial-state apl))
		((zerop numarg)
		 (not c++-auto-newline))
		((< arg 0) nil)
		(t t)))
	 (hungry (cond
		  ((not arg)
		   (if (memq c++-auto-hungry-toggle hpl)
		       (not c++-hungry-delete-key)
		     c++-hungry-delete-key))
		  ((listp arg)
		   (memq c++-auto-hungry-initial-state hpl))
		  ((zerop numarg)
		   (not c++-hungry-delete-key))
		  ((< arg 0) nil)
		  (t t))))
    (c++-set-auto-hungry-state auto hungry)))

(defun c++-tame-insert (arg)
  "Safely inserts certain troublesome characters in comment regions.
Because of syntax bugs in emacs, characters with string or parenthesis
syntax must be escaped with a backslash or lots of things get messed
up. Unfortunately, setting parse-sexp-ignore-comments to non-nil does
not fix the problem, but this function is unnecessary if you are
running a patched emacs.

See also the variable c++-untame-characters."
  (interactive "p")
  (if (and (memq last-command-char c++-untame-characters)
	   (memq (c++-in-literal) '(c c++)))
      (insert "\\"))
  (self-insert-command arg))

(defun c++-electric-delete (arg)
  "If c++-hungry-delete-key is non-nil, consumes all preceding
whitespace unless ARG is supplied, or point is inside a C or C++ style
comment or string.  If ARG is supplied, this just calls
backward-delete-char-untabify passing along ARG.

If c++-hungry-delete-key is nil, just call
backward-delete-char-untabify."
  (interactive "P")
  (cond
   ((or (not c++-hungry-delete-key) arg)
    (funcall c++-delete-function (prefix-numeric-value arg)))
   ((let ((bod (c++-point 'bod)))
      (not (or (memq (c++-in-literal bod) '(c c++ string))
	       (save-excursion
		 (skip-chars-backward " \t")
		 (= (preceding-char) ?#)))))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
	  (delete-region (point) here)
	(funcall c++-delete-function 1))))
   (t (funcall c++-delete-function 1))))

(defun c++-electric-pound (arg)
  (interactive "p")
  (if (memq (c++-in-literal) '(c c++ string))
      (self-insert-command arg)
    (let ((here (point-marker))
	  (bobp (bobp))
	  (bolp (bolp)))
      (if (memq 'alignleft c++-electric-pound-behavior)
	  (progn (beginning-of-line)
		 (delete-horizontal-space)))
      (if bobp
	  (insert (make-string arg last-command-char))
	(insert-before-markers (make-string arg last-command-char)))
      (if (not bolp)
	  (goto-char here))
      (set-marker here nil))))

(defun c++-electric-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos
	(last-command-char last-command-char)
	(bod (c++-point 'bod)))
    (if (and (not arg)
	     (save-excursion
	       (skip-chars-forward " \t")
	       (eolp))
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (let ((c++-auto-newline c++-auto-newline)
		       (open-brace-p (= last-command-char ?{)))
		   (if (and open-brace-p
			    (or (eq c++-hanging-braces t)
				(and c++-hanging-braces
				     (not (c++-at-top-level-p t bod)))))
		       (setq c++-auto-newline nil))
		   (if (c++-auto-newline)
		       ;; this may have auto-filled so we need to
		       ;; indent the previous line. we also need to
		       ;; indent the currently line, or
		       ;; c++-beginning-of-defun will not be able to
		       ;; correctly find the bod when
		       ;; c++-match-headers-strongly is nil.
		       (progn (c++-indent-line)
			      (save-excursion
				(forward-line -1)
				(c++-indent-line))))
		   t)))
	(progn
	  (if (and (memq last-command-char c++-untame-characters)
		   (memq (c++-in-literal bod) '(c c++)))
	      (insert "\\"))
	  (insert last-command-char)
	  ;; try to clean up empty defun braces if conditions apply
	  (let ((here (point-marker)))
	    (and (memq 'empty-defun-braces c++-cleanup-list)
		 (c++-at-top-level-p t bod)
		 c++-auto-newline
		 (= last-command-char ?\})
		 (progn (forward-char -1)
			(skip-chars-backward " \t\n")
			(= (preceding-char) ?\{))
		 (not (memq (c++-in-literal) '(c c++ string)))
		 (delete-region (point) (1- here)))
	    (goto-char here)
	    (set-marker here nil))
	  (let ((here (point-marker))
		mbeg mend)
	    (if (and (memq 'brace-else-brace c++-cleanup-list)
		     (= last-command-char ?\{)
		     (let ((status
			    (re-search-backward "}[ \t\n]*else[ \t\n]*{"
						nil t)))
		       (setq mbeg (match-beginning 0)
			     mend (match-end 0))
		       status)
		     (= mend here)
		     (not (memq (c++-in-literal bod) '(c c++ string))))
		(progn
		  ;; we should clean up brace-else-brace syntax
		  (delete-region mbeg mend)
		  (insert-before-markers "} else {")
		  (goto-char here)
		  (set-marker here nil))
	      (goto-char here)
	      (set-marker here nil)))
	  (c++-indent-line)
	  (if (c++-auto-newline)
	      (progn
		;; c++-auto-newline may have done an auto-fill
		(save-excursion
		  (let ((here (point-marker)))
		    (goto-char (- (point) 2))
		    (c++-indent-line)
		    (setq insertpos (- (goto-char here) 2))
		    (set-marker here nil)))
		(c++-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun c++-electric-slash (arg)
  "Insert slash, and if slash is second of a double-slash comment
introducing construct, indent line as comment.  This only indents if
we're on a comment-only line, otherwise use indent-for-comment (\\[indent-for-comment])."
  (interactive "P")
  (let ((here (point)) char)
    (self-insert-command (prefix-numeric-value arg))
    (and (setq char (char-after (1- here)))
	 (= char ?/)
	 (save-excursion
	   (goto-char here)
	   (c++-indent-line)))))

(defun c++-electric-star (arg)
  "Works with c++-electric-slash to auto indent C style comment lines."
  (interactive "P")
  (let ((here (point)) char)
    (self-insert-command (prefix-numeric-value arg))
    (if (and (setq char (char-after (1- here)))
	     (or (= char ?/)
		 (and (memq (c++-in-literal) '(c))
		      (or (= (point) (c++-point 'boi))
			  (= (preceding-char) ?*)))))
	(save-excursion
	  (goto-char here)
	  (c++-indent-line)))))

(defun c++-electric-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if (c++-in-literal)
      (self-insert-command (prefix-numeric-value arg))
    (let ((here (point-marker)))
      (if (and (memq 'defun-close-semi c++-cleanup-list)
	       c++-auto-newline
	       (progn
		 (skip-chars-backward " \t\n")
		 (= (preceding-char) ?})))
	  (delete-region here (point)))
      (goto-char here)
      (set-marker here nil))
    (c++-electric-terminator arg)))

(defun c++-electric-colon (arg)
  "Electrify colon.  De-auto-newline double colons. No auto-new-lines
for member initialization list."
  (interactive "P")
  (if (c++-in-literal)
      (self-insert-command (prefix-numeric-value arg))
    (let ((c++-auto-newline c++-auto-newline)
	  (insertion-point (point))
	  (bod (c++-point 'bod)))
      (save-excursion
	(cond
	 ;; check for double-colon where the first colon is not in a
	 ;; comment or literal region
	 ((progn (skip-chars-backward " \t\n")
		 (and (= (preceding-char) ?:)
		      (not (memq (c++-in-literal bod) '(c c++ string)))))
	  (progn (delete-region insertion-point (point))
		 (setq c++-auto-newline nil
		       insertion-point (point))))
	 ;; check for ?: construct which may be at any level
	 ((progn (goto-char insertion-point)
		 (condition-case premature-end
		     (backward-sexp 1)
		   (error nil))
		 (c++-backward-syntactic-ws bod)
		 (= (preceding-char) ?\?))
	  (setq c++-auto-newline nil))
	 ;; check for being at top level or top with respect to the
	 ;; class. if not, process as normal
	 ((progn (goto-char insertion-point)
		 (not (c++-at-top-level-p t bod))))
	 ;; if at top level, check to see if we are introducing a member
	 ;; init list. if not, continue
	 ((progn (c++-backward-syntactic-ws bod)
		 (= (preceding-char) ?\)))
	  (goto-char insertion-point)
	  ;; at a member init list, figure out about auto newlining. if
	  ;; nil or before then put a newline before the colon and
	  ;; adjust the insertion point, but *only* if there is no
	  ;; newline already before the insertion point
	  (if (and (memq c++-hanging-member-init-colon '(nil before))
		   c++-auto-newline)
	      (if (not (save-excursion (skip-chars-backward " \t")
				       (bolp)))
		  (let ((c++-auto-newline t))
		    (c++-auto-newline)
		    (setq insertion-point (point)))))
	  ;; if hanging colon is after or nil, then newline is inserted
	  ;; after colon. set up variable so c++-electric-terminator
	  ;; places the newline correctly
	  (setq c++-auto-newline
		(and c++-auto-newline
		     (memq c++-hanging-member-init-colon '(nil after)))))
	 ;; last condition is always put newline after colon
	 (t (setq c++-auto-newline nil))
	 ))				; end-cond, end-save-excursion
      (goto-char insertion-point)
      (c++-electric-terminator arg))))

(defun c++-electric-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos (end (point)))
    (if (and (not arg)
	     (save-excursion
	       (skip-chars-forward " \t")
	       (eolp))
	     (not (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    (or (= (following-char) ?#)
			;; Colon is special only after a label, or
			;; case, or another colon.
			;; So quickly rule out most other uses of colon
			;; and do no indentation for them.
			(and (eq last-command-char ?:)
			     (not (looking-at "case[ \t]"))
			     (save-excursion
			       (forward-word 1)
			       (skip-chars-forward " \t")
			       (< (point) end))
			     ;; Do re-indent double colons
			     (save-excursion
			       (end-of-line 1)
			       (looking-at ":")))
			(progn
			  (c++-beginning-of-defun)
			  (let* ((parse-sexp-ignore-comments t)
				 (pps (parse-partial-sexp (point) end)))
			    (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))))
	(progn
	  (insert last-command-char)
	  (c++-indent-line)
	  (and c++-auto-newline
	       (not (c++-in-parens-p))
	       (progn
		 ;; the new marker object, used to be just an integer
		 (setq insertpos (make-marker))
		 ;; changed setq to set-marker
		 (set-marker insertpos (1- (point)))
		 ;; do this before the newline, since in auto fill can break
		 (newline)
		 (c++-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun c++-indent-command (&optional whole-exp)
  "Indent current line as C++ code, or in some cases insert a tab character.

If c++-tab-always-indent is t, always just indent the current line.
If nil, indent the current line only if point is at the left margin or
in the line's indentation; otherwise insert a tab.  If not-nil-or-t,
then tab is inserted only within literals (comments and strings) and
inside preprocessor directives, but line is always reindented.

A numeric argument, regardless of its value, means indent rigidly all
the lines of the expression starting after point so that this line
becomes properly indented.  The relative indentation among the lines
of the expression are preserved."
  (interactive "P")
  (let ((bod (c++-point 'bod)))
    (if whole-exp
	;; If arg, always indent this line as C
	;; and shift remaining lines of expression the same amount.
	(let ((shift-amt (c++-indent-line bod))
	      beg end)
	  (save-excursion
	    (if (eq c++-tab-always-indent t)
		(beginning-of-line))
	    (setq beg (point))
	    (forward-sexp 1)
	    (setq end (point))
	    (goto-char beg)
	    (forward-line 1)
	    (setq beg (point)))
	  (if (> end beg)
	      (indent-code-rigidly beg end shift-amt "#")))
      (cond
       ;; CASE 1: indent when at column zero or in lines indentation,
       ;; otherwise insert a tab
       ((not c++-tab-always-indent)
	(if (and (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (or (looking-at "[ \t]*$")
		     (/= (point) (c++-point 'boi))
		     (bolp)))
	    (c++-indent-line bod)
	  (insert-tab)))
       ;; CASE 2: just indent the line
       ((eq c++-tab-always-indent t)
	(c++-indent-line bod))
       ;; CASE 3: if in a literal, insert a tab, but always indent the line
       ((or (memq (c++-in-literal bod) '(c c++ string))
	    (save-excursion
	      (skip-chars-backward " \t")
	      (= (preceding-char) ?#)))
	(let ((here (point))
	      (boi (save-excursion (back-to-indentation) (point)))
	      (indent-p nil))
	  (c++-indent-line bod)
	  (save-excursion
	    (back-to-indentation)
	    (setq indent-p (and (> here boi) (= (point) boi))))
	  (if indent-p (insert-tab))))
       ;; CASE 4: bogus, just indent the line
       (t (c++-indent-line bod))))))

(defun c++-indent-exp ()
  "Indent each line of the C++ grouping following point."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	restart outer-loop-done inner-loop-done state ostate
	this-indent last-sexp last-depth
	at-else at-brace
	(parse-sexp-ignore-comments t)
	(opoint (point))
	(next-depth 0))
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq inner-loop-done nil)
	(while (and (not inner-loop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  ;; fix by reed@adapt.net.com
	  ;; must pass in the return past the end of line, so that
	  ;; parse-partial-sexp finds it, and recognizes that a "//"
	  ;; comment is over. otherwise, state is set that we're in a
	  ;; comment, and never gets unset, causing outer-loop to only
	  ;; terminate in (eobp). old:
	  ;;(setq state (parse-partial-sexp (point)
	  ;;(progn (end-of-line) (point))
	  ;;nil nil state))
	  (let ((start (point))
		(line-end
		 (progn (end-of-line)
			(while (eq (c++-in-literal) 'c)
			  (forward-line 1)
			  (end-of-line))
			(skip-chars-backward " \t")
			(end-of-line)
			(point)))
		(end (progn (if (not (eobp)) (forward-char)) (point))))
	    (setq state (parse-partial-sexp start end nil nil state))
	    (goto-char line-end))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (or (nth 4 ostate))
	      (c++-indent-line))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq inner-loop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  ;; If this line had ..))) (((.. in it, pop out of the levels
	  ;; that ended anywhere in this line, even if the final depth
	  ;; doesn't indicate that they ended.
	  (while (> last-depth (nth 6 state))
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  ;; Add levels for any parens that were started in this line.
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack (or (car (cdr state))
					(save-excursion (forward-sexp -1)
							(point)))))
	  (forward-line 1)
	  (skip-chars-forward " \t")
	  ;; check for C comment block
	  (if (memq (c++-in-literal) '(c))
	      (let ((eoc (save-excursion
			   (re-search-forward "\\*/" (point-max) 'move)
			   (point))))
		(while (< (point) eoc)
		  (c++-indent-line)
		  (forward-line 1))))
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		;; Lines inside parens are handled specially.
		(if (or (/= (char-after (car contain-stack)) ?{)
			;;(c++-at-top-level-p t))
			;; baw hack for continued statement offsets
			;; repercussions???
			t)
		    (setq this-indent (car indent-stack))
		  ;; Line is at statement level.
		  ;; Is it a new statement?  Is it an else?
		  ;; Find last non-comment character before this line
		  (save-excursion
		    (setq at-else (looking-at "else\\W"))
		    (setq at-brace (= (following-char) ?{))
		    (c++-backward-syntactic-ws opoint)
		    (if (not (memq (preceding-char) '(nil ?\, ?\; ?} ?: ?{)))
			;; Preceding line did not end in comma or semi;
			;; indent this line  c-continued-statement-offset
			;; more than previous.
			(progn
			  (c-backward-to-start-of-continued-exp
			   (car contain-stack))
			  (setq this-indent
				(+ c-continued-statement-offset
				   (current-column)
				   (if at-brace c-continued-brace-offset 0))))
		      ;; Preceding line ended in comma or semi;
		      ;; use the standard indent for this level.
		      (if at-else
			  (progn (c++-backward-to-start-of-if opoint)
				 (back-to-indentation)
				 (skip-chars-forward "{ \t")
				 (setq this-indent (current-column)))
			(setq this-indent (car indent-stack))))))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (c++-calculate-indent
			  (if (car indent-stack)
			      (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))))
	    ;; Adjust line indentation according to its contents
 	    (if (looking-at c++-access-key)
 		(setq this-indent (+ this-indent c++-access-specifier-offset))
	      (if (or (looking-at "case[ \t]")
		      (and (looking-at "[A-Za-z]")
			   (save-excursion
			     (forward-sexp 1)
			     (looking-at ":[^:]"))))
		  (setq this-indent (max 0 (+ this-indent c-label-offset)))))
	    ;; looking at a comment only line?
	    (if (looking-at comment-start-skip)
		;; different indentation base on whether this is a
		;; col0 comment only line or not. also, if comment is
		;; in, or to the right of comment-column, the comment
		;; doesn't move
		(progn
		  (skip-chars-forward " \t")
		  (setq this-indent
			(if (>= (current-column) comment-column)
			    (current-column)
			  (c++-comment-offset (bolp) this-indent)))))
	    (if (looking-at "friend[ \t]")
		(setq this-indent (+ this-indent c++-friend-offset)))
	    (if (= (following-char) ?})
		(setq this-indent (- this-indent c-indent-level)))
	    (if (= (following-char) ?{)
		(setq this-indent (+ this-indent c-brace-offset)))
	    ;; check for continued statements
	    (if (save-excursion
		  (c++-backward-syntactic-ws (car contain-stack))
		  (and (not (c++-in-parens-p))
		       (not (memq (preceding-char)
				  '(nil ?\000 ?\, ?\; ?\} ?\: ?\{)))
		       (progn
			 (beginning-of-line)
			 (skip-chars-forward " \t")
			 (not (looking-at c++-class-key)))))
		(setq this-indent (+ this-indent c-continued-statement-offset))
	      )
	    ;; check for stream operator
	    (if (looking-at "\\(<<\\|>>\\)")
		(setq this-indent (c++-calculate-indent)))
	    ;; Put chosen indentation into effect.
	    (or (= (current-column) this-indent)
		(= (following-char) ?\#)
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to this-indent)))
	    ;; Indent any comment following the text.
	    (or (looking-at comment-start-skip)
		(if (re-search-forward
		     comment-start-skip
		     (c++-point 'eol) t)
		    (progn (indent-for-comment)
			   (beginning-of-line))))
	    ))))))

(defun c++-insert-header ()
  "Insert header denoting C++ code at top of buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "// "
	    "This may look like C code, but it is really "
	    "-*- C++ -*-"
	    "\n\n")))

(defun c++-tame-comments ()
  "Backslashifies all untamed in comment regions found in the buffer.
This is a workaround for emacs syntax bugs. This function is
unnecessary (and un-used automatically) if you are running a patched
emacs. Untamed characters to escape are defined in the variable
c++-untame-characters."
  (interactive)
  ;; make the list into a valid charset, escaping where necessary
  (let ((charset (concat "^" (mapconcat
			      (function
			       (lambda (char)
				 (if (memq char '(?\\ ?^ ?-))
				     (concat "\\" (char-to-string char))
				   (char-to-string char))))
			      c++-untame-characters ""))))
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
	(skip-chars-forward charset)
	(if (and (not (zerop (following-char)))
		 (memq (c++-in-literal) '(c c++))
		 (/= (preceding-char) ?\\ ))
	    (insert "\\"))
	(if (not (eobp))
	    (forward-char 1))))))

;; taken from match-paren.el. Author: unknown
(defun c++-match-paren ()
  "Jumps to the paren matching the one under point, if there is one."
  (interactive)
  (let ((parse-sexp-ignore-comments c++-emacs-is-fixed-p))
    (cond
     ((looking-at "[\(\[{]")
      (forward-sexp 1)
      (backward-char))
     ((looking-at "[])}]")
      (forward-char)
      (backward-sexp 1))
     (t (message "Could not find matching paren.")))))

(defun c++-forward-sexp (&optional arg)
  (interactive "p")
  (let ((parse-sexp-ignore-comments c++-emacs-is-fixed-p))
    (forward-sexp arg)))

(defun c++-backward-sexp (&optional arg)
  (interactive "p")
  (let ((parse-sexp-ignore-comments c++-emacs-is-fixed-p))
    (backward-sexp arg)))


;; ======================================================================
;; defuns for parsing syntactic elements
;; ======================================================================
(defun c++-parse-state (&optional limit)
  "Determinate the syntactic state of the code at point.
Iteratively uses parse-partial-sexp from point to LIMIT and returns
the result of parse-partial-sexp at point.  LIMIT is optional and
defaults to point-max."
  (setq limit (or limit (point-max)))
  (let (state (parse-sexp-ignore-comments t))
    (while (< (point) limit)
      (setq state (parse-partial-sexp (point) limit 0)))
    state))

(defun c++-at-top-level-p (wrt &optional bod)
  "Return t if point is not inside a containing C++ expression, nil
if it is embedded in an expression.  When WRT is non-nil, returns nil
if not at the top level with respect to an enclosing class, or the
depth of class nesting at point.  With WRT nil, returns nil if not at
the \"real\" top level.  Optional BOD is the beginning of defun."
  (save-excursion
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state containing-sexp paren-depth
	  (bod (or bod (c++-point 'bod)))
	  foundp)
      (goto-char bod)
      (setq state (c++-parse-state indent-point)
	    containing-sexp (nth 1 state)
	    paren-depth (nth 0 state))
      (cond
       ((eq major-mode 'c++-c-mode)
	(and (null containing-sexp) 0))
       ((not wrt)
	(null containing-sexp))
       ((null containing-sexp) 0)
       ((c++-in-parens-p) nil)
       (t
	;; calculate depth wrt containing (possibly nested) classes
	(goto-char containing-sexp)
	(while (and (setq foundp (re-search-backward
				  (concat "[;}]\\|" c++-class-key)
				  (point-min) t))
		    (let ((bod (c++-point 'bod)))
		      (or (c++-in-literal bod)
			  (c++-in-parens-p bod)
			  ;; see if class key is inside a template spec
			  (and (looking-at c++-class-key)
			       (progn (skip-chars-backward " \t\n")
				      (memq (preceding-char) '(?, ?<))))))))
	(if (memq (following-char) '(?} ?\;))
	    nil
	  (setq state (c++-parse-state containing-sexp))
	  (and foundp
	       (not (nth 1 state))
	       (nth 2 state)
	       paren-depth))
	)))))

(defun c++-in-literal-quick (&optional lim)
  "Determine if point is in a C++ `literal'.
Return 'c if in a C-style comment, 'c++ if in a C++ style comment,
'string if in a string literal, 'pound if on a preprocessor line, or
nil if not in a comment at all.  Optional LIM is used as the backward
limit of the search.  If omitted, or nil, c++-beginning-of-defun is
used."
  (save-excursion
    (let* ((backlim (or lim (c++-point 'bod)))
	   (here (point))
	   (parse-sexp-ignore-comments t) ; may not be necessary
	   (state (parse-partial-sexp backlim (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) (if (nth 7 state) 'c++ 'c))
       ((progn
	  (goto-char here)
	  (beginning-of-line)
	  (looking-at "[ \t]*#"))
	'pound)
       (t nil)))))

(defun c++-in-literal (&optional lim)
  "Determine if point is in a C++ `literal'.
Return 'c if in a C-style comment, 'c++ if in a C++ style comment,
'string if in a string literal, 'pound if on a preprocessor line, or
nil if not in a comment at all.  Optional LIM is used as the backward
limit of the search.  If omitted, or nil, c++-beginning-of-defun is
used."
  (save-excursion
    (let* ((here (point))
	   (state nil)
	   (match nil)
	   (backlim (or lim (c++-point 'bod))))
      (goto-char backlim)
      (while (< (point) here)
	(setq match
	      (and (re-search-forward "\\(/[/*]\\)\\|[\"']\\|\\(^[ \t]*#\\)"
				      here 'move)
		   (buffer-substring (match-beginning 0) (match-end 0))))
	(setq state
	      (cond
	       ;; no match
	       ((null match) nil)
	       ;; looking at the opening of a C++ style comment
	       ((string= "//" match)
		(if (<= here (progn (end-of-line) (point))) 'c++))
	       ;; looking at the opening of a C block comment
	       ((string= "/*" match)
		(if (not (re-search-forward "*/" here 'move)) 'c))
	       ;; looking at the opening of a double quote string
	       ((string= "\"" match)
		(if (not (save-restriction
			   ;; this seems to be necessary since the
			   ;; re-search-forward will not work without it
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this regexp matches a double quote
			    ;; which is preceeded by an even number
			    ;; of backslashes, including zero
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\"" here 'move)))
		    'string))
	       ;; looking at the opening of a single quote string
	       ((string= "'" match)
		(if (not (save-restriction
			   ;; see comments from above
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this matches a single quote which is
			    ;; preceeded by zero or two backslashes.
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)?'"
			    here 'move)))
		    'string))
	       ((string-match "[ \t]*#" match)
		(if (<= here (progn (end-of-line) (point))) 'pound))
	       (t nil)))
	) ; end-while
      state)))

(if c++-emacs-is-fixed-p
    (fset 'c++-in-literal 'c++-in-literal-quick))

(defun c++-in-parens-p (&optional lim)
  "Return t if inside a paren expression.
Optional LIM is used as the backward limit of the search."
  (let ((lim (or lim (c++-point 'bod))))
    (condition-case var
	(save-excursion
	  (save-restriction
	    (narrow-to-region (point) lim)
	    (goto-char (point-max))
	    (= (char-after (or (scan-lists (point) -1 1)
			       (point-min)))
	       ?\()))
      (error nil)
      )))

(defun c++-in-function-p (&optional containing)
  "Return t if inside a C++ function definition.
Optional CONTAINING is position of containing s-exp open brace. If not
supplied, point is used as search start."
  (save-excursion
    (let ((here (if (not containing)
		    (point)
		  (goto-char containing)
		  (c++-backward-syntactic-ws)
		  (point))))
      (if (and (= (preceding-char) ?t)
	       (forward-word -1)
	       (looking-at "\\<const\\>"))
	  (c++-backward-syntactic-ws)
	(goto-char here))
      (= (preceding-char) ?\)))))


;; ======================================================================
;; defuns for calculating indentation
;; ======================================================================
(defun c++-indent-line (&optional bod)
  "Indent current line as C++ code.
Return the amount the indentation changed by.  Optional BOD is the
point of the beginning of the C++ definition."
  (let* ((bod (or bod (c++-point 'bod)))
	 (indent (c++-calculate-indent nil bod))
	 beg shift-amt
	 (case-fold-search nil)
	 (pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond
     ((eq indent nil)
      (setq indent (current-indentation)))
     ((eq indent t)
      (setq indent (c++-calculate-c-indent-within-comment)))
     ((looking-at "[ \t]*#")
      (setq indent 0))
     ((save-excursion
	(back-to-indentation)
	(looking-at "//\\|/\\*"))
      ;; we've found a comment-only line. we now must try to determine
      ;; if the line is a continuation from a comment on the previous
      ;; line.  we check to see if the comment starts in or to the
      ;; right of comment-column and if so, we don't change its
      ;; indentation.
      (skip-chars-forward " \t")
      (if (>= (current-column) comment-column)
	  (setq indent (current-column))
	(setq indent (c++-comment-offset (bolp) indent))))
     (t
      (skip-chars-forward " \t")
      (if (listp indent) (setq indent (car indent)))
      (cond
       ((looking-at c++-access-key)
	(setq indent (+ indent c++-access-specifier-offset)))
       ((looking-at "default[ \t]*:")
	(setq indent (+ indent c-label-offset)))
       ((or (looking-at "case[ \t]+.*:")
	    (and (looking-at "[A-Za-z]")
		 (save-excursion
		   (forward-sexp 1)
		   (looking-at ":[^:]"))))
	(setq indent (max 1 (+ indent c-label-offset))))
       ((and (looking-at "else\\b")
	     (not (looking-at "else\\s_")))
	(setq indent (save-excursion
		       (c++-backward-to-start-of-if)
		       (back-to-indentation)
		       (skip-chars-forward "{ \t")
		       (current-column))))
       ((looking-at "\\<friend\\>")
	(setq indent (+ indent c++-friend-offset)))
       ((and (= (following-char) ?\))
	     c++-paren-as-block-close-p)
	(setq indent (+ (- indent c-indent-level)
			(if (save-excursion
			      (forward-char 1)
			      (c++-at-top-level-p nil bod))
			    (- c++-block-close-brace-offset)
			  c++-block-close-brace-offset))))
       ((= (following-char) ?})
	(setq indent (+ (- indent c-indent-level)
			(if (save-excursion
			      (forward-char 1)
			      (c++-at-top-level-p nil bod))
			    (- c++-block-close-brace-offset)
			  c++-block-close-brace-offset))))
       ((= (following-char) ?{)
	(setq indent (+ indent c-brace-offset))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    ;; save-excursion is necessary because things break if the hook
    ;; changes point or mark
    (save-excursion
      (run-hooks 'c++-special-indent-hook))
    shift-amt))

(defun c++-cont-indent (ipnt char lim)
  "Calculate the indentation for a continued statement.
IPNT is the indentation point; CHAR is the character before the
indentation point, excluding any intervenine whitespace; LIM is the
minimum point to search backwards to"
  (let ((charlist '(nil ?\000 ?\, ?\; ?\} ?\: ?\{))
	streamop-pos here)
    (goto-char ipnt)
    (c++-backward-syntactic-ws lim)
    (if (not (memq char charlist))
	;; This line is continuation of preceding line's statement
	(progn
	  (c-backward-to-start-of-continued-exp lim)
	  ;; take care of << and >> while in streams
	  (setq here (point))
	  (if (save-excursion
		(and (progn (goto-char ipnt)
			    (looking-at "[ \t]*\\(<<\\|>>\\)"))
		     (progn (goto-char here)
			    (skip-chars-forward "^><\n")
			    (setq streamop-pos (current-column))
			    (looking-at "\\(<<\\|>>\\)"))))
	      streamop-pos
	    (+ (current-column)
	       ;; prevent repeated continued indentation
	       (if (save-excursion
		     (beginning-of-line 1)
		     (c++-backward-syntactic-ws lim)
		     (memq (preceding-char) charlist))
		   c-continued-statement-offset
		 ;; the following statements *do* indent even
		 ;; for single statements (are there others?)
		 (if (looking-at "\\(do\\|else\\|for\\|if\\|while\\)\\b")
		     c-continued-statement-offset
		   ;; else may be a continued statement inside
		   ;; a simple for/else/while/if/do loop
		   (beginning-of-line 1)
		   (forward-char -1)
		   (c++-backward-syntactic-ws lim)
		   (c-backward-to-start-of-continued-exp lim)
		   (if (looking-at "\\(do\\|else\\|for\\|if\\|while\\)\\b")
		       c-continued-statement-offset
		     0)))
	       (save-excursion
		 (goto-char ipnt)
		 (skip-chars-forward " \t")
		 (cond
		  ((= (following-char) ?\{)
		   c-continued-brace-offset)
		  ((and (= (following-char) ?\})
			(progn (forward-char 1)
			       (c++-at-top-level-p nil lim)))
		   (- c-continued-statement-offset))
		  (t 0))))))
      nil)))

(defun c++-calculate-indent (&optional parse-start bod)
  "Return appropriate indentation for current line as C++ code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment.
Optional PARSE-START is the location to start parsing, and optional
BOD is the beginning of the C++ definition."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state do-indentation literal
	  containing-sexp streamop-pos char-before-ip
	  (inclass-shift 0) inclass-depth inclass-unshift
	  (bod (or bod (c++-point 'bod))))
      (if parse-start
	  (goto-char parse-start)
	(goto-char bod))
      (setq parse-start (point)
	    state (c++-parse-state indent-point)
	    containing-sexp (nth 1 state))
      ;; it is possible that c++-defun-header-weak could not find the
      ;; beginning of the C++ definition. The following code attempts
      ;; to work around this.  It is probably better to just use
      ;; c++-match-header-strongly, but there are performance questions
      (if (null state)
	  (let* ((c++-match-header-strongly t)
		 (bod (c++-point 'bod)))
	    (goto-char bod)
	    (setq state (c++-parse-state indent-point)
		  containing-sexp (nth 1 state)
		  parse-start (point))))
      (setq literal (c++-in-literal bod))
      ;; cache char before indent point
      (save-excursion
	(goto-char indent-point)
	(c++-backward-syntactic-ws bod)
	(setq char-before-ip (preceding-char)))
      (cond
       ;; CASE 1: in a string.
       ((memq literal '(string)) nil)
       ;; CASE 2: in a C or C++ style comment.
       ((memq literal '(c c++)) t)
       ;; CASE 3: Line is at top level.  May be comment-only line,
       ;; data or function definition, or may be function argument
       ;; declaration or member initialization.  Indent like the
       ;; previous top level line unless:
       ;;
       ;; 1. the previous line ends in a closeparen without
       ;; semicolon, in which case this line is the first
       ;; argument declaration or member initialization, or
       ;;
       ;; 2. the previous line ends with a closeparen
       ;; (closebrace), optional spaces, and a semicolon, in
       ;; which case this line follows a multiline function
       ;; declaration (class definition), or
       ;;
       ;; 3. the previous line begins with a colon, in which
       ;; case this is the second line of member inits.  It is
       ;; assumed that arg decls and member inits are not mixed.
       ;;
       ((setq inclass-depth (c++-at-top-level-p t bod))
	(+
	 ;; add an offset if we are inside a class defun body,
	 ;; i.e. we are at the top level, but only wrt a
	 ;; containing class
	 (let ((shift/level (+ c-indent-level c-brace-imaginary-offset)))
	   (setq inclass-shift (* shift/level inclass-depth)
		 inclass-unshift (* shift/level (max 0 (1- inclass-depth))))
	   inclass-shift)
	 (progn
	   (goto-char indent-point)
	   (skip-chars-forward " \t")
	   (if (or (= (following-char) ?{)
		   (progn
		     (c++-backward-syntactic-ws parse-start)
		     (bobp)))
	       0
	     (if (c++-in-function-p)
		 (progn			; first arg decl or member init
		   (goto-char indent-point)
		   (skip-chars-forward " \t")
		   (if (= (following-char) ?:)
		       c++-member-init-indent
		     c-argdecl-indent))
	       (if (= (preceding-char) ?\;)
		   (progn
		     (backward-char 1)
		     (skip-chars-backward " \t")))
	       ;; may be first line after a hanging member init
	       ;; colon. check to be sure its not a scope
	       ;; operator meaning we are inside a member def
	       (if (or (= (preceding-char) ?:)
		       (save-excursion
			 (forward-line 1)
			 (skip-chars-forward " \t")
			 (or (eobp) (forward-char 1))
			 (and (= (preceding-char) ?:)
			      (/= (following-char) ?:)))
		       (save-excursion
			 (and (= (preceding-char) ?,)
			      (let ((bol (c++-point 'bol)))
				(skip-chars-backward "^:" bol)
				(= (preceding-char) ?:))
			      (not (c++-in-parens-p))
			      (progn
				(forward-char -1)
				(skip-chars-backward " \t")
				(not (bolp)))
			      ;; make sure its not a multiple inheritance
			      ;; continuation line
			      (progn
				(beginning-of-line)
				(not (looking-at c++-inher-key)))
			      )))
		   ;; check to see if we're looking at a member
		   ;; init, or access specifier
		   (if (progn
			 (beginning-of-line)
			 (skip-chars-forward " \t")
			 (looking-at c++-access-key))
		       ;; access specifier. class defun opening brace
		       ;; may not be in col zero
		       (progn (goto-char (or containing-sexp bod))
			      (- (current-indentation)
				 ;; remove some nested inclass indentation
				 inclass-unshift))
		     ;; member init, so add offset. add additional
		     ;; offset if looking at line with just a member
		     ;; init colon
		     (+ c++-member-init-indent
			(if (looking-at ":[ \t]*$")
			    (or c++-continued-member-init-offset 0) 0)))
		 (if (or (= (preceding-char) ?})
			 (= (preceding-char) ?\))
			 (save-excursion
			   (beginning-of-line)
			   (looking-at "[ \t]*\\<friend\\>")))
		     ;; indentation of class defun opening brace
		     ;; may not be zero
		     (progn (goto-char (or containing-sexp bod))
			    (- (current-indentation)
			       ;; remove some nested inclass indentation
			       inclass-unshift))
		   ;; cont arg decls or member inits
		   (beginning-of-line)
		   ;; we might be inside a K&R C arg decl
		   (if (save-excursion
			 (c++-backward-syntactic-ws bod)
			 (and (eq major-mode 'c++-c-mode)
			      (= (preceding-char) ?\))))
		       c-argdecl-indent
		     (skip-chars-forward " \t")
		     (if (or (memq (c++-in-literal bod) '(c c++))
			     (looking-at "/[/*]"))
			 0
		       (if (= (following-char) ?:)
			   (if c++-continued-member-init-offset
			       (+ (current-indentation)
				  c++-continued-member-init-offset)
			     (progn
			       (forward-char 1)
			       (skip-chars-forward " \t")
			       (- (current-column)
				  inclass-shift)))
			 ;; else first check to see if its a
			 ;; multiple inheritance continuation line,
			 ;; but not a K&R C arg decl
			 (if (and (not (eq major-mode 'c++-c-mode))
				  (looking-at c++-inher-key))
			     (if (= char-before-ip ?,)
				 (progn (goto-char (match-end 0))
					(current-column))
			       ;; nope, its probably a nested class
			       0)
			   ;; we might be looking at the opening
			   ;; brace of a class defun
			   (if (= (following-char) ?\{)
			       ;; indentation of opening brace may not
			       ;; be zero
			       (- (current-indentation)
				  ;; remove some nested inclass indentation
				  inclass-unshift)
			     (if (eolp)
				 ;; looking at a blank line, indent
				 ;; next line to zero
				 0
			       (if (save-excursion
				     (goto-char indent-point)
				     (beginning-of-line)
				     (bobp))
				   ;; at beginning of buffer, if
				   ;; nothing else, indent to zero 
				   0
				 (if (c++-in-parens-p)
				     ;; we are perhaps inside a
				     ;; member init call
				     (while (and (c++-in-parens-p)
						 (< bod (point)))
				       (forward-line -1)
				       (skip-chars-forward " \t")))
				 ;; check to be sure that we're
				 ;; not on the first line of the
				 ;; member init list
				 (if (= (following-char) ?:)
				     (progn
				       (forward-char 1)
				       (skip-chars-forward " \t")))
				 ;; subtract inclass-shift since
				 ;; its already incorporated by
				 ;; default in current-column
				 (- (cond
				     ;;((save-excursion
				     ;;(c++-cont-indent
				     ;;indent-point char-before-ip
				     ;;(or containing-sexp bod))))
				     ;;((= char-before-ip ?\;)
				     ;;(goto-char (or containing-sexp bod))
				     ;;(+ (current-indentation)
				     ;;inclass-shift))
				     (t (current-column)))
				    inclass-shift)
				 )))))))))
	       )))))
       ;; CASE 4: line is expression, not statement. indent to just
       ;; after the surrounding open -- unless empty arg list, in
       ;; which case we do what c++-empty-arglist-indent says to do.
       ((/= (char-after containing-sexp) ?{)
	(if (and c++-empty-arglist-indent
		 (or c++-always-arglist-indent-p
		     (null (nth 2 state))
		     ;; indicates empty arg list.  Use a heuristic: if
		     ;; the first non-whitespace following left paren
		     ;; on same line is not a comment, is not an empty
		     ;; arglist.
		     (save-excursion
		       (goto-char (1+ containing-sexp))
		       (looking-at "[ \t]*[/\n]"))))
	    (progn
	      (goto-char containing-sexp)
	      (beginning-of-line)
	      (skip-chars-forward " \t")
	      (goto-char (min (+ (point) c++-empty-arglist-indent)
			      (1+ containing-sexp)))
	      (current-column))
	  ;; In C-mode, we would always indent to one after the
	  ;; left paren.  Here, though, we may have an
	  ;; empty-arglist, so we'll indent to the min of that
	  ;; and the beginning of the first argument.
	  (goto-char (1+ containing-sexp))
	  ;; we want to skip any whitespace b/w open paren and
	  ;; first argurment. this handles while (thing) style
	  ;; and while( thing ) style
	  (skip-chars-forward " \t")
	  (current-column)))
       ;; CASE 5: Statement.  Find previous non-comment character.
       (t
	(or (c++-cont-indent indent-point char-before-ip containing-sexp)
	  ;; This line may start a new statement, or it could
	  ;; represent the while closure of a do/while construct
	  (if (save-excursion
		(and (progn (goto-char indent-point)
			    (skip-chars-forward " \t\n")
			    (looking-at "while\\b"))
		     (progn
		       (c++-backward-to-start-of-do containing-sexp)
		       (looking-at "do\\b"))
		     (setq do-indentation (current-column))))
	      do-indentation
	    ;; this could be a case statement. if so we want to
	    ;; indent it like the first case statement after a switch
	    (if (save-excursion
		  (goto-char indent-point)
		  (skip-chars-forward " \t\n")
		  (looking-at "case[ \t]+.*:"))
		(progn
		  (goto-char containing-sexp)
		  (back-to-indentation)
		  (+ (current-column) c-indent-level))
	      ;; else, this is the start of a new statement
	      ;; Position following last unclosed open.
	      (goto-char containing-sexp)
	      ;; Is line first statement after an open-brace?
	      (or
	       (and c++-relative-offset-p
		    ;; If no, find that first statement and
		    ;; indent like it.
		    (save-excursion
		      (forward-char 1)
		      (while
			  (progn
			    (skip-chars-forward " \t\n")
			    (looking-at
			     (concat
			      "#\\|/\\*\\|//"
			      "\\|\\(case[ \t]+.*\\|default[ \t]*\\)"
			      "\\|[a-zA-Z0-9_$]*:[^:]"
			      "\\|friend[ \t]"
			      c++-class-key
			      "[ \t]")))
			;; Skip over comments and labels
			;; following openbrace.
			(cond
			 ((= (following-char) ?\#)
			  (forward-line 1))
			 ((looking-at "/\\*")
			  (search-forward "*/" nil 'move))
			 ((looking-at
			   (concat "//\\|friend[ \t]" c++-class-key
				   "[ \t]"))
			  (forward-line 1))
			 ((looking-at "\\(case[ \t]+.*\\|default[ \t]*\\):")
			  (forward-line 1))
			 (t
			  (re-search-forward ":[^:]" nil 'move))))
		      ;; The first following code counts
		      ;; if it is before the line we want to indent.
		      (and (< (point) indent-point)
			   (+ (current-column)
			      ;; check if this is a true
			      ;; statement continuation, not a
			      ;; list of enums or static arrays elems
			      (if (and
				   (= char-before-ip ?,)
				   (c++-in-function-p containing-sexp))
				  c-indent-level 0)))))
	       ;; If no previous statement, indent it relative to
	       ;; line brace is on.  For open brace in column
	       ;; zero, don't let statement start there too.  If
	       ;; c-indent-offset is zero, use c-brace-offset +
	       ;; c-continued-statement-offset instead.  For
	       ;; open-braces not the first thing in a line, add
	       ;; in c-brace-imaginary-offset.
	       (+ (if (and (bolp) (zerop c-indent-level))
		      (+ c-brace-offset c-continued-statement-offset)
		    c-indent-level)
		  ;; Move back over whitespace before the openbrace.
		  ;; If openbrace is not first nonwhite thing on the line,
		  ;; add the c-brace-imaginary-offset.
		  (progn (skip-chars-backward " \t")
			 (if (bolp) 0 c-brace-imaginary-offset))
		  ;; If the openbrace is preceded by a parenthesized exp,
		  ;; move to the beginning of that;
		  ;; possibly a different line
		  (progn
		    (if (eq (preceding-char) ?\))
			(forward-sexp -1))
		    ;; Get initial indentation of the line we are on.
		    (current-indentation)))))))) ; end t outer cond
       ))))

(defun c++-calculate-c-indent-within-comment ()
  "Return the indentation amount for line, assuming that
the current line is to be regarded as part of a block comment."
  (let (end stars indent)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq stars (if (looking-at "\\*\\*?")
		      (- (match-end 0) (match-beginning 0))
		    0))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if (re-search-forward "/\\*[ \t]*" end t)
	  (goto-char (+ (match-beginning 0)
			(cond
			 (c++-C-block-comments-indent-p 0)
			 ((= stars 1) 1)
			 ((= stars 2) 0)
			 (t (- (match-end 0) (match-beginning 0)))))))
      (current-column))))

(defun c++-comment-offset (col0-line-p indent)
  "Calculates and returns the comment-only line offset.
Offset is based on the value of c++-comment-only-line-offset, the
argument COL0-LINE-P, and the current indentation INDENT."
  (let ((offset 0))
    (if col0-line-p
	;; col0 means we need to look at the second member of the var's
	;; list value. if value is not a list, then zero is used
	(if (listp c++-comment-only-line-offset)
	    ;; it is a list, so second element must be nil or a number
	    (setq offset
		  (+ indent
		     (or (car (cdr c++-comment-only-line-offset))
			 (car c++-comment-only-line-offset)))))
      ;; not in column zero so indentation is car or value of variable
      (setq offset
	    (+ indent
	       (if (listp c++-comment-only-line-offset)
		   (car c++-comment-only-line-offset)
		 c++-comment-only-line-offset))))
    offset))


;; ======================================================================
;; defuns to look backwards for things
;; ======================================================================

(defun c++-backward-syntactic-ws (&optional lim)
  "Skip backwards over syntactic whitespace.
Syntactic whitespace is defined as lexical whitespace, C and C++ style
comments, and preprocessor directives. Search no farther back than
optional LIM.  If LIM is ommitted, beginning-of-defun is used."
  (let ((lim (or lim (c++-point 'bod)))
	literal stop)
    (if (and c++-backscan-limit
	     (> (- (point) lim) c++-backscan-limit))
	(setq lim (- (point) c++-backscan-limit)))
    (while (not stop)
      (skip-chars-backward " \t\n\r\f" lim)
      ;; c++ comment
      (if (eq (setq literal (c++-in-literal lim)) 'c++)
	  (progn
	    (skip-chars-backward "^/" lim)
	    (skip-chars-backward "/" lim)
	    (while (not (or (and (= (following-char) ?/)
				 (= (char-after (1+ (point))) ?/))
			    (<= (point) lim)))
	      (skip-chars-backward "^/" lim)
	      (skip-chars-backward "/" lim)))
	;; c comment
	(if (eq literal 'c)
	    (progn
	      (skip-chars-backward "^*" lim)
	      (skip-chars-backward "*" lim)
	      (while (not (or (and (= (following-char) ?*)
				   (= (preceding-char) ?/))
			      (<= (point) lim)))
		(skip-chars-backward "^*" lim)
		(skip-chars-backward "*" lim))
	      (or (bobp) (forward-char -1)))
	  ;; preprocessor directive
	  (if (eq literal 'pound)
	      (progn
		(beginning-of-line)
		(setq stop (<= (point) lim)))
	    ;; just outside of c block
	    (if (and (= (preceding-char) ?/)
		     (= (char-after (- (point) 2)) ?*))
		(progn
		  (skip-chars-backward "^*" lim)
		  (skip-chars-backward "*" lim)
		  (while (not (or (and (= (following-char) ?*)
				       (= (preceding-char) ?/))
				  (<= (point) lim)))
		    (skip-chars-backward "^*" lim)
		    (skip-chars-backward "*" lim))
		  (or (bobp) (forward-char -1)))
	      ;; none of the above
	      (setq stop t))))))))

(defun c++-fast-backward-syntactic-ws (&optional lim)
  "Skip backwards over syntactic whitespace.
Syntactic whitespace is defined as lexical whitespace, C and C++ style
comments, and preprocessor directives. Search no farther back than
optional LIM.  If LIM is ommitted, beginning-of-defun is used."
  (save-restriction
    (let ((parse-sexp-ignore-comments t)
	  donep boi char
	  (lim (or lim (c++-point 'bod))))
      (if (< lim (point))
	  (unwind-protect
	      (progn
		(narrow-to-region lim (point))
		(modify-syntax-entry ?# "< b" c++-mode-syntax-table)
		(while (not donep)
		  ;; if you're not running a patched lemacs, the new byte
		  ;; compiler will complain about this function. ignore that
		  (backward-syntactic-ws)
		  (if (not (looking-at "#\\|/\\*\\|//\\|\n"))
		      (forward-char 1))
		  (setq boi (c++-point 'boi)
			char (char-after boi))
		  (if (and char (= char ?#))
		      (progn (goto-char boi)
			     (setq donep (<= (point) lim)))
		    (setq donep t))
		  ))
	    (modify-syntax-entry ?# "." c++-mode-syntax-table)))
      )))

(if c++-emacs-is-really-fixed-p
    (fset 'c++-backward-syntactic-ws
	  'c++-fast-backward-syntactic-ws))

(defun c++-backward-to-start-of-do (&optional limit)
  "Move to the start of the last ``unbalanced'' do."
  (let ((do-level 1)
	(case-fold-search nil)
	(limit (or limit (c++-point 'bod))))
    (while (not (zerop do-level))
      ;; we protect this call because trying to execute this when the
      ;; while is not associated with a do will throw an error
      (condition-case err
	  (progn
	    (backward-sexp 1)
	    (cond
	     ((memq (c++-in-literal limit) '(c c++)))
	     ((looking-at "while\\b")
	      (setq do-level (1+ do-level)))
	     ((looking-at "do\\b")
	      (setq do-level (1- do-level)))
	     ((< (point) limit)
	      (setq do-level 0)
	      (goto-char limit))))
	(error
	 (goto-char limit)
	 (setq do-level 0))))))

(defun c++-backward-to-start-of-if (&optional limit)
  "Move to the start of the last ``unbalanced'' if."
  (let ((if-level 1)
	(case-fold-search nil)
	(limit (or limit (c++-point 'bod))))
    (while (and (not (bobp))
		(not (zerop if-level)))
      (c++-backward-sexp 1)
      (cond ((looking-at "else\\b")
	     (setq if-level (1+ if-level)))
	    ((looking-at "if\\b")
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))

(defun c++-auto-newline ()
  "Insert a newline iff we're not in a literal.
Literals are defined as being inside a C or C++ style comment or open
string according to mode's syntax."
  (let ((bod (c++-point 'bod)))
    (and c++-auto-newline
	 (not (c++-in-literal bod))
	 (not (newline)))))

(defun c++-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:
  bol -- beginning of line
  eol -- end of line
  bod -- beginning of defun
  boi -- back to indentation
This function does not modify point or mark."
  (let ((here (point)) bufpos)
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'eol) (end-of-line))
     ((eq position 'bod) (c++-beginning-of-defun))
     ((eq position 'boi) (back-to-indentation))
     )
    (setq bufpos (point))
    (goto-char here)
    bufpos))


;; ======================================================================
;; defuns for "macroizations" -- making C++ parameterized types via macros
;; ======================================================================
(defun c++-macroize-region (from to arg)
  "Insert backslashes at end of every line in region.
Useful for defining cpp macros.  If called with a prefix argument,
it will remove trailing backslashes."
  (interactive "r\nP")
  (save-excursion
    (goto-char from)
    (beginning-of-line 1)
    (let ((line (count-lines (point-min) (point)))
	  (to-line (save-excursion (goto-char to)
				   (count-lines (point-min) (point)))))
      (while (< line to-line)
	(c++-backslashify-current-line (null arg))
	(forward-line 1) (setq line (1+ line))))))

(defun c++-backslashify-current-line (doit)
  "Backslashifies current line."
  (end-of-line 1)
  (cond
   (doit
    ;; Note that "\\\\" is needed to get one backslash.
    (if (not (save-excursion (forward-char -1) (looking-at "\\\\")))
	(progn
	  (if (>= (current-column) c++-default-macroize-column)
	      (insert " \\")
	    (while (<= (current-column) c++-default-macroize-column)
	      (insert "\t") (end-of-line))
	    (delete-char -1)
	    (while (< (current-column) c++-default-macroize-column)
	      (insert " ") (end-of-line))
	    (insert "\\")))))
   (t
    (forward-char -1)
    (if (looking-at "\\\\")
	(progn (skip-chars-backward " \t")
	       (kill-line))))))


;; ======================================================================
;; defuns for  commenting out multiple lines.
;; ======================================================================
(defun c++-comment-region (beg end)
  "Comment out all lines in a region between mark and current point by
inserting comment-start in front of each line."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region
       (progn (goto-char beg) (beginning-of-line) (point))
       (progn (goto-char end) (or (bolp) (forward-line 1)) (point)))
      (goto-char (point-min))
      (while (not (eobp))
	(insert comment-start)
	(forward-line 1))
      (if (eq major-mode 'c++-c-mode)
	  (insert comment-end)))))

(defun c++-uncomment-region (beg end)
  "Uncomment all lines in region between mark and current point by deleting
the leading \"// \" from each line, if any."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region
       (progn (goto-char beg) (beginning-of-line) (point))
       (progn (goto-char end) (forward-line 1) (point)))
      (goto-char (point-min))
      (let ((comment-regexp
	     (if (eq major-mode 'c++-c-mode)
		 (concat "\\s *\\(" (regexp-quote comment-start)
			 "\\|"      (regexp-quote comment-end)
			 "\\)")
	       (concat "\\s *" (regexp-quote comment-start)))))
	(while (not (eobp))
	  (if (looking-at comment-regexp)
	      (delete-region (match-beginning 0) (match-end 0)))
	  (forward-line 1))))))


;; ======================================================================
;; grammar parsing
;; ======================================================================

;;; Below are two regular expressions that attempt to match defuns
;;; "strongly" and "weakly."  The strong one almost reconstructs the
;;; grammar of C++; the weak one just figures anything id or curly on
;;; the left begins a defun.  The constant "c++-match-header-strongly"
;;; determines which to use; the default is the weak one.

(defvar c++-match-header-strongly nil
  "*If NIL, use c++-defun-header-weak to identify beginning of definitions,
if nonNIL, use c++-defun-header-strong")

(defvar c++-defun-header-strong-struct-equivs
  "\\(class\\|struct\\|union\\|enum\\)"
  "Regexp to match names of structure declaration blocks in C++")

(defconst c++-defun-header-strong
  (let*
      (; valid identifiers
       ;; There's a real wierdness here -- if I switch the below
       (id "\\(\\w\\|_\\)+")
       ;; to be
       ;; (id "\\(_\\|\\w\\)+")
       ;; things no longer work right.  Try it and see!

       ; overloadable operators
       (op-sym1
	 "[---+*/%^&|~!=<>]\\|[---+*/%^&|<>=!]=\\|<<=?\\|>>=?")
       (op-sym2
	 "&&\\|||\\|\\+\\+\\|--\\|()\\|\\[\\]")	 
       (op-sym (concat "\\(" op-sym1 "\\|" op-sym2 "\\)"))
       ; whitespace
       (middle "[^\\*]*\\(\\*+[^/\\*][^\\*]*\\)*")
       (c-comment (concat "/\\*" middle "\\*+/"))
       (wh (concat "\\(\\s \\|\n\\|//.*$\\|" c-comment "\\)"))
       (wh-opt (concat wh "*"))
       (wh-nec (concat wh "+"))
       (oper (concat "\\(" "operator" "\\("
		     wh-opt op-sym "\\|" wh-nec id "\\)" "\\)"))
       (dcl-list "([^():]*)")
       (func-name (concat "\\(" oper "\\|" id "::" id "\\|" id "\\)"))
       (inits
	 (concat "\\(:"
		 "\\(" wh-opt id "(.*\\()" wh-opt "," "\\)\\)*"
		 wh-opt id "(.*)" wh-opt "{"
		 "\\|" wh-opt "{\\)"))
       (type-name (concat
		    "\\(" c++-defun-header-strong-struct-equivs wh-nec "\\)?"
		    id))
       (type (concat "\\(const" wh-nec "\\)?"
		     "\\(" type-name "\\|" type-name wh-opt "\\*+" "\\|"
		     type-name wh-opt "&" "\\)"))
       (modifier "\\(inline\\|virtual\\|overload\\|auto\\|static\\)")
       (modifiers (concat "\\(" modifier wh-nec "\\)*"))
       (func-header
	 ;;     type               arg-dcl
	 (concat modifiers type wh-nec func-name wh-opt dcl-list wh-opt inits))
       (inherit (concat "\\(:" wh-opt "\\(public\\|protected\\|private\\)?"
			wh-nec id "\\)"))
       (cs-header (concat
		    c++-defun-header-strong-struct-equivs
		    wh-nec id wh-opt inherit "?" wh-opt "{")))
    (concat "^\\(" func-header "\\|" cs-header "\\)"))
  "Strongly-defined regexp to match beginning of structure or
function definition.")


;; This part has to do with recognizing defuns.

;; The weak convention we will use is that a defun begins any time
;; there is a left curly brace, or some identifier on the left margin,
;; followed by a left curly somewhere on the line.  (This will also
;; incorrectly match some continued strings, but this is after all
;; just a weak heuristic.)  Suggestions for improvement (short of the
;; strong scheme shown above) are welcomed.

(defconst c++-defun-header-weak "^{\\|^[_a-zA-Z].*{"
  "Weakly-defined regexp to match beginning of structure or function definition.")


(defun c++-beginning-of-defun (&optional arg)
  (interactive "p")
  (if (not arg) (setq arg 1))
  (let ((c++-defun-header (if c++-match-header-strongly
			      c++-defun-header-strong
			    c++-defun-header-weak)))
    (cond
     ((or (= arg 0) (and (> arg 0) (bobp))) nil)
     ((and (not (looking-at c++-defun-header))
	   (let ((curr-pos (point))
		 (open-pos (if (search-forward "{" nil 'move)
			       (point)))
		 (beg-pos
		  (if (re-search-backward c++-defun-header nil 'move)
		      (match-beginning 0))))
	     (if (and open-pos beg-pos
		      (< beg-pos curr-pos)
		      (> open-pos curr-pos))
		 (progn
		   (goto-char beg-pos)
		   (if (= arg 1) t nil));; Are we done?
	       (goto-char curr-pos)
	       nil))))
     (t
      (if (and (looking-at c++-defun-header) (not (bobp)))
	  (forward-char (if (< arg 0) 1 -1)))
      (and (re-search-backward c++-defun-header nil 'move (or arg 1))
	   (goto-char (match-beginning 0)))))))


(defun c++-end-of-defun (arg)
  (interactive "p")
  (let ((c++-defun-header (if c++-match-header-strongly
			      c++-defun-header-strong
			    c++-defun-header-weak))
	(parse-sexp-ignore-comments t))
    (if (and (eobp) (> arg 0))
	nil
      (if (and (> arg 0) (looking-at c++-defun-header)) (forward-char 1))
      (let ((pos (point)))
	(c++-beginning-of-defun 
	  (if (< arg 0)
	      (- (- arg (if (eobp) 0 1)))
	    arg))
	(if (and (< arg 0) (bobp))
	    t
	  (if (re-search-forward c++-defun-header nil 'move)
	      (progn (forward-char -1)
		     (forward-sexp)
		     (beginning-of-line 2)))
	  (if (and (= pos (point)) 
		   (re-search-forward c++-defun-header nil 'move))
	      (c++-end-of-defun 1))))
      t)))

(defun c++-indent-defun ()
  "Indents the current function def, struct or class decl."
  (interactive)
  (let ((restore (point)))
    (c++-end-of-defun 1)
    (beginning-of-line 1)
    (let ((end (point-marker)))
      (c++-beginning-of-defun)
      (while (and (< (point) end))
	(c++-indent-line)
	(forward-line 1)
	(beginning-of-line 1))
      (set-marker end nil))
    (goto-char restore)))


;; ======================================================================
;; defuns for submitting bug reports
;; ======================================================================
(defconst c++-version "Revision: 2.302 "
  "c++-mode version number.")

(defun c++-version ()
  "Echo the current version of c++-mode."
  (interactive)
  (message "Using c++-mode.el %s" c++-version))

(defun c++-dump-state (mode)
  "Inserts into the c++-mode-state-buffer the current state of
c++-mode into the bug report mail buffer. MODE is either c++-mode or
c++-c-mode.

Use \\[c++-submit-bug-report] to submit a bug report."
  (let ((buffer (current-buffer))
	(varlist (list
		  'c++-C-block-comments-indent-p
		  'c++-access-specifier-offset
		  'c++-always-arglist-indent-p
		  'c++-auto-hungry-initial-state
		  'c++-auto-hungry-toggle
		  'c++-auto-newline
		  'c++-backscan-limit
		  'c++-block-close-brace-offset
		  'c++-cleanup-list
		  'c++-comment-only-line-offset
		  'c++-continued-member-init-offset
		  'c++-default-macroize-column
		  'c++-defun-header-strong-struct-equivs
		  'c++-delete-function
		  'c++-electric-pound-behavior
		  'c++-empty-arglist-indent
		  'c++-friend-offset
		  'c++-hanging-braces
		  'c++-hanging-member-init-colon
		  'c++-hungry-delete-key
		  'c++-match-header-strongly
		  'c++-member-init-indent
		  'c++-paren-as-block-close-p
		  'c++-relative-offset-p
		  'c++-tab-always-indent
		  'c++-untame-characters
		  'c-argdecl-indent
		  'c-brace-imaginary-offset
		  'c-brace-offset
		  'c-continued-brace-offset
		  'c-continued-statement-offset
		  'c-indent-level
		  'c-label-offset
		  'tab-width
		  )))
    (set-buffer buffer)
    (if c++-special-indent-hook
	(insert "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
		"c++-special-indent-hook is set to '"
		(format "%s" c++-special-indent-hook)
		".\nPerhaps this is your problem?\n"
		"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n"))
    (insert (emacs-version) "\n")
    (insert "c++-mode.el " c++-version " (editing "
	    (if (eq mode 'c++-mode) "C++" "C")
	    " code) \n"
	    (if c++-emacs-is-fixed-p
		"You've applied the (hopefully most recent) syntax patch!\n"
	      "No syntax patch applied.\n")
	    (if c++-emacs-is-really-fixed-p
		"Looks like you've also got the parse-back patch. Good!\n"
	      "No parse-back patch applied.\n")
	    "\ncurrent state:\n==============\n(setq\n")
    (mapcar
     (function
      (lambda (varsym)
	(let ((val (eval varsym))
	      (sym (symbol-name varsym)))
	  (insert "     " sym " "
		  (if (or (listp val) (symbolp val)) "'" "")
		  (prin1-to-string val)
		  "\n"))))
     varlist)
    (insert "     )\n")
    ))

(defun c++-submit-bug-report ()
  "Submit via mail a bug report using the mailer in c++-mailer."
  (interactive)
  (let* ((curbuf (current-buffer))
         (mode   major-mode)
         (mailbuf (progn (call-interactively c++-mailer)
                         (current-buffer))))
    (require 'sendmail)
    (pop-to-buffer curbuf)
    (pop-to-buffer mailbuf)
    ;; different mailers use different separators, some may not even
    ;; use m-h-s, but sendmail.el stuff must have m-h-s bound.
    (let ((mail-header-separator
           (save-excursion
             (re-search-forward
              (concat
               "^\\("			;beginning of line
               (mapconcat
                'identity
                (list "[        ]*"     ;simple SMTP form
                      "-+"              ;mh-e form
                      mail-header-separator) ;sendmail.el form
                "\\|")			;or them together
               "\\)$")			;end of line
              nil
              'move)			;search for and move
             (buffer-substring (match-beginning 0) (match-end 0)))))
      (mail-position-on-field "to")
      (insert c++-mode-help-address)
      (mail-position-on-field "subject")
      (insert "Bug in c++-mode.el " c++-version)
      (re-search-forward mail-header-separator (point-max) 'move)
      (forward-line 1)
      (set-mark (point))                ;user should see mark change
      (insert "\n\n")
      (c++-dump-state mode)
      (exchange-point-and-mark))))


;; this is sometimes useful
(provide 'c++-mode)

;;; c++-mode.el ends here

