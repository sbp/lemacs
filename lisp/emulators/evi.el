;; Copyright (c) 1991 Jeffrey R. Lewis
;; All rights reserved.
;; Redistribution and use is permitted provided that this entire copyright
;; notice and comment is retained.
;; THIS SOFTWARE IS SUPPLIED `AS IS', AND WITHOUT ANY EXPRESS OR IMPLIED
;; WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.

;; Evi 0.93 - Emulate Vi
;; Local modifications for version 19 - eb Wed Sep 11 10:37:44 1991

;; Here follows Evi 0.9, an even better vi emulator for serious vi users.
;; Evi's first goal is vi compatibility.  Its second goal is to be an
;; extension of vi, taking advantage of features supplied by emacs, without
;; simply becoming emacs with vi'ish key bindings.  In other words, Evi is
;; for the vi user who never really intends to eventually `go emacs'
;; (or, as it turns out, for those of us who must regularly use vi anyway
;; because our frumpy little unix boxen aren't always in the mood to
;; run emacs).  You shouldn't need any special manual to start using Evi,
;; other than your vi manual, and the documentation on extensions and
;; differences in this file.

;; Starting up Evi

;; To just test Evi out, do (don't actually type the spaces):
;;	M-x evi <RET>
;; You will now be in the emulator.  To make emacs always load and fire up
;; Evi, put the following at the end of your .emacs file:  (Note: this assumes
;; evi is installed in the Emacs library directory and autoloadable).
;;	(setq term-setup-hook 'evi)

;; Release Note:

;; At this point Evi is still `beta' software - it is also not yet
;; a `full' vi emulator, but it is much closer than vip 4.2.  I am mainly
;; releasing this version to get some feedback on how well it's doing and
;; whether the approaches I've taken will work well.  I'm also getting tired
;; of staring at it... ;-)
;; You'll probably want to byte-compile it before use to expand the numerous
;; macros used in Evi.  Using the newer byte compiler just posted may give
;; even better results because there are quite a few top level macros, which
;; are not expanded by the old byte compiler.
;; Unfortunately, sometime between version 18.54 and 18.57, some improvements
;; were made to emacs that Evi relies on.  I haven't consulted the ChangeLog
;; to determine exactly what these are, but it has been noticed that Evi
;; won't byte-compile under 18.54 (an apparent bug in the old byte-compiler),
;; and that the variable `buffer-undo-list' that Evi relies on to make undo
;; behave in a vi-bufflike manner is not present in 18.54.  My best recommendation
;; if you have an older emacs is to upgrade.  My apologies if that's not
;; convenient.

;; File and window management

;; Vi's file management commands have long tried to mimic having multiple
;; buffers, and as such, work well enough to use in an emacs setting.  They
;; of course have to take on slightly different meanings, since it makes
;; little sense to emulate the limitations of the vi/ex model that presumably
;; you are trying to avoid by using evi!
;;
;;	:e	Edit a file in the current window.  With no argument, brings
;;		in a new copy of the file, if it has been subsequently
;;		modified on disk.  `:e!' will override any complaints about
;;		the current buffer being modified, and discard all
;;		modifications.  With a filename argument, edits that file
;;		in the current window (using the copy already in the editor
;;		if it was previously read in).  I'm not sure if `:e! filename'
;;		should have a separate meaning from `:e filename' as in evi we
;;		don't need to worry about the disposition of the current
;;		file before editing the next one.  Opinions are welcome -
;;		currently there's no difference.  `:e#' is shorthand for
;;		edit the most recently accessed buffer not in a window.
;;	:E	Same as `:e', but edits the file in the other window, creating
;;		that window if necessary.  With no filename, splits the
;;		current buffer into two windows.
;;	:n	Switch to the next file in buffer list that's not currently
;;		displayed.  Rotates the current file to the end of the buffer
;;		list, so this will effectively cycle thru all buffers.
;;	:N	Same as `:n', but switches the other window, or creates an
;;		other window and puts the next file in it.
;;
;; Vi also has a key `z' for doing (minimal) window management.  The following
;; are extensions to this command.
;;
;;	zH
;;	zM
;;	zL	These are aliases for z<CR>, z., and z- and correspond to
;;		the arguments to the mark command (this is from vip).
;;	z<num>+
;;	z<num>-	These let you adjust the size of the current window by <num>.
;;		Use z<num>. to adjust the window size absolutely.
;;	z1=	Make the current window fill the screen.
;;	z2=	Split the current window in two.

;; Good news for file completion fans: the ex commands which accept filenames
;; as arguments can be file completed using space or tab.  Filename
;; completion is turned on after the space that separates the command
;; from the filename.  Try it.

;; If you do have some familiarity with emacs C-x prefix commands, you may
;; prefer to use them, as they are somewhat more flexible.  To do so, simply
;; map C-x to ctl-x-map as follows:
;;	(evi-define-key evi-all-keymaps "\C-x" ctl-x-map)
;; and use your C-x prefix commands as you normally would.
;; Also, you may want to access emacs' Meta prefix commands, normally accessed
;; via <ESC>.  I recommend using C-a for this purpose.
;;	(setq evi-meta-prefix-char ?\C-a)
;;	(evi-define-key evi-all-keymaps "\C-a" esc-map)
;; (meta-prefix-char is locally set to evi-meta-prefix-char in all evi buffers;
;; changing meta-prefix-char directly would mess up non-evi buffers.)
;;
;; You could of course map <ESC> to its original binding, although this has
;; the drawback of being incompatible with its use in exiting insert and
;; replace modes.  One incompatibility that's not easily fixed is how
;; vi recognizes arrow keys that send <ESC> prefixed sequences:  vi uses
;; a very short timeout to determine the difference between an <ESC> typed
;; by a person, and an <ESC> sequence sent by the terminal.  A compromise,
;; if you happen to have such a terminal and like your arrow keys a lot,
;; is to `:set notimeout', which makes <ESC><ESC> behave like a single <ESC>
;; and maps your arrow keys to `h', `j', `k', and `l'.  As a side effect
;; in Evi, the normal emacs <ESC> prefix commands are in effect (except for
;; <ESC><ESC> of course).  I'd be interested to know if the side effect
;; is desirable (it may cause some unexpected behaviour).

;; Emacs lisp startup code for evi can be placed in either ~/.evirc or .evirc.

;; New operators

;; I'm experimenting with some new complex operators.  I'm particularly
;; interested in your thoughts on these:
;;
;; `[{' operates over lines in a region.  It takes a motion, and a sequence
;; of operations to perform on each line in the region defined by the motion.
;; The sequence of operations is prompted for on the bottom line.  Double the
;; `{' to operate on whole lines.  The point starts in the first column for
;; each line operated on.
;; E.g.
;;	[{}i> C-v<ESC><RET>
;;	  ^ motion - forward paragraph
;;	   ^ sequence of operations - terminated with a <RET> or <ESC>
;; would prefix every line in the rest of the current paragraph with `> '.
;; The `C-v', `<ESC>' and `<RET>' are of course the single characters
;; generated by control-v, the escape key and the return key respectively.
;; The `C-v<ESC>' sequence inserts an <ESC> into the string you are entering
;; so that it will terminate input when the loop body is executed, not as you
;; are entering the command.
;; E.g.
;;	10[{{i/* C-v<ESC>A */C-v<ESC><RET>
;; would place C-style comments around the next 10 lines.
;; ZZ Oops, that particular example is temporarily out of order!
;;
;; `[(' defines a parameterized macro body.  A parameterized macro is diff-
;; erent from standard macro text in that it is parameterized by prefix count
;; and register specification.  `C-_' is a prefix for several keys useful in
;; defining parameterized macros.  `C-_#' is the prefix count applied to this
;; macro, and `C-_"' is the register specification applied to this macro.  All
;; non-macro modification commands can be specified by prefixing them with
;; `C-_'. This is important because the standard command keymap can be
;; changed, and you may want to specify a command macro such that its meaning
;; will be the same regardless of any customizations.  `All non-macro' is
;; specified because several standard modification commands are actually
;; macros themselves.  For example, `i' is the only non-macro insert command:
;; `I', `A', `o', `O', etc are all macros.
;; E.g.
;;	"a8[(jC-_#wC-_"dw<RET>
;; would go down one line, move over 8 words, then delete the next word into
;; register `a'.  This is rather contrived, but it gives you the idea.  Param-
;; eterized macro bodies are obviously not very useful typed out each time,
;; and are intended to be the body of a map macro.
;; E.g.
;;	:map M [(jC-_#wC-_"dwC-v<ESC><RET>
;;	"a8M
;; would be a much more likely scenario for the use of such a macro.

;; Various enhancements
;; If you think of any of these as more incompatibilities than enhancements,
;; please let me know.
;;
;; `_' is a new version of the repeat command `.' that prompts you with
;; the keystrokes to repeat, allowing you to edit them before executing.
;; This is particularly useful for the abovementioned complex operators.
;; Right now, commands like `a', `I', etc which are input commands implemented
;; with a macro will be expanded in the repeat history, so they'll look
;; a bit strange but will do the right thing.
;;
;; `C-^', which is supposed to be an alias for `:e#', instead circulates
;; thru the windows on the screen, switching to the most recently accessed
;; other buffer is there's only one window.
;;
;; `C-d' and `C-u' preserve the goal column (like `j', `k', `C-e' and `C-y' do)
;;
;; `/' takes a prefix count to find the nth occurence of a string
;;
;; `C-g' gives the column position (it's 0 based, whereas the line number is
;; 1 based - opinions on this are welcome)
;;
;; Arbitrary regions can be operated on via `m.' (mark current position) and
;; then using the motion `r' or `R' as a motion operand.  The region operated
;; on is bound by the mark and the cursor position at the time the operator
;; was invoked.  `R' extends the region to whole lines.  Thus, for example,
;; the sequence:
;;	m.3j5wdr
;; would delete the text from where the cursor started to 3 lines down and
;; 5 words over.  `R' is often handy for operating on large arbitrary sections
;; of text, for example say you needed to shift some text that ran on for
;; several pages and you weren't sure just how long it was at the start:
;;	m.C-fC-fC-fjjj>R
;; (this idea, of course, comes straight from vip, and emacs users have
;; been using arbitrary regions for years)
;;
;; `U' continues a previous undo, undoing more changes.  Thus a long enough
;; sequence of U's will take you back to the unmodified state.  If you went
;; back too far, a `u' will reverse this process and you can progress forward
;; in changes using `U'.  In vi U normally means `undo all changes on this
;; line'.  It seems reasonable to override this meaning since `continue undo'
;; can do that and more, and doesn't do more than the original command.
;; #### change by jwz: by popular demand, I have bound U to evi-undo-all, 
;;      which undoes all changes on the line; bind U to evi-undo-more if you
;;      want the above-described behavior.
;;
;; The marks used by the mark command `m' are emacs markers, thus they
;; mark a position in a buffer, not necessarily the current one.  This
;; affects the goto mark commands ``' and `''.  For example, if mark
;; `a' is placed in one buffer, then later in another buffer, the command
;; ``a' is typed, evi will first switch to that buffer, then go to the
;; location in that buffer.
;; `'' and ``' also accept `.' and `,' for pop context, and unpop context
;; respectively.  Thus, `'.' will take you to the previous context
;; (defined as in vi by a region of relative motion, with an `absolute'
;; motion pushing a new context.  quotes surround `absolute' because a
;; search is considered an absolute motion for this purpose), and `'.'
;; will take you to the context before that.  There is a ring of 10 contexts
;; so after 10 `'.' commands you'll end up at the original previous
;; context.  `Unpop context' means move forward thru the ring.  `''' and
;; ```' are defined as exchange current location with the location of the
;; previous context.  The context ring is buffer local, so use of it will
;; always keep you in the same buffer.
;;
;; `%' exhibits language sensitivity in that it ignores parentheses embedded
;; in quotes.  What defines quotes is based on what minor mode emacs is in
;; (such as c-mode or lisp-mode), or you can roll your own (see emacs
;; command modify-syntax-entry).
;;
;; `=' is no longer specific to :set lisp.  It indents according to the
;; mode.  See emacs command indent-according-to-mode.

;; Differences
;;
;; These I haven't gotten around to, would be too much bother to implement,
;; would require emacs C source modifications to do sensibly, or are simply
;; unnecessary ;; in an emacs environment.  I'll let you guess which is
;; which ;-)
;;
;; `C-@' in insert mode
;; `C-q'
;; `C-[' doesn't always do the right thing, and it doesn't do the
;;       timeout trick necessary to allow it to recognize keypad sequences
;; `#'
;; `&'
;; `/' search offsets
;; `Q'
;; `i' repeat counts
;;
;; digit registers don't work entirely correctly - there are circumstances
;; in which separate lines of a change/deletion are supposed to go into
;; separate registers
;;
;; `:set lisp' has no effect, however, emacs does largely take care of
;; any lisp'ish behaviour you'd want automatically if the file you're
;; editing is suffixed with `.l' or `.el'.  One particular loss, however,
;; is that `)' and `(' don't work on s-expressions like they would in vi
;; with lisp set.
;;
;; :set number
;;
;; actually a good number of the ex commands and options aren't implemented
;; now.  A more meaningful list would be those that are implemented:
;;	cd, chdir, copy, delete, edit, file, global, map, move, next, print,
;;	put, quit, read, set, source, substitute, tag, write, wq, yank, !, <, >
;; These are the options that are implemented:
;;	autoindent, ignorecase, magic, notimeout, shiftwidth, showmatch,
;;	tabstop, wrapscan

;; Note to vip users:
;;
;; Undo does not continue via `.'.  This is incompatible with vi - the sequence
;; `u.' in vi means `undo, then do again', whereas in vip it means `undo,
;; then undo some more.'  For the vip functionality use `U' (see above).
;;
;; There are actually a large number of functional differences between vip
;; and evi.  Most of this has to do with evi's goal of being as much like
;; vi as possible (for better or for worse, I tried not to make too many
;; judgement calls, so that the user could decide).


;; Version 18 vs. Lucid Emacs compatibility:
(defvar evi-new-event-model-p (fboundp 'event-to-character))

(cond ((not evi-new-event-model-p)
       (fset 'character-to-event (function (lambda (char event) char)))
       (fset 'allocate-event (function (lambda () nil)))))

(defun evi-unread-char (char)
  (if evi-new-event-model-p
      (setq unread-command-event
	    (if (null char)
		last-command-event
	      (character-to-event char (allocate-event))))
    (setq unread-command-char
	  (if (null char)
	      last-command-char
	    char))))

(defun evi-read-char ()
  (let ((event (allocate-event)))
    (next-command-event event)
    (let ((char (event-to-character event nil)))
      ;; This is a bit of a kludge.  Any non-
      ;; character event gets unread and an escape is returned instead.
      ;; So effectively any non-character event is just like escape
      ;; followed by the event, causing insert mode to be exited and
      ;; the event to be handled in regular vi mode.
      (cond ((null char)
	     (setq unread-command-event event)
	     ?\e)
	    (t (deallocate-event event)
	       char)))))


(defun evi-event-to-character (event)
  (if evi-new-event-model-p
      ;; This is just for compatibility with evi-read-char
      (or (event-to-character event) ?\e)
    event))


(defmacro defbuffervar (name default-value documentation)
  (list 'progn (list 'defvar name nil documentation)
	       (list 'make-variable-buffer-local (list 'quote name))
	       (list 'set-default (list 'quote name) default-value)))

(defbuffervar evi-mode 'vi
  "Current vi mode, one of vi, insert or replace.")

(defvar evi-command-keys nil
  "The keystrokes for the current modifying command.")

(defvar evi-meta-prefix-char ?\C-a	; -1 to disable
  "meta-prefix-char is locally set to this in all EVI buffers")

(defbuffervar evi-replace-max nil
  "Maximum excursion of a replace, after which it switches to insert.")

(defbuffervar evi-overstruck-char nil
  "Value of the character overstruck by the `$' marking a partial line change.")

(defbuffervar evi-context nil
  "Current motion context.  One of to-end, to-next, whole-line, or nil.
The value of this variable is passed to evi-motion-command, and is set by
prefix operators like 'd' or '>' to control the type of region defined by
the following motion command.")

(defbuffervar evi-prefix-count nil
  "Current prefix count.  \(buffer specific\)")

(defbuffervar evi-prefix-count-multiplier 1
  "Current prefix count multiplier.  \(buffer specific\)")

(defbuffervar evi-register nil
  "Current register to use for deletes, yanks, puts, etc.  \(buffer specific\)")

(defvar evi-digit-register 0
  "Current delete-ring register cursor.  Points to the register that
will be register 1.")

(defvar evi-last-macro-register nil
  "Last register used to invoke a macro via \\[evi-register-macro].")

(defvar evi-registers (make-vector 72 nil)
  "Vi registers.  0-8 are the delete ring, 9 is the unnamed text register,
10-35 are the alphabetic text registers, and 36-71 are the mark registers.
Each text register is a cons cell with the car being the text in the register
and the cdr being a flag indicating whether or not the text is whole lines.")

(defvar evi-register-unnamed 9
  "Symbolic name for the unnamed register.  Shouldn't change.")

(defbuffervar evi-region-whole-lines nil
  "If t, the current region specified by a motion as an operand encompasses
whole lines.  This is set by evi-motion-command if either the motion is
a vertical one, it is a horizontal motion that covers more than one line
or the operator command requires it ('>' for example only operates on
whole lines).  The value of this variable is stored in the cdr of any
register that gets stored as a result of the current command.  \(buffer
specific\)")

(defbuffervar evi-current-indentation 0
  "The indentation of the most recently auto-indented line.  Used by
evi-newline-and-indent to determine when to kill auto-indented whitespace.
\(buffer specific\)")

(defvar evi-internal-command nil
  "If t, next command will be executed in internal mode (certain interface
features turned off)")

(defvar evi-scroll-count nil
  "The last specified number of lines to scroll.")

(defbuffervar evi-goal-column 0
  "The column that vertical cursor motion will try to preserve, if possible.")

(defbuffervar evi-reset-goal-column t
  "If t, a horizontal motion has been performed, thus goal column must be reset.")

(defvar evi-search-pattern nil
  "The last pattern specified for searching.")

(defvar evi-search-forward t
  "If t, the last search command was a forward search.")

(defvar evi-find-character nil
  "The last character specified for finding.")

(defvar evi-find-forward t
  "If t, the last find command was a forward search.")

(defvar evi-find-up-to nil
  "If t, the last find command was a find up to command.")

(defbuffervar evi-context-ring (make-vector 10 nil)
  "The last 10 contexts for this buffer.  A context is a location in the buffer
where only relative motions were performed.  A new context is thus saved each
time a non-relative motion is performed.")

(defbuffervar evi-context-ring-cursor 0
  "The cursor pointing to the last context in the context ring.")

(defvar ex-work-space
  (let ((b (get-buffer-create " *ex-work-space*")))
    (if (boundp 'zmacs-regions)
	(save-excursion
	  (set-buffer b)
	  (set (make-local-variable 'zmacs-regions) nil)))
    b)
  "Evi work space for parsing ex commands.")

(defvar ex-find-file-shell "/bin/csh")

(defvar ex-tag nil
  "Last tag specified.")

; If you think the use of keymaps here has gotten a little out of hand,
; you're probably right...

(defun evi-make-empty-keymap ()
  "Makes a keymap and shadows everything that is in global map."
  (let ((map (make-keymap)))
    (if (vectorp map)
	(fillarray map 'undefined)
      ; Lucid Emacs
      (map-keymap (current-global-map)
		  (function (lambda (key value)
			      (define-key map key 'undefined)))))
    map))

; ZZ - maybe should be buffer local after it's initialized?
(defvar evi-vi-map (evi-make-empty-keymap)
  "The keymap used in vi mode.")

(defvar evi-vi-local-map nil
  "The local keymap used in evi-get-commmand.")

(defvar evi-internal-map (evi-make-empty-keymap)
  "A subkeymap of vi-map, used to hard-code standard modification operations
for use in defining command macros.")

(defvar evi-motion-map (evi-make-empty-keymap)
  "The keymap used for operand motions.")

(defvar evi-insert-map (let ((map (evi-make-empty-keymap))
			     (i 128))
			 (while (<= 0 (setq i (1- i)))
			   (define-key map (make-string 1 i)
			     'self-insert-command))
			 map)
  "The keymap used in insert mode.")

(defvar evi-replace-map (let ((map (evi-make-empty-keymap))
			     (i 128))
			 (while (<= 0 (setq i (1- i)))
			   (define-key map (make-string 1 i)
			     'evi-self-replace))
			 map)
  "The keymap used in replace mode.")

(defvar evi-minibuffer-map (copy-keymap evi-insert-map)
  "The keymap used when reading from the minibuffer.")

(defvar evi-minibuffer-completion-map (copy-keymap evi-insert-map)
  "The keymap used when reading with completion from the minibuffer.")

(defvar evi-minibuffer-must-match-map (copy-keymap evi-insert-map)
  "The keymap used when reading with must match completion from the minibuffer.")

(defvar evi-minibuffer-no-space-map (copy-keymap evi-insert-map)
  "The keymap used when reading from the minibuffer with no spaces.")

(defvar evi-ex-map (copy-keymap evi-insert-map)
  "The keymap used when reading ex commands from the minibuffer")

(defconst evi-all-input-maps
  '(insert replace minibuffer minibuffer-completion
	   minibuffer-must-match minibuffer-no-space ex)
  "All Evi keymaps associated with input.")

(defconst evi-all-keymaps
  '(vi insert replace minibuffer minibuffer-completion
       minibuffer-must-match minibuffer-no-space ex)
  "All Evi keymaps.")

(defconst evi-all-keymaps-but-insert
  (delq 'insert (copy-sequence evi-all-keymaps)))

(defvar evi-get-command-depth 0
  "Current nesting depth of evi-get-command's.")

;; ZZ should rewrite with catch/throw, or whatever...
(defvar evi-signal-abort nil
  "If t, abort the current command.")

(defbuffervar evi-register-parameter nil
  "Register specification to the current parameterized macro.")

(defbuffervar evi-prefix-count-parameter nil
  "Prefix count to the current parameterized macro.")

(defbuffervar evi-get-commands nil
  "If t, currently accepting commands from within evi-get-commands.")

(defvar evi-last-command-keys nil
  "Command keys for the last complete vi command.")

(defbuffervar evi-insert-point nil
  "The point at which the current insert command began.")

(defvar evi-error-string nil
  "If non-nil, the current command has encountered a non serious error.
This string will be presented to the user upon completion.")

;; Vi option variables
;; ZZ - could/should make some of these buffer local after reading EXINIT

(defconst evi-option-list
  '((("autoindent" "ai") . (bool . evi-auto-indent))
    (("autoprint" "ap") . (bool . nil))
    (("autowrite" "aw") . (bool . nil))
    (("beautify") . (bool . nil))
    (("directory" "dir") . (string . nil))
    (("edcompatible" "ed") . (bool . nil))
    (("errorbells" "eb") . (bool . nil))
    (("flash") . (bool . nil))
    (("hardtabs" "ht") . (number . nil))
    (("ignorecase" "ic") . (bool . evi-ignore-case))
    (("lisp") . (bool . nil))
    (("list") . (bool . nil))
    (("magic") . (bool . evi-search-magic))
    (("mesg") . (bool . nil))
    (("modeline") . (bool . nil))
    (("novice") . (bool . nil))
    (("number" "nu") . (bool . nil))
    (("optimize" "opt") . (bool . nil))
    (("paragraphs" "para") . (string . nil))
    (("prompt") . (bool . nil))
    (("readonly" "ro") . (bool . nil))
    (("redraw") . (bool . nil))
    (("remap") . (bool . nil))
    (("report") . (number . nil))
    (("redraw" "re") . (bool . nil))
    (("scroll") . (number . nil))
    (("sections" "sect") . (string . nil))
    (("shell") . (string . nil))
    (("shiftwidth" "sw") . (number . evi-shift-width))
    (("showmatch" "sm") . (bool . blink-matching-paren))
    (("slowopen" "slow") . (bool . nil))
    (("sourceany") . (bool . nil))
    (("tabstop" "ts") . (number . tab-width))
    (("tags") . (string . nil))
    (("taglength" "tl") . (number . nil))
    (("term") . (string . nil))
    (("terse") . (bool . nil))
    (("timeout") . (bool . evi-timeout))
    (("ttytype" "tty") . (string . nil))
    (("warn") . (bool . nil))
    (("wrapmargin" "wm") . (bool . nil))
    (("wrapscan" "ws") . (bool . evi-search-wraparound))
    (("writeany" "wa") . (bool . nil))))

(defconst evi-auto-indent nil
  "*If t, automatically indents text inserted on a new line.")

(defconst evi-ignore-case nil
  "*If t, ignore case in searches.")

(defconst evi-search-magic t
  "*If t, search patterns are normal regular expressions.  This is the default.
Otherwise, the `magic' characters `.' `[' and `*' are treated as literals and
must be escaped to get their regular expression interpretation.")

(defconst evi-shift-width 8
  "*The number of colums shifted by > and < command, and ^T and ^D
in insert mode.")

(defconst evi-timeout t
  "*If t, timeout is actually *not* implemented.  If nil, <ESC><ESC> becomes
<ESC>, and arrows keys are mapped to h, j, k and l.")

(defun evi-timeout (value)
  (if value
    (evi-define-key '(vi) "\e" 'nil)
    (progn (evi-define-key '(vi) "\e" esc-map)
	   (define-key function-keymap "l" 'evi-backward-char)
	   (define-key function-keymap "r" 'evi-forward-char)
	   (define-key function-keymap "u" 'evi-previous-line)
	   (define-key function-keymap "d" 'evi-next-line)
	   ;; ZZ should save \e\e binding and use that in :set timeout
	   (evi-define-key '(vi) "\e\e" 'nil))))

(defconst evi-search-wraparound t
  "*If t, search wraps around the end of the file.")

;; Ex commands

(defvar ex-commands
  '((("append" . 1) . ((1 . nil) . ex-not-implemented))
    (("args" . 2) . ((0 . nil) . ex-not-implemented))
    (("cd" . 2) . ((0 . ((t . rest-of-line))) . ex-change-directory))
    (("change" . 1) . ((2 . nil) . ex-not-implemented))
    (("chdir" . 3) . ((0 . ((t . rest-of-line))) . ex-change-directory))
    (("copy" . 2) . ((2 . ((t . address))) . ex-copy))
    (("delete" . 1) . ((2 . ((t . register))) . ex-delete))
    (("edit" . 1) . ((0 . ((nil . "!") (t . word))) . ex-edit))
    (("Edit" . 1) . ((0 . ((nil . "!") (t . word))) . ex-edit-other-window))
    (("file" . 1) . ((0 . ((t . word))) . ex-file))
    (("global" . 1) .
     ((2 . ((t . regular-expression) (t . command))) . ex-global))
    (("insert" . 1) . ((1 . nil) . ex-not-implemented))
    (("join" . 1) . ((2 . nil) . ex-not-implemented))
    (("list" . 1) . ((2 . nil) . ex-not-implemented))
    (("map" . 3) .
     ((0 . ((nil . "!") (t . word) (t . rest-of-line))) . ex-map))
    (("mark" . 2) . ((1 . nil) . ex-not-implemented))
    (("move" . 1) . ((2 . ((t . address))) . ex-move))
    (("next" . 1) . ((0 . ((nil . "!"))) . ex-next))
    (("Next" . 1) . ((0 . ((nil . "!"))) . ex-next-other-window))
    (("number" . 2) . ((2 . nil) . ex-not-implemented))
    (("previous" . 3) . ((0 . nil) . ex-not-implemented))
    (("print" . 1) . ((2 . nil) . ex-print))
    (("put" . 2) . ((1 . ((t . register))) . ex-put))
    (("quit" . 1) . ((0 . ((nil . "!"))) . ex-quit))
    (("read" . 1) . ((1 . ((t . "!") (t . rest-of-line))) . ex-read))
    (("rewind" . 3) . ((0 . nil) . ex-not-implemented))
    (("set" . 2) . ((0 . ((nil . settings))) . ex-set))
    (("source" . 2) . ((0 . ((t . rest-of-line))) . ex-source-file))
    (("substitute" . 1) .
     ((2 . ((t . regular-expression) (backup . regular-expression)
	    (nil . "g") (nil . "c"))) . ex-substitute))
    (("tag" . 1) . ((0 . ((t . word))) . ex-tag))
    (("undo" . 1) . ((0 . nil) . ex-not-implemented))
    (("unmap" . 3) . ((0 . nil) . ex-not-implemented))
    (("version" . 2) . ((0 . nil) . ex-not-implemented))
    (("write" . 1) . ((2 . ((nil . "!") (t . ">>") (t . word))) . ex-write))
    (("wq" . 2) . ((0 . nil) . ex-write-quit))
    (("xit" . 1) . ((0 . nil) . ex-save-quit))
    (("yank" . 1) . ((2 . ((t . register))) . ex-yank))
    (("!" . 1) . ((2 . ((t . rest-of-line))) . ex-shell-command))
    (("<" . 1) . ((2 . nil) . ex-shift-left))
    (("=" . 1) . ((2 . nil) . ex-not-implemented))
    ((">" . 1) . ((2 . nil) . ex-shift-right))
    (("&" . 1) . ((2 . nil) . ex-not-implemented))
    (("@" . 1) . ((2 . nil) . ex-not-implemented))
    (("" . 0) . ((2 . nil) . ex-null))))

;; Macros

(defmacro defmotion (&rest args)
  (let* ((direction (car args))
	 (function (car (cdr args)))
	 (params (nth 2 args))
	 (documentation (nth 3 args))
	 (body (nthcdr 4 args))
	 (do-function (intern (concat "do-" (symbol-name function)))))
    ; ZZ some rather narly hard-coding here, but does the trick for now
    (cond ((eq (car params) '&char)
	    (` (progn (defun (, function) (char) (, documentation)
			(interactive "c")
			(if evi-command-keys
			  (setq evi-command-keys
			    (concat evi-command-keys (char-to-string char))))
			(evi-motion-command (quote (, do-function))
					    (quote (, direction))
					    evi-prefix-count evi-context char))
		      (defun (, do-function) (, (cdr params)) (,@ body)))))
;(	    (list 'progn
;	      (list 'defun function '(char) documentation '(interactive "c")
;		    (function (if evi-command-keys
;		       (setq evi-command-keys
;			     (concat evi-command-keys (char-to-string char)))))
;		    (list 'evi-motion-command (list 'quote do-function)
;			  (list 'quote direction)
;			  'evi-prefix-count 'evi-context 'char))
;	      (append (list 'defun do-function (cdr params)) body)))
	  ((eq (car params) '&string)
	    (` (progn (defun (, function) () (, documentation)
			(interactive)
			(evi-motion-command
			  (quote (, do-function)) (quote (, direction))
			  evi-prefix-count evi-context
			  (evi-read-string (, (car (cdr params))))))
		      (defun (, do-function) (, (cdr (cdr params)))
			(,@ body)))))
;(	    (list 'progn
;	      (list 'defun function () documentation '(interactive)
;		      (list 'evi-motion-command (list 'quote do-function)
;			    (list 'quote direction)
;			    'evi-prefix-count 'evi-context
;			    (list 'evi-read-string (car (cdr params)))))
;	      (append (list 'defun do-function (cdr (cdr params))) body)))
	  (t
	    (` (progn (defun (, function) () (, documentation)
			(interactive)
			(evi-motion-command
			  (quote (, do-function)) (quote (, direction))
			  evi-prefix-count evi-context))
		      (defun (, do-function) (, params) (,@ body))))))))
;	    (list 'progn
;	      (list 'defun function () documentation '(interactive)
;		    (list 'evi-motion-command (list 'quote do-function)
;			  (list 'quote direction)
;			  'evi-prefix-count 'evi-context))
;	      (append (list 'defun do-function params) body))))))

(defmacro evi-iterate (count &rest body)
  (list 'let (list (list 'count count))
	  (append (list 'while (list '> 'count 0)) body
		  (list (list 'setq 'count (list '1- 'count))))
	  (list '= 'count 0)))

(defmacro evi-break ()
  (list 'setq 'count -1))

(defmacro evi-enumerate-condition (item list condition &rest body)
  (list 'let (list (list 'list list) (list item))
    (append
      (list 'while
	(list 'and 'list
	      (list 'progn (list 'setq item '(car list)) condition)))
      (if body
	(append body '((setq list (cdr list))))
	'((setq list (cdr list)))))
    'list))

(defmacro evi-register-text (register)
  (list 'car register))

(defmacro evi-register-whole-lines-p (register)
  (list 'cdr register))

(defmacro evi-single-change (&rest prog)
  (append '(let ((previous-undo-list buffer-undo-list)))
	     prog
	     '((setq buffer-undo-list
		     (evi-remove-undo-boundaries
		      buffer-undo-list previous-undo-list)))))

;; Keymaps

(defun evi-define-key (maps key def)
  (evi-enumerate-condition map maps t
    (eval (list 'define-key
		(intern (concat "evi-" (symbol-name map) "-map")) 'key 'def))))

;(defmacro evi-define-key (maps key def)
;  (append '(progn)
;    (mapcar (function
;	      (lambda (map)
;		(list 'define-key
;		      (intern (concat "evi-" (symbol-name map) "-map"))
;		      key def)))
;	    (if (listp maps) maps (symbol-value maps)))))

(defun evi-define-macro (maps key macro)
  (evi-enumerate-condition map maps t
    (eval (list 'define-key
		(intern (concat "evi-" (symbol-name map) "-map")) 'key
		(list 'quote (list 'lambda ()
		   '(interactive) (list 'evi-execute-command-macro macro)))))))

;(defmacro evi-define-macro (maps key macro)
;  (append '(progn)
;    (mapcar (function
;	      (lambda (map)
;		(list 'define-key
;		      (intern (concat "evi-" (symbol-name map) "-map"))
;		      key 
;		      (list 'function
;			    (list 'lambda () '(interactive)
;				  (list 'evi-execute-command-macro macro))))))
;	    (if (listp maps) maps (symbol-value maps)))))

(defun evi-make-local-keymap (keydefs)
  (let ((keymap (make-sparse-keymap)))
    (mapcar '(lambda (keydef)
	       (define-key keymap (eval (car keydef)) (car (cdr keydef))))
	    keydefs)
    keymap))

(evi-define-key '(vi motion internal) "]" nil)
(evi-define-key '(vi motion internal) "[" nil)
(evi-define-key '(vi) "Z" nil)

(evi-define-key '(vi internal motion minibuffer minibuffer-completion
		     minibuffer-must-match minibuffer-no-space)
		"\C-_" 'evi-internal-command)
(evi-define-key '(vi internal motion) "[(" 'evi-parameterized-macro)
(evi-define-key '(vi internal) "@" 'evi-register-macro)
(evi-define-key '(internal) "\"" 'evi-register-parameter)
(evi-define-key '(internal) "#" 'evi-prefix-count-parameter)
(evi-define-key '(internal) "\t" 'evi-maybe-indent)

;; Since these aren't in vi, they have been commented out.
;; :e already provides this functionality.
;; (evi-define-key '(vi) "v" 'evi-find-file)
;; (evi-define-key '(vi) "V" 'evi-find-file-other-window)

(evi-define-key evi-all-keymaps-but-insert "\C-^" 'evi-other-file)

(evi-define-key '(vi) "\C-f" 'evi-scroll-page-forward)
(evi-define-key '(vi) "\C-b" 'evi-scroll-page-backward)
(evi-define-key '(vi) "\C-d" 'evi-scroll-text-forward)
(evi-define-key '(vi) "\C-u" 'evi-scroll-text-backward)
(evi-define-key '(vi) "\C-e" 'evi-scroll-cursor-forward)
(evi-define-key '(vi) "\C-y" 'evi-scroll-cursor-backward)
(evi-define-key '(vi) "z" 'evi-window-control)

(evi-define-key '(vi internal) "i" 'evi-insert)
(evi-define-macro '(vi) "a" "\C-_l\C-_#\C-_i")
(evi-define-macro '(vi) "I" "\C-_0\C-_^\C-_i")
(evi-define-macro '(vi) "A" "\C-_$\C-_i")
(evi-define-macro '(vi) "o" "\C-_$\C-_i\r")
(evi-define-macro '(vi) "O" "\C-_0\C-_i\r\e\C-_k\C-_i") ; #### is this right?
(evi-define-key '(vi) "r" 'evi-replace-char)
(evi-define-key '(vi) "R" 'evi-replace)
(evi-define-key '(vi) "~" 'evi-toggle-case)
(evi-define-key '(vi internal) "c" 'evi-change)
(evi-define-macro '(vi) "C" "\C-_\"\C-_c\C-_#$")
(evi-define-macro '(vi) "s" "\C-_\"\C-_c\C-_#l")
(evi-define-macro '(vi) "S" "\C-_\"\C-_c\C-_#c")
(evi-define-key '(vi internal) "d" 'evi-delete)
(evi-define-macro '(vi) "x" "\C-_\"\C-_d\C-_#l")
(evi-define-macro '(vi) "X" "\C-_\"\C-_d\C-_#h")
; true to vi, `D' doesn't take a count...  this should change as I can't think
; of any good reason why it doesn't, esp since `C' does!
(evi-define-macro '(vi) "D" "\C-_\"\C-_d$")
(evi-define-key '(vi internal) "y" 'evi-yank)
; I wish I knew why D is d$ and C is c$, but Y is yy.  The manual says:
; 'a very useful synonym for yy'.  Well, that makes it clear! (never mind
; that shift-y is no easier to type than yy.)
(evi-define-macro '(vi) "Y" "\C-_\"\C-_y\C-_#y")
(evi-define-key '(vi) "p" 'evi-put-after)
(evi-define-key '(vi) "P" 'evi-put)
(evi-define-key '(vi internal) ">" 'evi-shift-right)
(evi-define-key '(vi internal) "<" 'evi-shift-left)
(evi-define-key '(vi internal) "=" 'evi-indent)
(evi-define-key '(vi internal) "!" 'evi-shell-filter)
(evi-define-key '(vi internal) "[{" 'evi-loop-over-lines-in-region)
(evi-define-key '(vi internal) "J" 'evi-join-lines)

(evi-define-key '(vi internal motion) "l" 'evi-forward-char)
(evi-define-key '(vi internal motion) " " 'evi-forward-char)
(evi-define-key '(vi internal motion) "h" 'evi-backward-char)
(evi-define-key '(vi internal motion) "\C-h" 'evi-backward-char)
(if (fboundp 'map-keymap)
    (evi-define-key '(vi internal motion) 'backspace 'evi-backward-char))
(evi-define-key '(vi internal motion) "j" 'evi-next-line)
(evi-define-key '(vi internal motion) "\C-j" 'evi-next-line)
(evi-define-key '(vi internal motion) "\C-n" 'evi-next-line)
(evi-define-key '(vi internal motion) "\C-m" 'evi-beginning-of-next-line)
(evi-define-key '(vi internal motion) "+" 'evi-beginning-of-next-line)
(evi-define-key '(vi internal motion) "k" 'evi-previous-line)
(evi-define-key '(vi internal motion) "\C-p" 'evi-previous-line)
(evi-define-key '(vi internal motion) "-" 'evi-beginning-of-previous-line)
(evi-define-key '(vi internal motion) "G" 'evi-goto-line)
(evi-define-key '(vi internal motion) "H" 'evi-goto-top-of-window)
(evi-define-key '(vi internal motion) "M" 'evi-goto-middle-of-window)
(evi-define-key '(vi internal motion) "L" 'evi-goto-bottom-of-window)
(evi-define-key '(vi internal motion) "|" 'evi-goto-column)
(evi-define-key '(vi internal motion) "0" 'evi-beginning-of-line)
(evi-define-key '(vi internal motion) "^" 'evi-goto-indentation)
(evi-define-key '(vi internal motion) "$" 'evi-end-of-line)
(evi-define-key '(vi internal motion) "w" 'evi-forward-word)
(evi-define-key '(vi internal motion) "W" 'evi-forward-white-word)
(evi-define-key '(vi internal motion) "e" 'evi-end-of-word)
(evi-define-key '(vi internal motion) "E" 'evi-end-of-white-word)
(evi-define-key '(vi internal motion) "b" 'evi-backward-word)
(evi-define-key '(vi internal motion) "B" 'evi-backward-white-word)
(evi-define-key '(vi internal motion) ")" 'evi-forward-sentence)
(evi-define-key '(vi internal motion) "(" 'evi-backward-sentence)
(evi-define-key '(vi internal motion) "}" 'evi-forward-paragraph)
(evi-define-key '(vi internal motion) "{" 'evi-backward-paragraph)
(evi-define-key '(vi internal motion) "]]" 'evi-forward-section)
(evi-define-key '(vi internal motion) "[[" 'evi-backward-section)
(evi-define-key '(internal motion) "r" 'evi-region)
(evi-define-key '(internal motion) "R" 'evi-region-whole-lines)
(evi-define-key '(vi internal motion) "/" 'evi-search-forward)
(evi-define-key '(vi internal motion) "?" 'evi-search-backward)
(evi-define-key '(vi internal motion) "n" 'evi-search-next)
(evi-define-key '(vi internal motion) "N" 'evi-search-next-reverse)
(evi-define-key '(vi internal motion) "f" 'evi-find-character)
(evi-define-key '(vi internal motion) "F" 'evi-find-character-backwards)
(evi-define-key '(vi internal motion) "t" 'evi-find-character-before)
(evi-define-key '(vi internal motion) "T" 'evi-find-character-backwards-after)
(evi-define-key '(vi internal motion) ";" 'evi-find-next-character)
(evi-define-key '(vi internal motion) "," 'evi-find-next-character-reverse)
(evi-define-key '(vi internal motion) "%" 'evi-paren-match)

(evi-define-key '(vi) "m" 'evi-mark)
(evi-define-key '(vi internal motion) "`" 'evi-goto-mark-horizontal)
(evi-define-key '(vi internal motion) "'" 'evi-goto-mark-vertical)

(evi-define-key '(vi) "." 'evi-repeat)
(evi-define-key '(vi) "_" 'evi-prompt-repeat)

(evi-define-key '(vi) "u" 'evi-undo)
;(evi-define-key '(vi) "U" 'evi-undo-more)
(evi-define-key '(vi) "U" 'evi-undo-all)

(evi-define-key '(vi motion) "1" 'evi-prefix-digit)
(evi-define-key '(vi motion) "2" 'evi-prefix-digit)
(evi-define-key '(vi motion) "3" 'evi-prefix-digit)
(evi-define-key '(vi motion) "4" 'evi-prefix-digit)
(evi-define-key '(vi motion) "5" 'evi-prefix-digit)
(evi-define-key '(vi motion) "6" 'evi-prefix-digit)
(evi-define-key '(vi motion) "7" 'evi-prefix-digit)
(evi-define-key '(vi motion) "8" 'evi-prefix-digit)
(evi-define-key '(vi motion) "9" 'evi-prefix-digit)

(evi-define-key '(vi) "\"" 'evi-prefix-register)

(evi-define-key '(vi) "\C-g" 'evi-file-info)
(evi-define-key '(vi) "\C-]" 'evi-tag)

(evi-define-key '(vi) ":" 'evi-ex-command)

(evi-define-key '(vi) "\C-c" 'keyboard-quit)
(evi-define-key '(vi) "\C-l" 'evi-redraw-selected-screen)
(evi-define-key evi-all-keymaps-but-insert "\C-r" 'evi-redraw-selected-screen)
;; Redefined to return to normal emacs mode.
;;(evi-define-key evi-all-keymaps-but-insert "\C-z" 'suspend-emacs)
(evi-define-key '(vi) "ZZ" 'evi-save-and-exit)

(evi-define-key evi-all-input-maps "\C-v" 'quoted-insert)

(evi-define-key '(insert) "\C-c" 'evi-input-mode-quit)
(evi-define-key '(insert) "\C-d" 'evi-backward-indent)
(evi-define-key '(insert) "\C-h" 'evi-insert-mode-delete-backward-char)
(if (fboundp 'map-keymap)
    (evi-define-key '(insert) 'backspace 'evi-insert-mode-delete-backward-char))
(evi-define-key '(insert) "\C-t" 'evi-forward-indent)
(evi-define-key '(insert) "\C-w" "\C-_d\C-_b")
;(evi-define-key (insert replace) "\e" 'evi-exit-input-mode)
(evi-define-key '(insert replace) "\e" 'evi-exit-get-commands)
(evi-define-key '(insert) "\177" 'evi-insert-mode-delete-backward-char)

(evi-define-key '(replace) "\C-c" 'evi-input-mode-quit)
;(evi-define-key (replace) "\C-d" 'evi-backward-indent)
(evi-define-key '(replace) "\C-h" 'evi-replace-mode-delete-backward-char)
(if (fboundp 'map-keymap)
    (evi-define-key '(replace) 'backspace 'evi-replace-mode-delete-backward-char))
;(evi-define-key (replace) "\C-t" 'evi-forward-indent)
;(evi-define-key (replace) "\C-w" 'evi-delete-backward-word)
(evi-define-key '(replace) "\177" 'evi-replace-mode-delete-backward-char)

(evi-define-key '(minibuffer minibuffer-completion
			     minibuffer-must-match minibuffer-no-space ex)
		"\C-c" 'abort-recursive-edit)
(evi-define-key '(minibuffer) "\C-h" 'evi-minibuffer-delete-backward-char)
(evi-define-key '(minibuffer-completion
		  minibuffer-must-match minibuffer-no-space)
		"\C-h" 'delete-backward-char)
(evi-define-key '(minibuffer) "\177" 'evi-minibuffer-delete-backward-char)
(evi-define-key '(minibuffer-completion
		  minibuffer-must-match minibuffer-no-space)
		"\177" 'delete-backward-char)
(evi-define-key '(minibuffer minibuffer-completion
			     minibuffer-no-space ex)
		"\C-j" 'exit-minibuffer)
(evi-define-key '(minibuffer minibuffer-completion minibuffer-no-space ex)
		"\C-m" 'exit-minibuffer)
(evi-define-key '(minibuffer minibuffer-completion
			     minibuffer-must-match minibuffer-no-space)
		"\C-w" "\C-_d\C-_b")
(evi-define-key '(minibuffer minibuffer-completion minibuffer-no-space ex)
		"\e" 'exit-minibuffer)

(evi-define-key '(minibuffer-completion minibuffer-must-match)
		"\C-i" 'minibuffer-complete)
(evi-define-key '(minibuffer-completion minibuffer-must-match)
		" " 'minibuffer-complete-word)
(evi-define-key '(minibuffer-completion minibuffer-must-match)
		"?" 'minibuffer-completion-help)

(evi-define-key '(minibuffer-must-match) "\C-j" 'minibuffer-complete-and-exit)
(evi-define-key '(minibuffer-must-match) "\C-m" 'minibuffer-complete-and-exit)
(evi-define-key '(minibuffer-must-match) "\e" 'minibuffer-complete-and-exit)

(evi-define-key '(minibuffer-no-space) "\C-i" 'exit-minibuffer)
(evi-define-key '(minibuffer-no-space) " " 'exit-minibuffer)
(evi-define-key '(minibuffer-no-space) "?" 'self-insert-and-exit)

(evi-define-key '(ex) "\C-h" 'ex-delete-backward-char)
(evi-define-key '(ex) "\177" 'ex-delete-backward-char)
(evi-define-key '(ex) "\C-i" 'ex-complete)
;(evi-define-key (ex) "\C-w" 'ex-delete-backward-word)
(evi-define-key '(ex) " " 'ex-space)

(evi-define-key evi-all-keymaps "\C-x" ctl-x-map)
(evi-define-key evi-all-keymaps "\C-a" esc-map)


;; Command macros

(defun evi-execute-command-macro (macro)
  (evi-save-command-keys)
  (let* ((evi-last-command-keys nil)
	 (evi-register-parameter evi-register)
	 (evi-register nil)
	 (evi-prefix-count-parameter evi-prefix-count)
	 (evi-prefix-count nil)
	 (previous-undo-list buffer-undo-list))
    (evi-single-change
      (execute-kbd-macro macro))
	     (evi-fixup-cursor 'vertical)))

(defun evi-parameterized-macro ()
  (interactive)
  (evi-start-command-keys)
  (let ((macro (evi-read-string "(")))
    (evi-execute-command-macro macro)))

(defun evi-register-macro (char)
  (interactive "c")
  (evi-save-command-keys)
  (let* ((evi-last-command-keys nil)
	 (register-number (if (= char ?@)
			    (or evi-last-macro-register
				(error "No previous macro register specified"))
			    (evi-register-number char)))
	 (macro (evi-register-text (aref evi-registers register-number))))
    (setq evi-last-macro-register register-number)
    (execute-kbd-macro macro)))

(defun evi-internal-command ()
  (interactive)
  (let ((evi-internal-command t)
	(evi-vi-map evi-internal-map))
    (evi-get-command)))

(defun evi-register-parameter ()
  (interactive)
  (let ((evi-register evi-register-parameter))
    (evi-get-command)))

(defun evi-prefix-count-parameter ()
  (interactive)
  (let ((evi-prefix-count evi-prefix-count-parameter))
    (evi-get-command)))

;; Get command

(defun evi-get-commands (&optional local-map)
  (setq evi-get-commands t)
  (let ((echo-keystrokes 0))
    (while evi-get-commands
      (evi-get-command nil local-map))))

(defun evi-exit-get-commands ()
  (interactive)
  (setq evi-get-commands nil))

(defun evi-this-command-keys-string ()
  ;; this-command-keys now returns a vector of events.
  ;; convert that to a string of ascii characters.
  (concat (mapcar 'evi-event-to-character (append (this-command-keys) nil))))

(defun evi-get-command (&optional save-command-keys local-map)
  (if (and save-command-keys (eq evi-command-keys nil))
    (setq evi-command-keys (evi-this-command-keys-string)))
  (let ((current-keymap evi-vi-map)
	(current-local-keymap (or local-map evi-vi-local-map))
	(evi-get-command-depth (1+ evi-get-command-depth)))
    (while current-keymap
      ;; In the Lucid Emacs, keyboard macros terminate by throwing to
      ;; execute-kbd-macro instead of by returning -1 from read-char.
      ;; The following code works in v18 or Lucid Emacs.
      (let ((char (catch 'execute-kbd-macro (evi-read-char))))
	(if (or (eq char t) (= char -1))
	    (progn (if executing-macro
		       (setq executing-macro nil)
		     (error "Unknown source of EOS"))
		   (setq char (evi-read-char))))
	(let ((keydef
	       (or (if current-local-keymap
		       (lookup-key current-local-keymap
				   (char-to-string char)))
		   (lookup-key current-keymap (char-to-string char)))))
	  (setq last-command-char char
		last-command-event (character-to-event char (allocate-event)))
	  ; probably lousy on garbage collection... 
	  (if evi-command-keys
	    (setq evi-command-keys
		  (concat evi-command-keys (char-to-string char))))
	  (cond ((keymapp keydef)
		  (setq current-keymap keydef)
		  (setq current-local-keymap nil))
		((stringp keydef)
		  (execute-kbd-macro keydef)
		  (setq current-keymap nil))
		((commandp keydef)
		  (call-interactively keydef)
		  (setq current-keymap nil))
		(t (error "Unknown command"))))))
    (prog1
      (not evi-signal-abort)
      (if (= evi-get-command-depth 1)
	(setq evi-signal-abort nil)))))

(defun evi-read-string (prompt)
  (let ((string (read-string prompt)))
    (if evi-signal-abort
      (setq evi-command-keys nil)
      (if evi-command-keys
	(setq evi-command-keys (concat evi-command-keys string "\e"))))
    string))

(defun evi-start-command-keys ()
  (setq evi-command-keys (evi-this-command-keys-string)))

(defun evi-save-command-keys ()
  (setq evi-last-command-keys
	(or evi-command-keys
	    (evi-this-command-keys-string)))
  (setq evi-command-keys nil))

; ZZ belongs here?
(defun evi-interactive-args ()
  (list evi-prefix-count))

(defun evi-character-arg ()
  (list (let ((char (evi-read-char)))
	  ;; C-m always means C-j in vi.
	  (if (eq char ?\C-m)
	      ?\C-j
	    char))
	evi-prefix-count))

;; Mode line

(defun evi-change-mode-id (string)
  "Change the mode identification string to STRING."
  (setq mode-line-buffer-identification '("EVI 0.9b: %16b"))
  (setq mode-name string))

(defun evi-refresh-mode-line ()
  "Redraw mode line."
  (set-buffer-modified-p (buffer-modified-p)))

;; Startup

(defvar evi-install-undo-list nil)

(defun evi-install-var (var value)
  (or (assq var evi-install-undo-list)
      (setq evi-install-undo-list
	    (cons (cons var (symbol-value var)) evi-install-undo-list)))
  (set var value))


(defvar evi-startup-hook nil
  "function or functions to run when evi is started.")
(defvar evi-exit-hook nil
  "function or functions to run when evi mode is turned off.")
(defvar evi-mode-hook nil
  "function or functions to run for each buffer that is placed in evi-mode.")

(defun evi ()
  "Start global vi emulation."
  (interactive)
  (let ((evi-was-on-already (not (null evi-install-undo-list))))
    (evi-install-var 'minibuffer-local-map evi-minibuffer-map)
    (evi-install-var 'minibuffer-local-completion-map
		     evi-minibuffer-completion-map)
    (evi-install-var 'minibuffer-local-must-match-map
		     evi-minibuffer-must-match-map)
    (evi-install-var 'minibuffer-local-ns-map evi-minibuffer-no-space-map)
    (and (boundp 'interrupt-char) (evi-install-var 'interrupt-char ?\^C))
    (evi-load-init-files)
    (or evi-was-on-already (run-hooks 'evi-startup-hook))
    (evi-mode)
    (evi-refresh-mode-line)))

(defvar vi-buffer-p)

(defun evi-mode ()
  "Start vi emulation in this buffer."
  (or evi-install-undo-list (evi))
  (evi-change-mode-id "Vi")
  (use-local-map evi-vi-map)
  (set (make-local-variable 'vi-buffer-p) t)
  (set (make-local-variable 'scroll-step) 1)
  (set (make-local-variable 'evi-last-changed-line) nil)
  (if (boundp 'zmacs-regions)
      (set (make-local-variable 'zmacs-regions) nil))
  ;; we have to disable the meta key in vi mode to make vi mode and
  ;; emacs modes coexist (if we don't disable the meta key, then we
  ;; have to install a new global-map, which is Bad.)
  (set (make-local-variable 'meta-prefix-char) evi-meta-prefix-char)
  (evi-refresh-mode-line)
  (setq buffer-read-only nil)  ; vi lets you edit read-only files
  (run-hooks 'evi-mode-hook)
  )

(defun evi-exit-to-emacs ()
  "Stop vi emulation."
  (interactive)
  (mapcar '(lambda (cons)
	     (set (car cons) (cdr cons)))
	  evi-install-undo-list)
  (setq evi-install-undo-list nil)
  (save-excursion
    (let ((rest (buffer-list)))
      (while rest
	(set-buffer (car rest))
	(if (and (boundp 'vi-buffer-p) vi-buffer-p)
	    (progn
	      (setq vi-buffer-p nil)
	      (if (and (buffer-file-name)
		       (not (buffer-modified-p))
		       (not (file-writable-p (buffer-file-name))))
		  (setq buffer-read-only t))
	      (use-local-map nil)
	      (kill-all-local-variables)
	      (after-find-file nil nil)))
	(setq rest (cdr rest)))))
  (evi-refresh-mode-line)
  (run-hooks 'evi-exit-hook)
  (top-level))

;; Minibuffer

(defun evi-minibuffer-delete-backward-char ()
  "Backup and delete previous character, aborting command if at
beginning of input."
  (interactive)
  (if (bolp)
    (progn (setq evi-signal-abort t)
	   (exit-minibuffer))
    (delete-backward-char 1)))

;; Finding files

;; These have been removed due to non-vi-ness

;;(defun evi-find-file ()
;;  "Find file for editing in this window."
;;  (interactive)
;;    (find-file (read-file-name "find file: "))
;;  (evi-mode))

;;(defun evi-find-file-other-window ()
;;  "Find file for editing in this window."
;;  (interactive)
;;  (find-file-other-window (read-file-name "find file: "))
;;  (evi-mode))

(defun evi-other-file ()
  "Switch to other file."
  (interactive)
  (if (one-window-p)
    (let ((buffer (evi-next-file-buffer)))
      (if buffer
	(switch-to-buffer buffer)
	(message "No other file to display")))
    (other-window 1)))

;; Scrolling

(defun evi-scroll-page-forward (&optional count)
  "Scroll COUNT pages forward."
  (interactive (evi-interactive-args))
  (scroll-up (if (eq (or count 1) 1)
	       (- (window-height) 3)
	       (* (1- (window-height)) (or count 1))))
  (setq evi-reset-goal-column t))

(defun evi-scroll-page-backward (&optional count)
  "Scroll COUNT pages backward."
  (interactive (evi-interactive-args))
  (scroll-down (if (eq (or count 1) 1)
		 (- (window-height) 3)
		 (* (1- (window-height)) (or count 1))))
  (setq evi-reset-goal-column t))

(defun evi-scroll-text-forward (&optional count)
  "Scroll COUNT lines forward.  Default is one half of a page or the last COUNT
specified to either \\[evi-scroll-up] or \\[evi-scroll-down] if one was previously
given.  The position of the cursor on the screen is maintained."
  (interactive (evi-interactive-args))
  (if evi-reset-goal-column
    (progn (setq evi-goal-column (current-column))
	   (setq evi-reset-goal-column nil)))
  (let ((line-count (if count
		      (setq evi-scroll-count count)
		      (or evi-scroll-count (/ (1- (window-height)) 2))))
	(window-line (count-lines (window-start) (1+ (point))))
	(window-text-height (count-lines (window-start) (window-end))))
    (if (<= line-count window-text-height)
	(progn
	  (scroll-up line-count)
	  (forward-line (min (1- window-line) line-count)))
      (if (<= line-count (/ window-text-height 2))
	  (forward-line (/ window-text-height 2))
	(if (save-excursion (forward-line 1) (eobp))
	    (error "End of buffer")
	  (goto-char (point-max)))))
    (evi-move-to-column evi-goal-column)
    (evi-fixup-cursor 'vertical)))

(defun evi-scroll-text-backward (&optional count)
  "Scroll COUNT lines backward.  Default is one half of a page or the last COUNT
specified to either \\[evi-scroll-up] or \\[evi-scroll-down] if one was previously
given.  The position of the cursor on the screen is maintained."
  (interactive (evi-interactive-args))
  (if evi-reset-goal-column
    (progn (setq evi-goal-column (current-column))
	   (setq evi-reset-goal-column nil)))
  (let ((line-count (if count
		      (setq evi-scroll-count count)
		      (or evi-scroll-count (/ (1- (window-height)) 2))))
	(window-line (count-lines (window-start) (1+ (point)))))
    (scroll-down line-count)
    (forward-line (- (min (- (1- (window-height)) window-line) line-count)))
    (evi-move-to-column evi-goal-column)))

(defun evi-scroll-cursor-forward (&optional count)
  "Scroll COUNT lines forward.  Maintain cursor position in the file
if possible."
  (interactive (evi-interactive-args))
  (if evi-reset-goal-column
    (progn (setq evi-goal-column (current-column))
	   (setq evi-reset-goal-column nil)))
  (scroll-up (or count 1))
  (evi-move-to-column evi-goal-column))

(defun evi-scroll-cursor-backward (&optional count)
  "Scroll COUNT lines backward.  Maintain cursor position in the file
if possible."
  (interactive (evi-interactive-args))
  (if evi-reset-goal-column
    (progn (setq evi-goal-column (current-column))
	   (setq evi-reset-goal-column nil)))
  (scroll-down (or count 1))
  (evi-move-to-column evi-goal-column))

(defun evi-window-control (char &optional linenumber)
  "Position current line on the screen according to the following character.
With a prefix count, position that line."
  (interactive (evi-character-arg))
  (if linenumber
    (do-evi-goto-line linenumber))
  (if (and (>= char ?0) (<= char ?9))
    (let* ((count (evi-read-number (- char ?0)))
	   (char (evi-read-char)))
      (cond ((= char ?.) (enlarge-window (- count (1- (window-height)))))
	    ((= char ?+) (enlarge-window count))
	    ((= char ?-) (shrink-window count))
	    ((= char ?=) (if (= count 1)
			   (delete-other-windows)
			   (split-window-vertically)))))
    (let ((position
	    (cond ((or (eq char ?\r) (eq char ?H)) 0)
		  ((or (eq char ?.) (eq char ?M)) (/ (window-height) 2))
		  ((or (eq char ?-) (eq char ?L)) (- (window-height) 2)))))
      (recenter position))))

;; unlike the motion commands, the scroll commands have no wrapper function
;; to fixup the cursor, soo...
(defun evi-move-to-column (column)
  (move-to-column column)
  (if (and (eolp) (not (bolp)))
    (backward-char)))

;; Insert mode

(defun evi-insert (&optional count)
  "Enter insert mode, adding new text before the cursor."
  (interactive (evi-interactive-args))
  ;; ZZ this could be a problem if an internal command is used...
  (evi-start-command-keys)
  (setq evi-insert-point (point))
  (evi-insert-mode count)
  (if (not (bolp)) (backward-char)))

(defun evi-insert-mode (&optional count)
  (setq evi-mode 'insert)
  (if (eobp) (progn (newline 1) (backward-char 1)))
  (if evi-auto-indent
    (define-key evi-insert-map "\C-m" 'evi-newline-and-indent)
    (define-key evi-insert-map "\C-m" 'newline))
  (evi-change-mode-id "Insert")
  (evi-remember-last-changed-line)
  (evi-refresh-mode-line)
  ; don't want to record *every* keystroke here... just want the final result
  (let ((evi-command-keys nil))
    (evi-get-commands evi-insert-map))
  (evi-maybe-kill-indentation)
  (evi-exit-input-mode count))

(defun evi-exit-input-mode (&optional count)
  "Exit from an input mode."
  (interactive)
  (let ((input-string (buffer-substring evi-insert-point (point))))
    (if evi-command-keys
      (progn (setq evi-command-keys
		   (concat evi-command-keys input-string "\e"))
	     (evi-save-command-keys)))
    (if count
      (evi-iterate (1- count) (insert input-string))))
  (setq evi-mode 'vi)
  (evi-change-mode-id "Vi")
  (evi-refresh-mode-line))

(defun evi-input-mode-quit ()
  "Abort and exit from and input mode."
  (interactive)
  (evi-exit-input-mode)
  (keyboard-quit))

(defun evi-insert-mode-delete-backward-char ()
  "Backup and delete previous character, but no further than insert point."
  (interactive)
  (if (> (point) evi-insert-point)
    (delete-backward-char 1)
    (message "Beginning of inserted text")))

(defun evi-maybe-indent ()
  (interactive)
  (if evi-auto-indent
    (progn (indent-according-to-mode)
	   (setq evi-current-indentation (current-column)))))

(defun evi-maybe-kill-indentation ()
  (if (and evi-auto-indent (= evi-current-indentation (current-column)))
    (let ((region
	   (save-excursion
	     (let ((start (if (progn (skip-chars-backward " \t") (bolp))
			    (point))))
	       (if (and start (progn (skip-chars-forward " \t") (eolp)))
		 (cons start (point)))))))
      (if region
	(delete-region (car region) (cdr region))))))

(defun evi-newline-and-indent ()
  "Insert a newline, and indent to the current indentation level.
Kills indentation on current line if the line is otherwise empty."
  (interactive)
  (evi-maybe-kill-indentation)
  (insert ?\n)
  (indent-according-to-mode)
  (setq evi-current-indentation (current-column)))

(defun evi-forward-indent ()
  "Move forward to the next indentation level, defined by shiftwidth."
  (interactive)
; eat all preceeding blanks, then fill with tabs, and pad with spaces
; to reach the target column
  (let* ((start-column (current-column))
	 (target-column (+ start-column (- evi-shift-width
					   (% start-column evi-shift-width))))
	 (backup-point (save-excursion
			 (skip-chars-backward " ")
			 (point))))
    (delete-backward-char (- (point) backup-point))
    (while (< (setq start-column (current-column)) target-column)
      (insert ?\t))
    (if (> start-column target-column) (delete-backward-char 1))
    (insert-char ?\ (- target-column (current-column)))))

(defun evi-backward-indent ()
  "Move backward to the previous indentation level, defined by shiftwidth."
  (interactive)
  (let* ((start-column (current-column))
	 (offset (let ((toffset (% start-column evi-shift-width)))
		   (if (= toffset 0) evi-shift-width toffset)))
	 (furthest (save-excursion
		     (skip-chars-backward " \t" (max 0 (- (point) offset)))
		     (- start-column (current-column)))))
    (backward-delete-char-untabify (min offset furthest) nil)))

;; Replace mode

(defun evi-replace ()
  "Enter replace mode."
  (interactive)
  (evi-start-command-keys)
  (evi-replace-mode (save-excursion (end-of-line) (point))) ; jwz
  (if (not (bolp)) (backward-char))
  (if evi-replace-max
      (set-marker evi-replace-max nil)))

;(define-key evi-replace-map "\C-d" 'evi-backward-indent)
;(define-key evi-replace-map "\C-t" 'evi-forward-indent)
;(define-key evi-replace-map "\C-w" 'evi-delete-backward-word)

(defvar evi-replaced-string nil)
(defvar evi-replaced-string-index nil)

(defun evi-replace-mode (max-replace-position)
  (or evi-replace-max
      (setq evi-replace-max (make-marker)))
  (set-marker evi-replace-max max-replace-position)
  (setq evi-mode 'replace)
  (setq evi-insert-point (point))
  (setq evi-replaced-string "")
  (setq evi-replaced-string-index 0)
  (evi-change-mode-id "Replce")
  (evi-remember-last-changed-line)
  (evi-refresh-mode-line)
  (if (catch 'switch-to-insert
	(let ((evi-command-keys nil))
	  (evi-get-commands evi-replace-map))
	t)
    (if (< evi-replaced-string-index (length evi-replaced-string))
      (save-excursion
	(delete-region (point)
		       (+ (point)
			  (- (length evi-replaced-string)
			     evi-replaced-string-index)))
	(insert (substring evi-replaced-string evi-replaced-string-index))))
    (progn
      (set-marker evi-replace-max nil)
      (evi-insert-mode)))
  (evi-exit-input-mode))

(defun evi-self-replace ()
  "Replace character under cursor with the command character."
  (interactive)
  (if (or (>= (point) evi-replace-max)
	  (and (= last-command-char ?\n)
	       (= (following-char) ?\n)))
      (progn (evi-unread-char nil)
	     (throw 'switch-to-insert nil))
    (progn (if (= evi-replaced-string-index (length evi-replaced-string))
	     (setq evi-replaced-string
	       (concat evi-replaced-string (char-to-string (following-char)))))
	   (setq evi-replaced-string-index (1+ evi-replaced-string-index))
	   (delete-region (point) (1+ (point)))
	   (if (and evi-overstruck-char (= (point) evi-replace-max))
	     (progn (aset (car (car buffer-undo-list))
			  0 evi-overstruck-char)
		    (setq evi-overstruck-char nil)))
	   (insert last-command-char)
	   (if (= last-command-char ?\n)
	     (evi-maybe-indent)))))
	     ; ZZ may not be entirely correct?  what about killing
	     ; indentation on current line?
	     ; also plays havok with evi-replaced-string...

(defun evi-replace-mode-delete-backward-char ()
  "Backup to previous character, undoing last replacement, but no further
than insert point."
  (interactive)
  (if (> (point) evi-insert-point)
    (progn (backward-char)
	   (setq evi-replaced-string-index (1- evi-replaced-string-index)))
    (message "Beginning of replaced text")))

; ZZ - doesn't do things like auto-indent correctly
(defun evi-replace-char (char &optional count)
  "Replace the following COUNT characters with CHAR."
  (interactive (evi-character-arg))
  (evi-motion-command 'do-evi-forward-char 'horizontal count 'to-end)
  (if evi-error-string
    (message "Can't replace that many characters")
    (progn (delete-region (mark) (point))
	   (insert-char char (or count 1))
	   (if (not (bolp)) (backward-char))))
  (evi-save-command-keys))

(defun evi-toggle-case (&optional count)
  "Toggle the case of the following COUNT characters."
  (interactive (evi-interactive-args))
  (evi-motion-command 'do-evi-forward-char 'horizontal count 'to-end)
  (save-excursion
    (evi-iterate (- (point) (mark))
      (backward-char)
      (let ((char (following-char)))
	(cond ((and (>= char ?a) (<= char ?z))
		(upcase-region (point) (1+ (point))))
	      ((and (>= char ?A) (<= char ?Z))
		(downcase-region (point) (1+ (point))))))))
  (evi-fixup-cursor 'horizontal)
  (evi-save-command-keys))

;; Modification operators

(defun evi-change (&optional count)
  "Change operator."
  (interactive (evi-interactive-args))
  (evi-operator-command (or count 1) 'to-end '(evi-change-internal) 1))

(defun evi-change-internal ()
  ; If the region is contained on one line, throw a `$' out to mark the
  ; end of the region, then enter replace mode and delete any un-replaced
  ; text when that is exited, with the replace-max set at the end of the
  ; region so that it will switch to insert mode if necessary.  Otherwise,
  ; delete the region first, and enter insert mode.
  (evi-copy-region-to-registers t)
  (evi-start-command-keys)
  ; this makes the undo leave the point at the start of the undone text
  (exchange-point-and-mark)
  (evi-single-change
    (if (save-excursion (end-of-line) (> (mark) (point)))
      (progn (delete-region (point) (mark))
	     (setq evi-insert-point (point))
	     (evi-insert-mode))
      (progn (setq evi-overstruck-char (char-after (1- (mark))))
	     (save-excursion
	       (exchange-point-and-mark)
	       (delete-region (1- (point)) (point))
	       (insert ?$)
	       ;; this is a bit of song and dance to get the cursor to
	       ;; end up in the right place after an undo.  the problem
	       ;; is these two previous statements, which are the first
	       ;; things changed, and thus where the cursor will be left
	       ;; after an undo.  first step: erase the fact that we put
	       ;; the dollar sign there in the first place.
	       (setq buffer-undo-list (cdr (cdr buffer-undo-list))))
	       ;; second step: wherever we finally delete the `$', rewrite
	       ;; the undo record with the original overstruck character.
	     (evi-replace-mode (1+ (mark)))
	     (if (and (marker-position evi-replace-max)
		      (< (point) evi-replace-max))
	       (let ((overstrike-offset (1- (- evi-replace-max (point)))))
		 (delete-region (point) (marker-position evi-replace-max))
		 (set-marker evi-replace-max nil)
		 ;; alter the undo list 
		 (aset (car (car buffer-undo-list))
		       overstrike-offset evi-overstruck-char)))))
    (if (not (bolp)) (backward-char))))

(defun evi-delete (&optional count)
  "Delete operator."
  (interactive (evi-interactive-args))
  (evi-operator-command (or count 1) 'to-next '(evi-delete-internal)))

(defun evi-delete-internal ()
  (evi-copy-region-to-registers t)
  ; this makes the undo leave the point at the start of the undone text
  (exchange-point-and-mark)
  (if (= (point) (mark))
    (message
      (concat "Nothing deleted" (if evi-error-string ": ") evi-error-string))
    (delete-region (point) (mark)))
  (evi-fixup-cursor (if evi-region-whole-lines 'vertical 'horizontal)))

(defun evi-yank (&optional count)
  "Yank operator."
  (interactive (evi-interactive-args))
  (save-excursion
    (evi-operator-command (or count 1) 'to-next '(evi-yank-internal))))

(defun evi-yank-internal ()
  (evi-copy-region-to-registers nil)
  (if (= (mark) (point))
    (message
      (concat "Nothing to yank" (if evi-error-string ": ") evi-error-string))))

(defun evi-put-after ()
  "Put back yanked or deleted text after cursor."
  (interactive)
  (let ((register
	  (aref evi-registers (or (car evi-register) evi-register-unnamed))))
    (if register
      (if (evi-register-whole-lines-p register)
	(progn (end-of-line)
	       (if (not (eobp)) (forward-char))
	       (save-excursion (insert (evi-register-text register))))
	(progn (if (not (and (bolp) (eolp)))
		 (forward-char))
	       (insert (evi-register-text register))
	       (backward-char)))
      (if evi-register
	(message "Nothing in register %c" (evi-register-name evi-register))
	(message "No text to put"))))
  (evi-save-command-keys))

(defun evi-put ()
  "Put back yanked or deleted text."
  (interactive)
  (let ((register
	  (aref evi-registers (or (car evi-register) evi-register-unnamed))))
    (if register
      (if (evi-register-whole-lines-p register)
	(progn (beginning-of-line)
	       (save-excursion (insert (evi-register-text register))))
	(progn (insert (evi-register-text register))
	       (backward-char)))
      (if evi-register
	(message "Nothing in register %c" (evi-register-name evi-register))
	(message "No text to put"))))
  (evi-save-command-keys))

(defun evi-shift-right (&optional count)
  "Shift right operator."
  (interactive (evi-interactive-args))
  (evi-operator-command (or count 1) 'whole-lines '(evi-shift-internal 1)))

(defun evi-shift-left (&optional count)
  "Shift left operator."
  (interactive (evi-interactive-args))
  (evi-operator-command (or count 1) 'whole-lines '(evi-shift-internal -1)))

(defun evi-shift-internal (direction)
  (if (= (mark) (point))
    (message
      (concat "Nothing shifted" (if evi-error-string ": ") evi-error-string))
    (indent-rigidly (mark) (point) (* evi-shift-width direction)))
  (goto-char (mark))
  (skip-chars-forward " \t"))

(defun evi-indent (&optional count)
  "Indent region."
  (interactive (evi-interactive-args))
  (evi-operator-command (or count 1) 'whole-lines '(evi-indent-internal)))

(defun evi-indent-internal ()
  (if (= (mark) (point))
    (message
      (concat "Nothing indented" (if evi-error-string ": ") evi-error-string))
    (indent-region (mark) (point) nil))
  (goto-char (mark))
  (skip-chars-forward " \t"))

(defun evi-shell-filter (&optional count)
  "Filter region thru shell command."
  (interactive (evi-interactive-args))
  (save-excursion
    (evi-operator-command (or count 1) 'whole-lines
			  '(evi-filter-internal input-string) t)))

(defun evi-filter-internal (shell-command)
  (shell-command-on-region (mark) (point) shell-command t))

(defun evi-loop-over-lines-in-region (&optional count)
  "Execute a sequence of operations on every line in a region."
  (interactive (evi-interactive-args))
  (evi-operator-command (or count 1) 'whole-lines
			'(evi-loop-lines-internal input-string) t))

(defun evi-loop-lines-internal (macro)
  (let ((evi-last-command-keys nil)
	(ending-mark (set-marker (make-marker) (point-marker)))
	(undo-start buffer-undo-list)
	(evi-prefix-count nil))
    (goto-char (mark))
    (beginning-of-line)
    (while (< (point) (marker-position ending-mark))
      (execute-kbd-macro macro)
      (end-of-line)
      (forward-char))
    (setq buffer-undo-list
	  (evi-remove-undo-boundaries buffer-undo-list undo-start))
    (set-marker ending-mark nil)))

(defun evi-operator-command (count context operation &optional more-input)
  (if (let ((evi-context context)
	    (evi-prefix-count-multiplier count)
	    (evi-vi-map evi-motion-map)
	    (evi-vi-local-map
	      (evi-make-local-keymap
		'(((char-to-string last-input-char) evi-whole-lines)))))
	(evi-get-command t))
    (let ((input-string (if (eq more-input t)
			  (evi-read-string (concat evi-command-keys " ")))))
      ;; ZZ note - we're saving the keys even tho' there may be an abort
      (if (or (not more-input) (eq more-input t))
	(evi-save-command-keys))
      ;; ZZ should just use real error trap here...
      (if evi-signal-abort
	(setq evi-signal-abort nil)
	(eval operation)))))

(defmacro evi-right-paren () ?\))

(defun evi-join-lines (&optional count)
  "Join together COUNT + 1 lines, supplying appropriate whitespace."
  (interactive (evi-interactive-args))
  (evi-iterate (or count 1)
    (let ((starting-point (point)))
      (end-of-line)
      (if (or (eobp) (progn (forward-char) (eobp)))
	(progn (goto-char starting-point)
	       (evi-break))
	(progn (delete-region (1- (point))
			      (progn (skip-chars-forward " \t") (point)))
	       (if (and (/= (preceding-char) ? )
			(/= (following-char) (evi-right-paren)))
		 (insert-char ?  (if (= (preceding-char) ?.) 2 1)))))))
  (evi-save-command-keys))

;; Motion command

(defun evi-expand-region-to-lines (context)
  (exchange-point-and-mark)
  (beginning-of-line)
  (exchange-point-and-mark)
  (end-of-line)
  (if (not (or (eobp) (eq context 'to-end))) (forward-char))
  (setq evi-region-whole-lines t))

; 'normalizing' a horizontal region means expanding the region to whole lines
; when 1) the beginning of the region is on the first non-white character
; of a line, and 2) the ending of the region is on the end of the line

(defun evi-normalize-region ()
  (if (and (eolp)
	   (save-excursion
	     (beginning-of-line)
	     (and (> (point) (mark))
		  (progn (goto-char (mark))
			 (skip-chars-backward " \t")
			 (bolp)))))
    (progn (save-excursion
	     (goto-char (mark))
	     (beginning-of-line))
	   (if (not (eobp))
	     (forward-char))
	   (setq evi-region-whole-lines t))))

(defun evi-fixup-cursor (direction)
  (or evi-internal-command
    (if (eq direction 'horizontal)
      (progn (if (and (eobp) (not (bobp)))
	       (backward-char))
	     (if (and (eolp) (not (bolp)))
	       (backward-char)))
      (if (and (eobp) (not (bobp)))
	(progn (backward-char) (beginning-of-line))
	(if (and (eolp) (not (bolp))) (backward-char))))))

(defun evi-motion-command (move-function direction count context &optional arg)
  (if evi-signal-abort
    (if (not context)
      (setq evi-signal-abort nil))
    (progn
      (if context
	(set-mark (point))
	; else, maintain the goal column.  kinda gross this being here, but...
	(if (or (eq move-function 'do-evi-next-line)
		(eq move-function 'do-evi-previous-line))
	  (if evi-reset-goal-column
	    (progn (setq evi-goal-column (current-column))
		   (setq evi-reset-goal-column nil)))
	  (setq evi-reset-goal-column t)))
      (setq evi-error-string nil)
      (if arg
	(funcall move-function arg count context)
	(funcall move-function count context))
      (if context
	(progn
	  (if (< (point) (mark)) (exchange-point-and-mark))
	  (if (or (eq direction 'vertical) (eq context 'whole-lines))
	    (evi-expand-region-to-lines context)
	    (progn (setq evi-region-whole-lines nil)
		   (if (eq context 'to-next)
		     (evi-normalize-region)))))
	(progn
	  ; present any error messages
	  (if evi-error-string
	    (message evi-error-string))
	  ; fixup the location of the cursor, if necessary
	  (evi-fixup-cursor direction))))))

;; Simple motion commands

(defmotion horizontal evi-forward-char (&optional count context)
  "Move right COUNT characters on the current line."
  (forward-char (save-excursion
		  (set-mark (point))
		  (end-of-line)
		  (min (or count 1) (- (point) (mark)))))
  (if (and (eolp) (not context)) (setq evi-error-string "End of line")))

(defmotion horizontal evi-backward-char (&optional count context)
  "Move left COUNT characters on the current line."
  (backward-char (save-excursion
		   (set-mark (point))
		   (beginning-of-line)
		   (min (1- (or count 1)) (- (mark) (point)))))
  (if (bolp) (setq evi-error-string "Beginning of line") (backward-char)))

(defun evi-next-line-internal (count)
  (if (> count 0)
      (progn (end-of-line)
	     (if (or (eobp) (progn (forward-char) (eobp)))
	       (progn
		 (if (eq (preceding-char) ?\n) (backward-char))
		 (setq evi-error-string "Last line in buffer"))
	       (evi-next-line-internal (1- count))))))

;; ZZ'd[jk]' aren't strict yet - on the first and last line,
;; it should just be an error
(defmotion vertical evi-next-line (&optional count context)
  "Go to ARGth next line."
  (evi-next-line-internal (or count 1))
  (if (not context)
    (move-to-column evi-goal-column)))

(defmotion vertical evi-beginning-of-next-line (&optional count context)
  "Go to beginning of ARGth next line."
  (evi-next-line-internal (or count 1))
  (skip-chars-forward " \t"))

(defun evi-previous-line-internal (count)
  (if (> count 0)
      (progn (beginning-of-line)
	     (if (bobp)
	       (setq evi-error-string "First line in buffer")
	       (progn (backward-char)
		      (evi-previous-line-internal (1- count)))))))

(defmotion vertical evi-previous-line (&optional count context)
  "Go to ARGth previous line."
  (evi-previous-line-internal (or count 1))
  (if (eq context nil)
    (move-to-column evi-goal-column)))

(defmotion vertical evi-beginning-of-previous-line (&optional count context)
  "Go to beginning of ARGth previous line."
  (evi-previous-line-internal (or count 1))
  (back-to-indentation))

(defmotion vertical evi-goto-line (&optional count context)
  "Go to line number LINE, or to end of file if no count specified."
  ; ZZ once again... if we know the move won't be far (like on same screen)
  ; perhaps shouldn't push context...
  (evi-push-context)
  (if count
      (let ((p (point)))
	(goto-char (point-min))
	(if (or (> (forward-line (1- count)) 0) (eobp))
	    (progn
	      (setq evi-error-string "Last line in buffer")
	      (goto-char p))))
    (goto-char (point-max))
    (forward-line -1)))

(defmotion vertical evi-goto-top-of-window (&optional offset context)
  "Go to the top line of the window.  With an arg, OFFSET, goes to the
OFFSET'th line of the window."
  (move-to-window-line (1- (or offset 1)))
  (or context
      (skip-chars-forward " \t")))

(defmotion vertical evi-goto-middle-of-window (&optional offset context)
  "Go to the middle line of the window."
  (move-to-window-line (/ (window-height) 2))
  (or context
      (skip-chars-forward " \t")))

(defmotion vertical evi-goto-bottom-of-window (&optional offset context)
  "Go to the bottom line of the window.  With an arg, OFFSET, goes to the
OFFSET'th line from the bottom of the window."
  (move-to-window-line (- (1- (window-height)) (or offset 1)))
  (or context
      (skip-chars-forward " \t")))

(defmotion horizontal evi-goto-column (&optional column context)
  "Go to column COLUMN, or as close to that column as possible."
  (move-to-column (1- column)))

(defmotion vertical evi-whole-lines (&optional count context)
  "Go ARG - 1 lines forward."
  (evi-next-line-internal (1- (or count 1))))

(defmotion horizontal evi-beginning-of-line (&optional count context)
  "Go to beginning of line."
  (beginning-of-line))

; it's not at all clear why this doesn't take a count...
; maybe it should...
(defmotion horizontal evi-goto-indentation (&optional count context)
  "Go to beginning of indented text on current line."
  (beginning-of-line)
  (back-to-indentation))
 
(defmotion horizontal evi-end-of-line (&optional count context)
  "Go to end of line."
  (evi-next-line-internal (1- (or count 1)))
  (end-of-line))

;; Word, sentence, paragraph and section motion commands

;; Note - vi's word motion commands are often buggy in implementation
;; (most hang up on a line with whitespace on the end).  The only one I
;; have tried which didn't hang up, did the wrong thing on non-empty lines
;; that only contain whitespace (it skipped over the entire line, which is
;; inconsistent with its penchant for stopping on empty lines).  Hence,
;; evi uses my own notion of what was intended: every line contains at
;; least one word.  If the line is empty other than whitespace, that word
;; starts at the beginning of the line or ends at the end of the line.
;; Otherwise, there are three character classes: whitespace (W), alphanumeric
;; including underscore (A), and punctuation (P).  A word beginning boundary
;; is then WA, WP, AP, or PA, and a word ending boundary is AW, PW, AP, or PA.
;; ZZ - might give some alternative word definitions
;; ZZ - there is a small bugger if in the middle of a whitespace line and
;; do a 'e'

;; Hmm... turns out similar things can be said about sentence, paragraph, and
;; section motion.  Actually my interpretations differ somewhat from vi's
;; here, but I couldn't bring myself to implement what I was seeing.  Complain
;; if you think I missed the boat.

(defconst evi-word-beginning "[ \t\n][^ \t\n]\\|\n[ \t]*\n\\|[a-zA-Z0-9_][^a-zA-Z0-9_ \t\n]\\|[^a-zA-Z0-9_ \t\n][a-zA-Z0-9_]")

(defconst evi-word-backwards-beginning "[a-zA-Z0-9_][^a-zA-Z0-9_ \t\n]\\|[^a-zA-Z0-9_ \t\n][a-zA-Z0-9_]\\|\\([ \t\n]\\|\\`\\)[^ \t\n]\\|\n[ \t]*$")

(defconst evi-word-change-beginning "[ \t\n][^ \t]\\|[^ \t\n][ \t\n]\\|[a-zA-Z0-9_][^a-zA-Z0-9_ \t\n]\\|[^a-zA-Z0-9_ \t\n][a-zA-Z0-9_]")

(defconst evi-word-delete-beginning "[^ \t\n]?\\([ \t]+[^ \t]\\|\n\\)\\|[a-zA-Z0-9_][^a-zA-Z0-9_ \t\n]\\|[^a-zA-Z0-9_ \t\n][a-zA-Z0-9_]")

(defconst evi-word-ending "[^ \t\n][ \t\n]\\|^[ \t]*\n\\|[a-zA-Z0-9_][^a-zA-Z0-9_ \t\n]\\|[^a-zA-Z0-9_ \t\n][a-zA-Z0-9_]")

(defconst evi-white-word-beginning "[ \t\n][^ \t\n]\\|\n[ \t]*\n")

(defconst evi-white-word-backwards-beginning "\\([ \t\n]\\|\\`\\)[^ \t\n]\\|\n[ \t]*$")

(defconst evi-white-word-change-beginning "[ \t\n][^ \t]\\|[^ \t\n][ \t\n]")

(defconst evi-white-word-delete-beginning "[^ \t\n]?\\([ \t]+[^ \t]\\|\n\\)")

(defconst evi-white-word-ending "[^ \t\n][ \t\n]\\|^[ \t]*\n")

(defmotion horizontal evi-forward-word (&optional count context)
  "Move to the beginning of the COUNTth next word."
  (evi-forward-word-internal (or count 1) context
    evi-word-beginning (if (eq context 'to-next)
			 evi-word-delete-beginning
			 evi-word-change-beginning)))

(defmotion horizontal evi-forward-white-word (&optional count context)
  "Move to the beginning of the COUNTth next white-space delimited word."
  (evi-forward-word-internal (or count 1) context
    evi-white-word-beginning (if (eq context 'to-next)
			       evi-white-word-delete-beginning
			       evi-white-word-change-beginning)))

(defun evi-forward-word-internal (count match-end pattern end-pattern)
  ; might be nice if searches could overlap... be real easy too!
  ; turns out that wouldn't help as the whole search fails
  ; when the nth occurence isn't found (we want it to go as far as it can)
  (let ((good-so-far 
	 (evi-iterate (1- count)
	   (if (re-search-forward pattern nil t)
	     ; a backward-char would do, but would mess up the error case
	     (goto-char (1+ (match-beginning 0)))
	     (evi-break)))))
    (or
      (and
	; must search to the end regardless
        (if match-end
	  (if (re-search-forward end-pattern nil t)
	    (progn (backward-char)
		   t))
	  t)
	good-so-far
	(or match-end
	    (if (re-search-forward pattern nil t)
	      (progn (goto-char (1+ (match-beginning 0)))
		     t))))
      (setq evi-error-string "End of buffer"))))

(defmotion horizontal evi-end-of-word (&optional count context)
  "Move to the end of the COUNTth next word."
  (evi-end-of-word-internal (or count 1) context evi-word-ending))

(defmotion horizontal evi-end-of-white-word (&optional count context)
  "Move to the end of the COUNTth next whitespace delimited word."
  (evi-end-of-word-internal (or count 1) context evi-white-word-ending))

(defun evi-end-of-word-internal (count context pattern)
  (if (evi-iterate count
	(forward-char)
	(if (re-search-forward pattern nil t)
	  (progn (backward-char)
		 (if (not (and (bolp) (eolp)))
		   (backward-char)))
	  (evi-break)))
    (if context
	(forward-char))
    (setq evi-error-string "End of buffer")))

(defmotion horizontal evi-backward-word (&optional count context)
  "Move to the beginning of the COUNTth previous word."
  (evi-backward-word-internal
    (or count 1) context evi-word-backwards-beginning))

(defmotion horizontal evi-backward-white-word (&optional count context)
  "Move to the beginning of the COUNTth previous whitespace delimited word."
  (evi-backward-word-internal
    (or count 1) context evi-white-word-backwards-beginning))

(defun evi-backward-word-internal (count context pattern)
  (or (evi-iterate (or count 1)
	(and (bolp) (not (bobp))
	  (backward-char))
	(if (re-search-backward pattern nil t)
	  (goto-char (+ (match-beginning 0)
			(if (and (bobp) (match-beginning 1)) 0 1)))
	  (evi-break)))
    (setq evi-error-string "Beginning of buffer")))

(defconst evi-sentence-beginning "\\([.?!][]\"')]*\\($\\|\t\\| [ \t\n]\\)\\|^[ \t]*\n\\)[ \t\n]*")

(defconst evi-sentence-backwards-beginning "\\([.?!][]\"')]*\\($\\|\t\\| [ \t\n]\\)\\|^[ \t]*\n\\|\\`\\)[ \t\n]*[^ \t\n]")

(defconst evi-sentence-change-ending "\\([.?!][]\"')]*\\($\\|\t\\| [ \t\n]\\)\\|^[ \t]*$\\)")

(defconst evi-sentence-delete-ending "\\([.?!][]\"')]*\\( ?$\\|\t[ \t]*\\| [ \t]+\\)\\|^[ \t]*$\\)")

(defmotion horizontal evi-forward-sentence (&optional count context)
  "Move to the beginning of the COUNT'th next sentence."
  (let ((starting-point (point)))
    ; this gives us more context to go on...
    (skip-chars-backward " \t")
    (let ((good-so-far 
	   (evi-iterate (1- (or count 1))
	     (or (re-search-forward evi-sentence-beginning nil t)
		 (evi-break)))))
      (or
	(and
	  ; must search to the end regardless
	  (if context
	    (if (re-search-forward
		  (if (eq context 'to-next)
		    evi-sentence-delete-ending
		    evi-sentence-change-ending) nil 1)
	      (progn (skip-chars-backward " \t")
		     (if (and (eq context 'to-next)
			      (or (<= (point) starting-point) (not (bolp))))
		       (goto-char (match-end 0))
		       (backward-char))
		     t))
	    t)
	  good-so-far
	  (or context
	      (and (re-search-forward evi-sentence-beginning nil t)
		   (or (not (eobp))
		       (progn (goto-char starting-point)
			      nil)))))
	(setq evi-error-string "End of buffer")))))

(defmotion horizontal evi-backward-sentence (&optional count context)
  "Move to the beginning of the COUNT'th previous sentence."
  (or (evi-iterate (or count 1)
	(if (re-search-backward evi-sentence-backwards-beginning nil t)
	  (goto-char (1- (match-end 0)))
	  (evi-break)))
      (setq evi-error-string "Beginning of buffer")))

(defconst evi-paragraph-beginning "^[ \t]*\n[ \t\n]*[^ \t\n]")

(defconst evi-paragraph-backwards-beginning "\\(^[ \t]*\n\\|\\`\\)[ \t\n]*[^ \t\n]")

(defconst evi-paragraph-change-ending "^[ \t]*$")

(defconst evi-paragraph-delete-ending "\n\\([ \t]*\n\\)+")

(defmotion horizontal evi-forward-paragraph (&optional count context)
  "Move to the beginning of the COUNT'th next paragraph."
  (let ((starting-point (point)))
    ; this gives us more context to go on...
    (skip-chars-backward " \t")
    (let ((good-so-far 
	   (evi-iterate (1- (or count 1))
	     (or (re-search-forward evi-paragraph-beginning nil t)
		 (evi-break)))))
      (or
	(and
	  ; must search to the end regardless
	  (if context
	    (if (re-search-forward
		  (if (eq context 'to-next)
		    evi-paragraph-delete-ending
		    evi-paragraph-change-ending) nil 1)
	      (progn (backward-char)
		     t))
	    t)
	  good-so-far
	  (or context
	      (and (re-search-forward evi-paragraph-beginning nil t)
		   (progn (backward-char)
			  t))))
	(setq evi-error-string "End of buffer")))
    (if (< (point) starting-point)
      (goto-char starting-point))))

(defmotion horizontal evi-backward-paragraph (&optional count context)
  "Move to the beginning of the COUNT'th previous paragraph."
  (or (evi-iterate (or count 1)
	(if (re-search-backward evi-paragraph-backwards-beginning nil t)
	  (goto-char (1- (match-end 0)))
	  (evi-break)))
      (setq evi-error-string "Beginning of buffer")))

(defconst evi-section-beginning "\\`\\(.\\|\n\\)\\|^\\([{]\\|\\.\\(NH\\|SH\\|H\\|HU\\|nh\\|sh\\)[ \t\n]\\)")

(defconst evi-section-change-ending "[ \t\n]*\n\\([}]\\|\\.\\(NH\\|SH\\|H\\|HU\\|nh\\|sh\\)[ \t\n]\\)")

(defconst evi-section-delete-ending "^\\(}.*[ \t\n]*\\|\\.\\(NH\\|SH\\|H\\|HU\\|nh\\|sh\\)[ \t\n]\\)")

(defmotion horizontal evi-forward-section (&optional count context)
  "Move to the beginning of the COUNT'th next section."
  (let ((starting-point (point)))
    (forward-char)
    (let ((good-so-far 
	   (evi-iterate (1- (or count 1))
	     (or (re-search-forward evi-section-beginning nil t)
		 (evi-break)))))
      (or
	(and
	  ; must search to the end regardless
	  (if context
	    (if (re-search-forward (if (eq context 'to-next)
				     evi-section-delete-ending
				     evi-section-change-ending) nil 1)
	      (progn (if (eq context 'to-next)
		       (progn (beginning-of-line)
			      (backward-char))
		       (or (eq (preceding-char) ?})
			   (goto-char (match-beginning 0))))
		     t))
	    t)
	  good-so-far
	  (or context
	      (and (re-search-forward evi-section-beginning nil t)
		   (if (eobp)
		     (progn (goto-char starting-point)
			    nil)
		     (progn (goto-char (match-beginning 0))
			    t)))))
	(progn
	  (setq evi-error-string "End of buffer")
	  (goto-char (point-max))
	  (evi-fixup-cursor 'vertical)
	  )))
    (if (/= (point) (1+ starting-point))
      (evi-push-context starting-point))))

(defmotion horizontal evi-backward-section (&optional count context)
  "Move to the beginning of the COUNT'th previous section."
  (let ((starting-point (point)))
    (or (evi-iterate (or count 1)
	  (if (re-search-backward evi-section-beginning nil t)
	    (goto-char (match-beginning 0))
	    (evi-break)))
	(setq evi-error-string "Beginning of buffer"))
    (if (/= (point) starting-point)
      (evi-push-context starting-point))))

(defun evi-region ()
  "Define region bounded by mark and point."
  (interactive)
  (if (< (point) (mark)) (exchange-point-and-mark))
  (setq evi-region-whole-lines nil))

; ZZ
(defun evi-context-arg () (list evi-context))

(defun evi-region-whole-lines (context)
  "Define whole lines region bounded by mark and point."
  (interactive (evi-context-arg))
  (if (< (point) (mark)) (exchange-point-and-mark))
  (setq evi-region-whole-lines t)
  (evi-expand-region-to-lines evi-context))

;; Searching

(defmotion horizontal evi-search-forward
  (&string "/" string &optional count context)
  "Search forward for the ARGth occurence of a pattern.  A null string will
repeat the previous search."
  (if (not (string= string ""))
    (setq evi-search-pattern string))
  (if evi-search-pattern
    (evi-do-search (setq evi-search-forward t) evi-search-pattern (or count 1))
    (setq evi-error-string "No previous search pattern")))

(defmotion horizontal evi-search-backward
  (&string "?" string &optional count context)
  "Search backward for the ARGth occurence of a pattern.  A null string will
repeat the previous search."
  (if (not (string= string ""))
    (setq evi-search-pattern string))
  (if evi-search-pattern
    (evi-do-search
      (setq evi-search-forward nil) evi-search-pattern (or count 1))
    (setq evi-error-string "No previous search pattern")))

(defmotion horizontal evi-search-next (&optional count context)
  "Search for the next ARGth occurence of the previous search pattern."
  (if evi-search-pattern
    (evi-do-search evi-search-forward evi-search-pattern (or count 1))
    (setq evi-error-string "No previous search pattern")))

(defmotion horizontal evi-search-next-reverse (&optional count context)
  "Search for the next ARGth occurence of the previous search pattern
but look in the opposite direction."
  (let ((evi-search-forward (not evi-search-forward)))
    (do-evi-search-next count context)))

(defun evi-do-search (search-forward string count)
  (let ((case-fold-search evi-ignore-case)
	(search-string (if evi-search-magic string (evi-rework-magic string)))
	(starting-point (point)))
    (if (if search-forward
	  (evi-search-forward-count search-string count)
	  (evi-search-backward-count search-string count))
      (progn
	; ZZ if we know the search didn't take us far, perhaps we shouldn't
	; push a context...
	(evi-push-context starting-point)
        (goto-char (match-beginning 0)))
      (progn
	(goto-char starting-point)
	(setq evi-error-string
	      (concat
		(if (> count 1) "Nth occurrence not found" "Pattern not found")
		(if evi-search-wraparound ""
		  (if search-forward
		    " before end of file"
		    " before beginning of file"))))))))

;; rework the pattern so that . [ and * become literal, and \. \[ and \*
;; are 'magic' (i.e. behave as . [ and * in a regular expression)
(defun evi-rework-magic (string)
  (let ((offset (string-match "[\\.[*]" string)))
    (if offset
      (if (= (aref string offset) ?\\)
	(let ((next (1+ offset)))
	  (if (> (length string) next)
	    (let ((char (aref string next)))
	      (if (or (= char ?.) (= char ?[) (= char ?*))
		(concat (substring string 0 offset) (char-to-string char)
			(evi-rework-magic (substring string (1+ next))))
		(concat (substring string 0 (1+ next))
			(evi-rework-magic (substring string (1+ next))))))
	    string))
	(concat (substring string 0 offset) "\\"
		(char-to-string (aref string offset))
		(evi-rework-magic (substring string (1+ offset)))))
      string)))

; ZZ use evi-iterate
(defun evi-search-forward-count (string count)
  (if (> count 0)
    (progn (forward-char)
	   (if (re-search-forward string nil t)
	     (evi-search-forward-count string (1- count))
	     (if evi-search-wraparound
	       (progn (goto-char (point-min))
		      (if (re-search-forward string nil t)
			(evi-search-forward-count string (1- count)))))))
    t))

(defun evi-search-backward-count (string count)
  (if (> count 0)
    (if (re-search-backward string nil t)
      (evi-search-backward-count string (1- count))
      (if evi-search-wraparound
	(progn (goto-char (point-max))
	       (if (re-search-backward string nil t)
		 (evi-search-backward-count string (1- count))))))
    t))

(defmotion horizontal evi-find-character (&char char &optional count context)
  "Search for CHAR on the current line.  With COUNT find the COUNT'th occurance."
  (setq evi-find-character char)
  (setq evi-find-forward t)
  (setq evi-find-up-to nil)
  (evi-find-character-internal (or count 1) context))

(defmotion horizontal evi-find-character-backwards
  (&char char &optional count context)
  "Search backwards for CHAR on the current line.  With COUNT find the
COUNT'th occurance."
  (setq evi-find-character char)
  (setq evi-find-forward nil)
  (setq evi-find-up-to nil)
  (evi-find-character-backwards-internal (or count 1) context))

(defmotion horizontal evi-find-character-before
  (&char char &optional count context)
  "Search for CHAR on the current line and leave the cursor on the character
before it.  With COUNT find the COUNT'th occurance."
  (setq evi-find-character char)
  (setq evi-find-forward t)
  (setq evi-find-up-to t)
  (evi-find-character-internal (or count 1) context))

(defmotion horizontal evi-find-character-backwards-after
  (&char char &optional count context)
  "Search backwards for CHAR on the current line and leave the cursor on
the character after it.  With COUNT find the COUNT'th occurance."
  (setq evi-find-character char)
  (setq evi-find-forward nil)
  (setq evi-find-up-to t)
  (evi-find-character-backwards-internal (or count 1) context))

(defmotion horizontal evi-find-next-character (&optional count context)
  "Search for the next COUNT'th occurence of the previous search character."
  (if evi-find-character
    (if evi-find-forward
      (evi-find-character-internal (or count 1) context)
      (evi-find-character-backwards-internal (or count 1) context))
    (setq evi-error-string "No previous search character")))

(defmotion horizontal evi-find-next-character-reverse (&optional count context)
  "Search for the next COUNT'th occurence of the previous search character
in the opposite direction."
  (let ((evi-find-forward (not evi-find-forward)))
    (do-evi-find-next-character count context)))

(defun evi-find-character-internal (count context)
  (forward-char)
  (let ((case-fold-search nil))
    (if (search-forward (char-to-string evi-find-character)
			(save-excursion (end-of-line) (point)) t count)
      (if evi-find-up-to
	(backward-char))
      (setq evi-error-string "No more occurences on this line")))
  (or context
      (backward-char)))

(defun evi-find-character-backwards-internal (count context)
  (let ((case-fold-search nil))
    (or (search-backward (char-to-string evi-find-character)
			 (save-excursion (beginning-of-line) (point)) t count)
	(setq evi-error-string "No more occurences on this line")))
  (if evi-find-up-to
    (forward-char)))

;(defmacro evi-left-paren () ?()
;(defmacro evi-left-brace () ?{)
;(defmacro evi-left-bracket () ?[)
;(defmacro evi-right-paren () ?))
;(defmacro evi-right-brace () ?})
;(defmacro evi-right-bracket () ?])

(defmotion horizontal evi-paren-match (&optional count context)
  "Move cursor to matching parenthesis, brace or bracket."
  (let ((end-point (save-excursion (end-of-line) (point))))
    (if (re-search-forward "[][(){}]" end-point t)
      (progn (backward-char)
	     (if (looking-at "[({[]")
	       (progn (forward-sexp 1)
		      (or context (backward-char)))
	       (progn (forward-char)
		      (if context (set-mark (1+ (mark))))
		      (backward-sexp 1))))
      (setq evi-error-string "Nothing on rest of line to balance"))))

;; Repeating

(defun evi-repeat ()
  "Repeat last modifying command."
  (interactive)
  (execute-kbd-macro evi-last-command-keys))

(defun evi-prompt-repeat ()
  "Print last modifying command."
  (interactive)
  (let ((command (read-string "Repeat: " evi-last-command-keys)))
    (execute-kbd-macro command)
    (setq evi-last-command-keys command)))

;; Prefix counts

(defun evi-read-number (prefix-value)
  (let ((char (evi-read-char)))
    (if (and (>= char ?0) (<= char ?9))
	(evi-read-number (+ (* prefix-value 10) (- char ?0)))
      (progn (evi-unread-char char)
	     prefix-value))))

(defun evi-prefix-digit ()
  "Prefix count."
  (interactive)
  (let ((evi-prefix-count (* evi-prefix-count-multiplier
			     (evi-read-number (- last-input-char ?0)))))
    (evi-get-command)))

;; Registers

(defun evi-prefix-register ()
  "Prefix register."
  (interactive)
  (let* ((char (evi-read-char))
	 (evi-register (cons (evi-register-number char)
			     (not (and (>= char ?a) (<= char ?z))))))
    (evi-get-command)))

(defun evi-register-number (char)
  (if (or (and (>= char ?a) (<= char ?z))
	  (and (>= char ?A) (<= char ?Z))
	  (and (>= char ?0) (<= char ?9)))
    (cond ((and (>= char ?a) (<= char ?z)) (+ (- char ?a) 10))
	  ((and (>= char ?A) (<= char ?Z)) (+ (- char ?A) 10))
	  (t (% (+ evi-digit-register (- char ?1)) 9)))
    (error "Invalid register name")))

(defun evi-register-name (register-struct)
  (let ((register-number (car register-struct)))
    (if (> register-number 9)
      (+ register-number (- ?a 10))
      (+ register-number ?1))))

(defun evi-copy-region-to-registers (number-register-also)
  (let ((string (buffer-substring (mark) (point))))
    (aset evi-registers
	  evi-register-unnamed (cons string evi-region-whole-lines))
    (if evi-register
      (aset evi-registers (car evi-register)
	    (if (cdr evi-register)
	      (let ((register (aref evi-registers (car evi-register))))
		(cons (concat (car register) string) (cdr register)))
	      (cons string evi-region-whole-lines))))
    (if number-register-also
      (progn (aset evi-registers
		   evi-digit-register (cons string evi-region-whole-lines))
	     (setq evi-digit-register (% (1+ evi-digit-register) 9))))))

;; Undoing

(defun evi-undo ()
  "Undo previous change."
  (interactive)
  (message "undo!")
  (undo-start)
  ; if the first record is a boundary, skip it
  ; ZZ maybe make this a while?
  (if (null (car pending-undo-list))
    (setq pending-undo-list (cdr pending-undo-list)))
  (evi-undo-one-change)
  (evi-fixup-cursor 'vertical))

; ZZ should make this robust wrt U before u.
;(defun evi-undo-more ()
;  "Continue undoing previous changes."
;  (interactive)
;  (if (boundp 'pending-undo-list)
;    (progn (message "undo more!")
;	   (evi-undo-one-change)
;	   (evi-fixup-cursor 'vertical))
;    (error "No previous undo to continue")))


(defconst evi-last-changed-line nil)

(defun evi-remember-last-changed-line ()
  (save-excursion
    (beginning-of-line)
    (let ((p (point-marker)))
      (end-of-line)
      (or (equal p (car evi-last-changed-line))
	  (setq evi-last-changed-line (cons p buffer-undo-list))))))

(defun evi-tailp (cdr list)
  ;; whether cdr is eq to a cdr of list.
  (let ((more t))
    (while (if (eq cdr list)
	       (setq more nil)
	     (consp list))
      (setq list (cdr list)))
    (not more)))

(defun evi-undo-all ()
  "Undo all changes to current line"
  (interactive)
  (beginning-of-line)
  (let ((p (car evi-last-changed-line))
	(undo-ptr (cdr evi-last-changed-line)))
    (if (not (and p
		  (eq (current-buffer) (marker-buffer p))
		  (= (point) p)))
	(error "no undo information for this line")
      (undo-start)
      (if (eq pending-undo-list undo-ptr) ; doesn't happen?
	  (error "no more undo information for this line"))
      (if (not (evi-tailp undo-ptr pending-undo-list))
	  (error "undo lost"))
      (while (and pending-undo-list (evi-tailp undo-ptr pending-undo-list))
	(undo-more 1))
      (undo-start)
      (setq evi-last-changed-line nil)
      ;;(evi-remember-last-changed-line)
      (beginning-of-line))))

(defun evi-undo-one-change ()
  (let ((modified (buffer-modified-p)))
    (undo-more 1)
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary))))

;; well ain't this gross...  I hope I'm overlooking some better way
;; of doing this instead of patching things up after the fact...
; this was nice, but elisp isn't too big on recursion (dig that - a lisp
; that doesn't handle recursion well - kinda makes you wonder)
;(defun evi-remove-undo-boundaries (undo-list sentinal)
;  (if (eq undo-list sentinal)
;    undo-list
;    (if (null (car undo-list))
;      (evi-remove-undo-boundaries (cdr undo-list) sentinal)
;      (cons (car undo-list)
;	    (evi-remove-undo-boundaries (cdr undo-list) sentinal)))))

; and here's the new improved, makes-me-wanna-gag, C-style version
; (got nothing against C, just if I was going to code C, I might as well
; use C...)
; (suggestions for improvement are welcome!)
(defun evi-remove-undo-boundaries (undo-list sentinal)
  (let ((prev nil)
	(new-undo-list nil))
    (evi-enumerate-condition head undo-list (not (eq list sentinal))
      (if (null head)
	(if (null prev)
	  (setq new-undo-list (cdr list))
	  (setcdr prev (cdr list)))
	(progn (if (null new-undo-list)
		 (setq new-undo-list list))
	       (setq prev list))))
    new-undo-list))

;; Marks

(defun evi-mark (char)
  "Mark location."
  (interactive "c")
  (cond ((and (>= char ?a) (<= char ?z))
	  (aset evi-registers (+ (- char ?a) 36) (point-marker)))
	((eq char ?.)
	  (set-mark (point)))))

(defmotion horizontal evi-goto-mark-horizontal (&optional count context)
  "Goto a mark."
  (evi-goto-mark-internal (evi-read-char) context))

(defmotion vertical evi-goto-mark-vertical (&optional count context)
  "Goto a mark.  If an operand, define a whole lines region."
  (evi-goto-mark-internal (evi-read-char) context)
  (if (eq context nil)
    (back-to-indentation)))

; ZZ - it might be useful to have a form where you could move among the
; last n contexts in all buffers...
(defun evi-goto-mark-internal (char &optional context)
  (cond ((and (>= char ?a) (<= char ?z))
	  (let ((marker (aref evi-registers (+ (- char ?a) 36))))
	    (if (not (eq (current-buffer) (marker-buffer marker)))
	      (progn (switch-to-buffer (marker-buffer marker))
		     ; unpleasant, but best we can do... (?)
		     (if context (set-mark (point)))))
	    (goto-char marker)))
	((or (eq char ?`) (eq char ?'))
	  (goto-char (evi-exchange-context)))
	((eq char ?.)
	  (goto-char (evi-pop-context)))
	((eq char ?,)
	  (goto-char (evi-unpop-context)))))

(defun evi-push-context (&optional offset)
  (let ((marker (if offset (set-marker (make-marker) offset) (point-marker))))
    (aset evi-context-ring evi-context-ring-cursor marker)
    (setq evi-context-ring-cursor
	  (if (= evi-context-ring-cursor 9) 0 (1+ evi-context-ring-cursor)))))

(defun evi-pop-context ()
  (setq evi-context-ring-cursor
    (if (= evi-context-ring-cursor 0) 9 (1- evi-context-ring-cursor)))
  (aref evi-context-ring evi-context-ring-cursor))

(defun evi-unpop-context ()
  (setq evi-context-ring-cursor
    (if (= evi-context-ring-cursor 9) 0 (1+ evi-context-ring-cursor)))
  (aref evi-context-ring evi-context-ring-cursor))

(defun evi-exchange-context ()
  (let ((cursor
	 (if (= evi-context-ring-cursor 0) 9 (1- evi-context-ring-cursor))))
    (prog1 (aref evi-context-ring cursor)
	   (aset evi-context-ring cursor (point-marker)))))

;; Misc

; ZZ - should columns be zero or one based?
(defun evi-file-info ()
  "Give information on the file associated with the current buffer."
  (interactive)
  (let* ((line-number (count-lines 1 (1+ (point))))
	 (total-lines (1- (+ line-number (count-lines (point) (point-max)))))
	 (name (buffer-file-name)))
    (message "\"%s\"%s%s line %d of %d, column %d --%d%%--"
	     (or name "")
	     (if (buffer-modified-p) " [Modified]" "")
	     (if (and name (not (file-writable-p name))) " [Read only]" "")
	     line-number
	     total-lines
	     (current-column)
	     (/ (* line-number 100) total-lines))))

(defun evi-tag ()
  "Go to the tag which is the next word in the buffer."
  (interactive)
  (evi-motion-command 'do-evi-forward-word 'horizontal 1 'to-end)
  (ex-tag (buffer-substring (mark) (point))))

(defun evi-redraw-selected-screen ()
  "Clear the screen and redisplay it."
  (interactive)
  (if (fboundp 'redraw-screen)
      (redraw-screen (window-screen (selected-window)))
    (redraw-display)))

(defun evi-save-and-exit ()
  (interactive)
  (save-some-buffers t t)
  (ex-exit))
  

;; Ex

(defun evi-ex-command ()
  "Execute an ex command."
  (interactive)
  (evi-do-ex-command-string (ex-read-command))
  (evi-fixup-cursor 'vertical))

(defvar ex-reading-filename nil)
(defvar ex-restart-mark nil)

; there's some rather nasty hoop-jumping going on just to get file
; completion on :e commands.  hopefully I'm just ignorant and there's
; really a better way of doing this...
(defun ex-read-command ()
  (setq ex-reading-filename nil)
  (setq ex-restart-mark nil)
  (let* ((minibuffer-local-map evi-ex-map)
	 (command (read-string ":")))
    (ex-filename-substitute
     (if ex-restart-mark
	 (let ((initial (substring command 0 ex-restart-mark))
	      (file-start (substring command ex-restart-mark)))
	   (concat initial
		   (completing-read (concat ":" initial)
				    'read-file-name-internal
				    "." nil file-start)))
       command))))
      
(defun ex-filename-substitute (string)
  (if ex-reading-filename
      (let ((percent-place 0)
	    (space-pos (string-match " " string))
	    bufname)
	(if space-pos
	    (let ((cmd-string (substring string 0 (+ space-pos 1))))
	      (setq string (substring string (+ space-pos 1) nil))
	      (while (setq percent-place (string-match "\\(^%\\|[^\\]%\\)"
						       string))
		;; If it matched a non-backslash followed by percent, skip
		;; the non-backslash.
		(if (> percent-place 0)
		    (setq percent-place (1+ percent-place)))
		(cond ((null bufname)
		       (setq bufname (buffer-file-name))
		       (cond ((null bufname)
			      (message "Buffer has no file associated with it")
			      (setq bufname (buffer-name))
			      (cond ((null bufname)
				     (message
				      "Buffer has no name associated with it")
				     (setq bufname "noname")))))))
		(setq string (ex-substitute-for-percent
			      string percent-place bufname)))
	      ;; OK, percents are taken care of.  Now we pass the line 
	      ;; to a shell to expand things like $name, *, ?, etc.
	      (if (string-match "[^a-zA-Z0-9_.-/#+]" string)
		  (save-excursion 
		    (set-buffer ex-work-space)
		    (delete-region (point-min) (point-max))
		    (call-process ex-find-file-shell  nil t nil "-cf"
				  (format "echo -n %s" string))
		    (goto-char (point-min))
		    (if (search-forward " " nil t)
			(error "Too many file names")
		      (setq string (buffer-substring (point-min) (point-max))))))
	      (concat cmd-string string))
	  string))
    string))

(defun ex-substitute-for-percent (name pos sub)
  (concat (substring name 0 pos)
	  sub
	  (substring name (+ pos 1))))

(defun ex-space ()
  (interactive)
  (if ex-reading-filename
    (progn (skip-chars-backward "^ \t")
	   (setq ex-restart-mark (1- (point)))
	   ; ZZ - note the semi-bogus hardcoded space character....
	   (evi-unread-char ? )
	   (exit-minibuffer)))
  (insert ? )
  (save-excursion
    (goto-char (point-min))
    (let ((command (ex-scan-command-name)))
      (if (and command
	       ;; ZZ kinda sloppy...
	       (let ((function (cdr (cdr command))))
		 (or (eq function 'ex-change-directory)
		     (eq function 'ex-edit)
		     (eq function 'ex-edit-other-window)
		     (eq function 'ex-read)
		     (eq function 'ex-source-file)
		     (eq function 'ex-write))))
	(setq ex-reading-filename t)))))

(defun ex-delete-backward-char ()
  (interactive)
  (if (bolp)
    (exit-minibuffer)
    (progn
      (if (and ex-reading-filename (= (preceding-char) ? ))
	(setq ex-reading-filename nil))
      (delete-backward-char 1))))

(defun ex-complete ()
  (interactive)
  (if ex-reading-filename
    (progn (skip-chars-backward "^ \t")
	   (setq ex-restart-mark (1- (point)))
	   ; ZZ - note the bogus hardcoded TAB character here and farther down
	   (evi-unread-char ?\t)
	   (exit-minibuffer))
    (insert ?\t)))

(defvar ex-user-buffer)

(defun evi-do-ex-command-file (filename)
  (if (file-exists-p filename)
    (let ((ex-user-buffer (current-buffer)))
      (set-buffer ex-work-space)
      (delete-region (point-min) (point-max))
      (insert-file filename)
      (goto-char (point-min))
      (evi-do-ex-command)
      (set-buffer ex-user-buffer))))

(defun evi-do-ex-command-string (command-string)
  (let ((ex-user-buffer (current-buffer)))
    (set-buffer ex-work-space)
    (delete-region (point-min) (point-max))
    (insert command-string "\n")
    (goto-char (point-min))
    (evi-do-ex-command)
    (set-buffer ex-user-buffer)))

;; Note - it is expected that the function that calls this one has set
;; ex-user-buffer, and switched to buffer ex-work-space
(defun evi-do-ex-command ()
  (while (not (eobp))
    (let ((command (ex-scan-command)))
      (set-buffer ex-user-buffer)
      (eval command)
      (set-buffer ex-work-space)
      (skip-chars-forward "^|\n")
      (forward-char))))

(defun ex-scan-command ()
  (let* ((addresses (ex-scan-addresses))
	 (command-struct (ex-scan-command-name))
	 (number-of-addresses (car (car (cdr command-struct))))
	 (command-name (car (car command-struct)))
	 (command-prototype (cdr (car (cdr command-struct))))
	 (command-function (cdr (cdr command-struct))))
    (if (null command-struct)
      (error "Unknown ex command"))
    (if (> (ex-count-addresses addresses) number-of-addresses)
      (error "The %s command only needs %d addresses"
	     command-name number-of-addresses))
    (let ((parameter-list (ex-scan-parameter-list command-prototype)))
      (cons command-function
	    (cond ((eq number-of-addresses 1)
		    (cons (list 'quote (car addresses)) parameter-list))
		  ((eq number-of-addresses 2)
		    (cons (list 'quote addresses) parameter-list))
		  (t
		    parameter-list))))))

(defun ex-scan-parameter-list (prototype-list)
  (if prototype-list
    (let ((prototype (cdr (car prototype-list)))
	  (skip-white (eq (car (car prototype-list)) t)))
      (if skip-white
	(skip-chars-forward " \t")
	(if (eq (car (car prototype-list)) 'backup)
	  (backward-char)))
      (cons (cond ((null prototype)
		    nil)
		  ((stringp prototype)
		    (ex-scan-string prototype))
		  ((eq prototype 'address)
		    (list 'quote (ex-scan-address)))
		  ((eq prototype 'register)
		    (list 'quote (ex-scan-register)))
		  ((eq prototype 'rest-of-line)
		    (ex-scan-rest-of-line))
		  ((eq prototype 'word)
		    (ex-scan-word))
		  ((eq prototype 'regular-expression)
		    (ex-scan-regular-expression))
		  ((eq prototype 'command)
		    (list 'quote (ex-scan-command)))
		  ((eq prototype 'settings)
		    (list 'quote (ex-scan-settings))))
	    (ex-scan-parameter-list (cdr prototype-list))))))

(defun ex-scan-addresses ()
  (skip-chars-forward " \t")
  (if (= (following-char) ?%)
    (cons (cons (cons 'number 1) 0) (cons (cons 'dollar nil) 0))
    (if (looking-at "[-+0-9.$'/?]")
      (cons
	(ex-scan-address)
	(progn (skip-chars-forward " \t")
	       (if (= (following-char) ?,)
		 (progn (forward-char)
			(skip-chars-forward " \t")
			(ex-scan-address))
		 (cons (cons nil nil) 0))))
      (cons (cons (cons nil nil) 0) (cons (cons nil nil) 0)))))

(defun ex-scan-address ()
  (cons (ex-scan-linespec) (ex-scan-line-offset)))

(defun ex-scan-linespec ()
  (let ((char (following-char)))
    (cond
      ((and (>= char ?0) (<= char ?9))
	(set-mark (point))
	(skip-chars-forward "0-9")
	(cons 'number (string-to-int (buffer-substring (mark) (point)))))
      ((eq char ?.)
	(forward-char)
	(cons 'dot nil))
      ((eq char ?$)
	(forward-char)
	(cons 'dollar nil))
      ((eq char ?')
	(forward-char 2)
	(cons 'mark (preceding-char)))
      ((eq char ?/)
	(cons 're-forward (ex-scan-regular-expression)))
      ((eq char ??)
	(cons 're-backward (ex-scan-regular-expression))))))

(defun ex-scan-regular-expression ()
  (let ((skip-pattern (concat "^\n\\\\" (char-to-string (following-char)))))
    (forward-char)
    (let ((start (point)))
      (skip-chars-forward skip-pattern)
      (while (= (following-char) ?\\ )
	(forward-char 2)
	(skip-chars-forward skip-pattern))
      (prog1 (buffer-substring start (point))
	(if (not (= (following-char) ?\n))
	    (forward-char))))))

(defun ex-scan-line-offset ()
  (let ((char (following-char)))
    (if (or (eq char ?+) (eq char ?-))
      (progn (forward-char)
	     (set-mark (point))
	     (skip-chars-forward "0-9")
	     (let ((offset
		    (string-to-int (buffer-substring (mark) (point)))))
	       (if (eq char ?+)
		 offset
		 (- offset))))
      0)))

;; ZZ maybe recognize here that 0 is invalid?
(defun ex-define-region (addresses whole-lines default-whole-file)
  (let ((start (car addresses))
	(end (cdr addresses)))
    (if (and (null (car (car start))) default-whole-file)
      (progn (set-mark (point-min))
	     (goto-char (point-max)))
      (progn (let ((starting-point (point)))
	       (ex-goto-address start)
	       (set-mark (point))
	       ;; #### is this right?  without it, ":1,.w file" doesn't work
	       (goto-char starting-point)
	       (ex-goto-address end starting-point))
	     (if whole-lines
	       (evi-expand-region-to-lines 'ex))))))

(defun ex-goto-address (address &optional starting-point)
  (let ((token (car (car address)))
	(value (cdr (car address))))
    (cond ((eq token 'number)
	    (goto-line value))
	  ; no action needed for dot
	  ((eq token 'dollar)
	    (goto-char (point-max))
	    (forward-line -1))
	  ((eq token 'mark)
	    (evi-goto-mark-internal value))
	  ((eq token 're-forward)
	    (if starting-point (goto-char starting-point))
	    (forward-line)
	    ;; ZZ this is crying out for better error handling...
	    (setq evi-error-string nil)
	    (do-evi-search-forward value)
	    (if evi-error-string
	      (progn (forward-line -1)
		     (error evi-error-string))))
	  ((eq token 're-backward)
	    (if starting-point (goto-char starting-point))
	    (setq evi-error-string nil)
	    (do-evi-search-backward value)
	    (if evi-error-string
	      (error evi-error-string)))))
  (forward-line (cdr address)))

(defun ex-goto-line-after-address (address)
  (if (null (car (car address)))
    (forward-line)
    (if (and (eq (car (car address)) 'number)
	     (= (cdr (car address)) 0))
      (goto-char (point-min))
      (progn (ex-goto-address address)
	     (forward-line)))))

(defun ex-count-addresses (addresses)
  (if (eq (car (car (car addresses))) nil)
    0
    (if (eq (car (car (cdr addresses))) nil)
      1
      2)))

(defun ex-scan-command-name ()
  (skip-chars-forward " \t")
  (let ((beg (point)))
    (if (looking-at "[a-zA-Z!<=>&@]")
	(progn (forward-char)
	       (let ((char (preceding-char)))
		 (if (or (and (>= char ?a) (<= char ?z))
			 (and (>= char ?A) (<= char ?Z)))
		     (skip-chars-forward "a-zA-Z")))))
    (ex-lookup-command ex-commands (buffer-substring beg (point)))))

(defun ex-lookup-command (command-list command)
  (if command-list
    (if (ex-command-eq command (car (car command-list)))
      (car command-list)
      (ex-lookup-command (cdr command-list) command))))

(defun ex-command-eq (command command-cell)
  (let ((full-command (car command-cell)))
    (or (string= command full-command)
	(let ((command-length (length command)))
	  (and (>= command-length (cdr command-cell))
	       (< command-length (length full-command))
	       (string= command
			(substring (car command-cell) 0 (length command))))))))

(defun ex-scan-register ()
  (if (= (following-char) ?") ; ")
    (progn
      (forward-char 2)
      (let ((char (preceding-char)))
	(cons (evi-register-number char)
	      (not (and (>= char ?a) (<= char ?z))))))
    ; error checking?
    (cons evi-register-unnamed nil)))

(defun ex-scan-rest-of-line ()
  (set-mark (point))
  (skip-chars-forward "^|\n")
  (buffer-substring (mark) (point)))

(defun ex-scan-word ()
  (set-mark (point))
  (skip-chars-forward "^ \t|\n")
  (buffer-substring (mark) (point)))

(defun ex-scan-string (string)
  (let ((string-length (length string)))
    (if (<= string-length
	    (- (save-excursion (skip-chars-forward "^|\n") (point))
	       (point)))
      (let ((buffer-string
	      (buffer-substring (point) (+ (point) string-length))))
	(if (string= string buffer-string)
	  (progn (forward-char string-length)
		 t))))))

(defun ex-not-implemented (&optional arg)
  (message "Command not implemented"))

(defun ex-change-directory (directory-name)
  (setq default-directory (expand-file-name directory-name)))

(defun ex-copy (from-addresses to-address)
  (ex-define-region from-addresses t nil)
  (let ((text (buffer-substring (mark) (point))))
    (ex-goto-line-after-address to-address)
    (insert text)))

(defun ex-delete (addresses register-struct)
  (let ((evi-register register-struct))
    (ex-define-region addresses t nil)
    (evi-copy-region-to-registers t)
    ; to make undo's come out right
    (if (< (mark) (point))
      (exchange-point-and-mark))
    (delete-region (point) (mark))))

(defun ex-edit (exclam file-name)
  (ex-edit-internal exclam file-name nil))

(defun ex-edit-other-window (exclam file-name)
  (ex-edit-internal exclam file-name t))

(defun ex-edit-internal (exclam file-name other-window)
  (if (= (length file-name) 0)
    (if (and (not exclam) (not other-window) (buffer-modified-p))
      (message "Buffer modified since last save (use :edit! to override)")
      (if other-window
	(split-window-vertically)
	(if (null (buffer-file-name))
	  (message "Buffer has no file associated with it")
	  (revert-buffer nil t)
	  (evi-mode))))
    (cond ((string= file-name "#")
	   (let ((buffer (evi-next-file-buffer)))
	     (if buffer
		 (if other-window
		     (switch-to-buffer-other-window (evi-next-file-buffer))
		   (switch-to-buffer (evi-next-file-buffer)))
	       (message "No other file to display"))))
	  (t (if other-window
		 (find-file-other-window file-name)
	       (find-file file-name))))
    (evi-mode)))

(defun ex-file (file-name)
  (if (= (length file-name) 0)
    (evi-file-info)
    (set-visited-file-name file-name)))

(defun ex-global (addresses pattern command)
  (ex-define-region addresses t t)
  (exchange-point-and-mark)
  (let ((case-fold-search evi-ignore-case)
	(next-line-mark (make-marker))
	(end-line-mark (make-marker))
	(large-region (> (- (mark) (point)) 5000)))
    (if large-region
      (message "running global command... "))
    (set-marker end-line-mark (mark))
    (while (< (point) end-line-mark)
      (if (re-search-forward pattern end-line-mark 1)
	(progn
	  ;; check to make sure ex also does this in case of line wrap
	  (goto-char (match-beginning 0))
	  (save-excursion
	    (forward-line)
	    (set-marker next-line-mark (point)))
	  ; (beginning-of-line)
	  (eval command)
	  (goto-char next-line-mark))))
    (if large-region
      (message "running global command... complete."))
    (set-marker next-line-mark nil)
    (set-marker end-line-mark nil)))

;; ZZ needs to save previous mapping...
(defun ex-map (exclam char definition)
  (if exclam
    (evi-define-key evi-all-input-maps char definition)
    (evi-define-key '(vi) char definition)))

(defun ex-move (from-addresses to-address)
  (ex-define-region from-addresses t nil)
  (let ((text (buffer-substring (mark) (point)))
	(to-mark (copy-marker (save-excursion
				(ex-goto-line-after-address to-address)
				(point)))))
    ; to make undo's come out right
    (if (< (mark) (point))
      (exchange-point-and-mark))
    (delete-region (point) (mark))
    (goto-char to-mark)
    (insert text)
    (set-marker to-mark nil)))

(defun ex-print (addresses)
  (let ((position (save-excursion
		    (ex-define-region addresses t nil) (point))))
    (switch-to-buffer-other-window (current-buffer))
    (goto-char position)
    (select-window (previous-window))))

(defun ex-next (exclam)
  (ex-next-internal exclam nil))

(defun ex-next-other-window (exclam)
  (ex-next-internal exclam t))

(defun ex-next-internal (exclam other-window)
  (let* ((next-buffer (evi-next-file-buffer)))
    (if next-buffer
      (progn (bury-buffer (current-buffer))
	     (if other-window
	       (switch-to-buffer-other-window next-buffer)
	       (switch-to-buffer next-buffer)))
      (message "All files are displayed"))))

(defun evi-next-file-buffer ()
  (let ((rest-of-list
	  (evi-enumerate-condition buffer (buffer-list)
	    (or (get-buffer-window buffer)
		(null (buffer-file-name buffer))))))
    (if rest-of-list
      (car rest-of-list))))

(defun ex-put (address register-struct)
  (ex-goto-line-after-address address)
  (let ((register (aref evi-registers (car register-struct))))
    (if register
      (save-excursion
	(insert (evi-register-text register))
	(if (not (evi-register-whole-lines-p register))
	  (insert ?\n)))
      (if evi-register
	(message "Nothing in register %c" (evi-register-name evi-register))
	(message "No text to put")))))


(defvar ex-quit-should-exit-evi nil
  "*If this is t, then the `:q' command in EVI mode will exit VI emulation.
If this is nil (the default), then `:q' will kill the current buffer, leaving
you in EVI mode in the previously selected buffer.  This is closer to the way
that VI users tend to use :q in the real VI.")

(defun ex-quit (discard)
  (if discard
    (set-buffer-modified-p nil))
  (ex-exit))

(defun ex-exit ()
  (interactive)
  (if ex-quit-should-exit-evi
      (save-buffers-kill-emacs)
    (if (eq (current-buffer) ex-user-buffer)
	(setq ex-user-buffer (other-buffer (current-buffer))))
    (if (eq (current-buffer) ex-user-buffer)
	(setq ex-user-buffer (get-buffer-create "*scratch*")))
    (if (eq (current-buffer) ex-user-buffer)
	(erase-buffer)
      (kill-buffer (current-buffer))
      (set-buffer ex-user-buffer))
    (if (eq major-mode 'fundamental-mode) (evi-mode))))


(defun ex-read (address shell-command arg)
  (ex-goto-line-after-address address)
  (if shell-command
    (shell-command arg t)
    (evi-insert-file arg)))

; there's a bug in insert-file-contents that doesn't record an undo save
; boundary when it's appropriate
(defun evi-insert-file (filename)
  ; the insert will record a save record if appropriate
  (insert ?@)
  (delete-region (1- (point)) (point))
  ; now just erase the existence of the insert and delete
  (setq buffer-undo-list (cdr (cdr buffer-undo-list)))
  (insert-file-contents filename))

(defun ex-set (settings)
  (if settings
    (ex-set-internal settings)
    (message "Well set!")))

(defun ex-set-internal (settings)
  (if settings
    (let* ((setting (car settings))
	   (name (car setting))
	   (value (cdr setting)))
      (if (integerp value)
	;; ZZ try princ or (window-buffer (minibuffer-window))
	(progn (princ (evi-get-option name))
	       (princ " "))
	(evi-set-option name value))
      (ex-set-internal (cdr settings)))))

(defun ex-scan-settings ()
  (skip-chars-forward " \t")
  (if (looking-at "[^|\n]")
    (let* ((default-value
	     (if (looking-at "no") (progn (forward-char 2) nil) t))
	   (option
	     (progn (set-mark (point))
		    (skip-chars-forward "a-z")
		    (buffer-substring (mark) (point)))))
      (cond ((looking-at "=")
	      (progn (forward-char 1)
		     (set-mark (point))
		     (skip-chars-forward "^ \t|\n")
		     (cons (cons option (buffer-substring (mark) (point)))
			   (ex-scan-settings))))
	    ((looking-at "?")
	      (progn (forward-char 1)
		     (cons (cons option ??)
			   (ex-scan-settings))))
	    (t
	      (cons (cons option default-value)
		    (ex-scan-settings)))))))

(defun evi-get-option (option)
  (let* ((option-struct (evi-search-option-list evi-option-list option))
	 (type (car (cdr option-struct))))
    (if (eq type nil)
      (error (concat "invalid option: " option))
      (let* ((long-name (car option-struct))
	     (value (eval (cdr (cdr option-struct)))))
	(cond
	  ((eq (cdr (cdr option-struct)) nil)
	    (error "option `%s' not implemented" long-name))
	  ((eq type 'bool)
	    (if (eq value t) long-name (concat "no" long-name)))
	  ((eq type 'number)
	    (concat long-name "=" (int-to-string value)))
	  ((eq type 'string)
	    (concat long-name "=" value))
	  (t
	    (error (concat "invalid type: " (prin1-to-string type)))))))))

(defun evi-set-option (option value)
  (let* ((option-struct (evi-search-option-list evi-option-list option))
	 (type (car (cdr option-struct))))
    (cond
      ((eq type nil)
	(error "invalid option `%s'" option))
      ((eq (cdr (cdr option-struct)) nil)
	;; be gentle for now, just use message
	(message "option `%s' not implemented" (car option-struct)))
      ((eq type 'bool)
        (if (not (or (eq value t) (eq value nil)))
	    (error (concat "only " option " or no" option " allowed"))))
      ((eq type 'number)
        (if (or (eq value t) (eq value nil))
	    (error (concat "use " option "=<number> to set, or " option "? to query"))
	    (setq value (string-to-int value))))
      ((eq type 'string)
        (if (or (eq value t) (eq value nil))
	    (error (concat "use " option "=<string> to set, or " option "? to query"))))
      (t
	(error (concat "invalid type: " (prin1-to-string type)))))
    (if (cdr (cdr option-struct))
      (set (cdr (cdr option-struct)) value))
    (if (fboundp (cdr (cdr option-struct)))
      (funcall (cdr (cdr option-struct)) value))))

(defun evi-search-option-list (option-list option)
  (if option-list
    (let* ((option-struct (car option-list))
	  (option-strings (car option-struct)))
      (if (evi-string-list-match option-strings option)
	  (cons (car option-strings) (cdr option-struct))
	  (evi-search-option-list (cdr option-list) option)))
    '("".nil)))

(defun evi-string-list-match (string-list string)
  (if string-list
    (if (string= string (car string-list))
	t
	(evi-string-list-match (cdr string-list) string))))

(defun ex-source-file (file-name)
  (evi-do-ex-command-file file-name))

(defun ex-substitute (addresses pattern replacement global query)
  (ex-define-region addresses t nil)
  (exchange-point-and-mark)
  (let ((case-fold-search evi-ignore-case)
	(next-line-mark (make-marker))
	(end-line-mark (make-marker))
	(large-region (> (- (mark) (point)) 5000)))
    (if large-region
      (message "running substitute command... "))
    (set-marker end-line-mark (mark))
    (while (< (point) end-line-mark)
      (if (re-search-forward pattern end-line-mark 1)
	(progn
	  (goto-char (match-beginning 0))
	  (save-excursion
	    (if global
	      (goto-char (match-end 0))
	      (forward-line))
	    (set-marker next-line-mark (point)))
	  (ex-replace-match query replacement)
	  (goto-char next-line-mark))))
    (if large-region
      (message "running substitute command... complete."))
    (set-marker next-line-mark nil)
    (set-marker end-line-mark nil)))

(defun ex-replace-match (query replacement)
  (if (or (not query)
	  (let ((beginning (match-beginning 0))
		(end (match-end 0)))
	    (save-excursion
	      (goto-char beginning) (insert ?$)
	      (goto-char (1+ end)) (insert ?$))
	    (prog1
	      (y-or-n-p "replace? ")
	      (save-excursion
		(delete-region beginning (1+ beginning))
		(delete-region end (1+ end))
		(setq buffer-undo-list (nthcdr 4 buffer-undo-list))))))
    (progn (delete-region (match-beginning 0) (match-end 0))
	   (insert replacement))))

(defun ex-tag (tag)
  (if (= (length tag) 0)
    (if (null ex-tag)
      (error "No previous tag specified"))
    (setq ex-tag tag))
  (find-tag ex-tag)
  (evi-mode))

; ZZ exclam overrides readonly...
(defun ex-write (addresses exclam append file-arg)
  (let ((file-name (if (= (length file-arg) 0) (buffer-file-name) file-arg)))
    (save-excursion
      (ex-define-region addresses t t)
      (if (and (= (length file-arg) 0)
	       (= (mark) (point-min)) (= (point) (point-max)))
	(basic-save-buffer)
	(write-region (mark) (point) file-name append)))))

;; Technically, :wq should write the file whether it needs saving or not.
(defun ex-write-quit ()
  (basic-save-buffer)
  (ex-quit nil))

(defun ex-save-quit ()
  (basic-save-buffer)
  (ex-quit nil))

(defun ex-yank (addresses register-struct)
  (let ((evi-register register-struct))
    (save-excursion
      (ex-define-region addresses t nil)
      (evi-copy-region-to-registers nil))))

(defun ex-shell-command (addresses shell-command)
  (if (null (car (car (car addresses))))
    (shell-command shell-command)
    (progn (ex-define-region addresses t nil)
	   (shell-command-on-region (mark) (point) shell-command t))))

(defun ex-shift-right (addresses)
  (ex-define-region addresses t nil)
  (indent-rigidly (mark) (point) evi-shift-width)
  (forward-line -1)
  (skip-chars-forward " \t"))

(defun ex-shift-left (addresses)
  (ex-define-region addresses t nil)
  (indent-rigidly (mark) (point) (- evi-shift-width))
  (forward-line -1)
  (skip-chars-forward " \t"))

(defun ex-null (addresses)
  (ex-define-region addresses t nil)
  (let ((address (car addresses)))
    (if (null (car (car address)))
	(forward-line -1)
      (ex-goto-address address))))

;;; For Lucid GNU Emacs

(defun evi-mouse-track (event)
  (interactive "e")
  (mouse-track event)
  (evi-fixup-cursor 'horizontal)
  (evi-fixup-cursor 'vertical))

(defun evi-mouse-track-insert (event)
  (interactive "e")
  (mouse-track-insert event)
  (evi-fixup-cursor 'horizontal)
  (evi-fixup-cursor 'vertical))

(defun evi-x-mouse-kill (event)
  (interactive "e")
  (x-mouse-kill event)
  (evi-fixup-cursor 'horizontal)
  (evi-fixup-cursor 'vertical))

(defun evi-x-set-point-and-insert-selection (event)
  (interactive "e")
  (x-set-point-and-insert-selection event)
  (evi-fixup-cursor 'horizontal)
  (evi-fixup-cursor 'vertical))

(if evi-new-event-model-p
    (progn
;; Lucid Emacs bindings
(evi-define-key evi-all-keymaps-but-insert 'button1
		'evi-mouse-track)
(evi-define-key evi-all-keymaps-but-insert 'button2
		'evi-x-set-point-and-insert-selection)
(evi-define-key evi-all-keymaps-but-insert 'button3 'energize-popup-menu)
(evi-define-key evi-all-keymaps-but-insert '(control button1)
		'evi-mouse-track-insert)
(evi-define-key evi-all-keymaps-but-insert '(control button2) 'evi-x-mouse-kill)

(evi-define-key evi-all-keymaps-but-insert 'left 'evi-backward-char)
(evi-define-key evi-all-keymaps-but-insert 'right 'evi-forward-char)
(evi-define-key evi-all-keymaps-but-insert 'up 'evi-previous-line)
(evi-define-key evi-all-keymaps-but-insert 'down 'evi-next-line)
))

(evi-define-key evi-all-keymaps-but-insert "\C-z" 'evi-exit-to-emacs)

;; Initializing

(defvar evi-init-files-loaded nil)

(defun evi-load-init-files ()
  (if evi-init-files-loaded
      nil
    (evi-do-ex-command-file "~/.exrc")
    (evi-do-ex-command-file ".exrc")
    (let ((exinit (getenv "EXINIT")))
      (if exinit
	  (evi-do-ex-command-string exinit)))
    (if (file-exists-p "~/.evirc") (load-file "~/.evirc"))
    (if (file-exists-p ".evirc") (load-file ".evirc"))
    (setq evi-init-files-loaded t)))

; Notes...

; maybe use * for multiplier, as in 5*[(...\e

; 'C-c doesn't 'quit' like it should...

; should `up' put the contents of the undo (if the undo deleted text)?

; maybe make re-patterns for )}]] be selected by the major mode.  easy way
; would be to have each be a function that chose based on the mode

; try a general purpose iterator that takes a region, and an update motion...

; undoing can change contexts on you... (my vi doesn't do this tho')

; should shifting zap you back to the beginning of the region if you were
; defining the region using m.>R?

; should check out all opportunities to use &string &char, etc..

; fix ex code to :r doesn't pause so long on the space

; how does goto-mark affect the modification operators?  should they just
; abort, or should they do something handy like allow you to
; 1) yank remote text
; 2) cat text together from different buffers

; C-c can sometimes leave us on a newline

; also relative path movement
; doesn't work (../foo gets recognized as /foo)

; balance-parens does the (] thing wrong

; could try having (interactive (evi-motion-args)) (make evi-interactive-args
; become evi-count-arg).  this would save-excursion, get the motion command
; and make the args be the region start and end.  might be better... might
; not be...
; further could have two versions of passing in count:
; first passes in the raw count, the second defaults count to 1
; save on all those little (or count 1)'s everywhere...

; would be cool if any window switching command took you out of insert
; if you are switching to another window on the same buffer...

; >> doesn't leave the cursor correctly after an undo...

; >R doesn't repeat correctly...

; C-c in insert or replace is mapped to the wrong thing

; switch to insert mode needs to be advertised right away..

; need to wrap delete boundary around insert 'cause it can insert the
; newline

; % will often match to the ` before a paren..

; no longer a message with 'x' on empty line...

; `:e' when the file has been altered on disk, edits the old buffer anyway

; suggest map! for mapping things in insert mode to do emacs'ish stuff
; like map! ^k ^_d$

; the `wrap line with comments' example doesn't work anymore...
; bummer... emacs keyboard macros don't nest!?
; hmmm they nest just fine, but still don't do the right thing
; require termination of command_loop to unwind nesting
; (which hoses any command loops which may have been started by the
; macro!)

