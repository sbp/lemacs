;;; ediff.el --- an FSF & Lucid Emacs 19 visual interface to diff(1) & patch(1)

;; Author: Michael Kifer <kifer@cs.sunysb.edu>
;; Created: February 2, 1994
;; Version: 1.3
;; Keywords: comparing, merging, patching, version control.

;; LCD Archive Entry:
;; ediff|Michael Kifer|kifer@cs.sunysb.edu|
;; Visual interface to diff(1) and patch(1).|
;; 30-Mar-94|1.3|~/packages/ediff.el.Z|

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:
;;  ----------

;; Never read those diff(1) outputs again!
;; Apply patch(1) selectively, like a pro!

;; This package provides a convenient way of simultaneous brousing through
;; the differences between a pair of files or buffers.  The two files being
;; compared (file-A and file-B) are shown in separate windows (side by
;; side, one above the another, or in separate frames), and the differences
;; are highlighted as you step through them.  You can also copy difference
;; regions from one buffer to another (and recover old differences if you
;; change your mind).

;; In addition, Ediff can apply a patch to a file and then let you step
;; though both files, the patched and the original one, simultateously,
;; difference-by-difference.  You can even apply a patch right out of a
;; mail buffer, i.e., patches received by mail don't even have to be saved.
;; Since Ediff lets you copy differences between buffers, you can, in
;; effect, apply patches selectively (i.e., you can copy a difference
;; region from file.orig to file, thereby undoing any particular patch that
;; you don't like).

;; This package is based on emerge.el.  It uses a few utilities and
;; variables defined there and several other Ediff's functions are
;; adaptations from emerge.el.

;; Ediff is complimentary to Emerge.  While Emerge is better at heavy-duty
;; tasks that involve merging of files, Ediff is by far superior
;; for browsing through files compared via diff(1) and for patching files
;; with patch(1).  Furthermore, I feel that Ediff is more convenient for
;; merging tasks where one of the files is a designated output.  This
;; situation arises while patching files or when comparing an old version
;; of a file with a newer version (in such cases, it is often desirable to
;; selectively revert some portions of the new file to its old state). 

;; This version of Ediff is much faster than the previous ones and than
;; Emerge (in Emacs 19.23, Emerge will become much faster as well).
;; The difference in startup time is dramatic for large files with many
;; differences.

;; Window configuration:
;; ----------------------

;; By default, Ediff sets things up in one frame, splitting it between a
;; small control window and the two windows for file-A and file-B.  The
;; split between these latter windows can be horizontal or vertical, which
;; can be changed interactively by hitting 's' while the cursor is in the
;; control window.
;;
;; In a multi-frame situation, Ediff would work as follows.  When it starts,
;; it will place the control window in the frame that was selected at the
;; time of the invocation.  If file-A or file-B is seen in one of the
;; frames, Ediff will leave it there.  If a file (A/B) is not visible in any
;; frame, Ediff will arrange that it will share a frame with the control
;; window. (If none of the files is visible, then both will share the
;; control window frame.) The same algorithm works when you hit 'c'
;; (ediff-recenter), 'p' (ediff-previous-difference), 'n', etc.
;;
;; Thus, you can compare files in one frame or in different frames.
;; The former is done by default, while the latter can be achieved by
;; arranging files A and B to be seen in different frames.  Ediff
;; respects these arrangements, automatically adapting itself to 
;; the multi-frame mode.


;; A note to heavy-duty users: 

;;  Ediff lets the user run multiple sessions at once, i.e., you can invoke
;;  Ediff on different functions several times in a row, without exiting
;;  the previous Ediff sessions. Different sessions may even operate on the
;;  same pair of files.  So, in principle, it is possible to do, say,
;;  pairwise comparison of three (or more) different files.  Each session
;;  would have its own *ediff-control* buffer and all the regarding a
;;  particular session is local to the associated *ediff-control* buffer.
;;  You can switch between sessions by suspending one session and then
;;  switching to another *ediff-control* buffer. (Different such buffers
;;  are distinguished by a numerical suffix, e.g., *ediff-control*<3>.)
;;  This, if you would like to compare three files pairwise, you can do
;;  this by preparing three different frames, each with its own control
;;  window.  (This would require a very wide screen, and I never claimed
;;  that such 3-way comparison is very easy to do.)
;;
;; If you need to conduct multiple Ediff sessions on the same file, one
;; thing should be kept in mind: each time you invoke Ediff on a buffer that
;; already participates in another Ediff session, that buffer should not
;; have any ASCII Ediff flags in it. (Highlighting with faces is OK.)  If
;; flags are not removed, difference overlays won't be set correctly
;; for the second invocation of Ediff.  The simplest way to remove ASCII
;; flags from an Ediff buffer is to switch to that buffer and try to insert
;; or delete something.  If ASCII flags have been inserted by an Ediff
;; session, Ediff will ignore this first editing operation, but it will
;; remove all flags and notify you that this buffer can now be edited.
;;
;; To rehighlight Ediff buffers, hit 'c' in ediff-control buffer.


;;; Remarks: 
;;  -------

;;  1. Ediff is unlikely to run under Emacs 18 without some further work.
;;  2. If running Lucid Emacs, Ediff requires at least version 19.9.
;;  3. I didn't test Ediff on FSF versions older than 19.19.
;;  4. The function vc-ediff requires the vc.el version that comes with
;;     Emacs 19.22.


;;; Installation and use:
;;  --------------------

;; You can invoke Ediff interactively using the following functions:
;;
;;  	    ediff-buffers   	    	    	 - compare buffers
;;  	    ediff   (alias for ediff-files)
;;  	    ediff-files   	    	    	 - compare files
;;  	    ediff-patch-file	    	    	 - patch file then compare
;;  	    epatch  (alias for ediff-patch-file)
;;  	    ediff-patch-buffer	    	    	 - patch buffer then compare
;;  	    vc-ediff 	    	    	    	 - compare buffer & version
;;  	    	    	    	    	    	   using vc.el package
;;  	    	    	    	    	           (Emacs 19.22 and up).
;;  	    rcs-ediff	    	    	    	 - same using rcs.el; rcs.el
;;  	    	    	    	    	    	   is not part of the
;;  	    	    	    	    	    	   standard Emacs distribution.
;;
;; There is also the function ediff-files-remote, which can be invoked only
;; from within another Emacs Lisp function, i.e., non-interactively.
;;
;; To use Ediff, put this in your .emacs file:
;;
;;  (autoload 'ediff-buffers "ediff" "Visual interface to diff(1)" t)
;;  (autoload 'ediff  "ediff"  "Visual interface to diff(1)" t)
;;  (autoload 'ediff-files "ediff" "Visual interface to diff(1)" t)
;;  (autoload 'ediff-files-remote "ediff" "Visual interface to diff(1)") 
;;  (autoload 'epatch  "ediff"  "Visual interface to patch(1)" t)
;;  (autoload 'ediff-patch-file "ediff" "Visual interface to patch(1)" t)
;;  (autoload 'ediff-patch-buffer "ediff" "Visual interface to patch(1)" t)
;;  (autoload 'vc-ediff "ediff"
;;  	    	    	"Interface to diff & version control via vc.el" t) 
;;  (autoload 'rcs-ediff "ediff"
;;  	    	    	 "Interface to diff & version control via rcs.el" t)
;;
;;
;; If you want Ediff to be loaded from the very beginning, you should have
;;
;;  (require 'ediff)
;;
;; in your .emacs file.  This way it is also easier to figure out changes
;; to the default Ediff setting, if such changes become necessary --- see
;; Customization.
;;

;;; Compilation
;;  -----------
;;
;; When you byte-compile Ediff, you will get some warnings about functions
;; being undefined.  These can be safely ignored.
;;
;;   Warning:
;;   =======
;;
;;    If you are using advice.el (directly or indirectly, via one of the
;;    other packages), Ediff may not compile properly.  In this case, you
;;    should do:
;;
;;    	  M-x ad-deactivate-all RET
;;
;;  	  M-x byte-compile-file RET ediff.el RET
;;
;;  	  M-x ad-activate-all RET
;;
;;    This precaution will not be needed starting with GNU Emacs 19.23 and
;;    Lucid Emacs 19.10, due to fixing a bug in advice.el.


;;; Customization:
;;  -------------

;; If you don't like the default setting, you can change it through the
;; various variables and hooks.  In particular, the following hooks are
;; available: 

;;	    ediff-load-hooks
;;  	    ediff-before-setup-windows-hooks
;;  	    ediff-startup-hooks
;;  	    ediff-select-hooks
;;  	    ediff-unselect-hooks
;;  	    ediff-suspend-hooks
;;  	    ediff-quit-hooks
;;  	    ediff-prepare-buffer-hooks

;; The hooks in ediff-load-hooks can be used to change defaults after Ediff
;; is loaded.
;; The hooks in ediff-before-setup-windows-hooks, ediff-suspend-hooks, and
;; ediff-quit-hooks can be used to save and then restore whatever window
;; configuration you want.  However, make sure you understand what you are
;; doing.  Many variables that drive Ediff are local to the different
;; *ediff-control* buffers.  Take a look at ediff-default-suspend-hook and
;; ediff-default-quit-hook to see what's involved.
;; The hooks in ediff-prepare-buffer-hooks are executed for each Ediff buffer
;; (A and B) right after these buffers are arranged.
;;
;; The second group of Ediff variables that could be changed, if you so
;; wish, is: 
;;
;;  	    ediff-before-flag
;;  	    ediff-after-flag
;;
;;  	    ediff-current-diff-face-A
;;  	    ediff-current-diff-face-B
;;  	    ediff-even-diff-face-A
;;  	    ediff-even-diff-face-B
;;  	    ediff-odd-diff-face-A
;;  	    ediff-odd-diff-face-B
;
;; The first two are ASCII strings that mark the beginning and the end of
;; the differences found in file-A and file-B.  The rest are the faces used
;; to highlight text on X displays.  On X displays, Ediff uses
;; ediff-current-diff-face-A and ediff-current-diff-face-B to highlight the
;; current difference regions.  Other (non-current) difference regions are
;; displayed in alternating faces: ediff-even/odd-diff-face-A/B. (In GNU
;; Emacs, the odd and the even faces are actually identical on monochrome
;; displays, because it is rather poor in what you can do on such a
;; display. So, I chose to use italics to highlight other differences. Any
;; ideas would be welcome. In Lucid Emacs, the situation is better
;; because it supports pixmaps.)  There are two ways to change the default
;; setting for highlighting faces: either change the variables, as in
;;
;; (setq ediff-current-diff-face-A (internal-get-face 'bold-italic))
;;
;; (`internal-get-face' should be `get-face' if you are using Lucid Emacs)
;; or by selectively modifying the defaults:
;;
;; (add-hook 'ediff-load-hooks
;;   (function (lambda () 
;;                (set-face-foreground ediff-current-diff-face-B "blue")
;;                (set-face-background ediff-current-diff-face-B "red")
;;                (make-face-italic ediff-current-diff-face-B))))
;;
;; You may also want to take a look at how the above faces are defined in
;; Ediff. 
;;
;; The last variable in this group,
;;
;;  	    ediff-want-faces
;;
;; indicates whether---on a window system---you want differences to be
;; marked using ASCII strings (like on a dumb terminal) or using colors and
;; highlighting.  If you plan on changing these variables, they must be set
;; BEFORE ediff.el is loaded. 
;; Note: Ediff lets you switch between the two types of highlighting.  That
;; is you can switch, interactively, from highlighting using faces to
;; highlighting using ASCII flags, and back.  Of course, toggling has
;; effect only on a window system.  On a dumb terminal or in an xterm
;; window, the only available option is highlighting with ASCII flags.
;;
;; The third group of variables controls miscellaneous functions:
;;
;;  	    ediff-patch-program
;;  	    ediff-patch-options
;;  	    ediff-diff-program
;;  	    ediff-diff-options
;;
;; These specify the functions that produce differences and do patching.
;; The *-options variables specify which options to pass to these programs.
;; It is unlikely that you would want to change these.  One possible
;; exception is when you may want to generate differences with context
;; lines in order to send a patch file through email.  Then, you might want
;; to set ediff-diff-options to "-c".  The output from diff(1) is found in
;; *ediff-diff* buffer.  However, this makes sense only if you also intend
;; to use Ediff to browse through the diff'ed files before sending the
;; patch.  This is because diff.el is much faster in yielding the output of
;; diff(1) ;; (Ediff is a big gun, if used for this simple purpose).
;;
;; The last set of variables that can be modified is
;;
;;  	    ediff-split-window-function
;;  	    ediff-use-last-dir
;;  	    ediff-nix-help-in-control-buffer
;;
;; ediff-split-window-function controls the way you want the window be
;; split between file-A and file-B.  It defaults to vertical split, but you
;; can set it to 'split-window-horizontally, if you want.  Ediff lets you
;; toggle the way windows are split, so you can try different settings
;; interactively.  Note: if file-A and file-B are in different frames,
;; windows are not split, regardless of the value
;; ediff-split-window-function.  Instead, other windows on these frames are
;; deleted and Ediff starts displaying file-A and file-B using these two
;; frames, one file per frame.  You can then switch to one-frame mode
;; simply by hiding the file-A/B buffer that is displayed on a frame other
;; than the control-window frame.
;;
;; Note that if Ediff sees that the two buffers it compares are residing in
;; separate frames, it assumes that the user wants them to be so displayed
;; and stops splitting windows.  Instead, it will arrange each buffer to
;; occupy its own frame (possibly shared with Ediff's help window).
;;
;; The variable ediff-use-last-dir controls the way Ediff presents the
;; default directory when it prompts the user for files to compare.  If nil,
;; Ediff will use the default directory of the current buffer when it
;; prompts the user for file names.  Otherwise, it will use the
;; directories it had previously used for file-A and file-B. 
;;
;; The ediff-nix-help-in-control-buffer, if set to t, makes C-h behave like
;; the DEL key, i.e., it will move you back to the previous difference
;; rather than invoking help.  This is useful when, in an xterm window or on
;; a dumb terminal, the Backspace key is bound to C-h and is positioned
;; more conveniently than the DEL key.


;;; Commands
;;  --------

;; All Ediff commands are displayed in a help window, unless you hit '?' to
;; shrink it to just one line.  You can redisplay the help window by hitting
;; '?' again.
;;
;; Many Ediff commands take numeric prefix arguments.  For instance, if you
;; hit a number, n, and then 'j' (ediff-jump-to-difference), Ediff will
;; take you to n-th difference.  Hitting a number, n, and then 'ab'
;; (ediff-diff-to-diff) will copy n-th difference from buffer A to buffer B.
;; Hitting 'ba' does copying in the other direction.
;; Likewise, a number, n, followed by 'ra' will restore the n-th difference
;; region in buffer A (if it was previously saved as a result of copying
;; from B to A). 
;;
;; Without the prefix argument, all commands operate on the current
;; difference region.
;;
;; The total number of differences and the current difference number are
;; always displayed in the mode line of the control window. 

;;; Display Modes
;;  -------------

;; Ediff can display files in one frame, stacked side-by-side or one on top
;; of another; or it can display the files in different frames.  When you
;; start Ediff, it assumes a 1-frame mode.  You can toggle the side-by-side
;; and one-on-top-of-another displays by simply hitting 's'.
;;
;; Ediff switches to the multi-frame mode when:
;;
;;  1. file-A and file-B are in different frames (you have to put them into
;;     different frames manually); or
;;  2. *ediff-control* buffer is visible in one frame and one other file (A
;;     or B) is visible in another frame.  If, say, fileA is visible in a
;;     different frame than *ediff-control*, fileB doesn't have to be
;;     visible.  If it is, Ediff will continue displaying fileB in the frame
;;     where it was visible before.  If it isn't then Ediff will arrange for
;;     fileB to share a frame with *ediff-control*.
;;
;;  If all three buffers are in separate frames, Ediff will switch to a
;;  3-frame mode.  If Ediff buffers are currently visible only in two
;;  frames, Ediff will work in a 2-frame mode.  In this mode, one of the
;;  frames will be shared by *ediff-control* and file-A or file-B
;;  (whichever is appropriate).


;;; Change Log:
;;  ----------

;; Thu Feb  3, 1994 

;;     Added ediff-read-file-name, which is a stub that takes care of Lemacs
;;     versions of Emerge. (Thanks to Alastair Burt <burt@dfki.uni-kl.de>.)
;;
;;     Fixed a bug in ediff-setup-windows that caused control window to
;;     appear in a wrong place when split-window-keep-point is nil
;;     (Thanks to Kevin Broadey <KevinB@bartley.demon.co.uk>.)
;;
;;     Added mechanism for using faces instead of before/after flags.  This
;;     looks much better on an X display, especially on a color one.
;;     (Thanks to Boris Goldowsky <boris@cs.rochester.edu> for the code
;;     that led to ediff-highlight-diff.
;;     Also, thanks to Kevin Esler <esler@ch.hp.com> for suggestions
;;     regarding highlighting differences on X displays.)
;;
;;     Added functions to apply patches.
;;     (Thanks to Kevin Broadey <KevinB@bartley.demon.co.uk> for this
;;     suggestion.)

;; Fri Feb  4, 1994 

;;     Added mechanism for toggling vertical/horizontal window split.
;;     (Inspired by a suggestion from Allan Gottlieb
;;     <gottlieb@allan.ultra.nyu.edu> -- thanks.)
;;
;;     Added mechanism for toggling between highlighting using faces and
;;     highlighting using ASCII flags.
;;
;;     Fixed a problem with undo.  Now, Ediff has smartened up and doesn't
;;     keep undo info on ASCII flags inserted in buffer-A and buffer-B.
;;     So, if you edit the files while browsing through them, undo behaves
;;     as you would expect, i.e., faces/flags don't get in the way.

;; Sun Feb  6, 1994 

;;     Added horizontal scrolling.  Added ediff-position-region to ensure
;;     that difference regions in buffer-A and buffer-B are aligned with
;;     each other.  Disabled ediff-toggle-split when buffers are displayed
;;     in different frames.

;; Mon Feb  7, 1994

;;     Added toggle-window help (Suggested by Boris Goldowsky
;;     <boris@cs.rochester.edu>.)
;;     Added functions to copy differences from one buffer to another and to
;;     recover old differences.
;;     Added prefix arguments to ediff-next-difference and
;;     ediff-previous-difference.

;; Tue Feb  8, 1994

;;     Replaced text properties with overlays.  Fixed ediff-setup-windows.
;;     Added ediff-save-buffer to local-write-file-hooks to prevent user
;;     from saving corrupted states. (Thanks to <boris@cs.rochester.edu>
;;     for suggestion.)  Instead, Ediff now has a pair of functions for
;;     safe saving of buffers. 
;;     Changed ediff-read-file-name to be more intuitive on ediff-files.
;;     Added ediff-prepare-buffer-hooks. (Thanks to Kevin Esler
;;     <esler@ch.hp.com> for the idea.)

;; Wed Feb  9, 1994

;;     Cleanups in ediff-patch-file.  Protected ediff-copy-diff against
;;     a bug that Emacs has in kill-region.

;; Thu Feb 10, 1994

;;     Added support for Lemacs. (Thanks to Alastair Burt
;;     <burt@dfki.uni-kl.de> for coercing Ediff into working under Lemacs.)
;;     Added ediff-kill-buffer-carefully and other suggestions by Boris
;;     Goldowsky <boris@cs.rochester.edu>.
;;     Refined the protection against interference with highlighting caused
;;     by Hilit19.  Added the variable ediff-third-party-highlighting.
;;     Added mechanisn for unhighlighting regions highlighted with Hilit19
;;     before hightlighting them with Ediff's overlays. (And for
;;     rehighlighting them with Hilit19, when the current difference moves on.)

;; Sun Feb 13, 1994

;;     Added ediff-place-flags-in-buffer and ediff-remote-exit, which are
;;     modifications of Emerge's similar functions.  The difference is that
;;     in Ediff they make ediff-before-flag and ediff-after-flag into
;;     read-only regions, so the user can't change them by mistake.
;;
;;     Adopted a suggestion by Boris Goldowsky <boris@cs.rochester.edu>
;;     that led to a more elegant treatment of faces.
;;
;;     Added protection against interference with Font-Lock highlighting
;;     similar to that of Hilit19's protection.

;; Tue Feb 15, 1994

;;     Deleted spurious (auto-save-mode 1) in ediff-control-buffer, which
;;     was causing this buffer to be auto-saved for no good reason.
;;     Added read-only protection to ediff-before/after-flags in Lemacs.
;;     (Thanks to Alastair Burt <burt@dfki.uni-kl.de> for help in testing.)

;; Wed Feb 16, 1994

;;     Further fixes in the Lemacs part.  Changed highlighted region in
;;     ediff-highlight-diff so that an extra character will be highlighted
;;     only if a difference is empty (thereby allowing the user to see where an
;;     insertion or a deletion has taken place).
;;
;;     Simplified interaction with other highlighting packages by giving
;;     Ediff overlays the highest priority. (Taking a cue from
;;     ediff-highlight-diff-lemacs written by Alastair Burt
;;     <burt@dfki.uni-kl.de>.) Zapped ediff-third-party-highlighting
;;     variable and hooks that were previously used to
;;     unhighlight/rehighlight buffers when hilit19/font-lock are on.

;; Fri Feb 18, 1994

;;     Added a bit more sophistication to ediff-read-file-name.  Now,
;;     ediff-files remembers both, the file-A and the file-B directories.
;;     They are offered as defaults when ediff-use-last-dir is set to t.

;; Fri Feb 22, 1994

;;     Added ediff-before-change-guard to remove ASCII highlighting when
;;     the user attempts to change buffer-A/B.  This is needed because
;;     otherwise the undo info may become screwed up in those buffers.
;;     Hitting 'h' (ediff-toggle-hilit) on a dumb terminal will toggle
;;     between ASCII highlighting and no highlighting.

;; Fri Feb 24, 1994

;;     Fixed problems with multiple Ediff sessions running simultaneously.

;; Tue Mar 1, 1994

;;     Added vc-ediff, the Ediff interface to vc.el. (Thanks to Eric
;;     Freudenthal <freudent@jan.ultra.nyu.edu> for contributing this
;;     function.) 

;; Sun Mar 6, 1994

;;     Added rcs-ediff, an Ediff interface to RCS via rcs.el. (Thanks to
;;     Alastair Burt  <burt@dfki.uni-kl.de>.)
;;     Some minor improvements.

;; Tue March 15, 1994

;;     Fixed a buglet in defining ediff-current-diff-face-A/B.
;;     (Thanks to Job Ganzevoort  <Job.Ganzevoort@cwi.nl>.) 

;; Tue March 22, 1994

;;     Fixed a bug with ediffing narrowed buffers, reported by Kevin
;;     Broadey <KevinB@bartley.demon.co.uk>.
;;     Made Ediff to work with files that have incomplete last line.
;;     Made Ediff execute diff(1) and patch(1) using Bourne Shell, which
;;     should eliminate problems with $prompt that some people had.

;; Thu March 24, 1994

;;     Achieved quadratic speedup in the size of the file by replacing the
;;     slow goto-line by forward-line.  Ediff is now *much* faster than
;;     Emerge on large files.  Converted demarkation of difference regions
;;     from markers to overlays.  This will later allow us to highlight all
;;     diffs, not just the current one.

;; Mon March 30, 1994

;;     Under X, Ediff now highlights all differences in dim colors and the
;;     current difference in bright colors. Improved Lucid Emacs support.


;;; Code:

(require 'emerge) ;; Ediff is based on emerge


;;; Macros
(defmacro ediff-if-lucid ()
  (` (string-match "Lucid" emacs-version)))
(defmacro ediff-odd-p (arg)
  (` (eq (logand (, arg) 1) 1)))
(defmacro ediff-buffer-live-p (buf)
  (` (and (, buf) (get-buffer (, buf)) (buffer-name (get-buffer (, buf))))))


(defun ediff-mode ()
  "Ediff mode is used by the Ediff file-difference package.
It is entered only through one of the following commands:
	``ediff''
	``ediff-files''
	``ediff-buffers''
	``epatch''
	``ediff-patch-file''
	``ediff-patch-buffer''
	``vc-ediff''
	``rcs-ediff''
or through a non-interactive Emacs Lisp function	
	``ediff-files-remote''

Commands:
\\{ediff-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ediff-mode)
  (setq mode-name "Ediff"))

(defvar ediff-version "1.3"
  "The current version of Ediff.")

(defun ediff-version ()
  "Return string describing the version of Ediff.
When called interactively, displays the version."
  (interactive)
  (if (interactive-p)
      (message "Ediff version %s" (ediff-version))
    ediff-version))


;; Hook variables

(defvar ediff-before-setup-windows-hooks nil
  "* Hooks to run before Ediff sets its own window config.  This can be used
to save the previous window config, which can be restored on ediff-quit or
ediff-suspend.") 
(defvar ediff-startup-hooks nil
  "*Hooks to run in the control buffer after Ediff has been set up.")
(defvar ediff-select-hooks nil
  "*Hooks to run after a difference has been selected.")
(defvar ediff-unselect-hooks nil
  "*Hooks to run after a difference has been unselected.")
(defvar ediff-prepare-buffer-hooks  nil
  "*Hooks called after buffers A and B are set up.")
(defvar ediff-load-hooks nil
  "* Hook run after Ediff is loaded.  Can be used to change defaults.")

(defvar ediff-suspend-hooks 'ediff-default-suspend-hook
  "* Hooks to run in the Ediff control buffer each time Ediff is
suspended.")
(defvar ediff-quit-hooks 'ediff-default-quit-hook
  "* Hooks to run in the Ediff control buffer after the ediff has been
finished.") 

(make-variable-buffer-local 'local-write-file-hooks)
(make-variable-buffer-local 'before-change-function)

;; Help messages

(defconst ediff-help-message-long
"p,DEL - prev diff         c - recenter      ab - diff A to B  l - line numbers
n,SPC - next diff       v/V - scroll up/dn  ba - diff B to A  f - file names
  j,g - jump to diff    </> - scroll lt/rt  ra - restore A    z - suspend Ediff
    h - toggle hilit      s - toggle split  rb - restore B    q - quit Ediff
wa/wb - save buf A/B    A/B - toggle read-only buf A/B        ? - toggle help")
			  
(defconst ediff-help-message-short
"   	    	    	   ? - toggle help window")			  

(defvar ediff-help-message ediff-help-message-long
  "* The actual help message.")
 
 
(defvar ediff-diff-program "diff"
  "* Name of the program which compares two files.")
(defvar ediff-diff-options ""  
  "* Options to pass to ``ediff-diff-program''.")

;; Support for patches 

(defvar ediff-patch-program "patch"
  "* Name of the program that applies patches.")
(defvar ediff-patch-options ""
  "* Options to pass to ediff-patch-program.")
  
(defvar ediff-shell "sh"
  "* The shell used to run diff(1) and patch(1).  If user's .profile or
.cshrc files are set up correctly, any shell will do.  However, some people
set $prompt or other things incorrectly, which leads to undesirable output
messages.  These may cause Ediff to fail.  In such a case, set ediff-shell
to a shell that you are not using or, better, fix your shell's startup file.")
  
(defvar ediff-diff-ok-lines-regexp  
  "^\\([0-9,]+[acd][0-9,]+$\\|[<>] \\|---\\|Warning:\\)"
  "*Regexp that matches normal output lines from ``ediff-diff-program''.
This is mostly lifted from Emerge, except that Ediff also considers the
'Missing newline' message to be 'normal output.'
Lines that do not match are assumed to be error messages.")

(defvar ediff-match-diff-line (let ((x "\\([0-9]+\\)\\(\\|,\\([0-9]+\\)\\)"))
				(concat "^" x "\\([acd]\\)" x "$"))
  "*Pattern to match lines produced by diff that describe differences.")
  
(defvar ediff-patch-buf nil
  "The buffer of the patch file.")
(defvar ediff-patch-diagnostics nil
  "The buffer where patch(1) would display its diagnostics.")
  

;; Copying diffs betw buffers.    

(emerge-defvar-local ediff-killed-diffs-alist nil
  "A list of killed diffs.  A diff is saved here if it is replaced by a diff
from another buffer.  This alist has the form:
((num (A . diff) (B . diff)) ...), where A or B parts may be missing.")


;; Highlighting
(defvar ediff-before-flag "vvvvvvvvvvvvvvvv---- ediff ----vvvvvvvvvvvvvvv\n"
  "*Flag placed above the highlighted block of differences.  Must end with
newline.  Must be set before Ediff is loaded.  If set to nil, the flags from
emerge.el are used.")
(defvar ediff-after-flag  "^^^^^^^^^^^^^^^^---- ediff ----^^^^^^^^^^^^^^^\n"
  "*Flag placed below the highlighted block of differences.  Must end with
newline.  Must be set before Ediff is loaded.  If set to nil, the flags from
emerge.el are used.")

(defvar ediff-before-flag-length nil "")
(defvar ediff-before-flag-lines nil "")
(defvar ediff-before-flag-match nil "")
(defvar ediff-after-flag-length nil "")
(defvar ediff-after-flag-lines nil "")
(defvar ediff-after-flag-match nil "")

(emerge-defvar-local ediff-highlighting-style nil  
  "A var local to each ediff-control buffer.  Indicates highlighting style
in effect for this buffer: 'face, 'ascii, nil -- temporarily unhighlighted,
'off -- turned off \(on a dumb terminal only\).")

  
(emerge-defvar-local ediff-want-faces t
  "If t, differences will be highlighted using faces on a window
system.  If nil, they will be highlighted using ASCII flags, ediff-before-flag
and ediff-after-flag.  On a non-window system, differences are always
highlighted using ASCII flags.

This is not a user option.  Can be set either in .emacs or toggled
interactively, using ediff-toggle-hilit.")
  

;; Variables that control each Ediff session.  They are local to the
;; control buffer. 

;; Mode variables
(emerge-defvar-local ediff-A-buffer nil
  "The buffer in which the A variant is stored.")
(emerge-defvar-local ediff-B-buffer nil
  "The buffer in which the B variant is stored.")
(emerge-defvar-local ediff-control-buffer nil
  "The control buffer of ediff. ")

(emerge-defvar-local ediff-control-buffer-suffix nil
  "The suffix of the control buffer name. ")
  
(defvar ediff-control-window nil
  "The control window.")


(emerge-defvar-local ediff-A-buffer-values nil
  "Keeps working values of ediff-saved-variables for ediff-A-buffer.")
(emerge-defvar-local ediff-B-buffer-values nil
  "Keeps working values of ediff-saved-variables for ediff-B-buffer.")

(emerge-defvar-local ediff-A-buffer-values-setup nil
  "Remembers ediff-saved-variables for ediff-A-buffer as they were at setup.")
(emerge-defvar-local ediff-B-buffer-values-setup nil
  "Remembers ediff-saved-variables for ediff-B-buffer as they were at setup.")

(emerge-defvar-local ediff-difference-vector nil
  "Vector of differences between the variants.  Each difference is
represented by a vector of two overlays.  The first overlays the difference
section in the A buffer and the second overlays the diff in the B buffer.
If a difference section is empty, the corresponding overlay's endpoints
councide. ")

(emerge-defvar-local ediff-current-difference -1
  "The difference that is currently selected.")
(emerge-defvar-local ediff-number-of-differences nil
  "Number of differences found.")
  
(emerge-defvar-local ediff-diff-buffer nil
  "Buffer containing the output of diff(1), which is used by Ediff to step
through files.")
(emerge-defvar-local ediff-diff-error-buffer nil
  "Buffer containing the output of diff(1) when diff returns errors.")
  
(emerge-defvar-local ediff-this-buffer-control-sessions  nil
  "Keeps the list of ediff-control buffers associated with each buffer A/B
involved in an ediff session.")

(defvar ediff-disturbed-overlays nil
  "A list of difference overlays that were disturbed by copying or recovery
of the current diff.")
  
(defvar ediff-shaded-overlay-priority 
  (if (ediff-if-lucid)
      (1+ mouse-highlight-priority)
    100)    	;; 100 is a kludge. There is a bug in insert-in-front-hooks
		;; in Emacs < 19.23. When this is fixed, I will get rid of
		;; this kludge.
  "Priority of non-selected overlays.")


(if (ediff-if-lucid)
    (progn
      (fset 'ediff-overlayp (symbol-function 'extentp))
      (fset 'ediff-make-overlay (symbol-function 'make-extent))
      (fset 'ediff-delete-overlay (symbol-function 'delete-extent))
      (fset 'ediff-overlay-put (symbol-function 'set-extent-property))
      (fset 'ediff-move-overlay (symbol-function 'set-extent-endpoints))
      (fset 'ediff-overlay-start (symbol-function 'extent-start-position))
      (fset 'ediff-overlay-end (symbol-function 'extent-end-position))
      (fset 'ediff-overlay-get (symbol-function 'extent-property)))
  ;; GNU definitions
  (fset 'ediff-overlayp (symbol-function 'overlayp))
  (fset 'ediff-make-overlay (symbol-function 'make-overlay))
  (fset 'ediff-delete-overlay (symbol-function 'delete-overlay))
  (fset 'ediff-overlay-put (symbol-function 'overlay-put))
  (fset 'ediff-move-overlay (symbol-function 'move-overlay))
  (fset 'ediff-overlay-start (symbol-function 'overlay-start))
  (fset 'ediff-overlay-end (symbol-function 'overlay-end))
  (fset 'ediff-overlay-get (symbol-function 'overlay-get)))
  
(if window-system
    (if (ediff-if-lucid)
	(progn
	  (fset 'ediff-select-frame (symbol-function 'select-screen))
	  (fset 'ediff-window-frame (symbol-function 'window-screen))
	  (fset 'ediff-display-color-p (symbol-function 'x-color-display-p))
	  (fset 'ediff-get-face (symbol-function 'get-face)))
      (fset 'ediff-window-frame (symbol-function 'window-frame))
      (fset 'ediff-select-frame (symbol-function 'select-frame))
      (fset 'ediff-display-color-p (symbol-function 'x-display-color-p))
      (fset 'ediff-get-face (symbol-function 'internal-get-face)))
  ;; not a window system
  (fset 'ediff-window-frame (function (lambda (wind) (if wind 1 nil)) ))
  (fset 'ediff-select-frame (symbol-function 'identity))
  (fset 'ediff-make-current-diff-overlay (function (lambda (type) nil)))
  (fset 'ediff-unhighlight-diffs-totally (function (lambda () nil))))
  

(if (not window-system)
    ()
  (defvar ediff-current-diff-face-A
    (progn
      (make-face 'ediff-current-diff-face-A)
      (cond ((ediff-display-color-p)
	     (set-face-foreground 'ediff-current-diff-face-A "firebrick")
	     (set-face-background 'ediff-current-diff-face-A "pale green"))
	    (t
	     (if (ediff-if-lucid)
		 (copy-face 'modeline 'ediff-current-diff-face-A)
	       (copy-face 'highlight 'ediff-current-diff-face-A))
	     ))
      (ediff-get-face 'ediff-current-diff-face-A))
  "Face for highlighting the currently selected difference in buffer A of
the Ediff display") 

  (defvar ediff-current-diff-face-B
    (progn
      (make-face 'ediff-current-diff-face-B)
      (cond ((ediff-display-color-p)
	     (set-face-foreground 'ediff-current-diff-face-B "DarkOrchid")
	     (set-face-background 'ediff-current-diff-face-B "Yellow"))
	    (t 
	     (if (ediff-if-lucid)
		 (copy-face 'modeline 'ediff-current-diff-face-B)
	       (copy-face 'highlight 'ediff-current-diff-face-B))
	     ))
      (ediff-get-face 'ediff-current-diff-face-B))
    "Face for highlighting the currently selected difference in buffer B of
the Ediff display") 

  (defvar ediff-even-diff-face-A
    (progn
      (make-face 'ediff-even-diff-face-A)
      (cond ((ediff-display-color-p)
	     (set-face-background 'ediff-even-diff-face-A "light grey"))
	    (t 
	     (if (ediff-if-lucid)
		 (progn
		   (copy-face 'highlight 'ediff-even-diff-face-A)
		   (invert-face 'ediff-even-diff-face-A))
	       (make-face-italic 'ediff-even-diff-face-A))))
      (ediff-get-face 'ediff-even-diff-face-A))
    "Face used to highlight even-numbered differences in buffer A.")
      
  (defvar ediff-even-diff-face-B
    (progn
      (make-face 'ediff-even-diff-face-B)
      (cond ((ediff-display-color-p)
	     (set-face-foreground 'ediff-even-diff-face-B "White")
	     (set-face-background 'ediff-even-diff-face-B "Gray"))
	    (t 
	     (if (ediff-if-lucid)
		 (copy-face 'highlight 'ediff-even-diff-face-B)
	       (make-face-italic 'ediff-even-diff-face-B))))
      (ediff-get-face 'ediff-even-diff-face-B))
    "Face used to highlight even-numbered differences in buffer B.")

  (defvar ediff-odd-diff-face-A
    (progn
      (make-face 'ediff-odd-diff-face-A)
      (cond ((ediff-display-color-p)
	     (set-face-foreground 'ediff-odd-diff-face-A "White")
	     (set-face-background 'ediff-odd-diff-face-A "Gray"))
	  (t 
	   (if (ediff-if-lucid)
	       (copy-face 'highlight 'ediff-odd-diff-face-A)
	     (make-face-italic 'ediff-odd-diff-face-A))))
	  (ediff-get-face 'ediff-odd-diff-face-A))
    "Face used to highlight odd-numbered differences in buffer A.")
      
  (defvar ediff-odd-diff-face-B
    (progn
      (make-face 'ediff-odd-diff-face-B)
      (cond ((ediff-display-color-p)
	     (set-face-foreground 'ediff-odd-diff-face-B "Black")
	     (set-face-background 'ediff-odd-diff-face-B "light grey"))
	    (t 
	     (if (ediff-if-lucid)
		 (progn
		   (copy-face 'highlight 'ediff-odd-diff-face-B)
		   (invert-face 'ediff-odd-diff-face-B))
	       (make-face-italic 'ediff-odd-diff-face-B))))
      (ediff-get-face 'ediff-odd-diff-face-B))
    "Face used to highlight odd-numbered differences in buffer B.")
    
    
  ;; Create *-var faces. These are the actual faces used to highlight
  ;; odd-numbered difference regions.
  ;; They are used as follows: when highlighting is turned on,
  ;; ediff-odd/even-diff-face-A/B are copied
  ;; into ediff-odd/even-diff-face-A/B-var, and all odd/even overlays become
  ;; highlighted. When highlighting is turned off, then the face 'default is
  ;; copied into ediff-odd/even-diff-face-A/B-var, thereby unhighlighting all
  ;; difference regions.
  (make-face 'ediff-even-diff-face-A-var)
  (make-face 'ediff-even-diff-face-B-var)
  (make-face 'ediff-odd-diff-face-A-var)
  (make-face 'ediff-odd-diff-face-B-var)
  ;; initialize *-var faces
  (copy-face (if ediff-want-faces ediff-even-diff-face-A 'default)
	     'ediff-even-diff-face-A-var)
  (copy-face (if ediff-want-faces ediff-even-diff-face-B 'default)
	     'ediff-even-diff-face-B-var)
  (copy-face (if ediff-want-faces ediff-odd-diff-face-A 'default)
	     'ediff-odd-diff-face-A-var)
  (copy-face (if ediff-want-faces ediff-odd-diff-face-B 'default)
	     'ediff-odd-diff-face-B-var)
      

  ;;; Overlays

  (emerge-defvar-local ediff-current-diff-overlay-A nil
    "Overlay in buffer A.")
  (emerge-defvar-local ediff-current-diff-overlay-B nil
    "Overlay in buffer B.")
  
  (defun ediff-make-current-diff-overlay (type)
      (let ((overlay (if (eq type 'A)
			 'ediff-current-diff-overlay-A
		       'ediff-current-diff-overlay-B))
	    (buffer (if (eq type 'A) ediff-A-buffer ediff-B-buffer))
	    (face (if (eq type 'A) 
		      (face-name ediff-current-diff-face-A)
		    (face-name ediff-current-diff-face-B))))
	(set overlay (ediff-make-overlay (point-max) (point-max) buffer))
	(ediff-overlay-put (eval overlay) 'face face)
	(ediff-overlay-put (eval overlay) 'ediff ediff-control-buffer)
	))
	
  ;; Computes priority of ediff overlay.
  (defun ediff-highest-priority (start end buffer)
    (let ((pos (max 1 (1- start)))
	  ovr-list)
      (if (ediff-if-lucid)
	  (+ 2 mouse-highlight-priority)
	(emerge-eval-in-buffer
	 buffer
	 (while (< pos (min (point-max) (1+ end)))
	   (setq ovr-list (append (overlays-at pos) ovr-list))
	   (setq pos (next-overlay-change pos)))
	 (1+ (eval
	      (cons '+
		    (mapcar (function
			     (lambda (ovr)
			       (if ovr
				   (or (ediff-overlay-get ovr 'priority) 0)
				 0)))
			    ovr-list)
		    )))
	 ))))

)  ; end of window-system-only code.
  
  

;;; Misc

(defvar ediff-split-window-function 'split-window-vertically
  "* The function to be called to divide the main window between buffer-A
and buffer-B.  You can set it to be split horizontally instead of the
default verstical split by setting this variable to
'split-window-horizontally.  You can also have your own function for fancy
splits.  This variable has no effect when buffer-A and buffer-B are shown in
different frames.  In this case, Ediff will use those frames to display
these buffers.")
		 
(defconst ediff-saved-variables
  '(buffer-read-only
    buffer-auto-save-file-name)
  "Variables and properties of a buffer which are saved, modified and restored
during an Ediff session.")

(defconst ediff-working-values '(nil nil)
  "Values to be assigned to ediff-saved-variables during diff.")
  
(defvar ediff-use-last-dir nil
  "* If t, Ediff will use last directory it had seen as a default
directory when prompting for file names.")
  
(defvar ediff-nix-help-in-control-buffer nil
  "*Don't want C-h to invoke Emacs help.  Instead, C-h will jump to previous
difference.")
  
(defvar ediff-temp-file-prefix
  (let ((env (getenv "TMPDIR"))
	d)
    (setq d (if (and env (> (length env) 0))
		env
	      "/tmp"))
    (if (= (aref d (1- (length d))) ?/)
	(setq d (substring d 0 -1)))
    (concat d "/ediff"))
  "*Prefix to put on Ediff temporary file names.
Do not start with `~/' or `~user-name/'.")  

(defvar ediff-temp-file-mode 384	; u=rw only
  "*Mode for Ediff temporary files.")
  
(defvar ediff-last-dir-A nil
  "Last directory used by an Ediff command for file-A.")
(defvar ediff-last-dir-B nil
  "Last directory used by an Ediff command for file-B.")
  
;; Build keymaps

(defvar ediff-mode-map nil
  "Local keymap used in Ediff mode.")


(defun ediff-setup-keymap ()
  "Set up the keymap used in the control buffer of Ediff."
  (setq ediff-mode-map (make-sparse-keymap))
  (suppress-keymap ediff-mode-map)
  
  (define-key ediff-mode-map "p" 'ediff-previous-difference)
  (define-key ediff-mode-map "\C-?" 'ediff-previous-difference)
  (define-key ediff-mode-map "\C-h" (if ediff-nix-help-in-control-buffer
					'ediff-previous-difference nil))
  (define-key ediff-mode-map "n" 'ediff-next-difference)
  (define-key ediff-mode-map " " 'ediff-next-difference)
  (define-key ediff-mode-map "j" 'ediff-jump-to-difference)
  (define-key ediff-mode-map "g" 'ediff-jump-to-difference)
  (define-key ediff-mode-map "q" 'ediff-quit)
  (define-key ediff-mode-map "z" 'ediff-suspend)
  (define-key ediff-mode-map "c" 'ediff-recenter)
  (define-key ediff-mode-map "s" 'ediff-toggle-split)
  (define-key ediff-mode-map "h" 'ediff-toggle-hilit)
  (define-key ediff-mode-map "v" 'ediff-scroll-up)
  (define-key ediff-mode-map "\C-v" 'ediff-scroll-up)
  (define-key ediff-mode-map "^" 'ediff-scroll-down)
  (define-key ediff-mode-map "\M-v" 'ediff-scroll-down)
  (define-key ediff-mode-map "V" 'ediff-scroll-down)
  (define-key ediff-mode-map "<" 'ediff-scroll-left)
  (define-key ediff-mode-map ">" 'ediff-scroll-right)
  (define-key ediff-mode-map "f" 'ediff-file-names)
  (define-key ediff-mode-map "l" 'ediff-line-numbers)
  (define-key ediff-mode-map "?" 'ediff-toggle-help)
  (define-key ediff-mode-map "a"  nil)
  (define-key ediff-mode-map "ab" 'ediff-diff-to-diff)
  (define-key ediff-mode-map "b"  nil)
  (define-key ediff-mode-map "ba" 'ediff-diff-to-diff)
  (define-key ediff-mode-map "r"  nil)
  (define-key ediff-mode-map "ra" 'ediff-restore-diff)
  (define-key ediff-mode-map "rb" 'ediff-restore-diff)
  (define-key ediff-mode-map "o"   nil)
  (define-key ediff-mode-map "A"  'ediff-toggle-read-only)
  (define-key ediff-mode-map "B"  'ediff-toggle-read-only)
  (define-key ediff-mode-map "w"   nil)
  (define-key ediff-mode-map "wa"  'ediff-save-buffer)
  (define-key ediff-mode-map "wb"  'ediff-save-buffer)
  (define-key ediff-mode-map "k"   nil)
  (define-key ediff-mode-map "kkk" 'ediff-reload-keymap) ;; for debug
  ;; Allow ediff-mode-map to be referenced indirectly
  (fset 'ediff-mode-map ediff-mode-map))


;;; Setup functions

(defun ediff-find-file (file buffer &optional last-dir)
  "Visits FILE for ediff.
BUFFER is a variable symbol that is supposed to
get the buffer into which FILE is read.  LAST-DIR is the directory variable
symbol where FILE's dir name should be returned.
Arguments: (file 'buffer &optional 'last-dir)"
  (if (not (file-readable-p file))
      (error "File `%s' does not exist or is not readable" file))
  
  ;; Record the buffer
  (set buffer (find-file-noselect file))
  ;; Record the directory of the file
  (if last-dir
      (set last-dir (expand-file-name (file-name-directory file))))
  
  ;; Make sure the entire file is seen, and it reflects what is on disk
  (emerge-eval-in-buffer 
   (eval buffer)
   (widen)
   (let ((temp (file-local-copy file))
	 startup-hooks)
     (if temp
	 (setq file temp
	       startup-hooks
	       (cons (` (lambda () (delete-file (, file))))
		     startup-hooks))
       ;; Verify that the file matches the buffer
       (emerge-verify-file-buffer)))))

(defun ediff-files-internal (file-A file-B &optional startup-hooks)
  (let (buffer-A buffer-B)
    (message "Ediff: Reading file %s ... " file-A)(sit-for .5)
    (ediff-find-file file-A 'buffer-A 'ediff-last-dir-A)
    (message "Ediff: Reading file %s ... " file-B)(sit-for .5)
    (ediff-find-file file-B 'buffer-B 'ediff-last-dir-B)
    (ediff-setup buffer-A file-A buffer-B file-B startup-hooks)))
  
(defun ediff-get-patch-buffer (dir)
  "Obtain patch buffer.  If patch is already in a buffer---use it.
Else, read patch file into a new buffer."
  (if (y-or-n-p "Is the patch file already in a buffer? ")
      (setq ediff-patch-buf
	    (get-buffer (read-buffer "Patch buffer name: " nil t))) ;must match
    (setq ediff-patch-buf
	  (find-file-noselect (read-file-name "Patch file name: "
					      dir))))
  (emerge-eval-in-buffer
   ediff-patch-buf
   (toggle-read-only 1))
  (setq ediff-patch-diagnostics
	(get-buffer-create "*ediff patch diagnostics*"))
  (emerge-eval-in-buffer
   ediff-patch-diagnostics
   (insert-buffer ediff-patch-buf))
  )

;; Start up Ediff on two files
(defun ediff-setup (buffer-A file-A buffer-B file-B startup-hooks)
  (setq file-A (expand-file-name file-A))
  (setq file-B (expand-file-name file-B))
  (let* ((control-buffer-name (emerge-unique-buffer-name "*ediff-control" "*"))
	 (control-buffer (emerge-eval-in-buffer
			  buffer-A
			  (get-buffer-create control-buffer-name))))
    (emerge-eval-in-buffer
     control-buffer
     (ediff-mode) ;; in control buffer only
     (setq buffer-read-only nil)
     (setq ediff-A-buffer buffer-A)
     (setq ediff-B-buffer buffer-B)
     (setq ediff-control-buffer control-buffer)
     (setq ediff-control-buffer-suffix
	   (if (string-match "<[0-9]*>" control-buffer-name)
	       (substring control-buffer-name
			  (match-beginning 0) (match-end 0))
	     "<1>"))
     (ediff-remember-buffer-characteristics t) ;; remember at setup
     
     (ediff-set-keys)
     (setq ediff-difference-vector (ediff-make-diff-list file-A file-B))
     (setq ediff-number-of-differences (length ediff-difference-vector))
     (setq ediff-current-difference -1)
     (ediff-make-current-diff-overlay 'A)
     (ediff-make-current-diff-overlay 'B)
     (run-hooks 'ediff-before-setup-windows-hooks)
     (ediff-setup-windows buffer-A buffer-B control-buffer t)
         
     ;; all these must be inside emerge-eval-in-buffer control-buffer,
     ;; since these vars are local to control-buffer
     ;; These won't run if there are errors in diff
     (emerge-eval-in-buffer
      ediff-A-buffer
      (run-hooks 'ediff-prepare-buffer-hooks)
      (add-hook 'local-write-file-hooks 'ediff-block-write-file)
      (setq before-change-function 'ediff-before-change-guard)
      ;; add control-buffer to the list of sessions
      (or (memq control-buffer ediff-this-buffer-control-sessions)
	  (setq ediff-this-buffer-control-sessions
		(cons control-buffer ediff-this-buffer-control-sessions)))
      (setq mode-line-buffer-identification '("A: %b")))
     (emerge-eval-in-buffer
      ediff-B-buffer
      (run-hooks 'ediff-prepare-buffer-hooks)
      (add-hook 'local-write-file-hooks 'ediff-block-write-file)
      (setq before-change-function 'ediff-before-change-guard)
      ;; add control-buffer to the list of sessions
      (or (memq control-buffer ediff-this-buffer-control-sessions)
	  (setq ediff-this-buffer-control-sessions
		(cons control-buffer ediff-this-buffer-control-sessions)))
      (setq mode-line-buffer-identification '("B: %b")))

     (emerge-eval-in-buffer control-buffer
			    (run-hooks 'startup-hooks 'ediff-startup-hooks)
			    (setq buffer-read-only t)))))

;; Generate the Ediff difference list between two files
(defun ediff-make-diff-list (file-A file-B)
  (setq ediff-diff-buffer 
	(get-buffer-create (emerge-unique-buffer-name "*ediff-diff" "*")))
  (emerge-eval-in-buffer
   ediff-diff-buffer
   (erase-buffer)
   ;; shell-command tends to display old shell command buffers even when it
   ;; puts output in another buffer---probably an Emacs bug.
   (ediff-kill-buffer-carefully "*Shell Command Output*")
   (let (shell-command-file ediff-shell)
     (message "Ediff: Computing differences ...")(sit-for .5)
     (shell-command
      (format "%s %s %s %s"
	      ediff-diff-program ediff-diff-options
	      (emerge-protect-metachars file-A)
	      (emerge-protect-metachars file-B))
      t)
     ))
  (ediff-prepare-error-list ediff-diff-ok-lines-regexp)
  (message "Ediff: Computing differences ... Done.")(sit-for .5)
  (ediff-convert-diffs-to-overlays
   ediff-A-buffer ediff-B-buffer
   (ediff-extract-diffs ediff-diff-buffer ediff-A-buffer ediff-B-buffer)))
    
(defun ediff-prepare-error-list (ok-regexp)
  (let ((diff-buff ediff-diff-buffer))
    (setq ediff-diff-error-buffer
	  (get-buffer-create (emerge-unique-buffer-name
			      "*ediff-diff-errors" "*")))
    (emerge-eval-in-buffer
     ediff-diff-error-buffer
     (erase-buffer)
     (insert-buffer diff-buff)
     (delete-matching-lines ok-regexp))))

;;; Function to start Ediff by patching a file

;;;###autoload
(defun ediff-patch-file (file-to-patch &optional startup-hooks)
  "Run Ediff by patching FILE-TP-PATCH."
  (interactive "fFile to patch: ")
  
  (ediff-get-patch-buffer (file-name-directory file-to-patch))
  (let ((buf (get-file-buffer file-to-patch)))
    (if buf 
	(progn
	  (emerge-eval-in-buffer
	   buf
	   (if (buffer-modified-p buf)
	       (if (y-or-n-p 
		    (format 
		     "File '%s' is already in buffer %s.  Save before killing? "
		     file-to-patch (buffer-name buf)))
		   (save-buffer buf)))
	   (set-buffer-modified-p nil))
	  (ediff-kill-buffer-carefully buf))))
  (emerge-eval-in-buffer
   ediff-patch-diagnostics
   (let (shell-command-file ediff-shell)
     (message "Ediff: Applying patch ... ")(sit-for .5)
     (shell-command-on-region 
      (point-min) (point-max)
      (format "%s %s %s"
	      ediff-patch-program ediff-patch-options
	      (expand-file-name file-to-patch))
      t)
     (message "Ediff: Applying patch ... Done.")(sit-for .5)
     ))
  (switch-to-buffer ediff-patch-diagnostics)
  (sit-for 0) ;; synchronize
  
  (setq startup-hooks (cons 'ediff-toggle-read-only-A startup-hooks))
  (ediff-files (format "%s.orig" file-to-patch) file-to-patch startup-hooks)
  
  (bury-buffer ediff-patch-diagnostics)
  (message "Patch diagnostics available in buffer %s."
   	   (buffer-name ediff-patch-diagnostics)))

(defalias 'epatch 'ediff-patch-file)

;;; Function to start Ediff on files

;;;###autoload
(defun ediff-files (file-A file-B &optional startup-hooks)
  "Run Ediff on a pair files, FILE-A and FILE-B."
  (interactive
   (let (f)
     (list (setq f (ediff-read-file-name "File A to compare" 
					 (if ediff-use-last-dir
					     ediff-last-dir-A
					   default-directory)
					 nil nil))
	   (ediff-read-file-name "File B to compare" 
				 (if ediff-use-last-dir
				     ediff-last-dir-B  nil)
				 f f)
	   )))
  (ediff-files-internal file-A file-B startup-hooks))


(defalias 'ediff 'ediff-files)


;;; Function to start Ediff on buffers

;;;###autoload
(defun ediff-buffers (buffer-A buffer-B &optional startup-hooks)
  "Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B."
  (interactive "bBuffer A to compare: \nbBuffer B to compare: ")
  (let (ediff-file-A ediff-file-B)
    (emerge-eval-in-buffer
     buffer-A
     (setq ediff-file-A
	   (ediff-make-temp-file
	    (format ".%s." (file-name-nondirectory (buffer-name)))))
     (write-region (point-min) (point-max) ediff-file-A nil 'no-message))
    (emerge-eval-in-buffer
     buffer-B
     (setq ediff-file-B
	   (ediff-make-temp-file
	    (format ".%s." (file-name-nondirectory (buffer-name)))))
     (write-region (point-min) (point-max) ediff-file-B nil 'no-message))
    (ediff-setup (get-buffer buffer-A) ediff-file-A
		 (get-buffer buffer-B) ediff-file-B
		 (cons (` (lambda ()
			    (delete-file (, ediff-file-A))
			    (delete-file (, ediff-file-B))))
		       startup-hooks)
		 )))
		  
;;;###autoload
(defun ediff-patch-buffer (buffer-name &optional startup-hooks)		  
  "Run Ediff by patching BUFFER-NAME."
  (interactive "bBuffer to patch: ")
  
  (let* ((file-buffer (get-buffer buffer-name))
	 (file-name (if file-buffer (buffer-file-name  file-buffer))))
    (if (not file-name)
	(error "Buffer %s doesn't exist or doesn't visit any file.  Why patch?"
	       file-name))
    
    (ediff-patch-file file-name startup-hooks)))
      

;;; Versions Control functions      
      
;;;###autoload
(defun vc-ediff (rev)
  "Run ediff on version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created.
Note: this function will work starting with GNU Emacs 19.22."
  (interactive "sVersion to ediff with (default is the latest version): ")
  (or (featurep 'vc)
      (if (locate-library "vc") ;; if vc.el is available
	  (progn
	    (require 'vc-hooks) 
	    (define-key vc-prefix-map "=" 'vc-ediff))
	(error "The VC package is apparently not installed.")))
  (let ((newvers (current-buffer))
	(oldvers (vc-version-other-window rev)))
    (ediff-buffers newvers oldvers)
    ))
    
(defun rcs-ediff-view-revision (&optional rev)
  "View previous RCS revison of current file.
With prefix argument, prompts for a revision name." 
  (interactive (list (if current-prefix-arg 
			 (read-string "Revision: "))))
  (let* ((filename (buffer-file-name (current-buffer)))
	 (switches (append '("-p")
			   (if rev (list (concat "-r" rev)) nil)))
	 (buff (concat (file-name-nondirectory filename) ".~" rev "~")))
    (message "Working...")
    (setq filename (expand-file-name filename))
    (with-output-to-temp-buffer
	buff
      (let ((output-buffer (rcs-get-output-buffer filename buff)))
	(delete-windows-on output-buffer)
	(save-excursion
	  (set-buffer output-buffer)
	  (apply 'call-process "co" nil t nil
		 ;; -q: quiet (no diagnostics)
		 (append switches rcs-default-co-switches
			 (list "-q" filename))))) 
      (message "")
      buff)))    

;;;###autoload
(defun rcs-ediff (&optional rev)
  "Run Ediff on the current buffer, comparing it with previous RCS revison.  
With prefix argument, prompts for revision name." 
  (interactive (list (if current-prefix-arg 
			 (read-string "Revision: "))))
  (or (featurep 'rcs)
      (if (locate-library "rcs")      
	  (progn
	    (require 'rcs)
	    (global-set-key "\C-cD" 'rcs-ediff))
	(error "The RCS package is apparently not installed.")))
  (let ((newvers (current-buffer))
	(oldvers (rcs-ediff-view-revision rev)))
    (ediff-buffers newvers oldvers)
    ))


;;; Functions to start Ediff via remote request

;;;###autoload
(defun ediff-files-remote (file-a file-b)
  "Run Ediff on remote files, FILE-A and FILE-B."
  (ediff-files-internal file-a file-b nil)
  (throw 'client-wait nil))


(defun ediff-remote-exit (exit-func)
  "Exit remote Ediff session."
  (ediff-really-quit)
  (funcall exit-func))



;; Select the lowest window on the frame.
(defun ediff-select-lowest-window ()
  (let* ((lowest-window (selected-window))
	 (bottom-edge (car (cdr (cdr (cdr (window-edges))))))
         (last-window (previous-window))
         (window-search t))
    (while window-search
      (let* ((this-window (next-window))
             (next-bottom-edge (car (cdr (cdr (cdr 
                                               (window-edges this-window)))))))
        (if (< bottom-edge next-bottom-edge)
            (progn
              (setq bottom-edge next-bottom-edge)
              (setq lowest-window this-window)))

        (select-window this-window)
        (if (eq last-window this-window)
            (progn
              (select-window lowest-window)
              (setq window-search nil)))))))

;;; Common setup routines

;; Set up the window configuration.  If POS is given, set the points to
;; the beginnings of the buffers.
(defun ediff-setup-windows (buffer-A buffer-B control-buffer &optional pos)
  ;; Make sure we are not in the minibuffer window when we try to delete
  ;; all other windows.
  (if (eq (selected-window) (minibuffer-window))
      (other-window 1))
  (delete-other-windows)
  (switch-to-buffer control-buffer)
  (ediff-refresh-mode-line)
  
  (ediff-arrange-buffer buffer-A buffer-B (current-buffer) pos)
  (ediff-arrange-buffer buffer-B buffer-A (current-buffer) pos)
  ;; ediff-arrange-buffer always leaves in ctl buffer
  ;; setup ctl wind if it is not set.
  (ediff-setup-control-window)
  
  ;; If diff reports errors, display them rather than then compare buffers.
  (if (/= 0 (emerge-eval-in-buffer ediff-diff-error-buffer (buffer-size)))
      (let ((diff-output-buf  ediff-diff-buffer))
	(switch-to-buffer ediff-diff-error-buffer)
	(ediff-kill-buffer-carefully control-buffer)
	(error "Errors found in diff output.  Diff output buffer is %s"
	       diff-output-buf))))


;; Arranges goal-buf on the screen.
(defun ediff-arrange-buffer (goal-buf other-buf ctl-buf &optional pos)
  (let* ((ctl-wind (get-buffer-window ctl-buf t))
	 (goal-wind (get-buffer-window goal-buf t))
	 (other-wind (get-buffer-window other-buf t))
	 (ctl-frame (ediff-window-frame ctl-wind))
	 (goal-frame (if goal-wind (ediff-window-frame goal-wind)))
	 (other-frame (if other-wind (ediff-window-frame other-wind)))
	 (ctl-frame-shared (or (eq ctl-frame goal-frame)
			       (eq ctl-frame other-frame))))
			  
    (cond ((and goal-frame (not (eq goal-wind other-wind)))
	    	    ;; goal buffer is visible and we are not comparing file
		    ;; against itself (by mistake).
		    ;; Note:  goal-frame != ctl-frame, as we deleted other
		    ;; windows  on ctl-frame.
	    	      (ediff-select-frame goal-frame)
		      (select-window goal-wind)
		      (delete-other-windows)) 
	    	    	    	    	      
	  ;; goal-buf invisible, ctl-frame has only ctl-buf
	  ;; then put goal-buf on ctl-frame
	  ((null ctl-frame-shared)
		      (ediff-select-frame ctl-frame)
		      (split-window-vertically)
		      (ediff-select-lowest-window)
		      (setq ctl-wind (selected-window))
		      (switch-to-buffer ctl-buf)
		      (ediff-setup-control-window)
		      (other-window 1)
		      (switch-to-buffer goal-buf)) ; goal-buf set
	  ;; goal-buf invisible, ctl-frame has ctl-buf and other-buf
	  ;; So, put everything in one frame
	  (other-frame   ;; share with the other buf
	    	       (ediff-select-frame ctl-frame)
		       (select-window other-wind)
		       (funcall ediff-split-window-function)
		       (other-window 1)
		       (switch-to-buffer goal-buf))
	  (t ;; debug
	   (error "Funny window combination (Ediff bug?)")))

      (if pos
	  (goto-char (point-min)))
	  
      (ediff-select-frame ctl-frame)
      (select-window ctl-wind)
      (switch-to-buffer ctl-buf)))
      
;; This function assumes that we are in the window where control buffer is
;; to reside.
(defun ediff-setup-control-window ()
  "Set up window for control buffer."
  (erase-buffer)
  (insert ediff-help-message)
  (shrink-window-if-larger-than-buffer)
  (setq ediff-control-window (selected-window))
  (goto-char (point-min))
  (skip-chars-forward " \t\n"))
      

;; Set up the keymap in the control buffer
(defun ediff-set-keys ()
  "Set up Ediff keymap, if necessary."
  (if (null ediff-mode-map)
      (ediff-setup-keymap))
  (use-local-map ediff-mode-map))
  
;; Reload Ediff keymap.  For debugging only.
(defun ediff-reload-keymap ()
  (interactive)
  (setq ediff-mode-map nil)
  (ediff-set-keys))

(defun ediff-before-change-guard (start end)
  "If buffer is highlighted with ASCII flags, remove highlighting before
changing buf. Arguments, START and END are not used, but are provided
because this is required by ``before-change-function''."
  (let (notify)
    (save-window-excursion
      (mapcar
       (function
	(lambda (buf)
	  (if (ediff-buffer-live-p buf)
	      (emerge-eval-in-buffer
	       buf
	       (if (eq ediff-highlighting-style 'ascii)
		   (progn
		     (ediff-unselect-and-select-difference
		      ediff-current-difference 
		      'unselect-only 'no-recenter)
		     (setq notify t)
		     ))))))
       ediff-this-buffer-control-sessions)
      (if notify
	  (error "ASCII flags removed. You can edit now. Hit 'c' to rehighlight."))
     )))
		   

(defun ediff-remember-buffer-characteristics (&optional arg)
  "Record certain properties of the buffers being compared.
Must be called in the control buffer.  Saves ``read-only'', ``modified'',
and ``auto-save'' properties in buffer local variables.  Turns off
``auto-save-mode''.  These properties are restored via a call to
``ediff-restore-buffer-characteristics''."

  ;; remember and alter buffer characteristics
  (set  (if arg 'ediff-A-buffer-values-setup 'ediff-A-buffer-values)
	(emerge-eval-in-buffer
	 ediff-A-buffer
	 (prog1
	     (emerge-save-variables ediff-saved-variables)
	   (emerge-restore-variables ediff-saved-variables
				     ediff-working-values))))
  (set  (if arg 'ediff-B-buffer-values-setup 'ediff-B-buffer-values)
	(emerge-eval-in-buffer
	 ediff-B-buffer
	 (prog1
	     (emerge-save-variables ediff-saved-variables)
	   (emerge-restore-variables ediff-saved-variables
				     ediff-working-values)))))

(defun ediff-restore-buffer-characteristics (&optional arg)
  "Restores properties saved by ``ediff-remember-buffer-characteristics''."
  (let ((A-values (if arg ediff-A-buffer-values-setup ediff-A-buffer-values))
	(B-values (if arg ediff-B-buffer-values-setup ediff-B-buffer-values)))
    (emerge-eval-in-buffer ediff-A-buffer
			   (emerge-restore-variables ediff-saved-variables
						     A-values))
    (emerge-eval-in-buffer ediff-B-buffer
			   (emerge-restore-variables ediff-saved-variables
						     B-values))))


(defun ediff-extract-diffs (diff-buffer A-buffer B-buffer)
  (let (diff-list
	(a-prev 1) ;; this is needed to set the first diff line correctly
	(b-prev 1))
    (emerge-eval-in-buffer
     A-buffer
     (goto-char (point-min)))
    (emerge-eval-in-buffer
     B-buffer
     (goto-char (point-min)))
    (emerge-eval-in-buffer
     diff-buffer
     (goto-char (point-min))
     (while (re-search-forward ediff-match-diff-line nil t)
       (let* ((a-begin (string-to-int (buffer-substring (match-beginning 1)
							(match-end 1))))
	      (a-end  (let ((b (match-beginning 3))
			    (e (match-end 3)))
			(if b
			    (string-to-int (buffer-substring b e))
			  a-begin)))
	      (diff-type (buffer-substring (match-beginning 4) (match-end 4)))
	      (b-begin (string-to-int (buffer-substring (match-beginning 5)
							(match-end 5))))
	      (b-end (let ((b (match-beginning 7))
			   (e (match-end 7)))
		       (if b
			   (string-to-int (buffer-substring b e))
			 b-begin)))
	      a-begin-pt a-end-pt b-begin-pt b-end-pt)
	 ;; fix the beginning and end numbers, because diff is somewhat
	 ;; strange about how it numbers lines
	 (if (string-equal diff-type "a")
	     (setq b-end (1+ b-end)
		   a-begin (1+ a-begin)
		   a-end a-begin)
	   (if (string-equal diff-type "d")
	       (setq a-end (1+ a-end)
		     b-begin (1+ b-begin)
		     b-end b-begin)
	     ;; (string-equal diff-type "c")
	     (setq a-end (1+ a-end)
		   b-end (1+ b-end))))
	 ;; convert to relative line numbers
	 (emerge-eval-in-buffer
	  A-buffer
	  (forward-line (- a-begin a-prev))
	  (setq a-begin-pt (point))
	  (forward-line (- a-end a-begin))
	  (setq a-end-pt (point)
		a-prev a-end))
	 (emerge-eval-in-buffer
	  B-buffer
	  (forward-line (- b-begin b-prev))
	  (setq b-begin-pt (point))
	  (forward-line (- b-end b-begin))
	  (setq b-end-pt (point)
		b-prev b-end))
	 (setq diff-list (nconc diff-list (list (vector a-begin-pt a-end-pt
							b-begin-pt b-end-pt))))
	 )))
    diff-list
    ))
    
(defun ediff-convert-diffs-to-overlays (A-buffer B-buffer diff-list)
  (let* ((current-diff -1)
	 (total-diffs (length diff-list))
	 (control-buffer-suffix ediff-control-buffer-suffix)
	 diff-overlay-list list-element
	 a-begin a-end b-begin b-end
	 a-overlay b-overlay)

    (while diff-list
      (setq current-diff (1+ current-diff)
	    list-element (car diff-list)
	    a-begin 	 (aref list-element 0)
	    a-end 	 (aref list-element 1)
	    b-begin 	 (aref list-element 2)
	    b-end 	 (aref list-element 3))
	    
      ;; place overlays at the appropriate places in the buffers
      (setq a-overlay (ediff-make-overlay a-begin a-end A-buffer))
      ;; priority of a-overlay and b-overlay should be equal. otherwise it
      ;; won't work due to Emacs bug---insert-in-front-hooks will be called
      ;; only on behalf of the buffer with higher priority.
      (ediff-overlay-put a-overlay 'priority ediff-shaded-overlay-priority)
      (ediff-overlay-put a-overlay 'ediff-diff-num current-diff)
      (ediff-overlay-put a-overlay
			 'insert-in-front-hooks '(ediff-insert-in-front))
      (ediff-overlay-put a-overlay
			 'ediff-ctl-buf control-buffer-suffix)
      (ediff-overlay-put a-overlay 
			 'face (if (eq (logand current-diff 1) 1) ;; odd diff
				   'ediff-odd-diff-face-A-var
				 'ediff-even-diff-face-A-var))
			 
      (setq b-overlay (ediff-make-overlay b-begin b-end B-buffer))
      (ediff-overlay-put b-overlay 'priority ediff-shaded-overlay-priority)
      (ediff-overlay-put b-overlay 'ediff-diff-num current-diff)
      (ediff-overlay-put b-overlay
			 'insert-in-front-hooks '(ediff-insert-in-front))
      (ediff-overlay-put b-overlay
			 'ediff-ctl-buf control-buffer-suffix)
      (ediff-overlay-put b-overlay 
			 'face (if (ediff-odd-p current-diff) ;; odd diff
				   'ediff-odd-diff-face-B-var
				 'ediff-even-diff-face-B-var))
				 
      (if (ediff-if-lucid) ;; chars inserted at end will be inside extent
	  (progn
	    (ediff-overlay-put a-overlay
			       'ediff-marker 
			       (move-marker (make-marker) a-begin A-buffer))
	    (ediff-overlay-put b-overlay
			       'ediff-marker 
			       (move-marker (make-marker) b-begin B-buffer))
	    (ediff-overlay-put a-overlay 'end-open nil)
	    (ediff-overlay-put b-overlay 'end-open nil)))
			 
      ;; record all overlays for this difference
      (setq diff-overlay-list
	    (nconc diff-overlay-list (list (vector a-overlay b-overlay)))
	    diff-list (cdr diff-list))
      (message "Ediff: Processing diff region %d of %d"
	       current-diff total-diffs)
      ) ;; while
    ;; this is just to avoid confusing the user with diff num < total-diffs
    (message "Ediff: Processing diff region %d of %d"
	       (1+ current-diff) total-diffs)
    ;; convert the list of difference information into a vector for
    ;; fast access
    (setq ediff-difference-vector
	  (apply 'vector diff-overlay-list))))



;;; Commands

(defun ediff-recenter (&optional no-rehighlight)
  "Bring the highlighted region of all buffers A and B into view.
Reestablish the default three-window display."
  (interactive)
  (setq ediff-disturbed-overlays nil) ;; clear after use
  (let (buffer-read-only)
    (ediff-setup-windows ediff-A-buffer ediff-B-buffer ediff-control-buffer))
  ;; Redisplay whatever buffers are showing, if there is a selected difference
  (if (and (>= ediff-current-difference 0)
	   (< ediff-current-difference ediff-number-of-differences))
      (let* ( ;; context must be saved before switching to windows A/B
	     (buffer-A ediff-A-buffer)
	     (buffer-B ediff-B-buffer)
	     (wind (selected-window))
	     (control-buf ediff-control-buffer)
	     (before-flag-shift (if (eq ediff-highlighting-style 'ascii)
				    (1- ediff-before-flag-length)
				  0))
	     (after-flag-shift (if (eq ediff-highlighting-style 'ascii)
				   (1- ediff-after-flag-length)
				 0))
	     (window-A (get-buffer-window buffer-A t))
	     (window-B (get-buffer-window buffer-B t)))
	
	(or no-rehighlight
	    (ediff-operate-on-flags 'insert))
	    
	(if window-A (progn
		       (select-window window-A)
		       (ediff-position-region
			(- (ediff-get-diff-posn 'A 'beg nil control-buf)
			   before-flag-shift)
			(+ (ediff-get-diff-posn 'A 'end nil control-buf)
			   after-flag-shift)
			(1+ (ediff-get-diff-posn 'A 'beg nil control-buf)))))
	(if window-B (progn
		       (select-window window-B)
		       (ediff-position-region
			(- (ediff-get-diff-posn 'B 'beg nil control-buf)
			   before-flag-shift)
			(+ (ediff-get-diff-posn 'B 'end nil control-buf)
			   after-flag-shift)
			(1+ (ediff-get-diff-posn 'B 'beg nil control-buf)))))
	(select-window wind))))
	
(defun ediff-toggle-split ()
  "Toggle vertical/horizontal window split. 
Does nothing if file-A and file-B are in different frames."
  (interactive)
  (let* ((wind-A (get-buffer-window ediff-A-buffer t))
	 (wind-B (get-buffer-window ediff-B-buffer t))
	 (frame-A (if wind-A (ediff-window-frame wind-A)))
	 (frame-B (if wind-B (ediff-window-frame wind-B))))
    (if (eq frame-A frame-B)
	(setq ediff-split-window-function
	      (if (eq ediff-split-window-function 'split-window-vertically)
		  'split-window-horizontally
		'split-window-vertically))
      (message "Buffers A and B are residing in different frames. Why split?"))
    (ediff-recenter 'no-rehighlight)))
  
(defun ediff-toggle-hilit ()
  "Switch between highlighting using ASCII flags and highlighting using faces.
On a dumb terminal, switches between ASCII highlighting and no highlighting." 
  (interactive)
  (if (not window-system)
      (if (eq ediff-highlighting-style 'ascii)
	  (progn
	    (message "ASCII highlighting flags removed.")
	    (ediff-unselect-and-select-difference ediff-current-difference
						  'unselect-only)
	    (setq ediff-highlighting-style 'off))
	(ediff-unselect-and-select-difference ediff-current-difference
					      'select-only))
    (ediff-unselect-and-select-difference ediff-current-difference
					  'unselect-only)
    (setq ediff-want-faces (null ediff-want-faces))
    (if ediff-want-faces
	(if (not (face-differs-from-default-p 'ediff-odd-diff-face-A-var))
	    (progn
	      (copy-face ediff-odd-diff-face-A 'ediff-odd-diff-face-A-var)
	      (copy-face ediff-odd-diff-face-B 'ediff-odd-diff-face-B-var)
	      (copy-face ediff-even-diff-face-A 'ediff-even-diff-face-A-var)
	      (copy-face ediff-even-diff-face-B 'ediff-even-diff-face-B-var)))
      (copy-face 'default 'ediff-odd-diff-face-A-var)
      (copy-face 'default 'ediff-odd-diff-face-B-var)
      (copy-face 'default 'ediff-even-diff-face-A-var)
      (copy-face 'default 'ediff-even-diff-face-B-var))
    
    (ediff-unselect-and-select-difference ediff-current-difference
					  'select-only))
  (ediff-operate-on-flags 'insert)
  )
  
(defun ediff-toggle-help ()
  "Toggle short/long help message."
  (interactive)
  (let (buffer-read-only)
    (erase-buffer)
    (if (string= ediff-help-message ediff-help-message-long)
	(setq ediff-help-message ediff-help-message-short)
      (setq ediff-help-message ediff-help-message-long)))
  (ediff-recenter 'no-rehighlight))
  
  
(defun ediff-toggle-read-only-A ()
  "Used as a startup hook to set `.orig' patch file read-only."
  (let ((last-command-char ?A))
    (ediff-toggle-read-only)))
  
(defun ediff-toggle-read-only ()
  "Toggles buffer-read-only for buffer buffers A and B."
  (interactive)
  (emerge-eval-in-buffer
   (if (eq last-command-char ?A) ediff-A-buffer ediff-B-buffer)
   (setq buffer-read-only (null buffer-read-only))))

;;; Window scrolling operations
;; These operations are designed to scroll all three windows the same amount,
;; so as to keep the text in them aligned.

;; Perform some operation on two file windows (if they are showing).
;; Catches all errors on the operation in the A and B windows.
;; Usually, errors come from scrolling off the
;; beginning or end of the buffer, and this gives nice nice error messages.
(defun ediff-operate-on-windows (operation arg)
  (let* ((buffer-A ediff-A-buffer)
	 (buffer-B ediff-B-buffer)
	 (wind (selected-window))
	 (window-A (get-buffer-window buffer-A t))
	 (window-B (get-buffer-window buffer-B t)))
      (if window-A (progn
		     (select-window window-A)
		     (condition-case nil
			 (funcall operation arg)
		       (error))))
      (if window-B (progn
		     (select-window window-B)
		     (condition-case nil
			 (funcall operation arg)
		       (error))))
      (select-window wind)
		       ))

(defun ediff-scroll-up (&optional arg)
  "Scroll up buffers A and B, if they are in windows.
With optional argument ARG, scroll ARG lines; otherwise scroll by nearly
the height of window-A."
  (interactive "P")
  (ediff-operate-on-windows
   'scroll-up 
   ;; calculate argument to scroll-up
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount (the window height)
     (let* ((window-A (get-buffer-window ediff-A-buffer t))
	    (window-B (get-buffer-window ediff-B-buffer t))
	    default-amount)
       (if (or (null window-A) (null window-B))
	   (setq default-amount 0)
	 (setq default-amount 
	       (- (min (window-height window-A) (window-height window-B))
		  1 next-screen-context-lines)))
       ;; the window was found
       (if arg
	   ;; C-u as argument means half of default amount
	   (/ default-amount 2)
	 ;; no argument means default amount
	 default-amount)))))

(defun ediff-scroll-down (&optional arg)
  "Scroll down buffers A and B, if they are in windows.
With optional argument ARG, scroll ARG lines; otherwise scroll by nearly
the height of window-A."
  (interactive "P")
  (ediff-operate-on-windows
   'scroll-down
   ;; calculate argument to scroll-down
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount (the window height)
     (let* ((window-A (get-buffer-window ediff-A-buffer t))
	    (window-B (get-buffer-window ediff-B-buffer t))
	    default-amount)
       (if (or (null window-A) (null window-B))
	   (setq default-amount 0)
	 (setq default-amount 
	       (- (min (window-height window-A) (window-height window-B))
		  1 next-screen-context-lines)))
       ;; the window was found
       (if arg
	   ;; C-u as argument means half of default amount
	   (/ default-amount 2)
	 ;; no argument means default amount
	 default-amount)))))

(defun ediff-scroll-left (&optional arg)
  "Scroll left buffer-A and buffer-B, if they are in windows.
If an argument is given, that is how many columns are scrolled, else nearly
the width of the A and B windows."
  (interactive "P")
  (ediff-operate-on-windows
   'scroll-left
   ;; calculate argument to scroll-left
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount
     ;; (half the window width)
     (if (null ediff-control-window)
	 ;; no control window, use nil
	 nil
       (let ((default-amount
	       (- (/ (window-width ediff-control-window) 2) 3)))
	 ;; the window was found
	 (if arg
	     ;; C-u as argument means half of default amount
	     (/ default-amount 2)
	   ;; no argument means default amount
	   default-amount))))))

(defun ediff-scroll-right (&optional arg)
  "Scroll right buffer-A and buffer-B, if they are in windows.
If an argument is given, that is how many columns are scrolled, else nearly
the width of the A and B windows."
  (interactive "P")
  (ediff-operate-on-windows
   'scroll-right
   ;; calculate argument to scroll-right
   ;; if there is an explicit argument
   (if (and arg (not (equal arg '-)))
       ;; use it
       (prefix-numeric-value arg)
     ;; if not, see if we can determine a default amount
     ;; (half the window width)
     (if (null ediff-control-window)
	 ;; no control window, use nil
	 nil
       (let ((default-amount
	       (- (/ (window-width ediff-control-window) 2) 3)))
	 ;; the window was found
	 (if arg
	     ;; C-u as argument means half of default amount
	     (/ default-amount 2)
	   ;; no argument means default amount
	   default-amount))))))

(defun ediff-position-region (beg end pos)
  "This is a variation on ``emerge-position-region''. 
The difference is that it always tries to align difference regions in
buffer-A and buffer-B, so that it will be easier to compare them."
  (set-window-start (selected-window) beg)
  (if (pos-visible-in-window-p end)
      ;; Determine the number of lines that the region occupies
      (let ((lines 0))
	(while (> end (progn
			(move-to-window-line lines)
			(point)))
	  (setq lines (1+ lines)))
	;; And position the beginning on the right line
	(goto-char beg)
	(recenter (/ (1+ (- (1- (window-height (selected-window)))
			    lines))
		     2))))
  (goto-char pos))


(defun ediff-next-difference (arg)
  "Advance to the next difference. 
With a prefix argument, go back that many differences."
  (interactive "P")
  (if (< ediff-current-difference ediff-number-of-differences)
      (let ((n (min ediff-number-of-differences
		    (+ ediff-current-difference (if arg arg 1))))
	    (buffer-read-only nil))
	(ediff-unselect-and-select-difference n))
    (error "At end of the difference list.")))

(defun ediff-previous-difference (arg)
  "Go to the previous difference. 
With a prefix argument, go back that many differences."
  (interactive "P")
  (if (> ediff-current-difference -1)
      (let ((n (max -1 (- ediff-current-difference (if arg arg 1))))
	    (buffer-read-only nil))
	  (ediff-unselect-and-select-difference n))
    (error "At beginning of the difference list.")))

(defun ediff-jump-to-difference (difference-number)
  "Go to the difference specified as a prefix argument."
  (interactive "p")
  (let ((buffer-read-only nil))
    (setq difference-number (1- difference-number))
    (if (and (>= difference-number -1)
	     (< difference-number (1+ ediff-number-of-differences)))
	(ediff-unselect-and-select-difference difference-number)
      (error "Bad difference number"))))

;;; Copying diffs.

(defun ediff-diff-to-diff (arg)
  "Copy buffer-A'th diff to buffer B.
If numerical prefix argument, copy this diff specified in the arg.
Otherwise, copy the difference given by ``ediff-current-difference''." 
  (interactive "P")
  (if arg
      (ediff-jump-to-difference arg))
  (ediff-copy-diff ediff-current-difference
		   (if (eq last-command-char ?a) 'B  'A))
  (ediff-recenter 'no-rehighlight))


(defun ediff-copy-diff (n buf-type)
  "Copy diff N from BUF-TYPE \(given as 'A or 'B\)."
  (let* ((other-buf (if (eq buf-type 'A) 
			ediff-B-buffer ediff-A-buffer))
	 (buf (if (eq buf-type 'A) 
		  ediff-A-buffer ediff-B-buffer))
	 (other-buf-type (if (eq buf-type 'A) 'B 'A))
	 (ctrl-buf ediff-control-buffer)
	 reg-to-copy reg-to-delete
	 reg-to-delete-beg reg-to-delete-end)
	
    (ediff-operate-on-flags 'remove)
    (setq reg-to-delete-beg
	  (ediff-get-diff-posn other-buf-type 'beg n ctrl-buf))
    (setq reg-to-delete-end
	  (ediff-get-diff-posn other-buf-type 'end n ctrl-buf))
    (setq reg-to-copy (emerge-eval-in-buffer
		       buf
		       (buffer-substring (ediff-get-diff-posn
					  buf-type 'beg n ctrl-buf)
					 (ediff-get-diff-posn
					  buf-type 'end n ctrl-buf))))
    (setq reg-to-delete (emerge-eval-in-buffer
			 other-buf
			 (buffer-substring reg-to-delete-beg
					   reg-to-delete-end)))
    (setq ediff-disturbed-overlays nil) ;; clear before use
    
    (if (string= reg-to-delete reg-to-copy)
	(progn
	  (ding)
	  (message
	   "Diff regions %d are identical in buffers A and B. Nothing copied." 
	   (1+ n)))
	
      ;; seems ok to copy
      (if (ediff-test-save-region n other-buf-type)
	    (condition-case conds
		(let (inhibit-read-only)
		  (emerge-eval-in-buffer
		   other-buf
		   ;; to prevent flags from interfering if buffer is writable
		   (setq inhibit-read-only (null buffer-read-only))
		   (let ((before-change-function nil))
		     (goto-char reg-to-delete-end)
		     (insert-before-markers reg-to-copy)
		     (if (ediff-if-lucid)
			 (progn
			   (ediff-collect-extents-lucid reg-to-delete-beg)
			   (if (> reg-to-delete-end reg-to-delete-beg)
			       (progn
				 (kill-region reg-to-delete-beg
					      reg-to-delete-end) 
				 (if (string= reg-to-copy "")
				     (ediff-adjust-disturbed-extents-lucid
				      reg-to-delete-beg)))))
		       (if (> reg-to-delete-end reg-to-delete-beg)
			   (kill-region reg-to-delete-beg reg-to-delete-end)
			 (ediff-move-disturbed-overlays reg-to-delete-beg)))
		     ))
		  (ediff-save-diff-region n other-buf-type reg-to-delete))
	      (error (message "%s %s"
			      (car conds)
			      (mapconcat 'prin1-to-string (cdr conds) " "))
		     (beep 1))))
      )
    (ediff-operate-on-flags 'insert)
    ))
     
(defun ediff-save-diff-region (n buf-type reg)
  "Save N-th diff of buffer BUF-TYPE \('A or 'B\) on the
``ediff-killed-diffs-alist''.  REG is the region to save.
It is redundant here,but is passed anyway, for convenience."

  (let* ((n-th-diff-saved (assoc n ediff-killed-diffs-alist))
	 (this-buf-n-th-diff-saved (assoc buf-type (cdr n-th-diff-saved))))
	 
    (if this-buf-n-th-diff-saved
	;; either nothing saved for n-th diff and buffer or we OK'ed
	;; overriding
	(setcdr this-buf-n-th-diff-saved reg)
      (if n-th-diff-saved ;; n-th diff saved, but for another buffer
	  (nconc n-th-diff-saved  (list (cons buf-type reg)))
	(setq ediff-killed-diffs-alist  ;; create record for n-th diff
	      (cons (list n (cons buf-type reg))
		    ediff-killed-diffs-alist))))
    (message "Saved diff region #%d for buffer %S. To recover hit '%s'."
	     (1+ n) buf-type (if (eq buf-type 'A) "ra" "rb"))))
    
(defun ediff-test-save-region (n buf-type)
  "Test if saving N-th difference region of buffer BUF-TYPE is possible."
  (let* ((n-th-diff-saved (assoc n ediff-killed-diffs-alist))
	 (this-buf-n-th-diff-saved (assoc buf-type (cdr n-th-diff-saved))))
	 
    (if this-buf-n-th-diff-saved
	(if (yes-or-no-p
	     (format 
	      "You've previously copied diff %d from %S to %S. Confirm. "
	      (1+ n) (if (eq buf-type 'A) 'B 'A) buf-type))
	    t
	  (error "Quit."))
      t)))
	  
(defun ediff-pop-diff (n buf-type)
  "Pop last killed N-th diff region from buffer BUF-TYPE."
  (let* ((n-th-record (assoc n ediff-killed-diffs-alist))
	 (saved-rec (assoc buf-type (cdr n-th-record)))
	 (buf (if (eq buf-type 'A) ediff-A-buffer ediff-B-buffer))
	 saved-diff reg-beg reg-end recovered)
	
    (if (cdr saved-rec)
	(setq saved-diff (cdr saved-rec))
      (error "Nothing saved for diff %d in buffer %S." (1+ n) buf-type))
    
    (ediff-operate-on-flags 'remove)
	
    (setq reg-beg (ediff-get-diff-posn buf-type 'beg n ediff-control-buffer))
    (setq reg-end (ediff-get-diff-posn buf-type 'end n ediff-control-buffer))
    (setq ediff-disturbed-overlays nil) ;; clear before use
    
    (condition-case conds
	(emerge-eval-in-buffer
	 buf
	 (let ((inhibit-read-only (null buffer-read-only))
	       (before-change-function nil))
	   (goto-char reg-end)
	   (insert-before-markers saved-diff)
	   
	   (if (ediff-if-lucid)
	       (progn
		 (ediff-collect-extents-lucid reg-beg)
		 (if (> reg-end reg-beg)
		     (progn
		       (kill-region reg-beg reg-end)
		       (if (string= saved-diff "")
			   (ediff-adjust-disturbed-extents-lucid reg-beg)))))
	     (if (> reg-end reg-beg)
		 (kill-region reg-beg reg-end)
	       (ediff-move-disturbed-overlays reg-beg)))
	     
	   (setq recovered t)
	   ))
      (error (message "%s %s"
		      (car conds)
		      (mapconcat 'prin1-to-string (cdr conds) " "))
	     (beep 1)))
    
    (ediff-operate-on-flags 'insert)
    (if recovered
	(progn
	  (setq  n-th-record (delq saved-rec n-th-record))
	  (message "Diff region %d restored for buffer %S." (1+ n) buf-type)))
    ))
      
(defun ediff-restore-diff  (arg)
  "Restore ARG-th diff from ediff-killed-diffs-alist.
ARG is a prefix argument.  If ARG is `nil', restore current-difference."
  (interactive "P")
  (if arg
      (ediff-jump-to-difference arg))
  (ediff-pop-diff ediff-current-difference 
		  (if (eq last-command-char ?a) 'A   'B))
  (ediff-recenter 'no-rehighlight))
    

;;; Quitting, suspending, etc.
(defun ediff-quit ()
  "Finish an Ediff session and exit Ediff.
Unselects the selected difference, if any, restores the read-only and modified
flags of the compared file buffers, kills Ediff buffers for this session
\(but not file-A and file-B\)."
  (interactive)
  (if (prog1
	  (y-or-n-p "Do you really want to exit Ediff? ")
	(message ""))
      (ediff-really-quit)))

;; Perform the quit operations.
(defun ediff-really-quit ()
  (setq buffer-read-only nil)
  (ediff-unselect-and-select-difference -1)
  ;; null out the difference overlays so they won't slow down future editing
  ;; operations
  (mapcar (function (lambda (d)
		      (ediff-delete-overlay (aref d 0))
		      (ediff-delete-overlay (aref d 1))))
	  ediff-difference-vector)
  ;; allow them to be garbage collected
  (setq ediff-difference-vector nil)
  (setq ediff-help-message ediff-help-message-long)
  (ediff-restore-buffer-characteristics t) ;; restore as they were at setup
  (ediff-unhighlight-diffs-totally)
  
  ;; restore buffer mode line id's in buffer-A/B
  (let ((control-buffer ediff-control-buffer))
    (emerge-eval-in-buffer
     ediff-A-buffer
     (setq local-write-file-hooks 
	   (delq 'ediff-block-write-file local-write-file-hooks))
     (setq before-change-function nil)
     (setq ediff-this-buffer-control-sessions 
	   (delq control-buffer ediff-this-buffer-control-sessions))
     (kill-local-variable 'mode-line-buffer-identification))
    (emerge-eval-in-buffer
     ediff-B-buffer
     (setq local-write-file-hooks 
	   (delq 'ediff-block-write-file local-write-file-hooks))
     (setq ediff-this-buffer-control-sessions 
	   (delq control-buffer ediff-this-buffer-control-sessions))
     (setq before-change-function nil)
     (kill-local-variable 'mode-line-buffer-identification)))
   
  (run-hooks 'ediff-quit-hooks))
  
(defun ediff-kill-buffer-carefully (buf)
  "Kill buffer BUF if it exists."
  (if (ediff-buffer-live-p buf)
      (kill-buffer (get-buffer buf))))

;; The default way of quitting Ediff.
;; Kills control buffers and leaves the
;; frame split between the two diff'ed files.
(defun ediff-default-quit-hook ()
  (let ((buff-A ediff-A-buffer)
	(buff-B ediff-B-buffer))
    (ediff-kill-buffer-carefully ediff-diff-buffer)
    (ediff-kill-buffer-carefully ediff-diff-error-buffer)
    (ediff-kill-buffer-carefully ediff-control-buffer)
    (ediff-kill-buffer-carefully ediff-patch-diagnostics)
    (delete-other-windows)
    (switch-to-buffer buff-B)
    (split-window-vertically)
    (switch-to-buffer buff-A)))
    
;; The default way of suspending Ediff.
;; Buries Ediff buffers, kills all windows.
(defun ediff-default-suspend-hook ()
  (let ((buf-A ediff-A-buffer)
	(buf-B ediff-B-buffer)
	(buf-patch ediff-patch-buf)
	(buf-patch-diag ediff-patch-diagnostics)
	(buf-err  ediff-diff-error-buffer)
	(buf-diff ediff-diff-buffer))
    (bury-buffer) ;; ediff-control-buffer
    (delete-other-windows)
    (bury-buffer buf-err)
    (bury-buffer buf-diff)
    (bury-buffer buf-patch)
    (bury-buffer buf-patch-diag)
    (bury-buffer buf-A)
    (bury-buffer buf-B)))
     
     
(defun ediff-suspend ()
  "Suspend Ediff.  To resume, switch to the appropriate ``*ediff-control*''
buffer and then hit ``\\[ediff-recenter]''.  Ediff will automatically set
up an appropriate window config."
  (interactive)
  (run-hooks 'ediff-suspend-hooks)
  (message
   "To resume, switch to *ediff-control* and hit 'c' (ediff-recenter)."))


(defun ediff-file-names ()
  "Show the names of the buffers or files being operated on by Ediff.
Hit ``\\[ediff-recenter]'' to reset the windows afterward."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (emerge-eval-in-buffer ediff-A-buffer
			   (if buffer-file-name
			       (progn
				 (princ "File A is: ")
				 (princ buffer-file-name))
			     (progn
			       (princ "Buffer A is: ")
			       (princ (buffer-name))))
			   (princ "\n"))
    (emerge-eval-in-buffer ediff-B-buffer
			   (if buffer-file-name
			       (progn
				 (princ "File B is: ")
				 (princ buffer-file-name))
			     (progn
			       (princ "Buffer B is: ")
			       (princ (buffer-name))))
			   (princ "\n"))
    ))



(defun ediff-line-numbers ()
  "Display the current line numbers.
This function displays the line numbers of the points in the A, B."
  (interactive)
  (let* ((A-line (emerge-eval-in-buffer ediff-A-buffer
				       (count-lines (point-min) (point))))
	 (B-line (emerge-eval-in-buffer ediff-B-buffer
				       (count-lines (point-min) (point)))))
    (message "At lines:  A = %d, B = %d" A-line B-line)))


;;; Support routines

;; Select a difference by placing the ASCII flags around the appropriate
;; group of lines in the A, B buffers
(defun ediff-select-difference (n)
  (if (and (>= n 0) (< n ediff-number-of-differences))
      (progn
	(ediff-remember-buffer-characteristics)
	(if (and window-system ediff-want-faces)
	    (progn
	      (ediff-highlight-diff n)
	      (setq ediff-highlighting-style 'face))
	  (setq ediff-highlighting-style 'ascii)
	  (ediff-place-flags-in-buffer 'A ediff-A-buffer
				       ediff-control-buffer n)
	  (ediff-place-flags-in-buffer 'B ediff-B-buffer
				       ediff-control-buffer n)) 
	  
	(ediff-restore-buffer-characteristics)
	(run-hooks 'ediff-select-hooks))))
	

;; Unselect a difference by removing the ASCII flags in the buffers.
(defun ediff-unselect-difference (n)
  (if (and (>= n 0) (< n ediff-number-of-differences))
      (progn 
	(ediff-remember-buffer-characteristics)
	
	(cond ((and window-system ediff-want-faces)
	       (ediff-unhighlight-diff))
	      ((eq ediff-highlighting-style 'ascii)
	       (ediff-remove-flags-from-buffer
		ediff-A-buffer
		(ediff-get-diff-posn 'A 'beg n)
		(ediff-get-diff-posn 'A 'end n))
	       (ediff-remove-flags-from-buffer
		ediff-B-buffer
		(ediff-get-diff-posn 'B 'beg n)
		(ediff-get-diff-posn 'B 'end n))))
		
	(ediff-restore-buffer-characteristics)
	(setq ediff-highlighting-style nil)
	(run-hooks 'ediff-unselect-hooks))))
  

;; Unselects prev diff and selects a new one, if FLAG has value other than
;; 'select-only or 'unselect-only.  If FLAG is 'select-only, the
;; next difference is selected, but the current selection is not
;; unselected.  If FLAG is 'unselect-only then the current selection is
;; unselected, but the next one is not selected.  If NO-RECENTER is non-nil,
;; don't recenter buffers after selecting/unselecting.
;; 
;; Don't use ``ediff-select-difference'' and ``ediff-unselect-difference''
;; directly,;; since this will screw up the undo info in the presence of
;; ASCII flags. 
;; Instead, use ``ediff-unselect-and-select-difference'' with appropriate
;; flags.

(defun ediff-unselect-and-select-difference (n &optional flag no-recenter)
  (let ((wind (selected-window))
	;; save buf modified info
	(buf-A-modified (buffer-modified-p ediff-A-buffer))
	(buf-B-modified (buffer-modified-p ediff-B-buffer))
	;; temporarily disable undo so highlighting won't confuse the user
	buf-A-undo buf-B-undo)
	
      (emerge-eval-in-buffer
       ediff-A-buffer
       (setq buf-A-undo buffer-undo-list))
      (emerge-eval-in-buffer
       ediff-B-buffer
       (setq buf-B-undo buffer-undo-list))
       
      (buffer-disable-undo ediff-A-buffer)
      (buffer-disable-undo ediff-B-buffer)
    
      (unwind-protect    ;; we don't want to lose undo info due to error
	  (progn
	    (or (eq flag 'select-only)
		(ediff-unselect-difference ediff-current-difference))
		
	    ;; Auto-save buffers while Ediff flags are temporarily removed.
	    (emerge-eval-in-buffer
	     ediff-A-buffer
	     (if buf-A-modified
		 (do-auto-save)))
	    (emerge-eval-in-buffer
	     ediff-B-buffer
	     (if buf-B-modified
		 (do-auto-save)))
    
	    (or (eq flag 'unselect-only)
		(ediff-select-difference n))
	    (setq ediff-current-difference n)
	    ) ;; end protected section
      
	(select-window wind) ;; must be before recenter!
	(ediff-refresh-mode-line)
	(or no-recenter
	    (ediff-recenter 'no-rehighlight))
	  
	;; restore undo and buffer-modified info
	(emerge-eval-in-buffer
	 ediff-A-buffer
	 (set-buffer-modified-p buf-A-modified)
	 (setq buffer-undo-list buf-A-undo))
	(emerge-eval-in-buffer
	 ediff-B-buffer
	 (set-buffer-modified-p buf-B-modified)
	 (setq buffer-undo-list buf-B-undo))
	)))

;; Revise the mode line to display which difference we have selected

(defun ediff-refresh-mode-line ()
  (setq mode-line-buffer-identification
	(list (format "Ediff: %%b   diff %d of %d"
		      (1+ ediff-current-difference)
		      ediff-number-of-differences)))
  ;; Force mode-line redisplay
  (set-buffer-modified-p (buffer-modified-p)))



;; Verify that we have a difference selected.
(defun ediff-validate-difference ()
  (if (not (and (>= ediff-current-difference 0)
		(< ediff-current-difference ediff-number-of-differences)))
      (error "No difference selected")))

;; The following is added to take care of Lemacs.

	 
(defun ediff-read-file-name (prompt default-dir default-file A-file)
; This is a modified version of a similar function in ``emerge.el''.
; PROMPT should not have trailing ': ', so that it can be modified
; according to context.
; If both A-FILE and default-dir are set, the file constructed our of
; default-dir and the non-directory part of A-FILE is used as default and as
; initial input.
; If A-FILE is set (but default-dir is not), it is used as default and
; initial input. 
; If default-file is set, it should be used as the default value.
; If default-dir is non-nil, use it as the default directory.
; Otherwise, use the value in Emacs's var default-directory.
  (cond
   ((and A-file default-dir)
    (read-file-name (format "%s (default %s%s): "
			    prompt
			    (abbreviate-file-name
			     (expand-file-name default-dir))
			    (file-name-nondirectory A-file))
		    (expand-file-name default-dir)
		    (concat (expand-file-name default-dir)
			    (file-name-nondirectory A-file))
		    'confirm (file-name-nondirectory A-file)))
   (A-file
    (read-file-name (format "%s (default %s): "
			    prompt (file-name-nondirectory A-file))
		    (expand-file-name (file-name-directory A-file))
		    A-file
		    'confirm (file-name-nondirectory A-file)))
   ;; If there is a default file, but no A-file, use it.
   (default-file
     (read-file-name (format "%s (default %s): " prompt default-file)
		     default-dir ;; if nil then default-directory.
		     nil 'confirm))
   (t
    (read-file-name (concat prompt ": ")
		    default-dir ;; if nil then default-directory.
		    nil 'confirm))))

(defun ediff-new-flags ()
;; Function to be called to set up Emerge's ASCII flags and
;; to compute values that depend on the flags.
;; This function is a plagiarized ``emerge-new-flags'' from Emerge 5fsf.  It is
;; here, in Ediff, because some versions of Emerge don't have this function.
  (setq ediff-before-flag-length (length ediff-before-flag))
  (setq ediff-before-flag-lines
	(emerge-count-matches-string ediff-before-flag "\n"))
  (setq ediff-before-flag-match (regexp-quote ediff-before-flag))
  (setq ediff-after-flag-length (length ediff-after-flag))
  (setq ediff-after-flag-lines
	(emerge-count-matches-string ediff-after-flag "\n"))
  (setq ediff-after-flag-match (regexp-quote ediff-after-flag)))
  
(defun ediff-make-temp-file (prefix)
  (let ((f (make-temp-name (concat ediff-temp-file-prefix prefix))))
    ;; create the file
    (write-region (point-min) (point-min) f nil 'no-message)
    (set-file-modes f ediff-temp-file-mode)
    f))
    
(defun ediff-block-write-file ()    
  "Prevent writing files A and B directly."
  (error "Use 'wa' and 'wb' to save buffs A/B (first switch back to *ediff-control*."))

(defun ediff-insert-in-front (overl beg end)
  "Capture overlays that had insertions in the front.
Called when overlay OVERL gets insertion in front."
  (if (ediff-overlay-get overl 'ediff-diff-num)
      (setq ediff-disturbed-overlays
	    (cons overl ediff-disturbed-overlays)))
  )
  
(defun ediff-collect-extents-lucid (pos)
  "Collects all extents at POS having property `ediff-diff-num'.
Lucid Emacs causes headache by detaching empty extents, so I have to save
them before they disappear."
  (let (lis elt)
    (while (setq elt (extent-at pos nil 'ediff-diff-num elt))
      (setq lis (cons elt lis)))
    (setq ediff-disturbed-overlays lis)))
  
(defun ediff-move-disturbed-overlays (posn)  
  (mapcar (function (lambda (overl)
		       (ediff-move-overlay overl
					   posn
					   (ediff-overlay-end overl))
		       ))
	  ediff-disturbed-overlays)
  (setq ediff-disturbed-overlays nil))
  
(defun ediff-adjust-disturbed-extents-lucid (posn &optional posn-type)
;; POSN-TYPE tells if POSN should become a new start of the extents
;; (if 'new-start) or a new end (if 'new-end). If POSN-TYPE is nil, then
;; POSN is both the new start and the new end.
  (mapcar (function (lambda (overl)
		       (cond ((and (null posn-type)
				   (equal (ediff-overlay-start overl)
					  (ediff-overlay-end overl)))
			      (ediff-move-overlay overl posn posn))
			   
			     (posn-type
			      (ediff-move-overlay
			       overl
			       (if (eq posn-type 'new-start)
				   posn
				 (ediff-overlay-start overl))
			       (if (eq posn-type 'new-end)
				   posn
				 (ediff-overlay-end overl)))))))
	  ediff-disturbed-overlays)
    (setq ediff-disturbed-overlays nil))
  
(defun ediff-save-buffer ()
  "Safe way of saving buffers A and B."
  (interactive)
  (let ((hooks local-write-file-hooks))
    (ediff-unselect-and-select-difference ediff-current-difference
					  'unselect-only)
    (unwind-protect
	(emerge-eval-in-buffer
	 (if (eq last-command-char ?a) ediff-A-buffer ediff-B-buffer)
	 ;; temporarily remove writing block 
	 (setq hooks (delq 'ediff-block-write-file hooks))
	 (let ((local-write-file-hooks hooks))
	   (save-buffer)))
      (ediff-unselect-and-select-difference ediff-current-difference
					    'select-only)
      )))
    

       
(defun ediff-remove-flags-from-buffer (buffer before after)
  "Essentially ``emerge-remove-flags-in-buffer'', modified to allow deletion
of read-only flags."
  (emerge-eval-in-buffer
   buffer
   (let ((buffer-read-only nil)
	 (before-change-function nil)
	 (inhibit-read-only t))
     (goto-char after)
     (setq after (point-marker))  ;; this is because after is only a posn
     ;; remove the flags, if they're there
     (goto-char (- before ediff-before-flag-length))
     (if (ediff-if-lucid)
	 (ediff-collect-extents-lucid (+ (point) ediff-before-flag-length)))
     (if (looking-at ediff-before-flag-match)
	 (delete-region (point) (+ (point) ediff-before-flag-length))
       ;; the flag isn't there
       (ding)
       (message "Trouble removing ASCII flag"))
     (if (ediff-if-lucid)
	 (ediff-adjust-disturbed-extents-lucid (point)))
	 
     (if (ediff-if-lucid)
	 (ediff-collect-extents-lucid (point)))
	 ;(ediff-collect-extents-lucid (max (point-min) (1- (point)))))
     (goto-char after)
     (if (looking-at ediff-after-flag-match)
	 (delete-region (point) (+ (point) ediff-after-flag-length))
       ;; the flag isn't there
       (ding)
       (message "Trouble removing ASCII flag"))
     (if (ediff-if-lucid)
	 (ediff-adjust-disturbed-extents-lucid (point)))
     (setq after nil) ;; after has become a marker--garbage-collect
     )))


(defun ediff-place-flags-in-buffer (buf-type buffer ctl-buffer difference)
  "This is a modified ``emerge-place-flags-in-buffer''."
  (emerge-eval-in-buffer
   buffer
   (ediff-place-flags-in-buffer1 buf-type ctl-buffer difference)))

(defun ediff-place-flags-in-buffer1 (buf-type ctl-buffer difference)
  "Modified ``emerge-place-flags-in-buffer1''."
  (let ((buffer-read-only nil)
	(inhibit-read-only t)
	(before-change-function nil))
    ;; insert the flag before the difference
    (let ((before (ediff-get-diff-posn buf-type 'beg difference ctl-buffer)))
      (goto-char before)
      ;; insert the flag itself
      (if (ediff-if-lucid)
	  (ediff-collect-extents-lucid (point)))
      (insert-before-markers ediff-before-flag)
      (if (ediff-if-lucid)
	  ;; Lucid's extent end-points behave strangely; they won't
	  ;; respect insert-before-markers
	  (ediff-adjust-disturbed-extents-lucid (point) 'new-start))
      )
    ;; insert the flag after the difference
    (let* ((after (ediff-get-diff-posn buf-type 'end difference ctl-buffer)))
      (goto-char after)
      ;; insert the flag itself
      (if (ediff-if-lucid)
	  (ediff-collect-extents-lucid (point)))
      (insert ediff-after-flag)
      (if (ediff-if-lucid)
	  (ediff-adjust-disturbed-extents-lucid after 'new-end))
      )))

  
(defun ediff-get-diff-posn (buf-type pos &optional n control-buf)
  "Returns positions of difference sectors in the buffer denoted BUF-TYPE
\('A or 'B\). 
POS is either 'beg or 'end.  Specifies whether you want the position at the
beginning of a difference of at the end.  Optional N says which difference
\(default: ``ediff-current-difference''\).  Optional CONTROL-BUF says which
control buffer is in effect in case it is not the current buffer."

  (let (diff-overlay)
    (or control-buf
	(setq control-buf (current-buffer)))

    (emerge-eval-in-buffer
     control-buf
     (or n  (setq n ediff-current-difference))
     (if (or (< n 0) (>= n ediff-number-of-differences))
	 (error "There is no diff %d in this session. Valid diffs are 1 to %d."
		(1+ n) ediff-number-of-differences))
     (setq diff-overlay (aref (aref ediff-difference-vector n)
			      (if (eq buf-type 'A) 0 1))))
    
    (if (ediff-overlay-get diff-overlay 'detached)
	(ediff-move-overlay diff-overlay
			    (ediff-overlay-get diff-overlay 'ediff-marker)
			    (ediff-overlay-get diff-overlay 'ediff-marker)))
    (if (eq pos 'beg)
	(ediff-overlay-start diff-overlay)
      (ediff-overlay-end diff-overlay))
    ))
    


;; These would highlight differences under X
(defun ediff-highlight-diff (n)
  "Put face on diff N.  Invoked for X displays only."
  (let* ((last-A (emerge-eval-in-buffer ediff-A-buffer (point-max)))
	 (last-B (emerge-eval-in-buffer ediff-B-buffer (point-max)))
	 (begin-A (ediff-get-diff-posn 'A 'beg n))
	 (end-A (ediff-get-diff-posn 'A 'end n))
	 (xtraA (if (equal begin-A end-A) 1 0))
	 (end-A-hilit (min last-A (+ end-A xtraA)))
	 
	 (begin-B (ediff-get-diff-posn 'B 'beg n))
	 (end-B (ediff-get-diff-posn 'B 'end n))
	 (xtraB (if (equal begin-B end-B) 1 0))
	 (end-B-hilit (min last-B (+ end-B xtraB))))
	  
    (if (ediff-if-lucid)
	(progn 
	  (ediff-move-overlay
	   ediff-current-diff-overlay-A begin-A end-A-hilit)
	  (ediff-move-overlay
	   ediff-current-diff-overlay-B begin-B end-B-hilit))
      ;; GNU stuff has a bug, which requires that ediff-move-overlay will
      ;; have the buffer as a parameter. Will be fixed in 19.23
      (ediff-move-overlay ediff-current-diff-overlay-A
			  begin-A end-A-hilit ediff-A-buffer)
      (ediff-move-overlay ediff-current-diff-overlay-B
			  begin-B end-B-hilit ediff-B-buffer))
    ;; giving priority of 0 and then changing it may look funny, but
    ;; this is intended to overcome an Emacs bug.
    (ediff-overlay-put ediff-current-diff-overlay-A 'priority  0)
    (ediff-overlay-put ediff-current-diff-overlay-B 'priority  0)
    (ediff-overlay-put ediff-current-diff-overlay-A 'priority  
		 (ediff-highest-priority begin-A end-A-hilit ediff-A-buffer))
    (ediff-overlay-put ediff-current-diff-overlay-B 'priority 
		 (ediff-highest-priority begin-B end-B-hilit ediff-B-buffer))
		 
    (if (not (face-differs-from-default-p 'ediff-odd-diff-face-A-var))
	(progn
	  (copy-face ediff-odd-diff-face-A 'ediff-odd-diff-face-A-var)
	  (copy-face ediff-odd-diff-face-B 'ediff-odd-diff-face-B-var)
	  (copy-face ediff-even-diff-face-A 'ediff-even-diff-face-A-var)
	  (copy-face ediff-even-diff-face-B 'ediff-even-diff-face-B-var)))
	  
    ;; unhighlight the background overlay for the diff n so they won't
    ;; interfere with the current diff overlay
    (ediff-overlay-put (aref (aref ediff-difference-vector n) 0) 'face nil)
    (ediff-overlay-put (aref (aref ediff-difference-vector n) 1) 'face nil)
    
    (sit-for 0) ;; needs to synch for some reason
    ))


(defun ediff-unhighlight-diff ()
  "Remove overlays from buffers A and B."
    
  (ediff-move-overlay ediff-current-diff-overlay-A 1 1)
  (ediff-move-overlay ediff-current-diff-overlay-B 1 1)
  
  ;; rehighlight the overlay in the background of the
  ;; current difference region
  (ediff-overlay-put (aref (aref ediff-difference-vector
				 ediff-current-difference) 
			   0)
		     'face (if (ediff-odd-p ediff-current-difference)
			       'ediff-odd-diff-face-A-var
			     'ediff-even-diff-face-A-var))
  (ediff-overlay-put (aref (aref ediff-difference-vector
				 ediff-current-difference) 
			   1)
		     'face (if (ediff-odd-p ediff-current-difference)
			       'ediff-odd-diff-face-B-var
			     'ediff-even-diff-face-B-var))
			     
  )


(defun ediff-unhighlight-diffs-totally ()
    (if (and window-system ediff-want-faces)
	(let ((inhibit-quit t))
	  (if (face-differs-from-default-p 'ediff-odd-diff-face-A-var)
	      (progn
		(copy-face 'default 'ediff-odd-diff-face-A-var)
		(copy-face 'default 'ediff-odd-diff-face-B-var)
		(copy-face 'default 'ediff-even-diff-face-A-var)
		(copy-face 'default 'ediff-even-diff-face-B-var)))
	  (if (ediff-overlayp ediff-current-diff-overlay-A)
	      (ediff-delete-overlay ediff-current-diff-overlay-A))
	  (setq ediff-current-diff-overlay-A nil)
	  (if (ediff-overlayp ediff-current-diff-overlay-B)
	      (ediff-delete-overlay ediff-current-diff-overlay-B))
	  (setq ediff-current-diff-overlay-B nil))))

	  
(defun ediff-operate-on-flags (action)
  "Re/unhighlights buffers A and B with all flags from all active Ediff
sessions that involve these buffers.  This is usually needed only when a
buffer is involved in multiple Ediff sessions."
  (let* ((A-sessions (emerge-eval-in-buffer
		      ediff-A-buffer
		      ediff-this-buffer-control-sessions))
	 (B-sessions (emerge-eval-in-buffer
		      ediff-B-buffer
		      ediff-this-buffer-control-sessions))
	 (sessions (ediff-union A-sessions B-sessions))
	 (flag (if (eq action 'remove) 'unselect-only 'select-only)))
	 
    (mapcar (function (lambda (buf)
			(emerge-eval-in-buffer
			 buf
			 (or (if (eq action 'insert)
				 (memq ediff-highlighting-style '(ascii off))
			       (not (eq ediff-highlighting-style 'ascii)))
			     (ediff-unselect-and-select-difference
			      ediff-current-difference 
			      flag 'no-recenter))
			 )))
	    sessions)))
     
(defun ediff-union (list1 list2)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
This is a slightly simplified version from ``cl-seq.el''.  Added here to
avoid loading cl-*."
  (cond ((null list1) list2) ((null list2) list1)
	((equal list1 list2) list1)
	(t
	 (or (>= (length list1) (length list2))
	     (setq list1 (prog1 list2 (setq list2 list1))))
	 (while list2
	   (or (memq (car list2) list1)
	       (setq list1 (cons (car list2) list1)))
	   (setq list2 (cdr list2)))
	 list1)))
	 
(defun ediff-debug ()
  (interactive)
  (with-output-to-temp-buffer "*ediff-debug*"
    (princ 
     (format "Ctl buffer: %S\n\nediff-difference-vector:\n"
	     ediff-control-buffer))
    (mapcar (function
	     (lambda (overl-vec)
	       (princ (format "Diff %d:  %S %S %S\n\t %S %S %S\n" 
			      (1+ (ediff-overlay-get (aref overl-vec 0)
						     'ediff-diff-num))
			      (ediff-overlay-get (aref overl-vec 0)
						 'ediff-ctl-buf) 
			      (ediff-overlay-get (aref overl-vec 0)
						 'insert-in-front-hooks)
			      (aref overl-vec 0)
			      (ediff-overlay-get (aref overl-vec 1)
						 'ediff-ctl-buf) 
			      (ediff-overlay-get (aref overl-vec 0)
						 'insert-in-front-hooks)
			      (aref overl-vec 1)
			      ))))
	    ediff-difference-vector)
    (princ "\nediff-disturbed-overlays:\n")
    (mapcar (function
	     (lambda (overl)
	       (princ (format "%S  %S\n"
			      (ediff-overlay-get overl 'ediff-ctl-buf)
			      overl
			      ))))
	    ediff-disturbed-overlays)))

  
;; Calculate dependent variables
(ediff-new-flags)
  
(run-hooks 'ediff-load-hooks)
  

(provide 'ediff)

;;; ediff.el ends here
