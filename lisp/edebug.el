;; edebug.el  A source level debugger for Emacs Lisp.
;; Copyright (C) 1988-1992, Free Software Foundation, Inc

;; LCD Archive Entry:
;; edebug|Daniel LaLiberte|liberte@cs.uiuc.edu
;; |A source level debugger for Emacs Lisp.
;; |$Date: 92/06/03 13:29:45 $|$Revision: 2.9 $|~/modes/edebug.el

;; This file will be part of GNU Emacs.

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

;;;================================================================
;;; Contents of this file:
;;; =========
;;; Introduction
;;; Installation
;;; Change list
;;; Utilities
;;; Parser
;;; Debugger

;;; Introduction
;;; ------------
;;; This minor mode allows programmers to step through elisp source
;;; code while executing functions.  You can also set breakpoints,
;;; trace (stopping at each expression), evaluate expressions as if
;;; outside edebug, reevaluate and display a list of expressions,
;;; catch errors normally caught by debug, and display a debug style
;;; backtrace.

;;; To install, put edebug.el in some directory in your load-path and
;;; byte-compile it.  Put the following forms in your .emacs file.

;;; (define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)
;;; (autoload 'edebug-defun "edebug")

;;; If you wish to change the default edebug global command prefix...
;;; (setq edebug-global-prefix "...whatever you want")  ; default is C-xX

;;; ... other options, described below.

;;; Evaluate a defun or defmacro for edebug with edebug-defun (C-xx or C-xX).
;;; Evaluate your function normally and edebug should be invoked.
;;; Use the "?" command in edebug to describe other commands.
;;; See edebug.texinfo for more instructions.

;;; Send me your enhancements, ideas, bugs, or fixes.
;;; There is an edebug mailing list if you want to keep up
;;; with the latest developments: edebug@cs.uiuc.edu
;;; (requests to: edebug-request@cs.uiuc.edu)

;;; Daniel LaLiberte   217-244-0785
;;; University of Illinois, Urbana-Champaign
;;; Department of Computer Science
;;; 1304 W Springfield
;;; Urbana, IL  61801

;;; uiucdcs!liberte
;;; liberte@cs.uiuc.edu


;;;================================================================
;;; Change list
;;; -----------

;;; $Header: edebug.el,v 2.9 92/06/03 13:29:45 jwz Exp $
;;; $Log:	edebug.el,v $
;;; Revision 2.9  92/06/03  13:29:45  jwz
;;; *** empty log message ***
;;; 
;;; Revision 2.8  92/05/16  14:10:55  devin
;;; *** empty log message ***
;;; 
;;; Revision 2.7  92/03/23  18:29:26  liberte
;;; Fix edebug-get-displayed-buffer-points to actually change buffers.
;;; 
;;; Restore current buffer in edebug-set-buffer-points
;;; 
;;; Use epoch::version instead of edebug-epoch-running.
;;; 
;;; Apparently we need to set-buffer in edebug-pop-to-buffer,
;;; even after select-window.
;;; 
;;; Define dynamically bound variables to quite byte-compiler,
;;; but leave them unbound to cause runtime error if used improperly.
;;; 
;;; Fix other problems with window-start, current-buffer, and
;;; edebug-outside-excursion.
;;; 
;;; Revision 2.6  92/03/19  12:55:58  liberte
;;; Disable edebug-save-point.  Now point of source code buffers is always
;;; saved, mark is never saved, and window-start is always saved.
;;; 
;;; Change name of edebug-save-buffer-points to
;;; edebug-save-displayed-buffer-points.  Also, if non-nil, only displayed
;;; buffer points are saved.
;;; 
;;; Restructure definition of epoch specific functions so there is no
;;; overhead for non-epoch use.
;;; 
;;; Add support for custom-print functions to handle print-level and
;;; print-circle.  Use edebug-prin* functions instead of standard
;;; print functions.
;;; 
;;; Yet another change of the scheme for wrapping edebug calls:
;;; edebug-enter gets a lambda form which can be byte-compiled;
;;; edebug-after gets the after expression index from edebug-before which
;;; is given the before expression index.  (Perhaps it is false economy to
;;; avoid the after expression index.)  edebug-after also gets the
;;; evaluated expression result, so no explicit evals need be done.
;;; 
;;; Most of edebug-defun was moved to edebug-func-form which also
;;; handles embedded defuns.
;;; 
;;; Add functions edebug-forms and edebug-sexps.
;;; 
;;; Rename edebug-list to edebug-list-form.
;;; 
;;; Use edebug-form-specs for all special forms.  The spec may now be
;;; a function which is called to process args.  Added -form to
;;; the names of special form parser functions.
;;; 
;;; Rename edebug-form-parser to edebug-interpret-form-spec.  Add handling
;;; of [...], function spec, and backtracking.  &optional now only applies
;;; to one following spec.  Fixed some other bugs.
;;; 
;;; Added macro def-edebug-form-spec for convenience, and to convert
;;; 0 and t values to edebug-forms and edebug-sexps.
;;; 
;;; Add edebug-form-specs for mapcar, mapconcat, mapatoms, apply, and funcall
;;; that all use the new function spec.
;;; 
;;; Rebuilt edebug-read-sexp to be simpler, faster, and more complete.
;;; 
;;; Accummulate frequencies of expression evaluation, displayable
;;; with edebug-display-freq-count.
;;; 
;;; No longer do save-restriction since edebug's eval-region doesnt narrow.
;;; 
;;; Numerous other display changes related to source code buffer's
;;; point and window-start.
;;; 
;;; Add -mode to the names of mode changing functions.
;;; 
;;; Set debugger to edebug-debug while inside edebug - it's almost
;;; always useful inside, and not useful outside of edebug.
;;; 
;;; Add edebug-trace function to output FMT with ARGS to *edebug-trace* buffer.
;;; 
;;; Other changes I've forgotten.
;;; 
;;;
;;;
;;; Revision 2.5  91/07/25  13:32:53  liberte
;;; Doc string cleanup.
;;; If edebug-form-spec is t, evaluate all arguments.
;;; If edebug-form-spec is 0, evaluate no arguments.
;;; If edebug-form-spec is nil, evaluate macro args according
;;; 	to edebug-eval-macro-args.
;;; Save the outside value of executing macro.
;;; Save and restore the outside restriction.
;;; Dont force update for go and Go-nonstop.
;;; Save and restore last-command-char, last-command,
;;; 	this-command, last-input-char.
;;; For epoch, do epoch::dispatch-events before sit-for
;;; 	and input-pending-p since X events could interfere.
;;; Warn about unsetting non-existent breakpoint.
;;; Fix edebug-forward-sexp with prefix arg.
;;; Add edebug-step-out to exit from current sexp.
;;; 
;;; Revision 2.4  91/03/18  12:35:44  liberte
;;; Force update after go or Go-nonstop modes, so overlay arrow is correct.
;;; Support debug-on-quit.  Remove edebug-on-error.
;;; Fix edebug-anonymous.  Bug found by jackr@wpd.sgi.com (Jack Repenning).
;;; Don't discard-input anymore.  Easier to change modes this way.
;;; Fix max-lisp-eval-depth and max-specpdl-size incrementing.
;;; Save and restore points in all buffers, if
;;;         edebug-save-buffer-points is non-nil.  Expensive!
;;;         Bug caught by wolfgang@wsrcc.com (Wolfgang S. Rupprecht)
;;; Save standard-output and standard-input in edebug-recursive-edit
;;;         so that edebug-outside-excursion can restore them.
;;; Call set-buffer in edebug-pop-to-buffer since
;;;         select-window does not do that.
;;; Fix edebug's eval-defun to remember current buffer inside evaluations
;;;         and to evaluate top-level forms.  Found by Jamie Zawinski.
;;; Add edebug-interactive-entry to support interactive forms with
;;;         non-string arg. Bug found by Jack Repenning.
;;; Simplify edebug-restore-match-data to just store-match-data.
;;;         Motivated by linus@lysator.liu.se.
;;; Move the match-data call to before the outside
;;;         buffer is changed, since it assumes that.
;;; 
;;; Revision 2.3  91/01/17  20:55:14  liberte
;;; Fix bug found by hollen@megatek.uucp.
;;; 	Current buffer was not being restored.
;;; Call edebug with (edebug begin end 'exp)
;;; 	and add additional wrapper around body of functions:
;;; 	(edebug-enter function body).
;;; Make &optional only apply to immediate next arg
;;; 	in edebug-interpret-form-spec (was edebug-macro-parser).
;;; Catch debug errors with edebug.  Yeah!
;;; Reset edebug-mode on first function entry.  Yeah!
;;; 	Motivated by Dion Hollenbeck.
;;; Add the missing bindings to the global-edebug-map.
;;; eval-current-buffer now uses eval-region.
;;; eval-region now does not narrow region.
;;; 	Narrowing was the cause of the window-start being set wrong.
;;; Reset edebug-mode only on
;;; 	first entry of any function at each recursive-edit level.
;;; Add edebug-backtrace, to generate cleaned up
;;; 	backtrace.  It doesnt "work" like the debug backtrace, however.
;;; Require reselecting outside window even if
;;; 	quit occurs, otherwise save-excursions may restore
;;; 	buffer to the wrong window.
;;; 
;;; Revision 2.2  90/11/26  21:14:22  liberte
;;; Shadow eval-defun and eval-region.  Toggle
;;; 	edebugging with edebug-all-defuns.
;;; Call edebug with (edebug 'function begin end 'exp)
;;; 	Suggested by Jamie Zawinski <jwz@lucid.com>.
;;; Add edebug-interpret-form-spec to process macro args.
;;; 	Motivated by Darryl Okahata darrylo@hpnmxx.hp.com.
;;; Fix by Roland McGrath <roland@ai.mit.edu>
;;; 	to wrap body of edebug-save-restriction in progn.
;;; Fix by Darryl Okahata <darrylo%hpnmd@hpcea.hp.com>
;;; 	to add (set-window-hscroll (selected-window) 0) to
;;; 	edebug-pop-to-buffer.
;;; 
;;; Revision 2.1  90/11/16  21:55:35  liberte
;;; Clean up.
;;; Add edebug-form-spec to edebug macro calls. Thanks to Joe Wells.
;;; edebug-forward-sexp uses step mode if no forward-sexp.
;;; 
;;; Revision 2.0  90/11/14  22:30:54  liberte
;;; Handle lambda forms, function, interactive evals, defmacro.
;;; Clean up display for Epoch - save and restore screen configurations.
;;;   Note: epoch 3.2 broke set-window-configuration.
;;;   Also, sit-for pauses do not always work in epoch.
;;; Display evaluations window.
;;; Display result after expression evaluation.
;;;   Thanks to discussions with Shinichirou Sugou.
;;; Conditional and temporary breakpoints.
;;; Change "continue" to "go" mode and add different "continue" mode.
;;; Option to stop before symbols.
;;;
;;; Fix by: Glen Ditchfield  gjditchfield@violet.uwaterloo.ca
;;; to handle ?# type chars.
;;;
;;; Revision 1.5  89/05/10  02:39:27  liberte
;;; Fix condition-case expression lists.
;;; Reorganize edebug.
;;; 
;;; Revision 1.4  89/02/14  22:58:34  liberte
;;; Fix broken breakpointing.
;;; Temporarily widen elisp buffer during edebug.
;;; 
;;; Revision 1.3  89/01/30  00:26:09  liberte
;;; More bug fixes for cond and let.
;;; Another parsing fix backquote.
;;; Fix for lambda forms inside defuns.
;;; Leave point at syntax error, mark at starting position.
;;; 
;;; Revision 1.2  88/11/28  12:14:15  liberte
;;; Bug fixes: cond construct didnt execute.
;;;   () in sexp list didnt parse
;;;   () as variable in condition-case didnt parse.
;;; 
;;; Revision 1.1  88/11/28  12:11:27  liberte
;;; Initial revision
;;; 


;;; Options
;;; -------

(defvar edebug-all-defuns nil
  "*If non-nil, all defuns and defmacros evaluated will use edebug.
eval-defun without prefix arg and eval-region will use edebug-defun.

If nil, eval-region evaluates normally, but eval-defun with prefix arg
uses edebug-defun.  eval-region is called by eval-defun, eval-last-sexp,
and eval-print-last-sexp.

You may wish to make this variable local to each elisp buffer by calling
(make-local-variable 'edebug-all-defuns) in your emacs-lisp-mode-hook.
You can use the function edebug-all-defuns to toggle its value.")


(defvar edebug-eval-macro-args nil
  "*If non-nil, edebug will assume that all macro call arguments for
macros that have no edebug-form-spec may be evaluated, otherwise it
assumes they will not be evaluated.  To specify exceptions for macros
that have some arguments evaluated and some not, you should specify an
edebug-form-spec")

(defvar edebug-stop-before-symbols nil
  "*Non-nil causes edebug to stop before symbols as well as after.
In any case, it is possible to stop before a symbol with a breakpoint or
interrupt.")

(defvar edebug-save-windows t
  "*If non-nil, save and restore window configuration on edebug calls.
It takes some time to save and restore, so if your program does not care
what happens to the window configurations, it is better to set this
variable to nil.")

;;(defvar edebug-save-point t
;;  "*If non-nil, save and restore the point and mark in source code buffers.")

(defvar edebug-save-displayed-buffer-points nil
  "*If non-nil, save and restore the points of all displayed buffers.

Saving and restoring buffer points is necessary if you are debugging
code that changes the point of a buffer which is displayed in a
non-selected window.  If edebug or the user then selects the
window, the buffer's point will be changed to the window's point.

Saving and restoring is an expensive operation since it visits each
window and each displayed buffer twice for each edebug call, so it is
best to avoid it if you can.")

(defvar edebug-initial-mode 'step
  "*Global initial mode for edebug, if non-nil.
This is used when edebug is first entered for each recursive-edit level.
Possible values are nil (meaning keep using edebug-mode), step, go,
Go-nonstop, trace, Trace-fast, continue, and Continue-fast.")

(defvar edebug-trace nil
  "*Non-nil if edebug should show a trace of function entry and exit.
Tracing output is displayed in a buffer named *edebug-trace*, one
function entry or exit per line, indented by the recursion level.  You
can customize by replacing functions edebug-print-trace-entry and
edebug-print-trace-exit.")



;;;========================================================================
;;; Utilities
;;; ---------

(defun edebug-which-function ()
  "Return the symbol of the function we are in."
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (down-list 1)
    (if (not (memq (read (current-buffer)) '(defun defmacro)))
	(error "Not in defun or defmacro."))
    (read (current-buffer))))

(defun edebug-last-sexp ()
  "Return the last sexp before point in current buffer.
Assumes elisp syntax is active."
  (car
   (read-from-string
    (buffer-substring
     (save-excursion
       (forward-sexp -1)
       (point))
     (point)))))

;; Not used.
(defun edebug-window-list ()
  "Return a list of windows, in order of next-window."
  ;; This doesnt work for epoch.
  (let* ((first-window (selected-window))
	 (window-list (list first-window))
	 (next (next-window first-window)))
    (while (not (eq next first-window))
      (setq window-list (cons next window-list))
      (setq next (next-window next)))
    (nreverse window-list)))

(defun edebug-get-emacs-displayed-buffer-points ()
  "Return a list of buffer point pairs, for all displayed buffers."
  (save-excursion
    (let* ((first-window (selected-window))
	   (next (next-window first-window))
	   (buffer-point-list nil)
	   buffer)
      (while (not (eq next first-window))
	(set-buffer (setq buffer (window-buffer next)))
	(setq buffer-point-list
	      (cons (cons buffer (point)) buffer-point-list))
	(setq next (next-window next)))
      buffer-point-list)))


(defun edebug-set-buffer-points (buffer-points)
  "Restore the buffer-points created by edebug-get-displayed-buffer-points."
  (let ((current-buffer (current-buffer)))
    (mapcar (function (lambda (buf-point)
			(if (buffer-name (car buf-point)) ; still exists
			    (progn
			      (set-buffer (car buf-point))
			      (goto-char (cdr buf-point))))))
	    buffer-points)
    (set-buffer current-buffer)))

;; Not used.
(defun edebug-two-window-p ()
  "Return t if there are two windows."
  (and (not (one-window-p))
       (eq (selected-window)
	   (next-window (next-window (selected-window))))))

(defun edebug-macrop (object)
  "Return the macro named by OBJECT, or nil if it is not a macro."
  (while (and (symbolp object) (fboundp object))
    (setq object (symbol-function object)))
  (if (and (listp object)
	   (eq 'macro (car object))
	   (edebug-functionp (cdr object)))
      object))

(defun edebug-functionp (object)
  "Returns the function named by OBJECT, or nil if it is not a function."
  (while (and (symbolp object) (fboundp object))
    (setq object (symbol-function object)))
  (if (or (subrp object)
	  (and (listp object)
	       (eq (car object) 'lambda)
	       (listp (car (cdr object)))))
      object))

(defun edebug-sort-alist (alist function)
  "Return the ALIST sorted with comparison function FUNCTION.
This uses 'sort so the sorting is destructive."
  (sort alist (function
	       (lambda (e1 e2)
		 (funcall function (car e1) (car e2))))))

(put 'edebug-save-restriction 'edebug-form-spec t)

;; Not used.
(defmacro edebug-save-restriction (&rest body)
  "Evaluate BODY while saving the current buffers restriction.
BODY may change buffer outside of current restriction, unlike
save-restriction.  BODY may change the current buffer,
and the restriction will be restored to the original buffer,
and the current buffer remains current.
Return the result of the last expression in BODY."
  (` (let ((edebug:s-r-beg (point-min-marker))
	   (edebug:s-r-end (point-max-marker)))
       (unwind-protect
	   (progn (,@ body))
	 (save-excursion
	   (set-buffer (marker-buffer edebug:s-r-beg))
	   (narrow-to-region edebug:s-r-beg edebug:s-r-end))))))


;;--------------------------
;; Epoch related things

;; We could use zones to highlight the current expression,
;; or to indicate frequency of use.

(defvar epoch::version nil
  "This will be predefined as non-nil in epoch.")


(defmacro def-edebug-func (edebug-func epoch-func emacs-func)
  "Define a function as either the epoch version or the emacs version."
  (` (fset (quote (, edebug-func))
	   (if epoch::version
		  (symbol-function (quote (, epoch-func)))
		(symbol-function (quote (, emacs-func)))))))


(defun edebug-get-epoch-displayed-buffer-points ()
  "Return a list of buffer point pairs, for all displayed buffers."
  (let ((buffer-point-list nil))
    (mapcar 
     (function 
      (lambda (screen)
	(select-screen screen)
	(save-excursion
	  (let* ((first-window (selected-window))
		 (next (next-window first-window)))
	    (while (not (eq next first-window))
	      (setq buffer-point-list
		    (cons (cons (window-buffer next) (point)) 
			  buffer-point-list))
	      (setq next (next-window next)))
	    buffer-point-list))))
     (epoch::screen-list 'unmapped))
    buffer-point-list
    ))

(def-edebug-func edebug-get-displayed-buffer-points
  edebug-get-epoch-displayed-buffer-points
  edebug-get-emacs-displayed-buffer-points)

(defun edebug-pop-to-buffer (buffer)
  "Like pop-to-buffer, but select a window that buffer was shown in.
If running epoch, use the same screen too."
  (let ((edebug-window (edebug-get-buffer-window buffer)))
    (if edebug-window 
	(select-window edebug-window)
      ;; It is not currently displayed, so find some place to display it.
      (if epoch::version
	  ;; Select a screen that the buffer has been displayed in before
	  ;; or the current screen otherwise.
	  (select-screen
	   ;; allowed-screens in epoch 3.2, was called screens before that
	   (or (car (symbol-buffer-value 'allowed-screens buffer))
	       (epoch::current-screen))))
      (if (one-window-p)
	  (split-window))
      (select-window (next-window))
      (set-window-buffer (selected-window) buffer)
      (set-window-hscroll (selected-window) 0)
      ))
  ;; Selecting the window does not set the buffer??  docs says it does.
  ;; But just returning the buffer is not equivalent.
  (set-buffer buffer)
  ;; buffer
  )


;; For epoch, we need to save and restore screens as well as windows.

(defun edebug-current-screen-configuration ()
  "Return an object recording the current configuration of Epoch screen-list.  
The object is a list of pairs of the form (SCREEN .  CONFIGURATION)
where SCREEN has window-configuration CONFIGURATION.  The current
screen is the head of the list."
  (let ((screen-list (epoch::screen-list 'unmapped))
	(current-screen (epoch::get-screen))
	(current-buffer (current-buffer))
	)
    ;; Put current screen first.
    (setq screen-list (cons current-screen (delq current-screen screen-list)))
    (prog1
	(mapcar (function
		 (lambda (screen)
		   (cons screen
			 (progn
			   (epoch::select-screen screen)
			   (current-window-configuration)))))
		screen-list)

      ;; Recover from the excursion.
      (epoch::select-screen current-screen)
      (set-buffer current-buffer)
      )))

"Return the current window or screen configuration."
(def-edebug-func edebug-current-window-configuration 
  edebug-current-screen-configuration
  current-window-configuration)


(defun edebug-set-screen-configuration (sc)
  "Set the window-configuration for all the screens in SC.
Set the current screen to be the head of SC."
    (mapcar (function
	     (lambda (screen-conf)
	       (if (epoch::screen-p (car screen-conf))  ; still exist?
		   (progn
		     (epoch::select-screen (car screen-conf))
		     (set-window-configuration (cdr screen-conf))))))
	    sc)
    (if (epoch::screen-p (car (car sc)))
	(epoch::select-screen (car (car sc))))
    )

"Set the window or screen configuration to CONF."
(def-edebug-func edebug-set-window-configuration 
  edebug-set-screen-configuration
  set-window-configuration)


(def-edebug-func edebug-get-buffer-window
  epoch::get-buffer-window
  get-buffer-window)


;; Input event handling in epoch requires we do a dispatch-events
;; in order to get the full effect of sit-for and input-pending-p.

(defun edebug-epoch-sit-for (arg)
  (epoch::dispatch-events)
  (sit-for arg))

(def-edebug-func edebug-sit-for edebug-epoch-sit-for sit-for)


(defun edebug-epoch-input-pending-p ()
  (epoch::dispatch-events)
  (input-pending-p))

(def-edebug-func edebug-input-pending-p
  edebug-epoch-input-pending-p
  input-pending-p)

;;=============================================================
;; Printing functions

(defun edebug-install-custom-print-funcs ()
  "Replace edebug print functions with custom versions.
Updating the custom print functions, or changing print-length,
print-level, print-circle, custom-print-list or custom-print-vector
have immediate effect."
  (interactive)
  (require 'custom-print)
  (fset 'edebug-prin1 'custom-prin1)
  (fset 'edebug-print 'custom-print)
  (fset 'edebug-prin1-to-string 'custom-prin1-to-string)
  (fset 'edebug-format 'custom-format))

  
(defun edebug-reset-print-funcs ()
  "Replace edebug custom print functions with internal versions."
  (interactive)
  (require 'custom-print)
  (fset 'edebug-prin1 (symbol-function 'CP::internal-prin1))
  (fset 'edebug-print (symbol-function 'CP::internal-print))
  (fset 'edebug-prin1-to-string
	(symbol-function 'CP::internal-prin1-to-string))
  (fset 'edebug-format (symbol-function 'CP::internal-format))
  )

;; Default print functions are the same as Emacs'.
(fset 'edebug-prin1 (symbol-function 'prin1))
(fset 'edebug-print (symbol-function 'print))
(fset 'edebug-prin1-to-string (symbol-function 'prin1-to-string))
(fset 'edebug-format (symbol-function 'format))


;;;=============================================================
;;; Redefine eval-defun, eval-region, and eval-current-buffer.
;;; -----------------------------------------------------------

(defun edebug-all-defuns ()
  "Toggle edebugging of all defuns and defmacros,
not including those evaluated in the minibuffer, or during load."
  (interactive)
  (setq edebug-all-defuns (not edebug-all-defuns))
  (message "Edebugging is %s." (if edebug-all-defuns "on" "off")))


(if (not (fboundp 'edebug-emacs-eval-defun))
    (fset 'edebug-emacs-eval-defun (symbol-function 'eval-defun)))
;; Use the following to undo the above.
;;(fset 'eval-defun (symbol-function 'edebug-emacs-eval-defun))

(defun eval-defun (edebug-debug-it)
  "Edebug replacement for eval-defun.  Print value in the minibuffer.
Evaluate the top-level form that point is in or before.  Note:
eval-defun normally evaluates any top-level form, not just defuns.  

Here are the differences from the standard eval-defun:  If the prefix
argument is the same as edebug-all-defuns (nil or non-nil), evaluate
normally; otherwise edebug-defun is called to wrap edebug calls around
evaluatable expressions in the defun or defmacro body.  Also, the
value printed by edebug-defun is not just the function name."
  (interactive "P")
  (let ((edebug-all-defuns
	 (not (eq (not edebug-debug-it) (not edebug-all-defuns)))))
    (edebug-emacs-eval-defun nil)
    ))


(if (not (fboundp 'edebug-emacs-eval-region))
    (fset 'edebug-emacs-eval-region (symbol-function 'eval-region)))
;; Use the following to undo the above.
;; (fset 'eval-region (symbol-function 'edebug-emacs-eval-region))

(defun eval-region (edebug-e-r-start edebug-e-r-end
				      &optional edebug-e-r-output)
  "Edebug replacement for eval-region.
Like eval-region, but call edebug-defun for defuns or defmacros.
Also, this eval-region does not narrow to the region and
if an error occurs, point is left at the error."
  ;; One other piddling difference concerns whitespace after the expression.
  (interactive "r")
  (let ((standard-output (or edebug-e-r-output 'symbolp))
	(edebug-e-r-pnt (point))
	(edebug-e-r-buf (current-buffer))
	(edebug-e-r-inside-buf (current-buffer))
	;; Mark the end because it may move.
	(edebug-e-r-end-marker (set-marker (make-marker) edebug-e-r-end))
	edebug-e-r-val
	)
    (goto-char edebug-e-r-start)
    (edebug-skip-whitespace)
    (while (< (point) edebug-e-r-end-marker)
      (if (and edebug-all-defuns
	       (eq 'lparen (edebug-next-token-class))
	       (save-excursion
		 (forward-char 1)	; skip \(
		 (memq (edebug-read-sexp) '(defun defmacro))))
	  (progn
	    (edebug-defun)
	    ;; Potential problem: edebug-defun always prints name.
	    (forward-sexp 1)		; skip the defun
	    )
	(if (and (eq 'lparen (edebug-next-token-class))
		 (save-excursion
		   (forward-char 1)	; skip \(
		   (memq (edebug-read-sexp) '(defun defmacro))))
	    ;; If it's a defun or defmacro, but not edebug-all-defuns
	    ;; reset the symbols edebug property to be just a marker at
	    ;; the definitions source code.
	    (put (edebug-which-function) 'edebug (point-marker)))

	;; Evaluate normally - after restoring the current-buffer.
	(setq edebug-e-r-val (edebug-read-sexp))
	(save-excursion
	  (set-buffer edebug-e-r-inside-buf)
	  (setq edebug-e-r-val (eval edebug-e-r-val))
	  ;; Remember current buffer for next time.
	  (setq edebug-e-r-inside-buf (current-buffer)))

	(if edebug-e-r-output
	    (progn
	      (setq values (cons edebug-e-r-val values))
	      (if (eq standard-output t)
		  (prin1 edebug-e-r-val)
		(princ "\n")
		(prin1 edebug-e-r-val)
		(princ "\n")
		)))
	)
      (goto-char
       (min (max edebug-e-r-end-marker (point))
	    (progn (edebug-skip-whitespace) (point))))
      )					; while
    (if (null edebug-e-r-output)
	;; do the save-excursion recovery
	(progn
	  ;; but mark is not restored
	  (set-buffer edebug-e-r-buf)
	  (goto-char edebug-e-r-pnt)))
    nil
    ))


;; The standard eval-current-buffer doesn't use eval-region.
(if (not (fboundp 'edebug-emacs-eval-current-buffer))
    (fset 'edebug-emacs-eval-current-buffer
	  (symbol-function 'eval-current-buffer)))
;; Use the following to undo the above.
;; (fset 'eval-current-buffer (symbol-function 'edebug-emacs-eval-current-buffer))

(defun eval-current-buffer (&optional edebug-e-c-b-output)
  "Call eval-region on the whole buffer."
  (interactive)
  (eval-region (point-min) (point-max) edebug-e-c-b-output))



;;;======================================================================
;;; The Parser
;;; ----------

;;; The top level function for parsing defuns is edebug-defun; it
;;; calls all the rest.  It checks the syntax a bit and leaves point
;;; at any error it finds, but otherwise should appear to work like
;;; eval-defun.

;;; The basic plan is to surround each expression with a call to
;;; the edebug debugger together with indexes into a table of positions of
;;; all expressions.  Thus an expression "exp" in function foo
;;; becomes:

;;; (edebug-after (edebug-before 1) exp)

;;; First point moved to to the beginning of exp (offset 1 of the
;;; current function).  Then the expression is evaluated and point is
;;; moved to offset 2 (calculated from offset 1), after the end of exp.

;;; The top level expressions of the function are wrapped in a call to
;;; edebug-enter, which supplies the function name and the actual
;;; arguments to the function.  See functions edebug-enter, edebug-before,
;;; and edebug-after for more details.


(defun edebug-defun ()
  "Evaluate defun or defmacro, like eval-defun, but with edebug calls.
Print its name in the minibuffer and leave point where it is,
or if an error occurs, leave point after it with mark at the original point."
  (interactive)
  (let ((starting-point (point))
	def-name)
  
    (end-of-defun)
    (beginning-of-defun)
    (condition-case err
	(let (def-kind def-name)
	  (down-list 1)
	  (setq 
	   def-kind  (edebug-read-sexp) ; defmacro or defun?
	   def-name
	   (eval
	    (if (not (memq def-kind '(defun defmacro)))
		(edebug-syntax-error "%s is not a defun or defmacro."
				     def-kind)
	      (cons def-kind (edebug-func-form)))))
	  (message "edebug: %s" def-name)

	  ;; Recover point, like save-excursion but only if no error occurs.
	  (goto-char starting-point)
	  def-name
	  )				; progn
      
      (invalid-read-syntax
       ;; Set mark at starting-point so user can return.
       ;; Leave point at error.
       (save-excursion  
	 (goto-char starting-point)
	 (set-mark-command nil))
       (message "Syntax error: %s" (cdr err))
       ;;       (signal 'invalid-read-syntax (cdr err))  ; pass it on, to who?
       )
      )					; condition-case
    ))


;; Dynamically bound vars, left unbound, but globally declared.
(defvar edebug-func-mark) ; the mark for function being evaluated.
(defvar edebug-offset-index) ; the next available offset index.
(defvar edebug-offset-list) ; the list of offset positions.
(defvar edebug-before-to-after-alist) ; conversion from before to after index
  
(defvar def-name) ; function or macro being defined, used by interactive-form

(defun edebug-func-form ()
  "Process the defun form starting before defun or defmacro."
  ;; This may now be used as a edebug-form-spec function.
  (let (
	def-name
	def-args
	def-docstring
	defun-interactive
	edebug-forms
	(edebug-offset-index 0)
	edebug-offset-list
	edebug-before-to-after-alist
	edebug-func-mark
	tmp-point
	(parse-sexp-ignore-comments t))
    
    (setq edebug-func-mark (point-marker))
    (setq def-name (edebug-read-sexp))
    (if (not (symbolp def-name))
	(edebug-syntax-error "Bad name: %s" def-name))
    (setq def-args (edebug-read-sexp))
    (if (not (listp def-args))
	(edebug-syntax-error "Bad arg list: %s" def-args))
	  
    ;; Look for doc string.
    (setq tmp-point (point))
    (if (eq 'string (edebug-next-token-class))
	(progn
	  (setq def-docstring (edebug-read-sexp))
	  (setq tmp-point (point))))
	  
    ;; Look for interactive form.
    (if (eq 'lparen (edebug-next-token-class))
	(progn
	  (forward-char 1)		; skip \(
	  (if (eq 'interactive (edebug-read-sexp))
	      (progn
		(setq defun-interactive
		      (cons 'interactive (edebug-interactive-form)))
		(forward-char 1)	; skip \)
		(setq tmp-point (point))
		))))
	  
    (goto-char tmp-point)

    ;; Process the rest of the body.
    (setq edebug-forms (edebug-forms))

    ;; Side effects on the property list of def-name.
    ;; Create and store the after-index vector.
    (put def-name 'edebug-after-index
	 (let ((after-vector
		(make-vector (length edebug-offset-list) -1)))
	   (mapcar (function
		    (lambda (pair)
		      (aset after-vector (car pair) (cdr pair))))
		   edebug-before-to-after-alist)
	   after-vector))
    
    ;; Create initial frequency count vector.
    (put def-name 'edebug-freq-count
	 (make-vector (length edebug-offset-list) 0))
				   
    ;; Store the offset list in function's property list.
    ;; destructively modifies edebug-offset-list
    (put def-name 'edebug
	 (list edebug-func-mark
	       nil			; clear breakpoints
	       (vconcat (nreverse edebug-offset-list))))

    ;; Build and return the new arguments.
    (list def-name def-args
	  def-docstring
	  defun-interactive
	  (` (edebug-enter
	      (quote (, def-name))
	      (list (,@ (delq '&rest (delq '&optional 
					   (copy-sequence def-args)))))
	      (function 
	       (lambda ()
		 ;; the remainder is a list of forms
		 (,@ edebug-forms))))
	     ))
    ))


;;(proclaim-inline 'edebug-forms)

(defun edebug-forms ()
  "Process a list of forms."
  (edebug-sexp-list t))

;;(proclaim-inline 'edebug-sexps)

(defun edebug-sexps ()
  "Process a list of sexps, unevaluated."
  (edebug-sexp-list nil))

(defun edebug-sexp-list (debuggable)
  "Return an edebug form built from the sexp list following point in the
current buffer. If DEBUGGABLE then wrap edebug calls around each sexp.
The sexp list does not start with a left paren; we are already in the list.
Leave point at (before) the trailing right paren."
  (let (sexp-list)
    (while (not (eq 'rparen (edebug-next-token-class)))
      (setq sexp-list (cons (if debuggable
				(edebug-form)
			      (edebug-read-sexp))
			    sexp-list)))
    (nreverse sexp-list)))


(defun edebug-increment-offset ()
  ;; modifies edebug-offset-index and edebug-offset-list
  ;; accesses edebug-func-marc and buffer point
  (setq edebug-offset-index (1+ edebug-offset-index))
  (setq edebug-offset-list (cons (- (point) edebug-func-mark)
				 edebug-offset-list)))


(defun edebug-make-edebug-form (index form)
  "Return the edebug form for the current function at offset INDEX given FORM.
Looks like: (edebug-after (edebug-before INDEX) FORM).
Also increment the offset index."
  (prog1
      (list 'edebug-after
	    (if (or (not (symbolp form))
		    edebug-stop-before-symbols)
		(list 'edebug-before index)
	      edebug-offset-index)
	    form)
    (setq edebug-before-to-after-alist 
	  (cons (cons index edebug-offset-index) edebug-before-to-after-alist))
    (edebug-increment-offset)
    ))


(defun edebug-form  ()
  "Return the debug form for the following form.  Add the point offset
to the edebug-offset-list for the function and move point to
immediately after the form."
  (let* ((index edebug-offset-index)
	 form class)
    ;; The point must be added to the offset list now
    ;; because edebug-list-form will add more offsets indirectly.
    (edebug-skip-whitespace)
    (edebug-increment-offset)
    (setq class (edebug-next-token-class))
    (cond
     ((eq 'lparen class)
      (edebug-make-edebug-form index (edebug-list-form)))

     ((eq 'symbol class)
      (if (and (not (memq (setq form (edebug-read-sexp)) '(nil t)))
	       ;; note: symbol includes numbers, see parsing utilities
	       (not (numberp form)))
	  (edebug-make-edebug-form index form)
	form))
     (t (edebug-read-sexp)))))


(defun edebug-list-form ()
  "Return an edebug form built from the list form that follows point.
Insert debug calls as appropriate to the form.  Start with point before
the left paren; leave point after the right paren."
  (let (class
	head)
    
    (forward-char 1)    ; skip \(
    (setq class (edebug-next-token-class))
    (cond
     ((eq 'symbol class) (setq head (edebug-read-sexp)))
     ((eq 'lparen class) (setq head (edebug-anonymous-form)))
     ((eq 'rparen class) (setq head nil))  ; () is legal

     (t (edebug-syntax-error
	 "Head of list must be a symbol or lambda expression.")))
    
    (prog1
	(if head
	    (cons head
		  (cond
		   ((symbolp head)
		    (let ((form (get head 'edebug-form-spec)))
		      (if form
			  (if (symbolp form)
			      (funcall form)

			    ;; Otherwise it is a spec.
			    (edebug-interpret-form-spec form))
			
			;; No edebug-form-spec provided.
			(if (edebug-macrop head)
			    (edebug-sexp-list edebug-eval-macro-args)
			  ;; Otherwise it is a function call.
			  (edebug-forms))
			)))

		   (t (edebug-forms))
		   )))
      
      ;; Is this needed?                 
      (if (eq 'rparen (edebug-next-token-class))
	  (forward-char 1) ; skip \)
	(edebug-syntax-error "Too many arguments."))
      )))


(defun edebug-interpret-form-spec (specs)
  "Parse the macro arguments that follow based on SPECS.  
SPECS describes the types of the arguments of a list form.  Each of the SPECS
is processed left to right, in the same order as the arguments of the
list form.  See the edebug documentation for more details.  The SPECS
may be one of the following:

 sexp - An unevaluated sexp (it may be an atom or list).
 form - An evaluated sexp.
 function - A function argument may be a quoted symbol or lambda
	expression (using quote or function) or a form (that evaluates
	to a function or lambda expression).

 foo -  Any other symbol should be the name of a function; this
	function is called on the argument as a predicate and an error
	is signaled if the predicate fails.  Standard predicates
 	include symbolp, integerp, stringp, vectorp, and atom.
 	An atom is an unevaluated number, string, symbol, or vector.

 &optional - All following specs in the list may or may not appear. 
 &rest - All following specs are repeated zero or more times.
	This is an extension of the normal meaning of &rest.
 &or -  Each of the following specs are alternatives, processed left to
	right until one succeeds.

 (...) - A sublist of the same format as the top level, processed recursively.
	Special case: if the car of the list is quote, the argument must match
	the quoted sexp (see example below of 'for macro).

 [...] - A sublist of the same format as the top level, processed recursively.
	It is processed like (...) except the matched arguments are inserted
	in-line into the arguments matched by the containing list.  This may be
	used for grouping to get one list element of the higher level list.
"
  ;; At the top level, we just catch any errors found and not caught below.
  (let* ((edebug-form)
	 (error
	  (catch 'no-match
	    (setq edebug-form (edebug-interpret-form-spec1 specs nil))
	    nil)))
    (if error
	(edebug-syntax-error error))
    edebug-form))


(defun edebug-interpret-form-spec1 (specs in-line)
  "Helper for edebug-interpret-form-spec.
If in-line is non-nil, don't reverse the resulting edebug form,
and don't complain about extra arguments."
  ;; Basic plan is to traverse the specs while matching with actual
  ;; arguments.  If a match fails, or if a spec is to be skipped,
  ;; no-match is thrown and caught within the loop.

  (let ((speclist specs)
	spec form form-list class was-in-line
	&optional &rest &or)

    ;; Process spec list until end of argument list.
    (while (and speclist
		(not (eq 'rparen (setq class (edebug-next-token-class)))))
      (setq spec (car speclist)
	    speclist (cdr speclist)
	    &or (and &or speclist)) ; Alternatives exist as long as specs do.
      
      ;; Process all the special specs.
      (while (memq spec '(&optional &rest &or))
	  ;; Check reasonableness of specs
	  (if (and (eq spec '&optional)
		   (or &or &rest))
	      (error "&optional doesn't make sense after %s"
		     (if &or '&or '&rest)))
	  (if (and (eq spec '&rest) &or)
	      (error "&rest doesn't make sense after &or"))

	  ;; Remember speclist at this point.
	  (set spec speclist)
	  (setq spec (car speclist)
		speclist (cdr speclist)
		&or (and &or speclist)))
      
      (let ((pnt (point)) ; Remember the last point we started matching at.
	    (error
	     (catch 'no-match

	       (cond
		((eq spec 'form)
		 (setq form (edebug-form)))

		((eq spec 'sexp)
		 (setq form (edebug-read-sexp)))

		((eq spec 'function)
		 ;; A function argument may be a quoted symbol or lambda form
		 ;; (with quote or function) or any evaluated form.
		 ;; This needs a hand-crafted function for speed.
		 (setq form 
		       (edebug-interpret-form-spec1 
			'(&or ('function
			       &or symbolp 
			       ('lambda (&rest symbolp) &rest form))
			      ('quote 
			       &or symbolp
			       ('lambda (&rest symbolp) &rest form))
			      form)
			'in-line))
		 (setq was-in-line t))
	  
		((listp spec)
		 (cond

		  ((eq 'quote (car spec))
		   ;; Special case, match the quoted symbol.
		   (setq spec (car (cdr spec)))
		   (if (not (and (eq class 'symbol)
				 (eq spec (setq form (edebug-read-sexp)))))
		       (throw 'no-match (format "\"%s\" expected." spec))
		     ))

		  ((eq class 'lparen)
		   (forward-char 1)	; skip \(
		   (setq form (edebug-interpret-form-spec1 spec nil))
		   (forward-char 1)	; skip \)
		   )
	    
		  ;; Handle weird special case for '(lambda ...).
		  ((and (eq class 'quote)
			(equal (car spec) '(quote quote)))
		   (forward-char 1)	; skip \'
		   (setq form 
			 (cons 'function 
			       (edebug-interpret-form-spec1 (cdr spec) t))))
	   
		  (t (throw 'no-match "List expected."))
		  ))

		((symbolp spec)
		 (let ((pred (if (fboundp spec) 
				 (symbol-function spec)
			       (error "%s is not a function." spec))))
		   (if (not (funcall pred (setq form (edebug-read-sexp))))
		       (throw 'no-match
			      (format "Predicate %s failed on %s." spec form))
		     )))

		((vectorp spec)
		 (setq form (edebug-interpret-form-spec1 (append spec nil) t))
		 (setq was-in-line t)	; only if successful
		 )

		(t (error "Bad item in edebug-form-spec: %s" spec))
		)			; cond

	       ;; The following are skipped by no-match throw.

	       ;; Add the form to the form list.
	       (if was-in-line
		   (setq form-list (append form form-list)
			 was-in-line nil)
		 (setq form-list (cons form form-list)))

	       ;; Since we matched, if we are in an &or, 
	       ;; then skip rest of the spec.
	       (if &or (setq speclist nil
			     &or nil))	; Means the &or was matched.

	       ;; If speclist is nil and &rest is not, reset speclist.
	       (if (and &rest (null speclist))
		   (setq speclist &rest))

	       nil
	       )			; catch no-match
	     ))

	(if error
	    (if (not (or &or &optional))
		(throw 'no-match error)	; leave point at error
	      (goto-char pnt)		; retry
	      )))
      ) ; while

    ;; Check for errors at end of list.
    ;; Are there specs left over?
    (if (and speclist (not (or &optional &rest
			       ;; &optional or &rest may be next
			       (memq (car speclist)
				     '(&optional &rest)))))
	(throw 'no-match "Not enough arguments."))

    (if in-line nil
      ;; Are there arguments left over?
      (if (not (eq 'rparen (setq class (edebug-next-token-class))))
	  (if (and &or (not (or &optional &rest)))
	      (throw 'no-match "Unrecognized argument.")
	    (throw 'no-match "Too many arguments.")))

      ;; Also reverse the elements of the form-list if not in a vector.
      (setq form-list (nreverse form-list))
      )
    form-list))


(defmacro def-edebug-form-spec (symbol form)
  "Set the edebug-form-spec property of SYMBOL according to FORM.  If
FORM is t, set it to edebug-forms.  If 0, set it to edebug-sexps.
Otherwise just use the evaluated FORM.  FORM can evaluate to a symbol,
which should be the name of a function, or a spec list for
edebug-interpret-form-spec."
  (` (put (quote (, symbol)) 'edebug-form-spec 
      (, (cond ((eq form t)
		'(quote edebug-forms))
	       ((eq form 0)
		'(quote edebug-sexps))
	       (t form))))))

(def-edebug-form-spec testit t)

(def-edebug-form-spec mapcar '(function form))
(def-edebug-form-spec mapconcat '(function form form))
(def-edebug-form-spec mapatoms '(function &optional form))
(def-edebug-form-spec apply '(function &rest form))
(def-edebug-form-spec funcall '(function &rest form))


;; for loop defined in elisp manual
(def-edebug-form-spec for '(symbolp 'from form 'to form 'do &rest form))

;; case and do defined in cl.el
(def-edebug-form-spec case '(form &rest (sexp form)))

(def-edebug-form-spec do
  '((&rest &or symbolp (symbolp &optional form form))
    (form &rest form)
    &rest body))

;; quote allows only one argument.
(def-edebug-form-spec quote 'edebug-read-sexp)

;; The defs.
(def-edebug-form-spec defvar '(symbolp &optional form stringp))

(def-edebug-form-spec defconst '(symbolp &optional form stringp))

;; This is not right because the top level edebug-enter form is missing.
(def-edebug-form-spec defun 
  '(symbolp (&rest symbolp)
	    &optional stringp
	    ('interactive &optional &or stringp form)
	    &rest form))
;; Use edebug-func-form instead.
(def-edebug-form-spec defun 'edebug-func-form)

(def-edebug-form-spec defmacro 'edebug-func-form)


;; function expects a symbol or a lambda expression
;; A macro is allowed by Emacs.
(def-edebug-form-spec function
  '(&or symbolp 
	([&optional 'macro] 'lambda (&rest symbolp) &rest form)))


(defun edebug-anonymous-form ()
  "Return the edebug form for an anonymous lambda or macro.
Point starts before the left paren and ends after it."
  (forward-char 1)	; skip \(
  (prog1
      (let ((head (edebug-read-sexp)))
	(cond 
	 ((eq head 'lambda)
	  (edebug-lambda-form))
	 ((eq head 'macro)
	  (if (not (eq 'lambda (edebug-read-sexp)))
	      (edebug-syntax-error "lambda expected."))
	  (cons 'macro (edebug-lambda-form)))
	 (t (edebug-syntax-error "Anonymous lambda or macro expected."))))
    (forward-char 1)	; skip \)
    ))


(defun edebug-lambda-form ()
  "Return the edebug form for the lambda form that follows.
Point starts after the lambda symbol and is moved to before the right paren."
  (append
   (list 'lambda (edebug-read-sexp)) ; the args
   (edebug-forms))) ; the body



(def-edebug-form-spec let
  '((&rest
    &or symbolp (symbolp &optional form))
   &rest form))

(def-edebug-form-spec let*
  '((&rest
     &or symbolp (symbolp &optional form))
    &rest form))

(def-edebug-form-spec let 'edebug-let-form)
(def-edebug-form-spec let* 'edebug-let-form)


(defun edebug-let-form ()
  "Return the edebug form of the let or let* form.
Leave point before the right paren."
  (let (var-value-list
	token
	class)
    (cons
     ;; first process the var/value list
     (if (not (eq 'lparen (edebug-next-token-class)))
	 (if (setq token (edebug-read-sexp))
	     (edebug-syntax-error "Bad var list in let.") ; should be nil
	   token  ; == nil
	   )
       
       (forward-char 1)			; lparen
       (while (not (eq 'rparen (setq class (edebug-next-token-class))))
	 (setq var-value-list
	       (cons
		(if (not (eq 'lparen class))
		    (edebug-read-sexp)
		  (forward-char 1)		; lparen
		  (prog1
		      (edebug-var-value)
		    (if (not (eq 'rparen (edebug-next-token-class)))
			(edebug-syntax-error "Right paren expected in let.")
		      (forward-char 1)		; rparen
		      )))
		var-value-list)))
       (forward-char 1)			; rparen
       (nreverse var-value-list))
     
     ;; now process the expression list
     (edebug-forms))))


(defun edebug-var-value ()
  "Return the edebug form of the var and optional value that follow point.  
Leave point after the value, if there is one."
  (list
   (edebug-read-sexp) ; the variable
   (if (not (eq 'rparen (edebug-next-token-class)))
       (edebug-form))))


(def-edebug-form-spec setq
  '(&rest symbolp form))

(def-edebug-form-spec setq-default
  '(&rest symbolp form))

(def-edebug-form-spec setq 'edebug-setq-form)
(def-edebug-form-spec setq-default 'edebug-setq-form)


(defun edebug-setq-form ()
  "Return the edebug form of the setq or setq-default var-value list."
  (let (var-value-list)
    (while (not (eq 'rparen (edebug-next-token-class)))
      (setq var-value-list
	    (append var-value-list
		    (edebug-var-value))))
    var-value-list))


(def-edebug-form-spec interactive
  '(&optional &or stringp form))
;; This should not be used, since the interactive form will not be seen.
(def-edebug-form-spec interactive 'edebug-interactive-form)

(defun edebug-interactive-form ()
  "Return the edebug form of the interactive form."
  (list
   (if (not (eq 'rparen (edebug-next-token-class)))
       (if (eq 'string (edebug-next-token-class))
	   (edebug-read-sexp)
	 (prog1
	     (` (edebug-enter
		 (quote (, def-name))  ; from edebug-defun
		 'nil
		 (function (lambda () (, (edebug-form))))))
	   (if (not (eq 'rparen (edebug-next-token-class)))
	       (edebug-syntax-error 
		"Only first expression used in interactive form.")))))))


(def-edebug-form-spec cond
  '(&rest (form &rest form)))
(def-edebug-form-spec cond 'edebug-cond-form)

(defun edebug-cond-form ()
  "Return the edebug form of the cond form."
  (let (value-value-list
	class)
    (while (not (eq 'rparen (setq class (edebug-next-token-class))))
      (setq value-value-list
	    (cons
	     (if (not (eq 'lparen class))
		 (let ((thing (edebug-read-sexp)))
		   (if thing
		       (edebug-syntax-error "Condition expected in cond")
		     nil))
	       (forward-char 1) ; \(
	       (prog1
		   (cons
		    (edebug-form)
		    (if (eq 'rparen (edebug-next-token-class))
			nil
		      (edebug-forms)))
		 (if (not (eq 'rparen (edebug-next-token-class)))
		     (edebug-syntax-error "Right paren expected in cond"))
		 (forward-char 1) ; \)
		 ))
	     value-value-list)))
    (nreverse value-value-list)))


(def-edebug-form-spec condition-case
  '(symbolp
    form
    &rest (symbolp &optional form)))
(def-edebug-form-spec condition-case 'edebug-condition-case-form)

(defun edebug-condition-case-form ()
  "Return the edebug form of the condition-case form."
  (cons
   (let (token)
     ;; read the variable or nil
     (setq token (edebug-read-sexp))
     (if (not (symbolp token))
	 (edebug-syntax-error
	  "Variable or nil required for condition-case; found: %s" token))
     token)
   
   (cons
    (edebug-form)			; the form
    
    ;; process handlers
    (let (symb-sexp-list
	  class)
      (while (not (eq 'rparen (setq class (edebug-next-token-class))))
	(setq symb-sexp-list
	      (cons
	       (if (not (eq 'lparen class))
		   (edebug-syntax-error "Bad handler in condition-case.")
		 (forward-char 1)	; \(
		 (prog1
		     (cons 
		      (edebug-read-sexp) ; the error-condition
		      (and (not (eq 'rparen (edebug-next-token-class)))
			   (edebug-forms)))
		   (forward-char 1)	; \)
		   ))
	       symb-sexp-list)))
      (nreverse symb-sexp-list)))))



;;------------------------------------------------
;; Parser utilities

(defconst edebug-syntax-table
  (let ((table (make-vector 256 'symbol))
	(i 0))
    (while (< i ?!)
      (aset table i 'space)
      (setq i (1+ i)))
    (aset table ?\( 'lparen)
    (aset table ?\) 'rparen)
    (aset table ?\' 'quote)
    (aset table ?\" 'string)
    (aset table ?\? 'char)
    (aset table ?\[ 'vector)
    (aset table ?\] 'vector-end)
    (aset table ?\. 'dot)
    ;; We treat numbers as symbols, because of confusion with -, -1, and 1-.
    ;; We dont care about any other chars since they wont be seen.
    table)
  "Lookup table for significant characters indicating the class of the
token that follows.  This is not a \"real\" syntax table.")

(defun edebug-next-token-class ()
  "Move to the next token and return its class.  We only care about
lparen, rparen, dot, quote, string, char, vector, or symbol."
  (edebug-skip-whitespace)
  (aref edebug-syntax-table (following-char)))


(defun edebug-syntax-error (msg &rest args)
  "Signal an invalid-read-syntax with MSG and ARGS.  
   This is caught by edebug-defun."
  (signal 'invalid-read-syntax (apply 'format msg args)))


(defun edebug-skip-whitespace ()
  "Leave point before the next token, skipping white space and comments."
  (skip-chars-forward " \t\r\n\f")
  (while (= (following-char) ?\;)
    (skip-chars-forward "^\n\r")  ; skip the comment
    (skip-chars-forward " \t\r\n\f")))


(defun edebug-read-sexp ()
  "Read one sexp from the current buffer starting at point.
Leave point immediately after it.  A sexp can be a list or atom.
An atom is a symbol (or number), character, string, or vector."
  ;; This works for reading anything legitimate, but it
  ;; is gummed up by parser inconsistencies (bugs?)
  (let ((class (edebug-next-token-class)))
    (cond
     ;; read goes one too far if a (possibly quoted) string or symbol
     ;; is immediately followed by non-whitespace.
     ((eq class 'symbol) (prog1
			     (read (current-buffer))
			   (if (not (eq (aref edebug-syntax-table 
					      (preceding-char)) 'symbol))
			       (forward-char -1))))
     ((eq class 'string) (prog1
			     (read (current-buffer))
			   (if (/= (preceding-char) ?\")
			       (forward-char -1))))
     ((eq class 'quote) (forward-char 1)
      (list 'quote (edebug-read-sexp)))
     (t 
      (read (current-buffer))))))




;;;=================================================================
;;; The debugger itself
;;; -------------------


(defvar edebug-active nil
  "Non-nil when edebug is active")


;;; add minor-mode-alist entry
(or (assq 'edebug-active minor-mode-alist)
    (setq minor-mode-alist (cons (list 'edebug-active " *Debugging*")
				 minor-mode-alist)))

(defvar edebug-stack nil
  "Stack of active functions evaluated via edebug.
Should be nil at the top level.")

(defvar edebug-stack-depth -1
  "Index of last edebug-stack item.")

(defvar edebug-offset-indices nil  ; not used yet.
  "Stack of offset indices of visited edebug sexps.
Should be nil at the top level.")

(defvar edebug-entered nil
  "Non-nil if edebug has already been entered at this recursive edit level.
This should stay nil at the top level.")

;;(defvar edebug-buffer-visited nil
;;  "Remember whether each buffer was visited since the last command
;;outside of edebug.  Should be nil at top level.")

;;(setq-default edebug-buffer-visited nil)
;;(make-variable-buffer-local 'edebug-buffer-visited)

;;	 (edebug-buffer-visited edebug-buffer-visited)


;; Dynamically bound variables, declared globally but left unbound.
(defvar edebug-function) ; the function being executed
(defvar edebug-args) ; the arguments of the function
(defvar edebug-data) ; the edebug data for the function
(defvar edebug-after-index-vector)
(defvar edebug-func-mark) ; the mark for the function
(defvar edebug-freq-count) ; the count of expression visits.
(defvar edebug-buffer) ; which buffer the function is in.
(defvar edebug-result) ; the result of the function call returned by body
(defvar edebug-outside-executing-macro)

(defvar edebug-mode 'step
  "Current edebug mode set by user.")

(defun edebug-enter
  (edebug-function edebug-args edebug-body)
  "Entering FUNC.  The arguments are ARGS, and the body is BODY.
Setup edebug variables and evaluate BODY.  This function is called
when a function evaluated with edebug-defun is entered.  Return the
result of BODY."

  ;; Is this the first time we are entering edebug since
  ;; lower-level recursive-edit command?
  (if (and (not edebug-entered)
	   edebug-initial-mode)
      ;; Reset edebug-mode to the initial mode.
      (setq edebug-mode edebug-initial-mode))

  (let* ((edebug-entered t)
	 (edebug-data (get edebug-function 'edebug))
	 (edebug-after-index-vector (get edebug-function 'edebug-after-index))
	 (edebug-func-mark (car edebug-data))	; mark at function start
	 (edebug-freq-count (get edebug-function 'edebug-freq-count))

	 (edebug-buffer (marker-buffer edebug-func-mark))
	 (edebug-stack (cons edebug-function edebug-stack))
	 (edebug-offset-indices edebug-offset-indices) ; protect from quit

	 (max-lisp-eval-depth (+ 5 max-lisp-eval-depth))  ; too much??
	 (max-specpdl-size (+ 15 max-specpdl-size)) ; the args and these vars

	 ;; Save the outside value of executing macro.
	 (edebug-outside-executing-macro executing-macro)
	 ;; Don't keep reading from an executing kbd macro within edebug!
	 (executing-macro nil)

	 (debugger 'edebug-debug)  ; only while edebug is active.
	 )
    (if edebug-trace
	(let ((edebug-stack-depth (1+ edebug-stack-depth))
	      edebug-result)
	  (edebug-print-trace-entry)
	  (setq edebug-result (funcall edebug-body))
	  (edebug-print-trace-exit)
	  edebug-result)

      (funcall edebug-body)
      )))


(defun edebug-print-trace-entry ()
  (edebug-trace-display
   "*edebug-trace*"
   "%s> %s args: %s\n" 
   (make-string edebug-stack-depth ?\-) 
   edebug-function edebug-args))

(defun edebug-print-trace-exit ()
  (edebug-trace-display
   "*edebug-trace*"
   "%s< %s result: %s\n" 
   (make-string edebug-stack-depth ?\-) 
   edebug-function edebug-result))


(defun edebug-display-freq-count ()
  "Display the frequency count in front of each line of the current function."
  (interactive)
  (let* ((edebug-function (edebug-which-function))
	 (edebug-freq-count (get edebug-function 'edebug-freq-count))
	 (edebug-after-index-vector (get edebug-function 'edebug-after-index))
	 (edebug-data (get edebug-function 'edebug))
	 (edebug-func-mark (car edebug-data))	; mark at function start
	 (edebug-points (car (cdr (cdr edebug-data))))
	 (len (length edebug-points))
	 (i len)
	 (last-point)
	 (last-count)
	 )
    (save-excursion
      (while (and (>= (setq i (1- i)) 0)
		  (= -1 (aref edebug-after-index-vector i))))
      (goto-char (+ edebug-func-mark (aref edebug-points i))))
      (beginning-of-line)
      (setq last-point (point))

      ;; Traverse in reverse order so offsets are correct.
      (while (>= (setq i (1- i)) 0)
	(if (< 0 (aref edebug-after-index-vector i))
	    (let ((count (aref edebug-freq-count i)))
	      (goto-char (+ edebug-func-mark (aref edebug-points i)))
	      (beginning-of-line)
	      (if (/= last-point (point))
		  (save-excursion
		    (goto-char last-point)
		    (insert (format "%3d:" last-count))))
	      (setq last-point (point)
		    last-count count))))
      (goto-char last-point)
      (insert (format "%3d:" last-count)))
    )


(defun edebug-before (edebug-before-index)
  "Debug current function given BEFORE position.
edebug-before is called from functions compiled with edebug-defun.  
Return the after index corresponding to this before index."
  (setq edebug-offset-indices
   (cons edebug-before-index edebug-offset-indices))

  ;; Increment frequency count 
  (aset edebug-freq-count edebug-before-index
	(1+ (aref edebug-freq-count edebug-before-index)))

  (if (not (and (eq edebug-mode 'Go-nonstop)
		(not (edebug-input-pending-p))))
      (edebug-debugger edebug-before-index 'enter nil))
  ;; Return the after-index
  (aref edebug-after-index-vector edebug-before-index))


(defun edebug-after (edebug-after-index edebug-value)
  "Debug current function given AFTER position and VALUE.
edebug is called from functions compiled with edebug-defun.
Return VALUE."
  ;; Pop the before index.
  (setq edebug-offset-indices (cdr edebug-offset-indices))
  (if (and (eq edebug-mode 'Go-nonstop)
	   (not (edebug-input-pending-p)))
      ;; Just return result.
      edebug-value
    (edebug-debugger edebug-after-index 'exit edebug-value)
    ))


;; Dynamically declared unbound variables.
(defvar edebug-arg-mode)  ; the mode, either enter, exit, or error
(defvar edebug-outside-debug-on-error) ; the value of debug-on-error outside
(defvar edebug-break) ; whether a break occurred.
(defvar edebug-exp) ; the result of the last expression.


(defun edebug-debugger (edebug-offset-index edebug-arg-mode edebug-exp)
  "Check breakpoints and pending input.
If edebug display should be updated, call edebug-display.
Return edebug-exp."
  (let* ((max-lisp-eval-depth (+ 1 max-lisp-eval-depth)) ; edebug-after
	 (max-specpdl-size (+ 10 max-specpdl-size)) ; the args and vars

	 ;; This needs to be here since breakpoints may be changed.
	 (edebug-breakpoints (car (cdr edebug-data))) ; list of breakpoints
	 (edebug-break-data (assq edebug-offset-index edebug-breakpoints))
	 (edebug-break
	  (if edebug-break-data
	      (let ((edebug-break-condition
		     (car (cdr edebug-break-data))))
		(or (not edebug-break-condition)
		    (eval edebug-break-condition)))))
	 )
    (if (and edebug-break
	     (car (cdr (cdr edebug-break-data)))) ; is it temporary?
	;; Delete the breakpoint.
	(setcdr edebug-data
		(cons (delq edebug-break-data edebug-breakpoints)
		      (cdr (cdr edebug-data)))))
      
    ;; Dont do anything if mode is go, continue, or Continue-fast
    ;; and no break, and no input.
    (if (or (not (memq edebug-mode '(go continue Continue-fast)))
	    (edebug-input-pending-p)
	    edebug-break)
	(edebug-display))   ; <--------------- display
    
    edebug-exp
    ))


(defvar edebug-window-start nil
  "Remember where each buffers' window starts between edebug calls.
This is to avoid spurious recentering.")

(setq-default edebug-window-start nil)
(make-variable-buffer-local 'edebug-window-start)


;; Dynamically declared unbound vars
(defvar edebug-point) ; the point in edebug buffer
(defvar edebug-outside-buffer) ; the current-buffer outside of edebug
(defvar edebug-outside-point) ; the point outside of edebug
(defvar edebug-outside-mark) ; the mark outside of edebug
(defvar edebug-outside-windows) ; outside window configuration
(defvar edebug-eval-buffer) ; for the evaluation list.
(defvar edebug-outside-o-a-p) ; outside overlay-arrow-position
(defvar edebug-outside-o-a-s) ; outside overlay-arrow-string
(defvar edebug-outside-c-i-e-a) ; outside cursor-in-echo-area

(defvar edebug-eval-list nil
  "List of expressions to evaluate.")

(defvar edebug-previous-result nil
  "Last result returned from an expression.")



(defun edebug-display ()
  "Setup windows for edebug, determine mode, maybe enter recursive-edit."
  ;; Uses local variables of edebug-enter, edebug, and edebug-debugger.
  (let ((edebug-active t)		; for minor mode alist
	edebug-stop			; should we enter recursive-edit
	(edebug-point (+ edebug-func-mark
			 (aref (car (cdr (cdr edebug-data)))
			       edebug-offset-index)))
	edebug-window			; window displaying edebug-buffer
	(edebug-outside-window (selected-window))
	(edebug-outside-buffer (current-buffer))
	(edebug-outside-point (point))
	(edebug-outside-mark (let ((zmacs-regions nil)) (mark)))
	edebug-outside-windows		; window or screen configuration
	;; edebug-outside-edebug-point	; current point in edebug buffer
	;; edebug-outside-edebug-mark      ; mark is too much trouble
	edebug-buffer-points
	
	edebug-eval-buffer		; declared here so we can kill it below
	(edebug-eval-result-list (and edebug-eval-list
				      (edebug-eval-result-list)))
	(edebug-outside-o-a-p overlay-arrow-position)
	(edebug-outside-o-a-s overlay-arrow-string)
	(edebug-outside-c-i-e-a cursor-in-echo-area)

	;;edebug-outside-point-min
	;;edebug-outside-point-max

	overlay-arrow-position
	overlay-arrow-string
	(cursor-in-echo-area nil)
	;; any others??
	)
    (if (not (buffer-name edebug-buffer))
	(let ((debug-on-error nil))
	  (error "Buffer defining %s not found." edebug-function)))
    
    ;; Save windows now before we modify them.
    (if edebug-save-windows
	(setq edebug-outside-windows (edebug-current-window-configuration)))
    
    (if edebug-save-displayed-buffer-points
	(setq edebug-buffer-points (edebug-get-displayed-buffer-points)))

    ;; First move the edebug buffer point to edebug-point
    ;; so that window start doesnt get changed when we display it.
    ;; I dont know if this is going to help.
    ;;(set-buffer edebug-buffer)
    ;;(goto-char edebug-point)

    ;; If edebug-buffer is not currently displayed,
    ;; first find a window for it.
    (edebug-pop-to-buffer edebug-buffer)
    (setq edebug-window (selected-window))

    ;; Now display eval list, if any.
    ;; This is done after the pop to edebug-buffer 
    ;; so that buffer-window correspondence is correct after quitting.
    (edebug-eval-display edebug-eval-result-list)
    ;; The evaluation list better not have deleted edebug-window.
    (select-window edebug-window)

    (goto-char edebug-point)
	    
    ;; If edebug-buffer has not been visited yet, set its edebug-window-start
    ;;      (if (not edebug-buffer-visited)
    ;;	  (setq edebug-buffer-visited t
    ;;		edebug-window-start (window-start)))
    (setq edebug-window-start
	  (edebug-adjust-window edebug-window-start))
	    
    ;; Test if there is input, not including keyboard macros.
    (if (edebug-input-pending-p)
	(progn
	  (setq edebug-mode 'step)
	  (setq edebug-stop t)
	  (edebug-stop)
	  ;;	    (discard-input)		; is this unfriendly??
	  ))
    ;; Now display arrow based on mode.
    (edebug-overlay-arrow)
	    
    (cond
     ((eq 'exit edebug-arg-mode)
      ;; Display result of previous evaluation.
      (setq edebug-previous-result edebug-exp)
      (edebug-previous-result))

     ((eq 'error edebug-arg-mode)
      ;; Display error message
      (beep)
      (if (eq 'quit (car edebug-exp))
	  (message "Quit")
	(message "%s: %s"
		 (get (car edebug-exp) 'error-message)
		 (car (cdr edebug-exp)))))
     
     (edebug-break
      (message "Break"))
     (t (message "")))
    
    (if edebug-break
	(if (not (memq edebug-mode '(continue Continue-fast)))
	    (setq edebug-stop t)
	  (if (eq edebug-mode 'continue)
	      (edebug-sit-for 1)
	    (edebug-sit-for 0)))
      ;; not edebug-break
      (if (eq edebug-mode 'trace)
	  (edebug-sit-for 1)		; Force update and pause.
	(if (eq edebug-mode 'Trace-fast)
	    (edebug-sit-for 0)		; Force update and continue.
	  )))
    
    (unwind-protect
	(if (or edebug-stop
		(eq edebug-mode 'step)
		(eq edebug-arg-mode 'error)) 
	    (progn
	      (setq edebug-mode 'step)
	      (edebug-overlay-arrow)	; this doesnt always show up.
	      (edebug-recursive-edit));;   <---------- Recursive edit
	  )

      ;; Reset the edebug-window to whatever it is now.
      (setq edebug-window (edebug-get-buffer-window edebug-buffer))
      ;; Remember the window start for the edebug buffer,
      ;; if it is still displayed.
      (if edebug-window
	  (progn
	    (set-buffer edebug-buffer) ; to set buffer-local var
	    (setq edebug-window-start (window-start edebug-window))))

      ;; Restore windows before continuing.
      (if edebug-save-windows
	  (progn
	    (edebug-set-window-configuration edebug-outside-windows)
	    ;; Unrestore edebug-buffer's window start, if displayed.
	    (setq edebug-window (edebug-get-buffer-window edebug-buffer))
	    (if edebug-window
		(save-excursion
		  (set-buffer edebug-buffer)
		  (set-window-start edebug-window 
				    edebug-window-start 'no-force))))
	;; Since we may be in a save-excursion, in case of quit,
	;; reselect the outside window only.
	;; Only needed if we are not recovering windows.
	(if (window-point edebug-outside-window)
	    (select-window edebug-outside-window))
	)  ; if edebug-save-windows

      ;; Restore displayed buffer points.
      ;; Is this needed if restoring windows??
      (if edebug-save-displayed-buffer-points
	  (edebug-set-buffer-points edebug-buffer-points))

      ;; Restore current buffer, point, and mark
      ;; Is this needed if restoring windows??
      (set-buffer edebug-outside-buffer)  ; must do this
      (goto-char edebug-outside-point)
      (let ((zmacs-regions nil))
	(if (marker-buffer (mark-marker))
	    (set-marker (mark-marker) edebug-outside-mark)))
      )					; unwind-protect
    ;; None of the following is done if quit or signal occurs.
    ;; ... nothing anymore.
   ))



(defvar edebug-depth 0
  "Number of recursive edits started by edebug.
Should be 0 at the top level.")

(defvar edebug-recursion-depth 0
  "Value of recursion-depth when edebug was called.")


;; Dynamically declared unbound vars
(defvar edebug-outside-match-data) ; match data outside of edebug
(defvar edebug-backtrace-buffer) ; each recursive edit gets its own
(defvar edebug-inside-windows) 

(defvar edebug-outside-map)
(defvar edebug-outside-standard-output)
(defvar edebug-outside-standard-input)
(defvar edebug-outside-last-command-char)
(defvar edebug-outside-last-command)
(defvar edebug-outside-this-command)
(defvar edebug-outside-last-input-char)

;;; for Lucid GNU Emacs
(defvar edebug-outside-last-command-event)
(defvar edebug-outside-unread-command-char)
(defvar edebug-outside-unread-command-event)
(defvar edebug-outside-last-input-event)

(defun edebug-recursive-edit ()
  "Start up a recursive edit inside of edebug."
  ;; The current buffer is the edebug-buffer, which is put into edebug-mode.
  ;; Assume that none of the variables below are buffer-local.
  (let ((edebug-buffer-read-only buffer-read-only)
	;; match-data must be done in the outside buffer
	(edebug-outside-match-data
	 (save-excursion  ; might be unnecessary now??
	   (set-buffer edebug-outside-buffer)  ; in case match buffer different
	   (match-data)))

	(edebug-depth (1+ edebug-depth))
	(edebug-recursion-depth (recursion-depth))
	edebug-entered			; bind locally to nil
	edebug-backtrace-buffer		; each recursive edit gets its own
	;; The window configuration may be saved and restored
	;; during a recursive-edit
	edebug-inside-windows

	(edebug-outside-map (current-local-map))
	(edebug-outside-standard-output standard-output)
	(edebug-outside-standard-input standard-input)

	(edebug-outside-last-command-char last-command-char)
	(edebug-outside-last-command last-command)
	(edebug-outside-this-command this-command)
	(edebug-outside-last-input-char last-input-char)
	;; the boundp checks are for Lucid GNU Emacs compatibility
	(edebug-outside-unread-command-char
	 (and (boundp 'unread-command-char) unread-command-char))
	(edebug-outside-last-input-event
	 (and (boundp 'last-input-event) last-input-event))
	(edebug-outside-last-command-event
	 (and (boundp 'last-command-event) last-command-event))
	(edebug-outside-unread-command-event
	 (and (boundp 'unread-command-event) unread-command-event))

	;; Declare the following local variables to protect global values.
	;; Make it local, but use global value.
	;; We could set these to the values for previous edebug call.
	(last-command-char last-command-char)
	(last-command last-command) 
	(this-command this-command)
	(last-input-char last-input-char)
	;; Assume no edebug command sets unread-command-char.
	(unread-command-char -1)
	(last-input-event nil)
	(last-command-event nil)
	(unread-command-event nil)
	(debug-on-error debug-on-error)
	
	;; others??
	)

    (and (fboundp 'zmacs-deactivate-region) (zmacs-deactivate-region))
    (if (and (eq edebug-mode 'go)
	     (not (memq edebug-arg-mode '(exit error))))
	(message "Break"))
    (edebug-mode)
    (if (boundp 'edebug-outside-debug-on-error)
	(setq debug-on-error edebug-outside-debug-on-error))

    (setq buffer-read-only t)
    (unwind-protect
	(recursive-edit)     ;  <<<<<<<<<< Recursive edit

      ;; Do the following, even if quit occurs.
      (if edebug-backtrace-buffer
	  (kill-buffer edebug-backtrace-buffer))
      ;; Could be an option to keep eval display up.
      (if edebug-eval-buffer (kill-buffer edebug-eval-buffer))

      ;; Remember selected-window after recursive-edit.
;;      (setq edebug-inside-window (selected-window))

      (store-match-data edebug-outside-match-data)

      ;; Recursive edit may have changed buffers,
      ;; so set it back before exiting let.
      (if (buffer-name edebug-buffer)	; if it still exists
	  (progn
	    (set-buffer edebug-buffer)
	    (if (memq edebug-mode '(go Go-nonstop))
		(edebug-overlay-arrow))
	    (setq buffer-read-only edebug-buffer-read-only)
	    (use-local-map edebug-outside-map)
	    )
	;; gotta have some other buffer
	(get-buffer-create " bogus edebug buffer"))
      )))


;;--------------------------
;; Display related functions

(defun edebug-adjust-window (old-start)
  "If pos is not visible, adjust current window to fit following context."
;;  (message "window-start: %s" (window-start)) (sit-for 1)
  (if old-start
      (set-window-start (selected-window) old-start))
  (if (not (pos-visible-in-window-p))
      (progn
	(set-window-start
	 (selected-window)
	 (save-excursion
	   (forward-line
	    (if (< (point) (window-start)) -1	; one line before if in back
	      (- (/ (window-height) 2)) ; center the line moving forward
	      ))
	   (beginning-of-line)
	   (point)))))
  (window-start))
  


(defconst edebug-arrow-alist
  '((Continue-fast . ">")
    (Trace-fast . ">")
    (continue . ">")
    (trace . "->")
    (step . "=>")
    (go . "<>")
    (Go-nonstop . "..")  ; not used
    )
  "Association list of arrows for each edebug mode.
If you come up with arrows that make more sense, let me know.")

(defun edebug-overlay-arrow ()
  "Set up the overlay arrow at beginning-of-line in current buffer.
The arrow string is derived from edebug-arrow-alist and edebug-mode."
  (let* ((pos))
    (save-excursion
      (beginning-of-line)
      (setq pos (point)))
    (setq overlay-arrow-string
	  (cdr (assq edebug-mode edebug-arrow-alist)))
    (setq overlay-arrow-position (make-marker))
    (set-marker overlay-arrow-position pos (current-buffer))))




(defun edebug-toggle-save-windows ()
  "Toggle the edebug-save-windows variable.
Each time you toggle it, the inside and outside window configurations
become the same as the current configuration."
  (interactive)
  (if (setq edebug-save-windows (not edebug-save-windows))
      (setq edebug-inside-windows
	    (setq edebug-outside-windows
		  (edebug-current-window-configuration))))
  (message "Window saving is %s."
	   (if edebug-save-windows "on" "off")))


(defun edebug-where ()
  "Show the debug windows and where we stopped in the program."
  (interactive)
  (if (not edebug-active)
      (error "edebug is not active."))
  (edebug-pop-to-buffer edebug-buffer)
  (goto-char edebug-point)  ; from edebug
  )

(defun edebug-view-outside ()
  "Change to the outside window configuration."
  (interactive)
  (if (not edebug-active)
      (error "edebug is not active."))
  (setq edebug-inside-windows (edebug-current-window-configuration))
  (edebug-set-window-configuration edebug-outside-windows)
  (goto-char edebug-outside-point)
  (message "Window configuration outside of edebug.  Return with %s"
	   (substitute-command-keys "\\<global-map>\\[edebug-where]")))


(defun edebug-bounce-point ()
  "Bounce the point in the outside current buffer."
  (interactive)
  (if (not edebug-active)
      (error "edebug is not active."))
  (save-excursion
    ;; If the buffer's currently displayed, avoid the set-window-configuration.
    (save-window-excursion
      (edebug-pop-to-buffer edebug-outside-buffer)
      (goto-char edebug-outside-point)
      (let ((zmacs-regions nil))
	(if (marker-buffer (mark-marker))
	    (message "Current buffer: %s Point: %s Mark: %s" 
		     (current-buffer) (point) (+ 0 (mark-marker)))
	  (message "Current buffer: %s point: %s" (current-buffer) (point))))
      (edebug-sit-for 1)
      (edebug-pop-to-buffer edebug-buffer))))





;;---------------------------------------------------
;; Breakpoint related functions

(defun edebug-find-stop-point ()
  "Return (function . index) of the nearest edebug stop point."
  (let* ((def-name (edebug-which-function))
	 (edebug-data
	  (or (get def-name 'edebug)
	      (error
	       "%s must first be evaluated with edebug-defun." def-name)))
	 ;; pull out parts of edebug-data.
	 (edebug-func-mark (car edebug-data))
	 (edebug-breakpoints (car (cdr edebug-data)))

	 (offset-vector (car (cdr (cdr edebug-data))))
	 (offset (- (save-excursion
		      (if (looking-at "[ \t]")
			  ;; skip backwards until non-whitespace, or bol
			  (skip-chars-backward " \t"))
		      (point))
		    edebug-func-mark))
	 len i)
    ;; the offsets are in order so we can do a linear search
    (setq len (length offset-vector))
    (setq i 0)
    (while (and (< i len) (> offset (aref offset-vector i)))
      (setq i (1+ i)))
    (if (and (< i len)
	     (<= offset (aref offset-vector i)))
	;; return the relevant info
	(cons def-name i)
      (message "Point is not on an expression in %s."
	       def-name)
      )))


(defun edebug-next-breakpoint ()
  "Move point to the next breakpoint, or first if none past point."
  (interactive)
  (let ((edebug-stop-point (edebug-find-stop-point)))
    (if edebug-stop-point
	(let* ((def-name (car edebug-stop-point))
	       (index (cdr edebug-stop-point))
	       (edebug-data (get def-name 'edebug))
	       
	       ;; pull out parts of edebug-data
	       (edebug-func-mark (car edebug-data))
	       (edebug-breakpoints (car (cdr edebug-data)))
	       (offset-vector (car (cdr (cdr edebug-data))))
	       breakpoint)
	  (if (not edebug-breakpoints)
	      (message "No breakpoints in this function.")
	    (let ((breaks edebug-breakpoints))
	      (while (and breaks
			  (<= (car (car breaks)) index))
		(setq breaks (cdr breaks)))
	      (setq breakpoint
		    (if breaks
			(car breaks)
		      ;; goto the first breakpoint
		      (car edebug-breakpoints)))
	      (goto-char (+ edebug-func-mark
			    (aref offset-vector (car breakpoint))))
	      
	      (message (concat (if (car (cdr (cdr breakpoint)))
				   "Temporary " "")
			       (if (car (cdr breakpoint))
				   (format "Condition: %s"
					   (edebug-prin1-to-string
					    (car (cdr breakpoint))))
				 "")))
	      ))))))


(defun edebug-modify-breakpoint (flag &optional condition temporary)
  "Modify the breakpoint for the form at point or after it according
to FLAG: set if t, clear if nil.  Then move to that point.
If CONDITION or TEMPORARY are non-nil, add those attributes to
the breakpoint.  "  
  (let ((edebug-stop-point (edebug-find-stop-point)))
    (if edebug-stop-point
	(let* ((def-name (car edebug-stop-point))
	       (index (cdr edebug-stop-point))
	       (edebug-data (get def-name 'edebug))
	       
	       ;; pull out parts of edebug-data
	       (edebug-func-mark (car edebug-data))
	       (edebug-breakpoints (car (cdr edebug-data)))
	       (offset-vector (car (cdr (cdr edebug-data))))
	       present)
	  ;; delete it either way
	  (setq present (assq index edebug-breakpoints))
	  (setq edebug-breakpoints (delq present edebug-breakpoints))
	  (if flag
	      (progn
		;; add it to the list and resort
		(setq edebug-breakpoints
		      (edebug-sort-alist
		       (cons
			(list index condition temporary)
			edebug-breakpoints) '<))
		(message "Breakpoint set in %s." def-name))
	    (if present
		(message "Breakpoint unset in %s." def-name)
	      (message "No breakpoint here.")))
	  
	  (setcdr edebug-data
		  (cons edebug-breakpoints (cdr (cdr edebug-data))))
	  (goto-char (+ edebug-func-mark (aref offset-vector index)))
	  ))))

(defun edebug-set-breakpoint (arg)
  "Set the breakpoint of nearest sexp.
With prefix argument, make it a temporary breakpoint."
  (interactive "P")
  (edebug-modify-breakpoint t nil arg))

(defun edebug-unset-breakpoint ()
  "Clear the breakpoint of nearest sexp."
  (interactive)
  (edebug-modify-breakpoint nil))

(defun edebug-set-conditional-breakpoint (arg condition)
  "Set a conditional breakpoint at nearest sexp.
The condition is evaluated in the outside context.
With prefix argument, make it a temporary breakpoint."
  (interactive "P\nxCondition: ")
  (edebug-modify-breakpoint t condition arg))


;;--------------------------
;; Mode switching functions

(defun edebug-set-mode (mode shortmsg msg)
  "Set the edebug mode to MODE.
Display SHORTMSG, or MSG if not within edebug."
  (interactive)
  (setq edebug-mode mode)
  (if (< 0 edebug-depth)
      (if (eq (current-buffer) edebug-buffer)
	  (progn
	    (message shortmsg)
	    (exit-recursive-edit)))
    (message msg)))


(defun edebug-step-through-mode ()
  "Proceed to next debug step."
  (interactive)
  (edebug-set-mode 'step "" "edebug will stop before next eval."))

(defun edebug-go-mode (arg)
  "Go, evaluating until break.
With ARG set temporary break at stop point and go."
  (interactive "P")
  (if arg
      (edebug-set-breakpoint t))
  (edebug-set-mode 'go "Go..." "edebug will go until break."))

(defun edebug-Go-nonstop-mode ()
  "Go, evaluating without debugging."
  (interactive)
  (edebug-set-mode 'Go-nonstop "Go-Nonstop..."
		   "edebug will not stop at breaks."))


(defun edebug-trace-mode ()
  "Begin trace mode."
  (interactive)
  (edebug-set-mode 'trace "Tracing..." "edebug will trace with pause."))

(defun edebug-Trace-fast-mode ()
  "Trace with no wait at each step."
  (interactive)
  (edebug-set-mode 'Trace-fast
		   "Trace fast..." "edebug will trace without pause."))

(defun edebug-continue-mode ()
  "Begin continue mode."
  (interactive)
  (edebug-set-mode 'continue "Continue..."
		   "edebug will pause at breakpoints."))

(defun edebug-Continue-fast-mode ()
  "Trace with no wait at each step."
  (interactive)
  (edebug-set-mode 'Continue-fast "Continue fast..."
		   "edebug will stop and go at breakpoints."))

;; ------------------------------------------------------------
;; The following use the mode changing commands and breakpoints.


(defun edebug-goto-here ()
  "Proceed to this stop point."
  (interactive)
  (edebug-go-mode t)
  )


(defun edebug-stop ()
  "Stop execution and do not continue.
Useful for exiting from trace loop."
  (interactive)
  (message "Stop"))


(defun edebug-forward-sexp (arg)
  "Proceed from the current point to the end of the ARGth sexp ahead.
If there are not ARG sexps ahead, then do edebug-step-out."
  (interactive "p")
  (condition-case err
      (let ((parse-sexp-ignore-comments t))
	;; Call forward-sexp repeatedly until done or failure.
	(forward-sexp arg)
	(edebug-go-mode t))
    (error
     (edebug-step-out)
     )))

(defun edebug-step-out ()
  "Proceed from the current point to the end of the containing sexp.
If there is no containing sexp that is not the top level defun,
go to the end of the last sexp, or if that is the same point, then step."
  (interactive)
  (condition-case err
      (let ((parse-sexp-ignore-comments t))
	(up-list 1)
	(save-excursion
	  ;; Is there still a containing expression?
	  (up-list 1))
	(edebug-go-mode t))
    (error
     ;; At top level - 1, so first check if there are more sexps at this level.
     (let ((start-point (point)))
;;       (up-list 1)
       (down-list -1)
       (if (= (point) start-point)
	   (edebug-step-through-mode)	; No more at this level, so step.
	 (edebug-go-mode t)
	 )))))

(defun edebug-step-in ()
  "Step into the function about to be called.
Do this before the arguments are evaluated since otherwise it will be
too late.  One side effect of using edebug-step-in is that the next
time the function is called, edebug will be called there as well."
  (interactive)
  (if (not (eq 'enter edebug-arg-mode))
      (error "You must be in front of a function or macro call."))
  (let (func func-marker)
    (save-excursion
      (edebug-where)
      (forward-char 1)
      (setq func (read (current-buffer)))
      )
    (setq func-marker (get func 'edebug))
    (cond
     ((markerp func-marker)
      (save-excursion
	(set-buffer (marker-buffer func-marker))
	(goto-char func-marker)
	(edebug-defun)))
     ((listp func-marker)
      ;; Its already been evaluated for edebug.
      nil)
     (t (error "First please eval the def of %s so edebug knows where it is." func))))
  (exit-recursive-edit))


;;(defun edebug-exit-out ()
;;  "Go until the current function exits."
;;  (interactive)
;;  (edebug-set-mode 'exiting "Exit..."))



;;--------------------------
;; Evaluation of expressions

(def-edebug-form-spec edebug-outside-excursion t)

(defmacro edebug-outside-excursion (&rest body)
  "Evaluate an expression list in the outside context.
Return the result of the last expression."
  (` (save-excursion			; of current-buffer
       (if edebug-save-windows
	   (progn
	     ;; After excursion, we will 
	     ;; restore to current window configuration.
	     (setq edebug-inside-windows
		   (edebug-current-window-configuration))
	     ;; Restore outside windows.
	     (edebug-set-window-configuration edebug-outside-windows)))

       (set-buffer edebug-buffer)  ; why?
       ;; (use-local-map edebug-outside-map)
       (store-match-data edebug-outside-match-data)
       ;; Restore outside context.
       (let ((edebug-inside-map (current-local-map))
	     (last-command-char edebug-outside-last-command-char)
	     (last-command-event edebug-outside-last-command-event)
	     (last-command edebug-outside-last-command)
	     (this-command edebug-outside-this-command)
	     (unread-command-char edebug-outside-unread-command-char)
	     (unread-command-event edebug-outside-unread-command-event)
	     (last-input-char edebug-outside-last-input-char)
	     (last-input-event edebug-outside-last-input-event)
	     (overlay-arrow-position edebug-outside-o-a-p)
	     (overlay-arrow-string edebug-outside-o-a-s)
	     (cursor-in-echo-area edebug-outside-c-i-e-a)
	     (standard-output edebug-outside-standard-output)
	     (standard-input edebug-outside-standard-input)
	     (executing-macro edebug-outside-executing-macro)
	     )
	 (unwind-protect
	     (save-excursion		; of edebug-buffer
	       (set-buffer edebug-outside-buffer)
	       (goto-char edebug-outside-point)
	       (let ((zmacs-regions nil))
		 (if (marker-buffer (mark-marker))
		     (set-marker (mark-marker) edebug-outside-mark)))
	       (,@ body))

	   ;; Back to edebug-buffer.  Restore rest of inside context.
	   ;; (use-local-map edebug-inside-map)
	   (if edebug-save-windows
	       ;; Restore inside windows.
	       (edebug-set-window-configuration edebug-inside-windows))
	   ))				; let
       )))


(defun edebug-previous-result ()
  "Return the previous result."
  (interactive)
  (let ((print-escape-newlines t)
	(print-length 50))
    (message "Result: %s" (edebug-prin1-to-string edebug-previous-result))))


(defun edebug-eval (expr)
  "Evaluate EXPR in the outside environment."
  (if (not edebug-active)
      (error "edebug is not active."))
  (edebug-outside-excursion
   (eval expr)))

(defun edebug-eval-expression (expr)
  "Prompt and evaluate an expression in the outside environment.  
Print result in minibuffer."
  (interactive "xEval: ")
  (edebug-prin1 (edebug-eval expr)))

(defun edebug-eval-last-sexp ()
  "Evaluate sexp before point in the outside environment;
print value in minibuffer."
  (interactive)
  (edebug-prin1 (edebug-eval (edebug-last-sexp))))

(defun edebug-eval-print-last-sexp ()
  "Evaluate sexp before point in the outside environment; 
print value into current buffer."
  (interactive)
  (let ((standard-output (current-buffer)))
    (edebug-print 
     (condition-case err
	 (edebug-eval (edebug-last-sexp))
       (error (edebug-format "%s: %s"
			     (get (car err) 'error-message)
			     (car (cdr err))))))))

;;;---------------------------------
;;; edebug minor mode initialization

(defvar edebug-mode-map nil)
(if edebug-mode-map
    nil
  (progn
    (setq edebug-mode-map (copy-keymap emacs-lisp-mode-map))
    ;; control
    (define-key edebug-mode-map " " 'edebug-step-through-mode)
    (define-key edebug-mode-map "g" 'edebug-go-mode)
    (define-key edebug-mode-map "G" 'edebug-Go-nonstop-mode)
    (define-key edebug-mode-map "t" 'edebug-trace-mode)
    (define-key edebug-mode-map "T" 'edebug-Trace-fast-mode)
    (define-key edebug-mode-map "c" 'edebug-continue-mode)
    (define-key edebug-mode-map "C" 'edebug-Continue-fast-mode)

    (define-key edebug-mode-map "f" 'edebug-forward-sexp)
    (define-key edebug-mode-map "h" 'edebug-goto-here)

    (define-key edebug-mode-map "r" 'edebug-previous-result)

    (define-key edebug-mode-map "i" 'edebug-step-in)
    (define-key edebug-mode-map "o" 'edebug-step-out)
    
    (define-key edebug-mode-map "q" 'top-level)
    (define-key edebug-mode-map "a" 'abort-recursive-edit)
    (define-key edebug-mode-map "S" 'edebug-stop)

    ;; breakpoints
    (define-key edebug-mode-map "b" 'edebug-set-breakpoint)
    (define-key edebug-mode-map "u" 'edebug-unset-breakpoint)
    (define-key edebug-mode-map "B" 'edebug-next-breakpoint)
    (define-key edebug-mode-map "x" 'edebug-set-conditional-breakpoint)
    
    ;; evaluation
    (define-key edebug-mode-map "e" 'edebug-eval-expression)
    (define-key edebug-mode-map "\C-x\C-e" 'edebug-eval-last-sexp)
    (define-key edebug-mode-map "E" 'edebug-visit-eval-list)
    
    ;; views
    (define-key edebug-mode-map "w" 'edebug-where)
    (define-key edebug-mode-map "v" 'edebug-view-outside)
    (define-key edebug-mode-map "p" 'edebug-bounce-point)
    (define-key edebug-mode-map "W" 'edebug-toggle-save-windows)
    
    ;; misc
    (define-key edebug-mode-map "?" 'edebug-help)
    (define-key edebug-mode-map "d" 'edebug-backtrace)
    
    (define-key edebug-mode-map "-" 'negative-argument)
    ))


(defvar global-edebug-prefix "\^XX"
  "Prefix key for global edebug commands, available from any buffer.")

(defvar global-edebug-map nil
  "Global map of edebug commands, available from any buffer.")

(if global-edebug-map
    nil
  (setq global-edebug-map (make-sparse-keymap))

  (global-unset-key global-edebug-prefix)
  (global-set-key global-edebug-prefix global-edebug-map)

  (define-key global-edebug-map " " 'edebug-step-through-mode)
  (define-key global-edebug-map "g" 'edebug-go-mode)
  (define-key global-edebug-map "G" 'edebug-Go-nonstop-mode)
  (define-key global-edebug-map "t" 'edebug-trace-mode)
  (define-key global-edebug-map "T" 'edebug-Trace-fast-mode)
  (define-key global-edebug-map "c" 'edebug-continue-mode)
  (define-key global-edebug-map "C" 'edebug-Continue-fast-mode)

  (define-key global-edebug-map "b" 'edebug-set-breakpoint)
  (define-key global-edebug-map "x" 'edebug-set-conditional-breakpoint)
  (define-key global-edebug-map "u" 'edebug-unset-breakpoint)
  (define-key global-edebug-map "w" 'edebug-where)
  (define-key global-edebug-map "q" 'top-level)
  )


(defun edebug-help ()
  (interactive)
  (describe-function 'edebug-mode))


(defun edebug-mode ()
  "Mode for Emacs Lisp buffers while in edebug.

There are both buffer local and global key bindings to several
functions.  E.g. edebug-step-through is bound to
\\[edebug-step-through] in the debug buffer and \\<global-map>\\[edebug-step-through] in any buffer.

Also see bindings for the eval list buffer, *edebug*.

The edebug buffer commands:
\\{edebug-mode-map}

Global commands prefixed by global-edbug-prefix:
\\{global-edebug-map}

Options:
edebug-all-defuns
edebug-eval-macro-args
edebug-stop-before-symbols
edebug-save-windows
edebug-save-displayed-buffer-points
edebug-initial-mode
edebug-trace
"
  (use-local-map edebug-mode-map))



;;===============================================
;; edebug eval list mode
;; A list of expressions and their evaluations is displayed in *edebug*.

;;(defvar edebug-eval-buffer "*edebug*"
;;  "*Declared globally so edebug-eval-display can be called independent
;;of edebug (not implemented yet).")


(defun edebug-eval-result-list ()
  "Return a list of evaluations of edebug-eval-list"
  ;; Assumes in outside environment.
  (mapcar (function
	   (lambda (expr)
	     (condition-case err
		 (eval expr)
	       (error (edebug-format "%s: %s"
				     (get (car err) 'error-message)
				     (car (cdr err))))
	       )))
	  edebug-eval-list))

(defun edebug-eval-display-list (edebug-eval-result-list)
  ;; Assumes edebug-eval-buffer exists.
  (let ((edebug-eval-list-temp edebug-eval-list)
	(standard-output edebug-eval-buffer)
	(edebug-display-line
	 (format ";%s\n" (make-string (- (window-width) 2) ?-))))
    (edebug-pop-to-buffer edebug-eval-buffer)
    (erase-buffer)
    (while edebug-eval-list-temp
      (prin1 (car edebug-eval-list-temp)) (terpri)
      (edebug-prin1 (car edebug-eval-result-list)) (terpri)
      (princ edebug-display-line)
      (setq edebug-eval-list-temp (cdr edebug-eval-list-temp))
      (setq edebug-eval-result-list (cdr edebug-eval-result-list)))
    ))

(defun edebug-create-eval-buffer ()
  (if (not (and edebug-eval-buffer (buffer-name edebug-eval-buffer)))
      (progn
	(set-buffer (setq edebug-eval-buffer (get-buffer-create "*edebug*")))
	(edebug-eval-mode))))

;; Should generalize this to be callable outside of edebug
;; with calls in user functions, e.g. (edebug-eval-display)

(defun edebug-eval-display (edebug-eval-result-list)
  "Display expressions and evaluations in EVAL-LIST.
It modifies the context by popping up the eval display."
  (if edebug-eval-result-list
      (progn
	(edebug-create-eval-buffer)
	(edebug-pop-to-buffer edebug-eval-buffer)
	(edebug-eval-display-list edebug-eval-result-list)
	)))

(defun edebug-eval-redisplay ()
  "Redisplay eval list in outside environment.
May only be called from within edebug-recursive-edit."
  (edebug-create-eval-buffer)
  (edebug-pop-to-buffer edebug-eval-buffer)
  (edebug-outside-excursion
   (edebug-eval-display-list (edebug-eval-result-list))
   ))

(defun edebug-visit-eval-list ()
  (interactive)
  (edebug-eval-redisplay)
  (edebug-pop-to-buffer edebug-eval-buffer))


(defun edebug-update-eval-list ()
  "Replace the evaluation list with the sexps now in the eval buffer."
  (interactive)
  (let ((starting-point (point))
	new-list)
    (goto-char (point-min))
    ;; get the first expression
    (edebug-skip-whitespace)
    (if (not (eobp))
	(progn
	  (forward-sexp 1)
	  (setq new-list (cons (edebug-last-sexp) new-list))))
    
    (while (re-search-forward "^;" nil t)
      (forward-line 1)
      (skip-chars-forward " \t\n\r")
      (if (and (/= ?\; (following-char))
	       (not (eobp)))
	  (progn
	    (forward-sexp 1)
	    (setq new-list (cons (edebug-last-sexp) new-list)))))
    
    (setq edebug-eval-list (nreverse new-list))
    (edebug-eval-redisplay)
    (goto-char starting-point)))


(defun edebug-delete-eval-item ()
  "Delete the item under point and redisplay."
  ;; could add arg to do repeatedly
  (interactive)
  (if (re-search-backward "^;" nil 'nofail)
      (forward-line 1))
  (delete-region
   (point) (progn (re-search-forward "^;" nil 'nofail)
		  (beginning-of-line)
		  (point)))
  (edebug-update-eval-list))



(defvar edebug-eval-mode-map nil
  "Keymap for edebug-eval-mode.  Superset of lisp-interaction-mode.")

(if edebug-eval-mode-map
    nil
  (setq edebug-eval-mode-map (copy-keymap lisp-interaction-mode-map))
  
  (define-key edebug-eval-mode-map "\C-c\C-w" 'edebug-where)
  (define-key edebug-eval-mode-map "\C-c\C-d" 'edebug-delete-eval-item)
  (define-key edebug-eval-mode-map "\C-c\C-u" 'edebug-update-eval-list)
  (define-key edebug-eval-mode-map "\C-x\C-e" 'edebug-eval-last-sexp)
  (define-key edebug-eval-mode-map "\C-j" 'edebug-eval-print-last-sexp)
  )


(defun edebug-eval-mode ()
  "Mode for data display buffer while in edebug.  Under construction.
... ignore the following...
There are both buffer local and global key bindings to several
functions.  E.g. edebug-step-through is bound to
\\[edebug-step-through] in the debug buffer and
\\<global-map>\\[edebug-step-through] in any buffer.

Eval list buffer commands:
\\{edebug-eval-mode-map}

Global commands prefixed by global-edbug-prefix:
\\{global-edebug-map}
"
  (lisp-interaction-mode)
  (setq major-mode 'edebug-eval-mode)
  (setq mode-name "Edebug-Eval")
  (use-local-map edebug-eval-mode-map))


;;========================================
;; Interface with standard debugger.

;; (setq debugger 'edebug-debug) ; to use the edebug debugger
;; (setq debugger 'debug)  ; use the standard debugger

;; Note that debug and its utilities must be byte-compiled to work, since
;; they depend on the backtrace looking a certain way.

(defun edebug-debug (&rest debugger-args)
  "Replacement for debug.  
If an error or quit occurred and we are running an edebugged function,
show where we last were.  Otherwise call debug normally."
  (if (and edebug-entered  ; anything active?
	   (eq (recursion-depth) edebug-recursion-depth)
	   )

      ;; Where were we before the error occurred?
      (let ((edebug-offset-index (car edebug-offset-indices))
	    (edebug-arg-mode (car debugger-args))
	    (edebug-exp (car (cdr debugger-args)))
	    edebug-break-data 
	    edebug-break
	    (edebug-outside-debug-on-eror debug-on-error)
	    (debug-on-error nil))
	(edebug-display)
	)

    ;; Otherwise call debug normally.
    ;; Still need to remove extraneous edebug calls from stack.
    (apply 'debug debugger-args)
    ))


(defun edebug-backtrace ()
  "Display a non-working backtrace.  Better than nothing..."
  (interactive)
  (let ((old-buf (current-buffer)))
    (if (not edebug-backtrace-buffer)
	(setq edebug-backtrace-buffer
	      (let ((default-major-mode 'fundamental-mode))
		(generate-new-buffer "*Backtrace*"))))
    (edebug-pop-to-buffer edebug-backtrace-buffer)
    (erase-buffer)
    (let ((standard-output (current-buffer))
	  (print-escape-newlines t)
	  (print-length 50)
	  last-ok-point
	  )
      (setq truncate-lines t)
      (backtrace)

      ;; Clean up the backtrace.  Not quite right for current edebug scheme.
      (goto-char (point-min))
      (delete-region
       (point)
       (progn
	 ;; Everything up to the first edebug is internal.
	 (re-search-forward "^  edebug")
	 (forward-line 1)
	 (point)))
      (forward-line 1)
      (setq last-ok-point (point))

      ;; Delete interspersed edebug internals.
      (while (re-search-forward "^  edebug" nil t)
	(if (looking-at "-enter")
	    ;; delete extraneous progn at top level of function body
	    (save-excursion
	      (goto-char last-ok-point)
	      (forward-line -1)
	      (setq last-ok-point (point))))
	(forward-line 1)
	(delete-region last-ok-point (point))
	(forward-line 1) ; skip past the good line
	(setq last-ok-point (point))
	)
      )
    (edebug-pop-to-buffer old-buf)
    ))


;;========================================================================
;; Trace display - append text to a buffer, and update display.
;;; e.g.
;;;	 (edebug-trace-display
;;;	  "*trace-point*"
;;;	  "saving: point = %s  window-start = %s\n"
;;;	  (point) (window-start))

(defun edebug-trace-display (buf-name fmt &rest args)
  "In buffer BUF-NAME, display FMT and ARGS at the end and make it visible.
The buffer is created if it does not exist.
You must include newlines in FMT to break lines."
  (let* ((selected-window (selected-window))
	 (buffer (get-buffer-create buf-name))
	 (buf-window))
    (edebug-pop-to-buffer buffer)
    (save-excursion
      (setq buf-window (selected-window))
      (set-buffer buffer)
      (goto-char (point-max))
      (insert (apply 'edebug-format fmt args))
      (set-window-point buf-window (point))
      (forward-line (- 1 (window-height buf-window)))
      (set-window-start buf-window (point))
;;      (edebug-sit-for 1)
      (bury-buffer buffer)
      )
    (select-window selected-window))
  buf-name)


(defun edebug-trace (fmt &rest args)
  "Convenience call to edebug-trace-display for buffer *edebug-trace*"
  (apply 'edebug-trace-display "*edebug-trace*" fmt args))
