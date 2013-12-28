;;;; edebug.el  A source level debugger for Emacs Lisp.
;; Copyright (C) 1988,'89,'90,'91,'92,'93, Free Software Foundation, Inc

;; LCD Archive Entry:
;; edebug|Daniel LaLiberte|liberte@cs.uiuc.edu
;; |A source level debugger for Emacs Lisp.
;; |!Date: 1993/09/21 21:06:30 !|!Revision: 3.2 !|~/modes/edebug.el|

;;; This minor mode allows programmers to step through elisp source
;;; code while executing functions.  You can also set breakpoints,
;;; trace (stopping at each expression), evaluate expressions as if
;;; outside edebug, reevaluate and display a list of expressions,
;;; catch errors normally caught by debug, and display a debug style
;;; backtrace.

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

;;;; Installation
;;; =============

;;; To install, put the .el files in some directory in your load-path and
;;; byte-compile them.  Put the following forms in your .emacs file.

;;; (define-key emacs-lisp-mode-map "\C-xx" 'edebug-eval-top-level-form)
;;; (autoload 'edebug-eval-top-level-form "edebug")

;;; If you wish to change the default edebug global command prefix...
;;; (setq edebug-global-prefix "...whatever you want")  ; default is C-xX

;;; If you wish to change the default edebug global command prefix, change:
;;; (setq edebug-global-prefix "\C-xX")

;;; Other options, are described in the manual.
;;; Also see edebug-emacs19.el, edebug-lemacs.el, cl-specs.el,
;;; and edebug-cl-read.el if they apply to you.

;;; In previous versions of edebug, users were directed to set
;;; `debugger' to `edebug-debug'.  This is no longer necessary
;;; since edebug automatically sets it whenever any code is being edebugged.

;;;; Minimal Instructions
;;; =====================

;;; First evaluate a defun with C-xx, then run the function.  Step through
;;; the code with SPC, mark breakpoints with b, go until a breakpoint is
;;; reached with g, and quit execution with q.  Use the "?" command in
;;; edebug to describe other commands.  See edebug.tex for more
;;; instructions.

;;; Send me your enhancements, ideas, bugs, or fixes.
;;; For bugs, you can call edebug-submit-bug-report if you have reporter.el.
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

;;; ===============================
;;; !Header: /import/kaplan/kaplan/liberte/Edebug/RCS/edebug.el,v 3.2 1993/09/21 21:06:30 liberte Exp liberte !
;;; !Log: edebug.el,v !
;;; Revision 3.2  1993/09/21  21:06:30  liberte
;;; * Don't define keywordp if already defined (by cl.el).
;;; * Clean up docs of edebug versions of eval-defun, eval-region, etc.
;;; * Add :name spec for specifying additional name components.
;;; * Replace "Not enough arguments" by what was expected.
;;; * Replace "Too many arguments" for a list spec to say what was expected.
;;; * Support &define again in middle of specs, (e.g. cl lambda expressions)
;;; * Fix "vector" specs to not be order dependent.
;;; * Simplify and correct spec of def-edebug-spec.
;;; * Require at least one arg after &optional in lambda-list.
;;; * Added edebug-cl-read.el to support cl read syntax, using cl-read.el.
;;; * Allow forms to start with \# and \` as well as \(, for cl-read.
;;; * Support #' for function quoting, used by lemacs.
;;; * Make GUD bindings for all emacs-lisp-mode buffers.
;;;
;;; Revision 3.1  1993/08/04  16:25:05  liberte
;;; * For compatability with older version of Edebug, I added
;;;   edebug-all-defuns and def-edebug-form-spec.  Dont use them.
;;;
;;; * Fixed bad argument in def-edebug-spec.
;;;
;;; * Only use edebug-print-* options if non-nil.
;;;
;;; * Fixed edebug-display-freq-count.
;;;
;;; Revision 3.0  1993/07/17  22:15:39  liberte
;;; * Added edebug-setup-hook called when edebug is used.
;;;
;;; * Added predicates: keywordp and lambda-list-keywordp.
;;;
;;; * Changed the name of custom-print.el to cust-print.el,
;;;         but Lisp variables and functions still use "custom-".
;;;
;;; * Changed names of replacement eval functions (eval-region, etc) to
;;;         add "edebug-" prefix.  Then replace the standard functions
;;;         in edebug-install-eval-functions called at end of file.
;;;
;;; * In edebug-eval-region, bind standard-output only while printing.
;;;
;;; * Change def-edebug-form to def-edebug-spec.
;;;
;;; * Replace the parser to first read the form with positions using
;;;         edebug-read, then parse its structure.
;;;
;;; * Parsing uses generalized "edebug-match-" functions for matching specs.
;;;
;;; * Generalize handling of special specs (e.g. &something) to implicitly
;;;         bracket all following specs.
;;;
;;; * Added new specs: arg, lambda-expr, place, fence, &key, and nil.
;;;
;;; * Changed arglist to lambda-list.
;;;
;;; * def-form macro does not assume arguments defined.
;;;
;;; * Added support for dotted forms (with dotted spec lists and nil),
;;;         vectors, and the new backquote that supports nested backquotes.
;;;
;;; * Added utilities edebug-unwrap and edebug-unwrap*
;;;
;;; * Support emacs 19 "lambda" macros.
;;;
;;; * Moved cl.el support to cl-specs.el.  Many fixes, thanks to Dave Gillespie.
;;;
;;; * Added specs for advice.el by Hans Chalupsky (hans@cs.buffalo.edu).
;;;
;;; * Changed edebug-step-through-mode to edebug-step-mode.
;;;
;;; * Make setting of the initial execution mode outside of edebug change
;;;         the mode once, rather than using edebug-initial-mode.
;;;
;;; * Fix tracing so breakpoints stop.
;;;
;;; * Check while edebugging whether source was changed.
;;;
;;; * Fix edebug-step-in.
;;;
;;; * Added: edebug-print-length, edebug-print-level, edebug-print-circle.
;;;
;;; * Do all edebug evaluations safely (in condition-case) and
;;;         if custom-print is being used, print safely.
;;;
;;; * Add bindings compatible with GUD standard.
;;;

;;; For the rest of the revision history, see edebug-history.

(provide 'edebug)

(require 'backquote)

;;;; Bug reporting
;;; ==============

(defconst edebug-version "3.1")
(defconst edebug-maintainer-address "liberte@cs.uiuc.edu")

(defun edebug-submit-bug-report ()
  "Submit via mail a bug report on edebug"
  (interactive)
  (require 'reporter)
  (and (y-or-n-p "Do you really want to submit a report on edebug? ")
       (reporter-submit-bug-report
         edebug-maintainer-address
         (concat "edebug.el " edebug-version)
         (list 'edebug-setup-hook
               'edebug-all-defs
               'edebug-all-forms
               'edebug-eval-macro-args
               'edebug-stop-before-symbols
               'edebug-save-windows
               'edebug-save-displayed-buffer-points
               'edebug-initial-mode
               'edebug-trace
               'edebug-test-coverage
               'edebug-continue-kbd-macro
               'edebug-print-length
               'edebug-print-level
               'edebug-print-circle
	       ))))


;;;; Options
;;; ===============================

(defvar edebug-setup-hook nil
  "*Functions to call before edebug is used.
Its value is reset to nil after being used, so each time it is set
to a new function, that function will be called once and only once.")

(defvar edebug-all-defs nil
  "*If non-nil, evaluation of any defining forms will use edebug.
eval-defun without prefix arg and eval-region will use
edebug-eval-top-level-form.

If nil, eval-region evaluates normally, but eval-defun with prefix arg
uses edebug-eval-top-level-form.  eval-region is called by eval-defun,
eval-last-sexp, and eval-print-last-sexp.

You can use the command edebug-all-defs to toggle the value of this
variable.  You may wish to make this variable local to each
buffer by calling (make-local-variable 'edebug-all-defs) in your
emacs-lisp-mode-hook.")

(defvar edebug-all-forms nil
  "*Non-nil means edebug the evaluation of all forms, including top level.  
Use the command edebug-all-forms to toggle the value of this option.")

(defvar edebug-eval-macro-args nil
  "*Non-nil means all macro call arguments may be evaluated.  
If this variable is nil, the default, edebug will *not* wrap
macro call arguments as if they will be evaluated.  
For each macro, a edebug-form-spec overrides this option.
So to specify exceptions for macros that have some arguments evaluated
and some not, you should specify an edebug-form-spec")

(defvar edebug-stop-before-symbols nil
  "*Non-nil causes edebug to stop before symbols as well as after.
In any case, it is possible to stop before a symbol with a breakpoint or
interrupt.")

(defvar edebug-save-windows t
  "*If non-nil, save and restore window configuration on edebug calls.
It takes some time to save and restore, so if your program does not care
what happens to the window configurations, it is better to set this
variable to nil.

For epoch, this option also controls preserving of screen configurations.")

;;(defvar edebug-save-point t
;;  "*If non-nil, save and restore the point and mark in source code buffers.")

(defvar edebug-save-displayed-buffer-points nil
  "*If non-nil, save and restore the points of all displayed buffers.

Saving and restoring buffer points is necessary if you are debugging
code that changes the point of a buffer which is displayed in a
non-selected window.  If edebug or the user then selects the
window, the buffer's point will be changed to the window's point.

But this preservation is an expensive operation since it visits each
window and its displayed buffer twice for each edebug call, so it is
best to avoid it if you can.")

(defvar edebug-initial-mode 'step
  "*Initial execution mode for edebug, if non-nil.  
This is used when edebug is first entered for each recursive-edit
level.  Possible values are nil (which means leave
edebug-execution-mode as is), step, (the default), next, go,
Go-nonstop, trace, Trace-fast, continue, and Continue-fast.")

(defvar edebug-trace nil
  "*Non-nil if edebug should show a trace of function entry and exit.
Tracing output is displayed in a buffer named by the variable
edebug-trace-buffer, one function entry or exit per line, indented by
the stack depth.  You can customize by replacing functions
edebug-print-trace-before and edebug-print-trace-after.")

(defconst edebug-trace-buffer "*edebug-trace*"
  "Name of the buffer to put trace info in.")

(defvar edebug-test-coverage nil
  "*If non-nil, Edebug tests coverage of all expressions debugged.
This is done by comparing the result of each expression
with the previous result. Coverage is considered OK if two different
results are found.  So to sufficiently test the coverage of your code,
try to execute it under conditions that evaluate all expressions more
than once, and produce different results for each expression.

Use `edebug-display-freq-count' to display the frequency count and
coverage information for a definition.")

(defvar edebug-continue-kbd-macro nil
  "*If non-nil, continue executing any keyboard macro that is executing outside.")

(defvar edebug-print-length 50
  "*Default value of print-length to use while printing results in edebug.")
(defvar edebug-print-level 50
  "*Default value of print-level to use while printing results in edebug.")
(defvar edebug-print-circle t
  "*Default value of print-circle to use while printing results in edebug.")


;;;; Utilities
;;; ===============================

(if (not (fboundp 'gensym))
    (progn

;; Define gensym - from old cl.el
(defvar *gensym-index* 0
  "Integer used by gensym to produce new names.")

(defun gensym (&optional prefix)
  "Generate a fresh uninterned symbol.
There is an  optional argument, PREFIX.  PREFIX is the
string that begins the new name. Most people take just the default,
except when debugging needs suggest otherwise."
  (if (null prefix)
      (setq prefix "G"))
  (let ((newsymbol nil)
        (newname   ""))
    (while (not newsymbol)
      (setq newname (concat prefix *gensym-index*))
      (setq *gensym-index* (+ *gensym-index* 1))
      (if (not (intern-soft newname))
          (setq newsymbol (make-symbol newname))))
    newsymbol))
))

(if (not (fboundp 'keywordp))
    (defun keywordp (object)
      "Return t if OBJECT is a keyword.
A keyword is a symbol that starts with "":""."
      (and (symbolp object)
	   (= ?: (aref (symbol-name object) 0)))))

(defun lambda-list-keywordp (object)
  "Return t if OBJECT is a lambda list keyword.
A lambda list keyword is a symbol that starts with ""&""."
  (and (symbolp object)
       (= ?& (aref (symbol-name object) 0))))

;; All use of this has been replaced by `edebug-form-data-symbol'
'(defun edebug-which-function ()
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
'(defun edebug-window-list ()
  "Return a list of windows, in order of next-window."
  ;; This doesnt work for epoch.
  (let* ((first-window (selected-window))
	 (window-list (list first-window))
	 (next (next-window first-window)))
    (while (not (eq next first-window))
      (setq window-list (cons next window-list))
      (setq next (next-window next)))
    (nreverse window-list)))

;; Not used.
'(defun edebug-two-window-p ()
  "Return t if there are two windows."
  (and (not (one-window-p))
       (eq (selected-window)
	   (next-window (next-window (selected-window))))))

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

;;(def-edebug-spec edebug-save-restriction t)

;; Not used.  If it is used, def-edebug-spec must be defined before use.
'(defmacro edebug-save-restriction (&rest body)
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

;;;; Epoch related things
;;; ===============================
;; We could use zones to highlight the current expression,
;; or to indicate frequency of use.

(defvar epoch::version) ; this will be predefined as non-nil in epoch.
(defconst edebug-epoch (and (boundp 'epoch::version) epoch::version))

(defun set-edebug-func (edebug-func epoch-func emacs-func)
  "Define a function as either the epoch version or the emacs version."
  (fset edebug-func
	(if edebug-epoch
	    (symbol-function epoch-func)
	  (symbol-function emacs-func))))


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

(set-edebug-func 'edebug-get-displayed-buffer-points
  'edebug-get-epoch-displayed-buffer-points
  'edebug-get-emacs-displayed-buffer-points)

(defun edebug-pop-to-buffer (buffer)
  "Like pop-to-buffer, but select a window that buffer was shown in.
If running epoch, use the same screen too."
  (let ((edebug-window (edebug-get-buffer-window buffer)))
    (if edebug-window 
	(select-window edebug-window)
      ;; It is not currently displayed, so find some place to display it.
      (if edebug-epoch
	  ;; Select a screen that the buffer has been displayed in before
	  ;; or the current screen otherwise.
	  (select-screen
	   ;; allowed-screens in epoch 3.2, was called screens before that
	   (or (car (symbol-buffer-value 'allowed-screens buffer))
	       (epoch::current-screen))))
      (if (one-window-p)
	  (split-window))
;;      (message "next window: %s" (next-window)) (sit-for 1)
      (if (eq (get-buffer-window edebug-trace-buffer) (next-window))
	  ;; Dont select trace window
	  nil
	(select-window (next-window)))
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

;; Return the current window or screen configuration.
(set-edebug-func 'edebug-current-window-configuration 
  'edebug-current-screen-configuration
  'current-window-configuration)


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

;; Set the window or screen configuration to CONF.
(set-edebug-func 'edebug-set-window-configuration 
  'edebug-set-screen-configuration
  'set-window-configuration)


(set-edebug-func 'edebug-get-buffer-window
  'epoch::get-buffer-window
  'get-buffer-window)


;; Input event handling in epoch requires we do a dispatch-events
;; in order to get the full effect of sit-for and input-pending-p.

(defun edebug-epoch-sit-for (arg)
  (epoch::dispatch-events)
  (sit-for arg))

(set-edebug-func 'edebug-sit-for 'edebug-epoch-sit-for 'sit-for)


(defun edebug-epoch-input-pending-p ()
  (epoch::dispatch-events)
  (input-pending-p))

(set-edebug-func 'edebug-input-pending-p
  'edebug-epoch-input-pending-p
  'input-pending-p)

;;;; Printing
;;;; ===============================
;;; Useful for printing circular or special structures.
;;; This requires cust-print, but only if you call these functions.

(defun edebug-install-custom-print-funcs ()
  "Replace edebug print functions with custom versions.
Modifying the custom print functions, or changing print-length,
print-level, print-circle, custom-print-list or custom-print-vector
have immediate effect."
  (interactive)
  (require 'cust-print)
  (fset 'edebug-prin1 'custom-prin1)
  (fset 'edebug-print 'custom-print)
  (fset 'edebug-prin1-to-string 'custom-prin1-to-string)
  (fset 'edebug-format 'custom-format)
  (fset 'edebug-message 'custom-message)
  )

  
(fset 'edebug-reset-print-funcs 'edebug-uninstall-custom-print-funcs)

(defun edebug-uninstall-custom-print-funcs ()
  "Replace edebug custom print functions with internal versions."
  (interactive)
  (require 'cust-print)
  (fset 'edebug-prin1 'CP::internal-prin1)
  (fset 'edebug-print 'CP::internal-print)
  (fset 'edebug-prin1-to-string 'CP::internal-prin1-to-string)
  (fset 'edebug-format 'CP::internal-format)
  (fset 'edebug-message 'CP::internal-message)
  )

;; Default print functions are the same as Emacs'.
(fset 'edebug-prin1 'prin1)
(fset 'edebug-print 'print)
(fset 'edebug-prin1-to-string 'prin1-to-string)
(fset 'edebug-format 'format)
(fset 'edebug-message 'message)


;;;; Redefine eval functions
;;; ===============================
;;; eval-defun, eval-region, and eval-current-buffer are redefined
;;; in order to maybe wrap code in edebug calls.
;;; These changes are installed at end of file.
;;; Once this is done, by loading the file, it can be undone doing:
;;; (edebug-uninstall-eval-functions)

  ;; Save standard versions.
(if (not (fboundp 'edebug-original-eval-defun))
    (fset 'edebug-original-eval-defun (symbol-function 'eval-defun)))
(if (not (fboundp 'edebug-original-eval-region))
    (fset 'edebug-original-eval-region (symbol-function 'eval-region)))
(if (not (fboundp 'edebug-original-eval-buffer))
    (fset 'edebug-original-eval-buffer 
	  (if (fboundp 'eval-buffer)  ;; only in Emacs 19
	      (symbol-function 'eval-buffer)
	    'eval-buffer)))
(if (not (fboundp 'edebug-original-eval-current-buffer))
    (fset 'edebug-original-eval-current-buffer
	  (symbol-function 'eval-current-buffer)))

(defun edebug-install-eval-functions ()
  ;; Store edebug versions
  (interactive)
  (fset 'eval-defun 'edebug-eval-defun)
  (fset 'eval-region 'edebug-eval-region)
  (fset 'eval-buffer 'edebug-eval-buffer)
  (fset 'eval-current-buffer 'edebug-eval-current-buffer)
  )

(defun edebug-uninstall-eval-functions ()
  (interactive)
  (fset 'eval-defun (symbol-function 'edebug-original-eval-defun))
  (fset 'eval-region (symbol-function 'edebug-original-eval-region))
  (fset 'eval-buffer 
	(symbol-function 'edebug-original-eval-buffer))
  (fset 'eval-current-buffer 
	(symbol-function 'edebug-original-eval-current-buffer))
  )


(fset 'edebug-all-defuns 'edebug-all-defs)  ;; Compatibility with old versions.
(defun edebug-all-defs ()
  "Toggle edebugging of all definitions,
not including those evaluated in the minibuffer, or during load."
  (interactive)
  (setq edebug-all-defs (not edebug-all-defs))
  (message "Edebugging all definitions is %s." 
	   (if edebug-all-defs "on" "off")))


(defun edebug-all-forms ()
  "Toggle edebugging of all forms,
not including those evaluated in the minibuffer, or during load."
  (interactive)
  (setq edebug-all-forms (not edebug-all-forms))
  (message "Edebugging all forms is %s." 
	   (if edebug-all-forms "on" "off")))


(defun edebug-eval-defun (edebug-it)
  "Evaluate the top-level form that point is in or before.
Print value in minibuffer.

This version, from edebug, has the following differences: If the
prefix argument is the same as edebug-all-defs (nil or non-nil),
evaluate normally; otherwise code is instrumented.  Also, the value
printed is \"edebug: <function name>\"."
  (interactive "P")
  (let ((edebug-all-defs
	 (not (eq (not edebug-it) (not edebug-all-defs)))))
    (edebug-original-eval-defun nil)
    ))


(defun edebug-eval-region (edebug-e-r-start edebug-e-r-end
				      &optional edebug-e-r-output)
  "Execute the region as Lisp code.
When called from programs, expects two arguments,
giving starting and ending indices in the current buffer
of the text to be executed.
Programs can pass third argument PRINTFLAG which controls printing of output:
nil means discard it; anything else is stream for print.

This version, from edebug, maybe instruments code for edebug depending
on the values of `edebug-all-defs' and `edebug-all-forms'.

If there is no error, point does not move.  If there is an error,
point remains at the end of the last character read from the buffer."

  ;; Because this doesnt narrow to the region,
  ;; one other difference concerns inserting whitespace after the expression.

  (interactive "r")
  (let (;; These variables need to be protected from edebug as well as user.
	(edebug-e-r-pnt (point))
	(edebug-e-r-buf (current-buffer))
	(edebug-e-r-inside-buf (current-buffer))
	;; Mark the end because it may move.
	(edebug-e-r-end-marker (set-marker (make-marker) edebug-e-r-end))
	edebug-e-r-form
	edebug-e-r-val)
    (goto-char edebug-e-r-start)
    (edebug-skip-whitespace)
    (while (< (point) edebug-e-r-end-marker)
      (setq edebug-e-r-form 
	    (edebug-read-and-maybe-wrap-form edebug-all-defs))

      ;; Evaluate normally - after restoring the current-buffer.
      (let ((edebug-e-r-current-buffer (current-buffer)))
	(set-buffer edebug-e-r-inside-buf)
	(setq edebug-e-r-val (eval edebug-e-r-form))
	;; Remember current buffer for next time.
	(setq edebug-e-r-inside-buf (current-buffer))
	(set-buffer edebug-e-r-current-buffer))

      (if edebug-e-r-output
	  (let ((standard-output (or edebug-e-r-output t)))
	    (setq values (cons edebug-e-r-val values))
	    (if (eq standard-output t)
		(prin1 edebug-e-r-val)
	      (princ "\n")
	      (prin1 edebug-e-r-val)
	      (princ "\n")
	      )))
      (goto-char
       (min (max edebug-e-r-end-marker (point))
	    (progn (edebug-skip-whitespace) (point))))
      )					; while
    (if (null edebug-e-r-output)
	;; like save-excursion recovery, but only if no error
	(progn
	  ;; but mark is not restored
	  (set-buffer edebug-e-r-buf)
	  (goto-char edebug-e-r-pnt)))
    nil
    ))


(defun edebug-eval-current-buffer (&optional edebug-e-c-b-output)
  "Execute the current buffer as Lisp code.
Programs can pass argument PRINTFLAG which controls printing of output:
nil means discard it; anything else is stream for print.

This version from edebug calls eval-region on the whole buffer."
  ;; The standard eval-current-buffer doesn't use eval-region.
  (interactive)
  (eval-region (point-min) (point-max) edebug-e-c-b-output))


(defun edebug-eval-buffer (edebug-e-b-bufname &optional edebug-e-b-printflag)
  "Execute BUFFER as Lisp code.  Programs can pass argument PRINTFLAG
which controls printing of output: nil means discard it; anything else
is stream for print.

This version from edebug calls eval-region on the whole buffer."
  (interactive "bBuffer: ")
  (save-excursion
    (set-buffer (or (get-buffer edebug-e-b-bufname) 
		    (error "No such buffer: %s" edebug-e-b-bufname)))
    (eval-region (point-min) (point-max) edebug-e-b-printflag)))


;;;; Edebug internal data
;;; ===============================

;;; The internal data that is needed for edebugging is kept in the
;;; buffer-local variable `edebug-form-data'. 

(make-variable-buffer-local 'edebug-form-data)

(defconst edebug-form-data nil
"Per-buffer local variable whose value is a list of entries.
@code{(@var{symbol} @var{begin-marker} @var{end-marker}).  The markers
are at the beginning and end of an entry level form and @var{symbol} is
a symbol that holds all edebug related information for the form on its
property list.")

(defun edebug-make-form-data-entry (symbol begin end)
  (list symbol begin end))

(defun edebug-get-form-data-entry (pnt)
  "Find the edebug form data entry which is closest to PNT.
Return `nil' if none found."
  (let ((rest edebug-form-data)
	closest-entry
	(closest-dist 999999))  ;; need maxint here
    (while (and rest (< 0 closest-dist))
      (let* ((entry (car rest))
	     (begin (nth 1 entry))
	     (dist (- pnt begin)))
	(setq rest (cdr rest))
	(if (and (< dist closest-dist)
		 (<= 0 dist)
		 (<= pnt (nth 2 entry)))
	    (setq closest-dist dist
		  closest-entry entry))))
    closest-entry))

(defun edebug-form-data-symbol ()
  "Return the edebug data symbol of the form where point is in.
If point is not inside a edebuggable form, cause error."
  (or  (car (edebug-get-form-data-entry (point)))
       (error "Form not instrumented.")))

(defun edebug-set-form-data-entry (new-entry)
  "Make NEW-ENTRY the first element in the `edebug-form-data' list."
  (edebug-clear-form-data-entry new-entry)
  (setq edebug-form-data (cons new-entry edebug-form-data)))

(defun edebug-clear-form-data-entry (entry)
;; If non-nil, clear ENTRY out of the form data.  
;; Maybe clear the markers and delete the symbol's edebug property?
  (if entry
      (progn
	;; (put (car entry) 'edebug nil)   ; 
	;; (mapcar 'edebug-clear-form-data-entry   ; dangerous
	;;   (get (car entry) 'edebug-dependents))
	;; (set-marker (nth 1 entry) nil)
	;; (set-marker (nth 2 entry) nil)
	(setq edebug-form-data (delq entry edebug-form-data)))))


;;;; Form spec utilities.
;;; ===============================

(defmacro def-edebug-spec (symbol spec)
  "Set the edebug-form-spec property of SYMBOL according to SPEC.  
The unevaluated SPEC can be 0, t, or a symbol (naming a function),
or a spec list."
  (` (put (quote (, symbol)) 'edebug-form-spec (quote (, spec)))))

(defmacro def-edebug-form-spec (symbol spec-form)
  ;; For compatibility with old version.
  (message "Obsolete: use def-edebug-spec instead.")
  (def-edebug-spec symbol (eval spec-form)))

(defun get-edebug-spec (symbol)
  ;; Get the spec of symbol resolving all indirection.
  (let ((edebug-form-spec (get symbol 'edebug-form-spec))
	indirect)
    (while (and (symbolp edebug-form-spec)
		(setq indirect (get edebug-form-spec 'edebug-form-spec)))
      ;; (edebug-trace "indirection: %s" edebug-form-spec)
      (setq edebug-form-spec indirect))
    edebug-form-spec
    ))

;;;; The Parser
;;; ===============================

;;; The top level function for parsing forms is
;;; edebug-eval-top-level-form; it calls all the rest.  It checks the
;;; syntax a bit and leaves point at any error it finds, but otherwise
;;; should appear to work like eval-defun.

;;; The basic plan is to surround each expression with a call to
;;; the edebug debugger together with indexes into a table of positions of
;;; all expressions.  Thus an expression "exp" in function foo
;;; becomes:

;;; (edebug 1 2 exp)

;;; First point moved to to the beginning of exp (offset 1 of the
;;; current function).  Then the expression is evaluated and point is
;;; moved to offset 2 after the end of exp.

;;; The top level expressions of the function are wrapped in a call to
;;; edebug-enter, which supplies the function name and the actual
;;; arguments to the function.  See functions edebug-enter, edebug-before,
;;; and edebug-after for more details.

;;;;* Dynamically bound vars, left unbound, but globally declared.
;; This is to quiet the byte compiler.

(defvar edebug-form-begin-marker) ; the mark for function being evaluated.
(defvar edebug-offset-index) ; the next available offset index.
(defvar edebug-offset-list) ; the list of offset positions.
  
(defvar edebug-def-args) ; args of defining form.
(defvar edebug-def-interactive) ; is it an emacs interactive function?

;; Bind this because we will test its value.
(defconst edebug-def-name nil) ; name of definition, used by interactive-form
;;(defconst edebug-old-def-name nil) ; previous name of containing definition.


;;;###autoload
(fset 'edebug-defun 'edebug-eval-top-level-form)

;;;###autoload
(defun edebug-eval-top-level-form ()
  "Evaluate a top level form, such as defun or defmacro.
This is like eval-defun, but with edebug calls.
Print its name in the minibuffer and leave point where it is,
or if an error occurs, leave point after it with mark at the original point."
  (interactive)
  (eval
   ;; Set edebug-all-forms only while reading, 
   ;; but this causes problems while edebugging edebug.
   (let ((edebug-all-forms t))
     (edebug-read-top-level-form))))
    


(defun edebug-read-top-level-form ()
  "Read and wrap the top level form with but don't evaluate it."
  (let ((starting-point (point)))
    (end-of-defun)
    (beginning-of-defun)
    (prog1
	(edebug-read-and-maybe-wrap-form 'edebug-defs)

    ;; Recover point, but only if no error occurred.
    (goto-char starting-point)
;;    (message "%s" wrapped-form) (sit-for 10)
    )))


(defconst edebug-error-point nil)

(defun edebug-read-and-maybe-wrap-form (edebug-defs)
  ;; Read a form and wrap it with edebug calls, if the conditions are right.
  ;; Here we just catch any no-match not caught below and signal an error.

  ;; Run the setup hook.
  (run-hooks 'edebug-setup-hook)
  (setq edebug-setup-hook nil)

  (if (string-match "Lucid" emacs-version)
      (require 'edebug-lucid))

  (let (result
	edebug-def-name;; make sure it is locally nil
	;; I dont like these here!!
	edebug-&optional
	edebug-&rest
	edebug-fence
	edebug-best-error
	edebug-error-point
	no-match)
    (setq no-match
	  (catch 'no-match
	    (setq result (edebug-read-and-maybe-wrap-form1 edebug-defs))
	    nil))
    (if no-match
	(apply 'edebug-syntax-error no-match))
    result))


(defun edebug-read-and-maybe-wrap-form1 (edebug-defs)
  (let (spec
	def-kind
	defining-form-p
	def-name
	edebug-offsets
	edebug-offsets-stack
	edebug-current-offset ; reset to nil
	form
	offsets)
    (save-excursion
      (if (and (eq 'lparen (edebug-next-token-class))
	       (eq 'symbol (progn (forward-char 1) (edebug-next-token-class))))
	  ;; Find out if this is a defining form from first symbol
	  (setq def-kind (read (current-buffer))
		spec (and (symbolp def-kind) (get-edebug-spec def-kind))
		defining-form-p (and (listp spec)
				     (eq '&define (car spec)))
		def-name (if (and defining-form-p 
				  (eq 'name (car (cdr spec)))
				  (eq 'symbol (edebug-next-token-class)))
			     (read (current-buffer))))))
    (cond
     (defining-form-p
       (if edebug-defs
	   ;; If it is a defining form and we are edebugging defs,
	   ;; then let edebug-list-form start it.
	   (progn
	     (setq form (edebug-read (current-buffer))
		   offsets edebug-offsets)
	     (let ((cursor (edebug-new-cursor (list form) (list offsets))))
	       (car
		(edebug-make-form-wrapper
		 cursor
		 (edebug-before-offset cursor) 
		 (edebug-after-offset cursor) 
		 (list (cons (symbol-name def-kind) (cdr spec)))))))

	 ;; Not edebugging this form, so reset the symbol's edebug
	 ;; property to be just a marker at the definition's source code.
	 ;; This only works for defs with simple names.
	 (put def-name 'edebug (point-marker))
	 ;; Also nil out dependent defs.
	 '(mapcar (function 
		   (lambda (def)
		     (put def-name 'edebug nil)))
		  (get def-name 'edebug-dependents))
	 (edebug-read-sexp)))

     ;; If all forms are being edebugged, explicitly wrap it.
     (edebug-all-forms
      (setq form (edebug-read (current-buffer))
	    offsets edebug-offsets)
      (let ((cursor (edebug-new-cursor (list form) (list offsets))))
	(edebug-make-form-wrapper 
	 cursor
	 (edebug-before-offset cursor) 
	 (edebug-after-offset cursor) 
	 nil)))

     ;; Not a defining form, and not edebugging.
     (t
      (edebug-read-sexp)))
    ))


(defun edebug-interactive-p-name ()
  ;; Return a unique symbol for the variable used to store the
  ;; status of interactive-p for this function.
  (intern (format "edebug-%s-interactive-p" edebug-def-name)))


(defun edebug-wrap-def-forms (forms)
  "Wrap the FORMS of a definition body."
  (if edebug-def-interactive
      (` (let (((, (edebug-interactive-p-name))
		(interactive-p)))
	   (, (edebug-make-enter-wrapper forms))))
    (edebug-make-enter-wrapper forms)))


(defvar edebug-inside-func)  ;; whether code is inside function context.
;; Currently def-form sets this to nil, while def-body sets it to t.

(defun edebug-make-enter-wrapper (forms)
  ;; Generate the enter wrapper for some forms of a definition.
  ;; This is not to be used for the body of other forms, e.g. `while',
  ;; since it wraps the list of forms with a call to `edebug-enter'.
  ;; Uses the dynamically bound vars edebug-def-name and edebug-def-args.
  ;; Do this after parsing since that may find a name.
  (setq edebug-def-name (or edebug-def-name (gensym "edebug-anon")))
  (` (edebug-enter
      (quote (, edebug-def-name))
      (list (,@ (if edebug-inside-func 
		    ;; hmm. what about more than one def-body?
		    (nreverse edebug-def-args))))
      (function (lambda () (,@ forms)))
      )))


(defun edebug-unwrap (sexp)
  "Return the unwrapped SEXP or return it as is if it is not wrapped.
The SEXP might be the result of wrapping a body, which is a list of 
expressions; a `progn' form will be returned enclosing these forms."
  (if (consp sexp)
      (cond 
       ((eq 'edebug-after (car sexp))
	(nth 3 sexp))
       ((eq 'edebug-enter (car sexp))
	(let ((forms (nthcdr 2 (nth 1 (nth 3 sexp)))))
	  (if (> (length forms) 1)
	      (cons 'progn forms)  ;; could return (values forms) instead.
	    (car forms))))
       (t sexp);; otherwise it is not wrapped, so just return it.
       )
    sexp))

(defun edebug-unwrap* (sexp)
  "Return the sexp recursively unwrapped."
  (let ((new-sexp (edebug-unwrap sexp)))
    (while (not (eq sexp new-sexp))
      (setq sexp new-sexp
	    new-sexp (edebug-unwrap sexp)))
    (if (consp new-sexp)
	(mapcar 'edebug-unwrap* new-sexp)
      new-sexp)))


(defun edebug-defining-form (cursor form-begin form-end speclist)
  ;; Process the defining form, starting outside the form.
  ;; The speclist is a generated list spec that looks like:
  ;;   (("def-symbol" defining-form-spec-sans-&define))
  ;; Skip the first offset.
  (edebug-set-cursor cursor (edebug-cursor-object cursor)
		     (cdr (edebug-cursor-offsets cursor)))
  (edebug-make-form-wrapper 
   cursor 
   form-begin form-end
   speclist))

(defun edebug-make-form-wrapper (cursor form-begin form-end
					&optional speclist)
  ;; Wrap a form, usually a defining form, but any evaluated one.
  ;; If speclist is non-nil, this is being called by edebug-defining-form.
  ;; Otherwise it is being called from edebug-read-and-maybe-wrap-form1.
  ;; This is a hack, but I havent figured out a simpler way yet.
  (let* ((form-data-entry (edebug-get-form-data-entry form-begin))
	 ;; Set this marker before parsing.
	 (edebug-form-begin-marker 	       
	  (if form-data-entry 
	      (nth 1 form-data-entry)
	    (set-marker (make-marker) form-begin)))

	 edebug-offset-list
	 (edebug-offset-index 0)
	 result

	 ;; For definitions.
	 ;; (edebug-containing-def-name edebug-def-name)
	 ;; Locally bind edebug-def-name, and get name from form-data, if any.
	 ;;(edebug-old-def-name (car form-data-entry))
	 edebug-def-name
	 edebug-def-args
	 edebug-def-interactive
	 edebug-inside-func;; whether wrapped code executes inside a function.
	 )
    
    (setq result
	  (if speclist
	      (edebug-match cursor speclist)

	    ;; else wrap as an enter-form.
	    (edebug-make-enter-wrapper (list (edebug-form cursor)))))
    
    ;; Set the name here if it was not set by edebug-make-enter-wrapper.
    (setq edebug-def-name (or edebug-def-name (gensym "edebug-anon")))

    ;; Add this def as a dependent of containing def.
    '(if (and edebug-containing-def-name
	      (not (get edebug-containing-def-name 'edebug-dependents)))
	 (put edebug-containing-def-name 'edebug-dependents
	      (cons edebug-def-name 
		    (get edebug-containing-def-name 
			 'edebug-dependents))))

    ;; (message "wrapped: %s" result) (sit-for 2)
    
    ;; Create a form-data-entry or modify existing entry's markers.
    ;; In the latter case, pointers to the entry remain eq.
    (if (not form-data-entry)
	(setq form-data-entry 
	      (edebug-make-form-data-entry
	       edebug-def-name 
	       edebug-form-begin-marker
	       (if form-data-entry 
		   (nth 2 form-data-entry)
		 (set-marker (make-marker) form-end))
	       ))
      (setcar form-data-entry edebug-def-name) ;; in case name is changed
      (set-marker (nth 1 form-data-entry) form-begin)
      (set-marker (nth 2 form-data-entry) form-end))

;;    (message "defining: %s" edebug-def-name) (sit-for 2)
    (edebug-set-form-data-entry form-data-entry)
    (message "edebug: %s" edebug-def-name)
    ;;(debug edebug-def-name)

    ;; Destructively reverse edebug-offset-list and make vector from it.
    (setq edebug-offset-list (vconcat (nreverse edebug-offset-list)))

    ;; Side effects on the property list of edebug-def-name.
    (edebug-clear-frequency-count edebug-def-name)
    (edebug-clear-coverage edebug-def-name)

    ;; Store the edebug data in symbol's property list.
    (put edebug-def-name 'edebug
	 (list edebug-form-begin-marker
	       nil			; clear breakpoints
	       edebug-offset-list))
    result
    ))


(defun edebug-clear-frequency-count (name)
  ;; Create initial frequency count vector.
  ;; For each stop point, the counter is incremented each time it is visited.
  (put name 'edebug-freq-count
       (make-vector (length edebug-offset-list) 0)))


(defun edebug-clear-coverage (name)
  ;; Create initial coverage vector.  
  ;; Only need one per expression, but it is simpler to use stop points.
  (put name 'edebug-coverage 
       (make-vector (length edebug-offset-list) 'unknown)))


(defun edebug-inc-offset (offset)
  ;; modifies edebug-offset-index and edebug-offset-list
  ;; accesses edebug-func-marc and buffer point
  (prog1
      edebug-offset-index
    (setq edebug-offset-list (cons (- offset edebug-form-begin-marker)
				   edebug-offset-list)
	  edebug-offset-index (1+ edebug-offset-index))))


(defun edebug-make-before-and-after-form (before-index form after-index)
  ;; Return the edebug form for the current function at offset BEFORE-INDEX
  ;; given FORM.  Looks like: 
  ;; (edebug-after (edebug-before BEFORE-INDEX) AFTER-INDEX FORM)
  ;; Also increment the offset index for subsequent use.
  ;; if (not edebug-stop-before-symbols) and form is a symbol,
  ;; then dont call edebug-before.
  (list 'edebug-after 
	(list 'edebug-before before-index)
	after-index form))

(defun edebug-make-after-form (form after-index)
  ;; Like edebug-make-before-and-after-form, but only after.
  (list 'edebug-after 0 after-index form))


(defun edebug-form (cursor)
  "Return the instrumented form for the following form.  
Add the point offsets to the edebug-offset-list for the form."
  ;; fails if there is no form.
  (let* ((form (edebug-top-element-required cursor "Expected form"))
	 (offset (edebug-top-offset cursor)))
    (prog1
	(cond
	 ((consp form)
	  ;; The first offset for a list form is for the list form itself.
	  (cond
	   ((eq 'quote (car form))
	    form)
	   (t
	    (let* ((head (car form))
		   (spec (and (symbolp head) (get-edebug-spec head)))
		   (new-cursor (edebug-new-cursor form offset)))
	      ;; Find out if this is a defining form from first symbol.
	      (if (and (consp spec) (eq '&define (car spec)))
		  (edebug-defining-form 
		   new-cursor 
		   (car offset)  ;; before the form
		   (edebug-after-offset cursor) 
		   (cons (symbol-name head) (cdr spec)))
		(edebug-make-before-and-after-form 
		 (edebug-inc-offset (car offset))
		 (edebug-list-form new-cursor)
		 ;; After processing the list form, the new-cursor is left
		 ;; with the offset after the form.
		 (edebug-inc-offset (edebug-cursor-offsets new-cursor))))
	      ))))

	 ((symbolp form)
	  (cond
	   ;; Check for constant symbols that dont get wrapped.
	   ((or (memq form '(t nil))
		(keywordp form))
	    form)

	   (edebug-stop-before-symbols
	    (edebug-make-before-and-after-form 
	     (edebug-inc-offset (car offset))
	     form
	     (edebug-inc-offset (cdr offset))
	     ))

	   (t 
	    (edebug-make-after-form form (edebug-inc-offset (cdr offset))))))

	 ;; Anything else is self-evaluating.
	 (t form))
    (edebug-move-cursor cursor))))


(defun edebug-list-form (cursor)
  ;; Return an instrumented form built from the list form.
  ;; The after offset will be left in the cursor after processing the form.
  (let ((head (edebug-top-element-required cursor "Expected elements"))
	;; Prevent backtracking whenever instrumenting.
	(edebug-fence t)
	;; A list form is never optional because it matches anything.
	(edebug-&optional nil)
	(edebug-&rest nil))
    ;; Skip the first offset.
    (edebug-set-cursor cursor (edebug-cursor-object cursor)
		       (cdr (edebug-cursor-offsets cursor)))
    (cond
     ((null head) nil) ; () is legal.

     ((symbolp head)
      (cond
       ((null head)
	(edebug-syntax-error "nil head"))
       ((eq head 'interactive-p)
	;; Special case: replace (interactive-p) with variable
	(setq edebug-def-interactive 'check-it)
	(edebug-move-cursor cursor)
	(edebug-interactive-p-name))
       (t
	(cons head (edebug-list-form-args 
		    head (edebug-move-cursor cursor))))))

     ((consp head)
       ;; Process anonymous function and args.
       ;; This assumes no anonymous macros.
       (edebug-match-specs cursor '(lambda-expr body)))

     (t (edebug-syntax-error
	 "Head of list form must be a symbol or lambda expression.")))
      ))

;; Defined for compatibility with old versions of edebug.
(defun edebug-forms (cursor)  (edebug-match-body cursor))

(defun edebug-sexps (cursor)  (edebug-match cursor '(&rest sexp)))


(defun edebug-list-form-args (head cursor)
  ;; Process the arguments of a list form given that head of form is a symbol.
  ;; Helper for edebug-list-form
  (let ((spec (get-edebug-spec head))
	(max-lisp-eval-depth (+ max-lisp-eval-depth 12)))
    (if spec
	(cond
	 ((consp spec)
	  ;; It is a speclist.
	  (let (edebug-&optional
		  edebug-&rest
		  edebug-best-error
		  edebug-error-point)
	      (prog1
		  (edebug-match-specs cursor spec)
		;; This is redundant for body and sexps.
		(if (not (edebug-empty-cursor cursor))
		    (if edebug-best-error 
			(apply 'edebug-no-match cursor edebug-best-error)
		      (edebug-no-match cursor "Unmatched argument(s).")
		      )))))
	 ((eq t spec) (edebug-match-body cursor))
	 ((eq 0 spec) (edebug-sexps cursor))
	 ((symbolp spec) (funcall spec cursor))  ;; Not used by edebug,
					; but leave it in for compatibility.
	 )

      ;; No edebug-form-spec provided.
      (if (edebug-macrop head)
	  (if edebug-eval-macro-args
	      (edebug-match-body cursor)
	    (edebug-sexps cursor))
	;; Otherwise it is a function call.
	(edebug-match-body cursor))
      )))


;;;; Matching of specs.
;;; ===================

(defvar edebug-fence nil) ;; whether no-match forces an error.
(defvar edebug-best-error nil)

(defvar edebug-after-dotted-spec nil)

(defvar edebug-&optional)
(defvar edebug-&rest)

(defvar edebug-depth 0)  ;; initial value
(defconst edebug-max-depth 150)  ;; maximum number of matching recursions.

;; This could be a macro to be used at top level, sublists, and vectors.
(defun edebug-match (cursor specs)
  "Top level spec matching function."
  ;; Used also at each lower level of specs.
  (let (edebug-&optional
	edebug-&rest
	edebug-best-error
	edebug-error-point
	(edebug-fence edebug-fence)  ;; locally bound to limit effect
	)
    (edebug-match-specs cursor specs)))


(defun edebug-match-specs (cursor specs)
  ;; Append results of matching the list of specs.
  (if specs
      (let ((max-lisp-eval-depth (+ 20 max-lisp-eval-depth))
	    (max-specpdl-size (+ 25 max-specpdl-size))
	    (edebug-depth 
	     (if (> edebug-depth edebug-max-depth)
		 (error "too deep - perhaps infinite loop in spec?")
	       (1+ edebug-depth)))
	    (spec (if (consp specs) (car specs) specs))
	    first-char
	    ;; In case its optional, prepare to reset cursor to current state.
	    (this-form (edebug-cursor-object cursor))
	    (this-offset (edebug-cursor-offsets cursor))
	    dotted			; is the form dotted
	    ;; Is the spec dotted but not the form.
	    (edebug-after-dotted-spec edebug-after-dotted-spec)  ; save outside value.
	    result
	    no-match)
	(setq dotted (not (listp this-form))
	      edebug-after-dotted-spec (or edebug-after-dotted-spec 
				      (and (atom specs) (not dotted)))
	      first-char (and (symbolp spec) 
			      (aref (symbol-name spec) 0)))
	(setq no-match
	      (catch 'no-match
		(setq result 
		      (cond
		       ;; "&" symbols take all following specs.
		       ((eq ?& first-char)
			(let* ((func (get-edebug-spec spec))
			       (temp (funcall func cursor (cdr specs))))
			  (if edebug-&rest
			      (nconc 
			       temp 
			       (edebug-match-specs cursor edebug-&rest))
			    temp)))
			   ;; ":" symbols take one following spec.
		       ((eq ?: first-char)
			;; Assume spec has edebug-form-spec,
			;; and spec is followed by another spec.
			(nconc (funcall (get-edebug-spec spec) 
					cursor (car (cdr specs))) 
			       (edebug-match-specs cursor 
						   (cdr (cdr specs)))))

		       (dotted
			(edebug-set-cursor 
			 ;; Wrap the form in a list, and process normally
			 cursor (list this-form) this-offset)
			;; then waste the list.
			(car (edebug-match-one-spec cursor spec)))

		       (t
			(let ((temp (edebug-match-one-spec cursor spec))
			      (rest (or (and (consp specs) (cdr specs))
					;; If none remain, check edebug-&rest
					edebug-&rest)))
			  (if rest
			      (nconc temp (edebug-match-specs cursor rest))
			    temp)))))
		nil))			; nil means no no-match occured.

	(if no-match
	    (if edebug-&optional
		(progn 
		  ;; Don't fail; just reset cursor and return nil.
		  (edebug-set-cursor cursor this-form this-offset)
		  nil);; no result at this level.
	      ;; Pass any no-match up to the next level, or error.
	      (apply 'edebug-no-match cursor no-match))
	  result))))


(defun edebug-match-one-spec (cursor spec)
  ;; Match one spec, which is not a special &-spec.
  (cond
   ((symbolp spec) (edebug-match-symbol cursor spec))
   ((vectorp spec) (edebug-match cursor (append spec nil)))
   ((stringp spec) (edebug-match-string cursor spec))
   ((listp spec) (edebug-match-list cursor spec))
   ))



(defun edebug-no-match (cursor &rest edebug-args)
  ;; Remember this point anyway.
  (setq edebug-error-point (or edebug-error-point
			       (edebug-before-offset cursor))
	edebug-best-error (or edebug-best-error edebug-args))
  (if (and edebug-fence (not edebug-&optional))
      (progn
	(if edebug-error-point
	    (goto-char edebug-error-point))
	(apply 'edebug-syntax-error edebug-args))
    (funcall 'throw 'no-match edebug-args)))

(defun edebug-before-offset (cursor)
  (let ((offset (edebug-cursor-offsets cursor)))
    (if (consp offset)
	(car (car offset))
      ;; If there is nothing left in the offsets,
      ;; return one less than the offset itself, 
      ;; which is the after offset for a list.
      (1- offset))))

(defun edebug-after-offset (cursor)
  ;; Return the after offset of the cursor object.
  (let ((offset (edebug-top-offset cursor)))
    (while (consp offset)
      (setq offset (cdr offset)))
    offset))


;; Define specs for all the symbol specs with functions used to process them.
;; Perhaps we shouldnt be doing this with edebug-form-specs since the
;; user may want to define macros or functions with the same names.
;; We could use an internal obarray for these primitive specs, or prepend
;; "edebug-spec-" to the symbol names.

(mapcar 
 (function (lambda (pair)
	     (put (car pair) 'edebug-form-spec (cdr pair))))
 '((&optional . edebug-match-&optional)
   (&rest . edebug-match-&rest)
   (&or . edebug-match-&or)
   (&not . edebug-match-&not)
   (&key . edebug-match-&key)
   ;;   (nil . edebug-match-nil)  not this one - special case it.
   (sexp . edebug-match-sexp)
   (form . edebug-match-form)
   (place . edebug-match-place)
   (body . edebug-match-body)
   ;; (function . edebug-match-function)
   (lambda-expr . edebug-match-lambda-expr)
   ;; (keywordp . edebug-match-keywordp)
   (&define . edebug-match-&define)
   (name . edebug-match-name)
   (:name . edebug-match-:name)
   (arg . edebug-match-arg)
   (def-body . edebug-match-def-body)
   (def-form . edebug-match-def-form)
   (fence . edebug-match-fence)
   ))

(defun edebug-match-symbol (cursor symbol)
  ;; Match a symbol spec.
  (let* ((spec (get-edebug-spec symbol))
	 (max-lisp-eval-depth (+ 6 max-lisp-eval-depth))  ; too much??
	 (max-specpdl-size (+ 18 max-specpdl-size)) ; args and these vars
	 )
    (cond
     (spec 
      (if (consp spec)
	  ;; It is an indirect spec.
	  (edebug-match cursor spec)
	;; Otherwise it should be the symbol name of a function.
	(funcall spec cursor)))
	   
     ((null symbol)  ;; special case this.
      (edebug-match-nil cursor))

     ((fboundp symbol)			; is it a predicate? 
      (let ((sexp (edebug-top-element-required cursor "Expected" symbol)))
	(if (not (funcall symbol sexp))
	    (edebug-no-match cursor symbol "failed"))
	(edebug-move-cursor cursor)
	(list sexp)))
     (t (error "%s is not a form-spec or function." symbol))
     )))


(defun edebug-match-&optional (cursor specs)
  ;; Keep matching until one fails.
  ;; All that matches up until the failure becomes part of the result.
  (let ((edebug-&optional specs)
	(edebug-fence nil))
    (edebug-match-specs cursor specs)))


(defun edebug-match-&rest (cursor specs)
  ;; Keep matching until it fails.  &rest itself never fails.
  ;; Result is concatenation of results.
  (let ((edebug-&rest specs)
	(edebug-&optional specs)  ; &rest specs are optional
	(edebug-fence nil))
    (edebug-match-specs cursor specs)))

(defun edebug-match-&or (cursor specs)
  ;; Keep matching until one spec succeeds, and return its results.
  ;; If none match, fail.
  ;; This needs to be optimized since most specs spend time here.
  (let (result 
	(original-specs specs)
	(speclist (list (car specs)))  ;; temp list with first spec
	(this-form (edebug-cursor-object cursor))
	(this-offset (edebug-cursor-offsets cursor))
	succeeded
	)
    (while specs
      (if (catch 'no-match
	    (setq result 
		  (let ((edebug-fence nil))  ;; only while matching each spec
		    (edebug-match cursor speclist))
		  ;; match succeeded
		  succeeded t
		  specs nil)  ;; exit the loop
	    nil
	    );; non-nil means no-match
	  (progn
	    (setq specs (cdr specs))
	    (setcar speclist (car specs));; set up the next spec
	    (edebug-set-cursor cursor this-form this-offset)
	    )))
    (if (null succeeded)
	(apply 'edebug-no-match cursor "Expected one of" original-specs))
    result))


(defun edebug-match-&not (cursor specs)
  ;; If any specs match, then fail
  (if (null (catch 'no-match
	      (let ((edebug-fence nil))
		(save-excursion
		  (edebug-match-&or cursor specs)))
	      nil))
      ;; This means something matched, so it is a no match.
      (edebug-no-match cursor "Unexpected"))
  ;; This means nothing matched, so it is OK.
  nil) ;; So, return nothing
  

(def-edebug-spec &key edebug-match-&key)

(defun edebug-match-&key (cursor specs)
  ;; Following specs must look like (<name> <spec>) ...
  ;; where <name> is the name of a keyword, and spec is its spec.
  ;; This really doesnt save much over the expanded form.
  (edebug-match-&rest 
   cursor
   (cons '&or 
	 (mapcar (function (lambda (pair)
			     (vector (format ":%s" (car pair)) 
				     (car (cdr pair)))))
		 specs))))


(defun edebug-match-fence (cursor)
  ;; Simply set the fence to prevent backtracking at this level.
  (setq edebug-fence t)
  nil)


(defun edebug-match-list (cursor specs)
  ;; The spec is a list, but what kind of list, and what context?
  (let ((spec (car specs))
	(form (edebug-top-element-required cursor "Expected" specs))
	(offset (edebug-top-offset cursor)))
    (cond
     (edebug-after-dotted-spec
      ;; After dotted spec but form did not contain dot, 
      ;; so match spec elements.
      (setq edebug-after-dotted-spec nil)
      (edebug-match-specs cursor specs))
   
     ((and (eq 'vector spec) (vectorp form))
      ;; Special case: match a vector with the specs.
      (let ((result (edebug-match-sublist
		     (edebug-new-cursor form (cdr offset))
		     (cdr specs))))
	(edebug-move-cursor cursor)
	(setcar result (apply 'vector (car result)))
	result))

     ((listp form)
      (prog1
	  (edebug-match-sublist 
	   ;; First offset is for the list form itself.
	   ;; Treat nil as empty list.
	   (edebug-new-cursor form (cdr offset)) specs)
	(edebug-move-cursor cursor)))

     ;; The following cases must be after (listp form)
     ((eq 'quote spec)
      (let ((spec (car (cdr specs))))
	(cond
	 ((symbolp spec)
	  ;; Special case: spec quotes a symbol to match.
	  ;; Change in future.  Use "..." instead.
	  (if (not (eq spec form))
	      (edebug-no-match cursor "Expected" spec))
	  (edebug-move-cursor cursor)
	  (setq edebug-fence t)
	  form)
	 (t 
	  (error "Bad spec: %s" specs)))))
     
     (t (edebug-no-match cursor "Expected" specs)))
    ))


(defun edebug-match-sublist (cursor specs)
  ;; Match a sublist of specs.
  (let (edebug-&optional
	edebug-&rest
	;;edebug-best-error
	;;edebug-error-point
	)
    (prog1 
	;; match with edebug-match-specs so edebug-best-error is not bound.
	(list (edebug-match-specs cursor specs))
      (if (not (edebug-empty-cursor cursor))
	  (if edebug-best-error 
	      (apply 'edebug-no-match cursor edebug-best-error)
	    (edebug-no-match cursor "Failed matching" specs)
	    )))))


(defun edebug-match-string (cursor spec)
  (let ((sexp (edebug-top-element-required cursor "Expected" spec)))
    (if (not (eq (intern spec) sexp))
	(edebug-no-match cursor "Expected" spec)
      ;; Since it matched, failure means immediate error, unless &optional.
      (setq edebug-fence t)
      (edebug-move-cursor cursor)
      (list sexp)
      )))

(defun edebug-match-nil (cursor)
  ;; There must be nothing left to match a nil.
  (if (not (edebug-empty-cursor cursor))
      (edebug-no-match cursor "Unmatched argument(s).")
    nil))


(defun edebug-match-function (cursor)
  (error "Use function-form instead of function in edebug spec."))

(defun edebug-match-&define (cursor specs)
  ;; Match a defining form.
  ;; Normally, &define is interpretted specially other places.
  ;; This should only be called inside of a spec list to match the remainder
  ;; of the current list.  e.g. ("lambda" &define args def-body)
   (edebug-make-form-wrapper
    cursor 
    (edebug-before-offset cursor)
    ;; Find the last offset in the list.
    (let ((offsets (edebug-cursor-offsets cursor)))
      (while (consp offsets) (setq offsets (cdr offsets)))
      offsets)
    specs))

(defun edebug-match-lambda-expr (cursor)
  ;; The expression must be a function.
  ;; This will match any list form that begins with a symbol
  ;; that has an edebug-form-spec beginning with &define.  In
  ;; practice, only lambda expressions should be used.  
  ;; I could add a &lambda specification to avoid confusion.
  (let* ((sexp (edebug-top-element-required 
		cursor "Expected lambda expression"))
	 (offset (edebug-top-offset cursor))
	 (head (and (consp form) (car sexp)))
	 (spec (and (symbolp head) (get-edebug-spec head))))
    ;; Find out if this is a lambda expression from first symbol.
    (if (and (consp spec) (eq '&define (car spec)))
	(prog1
	    (list
	     (edebug-defining-form 
	      (edebug-new-cursor sexp offset)
	      (car offset);; before the sexp
	      (edebug-after-offset cursor) 
	      (cons (symbol-name head) (cdr spec))))
	  (edebug-move-cursor cursor))
      (edebug-no-match cursor "Expected lambda expression.")
      )))

(defun edebug-match-sexp (cursor)
  (list (prog1 (edebug-top-element-required cursor "Expected sexp")
	  (edebug-move-cursor cursor))))

(defun edebug-match-form (cursor)
  (list (edebug-form cursor)))

(fset 'edebug-match-place 'edebug-match-form)
  ;; Currently identical to edebug-match-form.
  ;; This is for common lisp setf-style place arguments.


;; Not needed if the predicate exists.
'(defun edebug-match-keywordp (cursor)
  ;; Match a common lisp style keyword symbol.
  (let ((sexp (edebug-top-element cursor)))
    (if (keywordp sexp)
	(prog1
	    (list sexp)
	  (edebug-move-cursor cursor))
      (edebug-no-match cursor "Keyword expected"))))


(defun edebug-match-body (cursor)
  ;; Short for "&rest form"
  (edebug-match cursor '(&rest form)))

		 
(defun edebug-match-name (cursor)
  ;; Set the edebug-def-name bound in edebug-defining-form.
  (let ((name (edebug-top-element-required cursor "Expected name")))
    ;; Maybe strings and numbers could be used.
    (if (not (symbolp name))
	(edebug-no-match cursor "Symbol expected for name of definition."))
    (setq edebug-def-name
	  (if edebug-def-name
	      ;; Construct a new name by appending to previous name.
	      (intern (format "%s@%s" edebug-def-name name))
	    name))
    (edebug-move-cursor cursor)
    (list name)))

(defun edebug-match-:name (cursor spec)
  ;; Set the edebug-def-name to the spec.
  (setq edebug-def-name
	(if edebug-def-name
	    ;; Construct a new name by appending to previous name.
	    (intern (format "%s@%s" edebug-def-name spec))
	  spec))
  nil)

(defun edebug-match-arg (cursor)
  ;; set the def-args bound in edebug-defining-form
  (let ((edebug-arg (edebug-top-element-required cursor "Expected arg")))
    (if (or (not (symbolp edebug-arg))
	    (lambda-list-keywordp edebug-arg))
      (edebug-no-match cursor "Bad argument:" edebug-arg))
    (edebug-move-cursor cursor)
    (setq edebug-def-args (cons edebug-arg edebug-def-args))
    (list edebug-arg)))

(defun edebug-match-def-form (cursor)
  ;; Like form but the form is wrapped in edebug-enter form.
  ;; The form is assumed to be executing outside of the function context.
  ;; This is a hack for now, since a def-form might execute inside as well.
  ;; Not to be used otherwise.
  (list (edebug-wrap-def-forms (list (edebug-form cursor)))))

(defun edebug-match-def-body (cursor)
  ;; Like body but body is wrapped in edebug-enter form.
  ;; The body is assumed to be executing inside of the function context.
  ;; Not to be used otherwise.
  (let ((edebug-inside-func t))
    (list (edebug-wrap-def-forms (edebug-forms cursor)))))


;;;; Parser utilities
;;; ===============================


(defun edebug-syntax-error (&rest args)
  ;; Signal an invalid-read-syntax with ARGS.
  (signal 'invalid-read-syntax args))


(defconst edebug-read-syntax-table
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
    (aset table ?\[ 'lbracket)
    (aset table ?\] 'rbracket)
    (aset table ?\. 'dot)
    (aset table ?\# 'hash)
    ;; We treat numbers as symbols, because of confusion with -, -1, and 1-.
    ;; We dont care about any other chars since they wont be seen.
    table)
  "Lookup table for significant characters indicating the class of the
token that follows.  This is not a \"real\" syntax table.")

(defun edebug-next-token-class ()
  "Move to the next token and return its class.  We only care about
lparen, rparen, dot, quote, string, char, vector, or symbol."
  (edebug-skip-whitespace)
  (aref edebug-read-syntax-table (following-char)))


(defun edebug-skip-whitespace ()
  ;; Leave point before the next token, skipping white space and comments.
  (skip-chars-forward " \t\r\n\f")
  (while (= (following-char) ?\;)
    (skip-chars-forward "^\n\r")  ; skip the comment
    (skip-chars-forward " \t\r\n\f")))


;; Obsolete reader, except it backs up after reading symbols and strings.

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
			   (if (not (eq (aref edebug-read-syntax-table 
					      (preceding-char)) 'symbol))
			       (forward-char -1))))
     ((eq class 'string) (prog1
			     (read (current-buffer))
			   (if (/= (preceding-char) ?\")
			       (forward-char -1))))
     ((eq class 'quote) (forward-char 1)
      (list 'quote (edebug-read-sexp)))
     ((eq class 'rparen) 
      (edebug-no-match "Not enough arguments"))
     ((eq class 'rbracket) 
      (edebug-no-match "Not enough arguments"))
     (t ; anything else, just read it.
      (read (current-buffer))))))


;; Offsets for reader

(defconst edebug-offsets nil)
;; Structure representing of offset positions of expressions.
;; Each offset structure looks like: (before . after) for constituents,
;; or for structures that have elements: (before <subexpressions> . after)
;; where the <subexpressions> is a list of offset structures for subexpressions
;; including the head.

;; Stack of offset structures in reverse order of the nesting.
;; This is used to get back to previous levels.
(defconst edebug-offsets-stack nil)
(defconst edebug-current-offset nil) ; Top of the stack, for convenience.

;; We must store whether we just read a list with a dotted form that
;; is itself a list.  This structure will be condensed, so the offsets
;; must also be condensed.
(defconst edebug-read-dotted-list nil)

(defun edebug-store-before-offset (point)
  ;; Add a new offset pair with POINT as the before offset.
  (let ((new-offset (list point)))
    (if edebug-current-offset
	(setcdr edebug-current-offset
		(cons new-offset (cdr edebug-current-offset)))
      ;; Otherwise, we are at the top level, so initialize.
      (setq edebug-offsets new-offset
	    edebug-offsets-stack nil
	    edebug-read-dotted-list nil))
    ;; Cons the new offset to the front of the stack.
    (setq edebug-offsets-stack (cons new-offset edebug-offsets-stack)
	  edebug-current-offset new-offset)
    ))

(defun edebug-store-after-offset (point)
  ;; Finalize the current offset struct by reversing it and
  ;; store POINT as the after offset.
  (if (not edebug-read-dotted-list)
      ;; Just reverse the offsets of all subexpressions.
      (setcdr edebug-current-offset (nreverse (cdr edebug-current-offset)))

    ;; We just read a list after a dot, which will be abbreviated out.
    (setq edebug-read-dotted-list nil)
    ;; Drop the corresponding offset pair.
    ;; That is, nconc the reverse of the rest of the offsets 
    ;; with the cdr of last offset.
    (setcdr edebug-current-offset
	    (nconc (nreverse (cdr (cdr edebug-current-offset)))
		   (cdr (car (cdr edebug-current-offset))))))

  ;; Now append the point using nconc.
  (setq edebug-current-offset (nconc edebug-current-offset point))
  ;; Pop the stack.
  (setq edebug-offsets-stack (cdr edebug-offsets-stack)
	edebug-current-offset (car edebug-offsets-stack)))

(defun edebug-ignore-offset ()
  ;; Ignore the last created offset pair.
  (setcdr edebug-current-offset (cdr (cdr edebug-current-offset))))

(def-edebug-spec edebug-storing-offsets (form body))
(put 'edebug-storing-offsets 'lisp-indent-hook 1)
(defmacro edebug-storing-offsets (point &rest body)
  (` (unwind-protect
	 (progn 
	   (edebug-store-before-offset (, point))
	   (,@ body)) 
       (edebug-store-after-offset (point)))))

;; ==========================================
;; Reader for Emacs Lisp.
;; Uses edebug-next-token-class (and edebug-skip-whitespace) above.

(defconst edebug-read-alist
  '((symbol . edebug-read-symbol)
    (lparen . edebug-read-list)
    (string . edebug-read-string)
    (quote . edebug-read-quote)
    (lbracket . edebug-read-vector)
    (hash . edebug-read-function)
    ))

(defun edebug-read (stream)
;; Read a sexp from STREAM.
;; STREAM is actually limited to the current buffer.
;; Create a parallel offset structure as described in doc for edebug-offsets.
  (unwind-protect
      (edebug-read1 stream)
    ;; Just make sure it is reset for the next time, even if there is an error.
    (setq edebug-current-offset nil)))

(defun edebug-read1 (stream)
  (let ((class (edebug-next-token-class))
	func
	edebug-read-dotted-list) ; see edebug-store-after-offset
    (edebug-store-before-offset (point))
    (prog1
	(if (setq func (assq class edebug-read-alist))
	    (funcall (cdr func) stream)
	  ;; anything else, just read it.
	  (read stream))
      (edebug-store-after-offset (point)))
    ))

(defun edebug-read-symbol (stream)
  (prog1
      (read stream)
    ;; loses for escaped chars
    (if (not (eq (aref edebug-read-syntax-table 
		       (preceding-char)) 'symbol))
	(forward-char -1))))

(defun edebug-read-string (stream)
  (prog1
      (read stream)
    (if (/= (preceding-char) ?\")
	(forward-char -1))))

(defun edebug-read-quote (stream)
  ;; Turn 'thing into (quote thing)
  (forward-char 1)
  ;; for quote
  (edebug-store-before-offset (point))
  (edebug-store-after-offset (point))
  (list 'quote (edebug-read1 stream)))

(defun edebug-read-function (stream)
  ;; Turn #'thing into (function thing)
  (forward-char 1)
  (if (/= ?\' (following-char)) (edebug-syntax-error "Bad char"))
  (forward-char 1)
  ;; for function
  (edebug-store-before-offset (point))
  (edebug-store-after-offset (point))
  (list 'function (edebug-read1 stream)))

(defun edebug-read-list (stream)
  (forward-char 1)			; skip \(
  (prog1 
      (let ((elements))
	(while (not (memq (edebug-next-token-class) '(rparen dot)))
	  (setq elements (cons (edebug-read1 stream) elements)))
	(setq elements (nreverse elements))
	(if (eq 'dot (edebug-next-token-class))
	    (let (dotted-form)
	      (forward-char 1)		; skip \.
	      (setq dotted-form (edebug-read1 stream))
		    elements (nconc elements dotted-form)
	      (if (not (eq (edebug-next-token-class) 'rparen))
		  (edebug-syntax-error "Expected `)'"))
	      (setq edebug-read-dotted-list (listp dotted-form))
	      ))
	elements)
    (forward-char 1)			; skip \)
    ))

(defun edebug-read-vector (stream)
  (forward-char 1)			; skip \[
  (prog1 
      (let ((elements))
	(while (not (eq 'rbracket (edebug-next-token-class)))
	  (setq elements (cons (edebug-read1 stream) elements)))
	(apply 'vector (nreverse elements)))
    (forward-char 1)			; skip \]
    ))


;;; Cursors for traversal of list and vector elements with offsets.

(defun edebug-new-cursor (object offsets)
  ;; Return a new cursor for OBJECT with OFFSETS.
  (if (vectorp object) 
      (setq object (append object nil)))
  (cons object offsets))

(defun edebug-set-cursor (cursor object offsets)
  ;; Set the CURSOR's OBJECT and OFFSETS to the given.
  ;; Return the cursor.
  (setcar cursor object)
  (setcdr cursor offsets)
  cursor)

'(defun edebug-copy-cursor (cursor)
  ;; Copy the cursor using the same object and offsets.
  (cons (car cursor) (cdr cursor)))

(defun edebug-cursor-object (cursor)
  (car cursor))
(defun edebug-cursor-offsets (cursor)
  (cdr cursor))

(defun edebug-empty-cursor (cursor)
  ;; Return non-nil if CURSOR is empty - meaning no more elements.
  (null (car cursor)))

(defun edebug-top-element-required (cursor &rest error)
  ;; Check if there are any more arguments.
  (if (not (car cursor))
      (apply 'edebug-no-match cursor error))
  ;; Return that top element.
  (edebug-top-element cursor))

(defun edebug-top-element (cursor)
  ;; Return the top element at the cursor.
  ;; Assumes not empty.
  (car (car cursor)))

(defun edebug-top-offset (cursor)
  ;; Return the top offset pair corresponding to the top element.
  (car (cdr cursor)))

(defun edebug-move-cursor (cursor)
  ;; Advance and return the cursor to the next element and offset.
  ;; throw no-match if empty before moving.
  ;; This is a violation of the cursor encapsulation, but
  ;; there is plenty of that going on while matching.
  ;; The following test should always fail.
  (if (null (car cursor)) (edebug-no-match cursor "Not enough arguments."))
  (setcar cursor (cdr (car cursor)))
  (setcdr cursor (cdr (cdr cursor)))
  cursor)


;;;; Edebug Form Specs
;;; ==========================================================
;;; See cl-specs.el for common lisp specs.

;;;;* Spec for def-edebug-spec
;;; Out of date.

(defun edebug-spec-p (object)
  "Return non-nil if OBJECT is a symbol with an edebug-form-spec property."
  (and (symbolp object)
       (get object 'edebug-form-spec)))

(def-edebug-spec def-edebug-spec
  ;; Top level is different from lower levels.
  (&define :name edebug-spec name 
	   &or "nil" edebug-spec-p "t" "0" (&rest edebug-spec)))

(def-edebug-spec edebug-spec-list
  ;; A list must have something in it, or it is nil, a symbolp
  ((edebug-spec . [&or nil edebug-spec])))

(def-edebug-spec edebug-spec
  (&or
   (vector &rest edebug-spec)		; matches a vector
   ("vector" &rest edebug-spec)		; matches a vector spec
   ("quote" symbolp)
   edebug-spec-list
   stringp
   [lambda-list-keywordp &rest edebug-spec]
   [keywordp fence edebug-spec]
   edebug-spec-p  ;; Including all the special ones e.g. form.
   symbolp;; a predicate
   ))


;;;;* Emacs special forms and some functions.

;; quote expects only one argument, although it allows any number.
(def-edebug-spec quote sexp)

;; The standard defining forms.
(def-edebug-spec defconst defvar)
(def-edebug-spec defvar (symbolp &optional form stringp))

(def-edebug-spec defun
  (&define name lambda-list
	   [&optional stringp]
	   [&optional ("interactive" interactive)]
	   def-body))
(def-edebug-spec defmacro
  (&define name lambda-list def-body))

(def-edebug-spec defsubst defun)  ;; new byte compiler.

(def-edebug-spec arglist lambda-list)  ;; denegrated - use lambda-list.

(def-edebug-spec lambda-list
  (([&rest arg]
    [&optional ["&optional" arg &rest arg]]
    &optional ["&rest" arg]
    )))

(def-edebug-spec interactive
  (&optional &or stringp def-form))

;; A function-form is for an argument that may be a function or a form.
;; This specially recognizes anonymous functions quoted with quote.
(def-edebug-spec function-form
  ;; form at the end could also handle "function",
  ;; but recognize it specially to avoid wrapping function forms.
  (&or ([&or "quote" "function"] &or symbolp lambda-expr) form))

;; function expects a symbol or a lambda or macro expression
;; A macro is allowed by Emacs.
(def-edebug-spec function (&or symbolp lambda-expr))

;; lambda is a macro in emacs 19.
(def-edebug-spec lambda (&define lambda-list
				 [&optional stringp]
				 [&optional ("interactive" interactive)]
				 def-body))

;; A macro expression is a lambda expression with "macro" prepended.
(def-edebug-spec macro (&define "lambda" lambda-list def-body))

;; (def-edebug-spec anonymous-form ((&or ["lambda" lambda] ["macro" macro])))

;; Standard functions that take function-forms arguments.
(def-edebug-spec mapcar (function-form form))
(def-edebug-spec mapconcat (function-form form form))
(def-edebug-spec mapatoms (function-form &optional form))
(def-edebug-spec apply (function-form &rest form))
(def-edebug-spec funcall (function-form &rest form))

(def-edebug-spec let
  ((&rest &or symbolp (fence symbolp &optional form))
   body))

(def-edebug-spec let* let)

(def-edebug-spec setq (&rest symbolp form))
(def-edebug-spec setq-default setq)

(def-edebug-spec cond (&rest (form &rest form)))

(def-edebug-spec condition-case
  (symbolp
   form
   &rest (symbolp body)))


(def-edebug-spec ` (backquote-form))

;; Supports quotes within , and ,@ but only if immediately.
;(def-edebug-spec backquote-form
;  (&or
;   ([&or "," ",@"] &or ("quote" backquote-form) form)
;   (backquote-form . [&or nil backquote-form])
;   (vector &rest backquote-form)
;   sexp))

;; new, less stack-chewing version from Dan
(def-edebug-spec backquote-form
  (&or
   ([&or "," ",@"] &or ("quote" backquote-form) form)
   (backquote-form &rest backquote-form)
   (vector &rest backquote-form)
   sexp))

;; Anything else?


;;====================
;; Some miscellaneous specs for macros in public packages.
;; Send me yours.

;; advice.el by Hans Chalupsky (hans@cs.buffalo.edu)

(def-edebug-spec ad-dolist ((symbolp form &optional form) body))
(def-edebug-spec defadvice 
  (&define name   ;; thing being advised.
	   (name ;; class is [&or "before" "around" "after" "activation" "deactivation"] 
	    name  ;; name of advice
	    &rest sexp  ;; optional position and flags
	    )
	   [&optional stringp]
	   [&optional ("interactive" interactive)]
	   def-body))


;;;; The debugger itself
;;; ===============================

(defvar edebug-active nil
  "Non-nil when edebug is active")

;;; add minor-mode-alist entry
(or (assq 'edebug-active minor-mode-alist)
    (setq minor-mode-alist (cons (list 'edebug-active " *Debugging*")
				 minor-mode-alist)))

(defvar edebug-stack nil)
;; Stack of active functions evaluated via edebug.
;; Should be nil at the top level.

(defvar edebug-stack-depth -1)
;; Index of last edebug-stack item.

(defvar edebug-offset-indices nil)
;; Stack of offset indices of visited edebug sexps.
;; Should be nil at the top level.
;; Each function adds one cons.  Top is modified with setcar.

(defvar edebug-global-break-condition nil
  "If non-nil, an expression to test for at every stop point.
If the result is non-nil, then break.  Errors are ignored.")


(defvar edebug-entered nil
  "Non-nil if edebug has already been entered at this recursive edit level.
This should stay nil at the top level.")


;; Dynamically bound variables, declared globally but left unbound.
(defvar edebug-function) ; the function being executed. change name!!
(defvar edebug-args) ; the arguments of the function
(defvar edebug-data) ; the edebug data for the function
(defvar edebug-value) ; the result of the expression
(defvar edebug-after-index)
(defvar edebug-def-mark) ; the mark for the definition
(defvar edebug-freq-count) ; the count of expression visits.
(defvar edebug-coverage) ; the coverage results of each expression of function.

(defvar edebug-buffer) ; which buffer the function is in.
(defvar edebug-result) ; the result of the function call returned by body
(defvar edebug-outside-executing-macro)
(defvar edebug-outside-defining-kbd-macro)

(defvar edebug-execution-mode 'step) ; Current edebug mode set by user.
(defvar edebug-next-execution-mode nil) ; Use once instead of initial mode.


(defvar edebug-debugger 'edebug
  "Name of function to use for debugging when error or quit occurs.
Set this to 'debug if you want to debug edebug.")
;; Should this be an option?


(defun edebug-enter (edebug-function edebug-args edebug-body)
  "Entering FUNC.  The arguments are ARGS, and the body is BODY.
Setup edebug variables and evaluate BODY.  This function is called
when a function evaluated with edebug-eval-top-level-form is entered.  
Return the result of BODY."

  ;; Is this the first time we are entering edebug since
  ;; lower-level recursive-edit command?
  (if (not edebug-entered)
      (if edebug-next-execution-mode
	  (setq edebug-execution-mode edebug-next-execution-mode
		edebug-next-execution-mode nil)
	(if edebug-initial-mode
	    ;; Reset edebug-execution-mode to the initial mode.
	    (setq edebug-execution-mode edebug-initial-mode))))

  (let* ((edebug-entered t)
	 (edebug-data (get edebug-function 'edebug))
	 (edebug-def-mark (car edebug-data))	; mark at def start
	 (edebug-freq-count (get edebug-function 'edebug-freq-count))
	 (edebug-coverage (get edebug-function 'edebug-coverage))

	 (edebug-buffer (marker-buffer edebug-def-mark))
	 (edebug-stack (cons edebug-function edebug-stack))
	 ;; Protect indices from non-local exit.
	 (edebug-offset-indices (cons 0 edebug-offset-indices))

	 (max-lisp-eval-depth (+ 6 max-lisp-eval-depth))  ; too much??
	 (max-specpdl-size (+ 18 max-specpdl-size)) ; the args and these vars

	 ;; Save the outside value of executing macro.
	 (edebug-outside-executing-macro executing-macro)
	 ;; Don't keep reading from an executing kbd macro within edebug
	 ;; unless edebug-continue-kbd-macro is non-nil.
	 (executing-macro (if edebug-continue-kbd-macro executing-macro))

	 (debugger edebug-debugger)  ; only while edebug is active.
	 )
    (if edebug-trace
	(let ((edebug-stack-depth (1+ edebug-stack-depth))
	      edebug-result)
	  (edebug-print-trace-before)
	  (setq edebug-result (funcall edebug-body))
	  (edebug-print-trace-after)
	  edebug-result)
      (funcall edebug-body))
    ))


(defun edebug-print-trace-before ()
  (edebug-trace-display
   edebug-trace-buffer
   "%s> %s args: %s" 
   (make-string edebug-stack-depth ?\-) 
   edebug-function edebug-args))

(defun edebug-print-trace-after ()
  (edebug-trace-display
   edebug-trace-buffer
   "%s< %s result: %s" 
   (make-string edebug-stack-depth ?\-) 
   edebug-function edebug-result))




;; Use edebug-before and -after instead
'(defun edebug-eval (edebug-before-index edebug-after-index edebug-sexp)
  (let ((edebug-offset-indices
	 (cons edebug-before-index edebug-offset-indices))
	edebug-result)

    ;; Increment frequency count 
    (aset edebug-freq-count edebug-before-index
	  (1+ (aref edebug-freq-count edebug-before-index)))

    (setq edebug-result
	  (if (and (eq edebug-execution-mode 'Go-nonstop)
		   (not (edebug-input-pending-p)))
	      (eval edebug-sexp)
	    (edebug-debugger edebug-before-index 'before edebug-sexp)
	    (edebug-debugger edebug-after-index 'after (eval edebug-sexp))))
    ;; If it gets here...
    (aset edebug-freq-count edebug-after-index
	  (1+ (aref edebug-freq-count edebug-after-index)))
    (edebug-update-coverage)
    edebug-result
    ))


(defun edebug-slow-before (edebug-before-index)
  ;; Debug current function given BEFORE position.
  ;; Called from functions compiled with edebug-eval-top-level-form.  
  ;; Return the before index.
  (setcar edebug-offset-indices edebug-before-index)

  ;; Increment frequency count 
  (aset edebug-freq-count edebug-before-index
	(1+ (aref edebug-freq-count edebug-before-index)))

  (if (or (not (memq edebug-execution-mode '(Go-nonstop next)))
	  (edebug-input-pending-p))
      (edebug-debugger edebug-before-index 'before nil))
  edebug-before-index)

(defun edebug-fast-before (edebug-before-index)
  ;; Do nothing.
  )

(defun edebug-slow-after (edebug-before-index edebug-after-index edebug-value)
  ;; Debug current function given AFTER position and VALUE.
  ;; Called from functions compiled with edebug-eval-top-level-form.
  ;; Return VALUE.
  (setcar edebug-offset-indices edebug-after-index)
  ;; Increment frequency count 
  (aset edebug-freq-count edebug-after-index
	(1+ (aref edebug-freq-count edebug-after-index)))
  (if edebug-test-coverage (edebug-update-coverage))

  (if (and (eq edebug-execution-mode 'Go-nonstop)
	   (not (edebug-input-pending-p)))
      ;; Just return result.
      edebug-value
    (edebug-debugger edebug-after-index 'after edebug-value)
    ))

(defun edebug-fast-after (edebug-before-index edebug-after-index edebug-value)
  ;; Do nothing but return the value.
  edebug-value)

(defun edebug-run-slow ()
  (fset 'edebug-before 'edebug-slow-before)
  (fset 'edebug-after 'edebug-slow-after))

(defun edebug-run-fast ()
  (fset 'edebug-before 'edebug-fast-before)
  (fset 'edebug-after 'edebug-fast-after))

(edebug-run-slow)


(defun edebug-update-coverage ()
  (let ((old-result (aref edebug-coverage edebug-after-index)))
    (cond
     ((eq 'ok-coverage old-result))
     ((eq 'unknown old-result)
      (aset edebug-coverage edebug-after-index edebug-value))
     ;; Test if a different result.
     ((not (eq edebug-value old-result))
      (aset edebug-coverage edebug-after-index 'ok-coverage)))))


;; Dynamically declared unbound variables.
(defvar edebug-arg-mode)  ; the mode, either before, after, or error
(defvar edebug-outside-debug-on-error) ; the value of debug-on-error outside
(defvar edebug-break) ; whether a break occurred.
(defvar edebug-global-break) ; whether a global break occurred.
(defvar edebug-value) ; the result of the last expression.
(defvar edebug-break-condition) ; whether the breakpoint is conditional.

(defvar edebug-break-result nil)
(defvar edebug-global-break-result nil)



(defun edebug-debugger (edebug-offset-index edebug-arg-mode edebug-value)
  "Check breakpoints and pending input.
If edebug display should be updated, call edebug-display.
Return edebug-value."
  (let* ((max-lisp-eval-depth (+ 3 max-lisp-eval-depth)) ; edebug-after
	 (max-specpdl-size (+ 11 max-specpdl-size)) ; the args and vars

	 ;; This needs to be here since breakpoints may be changed.
	 (edebug-breakpoints (car (cdr edebug-data))) ; list of breakpoints
	 (edebug-break-data (assq edebug-offset-index edebug-breakpoints))
	 (edebug-break-condition (car (cdr edebug-break-data)))
	 (edebug-global-break
	  (if edebug-global-break-condition
	      (condition-case err
		  (setq edebug-global-break-result
			(eval edebug-global-break-condition))
		(error nil))))
	 (edebug-break))

      
    ;; Display if mode is not go, continue, or Continue-fast
    ;; or break, or input is pending, 
;;    (edebug-trace "exp: %s" edebug-value)
    ;; Test whether we should break.
    (setq edebug-break 
	  (or edebug-global-break
	      (and edebug-break-data
		   (or (not edebug-break-condition)
		       (setq edebug-break-result
			     (eval edebug-break-condition))))))
    (if (and edebug-break
	     (car (cdr (cdr edebug-break-data)))) ; is it temporary?
	;; Delete the breakpoint.
	(setcdr edebug-data
		(cons (delq edebug-break-data edebug-breakpoints)
		      (cdr (cdr edebug-data)))))
    (if (or (not (memq edebug-execution-mode '(go continue Continue-fast)))
	    edebug-break
	    (edebug-input-pending-p))
	(edebug-display))   ; <--------------- display
    
    edebug-value
    ))


(defvar edebug-window-start nil)
;; Remember where each buffers' window starts between edebug calls.
;; This is to avoid spurious recentering.")
;; Does this still need to be buffer-local??
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

(defun edebug-mark ()
  (let ((zmacs-regions nil)) ;; for lemacs
    (mark)))

(if (string-match "^19" emacs-version)
    (defun edebug-mark ()
      (mark t)))  ;; Does this work for lemacs too?

(defun edebug-display ()
  "Setup windows for edebug, determine mode, maybe enter recursive-edit."
  ;; Uses local variables of edebug-enter, edebug-before, edebug-after
  ;; and edebug-debugger.
  (let ((max-specpdl-size (+ 22 max-specpdl-size)) ; the args and these vars
	(max-lisp-eval-depth (+ 4 max-lisp-eval-depth))	; too much??
	(edebug-active t)		; for minor mode alist
	edebug-stop			; should we enter recursive-edit
	(edebug-point (+ edebug-def-mark
			 (aref (car (cdr (cdr edebug-data)))
			       edebug-offset-index)))
	edebug-buffer-outside-point     ; current point in edebug-buffer
	edebug-window			; window displaying edebug-buffer
	(edebug-outside-window (selected-window))
	(edebug-outside-buffer (current-buffer))
	(edebug-outside-point (point))
 	(edebug-outside-mark (edebug-mark))
	edebug-outside-windows		; window or screen configuration
	edebug-buffer-points
	
	edebug-eval-buffer		; declared here so we can kill it below
	(edebug-eval-result-list (and edebug-eval-list
				      (edebug-eval-result-list)))
	(edebug-outside-o-a-p overlay-arrow-position)
	(edebug-outside-o-a-s overlay-arrow-string)
	(edebug-outside-c-i-e-a cursor-in-echo-area)

	overlay-arrow-position
	overlay-arrow-string
	(cursor-in-echo-area nil)
	;; any others??

	edebug-trace-window
	edebug-trace-window-start
	)
    (if (not (buffer-name edebug-buffer))
	(let ((debug-on-error nil))
	  (error "Buffer defining %s not found." edebug-function)))
    
    (if (eq 'after edebug-arg-mode)
	;; Compute result string now before windows are modified.
	(edebug-compute-previous-result edebug-value))

    (if edebug-save-windows
	;; Save windows now before we modify them.
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
    (set-buffer edebug-buffer)

    (setq edebug-buffer-outside-point (point))
    (goto-char edebug-point)
	    
    (if (eq 'before edebug-arg-mode)
	;; Check whether positions are uptodate - assumes never before symbol
	(if (not (memq (following-char) '(?\( ?\# ?\` )))
	    (let ((debug-on-error nil))
	      (error "Source has changed - reevaluate definition of %s." 
		     edebug-function)
	      )))

    (setq edebug-window-start;; current buffer must be edebug-buffer.
	  (edebug-adjust-window edebug-window-start))
	    
    ;; Test if there is input, not including keyboard macros.
    (if (edebug-input-pending-p)
	(progn
	  (setq edebug-execution-mode 'step
		edebug-stop t)
	  (edebug-stop)
	  ;;	    (discard-input)		; is this unfriendly??
	  ))
    ;; Now display arrow based on mode.
    (edebug-overlay-arrow)
	    
    (cond
     ((eq 'error edebug-arg-mode)
      ;; Display error message
      (setq edebug-execution-mode 'step)
      (edebug-overlay-arrow)
      (beep)
      (if (eq 'quit (car edebug-value))
	  (message "Quit")
	(message "%s: %s" (get (car edebug-value) 'error-message)
		 (mapconcat (function (lambda (arg) (format "%s" arg)))
			    (cdr edebug-value) ","))))
     
     (edebug-break
      (cond
       (edebug-global-break
	(message "Global Break: %s => %s" 
		 edebug-global-break-condition
		 edebug-global-break-result))
       (edebug-break-condition
	(message "Break: %s => %s" 
		 edebug-break-condition 
		 edebug-break-result))
       ((not (eq edebug-execution-mode 'Continue-fast))
	(message "Break"))
       (t)))

     (t (message "")))

    (if (eq 'after edebug-arg-mode)
	(progn
	  ;; Display result of previous evaluation.
	  (if (and edebug-break
		   (not (eq edebug-execution-mode 'Continue-fast)))
	      (sit-for 1))		; Show break message.
	  (edebug-previous-result)))
    
    (cond
     (edebug-break
      (cond
       ((eq edebug-execution-mode 'continue) (edebug-sit-for 1))
       ((eq edebug-execution-mode 'Continue-fast) (edebug-sit-for 0))
       (t (setq edebug-stop t))))
     ;; not edebug-break
     ((eq edebug-execution-mode 'trace)
      (edebug-sit-for 1))		; Force update and pause.
     ((eq edebug-execution-mode 'Trace-fast)
      (edebug-sit-for 0))		; Force update and continue.
     )
    
    (unwind-protect
	(if (or edebug-stop
		(memq edebug-execution-mode '(step next))
		(eq edebug-arg-mode 'error)) 
	    (progn
	      ;; (setq edebug-execution-mode 'step)
	      ;; (edebug-overlay-arrow)	; this doesnt always show up.
	      (edebug-recursive-edit))  ; <---------- Recursive edit
	  )

      ;; Reset the edebug-window to whatever it is now.
      (setq edebug-window (edebug-get-buffer-window edebug-buffer))
      ;; Remember window-start for edebug-buffer, if still displayed.
      (if edebug-window
	  (progn
	    (set-buffer edebug-buffer);; to set edebug-window-start
	    (setq edebug-window-start (window-start edebug-window))))

      ;; Save trace window point before restoring outside windows.
      ;; Could generalize this for other buffers.
      (setq edebug-trace-window (get-buffer-window edebug-trace-buffer))
      (if edebug-trace-window
	  (setq edebug-trace-window-start
		(and edebug-trace-window (window-start edebug-trace-window))))

      ;; Restore windows before continuing.
      (if edebug-save-windows
	  (progn
	    (edebug-set-window-configuration edebug-outside-windows)

	    ;; Restore displayed buffer points.
	    ;; Needed even if restoring windows because
	    ;; window-points are not restored. (correct?? should they be??)
	    (if edebug-save-displayed-buffer-points
		(edebug-set-buffer-points edebug-buffer-points))

	    ;; Unrestore trace window's window-point.
	    (if edebug-trace-window
		(set-window-start edebug-trace-window 
				  edebug-trace-window-start))

	    ;; Unrestore edebug-buffer's window-start, if displayed.
	    (setq edebug-window (edebug-get-buffer-window edebug-buffer))
	    (set-buffer edebug-buffer);; to use edebug-window-start and point
	    (if edebug-window
		(progn
		  ;; (edebug-trace "unrestore window start: %s and point"
		  ;;			edebug-window-start)
		  (set-window-start edebug-window 
				    edebug-window-start 'no-force)
		  ;; Unrestore edebug-buffer's window-point.
		  ;; Needed in addition to setting the buffer point
		  ;; because otherwise quitting doesnt leave point as is.
		  ;; But doing it causes point not to be restored other times.
		  (set-window-point edebug-window edebug-point)
		  ))
	    ;; Unrestore edebug-buffer's point.   Rerestored below.
	    ;;	    (goto-char edebug-point) ;; in edebug-buffer
	    ;;	    (edebug-trace "unrestore edebug-buffer point: %s" (point))
	    ;;	    (sit-for 1)
	    )
	;; Since we may be in a save-excursion, in case of quit,
	;; reselect the outside window only.
	;; Only needed if we are not recovering windows??
	(if (window-point edebug-outside-window)
	    (select-window edebug-outside-window))
	)				; if edebug-save-windows

      ;; Restore current buffer always, in case application needs it.
      (set-buffer edebug-outside-buffer)
      ;; Restore point, and mark.
      ;; Needed even if restoring windows because
      ;; that doesnt restore point and mark in the current buffer.
      ;; But dont restore point if edebug-buffer is same as current buffer.
      (if (not (eq edebug-buffer edebug-outside-buffer))
	  (goto-char edebug-outside-point))
      (let ((zmacs-regions nil))
	(if (marker-buffer (mark-marker))
	    (set-marker (mark-marker) edebug-outside-mark)))
      ;;      (edebug-trace "done restoring and unrestoring") (sit-for 1)
      )					; unwind-protect
    ;; None of the following is done if quit or signal occurs.

    ;; Restore edebug-buffer's outside point.
    ;;    (edebug-trace "restore edebug-buffer point: %s" 
    ;;		  edebug-buffer-outside-point)
    (let ((current-buffer (current-buffer)))
      (set-buffer edebug-buffer)
      (goto-char edebug-buffer-outside-point)
      (set-buffer current-buffer))
    ;; ... nothing more.
    ))


(defvar print-level nil)
(defvar print-circle nil)

(defun edebug-safe-prin1-to-string (value)
  (let ((print-escape-newlines t)
	(print-length (or edebug-print-length print-length))
	(print-level (or edebug-print-level print-level))
	(print-circle (or edebug-print-circle print-circle)))
    (edebug-prin1-to-string value)))


(defvar edebug-depth 0)
;; Number of recursive edits started by edebug.
;; Should be 0 at the top level.

(defvar edebug-recursion-depth 0)
;; Value of recursion-depth when edebug was called.


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
	(edebug-interactive-p nil)      ; again non-interactive
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

	;; More for Lucid Emacs.
	(last-input-event nil)
	(last-command-event nil)
	(unread-command-event nil)

;;	(debug-on-error debug-on-error)

	;; Save the outside value of defining macro.
	(edebug-outside-defining-kbd-macro defining-kbd-macro)
	;; Don't keep defining a kbd macro.
	(defining-kbd-macro (if edebug-continue-kbd-macro defining-kbd-macro))

	;; others??
	)

    (if (fboundp 'zmacs-deactivate-region)
	(zmacs-deactivate-region))
    (if (and (eq edebug-execution-mode 'go)
	     (not (memq edebug-arg-mode '(after error))))
	(message "Break"))
    (edebug-mode)
;;    (if (boundp 'edebug-outside-debug-on-error)
;;	(setq debug-on-error edebug-outside-debug-on-error))

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
	    (if (memq edebug-execution-mode '(go Go-nonstop))
		(edebug-overlay-arrow))
	    (setq buffer-read-only edebug-buffer-read-only)
	    (use-local-map edebug-outside-map)
	    )
	;; gotta have some other buffer
	(get-buffer-create " bogus edebug buffer"))
      )))


;;;; Display related functions
;;; ===============================

(defun edebug-adjust-window (old-start)
  "If pos is not visible, adjust current window to fit following context."
;;  (message "window: %s old-start: %s window-start: %s pos: %s" 
;;	   (selected-window) old-start (window-start) (point)) (sit-for 5)
  (if (not (pos-visible-in-window-p))
      (progn
	;; First try old-start
	(if old-start
	    (set-window-start (selected-window) old-start))
	(if (not (pos-visible-in-window-p))
	    (progn
;;	(message "resetting window start") (sit-for 2)
	(set-window-start
	 (selected-window)
	 (save-excursion
	   (forward-line
	    (if (< (point) (window-start)) -1	; one line before if in back
	      (- (/ (window-height) 2)) ; center the line moving forward
	      ))
	   (beginning-of-line)
	   (point)))))))
  (window-start))
  


(defconst edebug-arrow-alist
  '((Continue-fast . "=")
    (Trace-fast . "-")
    (continue . ">")
    (trace . "->")
    (step . "=>")
    (next . "=>")
    (go . "<>")
    (Go-nonstop . "..")  ; not used
    )
  "Association list of arrows for each edebug mode.
If you come up with arrows that make more sense, let me know.")

(defun edebug-overlay-arrow ()
  "Set up the overlay arrow at beginning-of-line in current buffer.
The arrow string is derived from edebug-arrow-alist and edebug-execution-mode."
  (let* ((pos))
    (save-excursion
      (beginning-of-line)
      (setq pos (point)))
    (setq overlay-arrow-string
	  (cdr (assq edebug-execution-mode edebug-arrow-alist)))
    (setq overlay-arrow-position (make-marker))
    (set-marker overlay-arrow-position pos (current-buffer))))


(defun edebug-toggle-save-windows ()
  "Toggle the edebug-save-windows variable.
Also, each time you toggle it on, the inside and outside window
configurations become the same as the current configuration."
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
  ;; Restore the window configuration to what it last was inside.
  ;; But it is not always set.   - experiment
  ;;(if edebug-inside-windows
  ;;  (edebug-set-window-configuration edebug-inside-windows))
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


(defun edebug-bounce-point (arg)
  "Bounce the point in the outside current buffer.
If prefix arg is supplied, sit for that many seconds before returning.
The default is one second."
  (interactive "p")
  (if (not edebug-active)
      (error "edebug is not active."))
  (save-excursion
    ;; If the buffer's currently displayed, avoid the set-window-configuration.
    (save-window-excursion
      (edebug-pop-to-buffer edebug-outside-buffer)
      (goto-char edebug-outside-point)
      (let ((zmacs-regions nil))
	(message "Current buffer: %s Point: %s Mark: %s" 
		 (current-buffer) (point) 
		 (if (marker-buffer (mark-marker))
		     (marker-position (mark-marker)) "<not set>")))
      (edebug-sit-for arg)
      (edebug-pop-to-buffer edebug-buffer))))


;; Joe Wells, here is a start at your idea of adding a buffer to the internal 
;; display list.  Still need to use this list in edebug-display.

(defvar edebug-display-buffer-list nil
  "List of buffers that edebug will display when it is active.")

(defun edebug-display-buffer (buffer)
  "Toggle display of a buffer inside of edebug."
  (interactive "bBuffer: ")
  (let ((already-displaying (memq buffer edebug-display-buffer-list)))
    (setq edebug-display-buffer-list
	  (if already-displaying
	      (delq buffer edebug-display-buffer-list)
	    (cons buffer edebug-display-buffer-list)))
    (message "Displaying %s %s" buffer
	     (if already-displaying "off" "on"))))


;;;; Breakpoint related functions
;;; ===============================

(defun edebug-find-stop-point ()
  "Return (function . index) of the nearest edebug stop point."
  (let* ((edebug-def-name (edebug-form-data-symbol))
	 (edebug-data
	   (let ((data (get edebug-def-name 'edebug)))
	     (if (or (null data) (markerp data))
		 (error "%s is not instrumented for edebug." edebug-def-name))
	     data))  ; we could do it automatically, if data is a marker.
	 ;; pull out parts of edebug-data.
	 (edebug-def-mark (car edebug-data))
	 (edebug-breakpoints (car (cdr edebug-data)))

	 (offset-vector (car (cdr (cdr edebug-data))))
	 (offset (- (save-excursion
		      (if (looking-at "[ \t]")
			  ;; skip backwards until non-whitespace, or bol
			  (skip-chars-backward " \t"))
		      (point))
		    edebug-def-mark))
	 len i)
    ;; the offsets are in order so we can do a linear search
    (setq len (length offset-vector))
    (setq i 0)
    (while (and (< i len) (> offset (aref offset-vector i)))
      (setq i (1+ i)))
    (if (and (< i len)
	     (<= offset (aref offset-vector i)))
	;; return the relevant info
	(cons edebug-def-name i)
      (message "Point is not on an expression in %s."
	       edebug-def-name)
      )))


(defun edebug-next-breakpoint ()
  "Move point to the next breakpoint, or first if none past point."
  (interactive)
  (let ((edebug-stop-point (edebug-find-stop-point)))
    (if edebug-stop-point
	(let* ((edebug-def-name (car edebug-stop-point))
	       (index (cdr edebug-stop-point))
	       (edebug-data (get edebug-def-name 'edebug))
	       
	       ;; pull out parts of edebug-data
	       (edebug-def-mark (car edebug-data))
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
	      (goto-char (+ edebug-def-mark
			    (aref offset-vector (car breakpoint))))
	      
	      (message (concat (if (car (cdr (cdr breakpoint)))
				   "Temporary " "")
			       (if (car (cdr breakpoint))
				   (format "Condition: %s"
					   (edebug-safe-prin1-to-string
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
	(let* ((edebug-def-name (car edebug-stop-point))
	       (index (cdr edebug-stop-point))
	       (edebug-data (get edebug-def-name 'edebug))
	       
	       ;; pull out parts of edebug-data
	       (edebug-def-mark (car edebug-data))
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
		(if condition
		    (message "Breakpoint set in %s with condition: %s."
			     edebug-def-name condition)
		  (message "Breakpoint set in %s." edebug-def-name)))
	    (if present
		(message "Breakpoint unset in %s." edebug-def-name)
	      (message "No breakpoint here.")))
	  
	  (setcar (cdr edebug-data) edebug-breakpoints)
	  (goto-char (+ edebug-def-mark (aref offset-vector index)))
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
  ;; (interactive "P\nxCondition: ")
  (interactive 
   (list
    current-prefix-arg
    ;; Edit previous condition as follows, but it is cumbersome:
    (let ((edebug-stop-point (edebug-find-stop-point)))
      (if edebug-stop-point
	  (let* ((edebug-def-name (car edebug-stop-point))
		 (index (cdr edebug-stop-point))
		 (edebug-data (get edebug-def-name 'edebug))
		 (edebug-breakpoints (car (cdr edebug-data)))
		 (edebug-break-data (assq index edebug-breakpoints))
		 (edebug-break-condition (car (cdr edebug-break-data))))
	    (read-minibuffer 
	     (format "Condition in %s: " edebug-def-name)
	     (if edebug-break-condition
		 (format "%s" edebug-break-condition)
	       (format ""))))))))
  (edebug-modify-breakpoint t condition arg))

(defun edebug-set-global-break-condition (expression)
  (interactive (list (read-minibuffer 
		      "Global Condition: " 
		      (format "%s" edebug-global-break-condition))))
  (setq edebug-global-break-condition expression))


;;;; Mode switching functions
;;; ===============================

(defun edebug-set-mode (mode shortmsg msg)
  "Set the edebug mode to MODE.
Display SHORTMSG, or MSG if not within edebug."
  (interactive)
  (if (< 0 edebug-depth)
      ;; Used to also test: (eq (current-buffer) edebug-buffer)
      (progn
	(setq edebug-execution-mode mode)
	(message shortmsg)
	;; Continue execution
	(exit-recursive-edit))
    (setq edebug-next-execution-mode mode)
    (message msg)))


(fset 'edebug-step-through-mode 'edebug-step-mode)

(defun edebug-step-mode ()
  "Proceed to next debug step."
  (interactive)
  (edebug-set-mode 'step "" "edebug will stop at next stop point."))

(defun edebug-next-mode ()
  "Proceed to next debug after step."
  (interactive)
  (edebug-set-mode 'next "" "edebug will stop after next eval."))

(defun edebug-go-mode (arg)
  "Go, evaluating until break.
With ARG set temporary break at current point and go."
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
  (edebug-go-mode t))


(defun edebug-stop ()
  "Stop execution and do not continue.
Useful for exiting from trace loop."
  (interactive)
  (message "Stop"))


'(defun edebug-forward ()
  "Proceed to the exit of the next expression to be evaluated."
  (interactive)
  (edebug-set-mode 
   'forward "Forward"
   "edebug will stop after exiting the next expression."))


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
	   (edebug-step-mode)	; No more at this level, so step.
	 (edebug-go-mode t)
	 )))))

(defun edebug-step-in ()
  "Step into the definition of the form about to be evaluated.
Do this when stopped before the form or it will be too late.  One side
effect of using edebug-step-in is that the next time the function is
called, edebug will be called there as well."
  (interactive)
  (edebug-where);; where are we?
  (if (not (eq edebug-arg-mode 'before))
      (message "You must be before a list form.")
    (let* ((func
	    (save-excursion
	      (forward-char 1)
	      (read (current-buffer))))
	   (func-marker (if (symbolp func) (get func 'edebug)
			  (message "Lambda expressions are instrumented."))))
      (cond
       ((markerp func-marker)
	(save-excursion
	  (set-buffer (marker-buffer func-marker))
	  (goto-char func-marker)
	  (edebug-eval-top-level-form)))
       ((consp func-marker)
	;; Its already been evaluated for edebug.
	(message "%s is already instrumented." func)
	nil)
       (t (message "Don't know where %s is defined." func))))))


(defun edebug-top-level-nonstop ()
  "Set mode to Go-nonstop, and exit to top-level.
This is useful for exiting even if unwind-protect code may be executed."
  (interactive)
  (setq edebug-execution-mode 'Go-nonstop)
  (top-level))


;;(defun edebug-exit-out ()
;;  "Go until the current function exits."
;;  (interactive)
;;  (edebug-set-mode 'exiting "Exit..."))


;;;; Evaluation of expressions
;;; ===============================

(def-edebug-spec edebug-outside-excursion t)

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
       (let ((max-specpdl-size (+ 19 max-specpdl-size))
	     (max-lisp-eval-depth (+ 6 max-lisp-eval-depth))
	     (edebug-inside-map (current-local-map))
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
	     (defining-kbd-macro edebug-outside-defining-kbd-macro)
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


(defun edebug-compute-previous-result (edebug-value)
  (setq edebug-previous-result
	(if (and (numberp edebug-value)
		 (< edebug-value 256)
		 (>= edebug-value 0))
	    (format "Result: %s = %s" edebug-value
		    (single-key-description edebug-value))
	  (concat "Result: " 
		  (edebug-safe-prin1-to-string edebug-value)))))

(defun edebug-previous-result ()
  "Print the previous result."
  (interactive)
  (message "%s" edebug-previous-result))


(defun edebug-safe-eval (edebug-expr)
  "Evaluate EXPR safely. 
If there is an error, a string is returned describing the error."
  (condition-case edebug-err
      (eval edebug-expr)
    (error (edebug-format "%s: %s"  ;; could 
			  (get (car edebug-err) 'error-message)
			  (car (cdr edebug-err))))))

(defun edebug-eval-expression (edebug-expr)
  "Evaluate an expression in the outside environment.  
If interactive, prompt for the expression.
Print result in minibuffer."
  (interactive "xEval: ")
  (princ
   (edebug-outside-excursion
    ;; To avoid creating strings, this should use Joe Wells' eval-expression.
    (edebug-safe-prin1-to-string (edebug-safe-eval edebug-expr)))))

(defun edebug-eval-last-sexp ()
  "Evaluate sexp before point in the outside environment;
print value in minibuffer."
  (interactive)
  (edebug-eval-expression (edebug-last-sexp)))

(defun edebug-eval-print-last-sexp ()
  "Evaluate sexp before point in the outside environment; 
print value into current buffer."
  (interactive)
  (let* ((edebug-form (edebug-last-sexp))
	 (edebug-result-string
	  (edebug-outside-excursion 
	   (edebug-safe-prin1-to-string (edebug-safe-eval edebug-form))))
	 (standard-output (current-buffer)))
    (princ "\n")
    ;; princ the string to get rid of quotes.
    (princ edebug-result-string)
    (princ "\n")
    ))


;;;; edebug Minor Mode 
;;; ===============================

;; Global GUD bindings for all emacs-lisp-mode buffers.
(define-key emacs-lisp-mode-map "\C-x\C-a\C-s" 'edebug-step-mode)
(define-key emacs-lisp-mode-map "\C-x\C-a\C-n" 'edebug-next-mode)
(define-key emacs-lisp-mode-map "\C-x\C-a\C-c" 'edebug-go-mode)
(define-key emacs-lisp-mode-map "\C-x\C-a\C-l" 'edebug-where)
    

(defvar edebug-mode-map nil)
(if edebug-mode-map
    nil
  (progn
    (setq edebug-mode-map (copy-keymap emacs-lisp-mode-map))
    ;; control
    (define-key edebug-mode-map " " 'edebug-step-mode)
    (define-key edebug-mode-map "n" 'edebug-next-mode)
    (define-key edebug-mode-map "g" 'edebug-go-mode)
    (define-key edebug-mode-map "G" 'edebug-Go-nonstop-mode)
    (define-key edebug-mode-map "t" 'edebug-trace-mode)
    (define-key edebug-mode-map "T" 'edebug-Trace-fast-mode)
    (define-key edebug-mode-map "c" 'edebug-continue-mode)
    (define-key edebug-mode-map "C" 'edebug-Continue-fast-mode)

    ;;(define-key edebug-mode-map "f" 'edebug-forward) not implemented
    (define-key edebug-mode-map "f" 'edebug-forward-sexp)
    (define-key edebug-mode-map "h" 'edebug-goto-here)

    (define-key edebug-mode-map "i" 'edebug-step-in)
    (define-key edebug-mode-map "o" 'edebug-step-out)
    
    ;; quitting and stopping
    (define-key edebug-mode-map "q" 'top-level)
    (define-key edebug-mode-map "Q" 'edebug-top-level-nonstop)
    (define-key edebug-mode-map "a" 'abort-recursive-edit)
    (define-key edebug-mode-map "S" 'edebug-stop)

    ;; breakpoints
    (define-key edebug-mode-map "b" 'edebug-set-breakpoint)
    (define-key edebug-mode-map "u" 'edebug-unset-breakpoint)
    (define-key edebug-mode-map "B" 'edebug-next-breakpoint)
    (define-key edebug-mode-map "x" 'edebug-set-conditional-breakpoint)
    (define-key edebug-mode-map "X" 'edebug-set-global-break-condition)
    
    ;; evaluation
    (define-key edebug-mode-map "r" 'edebug-previous-result)
    (define-key edebug-mode-map "e" 'edebug-eval-expression)
    (define-key edebug-mode-map "\C-x\C-e" 'edebug-eval-last-sexp)
    (define-key edebug-mode-map "E" 'edebug-visit-eval-list)
    
    ;; views
    (define-key edebug-mode-map "w" 'edebug-where)
    (define-key edebug-mode-map "v" 'edebug-view-outside)  ;; maybe obsolete??
    (define-key edebug-mode-map "p" 'edebug-bounce-point)
    (define-key edebug-mode-map "P" 'edebug-view-outside) ;; same as v
    (define-key edebug-mode-map "W" 'edebug-toggle-save-windows)

    ;; misc
    (define-key edebug-mode-map "?" 'edebug-help)
    (define-key edebug-mode-map "d" 'edebug-backtrace)
    
    (define-key edebug-mode-map "-" 'negative-argument)

    ;; statistics
    (define-key edebug-mode-map "=" 'edebug-temp-display-freq-count)

    ;; GUD bindings
    (define-key edebug-mode-map "\C-c\C-s" 'edebug-step-mode)
    (define-key edebug-mode-map "\C-c\C-n" 'edebug-next-mode)
    (define-key edebug-mode-map "\C-c\C-c" 'edebug-go-mode)

    (define-key edebug-mode-map "\C-x " 'edebug-set-breakpoint)
    (define-key edebug-mode-map "\C-c\C-d" 'edebug-unset-breakpoint)
    (define-key edebug-mode-map "\C-c\C-t" 
      (function (lambda () (edebug-set-breakpoint t))))
    (define-key edebug-mode-map "\C-c\C-l" 'edebug-where)
   
    ))

;;;###autoload
(defvar global-edebug-prefix (purecopy "\^XX")
  "Prefix key for global edebug commands, available from any buffer.")

(defvar global-edebug-map nil
  "Global map of edebug commands, available from any buffer.")

(if global-edebug-map
    nil
  (setq global-edebug-map (make-sparse-keymap))

  (global-unset-key global-edebug-prefix)
  (global-set-key global-edebug-prefix global-edebug-map)

  (define-key global-edebug-map " " 'edebug-step-mode)
  (define-key global-edebug-map "g" 'edebug-go-mode)
  (define-key global-edebug-map "G" 'edebug-Go-nonstop-mode)
  (define-key global-edebug-map "t" 'edebug-trace-mode)
  (define-key global-edebug-map "T" 'edebug-Trace-fast-mode)
  (define-key global-edebug-map "c" 'edebug-continue-mode)
  (define-key global-edebug-map "C" 'edebug-Continue-fast-mode)

  ;; breakpoints
  (define-key global-edebug-map "b" 'edebug-set-breakpoint)
  (define-key global-edebug-map "u" 'edebug-unset-breakpoint)
  (define-key global-edebug-map "x" 'edebug-set-conditional-breakpoint)
  (define-key global-edebug-map "X" 'edebug-set-global-break-condition)

  ;; views
  (define-key global-edebug-map "w" 'edebug-where)
  (define-key global-edebug-map "W" 'edebug-display-buffer)

  ;; quitting
  (define-key global-edebug-map "q" 'top-level)
  (define-key global-edebug-map "Q" 'edebug-top-level-nonstop)
  (define-key global-edebug-map "a" 'abort-recursive-edit)

  ;; statistics
  (define-key global-edebug-map "=" 'edebug-display-freq-count)
  )


(defun edebug-help ()
  (interactive)
  (describe-function 'edebug-mode))


(defun edebug-mode ()
  "Mode for Emacs Lisp buffers while in edebug.

There are both buffer local and global key bindings to several
functions.  E.g. edebug-step-mode is bound to
\\[edebug-step-mode] in the debug buffer and \\<global-map>\\[edebug-step-mode] in any buffer.

Also see bindings for the eval list buffer, *edebug*.

The edebug buffer commands:
\\{edebug-mode-map}

Global commands prefixed by global-edbug-prefix:
\\{global-edebug-map}

Options:
edebug-all-defs
edebug-all-forms
edebug-eval-macro-args
edebug-stop-before-symbols
edebug-save-windows
edebug-save-displayed-buffer-points
edebug-initial-mode
edebug-trace
"
  (use-local-map edebug-mode-map))


;;;; edebug eval list mode
;;; ===============================================
;; A list of expressions and their evaluations is displayed in *edebug*.

;;(defvar edebug-eval-buffer "*edebug*"
;;  "*Declared globally so edebug-eval-display can be called independent
;;of edebug (not implemented yet).")


(defun edebug-eval-result-list ()
  "Return a list of evaluations of edebug-eval-list"
  ;; Assumes in outside environment.
  (mapcar 'edebug-safe-eval edebug-eval-list))

(defun edebug-eval-display-list (edebug-eval-result-list)
  ;; Assumes edebug-eval-buffer exists.
  (let ((edebug-eval-list-temp edebug-eval-list)
	(standard-output edebug-eval-buffer)
	(edebug-comment-line
	 (format ";%s\n" (make-string (- (window-width) 2) ?-))))
    (set-buffer edebug-eval-buffer)
    (erase-buffer)
    (while edebug-eval-list-temp
      (prin1 (car edebug-eval-list-temp)) (terpri)
      (prin1 (car edebug-eval-result-list)) (terpri)
      (princ edebug-comment-line)
      (setq edebug-eval-list-temp (cdr edebug-eval-list-temp))
      (setq edebug-eval-result-list (cdr edebug-eval-result-list)))
    (edebug-pop-to-buffer edebug-eval-buffer)
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
	(edebug-eval-display-list edebug-eval-result-list)
	)))

(defun edebug-eval-redisplay ()
  "Redisplay eval list in outside environment.
May only be called from within edebug-recursive-edit."
  (edebug-create-eval-buffer)
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
functions.  E.g. edebug-step-mode is bound to
\\[edebug-step-mode] in the debug buffer and
\\<global-map>\\[edebug-step-mode] in any buffer.

Eval list buffer commands:
\\{edebug-eval-mode-map}

Global commands prefixed by global-edbug-prefix:
\\{global-edebug-map}
"
  (lisp-interaction-mode)
  (setq major-mode 'edebug-eval-mode)
  (setq mode-name "Edebug-Eval")
  (use-local-map edebug-eval-mode-map))


;;;; Interface with standard debugger.
;;; ========================================

;; (setq debugger 'edebug) ; to use the edebug debugger
;; (setq debugger 'debug)  ; use the standard debugger

;; Note that debug and its utilities must be byte-compiled to work, since
;; they depend on the backtrace looking a certain way.

(defun edebug (&rest debugger-args)
  "Replacement for debug.  
If we are running an edebugged function,
show where we last were.  Otherwise call debug normally."
;;  (message "entered: %s  depth: %s  edebug-recursion-depth: %s"
;;	   edebug-entered (recursion-depth) edebug-recursion-depth)
  (if (and edebug-entered  ; anything active?
	   (eq (recursion-depth) edebug-recursion-depth))
      (let (;; Where were we before the error occurred?
	    (edebug-offset-index (car edebug-offset-indices))
	    (edebug-arg-mode (car debugger-args))
	    (edebug-value (car (cdr debugger-args)))
	    ;; Bind variables required by edebug-display
	    edebug-break-data 
	    edebug-break-condition
	    edebug-global-break
	    (edebug-break (null debugger-args))
	    (edebug-outside-debug-on-error debug-on-error)
	    (debug-on-error nil))
	(edebug-display)
	(if (eq edebug-arg-mode 'error) 
	    nil
	  edebug-value))

    ;; Otherwise call debug normally.
    ;; Still need to remove extraneous edebug calls from stack.
    (apply 'debug debugger-args)
    ))


(defun edebug-backtrace ()
  "Display a non-working backtrace.  Better than nothing..."
  (interactive)
  (if (or (not edebug-backtrace-buffer)
	  (null (buffer-name edebug-backtrace-buffer)))
      (setq edebug-backtrace-buffer
	    (generate-new-buffer "*Backtrace*"))
    ;; else, could just display edebug-backtrace-buffer
    )
  (with-output-to-temp-buffer (buffer-name edebug-backtrace-buffer)
    (setq edebug-backtrace-buffer standard-output)
    (let ((print-escape-newlines t)
	  (print-length 50)
	  last-ok-point)
      (backtrace)

      ;; Clean up the backtrace.  
      ;; Not quite right for current edebug scheme.
      (set-buffer edebug-backtrace-buffer)
      (setq truncate-lines t)
      (goto-char (point-min))
      (setq last-ok-point (point))
      (if t (progn

      ;; Delete interspersed edebug internals.
      (while (re-search-forward "^  \(?edebug" nil t)
	(beginning-of-line)
	(cond 
	 ((looking-at "^  \(edebug-after")
	  ;; Previous lines may contain code, so just delete this line
	  (setq last-ok-point (point))
	  (forward-line 1)
	  (delete-region last-ok-point (point)))

	 ((looking-at "^  edebug")
	  (forward-line 1)
	  (delete-region last-ok-point (point))
	  )))
      )))))

;;;; Trace display
;; ===============================
;;  - append text to a buffer, and update display to show it.  e.g.
;;	 (edebug-trace-display "*trace-point*"
;;	  "saving: point = %s  window-start = %s"
;;	  (point) (window-start))

(defun edebug-trace-display (buf-name fmt &rest args)
  "In buffer BUF-NAME, display FMT and ARGS at the end and make it visible.
The buffer is created if it does not exist.
You must include newlines in FMT to break lines, but one newline is appended."
  (let* ((selected-window (selected-window))
	 (buffer (get-buffer-create buf-name))
	 buf-window)
;;    (message "before pop-to-buffer") (sit-for 1)
    (edebug-pop-to-buffer buffer)
    (setq buf-window (selected-window))
    (goto-char (point-max))
    (insert (apply 'edebug-format fmt args) "\n")
    (vertical-motion (- 1 (window-height)))
    (set-window-start buf-window (point))
    (goto-char (point-max))
;;    (set-window-point buf-window (point))
;;    (edebug-sit-for 0)
    (bury-buffer buffer)
    (select-window selected-window))
  buf-name)


(defun edebug-trace (fmt &rest args)
  "Convenience call to edebug-trace-display using edebug-trace-buffer"
  (apply 'edebug-trace-display edebug-trace-buffer fmt args))


;;;; Frequency count and coverage
;;; ==============================

(defun edebug-display-freq-count ()
  "Display the frequency count data for each line of the current
definition.  The frequency counts are inserted as comment lines after
each line, and you can undo all insertions with one `undo' command.
The counts are inserted starting under the `(' before an expression
or the `)' after an expression, or on the last char of a symbol.
The counts are only displayed when they differ from previous counts on
the same line.

If coverage is being tested, whenever all known results of an expression
are `eq', the char `=' will be appended after the count
for that expression.  Note that this is always the case for an
expression only evaluated once.

To clear the frequency count and coverage data for a definition,
reinstrument it."
  (interactive)
  (let* ((function (edebug-form-data-symbol))
	 (counts (get function 'edebug-freq-count))
	 (coverages (get function 'edebug-coverage))
	 (data (get function 'edebug))
	 (def-mark (car data))	; mark at def start
	 (edebug-points (car (cdr (cdr data))))
	 (i (1- (length edebug-points)))
	 (last-index)
	 (first-index)
	 (start-of-line)
	 (start-of-count-line)
	 (last-count)
	 (last-coverage)
	 )
    (save-excursion
      ;; Traverse in reverse order so offsets are correct.
      (while (<= 0 i)
	;; Start at last expression in line.
	(goto-char (+ def-mark (aref edebug-points i)))
	(beginning-of-line)
	(setq start-of-line (- (point) def-mark)
	      last-index i)

	;; Find all indexes on same line.
	(while (and (<= 0 (setq i (1- i))) 
		    (<= start-of-line (aref edebug-points i))))
	;; Insert all the indices for this line.
	(forward-line 1)
	(setq start-of-count-line (point)
	      first-index i   ; really last index for line above this one.
	      last-count -1  ; cause first count to always appear.
	      last-coverage nil) ; cause first coverage to be different
	(insert ";#")
	;; i == first-index still
	(while (<= (setq i (1+ i)) last-index)
	  (let ((count (aref counts i))
		(coverage (aref coverages i))
		(col (save-excursion
		       (goto-char (+ (aref edebug-points i) def-mark))
		       (- (current-column)
			  (if (= ?\( (following-char)) 0 1)))))
	    (insert (make-string 
		     (max 0 (- col (- (point) start-of-count-line))) ?\ )
		    (if (and (< 0 count)
			     (not (memq coverage 
					'(unknown ok-coverage))))
			"=" "")
		    (if (= count last-count) "" (int-to-string count))
		    " ")
	    (setq last-count count)))
	(insert "\n")
	(setq i first-index)))))

(defun edebug-temp-display-freq-count ()
  "Temporarily display the frequency count data for the current definition.
It is removed when you hit any char."
  ;; This seems not to work with Emacs 18.59. It undoes too far.
  (interactive)
  (let ((buffer-read-only nil))
    (undo-boundary)
    (edebug-display-freq-count)
    (setq unread-command-char (read-char))
    (undo)))


;;;; Byte-compiler
;;; ====================
;; Extension for bytecomp to resolve undefined function references.
;; Doesnt seem to work.
;; (require 'bytecomp-runtime)

;;(eval-when-compile
;;(defun byte-compile-resolve-function 
;;(mapcar (function (lambda (func)
;;          (setq byte-compile-unresolved-functions
;;	          (delq func byte-compile-unresolved-functions))))
;; '(
;;   ;; Output from bytecomp of unresolved functions:
;;    epoch::screen-list, edebug-get-buffer-window,
;;    symbol-buffer-value, epoch::current-screen, epoch::get-screen,
;;    epoch::select-screen, epoch::screen-p, epoch::dispatch-events,
;;    edebug-original-eval-defun, -eval-current-buffer, -eval-region,
;;    edebug-input-pending-p, edebug-current-window-configuration,
;;    edebug-get-displayed-buffer-points, edebug-sit-for,
;;    edebug-set-window-configuration, edebug-prin1-to-string,
;;    edebug-prin1, edebug-print, edebug-format
;;    ))))



(edebug-install-eval-functions)  ;; Install edebug eval functions.
