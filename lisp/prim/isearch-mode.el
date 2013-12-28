;; Incremental search minor mode.
;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.

;; LCD Archive Entry:
;; isearch-mode|Daniel LaLiberte|liberte@cs.uiuc.edu
;; |A minor mode replacement for isearch.el.

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

;;;====================================================================
;; Instructions

;; Searching with isearch-mode.el should work just like isearch.el,
;; except it is done in a temporary minor mode that terminates when
;; you finish searching.

;; Semi-modal searching is supported, using a recursive edit. If
;; isearching is started non-interactively by calling one of the
;; isearch commands (e.g. (isearch-forward), but not like gnus does
;; it: (call-interactively 'isearch-forward)), isearch-mode does not
;; return until the search is completed.  You should still be able
;; switch buffers, so be careful not to get things confused.

;; The key bindings active within isearch-mode are defined below in
;; `isearch-mode-map' which is given bindings close to the default
;; characters of isearch.el for version 19.  With `isearch-mode',
;; however, you can bind multi-character keys and it should be easier
;; to add new commands.  One bug though: keys with meta-prefix cannot
;; be longer than two chars.  Also see minibuffer-local-isearch-map
;; for bindings active during `isearch-edit-string'.

;; The search ring and completion commands automatically put you in
;; the minibuffer to edit the string.  This gives you a chance to
;; modify the search string before executing the search.  There are
;; three commands to terminate the editing: C-s and C-r exit the
;; minibuffer and search forward and reverse respectively, while C-m
;; exits and does a nonincremental search.

;; Exiting immediately from isearch uses isearch-edit-string instead
;; of nonincremental-search, if search-nonincremental-instead is non-nil.
;; The name of this option should probably be changed if we decide to
;; keep the behavior.  One difference is that isearch-edit-string does
;; not support word search yet; perhaps isearch-mode should support it
;; even for incremental searches, but how?

;;;====================================================================
;;; Change History

;;; Header: /import/kaplan/kaplan/liberte/Isearch/RCS/isearch-mode.el,v 1.3 92/06/29 13:10:08 liberte Exp Locker: liberte 
;;; Log:	isearch-mode.el,v 
;;;
;;; 20-aug-92  Hacked by jwz for Lucid Emacs 19.3.
;;;
;;; Revision 1.3  92/06/29  13:10:08  liberte
;;; Moved modal isearch-mode handling into isearch-mode.
;;; Got rid of buffer-local isearch variables.
;;; isearch-edit-string used by ring adjustments, completion, and
;;; nonincremental searching.  C-s and C-r are additional exit commands.
;;; Renamed all regex to regexp.
;;; Got rid of found-start and found-point globals.
;;; Generalized handling of upper-case chars.
 
;;; Revision 1.2  92/05/27  11:33:57  liberte
;;; Emacs version 19 has a search ring, which is supported here.
;;; Other fixes found in the version 19 isearch are included here.
;;;
;;; Also see variables search-caps-disable-folding,
;;; search-nonincremental-instead, search-whitespace-regexp, and
;;; commands isearch-toggle-regexp, isearch-edit-string.
;;;
;;; semi-modal isearching is supported.

;;; Changes for 1.1
;;; 3/18/92 Fixed invalid-regexp.
;;; 3/18/92 Fixed yanking in regexps.


(defun isearch-char-to-string (c)
  (if (integerp c)
      (make-string 1 c)
   (make-string 1 (event-to-character c nil nil t))))

;(defun isearch-text-char-description (c)
;  (isearch-char-to-string c))

(define-function 'isearch-text-char-description 'text-char-description)


;;;=========================================================================
;;; User-accessible variables

(defvar search-last-string ""
  "Last string search for by a search command.
This does not include direct calls to the primitive search functions,
and does not include searches that are aborted.")

(defvar search-last-regexp ""
  "Last string searched for by a regexp search command.
This does not include direct calls to the primitive search functions,
and does not include searches that are aborted.")

(defconst search-exit-option t
  "Non-nil means random control characters terminate incremental search.")

(defvar search-slow-window-lines 1
  "*Number of lines in slow search display windows.
These are the short windows used during incremental search on slow terminals.
Negative means put the slow search window at the top (normally it's at bottom)
and the value is minus the number of lines.")

(defconst search-slow-speed 1200
  "*Highest terminal speed at which to use \"slow\" style incremental search.
This is the style where a one-line window is created to show the line
that the search has reached.")

(defvar search-caps-disable-folding t
  "*If non-nil, upper case chars disable case fold searching.
This does not apply to \"yanked\" strings.")

(defvar search-nonincremental-instead t
  "*If non-nil, do a nonincremental search instead if exiting immediately.")
  
(defconst search-whitespace-regexp "\\(\\s \\|[\n\r]\\)+"
  "*If non-nil, regular expression to match a sequence of whitespace chars.")

;;;==================================================================
;;; Search ring.

(defvar search-ring nil
  "List of search string sequences.")
(defvar regexp-search-ring nil
  "List of regular expression search string sequences.")

(defconst search-ring-max 16
  "*Maximum length of search ring before oldest elements are thrown away.")
(defconst regexp-search-ring-max 16
  "*Maximum length of regexp search ring before oldest elements are thrown away.")

(defvar search-ring-yank-pointer nil
  "The tail of the search ring whose car is the last thing searched for.")
(defvar regexp-search-ring-yank-pointer nil
  "The tail of the regular expression search ring whose car is the last
thing searched for.")

;;;====================================================
;;; Define isearch-mode keymap.

(defvar isearch-mode-map nil
  "Keymap for isearch-mode.")

(if isearch-mode-map
    nil
  (let ((map (make-keymap)))
    (set-keymap-name map 'map)

    ;; Bind all printing characters to `isearch-printing-char'.
    ;; This isn't normally necessary, but if a printing character were 
    ;; bound to something other than self-insert-command in global-map, 
    ;; then it would terminate the search and be executed without this.
    (let ((i 32)
	  (str (make-string 1 0)))
      (while (< i 127)
	(aset str 0 i)
	(define-key map str 'isearch-printing-char)
	(setq i (1+ i))))
    (define-key map "\t" 'isearch-printing-char)

    ;; Several non-printing chars change the searching behavior.
    ;;
    (define-key map "\C-s" 'isearch-repeat-forward)
    (define-key map "\C-r" 'isearch-repeat-backward)
    (define-key map "\177" 'isearch-delete-char)
    (define-key map "\C-g" 'isearch-abort)

    (define-key map "\C-q" 'isearch-quote-char)

    (define-key map "\C-m" 'isearch-exit)
    (define-key map "\C-j" 'isearch-printing-char)
    (define-key map "\t" 'isearch-printing-char)

    (define-key map "\C-w" 'isearch-yank-word)
    (define-key map "\C-y" 'isearch-yank-line)

    ;; Define keys for regexp chars * ? |
    (define-key map "*" 'isearch-*-char)
    (define-key map "?" 'isearch-*-char)
    (define-key map "|" 'isearch-|-char)

    ;; Some bindings you may want to put in your isearch-mode-hook.
    ;; Suggest some alternates...
    ;; (define-key map "\C-t" 'isearch-toggle-regexp)
    ;; (define-key map "\C-^" 'isearch-edit-string)

    ;; backspace deletes, but C-h is help.
    (define-key map 'backspace 'isearch-delete-char)
    (define-key map '(control h) 'isearch-mode-help)

    (define-key map "\M-n" 'isearch-ring-advance)
    (define-key map "\M-p" 'isearch-ring-retreat)
    (define-key map "\M- " 'isearch-whitespace-chars)
    (define-key map "\M-\t" 'isearch-complete)

    (define-key map 'button2 'isearch-yank-x-selection)

    (setq isearch-mode-map map)
    ))

(defvar minibuffer-local-isearch-map nil
  "Keymap for editing isearch strings in the minibuffer.")

(if minibuffer-local-isearch-map
    nil
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (set-keymap-name map 'minibuffer-local-isearch-map)

    ;;#### This should just arrange to use the usual Emacs minibuffer histories
    (define-key map "\r" 'isearch-nonincremental-exit-minibuffer)
    (define-key map "\M-n" 'isearch-ring-advance-edit)
    (define-key map "\M-p" 'isearch-ring-retreat-edit)
    (define-key map "\M-\t" 'isearch-complete-edit)
    (define-key map "\C-s" 'isearch-forward-exit-minibuffer)
    (define-key map "\C-r" 'isearch-reverse-exit-minibuffer)

    (setq minibuffer-local-isearch-map map)))

;;;========================================================
;; Internal variables declared globally for byte-compiler.
;; These are all bound locally while editing the search string.

(defvar isearch-forward nil)	; Searching in the forward direction.
(defvar isearch-regexp nil)	; Searching for a regexp.
(defvar isearch-word nil)	; Searching for words.

(defvar isearch-cmds nil)   ; Stack of search status sets.
(defvar isearch-string "")  ; The current search string.
(defvar isearch-message "") ; text-char-description version of isearch-string

(defvar isearch-success t)		; Searching is currently successful.
(defvar isearch-invalid-regexp nil)	; Regexp not well formed.
(defvar isearch-other-end nil)	; Start (end) of match if forward (backward).
(defvar isearch-wrapped nil)	; Searching restarted from the top (bottom).
(defvar isearch-barrier 0)
(defvar isearch-buffer nil)	; the buffer we've frobbed the keymap of

(defvar isearch-case-fold-search nil)

(defvar isearch-adjusted nil)
(defvar isearch-slow-terminal-mode nil)
;;; If t, using a small window.
(defvar isearch-small-window nil)
(defvar isearch-opoint 0)
;;; The window configuration active at the beginning of the search.
(defvar isearch-window-configuration nil)
(defvar isearch-old-local-map nil)

;; Flag to indicate a yank occurred, so don't move the cursor.
(defvar isearch-yank-flag nil)

;;; A function to be called after each input character is processed.
;;; (It is not called after characters that exit the search.)
;;; It is only set from an optional argument to `isearch-mode'.
(defvar isearch-op-fun nil)

;;;  Is isearch-mode in a recursive edit for modal searching.
(defvar isearch-recursive-edit nil)

;;; Should isearch be terminated after doing one search?
(defvar isearch-nonincremental nil)

;; New value of isearch-forward after isearch-edit-string.
(defvar isearch-new-forward nil)


(defvar isearch-mode-hook nil
  "Function(s) to call after starting up an incremental search.")

(defvar isearch-mode-end-hook nil
  "Function(s) to call after terminating an incremental search.")

;;;==============================================================
;; Minor-mode-alist changes - kind of redundant with the
;; echo area, but if isearching in multiple windows, it can be useful.

(or (assq 'isearch-mode minor-mode-alist)
    (setq minor-mode-alist
	  (purecopy
	   (append minor-mode-alist
		   '((isearch-mode isearch-mode))))))

(defvar isearch-mode nil)
(make-variable-buffer-local 'isearch-mode)

;;;===============================================================
;;; Entry points to isearch-mode.
;;; These four functions should replace those in loaddefs.el
;;; An alternative is to fset isearch-forward etc to isearch-mode,
;;; and look at the last command to set the options accordingly.

(defun isearch-forward (&optional regexp-p)
  "Do incremental search forward.
With a prefix argument, do an incremental regular expression search instead.
\\<isearch-mode-map>
As you type characters, they add to the search string and are found.
The following non-printing keys are bound in `isearch-mode-map'.  

Type \\[isearch-delete-char] to cancel characters from end of search string.
Type \\[isearch-exit] to exit, leaving point at location found.
Type LFD (C-j) to match end of line.
Type \\[isearch-repeat-forward] to search again forward,\
 \\[isearch-repeat-backward] to search again backward.
Type \\[isearch-yank-word] to yank word from buffer onto end of search\
 string and search for it.
Type \\[isearch-yank-line] to yank rest of line onto end of search string\
 and search for it.
Type \\[isearch-quote-char] to quote control character to search for it.
Type \\[isearch-whitespace-chars] to match all whitespace chars in regexp.
\\[isearch-abort] while searching or when search has failed cancels input\
 back to what has
 been found successfully.
\\[isearch-abort] when search is successful aborts and moves point to\
 starting point.

Also supported is a search ring of the previous 16 search strings.
Type \\[isearch-ring-advance] to search for the next item in the search ring.
Type \\[isearch-ring-retreat] to search for the previous item in the search\
 ring.
Type \\[isearch-complete] to complete the search string using the search ring.

The above keys are bound in the isearch-mode-map.  To change the keys which
 are special to isearch-mode, simply change the bindings in that map.

Other control and meta characters terminate the search
 and are then executed normally (depending on `search-exit-option').

If this function is called non-interactively, it does not return to
the calling function until the search is done.

The bindings, more precisely:
\\{isearch-mode-map}"

;; Non-standard bindings
;; Type \\[isearch-toggle-regexp] to toggle regular expression with normal searching.
;; Type \\[isearch-edit-string] to edit the search string in the minibuffer.
;;  Terminate editing and return to incremental searching with CR.

  (interactive "P")
  (isearch-mode t (not (null regexp-p)) nil (not (interactive-p))))

(defun isearch-forward-regexp ()
  "\
Do incremental search forward for regular expression.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  (interactive)
  (isearch-mode t t nil (not (interactive-p))))

(defun isearch-backward (&optional regexp-p)
  "\
Do incremental search backward.
With a prefix argument, do an incremental regular expression search instead.
See \\[isearch-forward] for more information."
  (interactive "P")
  (isearch-mode nil (not (null regexp-p)) nil (not (interactive-p))))

(defun isearch-backward-regexp ()
  "\
Do incremental search backward for regular expression.
Like ordinary incremental search except that your input
is treated as a regexp.  See \\[isearch-forward] for more info."
  (interactive)
  (isearch-mode nil t nil (not (interactive-p))))


(defun isearch-mode-help ()
  (interactive)
  (describe-function 'isearch-forward)
  (isearch-update))


;;;==================================================================
;; isearch-mode only sets up incremental search for the minor mode.
;; All the work is done by the isearch-mode commands.

(defun isearch-mode (forward &optional regexp op-fun recursive-edit word-p)
  "Start isearch minor mode.  Called by isearch-forward, etc."

  (if executing-macro (setq recursive-edit nil))

  (let ((inhibit-quit t)) ; don't leave things in an inconsistent state...

    ;; Initialize global vars.
    (setq isearch-buffer (current-buffer)
	  isearch-forward forward
	  isearch-regexp regexp
	  isearch-word word-p
	  isearch-op-fun op-fun
	  isearch-case-fold-search case-fold-search
	  isearch-string ""
	  isearch-message ""
	  isearch-cmds nil
	  isearch-success t
	  isearch-wrapped nil
	  isearch-barrier (point)
	  isearch-adjusted nil
	  isearch-yank-flag nil
	  isearch-invalid-regexp nil
	  isearch-slow-terminal-mode (and (<= baud-rate search-slow-speed)
					  (> (window-height)
					     (* 4 search-slow-window-lines)))
	  isearch-other-end nil
	  isearch-small-window nil

	  isearch-opoint (point)
	  isearch-window-configuration (current-window-configuration)
	  isearch-old-local-map (current-local-map)

	  ;; bound below
	  ;;isearch-recursive-edit recursive-edit

	  isearch-old-pre-command-hook pre-command-hook

	  isearch-mode " Isearch"
	  )
    (add-hook 'pre-command-hook 'isearch-pre-command-hook)
    (set-buffer-modified-p (buffer-modified-p)) ; update modeline
    (isearch-push-state)

    (use-local-map isearch-mode-map)

    ;; This is so that the buffer-local bindings are accessible from isearch
    ;; as well; that way, a buffer-local binding will cause isearch to
    ;; terminate correctly (for example, if C-c is a buffer-local prefix key,
    ;; but is globally unbound.)
    (set-keymap-parent isearch-mode-map isearch-old-local-map)

    ) ; inhibit-quit is t before here

  (isearch-update)
  (run-hooks 'isearch-mode-hook)

  ;; isearch-mode can be made modal (in the sense of not returning to 
  ;; the calling function until searching is completed) by entering 
  ;; a recursive-edit and exiting it when done isearching.
  (if recursive-edit
      (let ((isearch-recursive-edit t))
	(recursive-edit)))
  )


;;;====================================================
;; Some high level utilities.  Others below.

(defun isearch-update ()
  ;; Called after each command to update the display.  
  (if (null unread-command-event)
      (progn
	(if (not (input-pending-p))
	    (isearch-message))
	(if (and isearch-slow-terminal-mode
		 (not (or isearch-small-window 
			  (pos-visible-in-window-p))))
	    (let ((found-point (point)))
	      (setq isearch-small-window t)
	      (move-to-window-line 0)
	      (let ((window-min-height 1))
		(split-window nil (if (< search-slow-window-lines 0)
				      (1+ (- search-slow-window-lines))
				    (- (window-height)
				       (1+ search-slow-window-lines)))))
	      (if (< search-slow-window-lines 0)
		  (progn (vertical-motion (- 1 search-slow-window-lines))
			 (set-window-start (next-window) (point))
			 (set-window-hscroll (next-window)
					     (window-hscroll))
			 (set-window-hscroll (selected-window) 0))
		(other-window 1))
	      (goto-char found-point)))
	(if isearch-other-end
	    (if (< isearch-other-end (point))
		(isearch-highlight isearch-other-end (point))
	      (isearch-highlight (point) isearch-other-end)))
	))
  (setq ;; quit-flag nil  not for isearch-mode
   isearch-adjusted nil
   isearch-yank-flag nil)
  )


(defun isearch-done ()
  ;; Called by all commands that terminate isearch-mode.
  (let ((inhibit-quit t)) ; danger danger!
    (if isearch-buffer
	(save-excursion
	  ;; some loser process filter might have switched the window's buffer,
	  ;; so be sure to set these variables back in the buffer we frobbed
	  ;; them in.
	  (set-buffer isearch-buffer)
	  (use-local-map isearch-old-local-map)
	  (setq pre-command-hook isearch-old-pre-command-hook)
	  (set-keymap-parent isearch-mode-map nil)
	  (setq isearch-mode nil)
	  (setq isearch-buffer nil)
	  (set-buffer-modified-p (buffer-modified-p));; update modeline
	  (isearch-dehighlight t)))

    ;; it's not critical that this be inside inhibit-quit, but leaving
    ;; things in small-window-mode would be bad.
    (let ((found-start (window-start (selected-window)))
	  (found-point (point)))
      (set-window-configuration isearch-window-configuration)

      ;; If there was movement, mark the starting position.
      ;; Maybe should test difference between and set mark iff > threshold.
      (if (/= (point) isearch-opoint)
	  (push-mark isearch-opoint)
	(message ""))
      (if isearch-small-window
	  (goto-char found-point)
	;; Exiting the save-window-excursion clobbers window-start; restore it.
	(set-window-start (selected-window) found-start t)))

    ) ; inhibit-quit is t before here

  (if (> (length isearch-string) 0)
      ;; Update the ring data.
      (if isearch-regexp 
	  (if (not (setq regexp-search-ring-yank-pointer
			 (member isearch-string regexp-search-ring)))
	      (progn
		(setq regexp-search-ring
		      (cons isearch-string regexp-search-ring)
		      regexp-search-ring-yank-pointer regexp-search-ring)
		(if (> (length regexp-search-ring) regexp-search-ring-max)
		    (setcdr (nthcdr (1- search-ring-max) regexp-search-ring)
			    nil))))
	(if (not (setq search-ring-yank-pointer
		       ;; really need equal test instead of eq.
		       (member isearch-string search-ring)))
	    (progn
	      (setq search-ring (cons isearch-string search-ring)
		    search-ring-yank-pointer search-ring)
	      (if (> (length search-ring) search-ring-max)
		  (setcdr (nthcdr (1- search-ring-max) search-ring) nil))))))

  (run-hooks 'isearch-mode-end-hook)
  (if isearch-recursive-edit (exit-recursive-edit)))


;;;====================================================
;; Commands active while inside of the isearch minor mode.

(defun isearch-exit ()
  "Exit search normally.
However, if this is the first command after starting incremental
search and `search-nonincremental-instead' is non-nil, do an
incremental search via `isearch-edit-string'."
  (interactive)
  (if (and search-nonincremental-instead 
	   (= 0 (length isearch-string)))
      (let ((isearch-nonincremental t))
	(isearch-edit-string))
    (isearch-done)))


(defun isearch-edit-string ()
  "Edit the search string in the minibuffer.
The following additional command keys are active while editing.
\\<minibuffer-local-isearch-map>
\\[exit-minibuffer] to exit editing and resume incremental searching.
\\[isearch-forward-exit-minibuffer] to resume isearching forward.
\\[isearch-backward-exit-minibuffer] to resume isearching backward.
\\[isearch-ring-advance-edit] to replace the search string with the next\
 item in the search ring.
\\[isearch-ring-retreat-edit] to replace the search string with the next\
 item in the search ring.
\\[isearch-complete-edit] to complete the search string from the search ring."

  ;; Editing doesnt back up the search point.  Should it?
  (interactive)

  (condition-case err
      (let ((minibuffer-local-map minibuffer-local-isearch-map)
	    isearch-nonincremental	; should search nonincrementally?
	    isearch-new-string
	    isearch-new-message
	    (isearch-new-forward isearch-forward)

	    ;; Locally bind all isearch global variables to protect them
	    ;; from recursive isearching.
	    (isearch-string isearch-string)
	    (isearch-message isearch-message)
	    (isearch-forward isearch-forward) ; set by commands below.

	    (isearch-forward isearch-forward)
	    (isearch-regexp isearch-regexp)
	    (isearch-word isearch-word)
	    (isearch-op-fun isearch-op-fun)
	    (isearch-cmds isearch-cmds)
	    (isearch-success isearch-success)
	    (isearch-wrapped isearch-wrapped)
	    (isearch-barrier isearch-barrier)
	    (isearch-adjusted isearch-adjusted)
	    (isearch-yank-flag isearch-yank-flag)
	    (isearch-invalid-regexp isearch-invalid-regexp)
	    (isearch-other-end isearch-other-end)
	    (isearch-opoint isearch-opoint)
	    (isearch-slow-terminal-mode isearch-slow-terminal-mode)
	    (isearch-small-window isearch-small-window)
	    (isearch-recursive-edit isearch-recursive-edit)
	    (isearch-window-configuration (current-window-configuration))
	    )
	;; Actually terminate isearching until editing is done.
	;; This is so that the user can do anything without failure, 
	;; like switch buffers and start another isearch, and return.
	(condition-case err
	    (isearch-done)
          ;;#### What does this mean?  There is no such condition!
	  (exit nil))			; was recursive editing

	(unwind-protect
	    (let ((prompt (isearch-message-prefix nil t))
                  event)
	      ;; If the first character the user types when we prompt them
	      ;; for a string is the yank-word character, then go into
	      ;; word-search mode.  Otherwise unread that character and
	      ;; read a string the normal way.
	      (let ((cursor-in-echo-area t))
		(message "%s" prompt)
		(setq event (next-command-event))
		(if (eq 'isearch-yank-word
			(lookup-key isearch-mode-map (vector event)))
		    (setq isearch-word t)
		  (setq unread-command-event event)))
	      (setq isearch-new-string
                    (if (fboundp 'gmhist-old-read-from-minibuffer)
                        ;; Eschew gmhist crockery
			(gmhist-old-read-from-minibuffer prompt isearch-string)
		      (read-string
		       prompt isearch-string
		       't            ;does its own history (but shouldn't)
;;                     (if isearch-regexp
;;                         ;; The search-rings aren't exactly minibuffer
;;                         ;;  histories, but they are close enough
;;                         (cons 'regexp-search-ring
;;                               (- (length regexp-search-ring-yank-pointer)
;;                                  (length regexp-search-ring)))
;;                         (cons 'search-ring
;;                               (- (length search-ring-yank-pointer)
;;                                  (length search-ring))))
		       ))
		    isearch-new-message (mapconcat
					 'isearch-text-char-description
					 isearch-new-string ""))
	      )
	  ;; Always resume isearching by restarting it.
	  (isearch-mode isearch-forward 
			isearch-regexp 
			isearch-op-fun 
			isearch-recursive-edit
			isearch-word)
	  )

	;; Copy new values in outer locals to isearch globals
	(setq isearch-string isearch-new-string
	      isearch-message isearch-new-message
	      isearch-forward isearch-new-forward)

	;; Empty isearch-string means use default.
	(if (= 0 (length isearch-string))
	    (setq isearch-string (if isearch-regexp search-last-regexp
				   search-last-string))
	  ;; Set last search string now so it is set even if we fail.
	  (if search-last-regexp
	      (setq search-last-regexp isearch-string)
	    (setq search-last-string isearch-string)))

	;; Reinvoke the pending search.
	(isearch-push-state)
	(isearch-search)
	(isearch-update)
	(if isearch-nonincremental (isearch-done)))

    (quit  ; handle abort-recursive-edit
     (isearch-abort)  ;; outside of let to restore outside global values
     )))

(defun isearch-nonincremental-exit-minibuffer ()
  (interactive)
  (setq isearch-nonincremental t)
  (exit-minibuffer))

(defun isearch-forward-exit-minibuffer ()
  (interactive)
  (setq isearch-new-forward t)
  (exit-minibuffer))

(defun isearch-reverse-exit-minibuffer ()
  (interactive)
  (setq isearch-new-forward nil)
  (exit-minibuffer))


(defun isearch-abort ()
  "Quit incremental search mode if searching is successful, signalling quit.
Otherwise, revert to previous successful search and continue searching.
Use `isearch-exit' to quit without signalling."
  (interactive)
;;  (ding)  signal instead below, if quiting
  (discard-input)
  (if isearch-success
      ;; If search is successful, move back to starting point
      ;; and really do quit.
      (progn (goto-char isearch-opoint)
	     (isearch-done)   ; exit isearch
	     (signal 'quit '(isearch)))  ; and pass on quit signal
    ;; If search is failing, rub out until it is once more successful.
    (while (not isearch-success) (isearch-pop-state))
    (isearch-update)))


(defun isearch-repeat (direction)
  ;; Utility for isearch-repeat-forward and -backward.
  (if (eq isearch-forward (eq direction 'forward))
      ;; C-s in forward or C-r in reverse.
      (if (equal isearch-string "")
	  ;; If search string is empty, use last one.
	  (setq isearch-string
		(or (if isearch-regexp
			(if regexp-search-ring-yank-pointer
			    (car regexp-search-ring-yank-pointer)
			  (car regexp-search-ring))
		      (if search-ring-yank-pointer
			  (car search-ring-yank-pointer)
			(car search-ring)))
		    "")
		isearch-message
		(mapconcat 'isearch-text-char-description
			   isearch-string ""))
	;; If already have what to search for, repeat it.
	(or isearch-success
	    (progn 

	      (goto-char (if isearch-forward (point-min) (point-max)))
	      (setq isearch-wrapped t))))
    ;; C-s in reverse or C-r in forward, change direction.
    (setq isearch-forward (not isearch-forward)))

  (setq isearch-barrier (point)) ; For subsequent \| if regexp.
  (setq isearch-success t)
  (or (equal isearch-string "")
      (progn
	;; If repeating a search that found
	;; an empty string, ensure we advance.
	(if (equal (match-end 0) (match-beginning 0))
	    (forward-char (if isearch-forward 1 -1)))
	(isearch-search)))
  (isearch-push-state)
  (isearch-update))

(defun isearch-repeat-forward ()
  "Repeat incremental search forwards."
  (interactive)
  (isearch-repeat 'forward))

(defun isearch-repeat-backward ()
  "Repeat incremental search backwards."
  (interactive)
  (isearch-repeat 'backward))

(defun isearch-toggle-regexp ()
  "Toggle regexp searching on or off."
  ;; The status stack is left unchanged.
  (interactive)
  (setq isearch-regexp (not isearch-regexp))
  (if isearch-regexp (setq isearch-word nil))
  (isearch-update))

(defun isearch-delete-char ()
  "Discard last input item and move point back.  
If no previous match was done, just beep."
  (interactive)
  (if (null (cdr isearch-cmds))
      (ding nil 'isearch-quit)
    (isearch-pop-state))
  (isearch-update))


(defun isearch-yank (chunk)
  ;; Helper for isearch-yank-word and isearch-yank-line
  (let ((word (if (stringp chunk)
		  chunk
		(save-excursion
		  (and (not isearch-forward) isearch-other-end
		       (goto-char isearch-other-end))
		  (buffer-substring
		   (point)
		   (save-excursion
		     (cond
		      ((eq chunk 'word)
		       (forward-word 1))
		      ((eq chunk 'line)
		       (end-of-line)))
		     (point)))))))
    ;; if configured so that typing upper-case characters turns off case
    ;; folding, then downcase the string so that yanking an upper-case
    ;; word doesn't mess with case-foldedness.
    (if (and search-caps-disable-folding isearch-case-fold-search)
	(setq word (downcase word)))
    (if isearch-regexp (setq word (regexp-quote word)))
    (setq isearch-string (concat isearch-string word)
	  isearch-message
	  (concat isearch-message
		  (mapconcat 'isearch-text-char-description
			     word ""))
	  ;; Don't move cursor in reverse search.
	  isearch-yank-flag t))
  (isearch-search-and-update))


(defun isearch-yank-word ()
  "Pull next word from buffer into search string."
  (interactive)
  (isearch-yank 'word))

(defun isearch-yank-line ()
  "Pull rest of line from buffer into search string."
  (interactive)
  (isearch-yank 'line))

(defun isearch-yank-x-selection ()
  "Pull the current X selection into the search string."
  (interactive)
  (isearch-yank (x-get-selection)))

(defun isearch-search-and-update ()
  ;; Do the search and update the display.
  (if (and (not isearch-success)
	   ;; unsuccessful regexp search may become
	   ;;  successful by addition of characters which
	   ;;  make isearch-string valid
	   (not isearch-regexp))
      nil
    ;; In reverse search, adding stuff at
    ;; the end may cause zero or many more chars to be
    ;; matched, in the string following point.
    ;; Allow all those possibilities without moving point as
    ;; long as the match does not extend past search origin.
    (if (and (not isearch-forward) (not isearch-adjusted)
	     (condition-case ()
		 (looking-at (if isearch-regexp isearch-string
			       (regexp-quote isearch-string)))
	       (error nil))
	       (or isearch-yank-flag
		   (<= (match-end 0) 
		       (min isearch-opoint isearch-barrier))))
	(setq isearch-success t 
	      isearch-invalid-regexp nil
	      isearch-other-end (match-end 0))
      ;; Not regexp, not reverse, or no match at point.
      (if (and isearch-other-end (not isearch-adjusted))
	  (goto-char (if isearch-forward isearch-other-end
		       (min isearch-opoint 
			    isearch-barrier 
			    (1+ isearch-other-end)))))
      (isearch-search)
      ))
  (isearch-push-state)
  (if isearch-op-fun (funcall isearch-op-fun))
  (isearch-update))


;; *, ?, and | chars can make a regexp more liberal.
;; They can make a regexp match sooner
;; or make it succeed instead of failing.
;; So go back to place last successful search started
;; or to the last ^S/^R (barrier), whichever is nearer.

(defun isearch-*-char ()
  "Handle * and ? specially in regexps."
  (interactive)
  (if isearch-regexp 

      (progn
	(setq isearch-adjusted t)
	(let ((cs (nth (if isearch-forward
			   5		; isearch-other-end
			 2)		; saved (point)
		       (car (cdr isearch-cmds)))))
	  ;; (car isearch-cmds) is after last search;
	  ;; (car (cdr isearch-cmds)) is from before it.
	  (setq cs (or cs isearch-barrier))
	  (goto-char
	   (if isearch-forward
	       (max cs isearch-barrier)
	     (min cs isearch-barrier))))))
  (isearch-process-search-char last-command-event))
  


(defun isearch-|-char ()
  "If in regexp search, jump to the barrier."
  (interactive)
  (if isearch-regexp
      (progn
	(setq isearch-adjusted t)
	(goto-char isearch-barrier)))
  (isearch-process-search-char last-command-event))

(defun isearch-quote-char ()
  "Quote special characters for incremental search."
  (interactive)
  (isearch-process-search-char (read-quoted-char (isearch-message t))))


(defun isearch-return-char ()
  "Convert return into newline for incremental search.
Obsolete."
  (interactive)
  (isearch-process-search-char ?\n))


(defun isearch-printing-char ()
  "Any other printing character => add it to the search string and search."
  (interactive)
  (isearch-process-search-char last-command-event))


(defun isearch-whitespace-chars ()
  "Match all whitespace chars, if in regexp mode."
  (interactive)
  (if (and isearch-regexp search-whitespace-regexp)
      (isearch-process-search-string search-whitespace-regexp " ")
    (beep)
    (isearch-process-search-char ?\ )
;    (if isearch-word
;	nil
;      (setq isearch-word t)
;      (goto-char isearch-other-end)
;      (isearch-process-search-char ?\ ))
    ))

(defun isearch-process-search-char (char)
  ;; Append the char to the search string, update the message and re-search.
  (isearch-process-search-string (isearch-char-to-string char) 
				 (isearch-text-char-description char)))

(defun isearch-process-search-string (string message)
  (setq isearch-string (concat isearch-string string)
	isearch-message (concat isearch-message message))
  (isearch-search-and-update))


;;===========================================================
;; Search Ring

(defvar search-ring-update nil
  "*Non-nil if advancing or retreating in the search ring should cause search.
Default nil means edit the string from the search ring first.")
  
(defun isearch-ring-adjust1 (advance)
  ;; Helper for isearch-ring-adjust
  (let* ((ring (if isearch-regexp regexp-search-ring search-ring))
	 (length (length ring))
	 (yank-pointer-name (if isearch-regexp
				'regexp-search-ring-yank-pointer
			      'search-ring-yank-pointer))
	 (yank-pointer (eval yank-pointer-name)))
    (if (zerop length)
	()
      (set yank-pointer-name
	   (setq yank-pointer
		 (nthcdr (% (+ (- length (length yank-pointer))
			       (if advance (1- length) 1))
			    length) ring)))
      (setq isearch-string (car yank-pointer)
	    isearch-message (mapconcat 'isearch-text-char-description
				       isearch-string "")))))

(defun isearch-ring-adjust (advance)
  ;; Helper for isearch-ring-advance and isearch-ring-retreat
  (if (cdr isearch-cmds)  ;; is there more than one thing on stack?
      (isearch-pop-state))
  (isearch-ring-adjust1 advance)
  (isearch-push-state)
  (if search-ring-update
      (progn
	(isearch-search)
	(isearch-update))
    (isearch-edit-string)
    ))

(defun isearch-ring-advance ()
  "Advance to the next search string in the ring."
  ;; This could be more general to handle a prefix arg, but who would use it.
  (interactive)
  (isearch-ring-adjust 'advance))

(defun isearch-ring-retreat ()
  "Retreat to the previous search string in the ring."
  (interactive)
  (isearch-ring-adjust nil))

(defun isearch-ring-adjust-edit (advance)
  "Use the next or previous search string in the ring while in minibuffer."
  (isearch-ring-adjust1 advance)
  (erase-buffer)
  (insert isearch-string))

(defun isearch-ring-advance-edit ()
  (interactive)
  (isearch-ring-adjust-edit 'advance))

(defun isearch-ring-retreat-edit ()
  "Retreat to the previous search string in the ring while in the minibuffer."
  (interactive)
  (isearch-ring-adjust-edit nil))


(defun isearch-complete1 ()
  ;; Helper for isearch-complete and isearch-complete-edit
  ;; Return t if completion OK, 
  (let* ((ring (if isearch-regexp regexp-search-ring search-ring))
         (alist (mapcar (function (lambda (string) (list string))) ring))
         (completion-ignore-case case-fold-search)
         (completion (try-completion isearch-string alist))
	 )
    (cond
     ((eq completion t)
      ;; isearch-string stays the same
      t)
     ((or completion ; not nil, must be a string
	  (= 0 (length isearch-string))) ; shouldnt have to say this
      (if (equal completion isearch-string)  ;; no extension?
	  (if completion-auto-help
	      (with-output-to-temp-buffer "*Isearch completions*"
		(display-completion-list 
		 (all-completions isearch-string alist))))
	(setq isearch-string completion))
      t)
     (t
      (temp-minibuffer-message "No completion")
      nil))))

(defun isearch-complete ()
  "Complete the search string from the strings on the search ring.
The completed string is then editable in the minibuffer.
If there is no completion possible, say so and continue searching."
  (interactive)
  (if (isearch-complete1)
      (isearch-edit-string)
    ;; else
    (sit-for 1)
    (isearch-update)))

(defun isearch-complete-edit ()
  "Same as `isearch-complete' except in the minibuffer."
  (interactive)
  (setq isearch-string (buffer-string))
  (if (isearch-complete1)
      (progn
	(erase-buffer)
	(insert isearch-string))))


;;;==============================================================
;; The search status stack (and isearch window-local variables, not used).

(defun isearch-top-state ()
;;  (fetch-window-local-variables)
  (let ((cmd (car isearch-cmds)))
    (setq isearch-string (car cmd)
	  isearch-message (car (cdr cmd))
	  isearch-success (nth 3 cmd)
	  isearch-forward (nth 4 cmd)
	  isearch-other-end (nth 5 cmd)
	  isearch-invalid-regexp (nth 6 cmd)
	  isearch-wrapped (nth 7 cmd)
	  isearch-barrier (nth 8 cmd))
    (goto-char (car (cdr (cdr cmd))))))

(defun isearch-pop-state ()
;;  (fetch-window-local-variables)
  (setq isearch-cmds (cdr isearch-cmds))
  (isearch-top-state)
  )

(defun isearch-push-state ()
  (setq isearch-cmds 
	(cons (list isearch-string isearch-message (point)
		    isearch-success isearch-forward isearch-other-end 
		    isearch-invalid-regexp isearch-wrapped isearch-barrier)
	      isearch-cmds)))


;;;==================================================================
;; Message string

(defun isearch-message (&optional c-q-hack ellipsis)
  ;; Generate and print the message string.
  (let ((cursor-in-echo-area ellipsis)
	(m (concat
	    (isearch-message-prefix c-q-hack)
	    isearch-message
	    (isearch-message-suffix c-q-hack)
	    )))
    (if c-q-hack m (message "%s" m))))

(defun isearch-message-prefix (&optional c-q-hack nonincremental)
  ;; If about to search, and previous search regexp was invalid,
  ;; check that it still is.  If it is valid now,
  ;; let the message we display while searching say that it is valid.
  (and isearch-invalid-regexp
       (condition-case ()
	   (progn (re-search-forward isearch-string (point) t)
		  (setq isearch-invalid-regexp nil))
	 (error nil)))
  (let ((m (concat (if isearch-success "" "failing ")
		   (if isearch-wrapped "wrapped ")
		   (if isearch-word "word " "")
		   (if isearch-regexp "regexp " "")
		   (if nonincremental "search" "I-search")
		   (if isearch-forward ": " " backward: ")
		   )))
    (aset m 0 (upcase (aref m 0)))
    m))


(defun isearch-message-suffix (&optional c-q-hack)
  (concat (if c-q-hack "^Q" "")
	  (if isearch-invalid-regexp
	      (concat " [" isearch-invalid-regexp "]")
	    "")))


;;;========================================================
;;; Exiting

(put 'isearch-printing-char			'isearch-command t)
(put 'isearch-return-char			'isearch-command t)
(put 'isearch-repeat-forward			'isearch-command t)
(put 'isearch-repeat-backward			'isearch-command t)
(put 'isearch-delete-char			'isearch-command t)
(put 'isearch-abort				'isearch-command t)
(put 'isearch-quote-char			'isearch-command t)
(put 'isearch-exit				'isearch-command t)
(put 'isearch-printing-char			'isearch-command t)
(put 'isearch-printing-char			'isearch-command t)
(put 'isearch-yank-word				'isearch-command t)
(put 'isearch-yank-line				'isearch-command t)
(put 'isearch-*-char				'isearch-command t)
(put 'isearch-*-char				'isearch-command t)
(put 'isearch-|-char				'isearch-command t)
(put 'isearch-toggle-regexp			'isearch-command t)
(put 'isearch-edit-string			'isearch-command t)
(put 'isearch-mode-help				'isearch-command t)
(put 'isearch-ring-advance			'isearch-command t)
(put 'isearch-ring-retreat			'isearch-command t)
(put 'isearch-ring-advance-edit			'isearch-command t)
(put 'isearch-ring-retreat-edit			'isearch-command t)
(put 'isearch-whitespace-chars			'isearch-command t)
(put 'isearch-complete				'isearch-command t)
(put 'isearch-complete-edit			'isearch-command t)
(put 'isearch-edit-string			'isearch-command t)
(put 'isearch-toggle-regexp			'isearch-command t)
(put 'isearch-forward-exit-minibuffer		'isearch-command t)
(put 'isearch-reverse-exit-minibuffer		'isearch-command t)
(put 'isearch-nonincremental-exit-minibuffer	'isearch-command t)
(put 'isearch-yank-x-selection			'isearch-command t)

(defun isearch-pre-command-hook ()
  ;;
  ;; For use as the value of `pre-command-hook' when isearch-mode is active.
  ;; If the command about to be executed is not one of the isearch commands,
  ;; then isearch-mode is turned off before that command is executed.
  ;;
  ;; If the command about to be executed is self-insert-command, or is a
  ;; keyboard macro of a single key sequence which is bound to self-insert-
  ;; command, then we add those chars to the search ring instead of inserting
  ;; them in the buffer.  In this way, the set of self-searching characters
  ;; need not be exhaustively enumerated, but is derived from other maps.
  ;;
  (isearch-maybe-frob-keyboard-macros)
  (if (and (symbolp this-command)
	   (get (or this-command 'undefined) 'isearch-command))
      nil
    (isearch-done)))

(defun isearch-maybe-frob-keyboard-macros ()
  ;;
  ;; If the command about to be executed is `self-insert-command' then change
  ;; the command to `isearch-printing-char' instead, meaning add the last-
  ;; typed character to the search string.
  ;;
  ;; If `this-command' is a string or a vector (that is, a keyboard macro)
  ;; and it contains only one command, which is bound to self-insert-command,
  ;; then do the same thing as for self-inserting commands: arrange for that
  ;; character to be added to the search string.  If we didn't do this, then
  ;; typing a compose sequence (a la x-compose.el) would terminate the search
  ;; and insert the character, instead of searching for that character.
  ;;
  (cond ((eq this-command 'self-insert-command)
	 (setq this-command 'isearch-printing-char))
	((and (stringp this-command)
	      (eq (key-binding this-command) 'self-insert-command))
	 (setq last-command-char (aref this-command 0)
	       last-command-event (character-to-event last-command-char)
	       this-command 'isearch-printing-char))
	((and (vectorp this-command)
	      (eq (key-binding this-command) 'self-insert-command))
	 (let* ((desc (aref this-command 0))
		(code (cond ((integerp desc) desc)
			    ((symbolp desc) (get desc character-set-property))
			    ((consp desc)
			     (and (null (cdr desc))
				  (get (car desc) character-set-property)))
			    (t nil))))
	   (if code
	       (setq last-command-char code
		     last-command-event (character-to-event last-command-char)
		     this-command 'isearch-printing-char))))
	))


;;;========================================================
;;; Highlighting

(defvar isearch-highlight t
  "*Whether isearch and query-replace should highlight the text which 
currently matches the search-string.")

(defvar isearch-extent nil)

;; this face is initialized by x-faces.el since isearch is preloaded.
(make-face 'isearch)

(defun isearch-highlight (begin end)
  (if (null isearch-highlight)
      nil
    (if (and (extentp isearch-extent)
	     (eq (extent-buffer isearch-extent) (current-buffer)))
	(set-extent-endpoints isearch-extent begin end)
      (if (and (extentp isearch-extent)
	       (bufferp (extent-buffer isearch-extent))
	       (buffer-name (extent-buffer isearch-extent)))
	  (delete-extent isearch-extent))
      (setq isearch-extent (make-extent begin end (current-buffer))))
    ;; make the isearch extent always take prescedence over any mouse-
    ;; highlighted extents we may be passing through, since isearch, being
    ;; modal, is more interesting (there's nothing they could do with a
    ;; mouse-highlighted extent while in the midst of a search anyway.)
    (set-extent-priority isearch-extent (1+ mouse-highlight-priority))
    (set-extent-face isearch-extent 'isearch)))

(defun isearch-dehighlight (totally)
  (if (and isearch-highlight isearch-extent)
      (if totally
	  (let ((inhibit-quit t))
	    (if (and (extentp isearch-extent)
		     (bufferp (extent-buffer isearch-extent))
		     (buffer-name (extent-buffer isearch-extent)))
		(delete-extent isearch-extent))
	    (setq isearch-extent nil))
	(if (and (extentp isearch-extent)
		 (bufferp (extent-buffer isearch-extent))
		 (buffer-name (extent-buffer isearch-extent)))
	    (set-extent-face isearch-extent 'default)
	  (isearch-dehighlight t)))))


;;;========================================================
;;; Searching

(defun isearch-search ()
  ;; Do the search with the current search string.
  (isearch-message nil t)
  (if (and case-fold-search search-caps-disable-folding)
      (setq isearch-case-fold-search (isearch-no-upper-case-p isearch-string)))

  (setq isearch-mode (if case-fold-search
                         (if isearch-case-fold-search
                             " Isearch"  ;As God Intended Mode
                             " ISeARch") ;Warn about evil case via StuDLYcAps.
		         "Isearch"
;		         (if isearch-case-fold-search
;                            " isearch"    ;Presumably case-sensitive losers
;                                          ;will notice this 1-char difference.
;                            " Isearch")   ;Weenie mode.
			 ))
  (condition-case lossage
      (let ((inhibit-quit nil)
	    (case-fold-search isearch-case-fold-search))
	(if isearch-regexp (setq isearch-invalid-regexp nil))
	(setq isearch-success
	      (funcall
	       (cond (isearch-word
		      (if isearch-forward
			  'word-search-forward 'word-search-backward))
		     (isearch-regexp
		      (if isearch-forward
			  're-search-forward 're-search-backward))
		     (t
		      (if isearch-forward 'search-forward 'search-backward)))
	       isearch-string nil t))
	(if isearch-success
	    (setq isearch-other-end
		  (if isearch-forward (match-beginning 0) (match-end 0)))))

    (quit (setq unread-command-event (character-to-event interrupt-char))
	  (setq isearch-success nil))

    (invalid-regexp 
     (setq isearch-invalid-regexp (car (cdr lossage)))
     (if (string-match
	  "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
	  isearch-invalid-regexp)
	 (setq isearch-invalid-regexp "incomplete input"))))

  (if isearch-success
      nil

    ;; If we're being run inside a keyboard macro, then the call to
    ;; ding will signal an error (to terminate the macro.)  We must
    ;; turn off isearch-mode first, so that we aren't still in isearch
    ;; mode after the macro exits.  Note that isearch-recursive-edit
    ;; must not be true if a keyboard macro is executing.
    (if (and executing-macro (not defining-kbd-macro))
	(progn
	  (isearch-done)
	  (ding nil 'isearch-failed)))

    ;; Ding if failed this time after succeeding last time.
    (and (nth 3 (car isearch-cmds))
	 (ding nil 'isearch-failed))
    (goto-char (nth 2 (car isearch-cmds)))))

;;;=================================================
;; This is called from incremental-search
;; if the first input character is the exit character.

;; We store the search string in `isearch-string'
;; which has been bound already by `isearch-search'
;; so that, when we exit, it is copied into `search-last-string'.

;(defun nonincremental-search (forward regexp)
;  ;; This may be broken.  Anyway, it is replaced by the isearch-edit-string.
;  ;; Missing features: word search option, command history.
;  (setq isearch-forward forward
;	isearch-regexp regexp)
;  (let (char function
;	inhibit-quit
;	(cursor-in-echo-area t))
;    ;; Prompt assuming not word search,
;    (setq isearch-message 
;	  (if isearch-regexp 
;	      (if isearch-forward "Regexp search: "
;		"Regexp search backward: ")
;	    (if isearch-forward "Search: " "Search backward: ")))
;    (message "%s" isearch-message)
;    ;; Read 1 char and switch to word search if it is ^W.
;    (setq char (read-char))
;    (if (eq char search-yank-word-char)
;	(setq isearch-message (if isearch-forward "Word search: " 
;				"Word search backward: "))
;      ;; Otherwise let that 1 char be part of the search string.
;      (setq unread-command-event (character-to-event char))
;      )
;    (setq function
;	  (if (eq char search-yank-word-char)
;	      (if isearch-forward 'word-search-forward 'word-search-backward)
;	    (if isearch-regexp
;		(if isearch-forward 're-search-forward 're-search-backward)
;	      (if isearch-forward 'search-forward 'search-backward))))
;    ;; Read the search string with corrected prompt.
;    (setq isearch-string (read-string isearch-message isearch-string))
;    ;; Empty means use default.
;    (if (= 0 (length isearch-string))
;	(setq isearch-string search-last-string)
;      ;; Set last search string now so it is set even if we fail.
;      (setq search-last-string isearch-string))
;    ;; Since we used the minibuffer, we should be available for redo.
;    (setq command-history 
;	  (cons (list function isearch-string) command-history))
;    ;; Go ahead and search.
;    (if search-caps-disable-folding
;	(setq isearch-case-fold-search 
;	      (isearch-no-upper-case-p isearch-string)))
;    (let ((case-fold-search isearch-case-fold-search))
;      (funcall function isearch-string))))


(defun isearch-no-upper-case-p (string)
  "Return t if there are no upper case chars in string.
But upper case chars preceeded by \\ do not count since they
have special meaning in a regexp."
  ;; this incorrectly returns t for "\\\\A"
  (let ((case-fold-search nil))
    (not (string-match "\\(^\\|[^\\]\\)[A-Z]" string))))
