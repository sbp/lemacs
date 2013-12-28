;;; minibuf.el
;; Copyright (C) 1992-1993 Free Software Foundation, Inc.
 
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

;; Written by Richard Mlynarik 2-Oct-92

(defvar insert-default-directory t
 "*Non-nil means when reading a filename start with default dir in minibuffer."
 )

(defvar minibuffer-completion-table nil
  "Alist or obarray used for completion in the minibuffer.
This becomes the ALIST argument to `try-completion' and `all-completions'.

The value may alternatively be a function, which is given three arguments:
  STRING, the current buffer contents;
  PREDICATE, the predicate for filtering possible matches;
  CODE, which says what kind of things to do.
CODE can be nil, t or `lambda'.
nil means to return the best completion of STRING, nil if there is none,
  or t if it is was already a unique completion.
t means to return a list of all possible completions of STRING.
`lambda' means to return t if STRING is a valid completion as it stands.")

(defvar minibuffer-completion-predicate nil
  "Within call to `completing-read', this holds the PREDICATE argument.")

(defvar minibuffer-completion-confirm nil
  "Non-nil => demand confirmation of completion before exiting minibuffer.")

(defvar minibuffer-confirm-incomplete nil
  "If true, then in contexts where completing-read allows answers which
are not valid completions, an extra RET must be typed to confirm the
response.  This is helpful for catching typos, etc.")

(defvar completion-auto-help t
  "*Non-nil means automatically provide help for invalid completion input.")

(defvar enable-recursive-minibuffers nil
  "*Non-nil means to allow minibuffer commands while in the minibuffer.
More precisely, this variable makes a difference when the minibuffer window
is the selected window.  If you are in some other window, minibuffer commands
are allowed even if a minibuffer is active.")


(defvar minibuffer-help-form nil
  "Value that `help-form' takes on inside the minibuffer.")

(defvar minibuffer-local-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'minibuffer-local-map)
    map)
  "Default keymap to use when reading from the minibuffer.")

(defvar minibuffer-local-completion-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'minibuffer-local-completion-map)
    (set-keymap-parent map minibuffer-local-map)
    map)
  "Local keymap for minibuffer input with completion.")

(defvar minibuffer-local-must-match-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'minibuffer-must-match-map)
    (set-keymap-parent map minibuffer-local-completion-map)
    map)
  "Local keymap for minibuffer input with completion, for exact match.")

(define-key minibuffer-local-map "\C-g" 'abort-recursive-edit)
(define-key minibuffer-local-map "\r" 'exit-minibuffer)
(define-key minibuffer-local-map "\n" 'exit-minibuffer)

;; Historical crock.  Unused by anything but user code, if even that
;(defvar minibuffer-local-ns-map
;  (let ((map (make-sparse-keymap)))
;    (set-keymap-name map 'minibuffer-local-ns-map)
;    (set-keymap-parent map minibuffer-local-map)
;    map)
;  "Local keymap for the minibuffer when spaces are not allowed.")
;(define-key minibuffer-local-ns-map [space] 'exit-minibuffer)
;(define-key minibuffer-local-ns-map [tab] 'exit-minibuffer)
;(define-key minibuffer-local-ns-map [?\?] 'self-insert-and-exit)

(define-key minibuffer-local-completion-map "\t" 'minibuffer-complete)
(define-key minibuffer-local-completion-map " " 'minibuffer-complete-word)
(define-key minibuffer-local-completion-map "?" 'minibuffer-completion-help)
(define-key minibuffer-local-must-match-map "\r" 'minibuffer-complete-and-exit)
(define-key minibuffer-local-must-match-map "\n" 'minibuffer-complete-and-exit)

(define-key minibuffer-local-map "\M-n" 'next-history-element)
(define-key minibuffer-local-map "\M-p" 'previous-history-element)
(define-key minibuffer-local-map [next] 'next-history-element)
(define-key minibuffer-local-map [prior] 'previous-history-element)
(define-key minibuffer-local-map "\M-r" 'previous-matching-history-element)
(define-key minibuffer-local-map "\M-s" 'next-matching-history-element)

(defvar read-expression-map (let ((map (make-sparse-keymap)))
                              (set-keymap-parent map minibuffer-local-map)
			      (set-keymap-name map 'read-expression-map)
                              map)
  "Minibuffer keymap used for reading Lisp expressions.")

(define-key read-expression-map "\M-\t" 'lisp-complete-symbol)

(defvar read-shell-command-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (set-keymap-name map 'read-shell-command-map)
    (define-key map "\t" 'comint-dynamic-complete)
    (define-key map "\M-\t" 'comint-dynamic-complete)
    (define-key map "\M-?" 'comint-dynamic-list-completions)
    map)
  "Minibuffer keymap used by shell-command and related commands.")

;;;; Guts of minibuffer invocation

;;>>> The only things remaining in C are
;; "Vminibuf_prompt" and the display junk
;;  "minibuf_prompt_width" and "minibuf_prompt_pix_width"
;; Also "active_screen", though I suspect I could already
;;   hack that in Lisp if I could make any sense of the
;;   complete mess of screen/frame code in Emacs.
;; Vminibuf_prompt could easily be made Lisp-bindable.
;;  I suspect that minibuf_prompt*_width are actually recomputed
;;  by redisplay as needed -- or could be arranged to be so --
;;  and that there could be need for read-minibuffer-internal to
;;  save and restore them.
;;>>> The only other thing which read-from-minibuffer-internal does
;;  which we can't presently do in Lisp is move the screen cursor
;;  to the start of the minibuffer line as it returns.  This is
;;  a rather nice touch and should be preserved -- probably by
;;  providing some Lisp-level mechanism (extension to cursor-in-echo-area ?)
;;  to effect it.


;; Like reset_buffer in buffer.c
;;  (Except that kill-all-local-variables doesn't nuke 'permanent-local
;;   variables -- we preserve them, reset_buffer doesn't.)
(defun reset-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    ;(if (fboundp 'unlock-buffer) (unlock-buffer))
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;(setq default-directory nil)
    (setq buffer-file-name nil)
    (setq buffer-file-truename nil)
    (set-buffer-modified-p nil)
    (setq buffer-backed-up nil)
    (setq buffer-auto-save-file-name nil)
    (set-buffer-dedicated-screen buffer nil)
    buffer))

(defun read-from-minibuffer (prompt &optional initial-contents
                                    keymap
                                    readp
                                    history)
  "Read a string from the minibuffer, prompting with string PROMPT.
If optional second arg INITIAL-CONTENTS is non-nil, it is a string
  to be inserted into the minibuffer before reading input.
  If INITIAL-CONTENTS is (STRING . POSITION), the initial input
  is STRING, but point is placed POSITION characters into the string.
Third arg KEYMAP is a keymap to use whilst reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HIST, if non-nil, specifies a history list
  and optionally the initial position in the list.
  It can be a symbol, which is the history list variable to use,
  or it can be a cons cell (HISTVAR . HISTPOS).
  In that case, HISTVAR is the history list variable to use,
  and HISTPOS is the initial position (the position in the list
  which INITIAL-CONTENTS corresponds to).
  If HIST is t, no history will be recorded.
  Positions are counted starting from 1 at the beginning of the list."
  (if (and (not enable-recursive-minibuffers)
           (> (minibuffer-depth) 0)
           (eq (selected-window) (minibuffer-window)))
      (error "Command attempted to use minibuffer while in minibuffer"))

  ;; catch this error before the poor user has typed something...
  (if history
      (if (symbolp history)
	  (or (boundp history)
	      (error "History list %S is unbound" history))
	(or (boundp (car history))
	    (error "History list %S is unbound" (car history)))))

  (if (noninteractive)
      (progn
        ;; Emacs in -batch mode calls minibuffer: print the prompt.
        (message "%s" prompt)
        ;;>>> force-output

        ;;>>> Should this even be falling though to the code below?
        ;;>>>  How does this stuff work now, anyway?
        ))
  (let* ((dir default-directory)
         (owindow (selected-window))
         (window (minibuffer-window))
         (buffer (if (eq (minibuffer-depth) 0)
                     (window-buffer window)
                     (get-buffer-create (format " *Minibuf-%d"
                                                (minibuffer-depth))))))
    (save-window-excursion
      (set-buffer buffer)
      (reset-buffer buffer)
      (setq default-directory dir)
      (make-local-variable 'print-escape-newlines)
      (setq print-escape-newlines t)
      (make-local-variable 'mode-motion-hook)
      (setq mode-motion-hook 'minibuf-mouse-tracker) ;>>>disgusting
      (set-window-buffer window buffer)
      (select-window window)
      (set-window-hscroll window 0)
      (erase-buffer)
      (buffer-enable-undo buffer)
      (message nil)
      (if initial-contents
          (if (consp initial-contents)
              (progn
                (insert (car initial-contents))
                (goto-char (cdr initial-contents)))
              (insert initial-contents)))
      (use-local-map (or keymap minibuffer-local-map))
      (let ((mouse-grabbed-buffer (current-buffer))
            (current-prefix-arg current-prefix-arg)
            (help-form minibuffer-help-form)
            (minibuffer-history-variable (cond ((not history)
                                                'minibuffer-history)
                                               ((consp history)
                                                (car history))
                                               (t
                                                history)))
            (minibuffer-history-position (cond ((consp history)
                                                (cdr history))
                                               (t
                                                0)))
            (minibuffer-scroll-window owindow))
        (unwind-protect
             (if (eq 't
                     (catch 'exit
                       (if (> (recursion-depth) (minibuffer-depth))
                           (let ((standard-output t)
                                 (standard-input t))
                             (read-minibuffer-internal prompt))
                           (read-minibuffer-internal prompt))))
                 ;; Translate an "abort" (throw 'exit 't)
                 ;;  into a real quit
                 (signal 'quit '())
                 ;; return value
                 (let ((val (progn (set-buffer buffer) (buffer-string)))
		       (list (symbol-value minibuffer-history-variable)))
                   ;; Add the value to the appropriate history list unless
		   ;; it's already the most recent element, or it's only
		   ;; two characters long.
		   (or (eq list t)
		       (null val)
		       (equal val (car list))
		       (and (stringp val) (< (length val) 3))
		       (set minibuffer-history-variable (cons val list)))
                   (if readp
                       (car (read-from-string val))
                       val)))
          ;; stupid display code requires this for some reason
          (set-buffer buffer)
          (buffer-disable-undo buffer)
          (setq buffer-read-only nil)
          (erase-buffer))))))


;;;; Guts of minibuffer completion


;; Used by minibuffer-do-completion
(defvar last-exact-completion)

(defun temp-minibuffer-message (m)
  (let ((savemax (point-max)))
    (save-excursion
      (goto-char (point-max))
      (insert m))
    (let ((inhibit-quit t))
      (sit-for 2)
      (delete-region savemax (point-max))
      ;;  If the user types a ^G while we're in sit-for, then quit-flag 
      ;;  gets set. In this case, we want that ^G to be interpreted 
      ;;  as a normal character, and act just like typeahead.
      (if (and quit-flag (not unread-command-event))
          (setq unread-command-event (character-to-event interrupt-char)
                quit-flag nil)))))


;; Determines whether buffer-string is an exact completion
(defun exact-minibuffer-completion-p (buffer-string)
  (cond ((not minibuffer-completion-table)
         ;; Empty alist
         nil)
        ((vectorp minibuffer-completion-table)
         (let ((tem (intern-soft buffer-string
                                 minibuffer-completion-table)))
           (if (or tem
                   (and (string-equal buffer-string "nil")
                        ;; intern-soft loses for 'nil
                        (catch 'found
                          (mapatoms
                           (function (lambda (s)
                             (if (string-equal
                                  (symbol-name s)
                                  buffer-string)
                                 (throw 'found t))))
                           minibuffer-completion-table)
                          nil)))
               (if minibuffer-completion-predicate
                   (funcall minibuffer-completion-predicate
                            tem)
                   t)
               nil)))
        ((and (consp minibuffer-completion-table)
              ;;>>> Emacs-Lisp truly sucks!
              ;; lambda, autoload, etc
              (not (symbolp (car minibuffer-completion-table))))
         (if (not completion-ignore-case)
             (assoc buffer-string minibuffer-completion-table)
             (let ((s (upcase buffer-string))
                   (tail minibuffer-completion-table)
                   tem)
               (while tail
                 (setq tem (car (car tail)))
                 (if (or (equal tem buffer-string)
                         (equal tem s)
                         (equal (upcase tem) s))
                     (setq s 'win
                           tail nil)    ;exit
                     (setq tail (cdr tail))))
               (eq s 'win))))
        (t
         (funcall minibuffer-completion-table
                  buffer-string
                  minibuffer-completion-predicate
                  'lambda)))
  )

;; 0 'none                 no possible completion
;; 1 'unique               was already an exact and unique completion
;; 3 'exact                was already an exact (but nonunique) completion
;; NOT USED 'completed-exact-unique completed to an exact and completion 
;; 4 'completed-exact      completed to an exact (but nonunique) completion
;; 5 'completed            some completion happened
;; 6 'uncompleted          no completion happened
(defun minibuffer-do-completion-1 (buffer-string completion)
  (cond ((not completion)
         'none)
        ((eq completion t)
         ;; exact and unique match
         'unique)
        (t
         ;; It did find a match.  Do we match some possibility exactly now?
         (let ((completedp (not (string-equal completion buffer-string))))
           (if completedp
               (progn
                 ;; Some completion happened
                 (erase-buffer)
                 (insert completion)
                 (setq buffer-string completion)))
           (if (exact-minibuffer-completion-p buffer-string)
               ;; An exact completion was possible
               (if completedp
;; Since no callers need to know the difference, don't bother
;;  with this (potentially expensive) discrimination.
;;                 (if (eq (try-completion completion
;;                                         minibuffer-completion-table
;;                                         minibuffer-completion-predicate)
;;                         't)
;;                     'completed-exact-unique
                       'completed-exact
;;                     )
                   'exact)
               ;; Not an exact match
               (if completedp
                   'completed
                   'uncompleted))))))


(defun minibuffer-do-completion (buffer-string)
  (let* ((completion (try-completion buffer-string
                                     minibuffer-completion-table
                                     minibuffer-completion-predicate))
         (status (minibuffer-do-completion-1 buffer-string completion))
         (last last-exact-completion))
    (setq last-exact-completion nil)
    (cond ((eq status 'none)
           ;; No completions
           (ding nil 'no-completion)
           (temp-minibuffer-message " [No match]"))
          ((eq status 'unique)
           )
          (t
           ;; It did find a match.  Do we match some possibility exactly now?
           (if (not (string-equal completion buffer-string))
               (progn
                 ;; Some completion happened
                 (erase-buffer)
                 (insert completion)
                 (setq buffer-string completion)))
           (cond ((eq status 'exact)
                  ;; If the last exact completion and this one were
                  ;;  the same, it means we've already given a
                  ;;  "Complete but not unique" message and that the
                  ;;  user's hit TAB again, so now we give help.
                  (setq last-exact-completion completion)
                  (if (equal buffer-string last)
                      (minibuffer-completion-help)))
                 ((eq status 'uncompleted)
                  (if completion-auto-help
                      (minibuffer-completion-help)
                      (temp-minibuffer-message " [Next char not unique]")))
                 (t
                  nil))))
    status))


;;;; completing-read

(defun completing-read (prompt table
                        &optional predicate require-match
                                  initial-contents hist)
  "Read a string in the minibuffer, with completion.
Args: PROMPT, TABLE, PREDICATE, REQUIRE-MATCH, INITIAL-CONTENTS, HIST.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
TABLE is an alist whose elements' cars are strings, or an obarray.
PREDICATE limits completion to a subset of TABLE.
See `try-completion' for more details on completion, TABLE, and PREDICATE.
If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
 the input is (or completes to) an element of TABLE.
 If it is also not t, Return does not exit if it does non-null completion.
If INITIAL-CONTENTS is non-nil, insert it in the minibuffer initially.
  If it is (STRING . POSITION), the initial input
  is STRING, but point is placed POSITION characters into the string.
HIST, if non-nil, specifies a history list
  and optionally the initial position in the list.
  It can be a symbol, which is the history list variable to use,
  or it can be a cons cell (HISTVAR . HISTPOS).
  In that case, HISTVAR is the history list variable to use,
  and HISTPOS is the initial position (the position in the list
  which INITIAL-CONTENTS corresponds to).
  If HIST is t, no history will be recorded.
  Positions are counted starting from 1 at the beginning of the list.
Completion ignores case if the ambient value of
  `completion-ignore-case' is non-nil."
  (let ((minibuffer-completion-table table)
        (minibuffer-completion-predicate predicate)
        (minibuffer-completion-confirm (if (eq require-match 't) nil t))
        (last-exact-completion nil))
    (read-from-minibuffer prompt
                          initial-contents
                          (if (not require-match)
                              minibuffer-local-completion-map
                              minibuffer-local-must-match-map)
                          nil
                          hist)))


;;;; Minibuffer completion commands


(defun minibuffer-complete ()
  "Complete the minibuffer contents as far as possible."
  (interactive)
  (let ((status (minibuffer-do-completion (buffer-string))))
    (if (eq status 'none)
        nil
      (progn
        (cond ((eq status 'unique)
               (temp-minibuffer-message " [Sole completion]"))
              ((eq status 'exact)
               (temp-minibuffer-message " [Complete, but not unique]")))
        t))))


(defun minibuffer-complete-and-exit ()
  "Complete the minibuffer contents, and maybe exit.
Exit if the name is valid with no completion needed.
If name was completed to a valid match,
a repetition of this command will exit."
  (interactive)
  (if (= (point-min) (point-max))
      ;; Crockishly allow user to specify null string
      (throw 'exit nil))
  (let ((buffer-string (buffer-string)))
    ;; Short-cut -- don't call minibuffer-do-completion if we already
    ;;  have an (possibly nonunique) exact completion.
    (if (exact-minibuffer-completion-p buffer-string)
        (throw 'exit nil))
    (let ((status (minibuffer-do-completion buffer-string)))
      (if (or (eq status 'unique)
              (eq status 'exact)
              (if (or (eq status 'completed-exact)
                      (eq status 'completed-exact-unique))
                  (if minibuffer-completion-confirm
                      (progn (temp-minibuffer-message " [Confirm]")
                             nil)
                      t)))
          (throw 'exit nil)))))


(defun self-insert-and-exit ()
  "Terminate minibuffer input."
  (interactive)
  (self-insert-command)
  (throw 'exit nil))

(defun exit-minibuffer ()
  "Terminate this minibuffer argument.
If minibuffer-confirm-incomplete is true, and we are in a completing-read
of some kind, and the contents of the minibuffer is not an existing
completion, requires an additional RET before the minibuffer will be exited
\(assuming that RET was the character that invoked this command:
the character in question must be typed again)."
  (interactive)
  (if (not minibuffer-confirm-incomplete)
      (throw 'exit nil))
  (let ((buffer-string (buffer-string)))
    (if (exact-minibuffer-completion-p buffer-string)
        (throw 'exit nil))
    (let ((completion (if (not minibuffer-completion-table)
                          t
                          (try-completion buffer-string
                                          minibuffer-completion-table
                                          minibuffer-completion-predicate))))
      (if (or (eq completion 't)
              ;; Crockishly allow user to specify null string
              (string-equal buffer-string ""))
          (throw 'exit nil))
      (temp-minibuffer-message (if completion
                                   " [incomplete; confirm]"
                                   " [no completions; confirm]"))
      (let ((event (allocate-event)))
        (let ((inhibit-quit t))
          (next-command-event event)
          (setq quit-flag nil))
        (cond ((equal event last-command-event)
               (throw 'exit nil))
              ((eq interrupt-char (event-to-character event nil))
               (deallocate-event event)
               ;; Minibuffer abort.
               (throw 'exit t)))
        (dispatch-event event)
        (deallocate-event event)))))

;;;; minibuffer-complete-word


;;;>>> I think I have done this correctly; it certainly is simpler
;;;>>>  than what the C code seemed to be trying to do.
(defun minibuffer-complete-word ()
  "Complete the minibuffer contents at most a single word.
After one word is completed as much as possible, a space or hyphen
is added, provided that matches some possible completion."
  (interactive)
  (let* ((buffer-string (buffer-string))
         (completion (try-completion buffer-string
                                     minibuffer-completion-table
                                     minibuffer-completion-predicate))
         (status (minibuffer-do-completion-1 buffer-string completion)))
    (cond ((eq status 'none)
           (ding nil 'no-completion)
           (temp-minibuffer-message " [No match]")
           nil)
          ((eq status 'unique)
           ;; New message, only in this new Lisp code
           (temp-minibuffer-message " [Sole completion]")
           t)
          (t
           (cond ((or (eq status 'uncompleted)
                      (eq status 'exact))
                  (let ((foo (function (lambda (s)
                               (condition-case ()
                                   (if (try-completion
                                        (concat buffer-string s)
                                        minibuffer-completion-table
                                        minibuffer-completion-predicate)
                                       (progn
                                         (goto-char (point-max))
                                         (insert s)
                                         t)
                                       nil)
                                   (error nil)))))
                        (char last-command-char))
                    ;; Try to complete by adding a word-delimiter
                    (or (and (integerp char) (> char 0)
                             (funcall foo (char-to-string char)))
                        (and (not (eq char ?\ ))
                             (funcall foo " "))
                        (and (not (eq char ?\-))
                             (funcall foo "-"))
                        (progn
                          (if completion-auto-help 
                              (minibuffer-completion-help)
                              ;; New message, only in this new Lisp code
                              (temp-minibuffer-message
                               (if (eq status 'exact)
                                   " [Complete, but not unique]"
                                   " [Ambiguous]")))
                          nil))))
                 (t
                  (erase-buffer)
                  (insert completion)
                  ;; First word-break in stuff found by completion
                  (goto-char (point-min))
                  (let ((len (length buffer-string))
                        n)
                    (if (and (< len (length completion))
                             (catch 'match
                               (setq n 0)
                               (while (< n len)
                                 (if (char-equal
                                       (upcase (aref buffer-string n))
                                       (upcase (aref completion n)))
                                     (setq n (1+ n))
                                     (throw 'match nil)))
                               t)
                             (progn
                               (goto-char (point-min))
                               (forward-char len)
                               (re-search-forward "\\W" nil t)))
                        (delete-region (point) (point-max))
                        (goto-char (point-max))))
                  t))))))

;;;; Completion help

(defun display-completion-list (completions)
  "Display the list of completions, COMPLETIONS, using `standard-output'.
Each element may be just a symbol or string
or may be a list of two strings to be printed as if concatenated."
  (let ((old-buffer (current-buffer))
        (bufferp (bufferp standard-output)))
    (if bufferp
        (set-buffer standard-output))
    (if (null completions)
        (princ "There are no possible completions of what you have typed.")
      (let ((win-width (if bufferp
                           ;; This needs fixing for the case of windows 
                           ;; that aren't the same width s the screen.
                           ;; Sadly, the window it will appear in is not known
                           ;; until after the text has been made.
                           (screen-width (selected-screen))
                           80)))
        (let ((count 0)
              (max-width 0))
          ;; Find longest completion
          (let ((tail completions))
            (while tail
              (let* ((elt (car tail))
                     (len (cond ((stringp elt)
                                 (length elt))
                                ((and (consp elt)
                                      (stringp (car elt))
                                      (stringp (car (cdr elt))))
                                 (+ (length (car elt))
                                    (length (car (cdr elt)))))
                                (t
                                 (signal 'wrong-type-argument
                                         (list 'stringp elt))))))
                (if (> len max-width)
                    (setq max-width len))
                (setq count (1+ count)
                      tail (cdr tail)))))
        
          (setq max-width (+ 2 max-width)) ; at least two chars between cols
          (let ((rows (let ((cols (min (/ win-width max-width) count)))
                        (if (<= cols 1)
                            count
                          (progn
                            ;; re-space the columns
                            (setq max-width (/ win-width cols))
                            (if (/= (% count cols) 0) ; want ceiling...
                                (1+ (/ count cols))
                                (/ count cols)))))))
            (princ "Possible completions are:")
            (let ((tail completions)
                  (r 0))
              (while (< r rows)
                (terpri)
                (let ((indent 0)
                      (column 0)
                      (tail2 tail))
                  (while tail2
                    (let ((elt (car tail2)))
                      (if (/= indent 0)
                          (if bufferp
                              (indent-to indent 1)
                              (while (progn (write-char ?\ )
                                            (setq column (1+ column))
                                            (< column indent)))))
                      (setq indent (+ indent max-width))
                      (if (consp elt)
                          (progn
                            (princ (car elt))
                            (princ (car (cdr elt)))
                            (or bufferp
                                (setq column (+ column
                                                (length (car elt))
                                                (length (car (cdr elt)))))))
                          (progn
                            (princ elt)
                            (or bufferp
                                (setq column (+ column (length elt)))))))
                    (setq tail2 (nthcdr rows tail2)))
                  (setq tail (cdr tail)
                        r (1+ r)))))))))
    (if bufferp
        (set-buffer old-buffer))))

(defun minibuffer-completion-help ()
  "Display a list of possible completions of the current minibuffer contents."
  (interactive)
  (message "Making completion list...")
  (let ((completions (all-completions (buffer-string)
                                      minibuffer-completion-table
                                      minibuffer-completion-predicate)))
    (if (null completions)
        (progn
          (ding nil 'no-completion)
          (temp-minibuffer-message " [No completions]")
          (message nil))
        (with-output-to-temp-buffer "*Completions*"
          (message nil)
          (display-completion-list (sort completions 
                                         (function string-lessp)))))))

;;;; Minibuffer History

(defvar minibuffer-history '()
  "Default minibuffer history list.
This is used for all minibuffer input except when an alternate history
list is specified.")

;; Some other history lists:
;;
(defvar minibuffer-history-search-history '())
(defvar minibuffer-sexp-history '())
(defvar minibuffer-command-history '())
(defvar minibuffer-function-history '())
(defvar minibuffer-variable-history '())
(defvar minibuffer-buffer-history '())
(defvar minibuffer-shell-command-history '())
(defvar minibuffer-file-name-history '())

(defvar minibuffer-history-sexp-flag nil ;weird RMS Emacs kludge
  "Non-nil when doing history operations on `command-history'.
More generally, indicates that the history list being acted on
contains expressions rather than strings.")

(defvar minibuffer-history-variable 'minibuffer-history
  "History list symbol to add minibuffer values to.
Each minibuffer output is added with
  (set minibuffer-history-variable
       (cons STRING (symbol-value minibuffer-history-variable)))")
(defvar minibuffer-history-position)

(defun previous-matching-history-element (regexp n)
  "Find the previous history element that matches REGEXP.
\(Previous history elements refer to earlier actions.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive
   (let ((enable-recursive-minibuffers t)
	 (minibuffer-history-sexp-flag nil))
     (if (eq t (symbol-value minibuffer-history-variable))
	 (error "history is not being recorded in this context"))
     (list (read-from-minibuffer "Previous element matching (regexp): "
				 nil
				 minibuffer-local-map
				 nil
				 'minibuffer-history-search-history)
	   (prefix-numeric-value current-prefix-arg))))
  (let ((history (symbol-value minibuffer-history-variable))
	prevpos
	(pos minibuffer-history-position))
    (if (eq history t)
	(error "history is not being recorded in this context"))
    (while (/= n 0)
      (setq prevpos pos)
      (setq pos (min (max 1 (+ pos (if (< n 0) -1 1))) (length history)))
      (if (= pos prevpos)
	  (error (if (= pos 1)
		     "No later matching history item"
		   "No earlier matching history item")))
      (if (string-match regexp
			(if minibuffer-history-sexp-flag
			    (prin1-to-string (nth (1- pos) history))
                            (nth (1- pos) history)))
	  (setq n (+ n (if (< n 0) 1 -1)))))
    (setq minibuffer-history-position pos)
    (erase-buffer)
    (let ((elt (nth (1- pos) history)))
      (insert (if minibuffer-history-sexp-flag
		  (prin1-to-string elt)
                  elt)))
      (goto-char (point-min)))
  (if (or (eq (car (car command-history)) 'previous-matching-history-element)
	  (eq (car (car command-history)) 'next-matching-history-element))
      (setq command-history (cdr command-history))))

(defun next-matching-history-element (regexp n)
  "Find the next history element that matches REGEXP.
\(The next history element refers to a more recent action.)
With prefix argument N, search for Nth next match.
If N is negative, find the previous or Nth previous match."
  (interactive
   (let ((enable-recursive-minibuffers t)
	 (minibuffer-history-sexp-flag nil))
     (if (eq t (symbol-value minibuffer-history-variable))
	 (error "history is not being recorded in this context"))
     (list (read-from-minibuffer "Next element matching (regexp): "
				 nil
				 minibuffer-local-map
				 nil
				 'minibuffer-history-search-history)
	   (prefix-numeric-value current-prefix-arg))))
  (previous-matching-history-element regexp (- n)))

(defun next-history-element (n)
  "Insert the next element of the minibuffer history into the minibuffer."
  (interactive "p")
  (if (eq t (symbol-value minibuffer-history-variable))
      (error "history is not being recorded in this context"))
  (let ((narg (min (max 1 (- minibuffer-history-position n))
		   (length (symbol-value minibuffer-history-variable)))))
    (if (= minibuffer-history-position narg)
	(error (format "No %s item in %s"
		       (if (= minibuffer-history-position 1)
			   "following" "preceding")
		       minibuffer-history-variable))
      (erase-buffer)
      (setq minibuffer-history-position narg)
      (let ((elt (nth (1- minibuffer-history-position)
		      (symbol-value minibuffer-history-variable))))
	(insert
	 (if minibuffer-history-sexp-flag
	     (condition-case ()
		 (let ((print-readably t)) (prin1-to-string elt))
	       (error (prin1-to-string elt)))
             elt)))
      (goto-char (point-max)))))

(defun previous-history-element (n)
  "Inserts the previous element of the minibuffer history into the minibuffer."
  (interactive "p")
  (next-history-element (- n)))


;;;; reading various things from a minibuffer

(defun read-minibuffer (prompt &optional initial-contents hist)
  "Return a Lisp object read using the minibuffer.
Prompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS
is a string to insert in the minibuffer before reading.
Third arg HIST, if non-nil, specifies a history list."
  (read-from-minibuffer prompt
                        initial-contents
                        minibuffer-local-map
                        t
			(or hist 'minibuffer-sexp-history)))

(defun read-string (prompt &optional initial-contents hist)
  "Return a string from the minibuffer, prompting with string PROMPT.
If non-nil, optional second arg INITIAL-CONTENTS is a string to insert
in the minibuffer before reading.
Third arg HIST, if non-nil, specifies a history list."
  (read-from-minibuffer prompt
                        initial-contents
                        minibuffer-local-map
                        nil hist))

(defun eval-minibuffer (prompt &optional initial-contents hist)
  "Return value of Lisp expression read using the minibuffer.
Prompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS
is a string to insert in the minibuffer before reading.
Third arg HIST, if non-nil, specifies a history list."
  (eval (read-minibuffer prompt initial-contents hist)))

;;;>> Screw this crock!!
;(defun read-no-blanks-input (prompt &optional initial-contents)
; "Read a string from the terminal, not allowing blanks.
;Prompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS
;is a string to insert in the minibuffer before reading."
; (read-from-minibuffer prompt
;                       initial-contents
;                       minibuffer-local-ns-map
;                       nil))

(defun read-command (prompt)
  "Read the name of a command and return as a symbol.
Prompts with PROMPT."
  (intern (completing-read prompt obarray 'commandp t nil
			   'minibuffer-command-history)))

(defun read-function (prompt)
  "Read the name of a function and return as a symbol.
Prompts with PROMPT."
  (intern (completing-read prompt obarray 'fboundp t nil
			   'minibuffer-function-history)))

(defun read-variable (prompt)
  "Read the name of a user variable and return it as a symbol.
Prompts with PROMPT.
A user variable is one whose documentation starts with a `*' character."
  (intern (completing-read prompt obarray 'user-variable-p t nil
			   'minibuffer-variable-history)))

(defun read-buffer (prompt &optional default require-match)
  "Read the name of a buffer and return as a string.
Prompts with PROMPT.  Optional second arg DEFAULT is value to return if user
enters an empty line.  If optional third arg REQUIRE-MATCH is non-nil,
only existing buffer names are allowed."
  (let ((prompt (if default 
                    (format "%s(default %s) "
                            prompt (if (bufferp default)
                                       (buffer-name default)
                                       default))
                    prompt))
        (alist (mapcar (function (lambda (b)
                         (cons (buffer-name b) b)))
                       (buffer-list)))
        result)
    (while (progn
             (setq result (completing-read prompt alist nil require-match
					   nil 'minibuffer-buffer-history))
             (cond ((not (equal result ""))
                    nil)
                   ((not require-match)
                    (setq result default)
                    nil)
                   ((not default)
                    t)
                   ((not (get-buffer default))
                    t)
                   (t
                    (setq result default)
                    nil))))
    (if (bufferp result)
        (buffer-name result)
      result)))

(defun read-number (prompt &optional integers-only)
  "Reads a number from the minibuffer."
  (let ((pred (if integers-only 'integerp 'numberp))
	num)
    (while (not (funcall pred num))
      (setq num (condition-case ()
		    (read-from-minibuffer
		     prompt (if num (prin1-to-string num)) nil t
		     t) ;no history
		  (invalid-read-syntax nil)
		  (end-of-file nil)))
      (or (funcall pred num) (beep)))
    num))

(defun read-shell-command (prompt &optional initial-input)
  "Just like read-string, but uses read-shell-command-map:
\\{read-shell-command-map}"
  (read-from-minibuffer prompt initial-input read-shell-command-map
			nil 'minibuffer-shell-command-history))


;;; This read-file-name stuff probably belongs in files.el

;; Quote "$" as "$$" to get it past substitute-in-file-name
(defun un-substitute-in-file-name (string)
  (let ((regexp "\\$")
        (olen (length string))
        new
        n o ch)
    (cond ((eq system-type 'vax-vms)
           string)
          ((not (string-match regexp string))
           string)
          (t
           (setq n 1)
           (while (string-match regexp string (match-end 0))
             (setq n (1+ n)))
           (setq new (make-string (+ olen n) ?$))
           (setq n 0 o 0)
           (while (< o olen)
             (setq ch (aref string o))
             (aset new n ch)
             (setq o (1+ o) n (1+ n))
             (if (eq ch ?$)
                 ;; already aset by make-string initial-value
                 (setq n (1+ n))))
           new))))
  
(defun read-file-name-1 (history prompt dir default 
                         must-match initial-contents
                         completer)
  (if (not dir)
      (setq dir default-directory))
  (setq dir (abbreviate-file-name dir))
  (let* ((insert (cond ((not insert-default-directory)
                        nil)
                       (initial-contents
                        (cons (un-substitute-in-file-name
                                (concat dir initial-contents))
                              (1+ (length dir))))
                       (t
                        (un-substitute-in-file-name dir))))
         (val (let ((completion-ignore-case (eq system-type 'vax-vms)))
                ;;  Hateful, broken, case-sensitive un*x
                (completing-read prompt
                                 completer
                                 dir
                                 must-match
                                 insert
                                 history))))
    (cond ((not val)
           (error "No file name specified"))
          ((and default
                (equal val (if (consp insert) (car insert) insert)))
           default)
          (t
           (substitute-in-file-name val)))))


(defun read-file-name (prompt
                       &optional dir default must-match initial-contents hist)
  "Read file name, prompting with PROMPT and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Value is subject to interpreted by substitute-in-file-name however.
Default name to DEFAULT if user enters a null string.
 (If DEFAULT is omitted, the visited file name is used.)
Fourth arg MUST-MATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL-CONTENTS specifies text to start with.
DIR defaults to current buffer's directory default."
  (read-file-name-1
   'minibuffer-file-name-history
   prompt dir (or default buffer-file-name) must-match initial-contents
   ;; A separate function (not an anonymous lambda-expression)
   ;; and passed as a symbol because of disgusting kludges in various
   ;; places which do stuff like (let ((filename-kludge-p (eq minibuffer-completion-table 'read-file-name-internal))) ...)
   'read-file-name-internal))

(defun read-directory-name (prompt
                            &optional dir default must-match initial-contents)
  ;;>>> document me
  (read-file-name-1 
    'minibuffer-file-name-history
    prompt dir (or default default-directory) must-match initial-contents
    'read-directory-name-internal))


;; Environment-variable completion hack
(defun read-file-name-internal-1 (string dir action completer)
  (if (not (string-match "[^$]\\(\\$\\$\\)*\\$\\([A-Za-z0-9_]*\\|{[^}]*\\)\\'"
                         string))
      (let* ((orig (if (equal string "") nil string))
	     (sstring (if orig (substitute-in-file-name string) string))
	     (specdir (if orig (file-name-directory sstring) nil)))
	(funcall completer 
		 action 
		 orig 
		 sstring 
		 specdir
		 (if specdir (expand-file-name specdir dir) dir)
		 (if orig (file-name-nondirectory sstring) string)))
      ;; An odd number of trailing $'s
      (let* ((start (match-beginning 2))
             (env (substring string 
                             (cond ((= start (length string))
                                    ;; "...$"
                                    start)
                                   ((= (aref string start) ?{)
                                    ;; "...${..."
                                    (1+ start))
                                   (t
                                    start))))
             (head (substring string 0 (1- start))))
        (cond ((eq action 'lambda)
               nil)
              ((eq action 't)
               ;; all completions
               (mapcar (function (lambda (p)
                         (if (and (> (length p) 0)
                                  ;;>>> Unix-specific
                                  ;;>>>  -- need absolute-pathname-p
                                  (/= (aref p 0) ?/))
                             (concat "$" p)
                             (concat head "$" p))))
                       (all-completions env (getenv t))))
              (t ;; 'nil
               ;; complete
               (let* ((e (getenv t))
                      (val (try-completion env e)))
                 (cond ((stringp val)
                        (if (string-match "[^A-Za-z0-9_]" val)
                            (concat head
                                    "${" val
                                    ;; completed uniquely?
                                    (if (eq (try-completion val e) 't)
                                        "}" ""))
                            (concat head "$" val)))
                       ((eql val 't)
                        (concat head
                                (un-substitute-in-file-name (getenv env))))
                       (t nil))))))))


(defun read-file-name-internal (string dir action)
  (read-file-name-internal-1 
    string dir action
    (function (lambda (action orig string specdir dir name)
      (cond ((eq action 'lambda)
             (if (not orig)
                 nil
                 (file-exists-p string)))
            ((eq action 't)
             ;; all completions
             (mapcar (function un-substitute-in-file-name)
                     (file-name-all-completions name dir)))
            (t;; 'nil
             ;; complete
	     (or specdir (setq specdir default-directory))
             (let ((val (file-name-completion name specdir)))
               (if (and (eq val 't)
                        (not (null completion-ignored-extensions)))
                   ;;>> (file-name-completion "foo") returns 't
                   ;;   when both "foo" and "foo~" exist and the latter
                   ;;   is "pruned" by completion-ignored-extensions.
                   ;; I think this is a bug in file-name-completion.
                   (setq val (let ((completion-ignored-extensions '()))
                               (file-name-completion name specdir))))
               (if (stringp val)
                   (un-substitute-in-file-name (if specdir
                                                   (concat specdir val)
                                                   val))
                   (let ((tem (un-substitute-in-file-name string)))
                     (if (not (equal tem orig))
                         ;; substitute-in-file-name did something
                         tem
                         val))))))))))


(defun read-directory-name-internal (string dir action)
  (read-file-name-internal-1 
    string dir action
    (function (lambda (action orig string specdir dir name)
      (let* (;; This looks better in a possibilities list than ""
             ;;>>>> Un*x-specific >>
             (standin "./")
             (dirs (function (lambda (fn)
                     (let ((l (if (equal name "")
                                  (cons standin (directory-files
                                                 dir
                                                 nil
                                                 ""
                                                 nil
                                                 'directories))
                                  (directory-files
                                   dir
                                   nil 
                                   (concat "\\`" (regexp-quote name))
                                   nil
                                   'directories))))
                       (mapcar fn
                               (cond ((eq system-type 'vax-vms)
                                      l)
                                     (t
                                      ;; Wretched unix
                                      (delete "." (delete ".." l))))))))))
        (cond ((eq action 'lambda)
               ;; complete?
               (if (not orig)
                   nil
                   (and (file-directory-p string)
                        ;; So "foo" is ambiguous between "foo/" and "foobar/"
                        (equal string (file-name-as-directory string)))))
              ((eq action 't)
               ;; all completions
               (funcall dirs (function (lambda (n)
                               (un-substitute-in-file-name 
                                (if (equal n standin) 
                                    standin
                                    (file-name-as-directory n)))))))
              (t
               ;; complete
               (let ((val (try-completion
                           name
                           (funcall dirs
                                    (function (lambda (n)
                                      (if (equal n standin)
                                          (list standin)
                                          (list (file-name-as-directory
                                                 n)))))))))
                 (if (stringp val)
                     (un-substitute-in-file-name (if specdir
                                                     (concat specdir val)
                                                     val))
                     (let ((tem (un-substitute-in-file-name string)))
                       (if (not (equal tem orig))
                           ;; substitute-in-file-name did something
                           tem
                           val)))))))))))

;;;; Stuff which has ended up here for want of a better place

(defun execute-extended-command (prefix-arg)
  (interactive "P")
  ;; Note:  This doesn't hack "this-command-keys"
  (let ((prefix-arg prefix-arg))
    (setq this-command (read-command
                        ;; Note: this has the hard-wired
                        ;;  "C-u" and "M-x" string bug in common
                        ;;  with all GNU Emacs's.
                        (cond ((eq prefix-arg '-)
                               "- M-x ")
                              ((equal prefix-arg '(4))
                               "C-u M-x ")
                              ((integerp prefix-arg)
                               (format "%d M-x " prefix-arg))
                              ((and (consp prefix-arg)
                                    (integerp (car prefix-arg)))
                               (format "%d M-x " (car prefix-arg)))
                              (t
                               "M-x ")))))
  (command-execute this-command t))

(defun y-or-n-p-minibuf (prompt)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".
Takes one argument, which is the string to display to ask the question.
It should end in a space; `y-or-n-p' adds `(y or n) ' to it.
No confirmation of the answer is requested; a single character is enough.
Also accepts Space to mean yes, or Delete to mean no."
  (let* ((prompt (format "%s(y or n) " prompt))
         (p prompt)
         (event (allocate-event)))
    (while (stringp p)
      (if (let ((cursor-in-echo-area t)
                (inhibit-quit t))
            (message "%s" p)
            (next-command-event event)
            (prog1 quit-flag (setq quit-flag nil)))
          (progn
            (message "%s%s" p (single-key-description event))
            (deallocate-event event)
            (setq quit-flag nil)
            (signal 'quit '())))
      (let* ((key (and (key-press-event-p event) (event-key event)))
             (char (and key (event-to-character event))))
        (if char (setq char (downcase char)))
        (cond ((or (eq char ?y) (eq char ? ))
               (message "%sYes" p)
               (setq p t))
              ((or (eq char ?n) (eq key 'delete))
               (message "%sNo" p)
               (setq p nil))
	      ((button-release-event-p event) ; ignore them
	       nil)
              (t
               (message "%s%s" p (single-key-description event))
               (ding nil 'y-or-n-p)
               (discard-input)
               (if (eq p prompt)
                   (setq p (concat "Please answer y or n.  " prompt)))))))
    (deallocate-event event)
    p))

(defun yes-or-no-p-minibuf (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes.
Takes one argument, which is the string to display to ask the question.
It should end in a space; `yes-or-no-p' adds `(yes or no) ' to it.
The user must confirm the answer with RET,
and can edit it until it as been confirmed."
  (let ((p (concat prompt "(yes or no) ")))
    (while (stringp p)
      (setq p (downcase (read-string p nil t))) ;no history
      (cond ((string-equal p "yes")
             (setq p 't))
            ((string-equal p "no")
             (setq p 'nil))
            (t
             (ding nil 'yes-or-no-p)
             (discard-input)
             (message "Please answer yes or no.")
             (sleep-for 2))))
    p))

;; these may be redefined later, but make the original def easily encapsulable
(fset 'yes-or-no-p 'yes-or-no-p-minibuf)
(fset 'y-or-n-p 'y-or-n-p-minibuf)
