;;;  TPU-edt by Rob Riepel - Emacs emulating TPU emulating EDT
;;;
;;;      Based on tpu.el by Jeff Kowalski (November, 1988)
;;;

;; Copyright (C) 1992 Rob Riepel.

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
;;


;;;
;;;
;;;  Revision Information
;;;
;;;
(defconst tpu-revision "-Revision: 4.8 -"
  "Revision number of TPU-edt.")
(defconst tpu-revision-date "-Date: 1992/06/24 21:16:18 -"
  "Date current revision of TPU-edt was created.")


;;;
;;;
;;;  User Configurable Variables
;;;
;;;
(defconst tpu-have-ispell t
  "*If non-nil (default), TPU-edt uses ispell for spell checking.")

(defconst tpu-kill-buffers-silently nil
  "*If non-nil, TPU-edt kills modified buffers without asking.")

(defvar tpu-percent-scroll 75
  "*Percentage of the screen to scroll for next/previous screen commands.")


;;;
;;;
;;;  G L O B A L
;;;  K E Y M A P S
;;;
;;;
;;;;; (defvar CSI-map nil
;;;;;   "Maps the CSI function keys on the VT100 keyboard.
;;;;; CSI is DEC's name for the sequence <ESC>[.")
;;;;; (setq CSI-map (make-sparse-keymap))

;;;;; (defvar SS3-map nil
;;;;;   "Maps the SS3 function keys on the VT100 keyboard.
;;;;; SS3 is DEC's name for the sequence <ESC>O.")
;;;;; (setq SS3-map (make-sparse-keymap))

;;;;; (defvar GOLD-map nil
;;;;;    "Maps the function keys on the VT100 keyboard preceeded by PF1.
;;;;; GOLD is the ASCII 7-bit escape sequence <ESC>OP.")
;;;;; (setq GOLD-map (make-sparse-keymap))

;;;;; (defvar GOLD-CSI-map nil
;;;;;    "Maps the function keys on the VT100 keyboard preceeded by GOLD-CSI.")
;;;;; (setq GOLD-CSI-map (make-sparse-keymap))

;;;;; (defvar GOLD-SS3-map nil
;;;;;    "Maps the function keys on the VT100 keyboard preceeded by GOLD-SS3.")
;;;;; (setq GOLD-SS3-map (make-sparse-keymap))


;;;
;;;
;;;  G L O B A L
;;;  V A R I A B L E S
;;;
;;;
(defvar tpu-global-key-plist nil
  "Original emacs definitions of global keys, so they may be restored.")
(defvar tpu-breadcrumb-plist nil
  "The set of user-defined markers (breadcrumbs), as a plist.")
(defvar tpu-counter 0
  "A general-purpose counter available for the user.")
(defvar tpu-counter-format "%d"
  "The format to use to when displaying the tpu-counter.")
(defvar tpu-last-replaced-text ""
  "Last text deleted by a TPU-edt replace command.")
(defvar tpu-last-deleted-region ""
  "Last text deleted by a TPU-edt remove command.")
(defvar tpu-last-deleted-lines ""
  "Last text deleted by a TPU-edt line-delete command.")
(defvar tpu-last-deleted-words ""
  "Last text deleted by a TPU-edt word-delete command.")
(defvar tpu-last-deleted-char ""
  "Last character deleted by a TPU-edt character-delete command.")
(defvar tpu-search-last-string ""
  "Last text searched for by the TPU-edt search commands.")
(defvar tpu-regexp-p nil
  "If non-nil, TPU-edt uses regexp search and replace routines.")
(defvar tpu-rectangular-p nil
  "If non-nil, TPU-edt removes and inserts rectangles.")
(defvar tpu-advance t
  "True when TPU-edt is operating in the forward direction.")
(defvar tpu-reverse nil
  "True when TPU-edt is operating in the backward direction.")
(defvar tpu-rectangle-string nil
  "Mode line string to identify rectangular mode.")


;;;
;;;
;;;  L O C A L
;;;  V A R I A B L E S
;;;
;;;
(defvar newline-and-indent-p nil
  "If non-nil, Return produces a newline and indents.")
(make-variable-buffer-local 'newline-and-indent-p)

(defvar tpu-saved-delete-func nil
  "Saved value of the delete key.")
(make-variable-buffer-local 'tpu-saved-delete-func)

(defvar tpu-buffer-local-map nil
  "TPU-edt buffer local key map.")
(make-variable-buffer-local 'tpu-buffer-local-map)


;;;
;;;
;;;  Modify the the mode line to show if the mark is set.
;;;
;;;
(defvar tpu-mark-flag " ")

(make-variable-buffer-local 'tpu-mark-flag)

(setq-default mode-line-format
  (list (purecopy "")
   'mode-line-modified
   'mode-line-buffer-identification
   (purecopy "  ")
   'global-mode-string
   (purecopy "  ")
   'tpu-mark-flag
   (purecopy " %[(")
   'mode-name 'minor-mode-alist "%n" 'mode-line-process
   (purecopy ")%]----")
   (purecopy '(-3 . "%p"))
   (purecopy "-%-")))


;;;
;;;
;;;  Match markers -
;;;
;;;     Set in:  Search
;;;
;;;     Used in: Replace, Substitute, Store-Text, Cut/Remove,
;;;              Append, and Change-Case
;;;
;;;
(setq tpu-match-beginning-mark (make-marker))
(setq tpu-match-end-mark (make-marker))

(defun tpu-set-match nil
  "Set markers at match beginning and end."
  ;; Add one to beginning mark so it stays with the first character of
  ;;   the string even if characters are added just before the string.
  (setq tpu-match-beginning-mark (copy-marker (1+ (match-beginning 0))))
  (setq tpu-match-end-mark (copy-marker (match-end 0))))

(defun tpu-unset-match nil
  "Unset match beginning and end markers."
  (set-marker tpu-match-beginning-mark nil)
  (set-marker tpu-match-end-mark nil))

(defun tpu-match-beginning nil
  "Returns the location of the last match beginning."
  (1- (marker-position tpu-match-beginning-mark)))

(defun tpu-match-end nil
  "Returns the location of the last match end."
  (marker-position tpu-match-end-mark))

(defun tpu-check-match nil
  "Returns t if point is between tpu-match markers.
Otherwise sets the tpu-match markers to nil and returns nil."
  ;; make sure 1- marker is in this buffer
  ;;           2- point is at or after beginning marker
  ;;           3- point is before ending marker, or in the case of
  ;;              zero length regions (like bol, or eol) that the
  ;;              beginning, end, and point are equal.
  (cond ((and
          (equal (marker-buffer tpu-match-beginning-mark) (current-buffer))
          (>= (point) (1- (marker-position tpu-match-beginning-mark)))
          (or
           (< (point) (marker-position tpu-match-end-mark))
           (and (= (1- (marker-position tpu-match-beginning-mark))
                   (marker-position tpu-match-end-mark))
                (= (marker-position tpu-match-end-mark) (point))))) t)
        (t
         (tpu-unset-match) nil)))

(defun tpu-show-match-markers nil
  "Show the values of the match markers."
  (interactive)
  (if (markerp tpu-match-beginning-mark)
      (let ((beg (marker-position tpu-match-beginning-mark)))
        (message "(%s, %s) in %s -- current %s in %s"
                 (if beg (1- beg) nil)
                 (marker-position tpu-match-end-mark)
                 (marker-buffer tpu-match-end-mark)
                 (point) (current-buffer)))))


;;;
;;;
;;;  U T I L I T I E S
;;;
;;;
(defun caar (thingy) (car (car thingy)))
(defun cadr (thingy) (car (cdr thingy)))
(defun cadar (thingy) (car (cdr (car thingy))))
(defun caddar (thingy) (car (cdr (cdr (car thingy)))))

(defun tpu-y-or-n-p (prompt &optional not-yes)
  "Prompt for a y or n answer with positive default.
Optional second argument NOT-YES changes default to negative.
Like emacs y-or-n-p, also accepts space as y and DEL as n."
  (message (format "%s[%s]" prompt (if not-yes "n" "y")))
  (let ((doit t))
    (while doit
      (setq doit nil)
      (let ((ans (read-char)))
        (cond ((or (= ans ?y) (= ans ?Y) (= ans ?\ ))
               (setq tpu-last-answer t))
              ((or (= ans ?n) (= ans ?N) (= ans ?\C-?))
               (setq tpu-last-answer nil))
              ((= ans ?\r) (setq tpu-last-answer (not not-yes)))
              (t
               (setq doit t) (beep)
               (message (format "Please answer y or n.  %s[%s]"
                                prompt (if not-yes "n" "y"))))))))
  (message "")				; Clear minibuffer
  tpu-last-answer)


;;;
;;;  Breadcrumbs
;;;
(defun drop-breadcrumb (num)
  "Drops a breadcrumb that can be returned to later with goto-breadcrumb."
  (interactive "p")
  (put tpu-breadcrumb-plist num (list (current-buffer) (point)))
  (message "Mark %d set." num))

(defun goto-breadcrumb (num)
  "Returns to a breadcrumb set with drop-breadcrumb."
  (interactive "p")
  (cond ((get tpu-breadcrumb-plist num)
         (switch-to-buffer (car (get tpu-breadcrumb-plist num)))
         (goto-char (cadr (get tpu-breadcrumb-plist num)))
         (message "mark %d found." num))
        (t
         (message "mark %d not found." num))))


;;;
;;;  miscellaneous
;;;
(defun replace-global-key (key func)
  "Saves the current global key definition and replaces it with a new one."
  (interactive)
  (if (not (get tpu-global-key-plist key))
      (put tpu-global-key-plist key func))
  (global-set-key key func))

(defun restore-global-key (key)
  "Restores the original definition of a global key."
  (interactive)
  (global-set-key key (get tpu-global-key-plist key)))

(defun tpu-local-set-key (key func)
  "Replace a key in the TPU-edt local key map.
Create the key map if necessary."
  (cond ((not (keymapp tpu-buffer-local-map))
         (setq tpu-buffer-local-map (if (current-local-map)
                                        (copy-keymap (current-local-map))
                                      (make-sparse-keymap)))
         (use-local-map tpu-buffer-local-map)))
  (local-set-key key func))

(defun current-line nil
  "Return the vertical position of point in the selected window.
Top line is 0.  Counts each text line only once, even if it wraps."
  (+ (count-lines (window-start) (point)) (if (= (current-column) 0) 1 0) -1))

(defun tpu-change-case (num)
  "Change the case of the character under the cursor or region.
Accepts a prefix argument of the number of characters to invert."
  (interactive "p")
  (cond ((mark)
         (let ((beg (region-beginning)) (end (region-end)))
           (while (> end beg)
             (funcall (if (<= ?a (char-after beg))
                          'upcase-region 'downcase-region)
                      beg (1+ beg))
             (setq beg (1+ beg)))
           (tpu-unselect t)))
        ((tpu-check-match)
         (let ((beg (tpu-match-beginning)) (end (tpu-match-end)))
           (while (> end beg)
             (funcall (if (<= ?a (char-after beg))
                          'upcase-region 'downcase-region)
                      beg (1+ beg))
             (setq beg (1+ beg)))
           (tpu-unset-match)))
        (t
         (while (> num 0)
           (funcall (if (<= ?a (following-char))
                        'upcase-region 'downcase-region)
                    (point) (1+ (point)))
           (forward-char (if tpu-reverse -1 1))
           (setq num (1- num))))))

(defun tpu-fill (num)
  "Fill paragraph or marked region.
With argument, fill and justify."
  (interactive "P")
  (cond ((mark)
         (fill-region (point) (mark) num)
         (tpu-unselect t))
        (t
         (fill-paragraph num))))

(defun tpu-trim-line-ends nil
  "Removes trailing whitespace from every line in the buffer."
  (interactive)
  (picture-clean))

(defun update-mode-line nil
  "Make sure mode-line in the current buffer reflects all changes."
  (setq tpu-mark-flag (if (mark) "M" " "))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

(defun tpu-version nil
  "Print the TPU-edt version number."
  (interactive)
  (message (concat "TPU-edt revision "
                   (substring tpu-revision 11 -2)
                   " by Rob Riepel (rob@icarus.ssd.loral.com)    "
                   (substring tpu-revision-date 7 -11))))

(defun reset-screen-size (height width)
  "Sets the screen size."
  (interactive "nnew screen height: \nnnew screen width: ")
  (set-screen-height height)
  (set-screen-width width))

(defun toggle-newline-and-indent nil
  "Toggle between 'newline and indent' and 'simple newline'."
  (interactive)
  (cond (newline-and-indent-p
         (setq newline-and-indent-p-string "")
         (setq newline-and-indent-p nil)
         (tpu-local-set-key '(control m) 'newline))
        (t
         (setq newline-and-indent-p-string " AutoIndent")
         (setq newline-and-indent-p t)
         (tpu-local-set-key '(control m) 'newline-and-indent)))
  (update-mode-line))

(defun spell-check nil
  "Checks the spelling of the region, or of the entire buffer if no
 region is selected."
  (interactive)
  (cond (tpu-have-ispell
         (if (mark) (ispell-region (mark) (point)) (ispell-buffer)))
        (t
         (if (mark) (spell-region (mark) (point)) (spell-buffer))))
  (if (mark) (tpu-unselect t)))

(defun tpu-what-line nil
  "Tells what line the point is on,
 and the total number of lines in the buffer."
  (interactive)
  (if (eobp)
      (message "You are at the End of Buffer.  The last line is %d."
               (count-lines 1 (point-max)))
    (message "Line %d of %d"
             (count-lines 1 (1+ (point)))
             (count-lines 1 (point-max)))))

(defun tpu-select (&optional quiet)
  "Sets the mark to define one end of a region."
  (interactive "P")
  (cond ((mark)
         (tpu-unselect quiet))
        (t
	 (push-mark)
	 (zmacs-activate-region)
         (update-mode-line)
         (if (not quiet) (message "Move the text cursor to select text.")))))

(defun tpu-unselect (&optional quiet)
  "Removes the mark to unselect the current region."
  (interactive "P")
  (setq mark-ring nil)
  (set-mark nil)
  (update-mode-line)
  (if (not quiet) (message "Selection canceled.")))

(defun toggle-overwrite-mode nil
   "Switches in and out of overwrite mode"
   (interactive)
   (cond (overwrite-mode
          (tpu-local-set-key "\177" tpu-saved-delete-func)
          (overwrite-mode 0))
         (t
          (setq tpu-saved-delete-func (local-key-binding "\177"))
          (tpu-local-set-key "\177" 'picture-backward-clear-column)
          (overwrite-mode 1))))

(defun tpu-special-insert (num)
  "Insert a character or control code according to
its ASCII decimal value."
  (interactive "P")
  (insert (if num num 0)))

(defun quit-emacs-now nil
  "Go away.  Now.  Just  g o  a w a y."
  (interactive)
  (kill-emacs t))


;;;
;;;  TPU-alike commands
;;;
(defun include (file)
  "TPU-like include file"
  (interactive "fInclude file: ")
  (save-excursion
    (insert-file file)
    (message "")))

(defun Get (file)
  "TPU-like get file"
  (interactive "FFile to get: ")
  (find-file file))

(defun exit nil
  "Exit the way TPU does, save current buffer and ask about others."
  (interactive)
  (if (not (eq (recursion-depth) 0))
    (exit-recursive-edit)
    (progn (save-buffer) (save-buffers-kill-emacs))))

(defun quit nil
  "Quit the way TPU does, ask to make sure changes should be abandoned."
  (interactive)
  (let ((list (buffer-list))
        (working t))
    (while (and list working)
      (let ((buffer (car list)))
        (if (and (buffer-file-name buffer) (buffer-modified-p buffer))
            (if (tpu-y-or-n-p
                 (format "Modifications will not be saved, continue quitting? " ))
                (kill-emacs t) (setq working nil)))
        (setq list (cdr list))))
    (if working (kill-emacs t))))


;;;
;;;  Command and Function Aliases
;;;
(fset 'EXIT 'exit)
(fset 'GET 'Get)
(fset 'INCLUDE 'include)
(fset 'QUIT 'quit)

(fset 'spell 'spell-check)
(fset 'SPELL 'spell-check)

(fset 'what\ line 'tpu-what-line)
(fset 'WHAT\ LINE 'tpu-what-line)

(fset 'replace 'tpu-lm-replace)
(fset 'REPLACE 'tpu-lm-replace)

(fset 'help 'tpu-help)
(fset 'HELP 'tpu-help)

;; Around emacs version 18.57, function line-move was renamed to
;; next-line-internal.  If we're running under an older emacs,
;; make next-line-internal equivalent to line-move.

(if (not (fboundp 'next-line-internal)) (fset 'next-line-internal 'line-move))


;;;
;;;  Help
;;;
(defun tpu-help nil
  "Display TPU-edt help."
  (interactive)
  ;; Save current window configuration
  (save-window-excursion
    ;; Create and fill help buffer if necessary
    (if (not (get-buffer "*TPU-edt Help*"))
        (progn (generate-new-buffer "*TPU-edt Help*")
               (switch-to-buffer "*TPU-edt Help*")
               (insert "\f
          _______________________    _______________________________
         | HELP  |      Do       |  |       |       |       |       |
         |KeyDefs|               |  |       |       |       |       |
         |_______|_______________|  |_______|_______|_______|_______|
          _______________________    _______________________________
         | Find  |Insert |Remove |  | Gold  | HELP  |FndNxt | Del L |
         |       |       |Sto Tex|  |  key  |E-Help | Find  |Undel L|
         |_______|_______|_______|  |_______|_______|_______|_______|
         |Select |Pre Scr|Nex Scr|  | Page  | Sect  |Append | Del W |
         | Reset |Pre Win|Nex Win|  |  Do   | Fill  |Replace|Undel W|
         |_______|_______|_______|  |_______|_______|_______|_______|
                 |Move up|          |Forward|Reverse|Remove | Del C |
                 |  Top  |          |Bottom |  Top  |Insert |Undel C|
          _______|_______|_______   |_______|_______|_______|_______|
         |Mov Lef|Mov Dow|Mov Rig|  | Word  |  EOL  | Char  |       |
         |StaOfLi|Bottom |EndOfLi|  |ChngCas|Del EOL|SpecIns| Enter |
         |_______|_______|_______|  |_______|_______|_______|       |
                                    |     Line      |Select | Subs  |
                                    |   Open Line   | Reset |       |
                                    |_______________|_______|_______|


\f

      Control Characters

      ^A  toggle insert and overwrite
      ^B  recall
      ^E  end of line

      ^G  Cancel current operation
      ^H  beginning of line
      ^J  delete previous word

      ^K  learn
      ^L  insert page break
      ^R  remember (during learn), re-center

      ^U  delete to beginning of line
      ^V  quote
      ^W  refresh

      ^Z  exit
    ^X^X  exchange point and mark - useful for checking region boundaries

\f
       Gold-<key> Functions

       B     Next Buffer - display the next buffer (all buffers)
       C     Recall - edit and possibly repeat previous commands
       E     Exit - save current buffer and ask about others

       G     Get - load a file into a new edit buffer
       I     Include - include a file in this buffer
       K     Kill Buffer - abandon edits and delete buffer

       M     Buffer Menu - display a list of all buffers
       N     Next File Buffer - display next buffer containing a file
       O     Occur - show following lines containing REGEXP

       Q     Quit - exit without saving anything
       R     Toggle rectangular mode for remove and insert
       S     Search and substitute - line mode REPLACE command

       U     Undo - undo the last edit
       W     Write - save current buffer
       X     Exit - save all modified buffers and exit

\f

   *** No more help, use P to view previous screen")
      (setq buffer-read-only t)))

    ;; Display the help buffer
    (switch-to-buffer "*TPU-edt Help*")
    (delete-other-windows)
    (move-to-beginning)
    (forward-line 1)
    (line-to-top-of-window)

    ;; Prompt for keys to describe, based on screen state (split/not split)
    (let ((key nil) (split nil))
      (while (not (equal key "\r"))
        (if split
            (setq key
                  (read-key-sequence
                   "Press the key you want help on (RET=exit, ENTER=redisplay, N=next, P=prev): "))
          (setq key
                (read-key-sequence
                 "Press the key you want help on (RET to exit, N next screen, P prev screen): ")))

        ;; Process the read key
        ;;
        ;;    ENTER    Display just the help window
        ;;    N or n   Next help or describe-key screen
        ;;    P or p   Previous help or describe-key screen
        ;;    RETRUN   Exit from TPU-help
        ;;    default  describe the key
        ;;
        (cond ((equal "\eOM" key)
               (setq split nil)
               (delete-other-windows))
              ((or (equal "N" key) (equal "n" key))
               (cond (split
                      (condition-case nil
                          (scroll-other-window 8)
                        (error nil)))
                     (t
                      (forward-page)
                      (forward-line 1)
                      (line-to-top-of-window))))
              ((or (equal "P" key) (equal "p" key))
               (cond (split
                      (condition-case nil
                          (scroll-other-window -8)
                        (error nil)))
                     (t
                      (backward-page 2)
                      (forward-line 1)
                      (line-to-top-of-window))))
              ((not (equal "\r" key))
               (setq split t)
               (describe-key key)
               ;; If the key is undefined, leave the
               ;;   message in the mini-buffer for 3 seconds
               (if (not (key-binding key)) (sit-for 3))))))))


;;;
;;;  Auto-insert
;;;
(defun insert-escape nil
  "Inserts an escape character, and so becomes the escape-key alias."
  (interactive)
  (insert "\e"))

(defun insert-formfeed nil
  "Inserts a formfeed character."
  (interactive)
  (insert "\C-L"))


;;;
;;;  Define key
;;;
(defun end-define-macro-key (key)
  "Ends the current macro definition"
  (interactive "kPress the key you want to use to do what was just learned: ")
  (end-kbd-macro nil)
  (global-set-key key last-kbd-macro)
  (global-set-key '(control r) 'recenter))

(defun define-macro-key nil
  "Bind a set of keystrokes to a single key, or key combination."
  (interactive)
  (global-set-key '(control r) 'end-define-macro-key)
  (start-kbd-macro nil))


;;;
;;;  Counter
;;;
(defun set-counter nil
  "Set the internal counter to the repeat count."
  (interactive)
  (message "Counter set to %d."
           (setq tpu-counter current-prefix-arg)))

(defun region-length-to-counter nil
  "Sets the counter to the length of the region."
  (interactive)
  (cond ((mark)
         (message "Counter set to %d."
                  (setq tpu-counter
                        (- (max (point) (mark)) (min (point) (mark))))))
        (t (message "The mark is not set now; no select range is active."))))

(defun decr-counter nil
  "Decrement the internal counter by the repeat count,
 or by one if no repeat count is defined."
 (interactive)
 (message "Counter set to %d."
          (setq tpu-counter (- tpu-counter (or current-prefix-arg 1)))))

(defun incr-counter nil
  "Increment the internal counter by the repeat count,
 or by one if no repeat count is defined."
 (interactive)
 (message "Counter set to %d."
          (setq tpu-counter (+ tpu-counter (or current-prefix-arg 1)))))

(defun change-counter-format (fmt)
  "Changes the display format for use when inserting the internal tpu counter.
 You may use standard C format expressions."
  (interactive "sFormat (C-like): ")
  (setq tpu-counter-format fmt))

(defun insert-counter nil
  "Inserts the internal tpu counter using the tpu counter format."
  (interactive)
  (insert (format tpu-counter-format tpu-counter)))

(defun insert-counter-as-char nil
  "Inserts the character value (mod 256) of the internal tpu counter."
  (interactive)
  (insert (mod tpu-counter 256)))


;;;
;;;  Buffer
;;;
(defun set-buffer-clean nil
  "Toggles the update mode of the buffer."
  (interactive)
  (set-buffer-modified-p (not (buffer-modified-p))))

(defun tpu-kill-buffer nil
  "Kills the current buffer.  If tpu-kill-buffers-silently is non-nil,
kills modified buffers without asking."
  (interactive)
  (if tpu-kill-buffers-silently (set-buffer-modified-p nil))
  (kill-buffer (current-buffer)))

(defun save-all-buffers-kill-emacs nil
  "Save all buffers and exit emacs."
  (interactive)
  (setq trim-versions-without-asking t)
  (save-buffers-kill-emacs t))

(defun write-current-buffers nil
  "Save all modified buffers without exiting."
  (interactive)
  (save-some-buffers t))

(defun next-buffer nil
  "Go to next buffer in ring."
  (interactive)
  (switch-to-buffer (car (reverse (buffer-list)))))

(defun next-file-buffer nil
  "Go to next buffer in ring that is visiting a file."
  (interactive)
  (setq starting-buffer (buffer-name))
  (switch-to-buffer (car (reverse (buffer-list))))
  (while (and (not (equal (buffer-name) starting-buffer))
              (not (buffer-file-name)))
    (switch-to-buffer (car (reverse (buffer-list)))))
  (if (equal (buffer-name) starting-buffer)
    (progn (beep) (message "No other buffers."))))


;;;
;;;  Repeat count
;;;
(defun repeat-command-0 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal last-command-event nil nil))

(defun repeat-command-1 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal last-command-event nil nil))

(defun repeat-command-2 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal last-command-event nil nil))

(defun repeat-command-3 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal last-command-event nil nil))

(defun repeat-command-4 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal last-command-event nil nil))

(defun repeat-command-5 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal last-command-event nil nil))

(defun repeat-command-6 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal last-command-event nil nil))

(defun repeat-command-7 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal last-command-event nil nil))

(defun repeat-command-8 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal last-command-event nil nil))

(defun repeat-command-9 nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal last-command-event nil nil))

(defun repeat-command-- nil
  "Repeats the following keystroke."
  (interactive)
  (prefix-arg-internal last-command-event nil nil))


;;;
;;;  Search
;;;
(defun tpu-toggle-regexp nil
   "Switches in and out of regular expression search and replace mode."
   (interactive)
   (setq tpu-regexp-p (not tpu-regexp-p))
   (tpu-set-search)
   (message "Regular expression search and substitute %sabled."
            (if tpu-regexp-p "en" "dis")))

(defun tpu-regexp-prompt (prompt)
  "Read a string, adding 'RE' to the prompt if tpu-regexp-p is set."
  (read-string (concat (if tpu-regexp-p "RE ") prompt)))

(defun tpu-search nil
  "Search for a string or regular expression.
The search is performed in the current direction."
  (interactive)
  (tpu-set-search)
  (tpu-search-internal ""))

(defun tpu-search-again nil
  "Search for the same string or regular expression as last time.
The search is performed in the current direction."
  (interactive)
  (tpu-search-internal tpu-search-last-string))

;;  tpu-set-search defines the search functions used by the TPU-edt internal
;;  search function.  It should be called whenever the direction changes, or
;;  the regular expression mode is turned on or off.  It can also be called
;;  to ensure that the next search will be in the current direction.  It is
;;  called from:

;;       tpu-advance              tpu-backup
;;       tpu-toggle-regexp        tpu-toggle-search-direction (t)
;;       tpu-search               tpu-lm-replace

(defun tpu-set-search (&optional arg)
  "Set the search functions and set the search direction to the current
direction.  If an argument is specified, don't set the search direction."
  (if (not arg) (setq searching-forward (if tpu-advance t nil)))
  (cond (searching-forward
         (cond (tpu-regexp-p
                (fset 'tpu-emacs-search 're-search-forward)
                (fset 'tpu-emacs-rev-search 're-search-backward))
               (t
                (fset 'tpu-emacs-search 'search-forward)
                (fset 'tpu-emacs-rev-search 'search-backward))))
        (t
         (cond (tpu-regexp-p
                (fset 'tpu-emacs-search 're-search-backward)
                (fset 'tpu-emacs-rev-search 're-search-forward))
               (t
                (fset 'tpu-emacs-search 'search-backward)
                (fset 'tpu-emacs-rev-search 'search-forward))))))

(defun tpu-search-internal (pat &optional quiet)
  "Search for a string or regular expression."
  (setq tpu-search-last-string
        (if (not (string= "" pat)) pat (tpu-regexp-prompt "Search: ")))

  (tpu-unset-match)
  (tpu-adjust-search)

  (cond ((tpu-emacs-search tpu-search-last-string nil t)
         (tpu-set-match) (goto-char (tpu-match-beginning)))

        (t
         (tpu-adjust-search t)
         (let ((found nil) (pos nil))
           (save-excursion
             (let ((searching-forward (not searching-forward)))
               (tpu-adjust-search)
               (setq found (tpu-emacs-rev-search tpu-search-last-string nil t))
               (setq pos (match-beginning 0))))

           (cond (found
                  (cond ((tpu-y-or-n-p
                          (format "Found in %s direction.  Go there? "
                                  (if searching-forward "reverse" "forward")))
                         (goto-char pos) (tpu-set-match)
                         (tpu-toggle-search-direction))))

                 (t
                  (if (not quiet)
                      (message
                       "%sSearch failed: \"%s\""
                       (if tpu-regexp-p "RE " "") tpu-search-last-string))))))))

(defun tpu-adjust-search (&optional arg)
  "For forward searches, move forward a character before searching,
and backward a character after a failed search.  Arg means end of search."
  (if searching-forward
      (cond (arg (if (not (bobp)) (forward-char -1)))
            (t (if (not (eobp)) (forward-char 1))))))

(defun tpu-toggle-search-direction nil
  "Toggle the TPU-edt search direction.
Used for reversing a search in progress."
  (interactive)
  (setq searching-forward (not searching-forward))
  (tpu-set-search t))


;;;
;;;  Delete
;;;
(defun tpu-toggle-rectangle nil
  "Toggle rectangular mode for remove and insert."
  (interactive)
  (setq tpu-rectangular-p (not tpu-rectangular-p))
  (setq tpu-rectangle-string (if tpu-rectangular-p " Rect" ""))
  (update-mode-line)
  (message "Rectangular cut and paste %sabled."
           (if tpu-rectangular-p "en" "dis")))

(defun tpu-arrange-rectangle nil
  "Adjust point and mark to mark upper left and lower right
corners of a rectangle."
  (let ((mc (current-column))
        (pc (progn (exchange-point-and-mark) (current-column))))

    (cond ((> (point) (mark))                      ; point on lower line
           (cond ((> pc mc)                        ; point @  lower-right
                  (exchange-point-and-mark))       ; point -> upper-left

                 (t                                ; point @  lower-left
                  (move-to-column-force mc)        ; point -> lower-right
                  (exchange-point-and-mark)        ; point -> upper-right
                  (move-to-column-force pc))))     ; point -> upper-left

          (t                                       ; point on upper line
           (cond ((> pc mc)                        ; point @  upper-right
                  (move-to-column-force mc)        ; point -> upper-left
                  (exchange-point-and-mark)        ; point -> lower-left
                  (move-to-column-force pc)        ; point -> lower-right
                  (exchange-point-and-mark)))))))  ; point -> upper-left

(defun tpu-cut nil
  "Delete the selected region.
The text is saved for the tpu-paste command."
  (interactive)
    (cond ((mark)
           (cond (tpu-rectangular-p
                  (tpu-arrange-rectangle)
                  (picture-clear-rectangle (point) (mark) (not overwrite-mode))
                  (tpu-unselect t))
                 (t
                  (setq tpu-last-deleted-region
                        (buffer-substring (mark) (point)))
                  (delete-region (mark) (point))
                  (tpu-unselect t))))
           ((tpu-check-match)
            (let ((beg (tpu-match-beginning)) (end (tpu-match-end)))
              (setq tpu-last-deleted-region (buffer-substring beg end))
              (delete-region beg end)
              (tpu-unset-match)))
           (t
            (beep) (message "No selection active."))))

(defun tpu-store-text nil
  "Copy the selected region to the cut buffer without deleting it.
The text is saved for the tpu-paste command."
  (interactive)
    (cond ((mark)
           (cond (tpu-rectangular-p
                  (save-excursion
                    (tpu-arrange-rectangle)
                    (setq picture-killed-rectangle
                          (extract-rectangle (point) (mark))))
                  (tpu-unselect t))
                 (t
                  (setq tpu-last-deleted-region
                        (buffer-substring (mark) (point)))
                  (tpu-unselect t))))
           ((tpu-check-match)
            (setq tpu-last-deleted-region
                  (buffer-substring (tpu-match-beginning) (tpu-match-end)))
            (tpu-unset-match))
           (t
            (beep) (message "No selection active."))))

(defun tpu-append-region nil
  "Delete the selected region and append it to the tpu-cut buffer."
  (interactive)
  (cond ((mark)
         (let ((beg (region-beginning)) (end (region-end)))
           (setq tpu-last-deleted-region
                 (concat tpu-last-deleted-region
                         (buffer-substring beg end)))
           (delete-region beg end)
           (tpu-unselect t)))
        ((tpu-check-match)
         (let ((beg (tpu-match-beginning)) (end (tpu-match-end)))
           (setq tpu-last-deleted-region
                 (concat tpu-last-deleted-region
                         (buffer-substring beg end)))
           (delete-region beg end)
           (tpu-unset-match)))
        (t
         (beep) (message "No selection active."))))

(defun delete-current-line (num)
  "Delete one or specified number of lines after point.
This includes the newline character at the end of each line.
They are saved for the TPU-edt undelete-lines command."
  (interactive "p")
  (let ((beg (point)))
    (forward-line num)
    (if (not (eq (preceding-char) ?\n))
        (insert "\n"))
    (setq tpu-last-deleted-lines
          (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun delete-to-eol (num)
  "Delete text up to end of line.
With argument, delete up to to Nth line-end past point.
They are saved for the TPU-edt undelete-lines command."
  (interactive "p")
  (let ((beg (point)))
    (forward-char 1)
    (end-of-line num)
    (setq tpu-last-deleted-lines
          (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun delete-to-bol (num)
  "Delete text back to beginning of line.
With argument, delete up to to Nth line-end past point.
They are saved for the TPU-edt undelete-lines command."
  (interactive "p")
  (let ((beg (point)))
    (backward-char 1)
    (beginning-of-line num)
    (setq tpu-last-deleted-lines
          (buffer-substring (point) beg))
    (delete-region (point) beg)))

(defun delete-current-word (num)
  "Delete one or specified number of words after point.
They are saved for the TPU-edt undelete-words command."
  (interactive "p")
  (let ((beg (point)))
    (forward-to-word num)
    (setq tpu-last-deleted-words
          (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun delete-previous-word (num)
  "Delete one or specified number of words before point.
They are saved for the TPU-edt undelete-words command."
  (interactive "p")
  (let ((beg (point)))
    (backward-to-word num)
    (setq tpu-last-deleted-words
          (buffer-substring (point) beg))
    (delete-region beg (point))))

(defun delete-current-char (num)
  "Delete one or specified number of characters after point.  The last
character deleted is saved for the TPU-edt undelete-char command."
  (interactive "p")
  (while (and (> num 0) (not (eobp)))
    (setq tpu-last-deleted-char (char-after (point)))
    (cond (overwrite-mode
           (picture-clear-column 1)
           (forward-char 1))
          (t
           (delete-char 1)))
    (setq num (1- num))))


;;;
;;;  Undelete
;;;
(defun tpu-paste (num)
  "Insert the last region or rectangle of killed text.
With argument reinserts the text that many times."
  (interactive "p")
  (while (> num 0)
    (cond (tpu-rectangular-p
           (let ((beg (point)))
             (save-excursion
               (picture-yank-rectangle (not overwrite-mode))
               (message ""))
             (goto-char beg)))
          (t
           (insert tpu-last-deleted-region)))
    (setq num (1- num))))

(defun undelete-lines (num)
  "Insert lines deleted by last TPU-edt line-deletion command.
With argument reinserts lines that many times."
  (interactive "p")
  (let ((beg (point)))
    (while (> num 0)
      (insert tpu-last-deleted-lines)
      (setq num (1- num)))
    (goto-char beg)))

(defun undelete-words (num)
  "Insert words deleted by last TPU-edt word-deletion command.
With argument reinserts words that many times."
  (interactive "p")
  (let ((beg (point)))
    (while (> num 0)
      (insert tpu-last-deleted-words)
      (setq num (1- num)))
    (goto-char beg)))

(defun undelete-char (num)
  "Insert character deleted by last TPU-edt character-deletion command.
With argument reinserts the character that many times."
  (interactive "p")
  (while (> num 0)
    (if overwrite-mode (prog1 (forward-char -1) (delete-char 1)))
    (insert tpu-last-deleted-char)
    (forward-char -1)
    (setq num (1- num))))


;;;
;;;  Replace and Substitute
;;;
(defun tpu-replace nil
  "Replace the selected region with the contents of the cut buffer."
  (interactive)
  (cond ((mark)
         (let ((beg (region-beginning)) (end (region-end)))
           (setq tpu-last-replaced-text (buffer-substring beg end))
           (delete-region beg end)
           (insert tpu-last-deleted-region)
           (tpu-unselect t)))
        ((tpu-check-match)
         (let ((beg (tpu-match-beginning)) (end (tpu-match-end)))
           (setq tpu-last-replaced-text (buffer-substring beg end))
           (replace-match tpu-last-deleted-region
                          (not case-replace) (not tpu-regexp-p))
           (tpu-unset-match)))
        (t
         (beep) (message "No selection active."))))

(defun tpu-substitute (num)
  "Replace the selected region with the contents of the cut buffer, and
repeat most recent search.  A numeric argument serves as a repeat count.
A negative argument means replace all occurrences of the search string."
  (interactive "p")
  (cond ((or (mark) (tpu-check-match))
         (while (and (not (= num 0)) (or (mark) (tpu-check-match)))
           (let ((beg (point)))
             (tpu-replace)
             (if searching-forward (forward-char -1) (goto-char beg))
             (tpu-search-again))
           (setq num (1- num))))
        (t
         (beep) (message "No selection active."))))

(defun tpu-lm-replace (from to)
  "Interactively search for OLD-string and substitute NEW-string."
  (interactive (list (tpu-regexp-prompt "Old String: ")
                     (tpu-regexp-prompt "New String: ")))

  (let ((doit t) (strings 0))

    ;; Can't replace null strings
    (if (string= "" from) (error "No string to replace."))

    ;; Find the first occurrence
    (tpu-set-search)
    (tpu-search-internal from t)

    ;; Loop on replace question - yes, no, all, last, or quit.
    (while doit
      (if (not (tpu-check-match)) (setq doit nil)
        (progn (message "Replace? Type Yes, No, All, Last, or Quit: ")
               (let ((ans (read-char)))

                 (cond ((or (= ans ?y) (= ans ?Y) (= ans ?\r) (= ans ?\ ))
                        (let ((beg (point)))
                          (replace-match to (not case-replace) (not tpu-regexp-p))
                          (setq strings (1+ strings))
                          (if searching-forward (forward-char -1) (goto-char beg)))
                        (tpu-search-internal from t))

                       ((or (= ans ?n) (= ans ?N) (= ans ?\C-?))
                        (tpu-search-internal from t))

                       ((or (= ans ?a) (= ans ?A))
                        (save-excursion
                          (let ((beg (point)))
                            (replace-match to (not case-replace) (not tpu-regexp-p))
                            (setq strings (1+ strings))
                            (if searching-forward (forward-char -1) (goto-char beg)))
                          (tpu-search-internal from t)
                          (while (tpu-check-match)
                            (let ((beg (point)))
                              (replace-match to (not case-replace) (not tpu-regexp-p))
                              (setq strings (1+ strings))
                              (if searching-forward (forward-char -1) (goto-char beg)))
                            (tpu-search-internal from t)))
                        (setq doit nil))

                       ((or (= ans ?l) (= ans ?L))
                        (let ((beg (point)))
                          (replace-match to (not case-replace) (not tpu-regexp-p))
                          (setq strings (1+ strings))
                          (if searching-forward (forward-char -1) (goto-char beg)))
                        (setq doit nil))

                       ((or (= ans ?q) (= ans ?Q))
                        (setq doit nil)))))))

    (message "Replaced %s occurrence%s." strings
             (if (not (= 1 strings)) "s" ""))))

(defun tpu-emacs-replace (&optional dont-ask)
  "A TPU-edt interface to the emacs replace functions.  If TPU-edt is
currently in regular expression mode, the emacs regular expression
replace functions are used.  If an argument is supplied, replacements
are performed without asking.  Only works in forward direction."
  (interactive "P")
  (cond (dont-ask
         (setq current-prefix-arg nil)
         (call-interactively
          (if tpu-regexp-p 'replace-regexp 'replace-string)))
        (t
         (call-interactively
          (if tpu-regexp-p 'query-replace-regexp 'query-replace)))))

(defun tpu-add-at-bol (text)
  "Add text to the beginning of each line."
  (interactive "sString to add: ")
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "^" text)))

(defun tpu-add-at-eol (text)
  "Add text to the end of each line."
  (interactive "sString to add: ")
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "$" text)))


;;;
;;;  Position
;;;
(defun tpu-next-line (num)
  "Move to next line.
Prefix argument serves as a repeat count."
  (interactive "p")
  (next-line-internal num)
  (setq this-command 'next-line))

(defun tpu-previous-line (num)
  "Move to previous line.
Prefix argument serves as a repeat count."
  (interactive "p")
  (next-line-internal (- num))
  (setq this-command 'previous-line))

(defun next-beginning-of-line (num)
  "Move to beginning of line; if at beginning, move to beginning of next line.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (backward-char)
  (beginning-of-line num))

(defun tpu-end-of-line (num)
  "Move to the next end of line in the current direction.
A repeat count means move that many lines."
  (interactive "p")
  (if tpu-advance (next-end-of-line num) (previous-end-of-line num)))

(defun next-end-of-line (num)
  "Move to end of line; if at end, move to end of next line.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (forward-char)
  (end-of-line num))

(defun previous-end-of-line (num)
  "Move EOL upward.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (end-of-line (- 1 num)))

(defun tpu-line (num)
  "Move to the beginning of the next line in the current direction.
A repeat count means move that many lines."
  (interactive "p")
  (if tpu-advance (tpu-forward-line num) (tpu-backward-line num)))

(defun tpu-forward-line (num)
  "Move to beginning of next line.
Prefix argument serves as a repeat count."
  (interactive "p")
  (forward-line num))

(defun tpu-backward-line (num)
  "Move to beginning of previous line.
Prefix argument serves as repeat count."
  (interactive "p")
  (forward-line (- num)))

(defun tpu-paragraph (num)
  "Move to the next paragraph in the current direction.
A repeat count means move that many paragraphs."
  (interactive "p")
  (if tpu-advance
      (tpu-next-paragraph num) (tpu-previous-paragraph num)))

(defun tpu-next-paragraph (num)
  "Move to beginning of the next paragraph.
Accepts a prefix argument for the number of paragraphs."
  (interactive "p")
  (beginning-of-line)
  (while (and (not (eobp)) (> num 0))
    (if (re-search-forward "^[ \t]*$" nil t)
        (if (re-search-forward "[^ \t\n]" nil t)
            (goto-char (match-beginning 0))
          (goto-char (point-max))))
    (setq num (1- num))))

(defun tpu-previous-paragraph (num)
  "Move to beginning of previous paragraph.
Accepts a prefix argument for the number of paragraphs."
  (interactive "p")
  (end-of-line)
  (while (and (not (bobp)) (> num 0))
    (if (not (and (re-search-backward "^[ \t]*$" nil t)
                  (re-search-backward "[^ \t\n]" nil t)
                  (re-search-backward "^[ \t]*$" nil t)
                  (progn (re-search-forward "[^ \t\n]" nil t)
                         (goto-char (match-beginning 0)))))
        (goto-char (point-min)))
    (setq num (1- num))))

(defun move-to-beginning nil
  "Move cursor to the beginning of buffer, but don't set the mark."
  (interactive)
  (goto-char (point-min)))

(defun move-to-end nil
  "Move cursor to the end of buffer, but don't set the mark."
  (interactive)
  (goto-char (point-max))
  (recenter -1))

(defun goto-percent (perc)
  "Move point to ARG percentage of the buffer."
  (interactive "NGoto-percentage: ")
  (if (or (> perc 100) (< perc 0))
      (error "Percentage %d out of range 0 < percent < 100" perc)
    (goto-char (/ (* (point-max) perc) 100))))


;;;
;;;  Movement by character
;;;
(defun tpu-char (num)
  "Move to the next character in the current direction.
A repeat count means move that many characters."
  (interactive "p")
  (if tpu-advance (forward-char num) (backward-char num)))


;;;
;;;  Movement by word
;;;
(defconst tpu-word-separator-list '()
  "List of additional word separators.")
(defconst tpu-skip-chars "^ \t"
  "Characters to skip when moving by word.
Additional word separators are added to this string.")

(defun tpu-word (num)
  "Move to the beginning of the next word in the current direction.
A repeat count means move that many words."
  (interactive "p")
  (if tpu-advance (forward-to-word num) (backward-to-word num)))

(defun forward-to-word (num)
  "Move forward until encountering the beginning of a word.
With argument, do this that many times."
  (interactive "p")
  (while (and (> num 0) (not (eobp)))
    (let* ((beg (point))
           (end (prog2 (end-of-line) (point) (goto-char beg))))
      (cond ((eolp)
             (forward-char 1))
            ((memq (char-after (point)) tpu-word-separator-list)
             (forward-char 1)
             (skip-chars-forward " \t" end))
            (t
             (skip-chars-forward tpu-skip-chars end)
             (skip-chars-forward " \t" end))))
    (setq num (1- num))))

(defun backward-to-word (num)
  "Move backward until encountering the beginning of a word.
With argument, do this that many times."
  (interactive "p")
  (while (and (> num 0) (not (bobp)))
    (let* ((beg (point))
           (end (prog2 (beginning-of-line) (point) (goto-char beg))))
      (cond ((bolp)
             ( forward-char -1))
            ((memq (char-after (1- (point)))  tpu-word-separator-list)
             (forward-char -1))
            (t
             (skip-chars-backward " \t" end)
             (skip-chars-backward tpu-skip-chars end)
             (if (and (not (bolp)) (= ?  (char-syntax (char-after (point)))))
                 (forward-char -1)))))
    (setq num (1- num))))

(defun tpu-add-word-separators (separators)
  "Add new word separators for TPU-edt word commands."
  (interactive "sSeparators: ")
  (let* ((n 0) (length (length separators)))
    (while (< n length)
      (let ((char (aref separators n))
            (ss (substring separators n (1+ n))))
        (cond ((not (memq char tpu-word-separator-list))
               (setq tpu-word-separator-list
                     (append ss tpu-word-separator-list))
               (cond ((= char ?-)
                      (setq tpu-skip-chars (concat tpu-skip-chars "\\-")))
                     ((= char ?\\)
                      (setq tpu-skip-chars (concat tpu-skip-chars "\\\\")))
                     ((= char ?^)
                      (setq tpu-skip-chars (concat tpu-skip-chars "\\^")))
                     (t
                      (setq tpu-skip-chars (concat tpu-skip-chars ss))))))
        (setq n (1+ n))))))

(defun tpu-reset-word-separators nil
  "Reset word separators to default value."
  (interactive)
  (setq tpu-word-separator-list nil)
  (setq tpu-skip-chars "^ \t"))

(defun tpu-set-word-separators (separators)
  "Set new word separators for TPU-edt word commands."
  (interactive "sSeparators: ")
  (tpu-reset-word-separators)
  (tpu-add-word-separators separators))


;;;
;;;  Movement by page
;;;
(defun tpu-page (num)
  "Move to the next page in the current direction.
A repeat count means move that many pages."
  (interactive "p")
  (if tpu-advance (forward-page num) (backward-page num))
  (if (eobp) (recenter -1)))


;;;
;;;  Window
;;;
(defun tpu-scroll-window (num)
  "Scroll the display to the next section in the current direction.
A repeat count means scroll that many sections."
  (interactive "p")
  (if tpu-advance (scroll-window-up num) (scroll-window-down num)))

(defun scroll-window-down (num)
  "Scroll the display down to the next section.
A repeat count means scroll that many sections."
  (interactive "p")
  (let* ((beg (current-line))
         (height (1- (window-height)))
         (lines (* num (/ (* height tpu-percent-scroll) 100))))
    (next-line-internal (- lines))
    (if (> lines beg) (recenter 0))))

(defun scroll-window-up (num)
  "Scroll the display up to the next section.
A repeat count means scroll that many sections."
  (interactive "p")
  (let* ((beg (current-line))
         (height (1- (window-height)))
         (lines (* num (/ (* height tpu-percent-scroll) 100))))
    (next-line-internal lines)
    (if (>= (+ lines beg) height) (recenter -1))))

(defun beginning-of-window nil
  "Move cursor to top of window."
  (interactive)
  (move-to-window-line 0))

(defun end-of-window nil
  "Move cursor to bottom of window."
  (interactive)
  (move-to-window-line -1))

(defun line-to-bottom-of-window nil
  "Move the current line to the bottom of the window."
  (interactive)
  (recenter -1))

(defun line-to-top-of-window nil
  "Move the current line to the top of the window."
  (interactive)
  (recenter 0))

(defun tpu-pan-right (num)
  "Pan right 16 characters.
Accepts a prefix argument for the number of 16 character sets to scroll."
  (interactive "p")
  (scroll-left (* 16 num)))

(defun tpu-pan-left (num)
  "Pan left 16 characters.
Accepts a prefix argument for the number of 16 character sets to scroll."
  (interactive "p")
  (scroll-right (* 16 num)))

(defun tpu-next-window nil
  "Move to the next window."
  (interactive)
  (if (one-window-p) (message "There is only one window on screen.")
    (other-window 1)))

(defun tpu-previous-window nil
  "Move to the previous window."
  (interactive)
  (if (one-window-p) (message "There is only one window on screen.")
    (select-window (previous-window))))


;;;
;;;  Keyboard-style specific
;;;
(defun DEC-mode nil
  "Defines the DEC style keyboard."
  (interactive)
  (global-set-key '(control h) 'next-beginning-of-line)        ; ^H (BS)
  )

(defun SGI-mode nil
  "Defines the SGI style keyboard."
  (interactive)
  (global-set-key '(control h) 'backward-delete-char)          ; ^H (BS)
  )


;;;
;;;  Direction
;;;
(defun advance-direction nil
  "Set TPU Advance mode so keypad commands move forward."
  (interactive)
  (setq tpu-direction-string " Advance")
  (setq tpu-advance t)
  (setq tpu-reverse nil)
  (tpu-set-search)
  (update-mode-line))

(defun backup-direction nil
  "Set TPU Backup mode so keypad commands move backward."
  (interactive)
  (setq tpu-direction-string " Reverse")
  (setq tpu-advance nil)
  (setq tpu-reverse t)
  (tpu-set-search)
  (update-mode-line))


;;;
;;;  CSI-map key definitions
;;;
(global-set-key 'up 'tpu-previous-line)			    ; up
(global-set-key 'down 'tpu-next-line)			    ; down
(global-set-key 'left 'backward-char)
(global-set-key 'right 'forward-char)
;; left and right are defined in DEC-mode and SGI-mode
;; Sun type key
(global-set-key 'home 'move-to-beginning)		    ; Home
(global-set-key 'end 'move-to-end)			    ; End
(global-set-key 'pgup 'scroll-window-down)		    ; PgUp
(global-set-key 'pgdn 'scroll-window-up)		    ; PgDn
(global-set-key 'prscr nil)                                 ; PrScr
(global-set-key 'scrlck nil)				    ; ScrLck
(global-set-key 'pause nil)                                 ; Pause
(global-set-key 'delete 'backward-delete-char)		    ; Delete
;; Dec keys
(global-set-key 'find 'tpu-search)			    ; Find
(global-set-key 'insert 'tpu-paste)			    ; Insert Here
(global-set-key 'apXK_LineDel 'tpu-cut)			    ; Remove
(global-set-key 'DRemove 'tpu-cut)			    ; Remove
(global-set-key 'select 'tpu-select)			    ; Select
(global-set-key 'prior 'scroll-window-down)		    ; Prev Screen
(global-set-key 'next 'scroll-window-up)		    ; Next Screen
(global-set-key 'f1 'nil)				    ; F1
(global-set-key 'f2 'nil)				    ; F2
(global-set-key 'f3 'nil)				    ; F3
(global-set-key 'f4 'nil)				    ; F4
(global-set-key 'f5 'nil)				    ; F5
(global-set-key 'f6 'nil)				    ; F6
(global-set-key 'f7 'nil)				    ; F7
(global-set-key 'f8 'nil)				    ; F8
(global-set-key 'f9 'nil)				    ; F9
(global-set-key 'f10 'nil)				    ; F10 (was exit)
(global-set-key 'f11 'insert-escape)			    ; F11 (ESC)
(global-set-key 'f12 'next-beginning-of-line)		    ; F12 (BS)
(global-set-key 'f13 'delete-previous-word)		    ; F13 (LF)
(global-set-key 'f14 'toggle-overwrite-mode)		    ; F14
(global-set-key 'help 'tpu-help)                            ; HELP
(global-set-key 'menu 'execute-extended-command)            ; DO
(global-set-key 'f17 'goto-breadcrumb)			    ; F17
(global-set-key 'f18 'nil)				    ; F18
(global-set-key 'f19 'nil)				    ; F19
(global-set-key 'f20 'nil)				    ; F20


;;;
;;;  SS3-map key definitions
;;;
(global-set-key 'kp_f2 'tpu-help)				; PF2
(global-set-key 'kp_f3 'tpu-search-again)			; PF3
(global-set-key 'kp_f4 'delete-current-line)			; PF4
(global-set-key 'kp_equal 'describe-key)			; Apple KP=
(global-set-key 'kp_multiply 'delete-current-line)		; Apple KP*
(global-set-key 'kp_plus 'delete-current-char)			; Apple KP+
(global-set-key 'kp_separator 'delete-current-char)		; KP,
(global-set-key 'kp_subtract 'delete-current-word)		; KP-
(global-set-key 'kp_decimal 'tpu-select)			; KP.
(global-set-key 'kp_0 'tpu-line)				; KP0
(global-set-key 'kp_1 'tpu-word)				; KP1
(global-set-key 'kp_2 'tpu-end-of-line)				; KP2
(global-set-key 'kp_3 'tpu-char)				; KP3
(global-set-key 'kp_4 'advance-direction)			; KP4
(global-set-key 'kp_5 'backup-direction)			; KP5
(global-set-key 'kp_6 'tpu-cut)					; KP6
(global-set-key 'kp_7 'tpu-page)				; KP7
(global-set-key 'kp_8 'tpu-scroll-window)			; KP8
(global-set-key 'kp_9 'tpu-append-region)			; KP9
(global-set-key 'kp_enter 'newline)				; KPenter
								;
								;
;;;
;;;  GOLD-map key definitions
;;;
(global-set-key '[kp_f1 (control a)] 'nil)			; ^A
(global-set-key '[kp_f1 (control b)] 'nil)			; ^B
(global-set-key '[kp_f1 (control c)] 'nil)			; ^C
(global-set-key '[kp_f1 (control d)] 'nil)			; ^D
(global-set-key '[kp_f1 (control e)] 'nil)			; ^E
(global-set-key '[kp_f1 (control f)] 'nil)			; ^F
(global-set-key '[kp_f1 (control g)] 'keyboard-quit)		; safety first
(global-set-key '[kp_f1 (control h)] 'delete-other-windows)	; BS
(global-set-key '[kp_f1 (control i)] 'other-window)		; TAB
(global-set-key '[kp_f1 (control j)] 'nil)			; ^J
(global-set-key '[kp_f1 (control k)] 'nil)			; ^K
(global-set-key '[kp_f1 (control l)] 'downcase-region)		; ^L
(global-set-key '[kp_f1 (control m)] 'nil)			; ^M
(global-set-key '[kp_f1 (control n)] 'nil)			; ^N
(global-set-key '[kp_f1 (control o)] 'nil)			; ^O
(global-set-key '[kp_f1 (control p)] 'nil)			; ^P
(global-set-key '[kp_f1 (control q)] 'nil)			; ^Q
(global-set-key '[kp_f1 (control r)] 'nil)			; ^R
(global-set-key '[kp_f1 (control s)] 'nil)			; ^S
(global-set-key '[kp_f1 (control t)] 'nil)			; ^T
(global-set-key '[kp_f1 (control u)] 'upcase-region)		; ^U
(global-set-key '[kp_f1 (control v)] 'nil)			; ^V
(global-set-key '[kp_f1 (control w)] 'write-current-buffers)	; ^W
(global-set-key '[kp_f1 (control x)] 'nil)			; ^X
(global-set-key '[kp_f1 (control y)] 'nil)			; ^Y
(global-set-key '[kp_f1 (control z)] 'nil)			; ^Z
(global-set-key '[kp_f1 ? ] 'undo)				; SPC
(global-set-key '[kp_f1 ?!] 'change-counter-format)		; !
(global-set-key '[kp_f1 ?#] 'insert-counter)			; #
(global-set-key '[kp_f1 ?$] 'tpu-add-at-eol)			; $
(global-set-key '[kp_f1 ?%] 'goto-percent)			; %
(global-set-key '[kp_f1 ?&] nil)				; &
(global-set-key '[kp_f1 ?(] nil)				; (
(global-set-key '[kp_f1 ?)] nil)				; )
(global-set-key '[kp_f1 ?*] 'tpu-toggle-regexp)			; *
(global-set-key '[kp_f1 ?+] 'region-length-to-counter)		; +
(global-set-key '[kp_f1 ?,] 'goto-breadcrumb)			; ,
(global-set-key '[kp_f1 ?-] 'repeat-command--)			; -
(global-set-key '[kp_f1 ?.] 'drop-breadcrumb)			; .
(global-set-key '[kp_f1 ?/] 'tpu-emacs-replace)			; /
(global-set-key '[kp_f1 ?0] 'repeat-command-0)			; 0
(global-set-key '[kp_f1 ?1] 'repeat-command-1)			; 1
(global-set-key '[kp_f1 ?2] 'repeat-command-2)			; 2
(global-set-key '[kp_f1 ?3] 'repeat-command-3)			; 3
(global-set-key '[kp_f1 ?4] 'repeat-command-4)			; 4
(global-set-key '[kp_f1 ?5] 'repeat-command-5)			; 5
(global-set-key '[kp_f1 ?6] 'repeat-command-6)			; 6
(global-set-key '[kp_f1 ?7] 'repeat-command-7)			; 7
(global-set-key '[kp_f1 ?8] 'repeat-command-8)			; 8
(global-set-key '[kp_f1 ?9] 'repeat-command-9)			; 9
(global-set-key '[kp_f1 ?:] 'nil)				; :
(global-set-key '[kp_f1 ?;] 'tpu-trim-line-ends)                ; ;
(global-set-key '[kp_f1 ?<] 'decr-counter)			; <
(global-set-key '[kp_f1 ?=] 'set-counter)			; =
(global-set-key '[kp_f1 ?>] 'incr-counter)			; >
(global-set-key '[kp_f1 ??] 'spell-check)			; ?
(global-set-key '[kp_f1 ?A] 'toggle-newline-and-indent)		; A
(global-set-key '[kp_f1 ?B] 'next-buffer)			; B
(global-set-key '[kp_f1 ?C] 'repeat-complex-command)		; C
(global-set-key '[kp_f1 ?D] 'shell-command)			; D
(global-set-key '[kp_f1 ?E] nil)				; E (was exit)
(global-set-key '[kp_f1 ?F] 'nil)				; F
(global-set-key '[kp_f1 ?G] 'Get)				; G
(global-set-key '[kp_f1 ?H] 'nil)				; H
(global-set-key '[kp_f1 ?I] 'include)				; I
(global-set-key '[kp_f1 ?K] 'tpu-kill-buffer)			; K
(global-set-key '[kp_f1 ?L] 'tpu-what-line)			; L
(global-set-key '[kp_f1 ?M] 'buffer-menu)			; M
(global-set-key '[kp_f1 ?N] 'next-file-buffer)			; N
(global-set-key '[kp_f1 ?O] 'occur)				; O
(global-set-key '[kp_f1 ?P] 'lpr-buffer)			; P
(global-set-key '[kp_f1 ?Q] 'nil)				; Q (was quit)
(global-set-key '[kp_f1 ?R] 'tpu-toggle-rectangle)		; R
(global-set-key '[kp_f1 ?S] 'replace)				; S
(global-set-key '[kp_f1 ?T] 'line-to-top-of-window)		; T
(global-set-key '[kp_f1 ?U] 'undo)				; U
(global-set-key '[kp_f1 ?V] 'tpu-version)			; V
(global-set-key '[kp_f1 ?W] 'save-buffer)			; W
(global-set-key '[kp_f1 ?X] 'save-all-buffers-kill-emacs)	; X
(global-set-key '[kp_f1 ?Y] 'copy-region-as-kill)		; Y
(global-set-key '[kp_f1 ?Z] 'suspend-emacs)			; Z
(global-set-key '[kp_f1 ?\[] 'blink-matching-open)		; [
(global-set-key '[kp_f1 ?\\] 'nil)				; \
(global-set-key '[kp_f1 ?\]] 'blink-matching-open)		; ]
(global-set-key '[kp_f1 ?^] 'tpu-add-at-bol)			; ^
(global-set-key '[kp_f1 ?_] 'split-window-vertically)		; -
(global-set-key '[kp_f1 ?`] 'what-line)				; `
(global-set-key '[kp_f1 ?a] 'toggle-newline-and-indent)		; a
(global-set-key '[kp_f1 ?b] 'next-buffer)			; b
(global-set-key '[kp_f1 ?c] 'repeat-complex-command)		; c
(global-set-key '[kp_f1 ?d] 'shell-command)			; d
(global-set-key '[kp_f1 ?e] 'nil)				; e (was exit)
(global-set-key '[kp_f1 ?f] 'nil)				; f
(global-set-key '[kp_f1 ?g] 'Get)				; g
(global-set-key '[kp_f1 ?h] 'nil)				; h
(global-set-key '[kp_f1 ?i] 'include)				; i
(global-set-key '[kp_f1 ?k] 'tpu-kill-buffer)			; k
(global-set-key '[kp_f1 ?l] 'goto-line)				; l
(global-set-key '[kp_f1 ?m] 'buffer-menu)			; m
(global-set-key '[kp_f1 ?n] 'next-file-buffer)			; n
(global-set-key '[kp_f1 ?o] 'occur)				; o
(global-set-key '[kp_f1 ?p] 'lpr-region)			; p
(global-set-key '[kp_f1 ?q] 'nil)				; q (was quit)
(global-set-key '[kp_f1 ?r] 'tpu-toggle-rectangle)		; r
(global-set-key '[kp_f1 ?s] 'replace)				; s
(global-set-key '[kp_f1 ?t] 'line-to-top-of-window)		; t
(global-set-key '[kp_f1 ?u] 'undo)				; u
(global-set-key '[kp_f1 ?v] 'tpu-version)			; v
(global-set-key '[kp_f1 ?w] 'save-buffer)			; w
(global-set-key '[kp_f1 ?x] 'save-all-buffers-kill-emacs)	; x
(global-set-key '[kp_f1 ?y] 'copy-region-as-kill)		; y
(global-set-key '[kp_f1 ?z] 'suspend-emacs)			; z
(global-set-key '[kp_f1 ?{] 'nil)				; {
(global-set-key '[kp_f1 ?|] 'split-window-horizontally)		; |
(global-set-key '[kp_f1 ?}] 'nil)				; }
(global-set-key '[kp_f1 ?~] 'exchange-point-and-mark)		; ~
(global-set-key '[kp_f1 delete] 'delete-window)			; <X]


;;;
;;;  GOLD-CSI-map key definitions
;;;
(global-set-key '[kp_f1 find] 'nil)				; Find
(global-set-key '[kp_f1 insert] 'nil)				; Insert Here
(global-set-key '[kp_f1 apXK_LineDel] 'tpu-store-text)		; Remove
(global-set-key '[kp_f1 select] 'tpu-unselect)			; Select
(global-set-key '[kp_f1 prior] 'tpu-previous-window)		; Prev Screen
(global-set-key '[kp_f1 next] 'tpu-next-window)                 ; Next Screen

(global-set-key '[kp_f1 f1] 'nil)				; F1
(global-set-key '[kp_f1 f2] 'nil)				; F2
(global-set-key '[kp_f1 f3] 'nil)				; F3
(global-set-key '[kp_f1 f4] 'nil)				; F4
(global-set-key '[kp_f1 f5] 'nil)			    	; F5
(global-set-key '[kp_f1 f6] 'nil)		     		; F6
(global-set-key '[kp_f1 f7] 'nil)				; F7
(global-set-key '[kp_f1 f8] 'nil)				; F8
(global-set-key '[kp_f1 f9] 'nil)				; F9
(global-set-key '[kp_f1 f10] 'nil)				; F10
(global-set-key '[kp_f1 f11] 'nil)				; F11
(global-set-key '[kp_f1 f12] 'nil)				; F12
(global-set-key '[kp_f1 f13] 'nil)				; F13
(global-set-key '[kp_f1 f14] 'nil)				; F14
(global-set-key '[kp_f1 help] 'describe-bindings)		; HELP
(global-set-key '[kp_f1 menu] 'nil)				; DO
(global-set-key '[kp_f1 f17] 'drop-breadcrumb)			; F17
(global-set-key '[kp_f1 f18] 'nil)				; F18
(global-set-key '[kp_f1 f19] 'nil)				; F19
(global-set-key '[kp_f1 f20] 'nil)				; F20
(global-set-key '[kp_f1 up] 'move-to-beginning)			; up-arrow
(global-set-key '[kp_f1 down] 'move-to-end)			; down-arrow
(global-set-key '[kp_f1 right] 'end-of-line)			; right-arrow
(global-set-key '[kp_f1 left] 'beginning-of-line)		; left-arrow
(global-set-key '[kp_f1 home] 'move-to-beginning)		; Home
(global-set-key '[kp_f1 delete] 'backward-delete-char)		; Delete
(global-set-key '[kp_f1 end] 'move-to-end)			; End
(global-set-key '[kp_f1 pgup] 'scroll-window-down)		; PgUp
(global-set-key '[kp_f1 pgdn] 'scroll-window-up)		; PgDn
(global-set-key '[kp_f1 prscr] nil)				; PrScr
(global-set-key '[kp_f1 scrlck] nil)				; ScrLck
(global-set-key '[kp_f1 pause] nil)				; Pause


;;;
;;;  GOLD-SS3-map key definitions
;;;
(global-set-key '[kp_f1 kp_f1] 'keyboard-quit)			; PF1
(global-set-key '[kp_f1 kp_f2] 'help-for-help)			; PF2
(global-set-key '[kp_f1 kp_f3] 'tpu-search)			; PF3
(global-set-key '[kp_f1 kp_f4] 'undelete-lines)			; PF4
(global-set-key '[kp_f1 kp_0] 'open-line)			; KP0
(global-set-key '[kp_f1 kp_1] 'tpu-change-case)			; KP1
(global-set-key '[kp_f1 kp_2] 'delete-to-eol)			; KP2
(global-set-key '[kp_f1 kp_3] 'tpu-special-insert)		; KP3
(global-set-key '[kp_f1 kp_4] 'move-to-end)			; KP4
(global-set-key '[kp_f1 kp_5] 'move-to-beginning)		; KP5
(global-set-key '[kp_f1 kp_6] 'tpu-paste)			; KP6
(global-set-key '[kp_f1 kp_7] 'execute-extended-command)	; KP7
(global-set-key '[kp_f1 kp_8] 'tpu-fill)			; KP8
(global-set-key '[kp_f1 kp_9] 'tpu-replace)			; KP9
(global-set-key '[kp_f1 kp_subtract] 'undelete-words)		; KP-
(global-set-key '[kp_f1 kp_separator] 'undelete-char)		; KP,
(global-set-key '[kp_f1 kp_decimal] 'tpu-unselect)		; KP.
(global-set-key '[kp_f1 kp_enter] 'tpu-substitute)		; KPenter


;;;
;;;  Repeat complex command map additions to make arrows work
;;;
;(define-key repeat-complex-command-map "\e[A" 'previous-complex-command)
;(define-key repeat-complex-command-map "\e[B" 'next-complex-command)
;(define-key repeat-complex-command-map "\eOA" 'previous-complex-command)
;(define-key repeat-complex-command-map "\eOB" 'next-complex-command)


;;;
;;;  Minibuffer map additions to make KP_enter = RET
;;;
;(define-key minibuffer-local-map "\eOM" 'exit-minibuffer)
;(define-key minibuffer-local-ns-map "\eOM" 'exit-minibuffer)
;(define-key minibuffer-local-completion-map "\eOM" 'exit-minibuffer)
;(define-key minibuffer-local-must-match-map "\eOM" 'minibuffer-complete-and-exit)
;(define-key repeat-complex-command-map "\eOM" 'exit-minibuffer)


;;;
;;;  Start and Stop TPU-edt
;;;
(defun use-tpu nil
  "Sets TPU-edt emulation on."
  (interactive)
;  (replace-global-key "\e[" CSI-map)                           ; CSI map
;  (replace-global-key "\eO" SS3-map)                           ; SS3 map
  (replace-global-key '(control ?\\) 'quoted-insert)		; ^\
  (replace-global-key '(control a) 'toggle-overwrite-mode)	; ^A
  (replace-global-key '(control b) 'repeat-complex-command)	; ^B
  (replace-global-key '(control f) 'set-visited-file-name)	; ^F
  (replace-global-key '(control j) 'delete-previous-word)	; ^J (LF)
  (replace-global-key '(control k) 'define-macro-key)		; ^K
  (replace-global-key '(control l) 'insert-formfeed)		; ^L (FF)
  (replace-global-key '(control r) 'recenter)			; ^R
  (replace-global-key '(control u) 'delete-to-bol)		; ^U
  (replace-global-key '(control v) 'quoted-insert)		; ^V
  (replace-global-key '(control w) 'redraw-display)		; ^W
;  (replace-global-key '(control z) 'exit)			; ^Z
;  (define-key SS3-map "P" GOLD-map)                            ; GOLD map
;  (define-key GOLD-map "\e[" GOLD-CSI-map)                     ; GOLD-CSI map
;  (define-key GOLD-map "\eO" GOLD-SS3-map)                     ; GOLD-SS3 map
  (require 'picture)
  (DEC-mode)
  (advance-direction)
  ;; define ispell functions
  (autoload 'ispell-word "ispell" "Check spelling of word at or before point" t)
  (autoload 'ispell-complete-word "ispell" "Complete word at or before point" t)
  (autoload 'ispell-buffer "ispell" "Check spelling of entire buffer" t)
  (autoload 'ispell-region "ispell" "Check spelling of region" t)
  ;; set display line truncation and scrolling like TPU
  (setq-default page-delimiter "\f")
  (setq-default truncate-lines t)
  (setq scroll-step 1)
  ;; Make direction of motion show in mode line
  ;; while TPU emulation is turned on.
  ;; Note that the keypad is always turned on when in Emacs.
  (or (assq 'tpu-rectangular-p minor-mode-alist)
      (setq minor-mode-alist (cons '(tpu-rectangular-p tpu-rectangle-string)
                                   minor-mode-alist)))
  (or (assq 'tpu-direction-string minor-mode-alist)
      (setq minor-mode-alist (cons '(tpu-direction-string tpu-direction-string)
                                   minor-mode-alist)))
  (or (assq 'newline-and-indent-p minor-mode-alist)
      (setq minor-mode-alist
            (cons '(newline-and-indent-p newline-and-indent-p-string)
                  minor-mode-alist))))

(defun cancel-tpu nil
  "Sets TPU-edt emulation off."
  (interactive)
;  (restore-global-key "\e[")
;  (restore-global-key "\eO")
  (restore-global-key '(control a))				;
  (restore-global-key '(control b))				;
  (restore-global-key '(control ?\\))				;
  (restore-global-key '(control f))				;
  (restore-global-key '(control h))				;
  (restore-global-key '(control j))				;
  (restore-global-key '(control k))				;
  (restore-global-key '(control l))				;
  (restore-global-key '(control r))				;
  (restore-global-key '(control u))				;
  (restore-global-key '(control v))				;
  (restore-global-key '(control w))				;
;  (restore-global-key '(control z))				;
  (setq tpu-direction-string nil)
  (setq-default page-delimiter "^\f")
  (setq-default truncate-lines nil)
  (setq scroll-step 0))

(use-tpu)
