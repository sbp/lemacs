;; Buffer menu main function and support functions.
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


; Put buffer *Buffer List* into proper mode right away
; so that from now on even list-buffers is enough to get a buffer menu.

(defvar Buffer-menu-mode-map nil "")

(if Buffer-menu-mode-map
    ()
  (setq Buffer-menu-mode-map (make-keymap))
  (suppress-keymap Buffer-menu-mode-map t)
  (set-keymap-name Buffer-menu-mode-map 'Buffer-menu-mode-map)
  (define-key Buffer-menu-mode-map "q" 'Buffer-menu-select)
  (define-key Buffer-menu-mode-map "2" 'Buffer-menu-2-window)
  (define-key Buffer-menu-mode-map "1" 'Buffer-menu-1-window)
  (define-key Buffer-menu-mode-map "f" 'Buffer-menu-this-window)
  (define-key Buffer-menu-mode-map "o" 'Buffer-menu-other-window)
  (define-key Buffer-menu-mode-map "s" 'Buffer-menu-save)
  (define-key Buffer-menu-mode-map "d" 'Buffer-menu-delete)
  (define-key Buffer-menu-mode-map "k" 'Buffer-menu-delete)
  (define-key Buffer-menu-mode-map "\C-d" 'Buffer-menu-delete-backwards)
  (define-key Buffer-menu-mode-map "\C-k" 'Buffer-menu-delete)
  (define-key Buffer-menu-mode-map "x" 'Buffer-menu-execute)
  (define-key Buffer-menu-mode-map " " 'next-line)
  (define-key Buffer-menu-mode-map "\177" 'Buffer-menu-backup-unmark)
  (define-key Buffer-menu-mode-map "~" 'Buffer-menu-not-modified)
  (define-key Buffer-menu-mode-map "?" 'describe-mode)
  (define-key Buffer-menu-mode-map "u" 'Buffer-menu-unmark)
  (define-key Buffer-menu-mode-map "m" 'Buffer-menu-mark)
  (define-key Buffer-menu-mode-map "t" 'Buffer-menu-visit-tags-table)
  (define-key Buffer-menu-mode-map 'button2 'Buffer-menu-mouse-select)
  (define-key Buffer-menu-mode-map 'button3 'Buffer-menu-popup-menu)
  )

;; Buffer Menu mode is suitable only for specially formatted data.
(put 'Buffer-menu-mode 'mode-class 'special)

(defun Buffer-menu-mode ()
  "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
Letters do not insert themselves; instead, they are commands.
m -- mark buffer to be displayed.
q -- select buffer of line point is on.
  Also show buffers marked with m in other windows.
1 -- select that buffer in full-screen window.
2 -- select that buffer in one window,
  together with buffer selected before this one in another window.
f -- select that buffer in place of the buffer menu buffer.
o -- select that buffer in another window,
  so the buffer menu buffer remains visible in its window.
t -- visit-tags-table this buffer.
~ -- clear modified-flag on that buffer.
s -- mark that buffer to be saved, and move down.
d or k -- mark that buffer to be deleted, and move down.
C-d -- mark that buffer to be deleted, and move up.
x -- delete or save marked buffers.
u -- remove all kinds of marks from current line.
Delete -- back up a line and remove marks.

Precisely,\\{Buffer-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map Buffer-menu-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'Buffer-menu-mode)
  (setq mode-name "Buffer Menu")
  (require 'mode-motion)
  (setq mode-motion-hook 'mode-motion-highlight-line)
  (run-hooks 'buffer-menu-mode-hook))

(defvar Buffer-menu-buffer-column 4)

;; from mly@ai.mit.edu (Richard Mlynarik)
(defun Buffer-menu-buffer (error-if-non-existent-p)
  "Return buffer described by this line of buffer menu."
  (save-excursion
    (beginning-of-line)
    (forward-char Buffer-menu-buffer-column)
    (let* ((start (point))
           (string (if (/= (preceding-char) ?\")
                       ;; End of buffer name marked by tab or a space.
                       (progn (re-search-forward "\t\\| ")
                              (skip-chars-backward " \t")
                              (buffer-substring start (point)))
                       (progn
                         (backward-char 1)
                         (read (current-buffer))))))
      (or (get-buffer string)
	  (if error-if-non-existent-p
	      (error "No buffer named \"%s\"" string)
	    nil)))))

(defvar list-buffers-header-line
  (purecopy (concat " MR Buffer           Size  Mode         File\n"
                    " -- ------           ----  ----         ----\n")))

(defvar list-buffers-identification 'default-list-buffers-identification
  "String used to identify this buffer, or a function of one argument
to generate such a string.  This variable is always buffer-local.")
(make-variable-buffer-local 'list-buffers-identification)

(defvar list-buffers-directory)
(make-variable-buffer-local 'list-buffers-directory)

(defun default-list-buffers-identification (output)
  (save-excursion
    (let ((file (or (buffer-file-name (current-buffer))
		    (and (boundp 'list-buffers-directory)
			 list-buffers-directory)))
	  (size (buffer-size))
	  (mode mode-name)
	  eob p s col)
      (set-buffer output)
      (end-of-line)
      (setq eob (point))
      (prin1 size)
      (setq p (point))
      ;; right-justify the size
      (move-to-column 19 t)
      (setq col (point))
      (if (> eob col)
	  (goto-char eob))
      (setq s (- 6 (- p col)))
      (while (> s 0) ; speed/consing tradeoff...
	(insert ? )
	(setq s (1- s)))
      (end-of-line)
      (indent-to 27 1)
      (insert mode)
      (if (not file)
	  nil
	;; if the mode-name is really long, clip it for the filename
	(if (> 0 (setq s (- 39 (current-column))))
	    (delete-char (max s (- eob (point)))))
	(indent-to 40 1)
	(insert file)))))

(defun list-buffers-internal (output &optional predicate)
  (let ((current (current-buffer))
        (buffers (buffer-list)))
    (save-excursion
      (set-buffer output)
      (Buffer-menu-mode)
      (setq buffer-read-only nil)
      (buffer-disable-undo output)
      (insert list-buffers-header-line)

      (while buffers
        (let* ((col1 19)
               (buffer (car buffers))
               (name (buffer-name buffer))
               (file (buffer-file-name buffer)))
          (setq buffers (cdr buffers))
          (cond ((null name))           ;deleted buffer
                ((and predicate
                      (not (if (stringp predicate)
                               (string-match predicate name)
                               (funcall predicate buffer))))
                 nil)
                (t
                 (set-buffer buffer)
                 (let ((ro buffer-read-only)
                       (id list-buffers-identification))
                   (set-buffer output)
                   (insert (if (eq buffer current)
                               (progn (setq current (point)) ?\.)
                               ?\ ))
                   (insert (if (buffer-modified-p buffer)
                               ?\* 
                               ?\ ))
                   (insert (if ro
                               ?\%
                               ?\ ))
                   (if (string-match "[\n\"\\ \t]" name)
                       (let ((print-escape-newlines t))
                         (prin1 name output))
                       (insert ?\  name))
                   (indent-to col1 1)
                   (cond ((stringp id)
                          (insert id))
                         (id
                          (set-buffer buffer)
                          (condition-case e
                              (funcall id output)
                            (error
                             (princ "***" output) (prin1 e output)))
                          (set-buffer output)
                          (goto-char (point-max)))))
                 (insert ?\n)))))

      (setq buffer-read-only t)
      (if (not (bufferp current))
          (goto-char current)))))

(defun list-buffers (&optional files-only)
  "Display a list of names of existing buffers.
Inserts it in buffer *Buffer List* and displays that.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

The M column contains a * for buffers that are modified.
The R column contains a % for buffers that are read-only."
  (interactive (list (if current-prefix-arg t nil)))
  (with-output-to-temp-buffer "*Buffer List*"
    (save-excursion
      (list-buffers-internal standard-output
                             (if (memq files-only '(t nil))
                                 #'(lambda (b)
                                     (let ((n (buffer-name b)))
                                       (cond ((and (/= 0 (length n))
                                                   (= (aref n 0) ?\ ))
                                              ;;don't mention if starts with " "
                                              nil)
                                             (files-only
                                              (buffer-file-name b))
                                             (t
                                              t))))
                                 files-only)))))

(defun buffer-menu (arg)
  "Make a menu of buffers so you can save, delete or select them.
With argument, show only buffers that are visiting files.
Type ? after invocation to get help on commands available.
Type q immediately to make the buffer menu go away."
  (interactive "P")
  (list-buffers arg)
  (pop-to-buffer "*Buffer List*")
  (forward-line 2)
  (message
   "Commands: d, s, x; 1, 2, m, u, q; delete; ~;  ? for help."))

(defun Buffer-menu-mark ()
  "Mark buffer on this line for being displayed by \\[Buffer-menu-select] command."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-M]")
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?>)
      (forward-line 1))))

(defun Buffer-menu-unmark ()
  "Cancel all requested operations on buffer on this line."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-M]")
      (ding)
    (let* ((buf (Buffer-menu-buffer t))
	   (mod (buffer-modified-p buf))
	   (readonly (save-excursion (set-buffer buf) buffer-read-only))
	   (buffer-read-only nil))
      (delete-char 3)
      (insert (if readonly (if mod " *%" "  %") (if mod " * " "   ")))))
  (forward-line 1))

(defun Buffer-menu-backup-unmark ()
  "Move up and cancel all requested operations on buffer on line above."
  (interactive)
  (forward-line -1)
  (Buffer-menu-unmark)
  (forward-line -1))

(defun Buffer-menu-delete ()
  "Mark buffer on this line to be deleted by \\[Buffer-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-M]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?D)
      (forward-line 1))))

(defun Buffer-menu-delete-backwards ()
  "Mark buffer on this line to be deleted by \\[Buffer-menu-execute] command
and then move up one line"
  (interactive)
  (Buffer-menu-delete)
  (forward-line -2)
  (if (looking-at " [-M]") (forward-line 1)))

(defun Buffer-menu-save ()
  "Mark buffer on this line to be saved by \\[Buffer-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (forward-char 1)
  (if (looking-at " [-M]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?S)
      (forward-line 1))))

(defun Buffer-menu-not-modified ()
  "Mark buffer on this line as unmodified (no changes to save)."
  (interactive)
  (save-excursion
    (set-buffer (Buffer-menu-buffer t))
    (set-buffer-modified-p nil))
  (save-excursion
   (beginning-of-line)
   (forward-char 1)
   (if (looking-at "\\*")
       (let ((buffer-read-only nil))
	 (delete-char 1)
	 (insert ? )))))

(defun Buffer-menu-execute ()
  "Save and/or delete buffers marked with \\[Buffer-menu-save] or \\[Buffer-menu-delete] commands."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "^.S" nil t)
      (let ((modp nil))
	(save-excursion
	  (set-buffer (Buffer-menu-buffer t))
	  (save-buffer)
	  (setq modp (buffer-modified-p)))
	(let ((buffer-read-only nil))
	  (delete-char -1)
	  (insert (if modp ?* ? ))))))
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((buff-menu-buffer (current-buffer))
	  (buffer-read-only nil))
      (while (search-forward "\nD" nil t)
	(forward-char -1)
	(let ((buf (Buffer-menu-buffer nil)))
	  (or (eq buf nil)
	      (eq buf buff-menu-buffer)
	      (save-excursion (kill-buffer buf))))
	(if (Buffer-menu-buffer nil)
	    (progn (delete-char 1)
		   (insert ? ))
	  (delete-region (point) (progn (forward-line 1) (point)))
 	  (forward-char -1))))))

(defun Buffer-menu-select ()
  "Select this line's buffer; also display buffers marked with \">\".
You can mark buffers with the \\[Buffer-menu-mark] command."
  (interactive)
  (let ((buff (Buffer-menu-buffer t))
	(menu (current-buffer))	      
	(others ())
	tem)
    (goto-char (point-min))
    (while (search-forward "\n>" nil t)
      (setq tem (Buffer-menu-buffer t))
      (let ((buffer-read-only nil))
	(delete-char -1)
	(insert ?\ ))
      (or (eq tem buff) (memq tem others) (setq others (cons tem others))))
    (setq others (nreverse others)
	  tem (/ (1- (screen-height)) (1+ (length others))))
    (delete-other-windows)
    (switch-to-buffer buff)
    (or (eq menu buff)
	(bury-buffer menu))
    (while others
      (split-window nil tem)
      (other-window 1)
      (switch-to-buffer (car others))
      (setq others (cdr others)))
    (other-window 1)))			;back to the beginning!

(defun Buffer-menu-visit-tags-table ()
  "Visit the tags table in the buffer on this line.  See `visit-tags-table'."
  (interactive)
  (let ((file (buffer-file-name (Buffer-menu-buffer t))))
    (if file
	(visit-tags-table file)
      (error "Specified buffer has no file"))))

(defun Buffer-menu-1-window ()
  "Select this line's buffer, alone, in full screen."
  (interactive)
  (switch-to-buffer (Buffer-menu-buffer t))
  (bury-buffer (other-buffer))
  (delete-other-windows)
  ;; This is to get w->force_start set to nil.  Don't ask me, I only work here.
  (set-window-buffer (selected-window) (current-buffer)))

(defun Buffer-menu-this-window ()
  "Select this line's buffer in this window."
  (interactive)
  (switch-to-buffer (Buffer-menu-buffer t)))

(defun Buffer-menu-other-window ()
  "Select this line's buffer in other window, leaving buffer menu visible."
  (interactive)
  (switch-to-buffer-other-window (Buffer-menu-buffer t)))

(defun Buffer-menu-2-window ()
  "Select this line's buffer, with previous buffer in second window."
  (interactive)
  (let ((buff (Buffer-menu-buffer t))
	(menu (current-buffer))
	(pop-up-windows t))
    (switch-to-buffer (other-buffer))
    (pop-to-buffer buff)
    (bury-buffer menu)))


;;; mouseability

(defun Buffer-menu-mouse-select (event)
  (interactive "e")
  (mouse-set-point event)
  (Buffer-menu-select))

(defvar Buffer-menu-popup-menu
  '("Buffer Commands"
    ["Select Buffer"			Buffer-menu-select		t]
    ["Select buffer Other Window"	Buffer-menu-other-window	t]
    ["Clear Buffer Modification Flag"	Buffer-menu-not-modified	t]
    "----"
    ["Mark Buffer for Selection"	Buffer-menu-mark		t]
    ["Mark Buffer for Save"		Buffer-menu-save		t]
    ["Mark Buffer for Deletion"		Buffer-menu-delete		t]
    ["Unmark Buffer"			Buffer-menu-unmark		t]
    "----"
    ["Delete/Save Marked Buffers"	Buffer-menu-execute		t]
    ))

(defun Buffer-menu-popup-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (let ((buffer (Buffer-menu-buffer nil)))
    (if buffer
	(popup-menu
	 (nconc (list (car Buffer-menu-popup-menu)
		      (concat
		       "Commands on buffer \"" (buffer-name buffer) "\":")
		      "----")
		(cdr Buffer-menu-popup-menu)))
      (error "no buffer on this line"))))
