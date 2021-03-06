;; Define standard keybindings.
;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

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

;; All the global bindings should be here so that one can reload things
;; like files.el without trashing ones' personal bindings.

;; created by C code
(defvar global-map (current-global-map) "\
Default global keymap mapping Emacs keyboard input into commands.
The value is a keymap which is usually (but not necessarily) Emacs's
global map.")

;; created by C code
(defvar esc-map (symbol-function 'ESC-prefix) "\
Default keymap for ESC (meta) commands.
The normal global definition of the character ESC indirects to this keymap.")

(set-keymap-name global-map 'global-map)
(set-keymap-name esc-map 'ESC-prefix)

(define-prefix-command 'Control-X-prefix t)
(defvar ctl-x-map (symbol-function 'Control-X-prefix) "\
Default keymap for C-x commands.
The normal global definition of the character C-x indirects to this keymap.")
(define-key global-map "\C-x" 'Control-X-prefix)

(define-prefix-command 'ctl-x-4-prefix t)
(defvar ctl-x-4-map (symbol-function 'ctl-x-4-prefix) "\
Keymap for subcommands of C-x 4")
(define-key global-map "\C-x4" 'ctl-x-4-prefix)

(define-prefix-command 'ctl-x-5-prefix t)
(defvar ctl-x-5-map (symbol-function 'ctl-x-5-prefix) "\
Keymap for subcommands of C-x 5")
(define-key global-map "\C-x5" 'ctl-x-5-prefix)

(define-prefix-command 'mode-specific-command-prefix t)
(defvar mode-specific-map (symbol-function 'mode-specific-command-prefix) "\
Keymap for characters following C-c.")
(define-key global-map "\C-c" 'mode-specific-command-prefix)

(define-key global-map "\C-z" 'suspend-emacs)
(define-key global-map "\C-x\C-z" 'suspend-emacs)

(define-key global-map "\M-\C-c" 'exit-recursive-edit)
(define-key global-map "\C-]" 'abort-recursive-edit)
(define-key global-map "\M-x" 'execute-extended-command)

;;(define-key global-map "\C-g" 'keyboard-quit)
(define-key global-map (char-to-string interrupt-char) 'keyboard-quit)

;;(define-key global-map "\t" 'self-insert-command)
(define-key global-map "\t" 'indent-for-tab-command)

(let ((n 32)
      (s (make-string 1 ?.)))
  (while (< n 127)
    (aset s 0 n)
    (define-key global-map s 'self-insert-command)
    (setq n (1+ n))))

(define-key global-map "\C-a" 'beginning-of-line)
(define-key global-map "\C-b" 'backward-char)
(define-key global-map "\C-e" 'end-of-line)
(define-key global-map "\C-f" 'forward-char)
(define-key global-map "\C-d" 'delete-char)
(define-key global-map "\177" 'delete-backward-char)

(define-key global-map "\C-x\C-u" 'upcase-region)
(define-key global-map "\C-x\C-l" 'downcase-region)
(define-key global-map "\M-u" 'upcase-word)
(define-key global-map "\M-l" 'downcase-word)
(define-key global-map "\M-c" 'capitalize-word)

(define-key global-map "\C-u" 'universal-argument)
;; Make Control-0 - Control-9 set the prefix argument, like Meta-0.
(let ((i ?0))
  (while (<= i ?9)
    (define-key global-map (list 'meta i) 'digit-argument)
    (define-key global-map (list 'control i) 'digit-argument)
    (define-key global-map (list 'control 'meta i) 'digit-argument)
    (setq i (1+ i))))
(define-key global-map '(meta -) 'negative-argument)
(define-key global-map '(control -) 'negative-argument)
(define-key global-map '(control meta -) 'negative-argument)

(define-key global-map "\C-x0" 'delete-window)
(define-key global-map "\C-x1" 'delete-other-windows)
(define-key global-map "\C-x2" 'split-window-vertically)
(define-key global-map "\C-x3" 'split-window-horizontally)
;; Old lemacs binding
;;(define-key global-map "\C-x5" 'split-window-horizontally)
(define-key global-map "\C-x6" 'window-configuration-to-register)
;(define-key global-map "\C-x7" 'jump-to-register);ie register-to-window-config
(define-key global-map "\C-x}" 'enlarge-window-horizontally)
(define-key global-map "\C-x{" 'shrink-window-horizontally)

;; New FSF19 bindings: C-x 5 as prefix for window commands
(define-key global-map "\C-x52" 'make-screen)
(define-key global-map "\C-x50" 'delete-screen)
(define-key global-map "\C-x5o" 'other-screen)
(define-key global-map "\C-x5b" 'switch-to-buffer-other-screen)
(define-key global-map "\C-x5f" 'find-file-other-screen)
(define-key global-map "\C-x5\C-f" 'find-file-other-screen)
(define-key global-map "\C-x5r" 'find-file-read-only-other-screen)
(define-key global-map "\C-x5m" 'mail-other-screen)

(define-key global-map "\C-xo" 'other-window)
(define-key global-map "\C-x^" 'enlarge-window)
(define-key global-map "\C-x<" 'scroll-left)
(define-key global-map "\C-x>" 'scroll-right)

(define-key global-map "\C-v" 'scroll-up)
(define-key global-map "\M-v" 'scroll-down)
(define-key global-map "\M-\C-v" 'scroll-other-window)
(define-key global-map "\C-l" 'recenter)
(define-key global-map "\M-r" 'move-to-window-line)


(define-key global-map "\C-s" 'isearch-forward)
(define-key global-map "\C-r" 'isearch-backward)
(define-key global-map "\M-\C-s" 'isearch-forward-regexp)

(define-key global-map "\M-%" 'query-replace)

;; Old v18 bindings
;(define-key global-map "\C-x\C-a" 'add-mode-abbrev)
;(define-key global-map "\C-x+" 'add-global-abbrev)
;(define-key global-map "\C-x\C-h" 'inverse-add-mode-abbrev)
;(define-key global-map "\C-x-" 'inverse-add-global-abbrev)

(define-key global-map "\M-'" 'abbrev-prefix-mark)
(define-key global-map "\C-x'" 'expand-abbrev)

;; New FSF19 bindings: C-x a as a prefix for abbrev commands
(define-key global-map "\C-xal" 'add-mode-abbrev)
(define-key global-map "\C-xa\C-a" 'add-mode-abbrev)
(define-key global-map "\C-xag" 'add-global-abbrev)
(define-key global-map "\C-xa+" 'add-mode-abbrev)
(define-key global-map "\C-xaig" 'inverse-add-global-abbrev)
(define-key global-map "\C-xail" 'inverse-add-mode-abbrev)
(define-key global-map "\C-xa-" 'inverse-add-global-abbrev)
(define-key global-map "\C-xae" 'expand-abbrev)
(define-key global-map "\C-xa'" 'expand-abbrev)

(define-key global-map "\C-xb" 'switch-to-buffer)
(define-key global-map "\C-xk" 'kill-buffer)
(define-key global-map "\C-x\C-b" 'list-buffers)

;; New FSF19 bindings
(define-key global-map "\C-x-" 'shrink-window-if-larger-than-buffer)
(define-key global-map "\C-x+" 'balance-windows)

(define-key global-map "\C-xe" 'call-last-kbd-macro)
(define-key global-map "\C-x\(" 'start-kbd-macro)
(define-key global-map "\C-x\)" 'end-kbd-macro)

; autoloaded
;(define-key global-map "\C-x4a" 'add-change-log-entry-other-window)

(define-key global-map "\C-x`" 'next-error)

(define-key global-map "\M-/" 'dabbrev-expand)

; autoloaded
;(define-key global-map "\C-xd" 'dired)

; autoloaded
;(define-key global-map "\C-x4d" 'dired-other-window)

(define-key global-map "\M-$" 'ispell-word)

(define-key global-map "\C-xq" 'kbd-macro-query)

(define-key global-map "\C-x4m" 'mail-other-window)

(define-key global-map "\C-xm" 'mail)

(define-key global-map "\M-." 'find-tag)

(define-key global-map "\C-x4." 'find-tag-other-window)

(define-key global-map "\M-," 'tags-loop-continue)

;; this is so that where-is says backward-list is M-C-p instead of M-up.
(fset 'deprecated-backward-list 'backward-list)
(fset 'deprecated-forward-list  'forward-list)
(fset 'deprecated-backward-sexp 'backward-sexp)
(fset 'deprecated-forward-sexp  'forward-sexp)

(define-key global-map '(meta left)  'deprecated-backward-sexp)
(define-key global-map '(meta right) 'deprecated-forward-sexp)
(define-key global-map '(meta up)    'deprecated-backward-list)
(define-key global-map '(meta down)  'deprecated-forward-list)

(define-key global-map "\M-\C-f" 'forward-sexp)
(define-key global-map "\M-\C-b" 'backward-sexp)
(define-key global-map "\M-\C-u" 'backward-up-list)
(define-key global-map "\M-\C-@" 'mark-sexp)
(define-key global-map "\M-\C-d" 'down-list)
(define-key global-map "\M-\C-k" 'kill-sexp)
(define-key global-map "\M-\C-n" 'forward-list)
(define-key global-map "\M-\C-p" 'backward-list)
(define-key global-map "\M-\C-a" 'beginning-of-defun)
(define-key global-map "\M-\C-e" 'end-of-defun)
(define-key global-map "\M-\C-h" 'mark-defun)
(define-key global-map "\M-\(" 'insert-parentheses)
(define-key global-map "\M-\)" 'move-past-close-and-reindent)
(define-key global-map "\M-\t" 'lisp-complete-symbol)

(define-key global-map '(control meta delete) 'backward-kill-sexp)

(define-key global-map '(control <) 'mark-beginning-of-buffer)
(define-key global-map '(control >) 'mark-end-of-buffer)

(define-key global-map "\C-x\C-e" 'eval-last-sexp) ;bogus!

(define-key global-map "\C-x/" 'point-to-register)
(define-key global-map "\C-xj" 'jump-to-register)
(define-key global-map "\C-xx" 'copy-to-register)
(define-key global-map "\C-xg" 'insert-register)
;; Old v18 binding
;(define-key global-map "\C-xr" 'copy-rectangle-to-register)

;; New FSF19 bindings: C-x r as a prefix for register commands
(define-key global-map "\C-xr" (make-sparse-keymap))
(define-key global-map "\C-xr\C-@" 'point-to-register)
(define-key global-map "\C-xr " 'point-to-register)
(define-key global-map "\C-xrj" 'jump-to-register)
(define-key global-map "\C-xrs" 'copy-to-register)
(define-key global-map "\C-xrx" 'copy-to-register)
(define-key global-map "\C-xri" 'insert-register)
(define-key global-map "\C-xrg" 'insert-register)
(define-key global-map "\C-xrr" 'copy-rectangle-to-register)
(define-key global-map "\C-xrc" 'clear-rectangle)
(define-key global-map "\C-xrk" 'kill-rectangle)
(define-key global-map "\C-xry" 'yank-rectangle)
(define-key global-map "\C-xro" 'open-rectangle)
(define-key global-map "\C-xrt" 'string-rectangle)
(define-key global-map "\C-xrw" 'window-configuration-to-register)
;(define-key global-map "\C-xrf" 'screen-configuration-to-register)

(define-key global-map "\M-q" 'fill-paragraph)
(define-key global-map "\M-g" 'fill-region)
(define-key global-map "\C-x." 'set-fill-prefix)

(define-key global-map "\M-[" 'backward-paragraph)
(define-key global-map "\M-]" 'forward-paragraph)
(define-key global-map "\M-h" 'mark-paragraph)
(define-key global-map "\M-a" 'backward-sentence)
(define-key global-map "\M-e" 'forward-sentence)
(define-key global-map "\M-k" 'kill-sentence)
(define-key global-map "\C-x\177" 'backward-kill-sentence)

(define-key global-map "\C-x[" 'backward-page)
(define-key global-map "\C-x]" 'forward-page)
(define-key global-map "\C-x\C-p" 'mark-page)
(define-key global-map "\C-xl" 'count-lines-page)

(put 'erase-buffer 'disabled t)         ;from buffer.c

(put 'narrow-to-page 'disabled t)
(put 'narrow-to-region 'disabled t)

;; Old v18 bindings
;(define-key global-map "\C-xp" 'narrow-to-page)
;(define-key global-map "\C-xn" 'narrow-to-region)
;(define-key global-map "\C-xw" 'widen)

;; New FSF19 bindings: C-x n as a prefix for narrowing commands.
(define-key global-map "\C-xn" (make-sparse-keymap))
(define-key global-map "\C-xnn" 'narrow-to-region)
(define-key global-map "\C-xnw" 'widen)


(define-key global-map "\C-j" 'newline-and-indent)
(define-key global-map "\C-m" 'newline)
(define-key global-map "\C-o" 'open-line)
(define-key global-map "\M-\C-o" 'split-line)
(define-key global-map "\C-q" 'quoted-insert)
(define-key global-map "\M-^" 'delete-indentation)
(define-key global-map "\M-\\" 'delete-horizontal-space)
(define-key global-map "\M-m" 'back-to-indentation)
(define-key global-map "\C-x\C-o" 'delete-blank-lines)
(define-key global-map "\M- " 'just-one-space)
(define-key global-map "\M-z" 'zap-to-char)
(define-key global-map "\M-=" 'count-lines-region)
(define-key global-map "\C-x=" 'what-cursor-position)
(define-key global-map "\M-\e" 'eval-expression)
(put 'eval-expression 'disabled t)
(define-key global-map "\C-x\e" 'repeat-complex-command)
(define-key global-map "\C-xu" 'advertised-undo)
(define-key global-map "\C-_" 'undo)
;; Many people are used to typing C-/ on X terminals and getting C-_.
(define-key global-map '(control |) 'undo)
(define-key global-map "\M-!" 'shell-command)
(define-key global-map "\M-|" 'shell-command-on-region)


(define-key global-map "\C-k" 'kill-line)
(define-key global-map "\C-w" 'kill-region)
(define-key global-map "\M-w" 'kill-ring-save)
(define-key global-map "\M-\C-w" 'append-next-kill)
(define-key global-map "\C-y" 'yank)
(define-key global-map "\M-y" 'yank-pop)

;; Old v18 binding
;(define-key global-map "\C-xa" 'append-to-buffer)

(define-key global-map "\C-@" 'set-mark-command)
(define-key global-map "\C-x\C-x" 'exchange-point-and-mark)
(define-key global-map "\C-x\C-@" 'pop-global-mark)

(define-key global-map "\C-n" 'next-line)
(define-key global-map "\C-p" 'previous-line)
(define-key global-map "\C-x\C-n" 'set-goal-column)
;;; Many people have said they rarely use this feature, and often type
;;; it by accident.  Maybe it shouldn't even be on a key.
(put 'set-goal-column 'disabled t)

(define-key global-map "\C-t" 'transpose-chars)
(define-key global-map "\M-t" 'transpose-words)
(define-key global-map "\M-\C-t" 'transpose-sexps)
(define-key global-map "\C-x\C-t" 'transpose-lines)

(define-key global-map "\M-;" 'indent-for-comment)
(define-key global-map "\M-j" 'indent-new-comment-line)
(define-key global-map "\M-\C-j" 'indent-new-comment-line)
(define-key global-map "\C-x;" 'set-comment-column)
(define-key global-map "\C-xf" 'set-fill-column)
(define-key global-map "\C-x$" 'set-selective-display)

(define-key global-map "\M-@" 'mark-word)
(define-key global-map "\M-f" 'forward-word)
(define-key global-map "\M-b" 'backward-word)
(define-key global-map "\M-d" 'kill-word)
(define-key global-map "\M-\177" 'backward-kill-word)

(define-key global-map "\M-<" 'beginning-of-buffer)
(define-key global-map "\M->" 'end-of-buffer)
(define-key global-map "\C-xh" 'mark-whole-buffer)
(define-key global-map "\M-\\" 'delete-horizontal-space)

(define-key global-map "\C-x\C-f" 'find-file)
(define-key global-map "\C-x\C-q" 'toggle-read-only)
(define-key global-map "\C-x\C-r" 'find-file-read-only)
(define-key global-map "\C-x\C-v" 'find-alternate-file)
(define-key global-map "\C-x\C-s" 'save-buffer)
(define-key global-map "\C-xs" 'save-some-buffers)
(define-key global-map "\C-x\C-w" 'write-file)
(define-key global-map "\C-xi" 'insert-file)
(define-key global-map "\M-~" 'not-modified)
(define-key global-map "\C-x\C-d" 'list-directory)
(define-key global-map "\C-x\C-c" 'save-buffers-kill-emacs)

(define-key global-map "\C-x4f" 'find-file-other-window)
(define-key global-map "\C-x4r" 'find-file-read-only-other-window)
(define-key global-map "\C-x4\C-f" 'find-file-other-window)
(define-key global-map "\C-x4b" 'switch-to-buffer-other-window)
(define-key global-map "\C-x4\C-o" 'display-buffer)

(define-key global-map "\M-\C-l" 'switch-to-other-buffer)

(define-key global-map "\M-\C-\\" 'indent-region)
(define-key global-map "\C-x\t" 'indent-rigidly)
(define-key global-map "\M-i" 'tab-to-tab-stop)

;; Default binding of "Backspace" is the same as delete.
;; Default binding of "Control-h" is help.
(define-key global-map 'backspace '[delete])
(define-key global-map '(meta backspace) '[(meta delete)])
(define-key global-map '(control backspace) '[(control delete)])
(define-key global-map '(control meta backspace) '[(control meta delete)])

(define-key global-map '(control h) 'help-command)


;;; Other FSFmacs bindings to deal with once we have key-translation-map

;(define-key global-map [home]		'beginning-of-buffer)
;(define-key global-map [left]		'backward-char)
;(define-key global-map [up]		'previous-line)
;(define-key global-map [right]		'forward-char)
;(define-key global-map [down]		'next-line)
;(define-key global-map [prior]		'scroll-down)
;(define-key global-map [next]		'scroll-up)
;(define-key global-map [M-next]		'scroll-other-window)
;(define-key global-map [end]		'end-of-buffer)
;(define-key global-map [begin]		'beginning-of-buffer)
;;; (define-key global-map [select]		'function-key-error)
;;; (define-key global-map [print]		'function-key-error)
;(define-key global-map [execute]	'execute-extended-command)
;(define-key global-map [insert]		'overwrite-mode)
;(define-key global-map [undo]		'undo)
;(define-key global-map [redo]		'repeat-complex-command)
;;; (define-key global-map [clearline]	'function-key-error)
;(define-key global-map [insertline]	'open-line)
;(define-key global-map [deleteline]	'kill-line)
;;; (define-key global-map [insertchar]	'function-key-error)
;(define-key global-map [deletechar]	'delete-char)
