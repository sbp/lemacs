;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright © 1992-1993 by Lucid, Inc.  All Rights Reserved.

(require 'comint)
(require 'shell)
(require 'gdb)

(defun energize-comint-input-sender (energize-proc input)
  (energize-send-region (energize-user-input-buffer-mark) (point)))

(defvar energize-shell-prompt-pattern "^(.*gdb) ?"
  "*A regexp to recognize the prompt for the Energize debugger.")

(defun energize-debugger-mode ()
  "Major mode for the Energize Debugger buffers.
In addition to the normal cursor-motion commands, the following keys are bound:
\\{energize-debugger-map}"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp energize-shell-prompt-pattern
	comint-input-sentinel 'shell-directory-tracker
	comint-input-sender 'energize-comint-input-sender)
  (setq mode-line-process nil)
  (energize-mode-internal)
  (set-syntax-table c-mode-syntax-table)
  (setq major-mode 'energize-debugger-mode
	mode-name "Energize-Debugger")
  (use-local-map energize-debugger-map)
  (set (make-local-variable 'shell-dirstack) nil)
  (set (make-local-variable 'shell-dirtrackp) t)
  (set (make-local-variable 'gdb-last-frame) nil)
  (set (make-local-variable 'gdb-last-frame-displayed-p) t)
  (set (make-local-variable 'gdb-delete-prompt-marker) nil)
  (set (make-local-variable 'comint-input-autoexpand) nil)
  (run-hooks 'energize-debugger-mode-hook))


(if energize-debugger-map
    nil
;;  (setq energize-debugger-map (make-sparse-keymap))
;;  kludge!!
  (setq energize-debugger-map (copy-keymap energize-map))

  (set-keymap-name energize-debugger-map 'energize-debugger-map)
  (set-keymap-parent energize-debugger-map gdb-mode-map)
  (define-key energize-debugger-map "\M-\t" 'comint-dynamic-complete)
  (define-key energize-debugger-map "\M-?" 'comint-dynamic-list-completions)
  (define-key energize-debugger-map "\C-c<" 'energize-debugger-up-frame)
  (define-key energize-debugger-map "\C-c>" 'energize-debugger-down-frame)
  (define-key energize-debugger-map "\C-c\C-f" 'energize-debugger-return)
  (define-key energize-debugger-map "\C-c\C-c" 'energize-debugger-interrupt)
  ;; should be quit-subjob
  (define-key energize-debugger-map "\C-c\C-\\" 'energize-debugger-interrupt)
  (define-key energize-debugger-map "\C-c\C-z"
    'energize-debugger-interrupt) ; should suspend
  (define-key energize-debugger-map "\C-c\C-d" 'energize-debugger-send-eof)
  )


;;; Energize Debugger mode commands; it seems pointless to do this with advice.

(defun energize-debugger-step-line (arg)
  "Step one source line in the Energize debugger."
  (interactive "p")
  (energize-execute-command "steponce"))

(defun energize-debugger-step-instruction (arg)
  "Step one machine instruction in the Energize debugger."
  (interactive "p")
  (energize-execute-command "stepinst"))

(defun energize-debugger-next-line (arg)
  "Step one source line skipping function calls in the Energize debugger."
  (interactive "p")
  (energize-execute-command "stepnext"))

(defun energize-debugger-continue-program (arg)
  "Continue running program being debugged in the Energize debugger."
  (interactive "p")
  (energize-execute-command "continueprogram"))

(defun energize-debugger-up-frame (arg)
  "Go up one stack frame in the Energize debugger."
  (interactive "p")
  (energize-execute-command "upframe"))

(defun energize-debugger-down-frame (arg)
  "Go up one stack frame in the Energize debugger."
  (interactive "p")
  (energize-execute-command "downframe"))

(defun energize-debugger-return (arg)
  "Finish executing current function in the Energize debugger."
  (interactive "p")
  (energize-execute-command "continueuntilreturn"))

(defun energize-debugger-interrupt (arg)
  "Interrupt program or debugger command in the Energize debugger."
  (interactive "p")
  (energize-execute-command "stopprogram"))

(defun energize-debugger-send-eof ()
  "Send an EOF to the Energize debugger."
  (interactive)
  (save-excursion
    (insert ?\C-d)
    (energize-send-region (1- (point)) (point))
    (delete-char -1)))
