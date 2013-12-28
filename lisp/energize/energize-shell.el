;;; -*- Mode:Emacs-Lisp -*-

(require 'comint)
(require 'shell)

(defun energize-comint-mark ()
  (or (energize-user-input-buffer-mark)
      (energize-orig-comint-mark)))

(energize-advise-function 'comint-mark)


(defun energize-comint-input-sender (energize-proc input)
  (energize-send-region
   (marker-position energize-mark) ; spec-ref to energize-comint-send-input
   (point)))		          ; dependent on how comint-send-input works...

(defun energize-comint-send-input ()
  "This function has been augumented to work with Energize debugger buffers."
  (interactive)
  (if (energize-buffer-p (current-buffer))
      (let ((energize-proc (or (get-process "energize")
			       (get-process "energize\000") ; #### Nuke this!
			       (error "couldn't find \"energize\" process")))
	    (energize-mark (or (energize-user-input-buffer-mark)
				(error "couldn't find energize input mark"))))
	(unwind-protect
	    (let ((comint-input-sender 'energize-comint-input-sender))
	      (set-marker (process-mark energize-proc) energize-mark)
	      (set-process-buffer energize-proc (current-buffer))
	      (energize-orig-comint-send-input)
	      )
	  (set-marker energize-mark (process-mark energize-proc))
	  (set-process-buffer energize-proc nil)))
    (energize-orig-comint-send-input)))

(energize-advise-function 'comint-send-input)
(fset 'energize-bits 'random)


(defvar energize-shell-prompt-pattern "^(.*gdb[+]?) *"
  "*A regexp to recognize the prompt for gdb or gdb+.") 

(defun energize-user-input-mode ()
  "Major mode for the Energize user-input buffers.
In addition to the normal cursor-motion commands, the following keys are bound:
\\{energize-user-input-map}"
  (interactive)
  (comint-mode)
  (setq mode-line-process ())
  (energize-mode-internal)
  (setq major-mode 'energize-user-input-mode
	mode-name "Energize-Shell")
  (setq comint-prompt-regexp energize-shell-prompt-pattern
	comint-input-sentinel 'shell-directory-tracker)
  (set (make-local-variable 'shell-dirstack) nil)
  (set (make-local-variable 'shell-dirtrackp) t)
  (use-local-map energize-user-input-map)
  (run-hooks 'energize-user-input-mode-hook))


(defun energize-debugger-mode ()
  "Major mode for the Energize Debugger buffers.
In addition to the normal cursor-motion commands, the following keys are bound:
\\{energize-debugger-map}"
  (interactive)
  (energize-user-input-mode)
  (set-syntax-table c-mode-syntax-table)
  (setq major-mode 'energize-debugger-mode
	mode-name "Energize-Debugger")
  (use-local-map energize-debugger-map)
  (set (make-local-variable 'gdb-last-frame) nil)
  (set (make-local-variable 'gdb-last-frame-displayed-p) t)
  (set (make-local-variable 'gdb-delete-prompt-marker) nil)
  (run-hooks 'energize-debugger-mode-hook))


(if energize-debugger-map
    nil
  (setq energize-debugger-map (copy-keymap gdb-mode-map))
  (if (keymap-parent gdb-mode-map) (error "gdb-mode-map has a parent keymap?"))
  (set-keymap-parent energize-debugger-map energize-user-input-map)
;;(define-key energize-debugger-map "\M-s" 'energize-debugger-step-line)
;;(define-key energize-debugger-map "\M-i" 'energize-debugger-step-instruction)
;;(define-key energize-debugger-map "\M-n" 'energize-debugger-next-line)
;;(define-key energize-debugger-map "\M-c" 'energize-debugger-continue-program)
;;(define-key energize-debugger-map "\M-u" 'energize-debugger-up-frame)
;;(define-key energize-debugger-map "\M-d" 'energize-debugger-down-frame)
  (define-key energize-debugger-map "\C-c<" 'energize-debugger-up-frame)
  (define-key energize-debugger-map "\C-c>" 'energize-debugger-down-frame)
  (define-key energize-debugger-map "\C-c\C-f" 'energize-debugger-return)
  (define-key energize-debugger-map "\C-c\C-c" 'energize-debugger-interrupt)
  ;; should be quit-subjob
  (define-key energize-debugger-map "\C-c\C-\\" 'energize-debugger-interrupt)
  (define-key energize-debugger-map "\C-c\C-z"
    'energize-debugger-interrupt) ; should suspend
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
