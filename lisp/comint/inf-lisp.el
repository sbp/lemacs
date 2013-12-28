;; -*-Emacs-Lisp-*- run an external lisp interpreter in an Emacs window
;; Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.

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

;;; Hacked from tea.el and shell.el by Olin Shivers (shivers@cs.cmu.edu). 8/88
;;; Since these modes are built on top of the general command-interpreter-in-
;;; a-buffer mode (comint mode), they share a common base functionality, 
;;; and a common set of bindings, with all modes derived from comint mode.

;;; Needs fixin:
;;; The load-file/compile-file default mechanism could be smarter -- it
;;; doesn't know about the relationship between filename extensions and
;;; whether the file is source or executable. If you compile foo.lisp
;;; with compile-file, then the next load-file should use foo.bin for
;;; the default, not foo.lisp. This is tricky to do right, particularly
;;; because the extension for executable files varies so much (.o, .bin,
;;; .lbin, .mo, .vo, .ao, ...).
;;;
;;; It would be nice if lisp (and inferior scheme, T, ...) modes
;;; had a verbose minor mode wherein sending or compiling defuns, etc.
;;; would be reflected in the transcript with suitable comments, e.g.
;;; ";;; redefining fact". Several ways to do this. Which is right?
;;;
;;; When sending text from a source file to a subprocess, the process-mark can 
;;; move off the window, so you can lose sight of the process interactions.
;;; Maybe I should ensure the process mark is in the window when I send
;;; text to the process? Switch selectable?

(require 'comint)
(provide 'inf-lisp)

(defvar lisp-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "*What not to save on inferior Lisp's input history
Input matching this regexp is not saved on the input history in lisp
mode. Default is whitespace followed by 0 or 1 single-letter :keyword 
(as in :a, :c, etc.)")

;;; These commands augment lisp mode, so you can process lisp code in
;;; the source files.
(define-key lisp-mode-map "\C-c\C-f" 'lisp-load-file)
(define-key lisp-mode-map "\C-c\C-k" 'lisp-compile-file)
(define-key lisp-mode-map "\M-\C-x"  'lisp-eval-defun) ; Gnu convention
(define-key lisp-mode-map "\C-c\C-e" 'lisp-eval-region)
(define-key lisp-mode-map "\C-c\C-x" 'lisp-compile-defun)
(define-key lisp-mode-map "\C-c\C-c" 'lisp-compile-region)

(defvar inferior-lisp-mode-map nil)
(if inferior-lisp-mode-map ()
  (setq inferior-lisp-mode-map (copy-keymap shared-lisp-mode-map))
  (set-keymap-parent inferior-lisp-mode-map comint-mode-map)
  (define-key inferior-lisp-mode-map "\e\C-x" 'lisp-send-defun))

(defvar inferior-lisp-program "lisp"
  "*Program name for invoking an inferior Lisp with `lisp'.
Optionally, a list of strings the car of which is the programme to run
and the cdr of which is command line arguments desired.")

(defvar inferior-lisp-load-command "(load \"%s\")\n"
  "*Format-string for building a Lisp expression to load a file.
This format string should use %s to substitute a file name
and should result in a Lisp expression that will command the inferior Lisp
to load that file.  The default works acceptably on most Lisps.
The string \"(progn (load \\\"%s\\\" :verbose nil :print t) (values))\\\n\"
produces cosmetically superior output for this application,
but it works only in Common Lisp.")

(defvar inferior-lisp-prompt "^[^> ]*>+:? *"
  "Regexp to recognise prompts in the inferior Lisp.
Defaults to \"^[^> ]*>+:? *\", which works pretty good for Lucid, kcl,
and franz. This variable is used to initialise comint-prompt-regexp in the 
lisp buffer.

More precise choices:
Lucid Common Lisp: \"^\\(>\\|\\(->\\)+\\) *\"
franz: \"^\\(->\\|<[0-9]*>:\\) *\"
kcl: \"^>+ *\"")

(defvar inferior-lisp-mode-hook '() "*Hook for customising lisp mode")

(defun inferior-lisp-mode () 
  "Major mode for interacting with an inferior Lisp process.  
Runs a Lisp interpreter as a subprocess of Emacs, with Lisp I/O through an
Emacs buffer.  Variable inferior-lisp-program controls which Lisp interpreter
is run.  Variables inferior-lisp-prompt, lisp-filter-regexp and
inferior-lisp-load-command can customize this mode for different Lisp
interpreters.

\\{inferior-lisp-mode-map}

Customization: Entry to this mode runs comint-mode-hook and lisp-mode-hook,
in that order.

You can send text to the inferior Lisp process from other buffers containing
Lisp source.  
    lisp-eval-defun sends the current defun to the Lisp process.
    lisp-compile-defun compiles the current defun.
    lisp-eval-region sends the current region to the Lisp process.
    lisp-compile-region compiles the current region.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Lisp; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  (use-local-map inferior-lisp-mode-map)
  (setq major-mode 'inferior-lisp-mode
	mode-name "Inferior Lisp"
	comint-prompt-regexp inferior-lisp-prompt
	comint-get-old-input (function inferior-lisp-get-old-input)
	comint-input-filter (function inferior-lisp-input-filter)
	comint-input-sentinel 'ignore)
  (lisp-mode-variables t)
  (run-hooks 'inferior-lisp-mode-hook))

(defun inferior-lisp-get-old-input ()
  "Snarf the sexp ending at point"
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun inferior-lisp-input-filter (str)
  "Don't save anything matching lisp-filter-regexp"
  (not (string-match lisp-filter-regexp str)))

(defun run-lisp ()
  "Run an inferior Lisp process, input and output via buffer *lisp*.
If there is a process already running in *lisp*, just switch to that buffer.
Takes the program name from the variable inferior-lisp-program.
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive)
  (let ((prog inferior-lisp-program) switches
	(proc (get-buffer-process "*lisp*")))
    (if (listp prog) (setq switches (cdr prog) prog (car prog)))
    (cond ((not (and proc (memq (process-status proc) '(run stop))))
	   (set-buffer (apply 'make-comint "lisp" prog nil switches))
	   (inferior-lisp-mode))))
  (switch-to-buffer "*lisp*"))

(defun lisp-eval-region (start end)
  "Send the current region to the inferior Lisp process."
  (interactive "r")
  ;; i don't like the way all this code depends on the buffer name
  (let* ((proc (get-buffer-process "*lisp*"))
	 (filename (format "/tmp/eme%d.lisp" (process-id proc))))
    (if (not (and proc (memq (process-status proc) '(run stop))))
	(error "No current lisp process")
      (write-region start end filename nil 'nomessage)
      (send-string proc (format inferior-lisp-load-command filename))
      (send-string proc (format "(delete-file %s)\n" filename)))))

(defun lisp-eval-defun ()
  "Send the current defun to the inferior Lisp process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (lisp-eval-region (point) end))))

;;; CommonLisp COMPILE sux. 
(defun lisp-compile-region (start end)
  "Compile the current region in the inferior Lisp process."
  ;; deja vu.  code look familiar?
  (interactive "r")
  (let* ((proc (get-buffer-process "*lisp*"))
	 (filename (format "/tmp/emc%d.lisp" (process-id proc))))
    (if (not (and proc (memq (process-status proc) '(run stop))))
	(error "No current lisp process")
      (write-region
       (concat "(funcall (compile nil `(lambda () (progn 'compile "
	       (buffer-substring start end)
	       "))))\n")
       nil filename nil 'nomessage)
      (send-string proc (format inferior-lisp-load-command filename))
      (send-string proc (format "(delete-file %s)\n" filename)))))
			 
(defun lisp-compile-defun ()
  "Compile the current defun in the inferior Lisp process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((e (point)))
      (beginning-of-defun)
      (lisp-compile-region (point) e))))

(defvar lisp-prev-l/c-dir/file nil
  "Saves the (directory . file) pair used in the last lisp-load-file or
lisp-compile-file command. Used for determining the default in the 
next one.")

(defvar lisp-source-modes '(lisp-mode)
  "*Used to determine if a buffer contains Lisp source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a lisp source file by lisp-load-file and lisp-compile-file.
Used by these commands to determine defaults.")

(defun lisp-load-file (file-name)
  "Load a lisp file into the inferior Lisp process."
  (interactive (comint-get-source "Load Lisp file: " lisp-prev-l/c-dir/file
				  lisp-source-modes nil))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (send-string "lisp" (format inferior-lisp-load-command file-name)))

(defun lisp-compile-file (file-name)
  "Compile a Lisp file in the inferior Lisp process."
  (interactive (comint-get-source "Compile Lisp file: " lisp-prev-l/c-dir/file
				  lisp-source-modes nil)) ; NIL = don't need
                                                          ; suffix .lisp
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (send-string "lisp" (concat "(compile-file \"" file-name "\"\)\n")))
