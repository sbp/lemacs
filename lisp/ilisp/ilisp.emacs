;;;
;;; This file shows examples of some of the things you might want to
;;; do to install or customize ILISP.  You may not want to include all
;;; of them in your .emacs.  For example, the default key binding
;;; prefix for ILISP is C-z and this file changes the default prefix to
;;; C-c.  For more information on things that can be changed, see the
;;; file ilisp.el. 
;;;

;;; If ilisp lives in some non-standard directory, you must tell emacs
;;; where to get it. This may or may not be necessary.
(setq load-path (cons (expand-file-name "~jones/emacs/ilisp/") load-path))

;;; If you always want partial minibuffer completion
(require 'completer)

;;; If want TMC completion then you will have to Ftp it yourself from think.com
;;; It's become to flaky for me to deal with. -- Ivan
;;;(load "completion")
;;;(initialize-completions)

;;; If you want to redefine popper keys
(setq popper-load-hook
      '(lambda ()
	(define-key global-map "\C-c1" 'popper-bury-output)
	(define-key global-map "\C-cv" 'popper-scroll-output)
	(define-key global-map "\C-cg" 'popper-grow-output)
	(define-key global-map "\C-cb" 'popper-switch)))

;;; If you always want popper windows
(if (boundp 'epoch::version)
    (require 'epoch-pop)
    (require 'popper))

(autoload 'run-ilisp "ilisp" "Select a new inferior LISP." t)
;;; Autoload based on your LISP.  You only really need the one you use.
;;; If called with a prefix, you will be prompted for a buffer and
;;; program.
;;; 
;;; [Back to the old way now -- Ivan Mon Jun 28 23:30:51 1993]
;;;
(autoload 'clisp     "ilisp" "Inferior generic Common LISP." t)
(autoload 'allegro   "ilisp" "Inferior Allegro Common LISP." t)
(autoload 'lucid     "ilisp" "Inferior Lucid Common LISP." t)
(autoload 'cmulisp   "ilisp" "Inferior CMU Common LISP." t)
(autoload 'kcl       "ilisp" "Inferior Kyoto Common LISP." t)
(autoload 'akcl      "ilisp" "Inferior Austin Kyoto Common LISP." t)
(autoload 'ibcl      "ilisp" "Ibuki Common LISP." t)
(autoload 'scheme    "ilisp" "Inferior generic Scheme." t)
(autoload 'oaklisp   "ilisp" "Inferior Oaklisp Scheme." t)

;;; Define where LISP programs are found.  (This may already be done
;;; at your site.)
(setq allegro-program "/usr/misc/.allegro/bin/cl")
(setq lucid-program "/usr/misc/.lucid/bin/lisp")
(setq cmulisp-program "/usr/misc/.cmucl/bin/lisp")

;;; If you run cmu-cl then set this to where your source files are.
(setq cmulisp-local-source-directory 
      "/usr/local/utils/CMU-CL/")


;;; This makes reading a lisp file load in ilisp.
(set-default 'auto-mode-alist
	     (append '(("\\.lisp$" . lisp-mode)) auto-mode-alist))
(setq lisp-mode-hook '(lambda () (require 'ilisp)))

;;; Sample load hook
(setq ilisp-load-hook 
      '(lambda ()
	 ;; Change default key prefix to C-c
	 (setq ilisp-prefix "\C-c")
	 ;; Sample initialization hook.  Set the inferior LISP directory to
	 ;; the directory of the buffer that spawned it on the first prompt.
	 (setq ilisp-init-hook
	       '(lambda ()
		  (default-directory-lisp ilisp-last-buffer)))))



;;; To be honest, I'd suggest everyone disable the popper, bug or no bug.
;;; If you like it great. If you want to disembowel the thing, here's how:
(setq lisp-no-popper t)
(setq popper-load-hook 
      '(lambda ()
	(setq popper-pop-buffers nil)
	(setq popper-buffers-to-skip nil)))

