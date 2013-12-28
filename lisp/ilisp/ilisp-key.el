;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-key.el,v 1.20 1993/09/03 02:05:07 ivan Rel $
;;;%Header
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell, ccm@cs.cmu.edu.
;;;
;;; Send mail to ilisp-bug@darwin.bu.edu if you have problems.
;;;
;;; Send mail to ilisp-request@darwin.bu.edu if you want to be on the
;;; ilisp mailing list.
;;;
;;;


;;;
;;; ILISP keybinding definitions.
;;;

;;;
;;;%Bindings
(defun ilisp-defkey (keymap key command)
  "Define KEYMAP ilisp-prefix+KEY as command."
  (let ((prefix-map (lookup-key keymap ilisp-prefix)))
    (if (not (keymapp prefix-map))
	(setq prefix-map
	      (define-key keymap ilisp-prefix (make-sparse-keymap))))
    (define-key prefix-map key command)))

(defun defkey-ilisp (key command &optional inferior-only)
  "Define KEY as COMMAND in ilisp-mode-map and lisp-mode-map unless
optional INFERIOR-ONLY is T.  If the maps do not exist they will be
created.  This should only be called after ilisp-prefix is set to the
desired prefix."
  (if (not ilisp-mode-map) (ilisp-bindings))
  (define-key ilisp-mode-map key command)
  (define-key lisp-mode-map key command))

;;;
(defun lisp-bindings (keymap &optional inferior-p)
  "Set up the bindings for interacting with an inferior LISP in
KEYMAP."
  (if inferior-p
      (progn (define-key keymap "\C-m" 'return-ilisp)
	     (define-key keymap "\C-a" 'bol-ilisp)
	     (define-key keymap "\C-c\C-c" 'interrupt-subjob-ilisp)
	     (define-key keymap "\C-d" 'delete-char-or-pop-ilisp)
	     (ilisp-defkey keymap "#" 'raw-keys-ilisp))
      (ilisp-defkey keymap "\C-c" 'compile-defun-and-go-lisp)
      (define-key keymap "\C-m" 'newline-and-indent-lisp))

  (define-key   keymap "]"        'close-all-lisp)
  (define-key   keymap "\M-q"     'reindent-lisp)
  (define-key   keymap "\C-]"     'close-and-send-lisp)
  (define-key   keymap "\t"       'indent-line-ilisp)
  (define-key   keymap "\n"       'newline-and-indent-lisp)
  (define-key   keymap "\M-\C-q"  'indent-sexp-ilisp)
  (ilisp-defkey keymap ";"        'comment-region-lisp)
  (ilisp-defkey keymap ")"        'find-unbalanced-lisp)
  (define-key   keymap "\M-\C-a"  'beginning-of-defun-lisp)
  (define-key   keymap "\M-\C-e"  'end-of-defun-lisp)
  (define-key   keymap "\C-\M-r"  'reposition-window-lisp)
  (ilisp-defkey keymap "i"        'describe-lisp)
  (ilisp-defkey keymap "I"        'inspect-lisp)
  (ilisp-defkey keymap "a"        'arglist-lisp)
  (ilisp-defkey keymap "d"        'documentation-lisp)
  (ilisp-defkey keymap "m"        'macroexpand-1-lisp)
  (ilisp-defkey keymap "M"        'macroexpand-lisp)
  (define-key   keymap "\M-,"     'next-definition-lisp)
  (define-key   keymap "\M-."     'edit-definitions-lisp)
  (define-key   keymap "\M-?"     'search-lisp)
  (define-key   keymap "\M-\""    'replace-lisp)
  (ilisp-defkey keymap "^"        'edit-callers-lisp)
  (define-key   keymap "\M-`"     'next-caller-lisp)
  (define-key   keymap "\M-\t"    'complete-lisp)
  (define-key   keymap "\M-\C-m"  'complete)
  (ilisp-defkey keymap "r"        'eval-region-lisp)
  (define-key   keymap "\M-\C-x"  'eval-defun-lisp) ; Gnu convention
  (ilisp-defkey keymap "e"        'eval-defun-lisp)
  (ilisp-defkey keymap "n"        'eval-next-sexp-lisp)
  (ilisp-defkey keymap "p"        'package-lisp)
  (ilisp-defkey keymap "P"        'set-package-lisp)
  (ilisp-defkey keymap "w"        'compile-region-lisp)
  (ilisp-defkey keymap "c"        'compile-defun-lisp)
  (ilisp-defkey keymap "\C-r"     'eval-region-and-go-lisp)
  (ilisp-defkey keymap "\C-e"     'eval-defun-and-go-lisp)
  (ilisp-defkey keymap "\C-n"     'eval-next-sexp-and-go-lisp)
  (ilisp-defkey keymap "\C-w"     'compile-region-and-go-lisp)
  (ilisp-defkey keymap "t"        'trace-defun-lisp)
  (ilisp-defkey keymap "!"        'default-directory-lisp)
  (ilisp-defkey keymap " "        'mark-change-lisp)
  (let ((ilisp-prefix (concat ilisp-prefix "*")))
    (ilisp-defkey keymap "l"      'list-changes-lisp)
    (ilisp-defkey keymap "e"      'eval-changes-lisp)
    (ilisp-defkey keymap "c"      'compile-changes-lisp)
    (ilisp-defkey keymap "0"      'clear-changes-lisp))
  (ilisp-defkey keymap "b"        'switch-to-lisp)
  (ilisp-defkey keymap "y"        'call-defun-lisp)
  (ilisp-defkey keymap "z"        'reset-ilisp)
  (ilisp-defkey keymap "g"        'abort-commands-lisp)
  (ilisp-defkey keymap "s"        'status-lisp)
  (ilisp-defkey keymap "S"        'select-ilisp)
  (define-key   keymap "\C-x\C-f" 'find-file-lisp)
  (ilisp-defkey keymap "l"        'load-file-lisp)
  (ilisp-defkey keymap "k"        'compile-file-lisp)
  (ilisp-defkey keymap "A"        'fi:clman-apropos)
  (ilisp-defkey keymap "D"        'fi:clman))

;;;
(defun ilisp-bindings ()
  "Set up the key bindings for LISP and ILISP buffers."
  (if (fboundp 'set-keymap-parent) 
      (progn 
	(setq ilisp-mode-map (make-sparse-keymap))
	(set-keymap-parent ilisp-mode-map comint-mode-map))
    (setq ilisp-mode-map (full-copy-sparse-keymap comint-mode-map)))
  ;; Remove stop and quit subjob from comint
  (define-key ilisp-mode-map "\C-c\C-z" nil)
  (define-key ilisp-mode-map "\C-c\C-\\" nil)
  (if (fboundp 'lisp-mode-commands)
      (lisp-mode-commands ilisp-mode-map))
  (lisp-bindings ilisp-mode-map t)
  (if (boundp 'lisp-mode-map) 
      (lisp-bindings lisp-mode-map))
  (if (boundp 'scheme-mode-map) 
      (lisp-bindings scheme-mode-map))
  (ilisp-defkey emacs-lisp-mode-map ";" 'comment-region-lisp)

  (ilisp-defkey global-map "\C-t" 'trace-defun-lisp-break)
  (ilisp-defkey global-map "b" 'switch-to-lisp)
  (ilisp-defkey global-map "1" 'popper-bury-output)
  (ilisp-defkey global-map "v" 'popper-scroll-output)
  (ilisp-defkey global-map "G" 'popper-grow-output)
  (if (not (boundp 'fi:clman-mode-map))
      (setq fi:clman-mode-map (make-sparse-keymap)))
  (ilisp-defkey fi:clman-mode-map "D" 'fi:clman)
  (ilisp-defkey fi:clman-mode-map "A" 'fi:clman-apropos))



(provide 'ilisp-key )
