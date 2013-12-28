
(defun x-store-small-cutbuffer (string)
  (if (< (length string) 1000)
      (x-store-cutbuffer string)))

(cond ((string-match "Lucid" emacs-version)

       (setq find-file-use-truenames nil
	     find-file-compare-truenames t
	     minibuffer-confirm-incomplete t)

       (setq-default mode-line-buffer-identification '("Emacs19: %17b"))
       (setq mode-line-buffer-identification '("Emacs19: %17b"))

       (cond ((eq window-system 'x)
	      (global-set-key "\C-x\C-c" nil)
	      ;; I often kill big chunks of text (hundreds of k) and it's way
	      ;; too slow to send all this to the server each time, especially
	      ;; when I'm using my NCD from home.
	      (setq kill-hooks (delq 'x-store-cutbuffer kill-hooks))
	      (or (memq 'x-store-small-cutbuffer kill-hooks)
		  (setq kill-hooks (append kill-hooks
					   '(x-store-small-cutbuffer))))

	      (if (equal screen-title-format "%S: %b")
		  ;; that's to avoid interfering with the "-wn" argument.
		  (setq screen-title-format
			(concat "%S: " execution-path " [" emacs-version "]"
				(if nil ; (getenv "NCD")
				    ""
				  "   %b"))))
	      (if (getenv "NCD")
		  (setq bell-volume 40)
		(load-default-sounds))
	      ))

       ;; insert selection at point, instead of at position clicked!!
       (define-key global-map 'button2 'x-insert-selection)

       ;; LISPM bindings of Control-Shift-C and Control-Shift-E
       (define-key emacs-lisp-mode-map '(control C) 'elisp-compile-defun)
       (define-key emacs-lisp-mode-map '(control E) 'eval-defun)

       ;; Make backspace and delete be the same.  This doesn't work in all
       ;; cases; a better way would be to use xmodmap.
       (define-key global-map 'backspace [delete])
       (define-key global-map '(meta backspace) [(meta delete)])
       (define-key global-map '(control backspace) [(control delete)])
       (define-key global-map '(meta control backspace) [(meta control delete)])

       ;; Make ^X^M and ^X RET be different (since I do the latter by accident)
       (define-key global-map [(control x) return] nil)
       ))
