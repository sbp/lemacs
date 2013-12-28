
(defun x-store-small-cutbuffer (string)
  (if (< (length string) 1000)
      (x-store-cutbuffer string)))

(cond ((string-match "Lucid" emacs-version)
       ;;
       ;; Code for any version of Lucid Emacs goes here
       ;;
       (setq find-file-use-truenames nil
	     find-file-compare-truenames t
	     minibuffer-confirm-incomplete t)

       (setq-default mode-line-buffer-identification '("lemacs: %17b"))
       (setq mode-line-buffer-identification '("lemacs: %17b"))

       (cond ((eq window-system 'x)
	      ;;
	      ;; Code which applies only when running emacs under X goes here.
	      ;; (Currently, this is always the case in lemacs, but it will
	      ;; not be in the future.)
	      ;;
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
       (define-key emacs-lisp-mode-map '(control C) 'compile-defun)
       (define-key emacs-lisp-mode-map '(control E) 'eval-defun)

       ;; Make backspace and delete be the same.  This doesn't work in all
       ;; cases; a better way would be to use xmodmap.
       (global-set-key 'backspace [delete])
       (global-set-key '(meta backspace) [(meta delete)])
       (global-set-key '(control backspace) [(control delete)])
       (global-set-key '(meta control backspace) [(meta control delete)])

       ;; Make F5 be "Undo"
       (global-set-key 'f5 'undo)

       ;; Make F6 be "save-file" followed by "delete-window".
       (global-set-key 'f6 "\C-x\C-s\C-x0")

       ;; Make ^X^M and ^X RET be different (since I do the latter by accident)
       (define-key global-map [(control x) return] nil)

       ;; Change the cursor used when the mouse is over a mode line
       (setq x-mode-pointer-shape "leftbutton")

       ;; Change the cursor used during GC.
       ;; Note that this cursor image is rather large as cursor go, and so it
       ;; won't work on some X servers (such as the MIT R5 Sun server) because
       ;; servers may have lamentably small upper limits on cursor size.
       (if (featurep 'xpm)
	   (setq x-gc-pointer-shape
		 (expand-file-name "trash.xpm" data-directory)))

       ;; Add `dired' to the File menu
       (add-menu-item '("File") "Edit Directory" 'dired t)

       ;; Since we don't have scrollbars (yet!) add some functionally
       ;; similar buttons to the menubar.
       (add-menu-item nil "Top" 'beginning-of-buffer t)
       (add-menu-item nil "<<<" 'scroll-down t)
       (add-menu-item nil " . " 'recenter t)
       (add-menu-item nil ">>>" 'scroll-up t)
       (add-menu-item nil "Bot" 'end-of-buffer t)

       ))

(cond ((and (string-match "Lucid" emacs-version)
	    (not (string-lessp emacs-version "19.6")))
       ;;
       ;; Code which requires Lucid Emacs version 19.6 or newer goes here
       ;;
       ))

(cond ((not (string-lessp emacs-version "19"))
       ;;
       ;; Code for any vintage-19 emacs goes here
       ;;
       ))

(cond ((and (not (string-match "Lucid" emacs-version))
	    (not (string-lessp emacs-version "19")))
       ;;
       ;; Code specific to FSF Emacs 19 (not Lucid Emacs) goes here
       ;;
       ))

(cond ((string-lessp emacs-version "19")
       ;;
       ;; Code specific to emacs 18 goes here
       ;;
       ))
