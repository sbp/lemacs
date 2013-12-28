;;; sunpro-keys.el --- SunPro-specific key bindings

;; Copyright (C) 1993, 1994 Sun Microsystems, Inc

(define-key global-map 'find 'x-isearch-maybe-with-region)

(define-key isearch-mode-map 'f18 'isearch-yank-x-clipboard)
(add-hook 'isearch-mode-hook 'sunpro-set-isearch-direction)
(define-key isearch-mode-map 'f19 'isearch-repeat-forward)

(defun x-isearch-maybe-with-region (&optional backward-p)
  "Enter isearch mode.  If the region is active, find the selected text."
  (interactive "P")
  (let ((sunpro-isearch-direction
	 (if backward-p 'backward 'forward)))
    (if (and zmacs-regions (mark))
	(progn (isearch-mode (not backward-p)) (isearch-yank-x-selection))
      (if backward-p (isearch-backward) (isearch-forward)))))

(defun sunpro-set-isearch-direction ()
  (if (or (eq this-command 'isearch-backward)
	  (eq this-command 'isearch-backward-regexp)
	  (and (boundp 'sunpro-isearch-direction)
	       (eq sunpro-isearch-direction 'backward)))
      (define-key isearch-mode-map 'f19 'isearch-repeat-backward)
    (define-key isearch-mode-map 'f19 'isearch-repeat-forward)))
