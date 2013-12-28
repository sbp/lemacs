(defun SeLF-insert-command (arg)
  "Insert the character you TyPE.
Whichever character you TyPE to run ThIS command is inserted."
  (interactive "p")
  (let ((p (point))
	(case-fold-search nil))
    (self-insert-command arg)
    (save-excursion
      (goto-char p)
      (skip-chars-backward " \t\r\n")
      (if (condition-case () (forward-char -4) (error t))
	  nil
	(if (looking-at "\\<[A-Za-z][a-z][a-z][a-z][^A-Za-z]")
	    (progn
	      (insert (upcase (following-char))) (delete-char 1)
	      (forward-char 1)
	      (insert (upcase (following-char))) (delete-char 1)
	      (insert (upcase (following-char))) (delete-char 1)))))))

(define-key text-mode-map " " 'SeLF-insert-command)
(define-key text-mode-map "," 'SeLF-insert-command)
(define-key text-mode-map "." 'SeLF-insert-command)
(define-key text-mode-map "!" 'SeLF-insert-command)
(define-key text-mode-map "-" 'SeLF-insert-command)
(define-key text-mode-map "_" 'SeLF-insert-command)
(define-key text-mode-map ";" 'SeLF-insert-command)
(define-key text-mode-map ":" 'SeLF-insert-command)
