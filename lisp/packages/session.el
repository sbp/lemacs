(setq kill-emacs-hook
      '(save-session))

;; Record in `/.emacs-session' a command to re-visit the last file
;; that was being looked at.
(defun save-session ()
  (and buffer-file-name
       (write-region
	(concat "(" (if (eq major-mode 'rmail-mode)
			"rmail"
		      "find-file")
		" " (if (string= (concat (getenv "HOME") "/RMAIL")
				 buffer-file-name)
			;; Omit the file name if it is the primary rmail file
			;; to cause the default inboxes to be used.
			""
		      (prin1-to-string buffer-file-name))
		")\n"
		"(goto-char " (prin1-to-string (window-start)) ")\n"
		"(beginning-of-line)\n"
		"(set-window-start (selected-window) (point))\n"
		"(goto-line "
		(prin1-to-string (1+ (count-lines 1 (point))))
		")\n")
	nil (expand-file-name "~/.emacs-session")
	;; Don't print a message.
	nil 'lambda)))

;; Restore previous saved session, if there is one.
(load "~/.emacs-session" t t)
