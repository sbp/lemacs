(defconst era-version "0.93" "\
Version numbers of this version of Era.")

;; We're calling this version Sun Era.
(defconst sun-era t)

(defun era-version () "\
Return string describing the version of Era that is running."
  (interactive)
  (if (interactive-p)
      (message "%s" (era-version))
    (format "%sEra %s of %s %s on %s (%s)"
	    (if sun-era "Sun " "")
	    era-version
	    (substring emacs-build-time 0
		       (string-match " *[0-9]*:" emacs-build-time))
	    (substring emacs-build-time
                       (string-match "[0-9]*$" emacs-build-time))
	    emacs-build-system system-type)))

(setq bar-cursor 2)

(defun sunpro-maybe-connect-to-tooltalk ()
  (if (and (not (noninteractive))
	   (fboundp 'command-line-do-tooltalk))
      (command-line-do-tooltalk nil)))

;; sunpro-maybe-connect-to-tooltalk must appear in the hook list
;; before any clients that register patterns, like eos-load.el.
;; Currently eos-load.el places its functions at the end of the list

(add-hook 'before-init-hook 'sunpro-maybe-connect-to-tooltalk)

(setq x-pointer-shape "xterm")
(setq x-nontext-pointer-shape "xterm")

;; W3 doesn't know about using pageview, so let's fix that.

(defun sunpro-fix-postscript-viewer ()
  (if (not (noninteractive))
    (condition-case nil
      (w3-parse-mailcap
        (expand-file-name "sparcworks/sunpro-mailcap" data-directory))
      (error nil))))

(add-hook 'w3-load-hooks 'sunpro-fix-postscript-viewer)

(provide 'sunpro)

