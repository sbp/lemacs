;;; This is a replacement for view-mode
;;; that looks a lot like less.  It also acts like a minor mode, and
;;; doesn't rebind any keys that it doesn't have to.  

;; Written by David Gudeman (gudeman@arizona.edu)
;; Gnu Emacs v18 only.

;; Mods by Bengt Martensson, to closely resemble less
;; LastEditDate "Thu Jul 23 13:23:24 1987"

;; July 87, Gudeman again: added prefix for "q",

;; to make auto-view-mode work automatically when you read a
;; write-protected file, add the following to your .emacs file.
;;(or (member 'auto-view-mode find-file-hooks)
;;    (setq find-file-hooks (cons 'auto-view-mode find-file-hooks)))

(provide 'view)

(defvar view-search-string ""
  "Last string searched for with view-search functions.")

(defvar view-search-arg 1
  "Arg to last view search.")

(defvar view-default-lines 10
  "Default value for the \"d\" and \"u\" commands in view-mode")

(defvar view-kill-on-exit nil)

(defvar view-mode-map nil)
(if view-mode-map
    nil
  (setq view-mode-map (make-keymap))
  (set-keymap-name view-mode-map 'view-mode-map)
  (suppress-keymap view-mode-map)
  (define-key view-mode-map "-" 'negative-argument)
  (define-key view-mode-map " " 'scroll-up)
  (define-key view-mode-map "f" 'scroll-up)
  (define-key view-mode-map "\177" 'scroll-down)
  (define-key view-mode-map "b" 'scroll-down)
  (define-key view-mode-map 'backspace 'scroll-down)
  (define-key view-mode-map "\r" 'view-scroll-lines-up)
  (define-key view-mode-map "\n" 'view-scroll-lines-up)
  (define-key view-mode-map "e" 'view-scroll-lines-up)
  (define-key view-mode-map "j" 'view-scroll-lines-up)
  (define-key view-mode-map "y" 'view-scroll-lines-down)
  (define-key view-mode-map "k" 'view-scroll-lines-down)
  (define-key view-mode-map "d" 'view-scroll-some-lines-up)
  (define-key view-mode-map "u" 'view-scroll-some-lines-down)
  (define-key view-mode-map "r" 'recenter)
  (define-key view-mode-map "t" 'toggle-truncate-lines)
  (define-key view-mode-map "N" 'view-buffer)
  (define-key view-mode-map "E" 'view-file)
  (define-key view-mode-map "P" 'view-buffer)
  (define-key view-mode-map "!" 'shell-command)
  (define-key view-mode-map "|" 'shell-command-on-region)
  (define-key view-mode-map "=" 'what-line)
  (define-key view-mode-map "?" 'view-search-backward)
  (define-key view-mode-map "h" 'view-mode-describe)
  (define-key view-mode-map "s" 'view-repeat-search)
  (define-key view-mode-map "n" 'view-repeat-search)
  (define-key view-mode-map "/" 'view-search-forward)
  (define-key view-mode-map "\\" 'view-search-backward)
  (define-key view-mode-map "g" 'view-goto-line)
  (define-key view-mode-map "G" 'view-Goto-line)
  (define-key view-mode-map "%" 'view-goto-percent)
  (define-key view-mode-map "p" 'view-goto-percent)
  (define-key view-mode-map "m" 'point-to-register)
  (define-key view-mode-map "'" 'register-to-point)
  (define-key view-mode-map "C" 'view-cleanup-backspaces)
  (define-key view-mode-map "q" 'view-quit))

;;;###autoload
(defun view-file (file &optional p)
  "Find FILE, enter view mode.  With prefix arg use other window."
  (interactive "fView File: \nP")
  (let ((new-p (null (get-file-buffer file))))
    (if p
	(find-file-other-window file)
      (find-file file))
    (view-mode)
    (set (make-local-variable 'view-kill-on-exit) new-p)
    (view-brief-help)
    nil))

;;;###autoload
(defun view-buffer (buf &optional p)
  "Switch to BUF, enter view mode.  With prefix arg use other window."
  (interactive "bView Buffer: \nP")
  (if p
      (switch-to-buffer-other-window buf)
    (switch-to-buffer buf))
  (view-mode)
  (view-brief-help))

(defun view-file-other-window (file)
  "Find FILE in other window, and enter view mode."
  (view-file file t))

(defun view-buffer-other-window (buffer)
  "Switch to BUFFER in another window, and enter view mode."
  (view-buffer buffer t))

(defun view-brief-help ()
  (message
   (substitute-command-keys
    "\\[scroll-up] = page forward;\\[scroll-down] = page back;\
 \\[view-mode-describe] = help; \\[view-quit] = quit.")))

(defvar view-last-mode)

;;;###autoload
(defun view-mode (&optional p)
  "Mode for viewing text, with bindings like `less'.
Commands are:
\\<view-mode-map>
0..9	prefix args
-	prefix minus
\\[scroll-up]	page forward
\\[scroll-down]	page back
\\[view-scroll-lines-up]	scroll prefix-arg lines forward, default 1.
\\[view-scroll-lines-down]	scroll prefix-arg lines backward, default 1.
\\[view-scroll-some-lines-down]	scroll prefix-arg lines backward, default 10.
\\[view-scroll-some-lines-up]	scroll prefix-arg lines forward, default 10.
\\[what-line]	print line number
\\[view-mode-describe]	print this help message
\\[view-search-forward]	regexp search, uses previous string if you just hit RET
\\[view-search-backward]	as above but searches backward
\\[view-repeat-search]	repeat last search
\\[view-goto-line]	goto line prefix-arg, default 1
\\[view-Goto-line]	goto line prefix-arg, default last line
\\[view-goto-percent]	goto a position by percentage
\\[toggle-truncate-lines]	toggle truncate-lines
\\[view-file]	view another file
\\[view-buffer]	view another buffer
\\[view-cleanup-backspaces]	cleanup backspace constructions
\\[shell-command]	execute a shell command
\\[shell-command-on-region]\
	execute a shell command with the region as input
\\[view-quit]	exit view-mode, and bury the current buffer.

If invoked with the optional (prefix) arg non-nil, view-mode cleans up
backspace constructions.

More precisely:
\\{view-mode-map}"
  (interactive "P")
;;  (kill-all-local-variables) ; No, this is very bad.  Don't reset mode.
  (make-local-variable 'view-default-lines)
  (set (make-local-variable 'view-kill-on-exit) nil)
  ;; this lets the prevailing local map be accessible too.
  (let ((map (copy-keymap view-mode-map)))
    (set-keymap-parent map (current-local-map))
    (use-local-map map))
  ;; save previous major-mode
  (set (make-local-variable 'view-last-mode) major-mode)
  (setq mode-name "View")
  (setq major-mode 'view-mode)
  (if p (cleanup-backspaces))
  (setq mode-line-buffer-identification (list "View: %17b"))
  (setq buffer-read-only t))

(defun cleanup-backspaces ()
  "Cleanup backspace constructions.
_^H and ^H_ sequences are deleted.  x^Hx sequences are turned into x for all
characters x.  ^^H| and |^H^ sequences are turned into ^.  +^Ho and o^H+ are
turned into (+)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (= (following-char) ?\C-h)
      (delete-char 1))
    (while (search-forward "\C-h" nil t)
      (forward-char -2)
      (cond ((looking-at "_\C-h\\|\\(.\\)\C-h\\1\\||\C-h\\^")
	     (delete-char 2))
	    ((looking-at ".\C-h_\\|\\^\C-h|")
	     (forward-char 1)
	     (delete-char 2))
	    ((looking-at "+\C-ho\\|o\C-h+")
	     (delete-char 3)
	     (insert "(+)"))
	    ((looking-at "|\C-h-")
	     (delete-char 3)
	     (insert "*"))
	    (t (forward-char 2))))))

(defun toggle-truncate-lines (&optional p)
  "Toggles the values of truncate-lines."
  (interactive "P")
  (setq truncate-lines
	(cond ((null p) (not truncate-lines))
	      ((= 0 (prefix-numeric-value p)) nil)
	      (t t)))
  (recenter))

(defun view-cleanup-backspaces ()
  "Cleanup backspaces and if buffer is currently unmodified, don't flag it
as a modified buffer.  This works even if the buffer is read-only."
  (interactive)
  (let ((buffer-read-only)(buf-mod (buffer-modified-p)))
    (cleanup-backspaces)
    (set-buffer-modified-p buf-mod)))

(defun view-scroll-lines-up (p)
  "Scroll up prefix-arg lines, default 1."
  (interactive "p")
  (scroll-up p))

(defun view-scroll-lines-down (p)
  "Scroll down prefix-arg lines, default 1."
  (interactive "p")
  (scroll-up (- p)))

(defun view-scroll-some-lines-down (&optional n)
  "Scroll down prefix-arg lines, default 10, or last argument."
  (interactive "p")
  (if (> n 1) (setq view-default-lines n))
  (scroll-down view-default-lines))

(defun view-scroll-some-lines-up (&optional n)
  "Scroll up prefix-arg lines, default 10, or last argument."
  (interactive "p")
  (if (> n 1) (setq view-default-lines n))
  (scroll-up view-default-lines))

(defun view-goto-line (&optional n)
  "Goto line prefix, default 1."
  (interactive "p")
  (goto-line n))

(defun view-Goto-line (&optional n)
  "Goto line prefix, default last line."
  (interactive "p")
  (if current-prefix-arg (goto-line n)
    (end-of-buffer)
    (recenter -1)
    (move-to-window-line 0)))

(defun view-goto-percent (&optional p)
  "Sets mark and goes to a position PERCENT percent of the file."
  (interactive "p")
  (set-mark-command nil)
  (goto-char (+ (point-min) (/ (* p (- (point-max) (point-min))) 100)))
  (beginning-of-line))

(defun view-mode-describe ()
  (interactive)
  (let ((mode-name "View")
	(major-mode 'view-mode))
    (describe-mode)))

(defun view-search-forward (s p)
  "Search forward for REGEXP.  If regexp is empty, use last search string.
With prefix ARG, search forward that many occurrences."
  (interactive "sView search: \np")
  (unwind-protect
      (word-search-forward
       (if (string= "" s) view-search-string s) nil nil p)
    (setq view-search-arg p)
    (or (string= "" s)
	(setq view-search-string s))))

(defun view-search-backward (s p)
  "Search backward for REGEXP.  If regexp is empty, use last search string.
With prefix ARG, search forward that many occurrences."
  (interactive "sView search backward: \np")
  (view-search-forward s (- p)))

(defun view-repeat-search (p)
  "Repeat last view search command.  If a prefix arg is given, use that
instead of the previous arg, if the prefix is just a -, then take the
negative of the last prefix arg."
  (interactive "P")
  (view-search-forward
   view-search-string
   (cond ((null p) view-search-arg)
	 ((eq p '-) (- view-search-arg))
	 (t (prefix-numeric-value p)))))

(defun view-quit (&optional p)
  "Switch to another buffer and bury this one.
If the buffer being viewed had not been in a buffer already, it is killed.
With a prefix arg, it will be buried instead of killed."
  (interactive "P")
  (let ((b (current-buffer))
	(old-mode (and (boundp 'view-last-mode) view-last-mode)))
    (if (and view-kill-on-exit (not p))
	(kill-buffer b)
      (kill-all-local-variables)
      (normal-mode)
      ;; this might not be a file buffer; if normal-mode didn't put us
      ;; in some sensible mode, switch to the previous mode if possible.
      (if (and old-mode
	       (eq major-mode (or default-major-mode 'fundamental-mode))
	       (not (eq old-mode major-mode)))
	  (condition-case nil
	      (funcall old-mode)
	    (error nil)))
      (switch-to-buffer (other-buffer b))
      (bury-buffer b))))

(defun auto-view-mode ()
  "If the file of the current buffer is not writable, call view-mode.
  This is meant to be added to find-file-hooks."
  (if (and buffer-file-name
	   (not (file-writable-p buffer-file-name))) (view-mode)))
