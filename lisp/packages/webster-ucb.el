;;; -*- Mode:Emacs-Lisp -*-

;;; Yet Another Webster Protocol.
;;; This one is for talking to the kind of Webster server of which 
;;; pasteur.Berkeley.EDU port 1964 is an instance (the "edjames" protocol).
;;;
;;; The interface and much of the process-handling code in this file were
;;; lifted from the Webster client by Jason Glasgow that talks to the kind
;;; of Webster server of which mintaka.lcs.mit.edu port 103 is an instance.
;;;
;;; 13 nov 90  Jamie Zawinski <jwz@lucid.com>  created
;;; 14 sep 91  Jamie Zawinski <jwz@lucid.com>  hacked on some more
;;; 19 feb 91  Jamie Zawinski <jwz@lucid.com>  added Lucid Emacs font support
;;; 15 apr 92  Jamie Zawinski <jwz@lucid.com>  added mouse support
;;; 29 aug 92  Jamie Zawinski <jwz@lucid.com>  added 8-bit output
;;;  6 nov 92  Jamie Zawinski <jwz@lucid.com>  hack hack
;;; 31 dec 92  Jamie Zawinski <jwz@lucid.com>  made it guess the root word
;;; 17 mar 93  Jamie Zawinski <jwz@lucid.com>  more hacking, more gnashing
;;; 31 jul 93  Jamie Zawinski <jwz@lucid.com>  variable height fonts in 19.8

;; TODO:
;; 
;; vinculum has a "3 character overbar" code.  Really need to figure out
;; some way to hack overbars...  Background pixmap?  Need to know line
;; height in pixels to do that.  
;;
;; I don't event know what half of these special characters are supposed
;; to look like.  Like the "s," in the Turkish root of "chouse"...
;;
;; We could fake some of these chars (like upside-down-e) by including bitmaps
;; in this file, and using extent-begin-glpyhs.  Except that right now glyphs
;; have to come from files, not from '(w h "string") form, so that'll have to
;; be fixed first.  We could also just create an X font...
;;
;; note that googol says "10100" instead of "10(\bI100)\bI

(defvar webster-host "pasteur" "*The host with the webster server")
(defvar webster-port "1964" "*The port on which the webster server listens")

(defvar webster-running nil "Used to determine when connection is established")
(defvar webster-state "closed" "for the modeline")
(defvar webster-process nil "The current webster process")
(defvar webster-process-name "webster" "The current webster process")
(defvar webster-buffer nil "The current webster process")

(defvar webster-start-mark nil)

(defvar webster-fontify (string-match "Lucid" emacs-version)
  "*Set to t to use the Lucid Emacs font-change mechanism.")

(defvar webster-iso8859/1 (string-match "Lucid" emacs-version)
  "*Set to t to print certain special characters using ISO-8859/1 codes.")

(defconst webster-completion-table (make-vector 511 0))

(cond ((fboundp 'make-face)
       (or (find-face 'webster)
	   (face-differs-from-default-p (make-face 'webster))
	   (copy-face 'default 'webster))
       (or (find-face 'webster-bold)
	   (face-differs-from-default-p (make-face 'webster-bold))
	   (progn
	     (copy-face 'webster 'webster-bold)
	     (make-face-bold 'webster-bold)))
       (or (find-face 'webster-italic)
	   (face-differs-from-default-p (make-face 'webster-italic))
	   (progn
	     (copy-face 'webster 'webster-italic)
	     (make-face-italic 'webster-italic)))
       (or (find-face 'webster-bold-italic)
	   (face-differs-from-default-p (make-face 'webster-bold-italic))
	   (progn
	     (copy-face 'webster 'webster-bold-italic)
	     (make-face-bold-italic 'webster-bold-italic)))
       (or (find-face 'webster-underline)
	   (face-differs-from-default-p (make-face 'webster-underline))
	   (progn
	     (copy-face 'webster 'webster-underline)
	     (set-face-underline-p 'webster-underline t)))
       (or (find-face 'webster-small)
	   (face-differs-from-default-p (make-face 'webster-small))
	   (progn
	     (copy-face 'webster-bold 'webster-small)
	     (and (fboundp 'make-face-smaller)	; lemacs 19.8+
		  (make-face-smaller 'webster-small))))
       (or (find-face 'webster-subscript)
	   (face-differs-from-default-p (make-face 'webster-subscript))
	   (progn
	     (copy-face 'webster-italic 'webster-subscript)
	     (if (fboundp 'make-face-smaller)	; lemacs 19.8+
		 (and (make-face-smaller 'webster-subscript)
		      (make-face-smaller 'webster-subscript))
	       (set-face-underline-p 'webster-subscript t))))
       (or (find-face 'webster-superscript)
	   (face-differs-from-default-p (make-face 'webster-superscript))
	   ;; #### need some way to raise baseline...
	   (copy-face 'webster-subscript 'webster-superscript))
       ))

(defun webster-fontify (start end face &optional highlight)
  (let ((os start)
	(count 0)
	e)
    (save-excursion
      (goto-char start)
      ;; this mess is so we don't fontify the spaces between the words, so that
      ;; when the lines are wrapped, the stuff at the beginning of the line
      ;; doesn't go in the font of the split word.  Kludge kludge.
      (while (prog1
		 (/= (point) end)
	       (skip-chars-forward " \t")
	       (setq start (point))
	       (re-search-forward "[ \t]" (1+ end) 'go)
	       (forward-char -1))
	(setq e (make-extent start (point) (current-buffer)))
	(set-extent-face e face)
	(setq count (1+ count))))
    (if highlight
	(set-extent-attribute
	 ;; use the same extent if we didn't have to split it.
	 (if (= count 1) e (make-extent os end (current-buffer)))
	 'highlight))
    ))

(defconst webster-umlauts
  '((?A . ?\304) (?E . ?\313) (?I . ?\317) (?O . ?\326) (?U . ?\334)
    (?a . ?\344) (?e . ?\353) (?i . ?\357) (?o . ?\366) (?u . ?\374)
    (?y . ?\377)))

(defconst webster-graves
  '((?A . ?\300) (?E . ?\310) (?I . ?\314) (?O . ?\322) (?U . ?\331)
    (?a . ?\340) (?e . ?\350) (?i . ?\354) (?o . ?\362) (?u . ?\371)))

(defconst webster-acutes
  '((?A . ?\301) (?E . ?\311) (?I . ?\315) (?O . ?\323) (?U . ?\332)
    (?Y . ?\335) (?a . ?\341) (?e . ?\351) (?i . ?\355) (?o . ?\363)
    (?u . ?\372) (?y . ?\375)))

;;;
;;; Initial filter for ignoring information until successfully connected
;;;
(defun webster-initial-filter (proc string)
  (let ((this-buffer (current-buffer)))
    ;; don't use save-excursion so that point moves in webster-buffer
    (set-buffer webster-buffer)
    (goto-char (point-max))
    (setq webster-state "closed")
    (cond ((not (eq (process-status webster-process) 'run))
	   (setq webster-running t)
	   (message "Webster died"))
	  ((string-match "No such host" string)
	   (setq webster-running t)
	   (kill-buffer (process-buffer proc))
	   (error "No such host."))
	  ((string-match "]" string)
	   (setq webster-running t)
	   (setq webster-state "opening")
	   (set-process-filter proc 'webster-filter)))
    (set-buffer this-buffer)))


(defun webster-filter (proc string)
  (let ((this-buffer (current-buffer))
	(endp nil))
    (set-buffer webster-buffer)
    (widen)
    (goto-char (point-max))
    (cond ((not (eq (process-status webster-process) 'run))
	   (setq webster-state (format "%s" (process-status webster-process)))
	   (set-marker webster-start-mark (point-max))
	   (message "Webster died"))
	  ((string-match "Connection closed" string)
	   (message "Closing webster connection...")
	   (kill-process proc)
	   (setq webster-state "closed")
	   (replace-regexp "Process webster killed" "" nil)
	   (set-marker webster-start-mark (point-max))
	   (message "Closing webster connection...Done."))
	  ((let ((end-def-message (string-match "\n\\.\r?\n" string)))
	     (if end-def-message
		 (progn
		   (webster-filter 
		    proc
		    (concat (substring string 0 (- end-def-message 1)) "\n\n"))
		   (setq endp t)
		   (setq webster-state "ready")
		   t))))
	  (t
	   (setq webster-state "working")
	   (if (string-match "^[45][0-9][0-9]" string)
	       (setq webster-state "ready"
		     endp t))
	   (widen)
	   (let ((now (point)))
	     (goto-char (point-max))
	     (insert string)
	     (save-excursion
	       (goto-char now)
	       (while (search-forward "\r" nil t)
		 (delete-char -1))))
	   (if (process-mark proc)
	       (set-marker (process-mark proc) (point)))
	   (narrow-to-region (point-min) webster-start-mark)
	   ))
    (if endp
	;; if the *webster* window is visible, move the last line to the
	;; bottom of that window
	(let ((webster-window (get-buffer-window webster-buffer))
	      (window (selected-window))
	      error p)
	  (set-buffer webster-buffer)
	  (widen)
	  (goto-char (point-min))
	  (narrow-to-region webster-start-mark (point-max))
	  (let ((buffer-undo-list t))
	    (if (looking-at "WORD \"\\([^\"\n]*\\)\"\\(\n403 [^\n]+\\)\n")
		(progn
		  (downcase-word 1)
		  (setq error
			(buffer-substring (match-beginning 1) (match-end 1)))
		  (goto-char (match-beginning 2))
		  (delete-region (match-beginning 2) (match-end 2))
		  (insert " not found")
		  (setq error (webster-guess-root error))
		  (if error
		      (insert "; trying \"" error "\"...")
		    (insert "."))
		  )
	      (webster-convert)))
	  (widen)
	  (setq p (marker-position webster-start-mark))
	  (goto-char (point-max))
	  (or (bobp)
	      (save-excursion (forward-line -1) (looking-at "-"))
	      (insert "\n--------------------\n"))
	  (set-marker webster-start-mark (point-max))
	  (goto-char p)
	  (if webster-window
	      (progn
		(select-window webster-window)
		(goto-char p)
		(recenter 3)
		(select-window window)))
	  (if error (webster error))))))

(defun webster-guess-root (word)
  (let ((case-fold-search t))
    (cond ((null word) nil)
	  ((string-match "[ \t\n]" word)
	   nil)
	  ((string-match "[^aeiou]ing\\'" word)
	   (concat (substring word 0 (+ 1 (match-beginning 0))) "e"))
	  ((string-match "[a-z]ing\\'" word)
	   (substring word 0 (+ 1 (match-beginning 0))))
	  ((string-match "ies\\'" word)
	   (concat (substring word 0 (match-beginning 0)) "y"))
	  ((string-match "ied\\'" word)
	   (concat (substring word 0 (match-beginning 0)) "y"))
	  ((and (string-match "[^aeiouy][^aeiouy]ed\\'" word)
		(= (aref word (match-beginning 0))
		   (aref word (1+ (match-beginning 0)))))
	   (substring word 0 (+ 1 (match-beginning 0))))
	  ((string-match "[a-z]ed\\'" word)
	   (substring word 0 (+ 2 (match-beginning 0))))
	  ((string-match "[aeiouy]lly\\'" word)
	   (substring word 0 (+ 2 (match-beginning 0))))
	  ((string-match "[^l]ly\\'" word)
	   (substring word 0 (+ 1 (match-beginning 0))))
;	  ((string-match "es\\'" word)
;	   (substring word 0 (match-beginning 0)))
;	  ((string-match "[^e]s\\'" word)
;	   (substring word 0 (+ 1 (match-beginning 0))))
	  ((string-match "s\\'" word)
	   (substring word 0 (match-beginning 0)))
	  ((string-match "...ed\\'" word)
	   (substring word (1- (match-end 0))))
	  (t nil))))


(defun webster (arg)
  "Look up a word in the Webster's dictionary.
Open a network login connection to a webster host if necessary.
Communication with host is recorded in a buffer *webster*."
  (interactive (list
		(let ((prompt (concat "Look up word in webster ("
				      (current-word) "): "))
		      (completion-ignore-case t))
		  (downcase
		   (completing-read prompt webster-completion-table
				    nil nil)))))
  (if (equal "" arg) (setq arg (current-word)))
  (message "looking up %s..." (upcase arg))
  (webster-send-request "WORD" (prin1-to-string arg)))

(defun webster-endings (arg)
  "Look up endings for a word in the Webster's dictionary.
Open a network login connection to a webster host if necessary.
Communication with host is recorded in a buffer *webster*."
  (interactive (list
		(read-string
		 (concat
		  "Find endings for word in webster (" (current-word) "): "))))
  (if (equal "" arg) (setq arg (current-word)))
  (webster-send-request "PREFIX" arg)
  (webster-send-request "LIST" ""))

(defun webster-spell (arg)
  "Look spelling for a word in the Webster's dictionary.
Open a network login connection to a webster host if necessary.
Communication with host is recorded in a buffer *webster*."
  (interactive (list
		(read-string
		 (concat
		  "Try to spell word in webster (" (current-word) "): "))))
  (if (equal "" arg) (setq arg (current-word)))
  (webster-send-request "EXACT" arg)
  (webster-send-request "LIST" arg))


(defun webster-send-request (kind word)
  (require 'shell)
  (let ((webster-command (concat "open " webster-host " " webster-port "\n")))
    (if (or (not webster-buffer)
	    (not (buffer-name webster-buffer))
	    (not webster-process)
	    (not (eq (process-status webster-process) 'run)))
	(progn
	  (message
	   (concat "Attempting to connect to server " webster-host "..."))
	  (setq webster-buffer
		(if (not (fboundp 'make-shell)) ;emacs19
		    (make-comint webster-process-name "telnet")
		  (make-shell webster-process-name "telnet")))
	  (set-buffer webster-buffer)
	  (webster-mode)
	  (setq webster-process (get-process webster-process-name))
	  (process-kill-without-query webster-process)
	  (set-process-filter webster-process 'webster-initial-filter)
	  (process-send-string  webster-process webster-command)
	  (setq webster-running nil)
	  (while (not webster-running)	; wait for feedback
	    (accept-process-output webster-process))
	  (message
	   (concat "Attempting to connect to server " webster-host
		   "... Connected."))
	  ))
    (display-buffer webster-buffer nil)
    (process-send-string webster-process (concat kind " " word "\n"))))

(defun webster-quit ()
  "Close connection and quit webster-mode.  Buffer is not deleted."
  (interactive)
  (message "Closing connection to %s..." webster-host)
  (kill-process webster-process)
  (message "Closing connection to %s...done" webster-host)
  (setq webster-state "closed")
  (if (eq (current-buffer) webster-buffer)
      (bury-buffer)))


(defun webster-xref-data (event &optional selection-only)
  (let* ((buffer (window-buffer (event-window event)))
	 (extent (if buffer (extent-at (event-point event) buffer 'highlight)))
	 text)
    (cond ((and extent (not selection-only))
	   (setq text (save-excursion
			(set-buffer buffer)
			(buffer-substring
			 (extent-start-position extent)
			 (extent-end-position extent)))))
	  ((x-selection-owner-p) ; the selection is in this emacs process.
	   (setq text (x-get-selection))
	   (if (string-match "[\n\r]" text)
	       (setq text nil))))
    (if (null text)
	nil
      (while (string-match "\\." text)
	(setq text (concat (substring text 0 (match-beginning 0))
			   (substring text (match-end 0)))))
      (webster-unISO text)
      text)))

(defun webster-xref-word (event)
  "Define the highlighted word under the mouse.
Words which are known to have definitions are highlighted when the mouse
moves over them.  You may define any word by selecting it with the left
mouse button and then clicking middle."
  (interactive "e")
  (webster (or (webster-xref-data event)
	       (error "click on a highlighted word to define"))))

(defvar webster-menu
  '("Webster"
    ["Define Word..." webster t]
    ["List Words Beginning With..." webster-endings t]
    ["Check Spelling Of..." webster-spell t]
    "----"
    ["Quit Webster" webster-quit t]
    ))

(defun webster-menu (event)
  (interactive "e")
  (let ((text1 (webster-xref-data event nil))
	(text2 (webster-xref-data event t)))
    (if (equal text1 text2) (setq text2 nil))
    (popup-menu
     (nconc (list (car webster-menu))
	    (list "Webster Commands" "----")
	    (if text1 (list (vector (format "Define %s" (upcase text1))
				    (list 'webster text1) t)))
	    (if text2 (list (vector (format "Define %s" (upcase text2))
				    (list 'webster text2) t)))
	    (cdr webster-menu)))))


(defvar webster-mode-map nil)
(if webster-mode-map
    nil
  (setq webster-mode-map (make-sparse-keymap))
  (define-key webster-mode-map "?" 'describe-mode)
  (define-key webster-mode-map "d" 'webster)
  (define-key webster-mode-map "e" 'webster-endings)
  (define-key webster-mode-map "q" 'webster-quit)
  (define-key webster-mode-map "s" 'webster-spell)
  (cond ((string-match "Lucid" emacs-version)
	 (define-key webster-mode-map 'button2 'webster-xref-word)
	 (define-key webster-mode-map 'button3 'webster-menu)))
  )

(defun webster-mode ()
  "Major mode for interacting with on-line Webster's dictionary.
\\{webster-mode-map}
Use webster-mode-hook for customization."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'webster-mode)
  (setq mode-name "Webster")
  (use-local-map webster-mode-map)
  (setq mode-line-process '(" " webster-state))
  (make-local-variable 'kill-buffer-hook)
  (if (not (string= (buffer-name (current-buffer)) "*webster*"))
      (setq kill-buffer-hook '(lambda ()
				(if (get-buffer "*webster*")
				    (kill-buffer "*webster*")))))
  (set (make-local-variable 'webster-start-mark)
       (set-marker (make-marker) (point-max)))
  (set (make-local-variable 'page-delimiter) "^-")
  (if webster-iso8859/1 (setq ctl-arrow 'iso-8859/1))
  (run-hooks 'webster-mode-hook))

;; Snatched from unix-apropos by Henry Kautz
(defun current-word ()
   "Word cursor is over, as a string."
   (save-excursion
      (let (beg end)
	 (re-search-backward "\\w" nil 2)
	 (re-search-backward "\\b" nil 2)
	 (setq beg (point))
	 (re-search-forward "\\w*\\b" nil 2)
	 (setq end (point))
	 (buffer-substring beg end))))

(defun webster-intern (string)
  (intern (webster-strip-crud (webster-unISO (downcase string)))
	  webster-completion-table))

(defun webster-unISO (text)
  ;; turn the ISO chars into the closest ASCII equiv (how they are indexed)
  (while (string-match "\347" text) (aset text (match-beginning 0) ?c))
  (while (string-match "\307" text) (aset text (match-beginning 0) ?C))
  (while (string-match "\335" text) (aset text (match-beginning 0) ?Y))
  (while (string-match "[\375\377]" text) (aset text (match-beginning 0) ?y))
  (while (string-match "[\300-\305]" text) (aset text (match-beginning 0) ?A))
  (while (string-match "[\310-\313]" text) (aset text (match-beginning 0) ?E))
  (while (string-match "[\314-\317]" text) (aset text (match-beginning 0) ?I))
  (while (string-match "[\322-\326]" text) (aset text (match-beginning 0) ?O))
  (while (string-match "[\331-\334]" text) (aset text (match-beginning 0) ?U))
  (while (string-match "[\340-\345]" text) (aset text (match-beginning 0) ?a))
  (while (string-match "[\350-\353]" text) (aset text (match-beginning 0) ?e))
  (while (string-match "[\354-\357]" text) (aset text (match-beginning 0) ?i))
  (while (string-match "[\362-\366]" text) (aset text (match-beginning 0) ?o))
  (while (string-match "[\371-\374]" text) (aset text (match-beginning 0) ?u))
  text)

(defun webster-strip-crud (text)
  (while (string-match ".\b" text)
    (setq text (concat (substring text 0 (match-beginning 0))
		       (substring text (match-end 0)))))
  text)


(defun webster-textify-region (start end &optional nointern)
  (save-excursion
    (goto-char (1- end))
    (if (looking-at "[^\n]\n") (setq end (1+ end)))
    (save-restriction
     (let ((case-fold-search nil))
      (narrow-to-region start end)
      ;; translate silly "special character" codes into something we can use.
      ;; we need to do this before nuking the recursive backspace codes.
      ;;
      ;; Note that mostly these are used as modifiers, like "h(\bQsub-dot)\bQ"
      ;; meaning h with a dot under it.  We don't handle any of that...
      ;;
      (goto-char (point-min))
      (while (re-search-forward "(\bQ[-a-z0-9*$ ]+)\bQ" nil t)
	(goto-char (match-beginning 0))
	(let ((s (point))
	      (e (match-end 0)))
	  (forward-char 3)
	  (if (cond
	       ((looking-at "circumflex")	(insert ?^)	t)
	       ((looking-at "brace")		(insert ?\{)	t)
	       ((looking-at "tilda")		(insert ?\~)	t)
	       ((looking-at "prime")		(insert ?\')	t)
	       ((looking-at "accent grave")	(insert ?\`)	t)
	       ((looking-at "accent acute")	(insert ?\264)	t)
	       ((looking-at "sub-diaeresis")	(insert ?\250)	t)
	       ((looking-at "macron")		(insert ?\257)	t)
	       ((looking-at "a-e")	 	(insert ?\346)	t)
	       ((looking-at "curly-N")		(insert ?\361)	t)
	       ((looking-at "sub-macron")	(insert ?\367)	t)
	       ((looking-at "slash-o")		(insert ?\370)	t)
	       ((looking-at "cidilla")		(insert ?\371)	t)
	       ((looking-at "sup-circle")	(insert ?\372)	t)
	       ((looking-at "macron-tilda")	(insert ?\373)	t)
	       ((looking-at "hachek")		(insert ?\374)	t)
	       ((looking-at "sub-breve")	(insert ?\375)	t)
	       ((looking-at "breve")		(insert ?\376)	t)
	       ((looking-at "sub-dot")		(insert ?\377)	t)
	       ((looking-at "double-bar-\\$")	(insert ?$)	t)
	       ;; talk about your special-purpose characters...
	       ((looking-at "10\\*10\\*100")
		(delete-region s e)
		(insert "10^10^100")
		nil)
	       ((looking-at "plus squareroot -1")
		(delete-region s e)
		(insert "sqrt(-1)")
		nil)
	       ;; We don't handle these yet:
	       ;; aleph ayin beth breve c-bar check daleth double-arrows
	       ;; double-half-arrows double-hyphen edh fermata-up fermata-down
	       ;; fist flat-sign g-sub-macron gimel hachek he heth kaph lamed
	       ;; mem natural-sign nun parallel pe presa prime qoph radical
	       ;; radical-sign resh sadhe samekh shin sin slur-down spade
	       ;; stacked-commas tau teth thorn triple-bond waw yod yogh
	       ;; zayin "* * *" sadhe(final) "3 character overbar"
	       (t nil))
	      (progn
		(delete-region s (+ s 3))
		(delete-region (+ s 1) (- e 2))))))
      
      ;; nuke silly recursive backspace codes
      (goto-char (point-min))
      (while (search-forward "|\bB" nil t)
	(goto-char (point-min))
	(save-excursion
	  (while (search-forward "|\bB" nil t)
	    (delete-char -3)
	    (insert "\b"))))
      ;; convert @ to ~
      (goto-char (point-min))
      (while (search-forward "@" nil t)
	(delete-char -1) (insert "~")
	(if webster-fontify
	    (webster-fontify (- (point) 1) (point) 'webster-bold-italic)))
      ;; now convert lots of other magic codes...
      (goto-char (point-min))
      (while (search-forward "\b" nil t)
	(delete-char -1)
	(forward-char -1)
	(cond

	 ((looking-at "([MXYAIJ]")
	  ;; start smallcaps/italic/bold/super/sub/subitalic
	  (looking-at "([MXYAIJ]\\([^\)]*\\))")
	  (let ((start (match-beginning 1))
		(end (match-end 1)))
	    (and (not nointern) (looking-at "(M")
		 (webster-intern (buffer-substring start end)))
	    (if webster-fontify
		(let ((c (char-after (1- start))))
		  (webster-fontify start end
				   (cond ((= ?M c) 'webster-small)
					 ((= ?X c) 'webster-italic)
					 ((= ?Y c) 'webster-bold)
					 ((= ?A c) 'webster-superscript)
					 ((= ?I c) 'webster-subscript)
					 ((= ?J c) 'webster-subscript)
					 )
				   (= ?M c))))))

	 ;; #### dubious
	 ((looking-at "([BGR]")	; start greek/APL/symbol
	  (and webster-fontify
	       (looking-at "(\\(.\\)[^\)]*)\^H\\1")
	       (let ((c (char-after (1- (match-beginning 1)))))
		 (webster-fontify
		  (match-beginning 0) (match-end 0) 'webster-small))))

	 ((looking-at ")[ABGIJMRXY]")	; end font-shift
	  nil)

	 ((looking-at "<(\\|(<")
	  (insert (if webster-iso8859/1 ?\253 "<<"))
	  (if webster-fontify
	      (let ((p (point))
		    (e (and (save-excursion (search-forward ")\b>" nil t))
			    (match-beginning 0))))
		(if e
		    (webster-fontify p e 'webster-italic)))))

	 ((looking-at ")>\\|>)")
	  (insert  (if webster-iso8859/1 ?\273 ">>")))

	 ;; #### dubious
	 ((looking-at "[a-z\346][-._]")	; lineover,dotover/under,over/underbar
	  (insert (following-char))
	  (if webster-fontify
	      (webster-fontify (- (point) 1) (point) 'webster-underline)))

	 ((looking-at "[a-zA-Z]:")	; umlaut
	  (let (c)
	    (if (and webster-iso8859/1
		     (setq c (cdr (assq (following-char) webster-umlauts))))
		(insert c)
	      (insert (following-char))
	      (insert (if webster-iso8859/1 ?\250 ?:)))))

	 ((looking-at "[\"~][a-zA-Z]")	; umlaut
	  (let (c)
	    (delete-char 1)
	    (if (and webster-iso8859/1
		     (setq c (cdr (assq (following-char) webster-umlauts))))
		(insert c)
	      (insert (following-char))
	      (insert (if webster-iso8859/1 ?\250 ?:)))
	    (insert " ")
	    (forward-char -1)))

	 ((looking-at "[a-zA-Z]\)")	; grave
	  (let (c)
	    (if (and webster-iso8859/1
		     (setq c (cdr (assq (following-char) webster-graves))))
		(insert c)
	      (insert (following-char))
	      (insert "`"))))

	 ((looking-at ">[a-zA-Z]")	; grave
	  (let (c)
	    (delete-char 1)
	    (if (and webster-iso8859/1
		     (setq c (cdr (assq (following-char) webster-graves))))
		(insert c)
	      (insert (following-char))
	      (insert "`"))
	    (insert " ")
	    (forward-char -1)))

	 ((looking-at "[a-zES]\(")	; acute
	  (let (c)
	    (if (and webster-iso8859/1
		     (setq c (cdr (assq (following-char) webster-acutes))))
		(insert c)
	      (insert (following-char))
	      (insert (if webster-iso8859/1 ?\264 ?\')))))

	 ((looking-at "<[a-zA-Z]")	; acute
	  (let (c)
	    (delete-char 1)
	    (if (and webster-iso8859/1
		     (setq c (cdr (assq (following-char) webster-acutes))))
		(insert c)
	      (insert (following-char))
	      (insert (if webster-iso8859/1 ?\264 ?\')))
	    (insert " ")
	    (forward-char -1)))

	 ((looking-at ";[Cc]")		; ccedilla
	  (delete-char 1)
	  (if webster-iso8859/1
	      (progn
		(insert (if (= (following-char) ?C) ?\307 ?\347))
		(insert ? ) (forward-char -1))
	    (forward-char 1)
	    (insert ?\,)))

	 ((looking-at "|S")		; section
	  (insert (if webster-iso8859/1 ?\247 "SS")))

	 ((looking-at "|q")		; paragraph
	  (insert (if webster-iso8859/1 ?\266 "PP")))

	 ((looking-at "*o")		; centerdot
	  (insert (if webster-iso8859/1 ?\267 ?\*)))

	 ((looking-at "+=")		; plusminus
	  (insert (if webster-iso8859/1 ?\261 "+/-")))

	 ((looking-at "-:")		; division
	  (insert (if webster-iso8859/1 ?\367 "+/-")))

	 ((looking-at "-[xX]")		; multiplication
	  (insert (if webster-iso8859/1 ?\327 "+/-")))

	 ((looking-at "-m") (insert "--"))
	 ((looking-at "-n") (insert "-"))
	 ((looking-at "-/") (insert "\\"))
	 ((looking-at ")|") (insert ?\[))
	 ((looking-at "|)") (insert ?\]))
	 ((looking-at "-3") (insert "..."))
	 ((looking-at "=\\\\") (insert "$"))

	 ((looking-at "'o")		; degree
	  (insert (if webster-iso8859/1 ?\260 ?\*)))

	 ((or (looking-at "nj")		; nj symbol
	      (looking-at "|-")		; dagger
	      (looking-at "|=")		; doubledagger
	      (looking-at "|o")		; lowerphi
	      (looking-at "'b")		; stroke
	      )
	  (if webster-fontify
	      (webster-fontify (point) (+ (point) 2) 'webster-bold))
	  (insert "  ")
	  (forward-char -2))

	 ((looking-at "[cC]\371")	; (\bQcidilla)\bQ
	  (if webster-iso8859/1
	      (insert (if (= (following-char) ?C) ?\307 ?\347))
	    (forward-char 1)
	    (insert ?\,)))

;	 ((or (looking-at "[a-zA-Z]\250")	; (\bQsub-diaeresis)\bQ
;	      (looking-at "[a-zA-Z]\346")	; (\bQa-e)\bQ
;	      (looking-at "[a-zA-Z]\361")	; (\bQcurly-N)\bQ
;	      (looking-at "[a-zA-Z]\367")	; (\bQsub-macron)\bQ
;	      (looking-at "[a-zA-Z]\370")	; (\bQslash-o)\bQ
;	      (looking-at "[a-zA-Z]\371")	; (\bQcidilla)\bQ
;	      (looking-at "[a-zA-Z]\372")	; (\bQsup-circle)\bQ
;	      (looking-at "[a-zA-Z]\373")	; (\bQmacron-tilda)\bQ
;	      (looking-at "[a-zA-Z]\374")	; (\bQhachek)\bQ
;	      (looking-at "[a-zA-Z]\375")	; (\bQsub-breve)\bQ
;	      (looking-at "[a-zA-Z]\376")	; (\bQbreve)\bQ
;	      (looking-at "[a-zA-Z]\377")	; (\bQsub-dot)\bQ
;	      )
;	  (forward-char 1) (insert " ") (forward-char -1)
;	  (webster-fontify (1- (point)) (point) 'webster-underline))

	 ((looking-at "/[a-zA-Z]")		; greek
	  (forward-char 1)
	  (insert " <")
	  (forward-char 1)
	  (insert ?\>)
	  (forward-char -5))

	 ;; overstrike
	 ((looking-at (format "[%c][%c]" (following-char) (following-char)))
	  (insert (following-char))
	  (if webster-fontify
	      (webster-fontify (- (point) 1) (point) 'webster-bold)))

	 (t				; ## debug
	  (insert (following-char))
	  (insert "\b")
	  (insert (buffer-substring (+ 1 (point)) (+ 2 (point))))
	  ))
	(delete-char 2))

      (goto-char (point-min))
      (setq start (point)
	    end (point-max))
      (widen)
      (beginning-of-line)
      (narrow-to-region (point) end)
      (goto-char start)
      ;; (fill-region-as-paragraph (point-min) (point-max))
      (while (not (eobp))
	(setq start (point))
	(skip-chars-forward "^ \n\t")
	(if (>= (current-column) fill-column)
	    (progn
	      (goto-char start)
	      (delete-horizontal-space)
	      (insert "\n" (or fill-prefix "")))
	  (skip-chars-forward " \n\t")))
      ))))


(defun webster-pos (start end)
  (save-excursion
    (goto-char start)
    (cond ((and (= start (1- end)) (looking-at "n")) "noun")
	  ((or (not webster-fontify) (/= start (- end 2)))
	   (buffer-substring start end))
	  ((looking-at "ac") "adjective combinational form")
	  ((looking-at "aj") "adjective")
	  ((looking-at "as") "adjective suffix")
	  ((looking-at "av") "adverb")
	  ((looking-at "ca") "adjective combinational form")
	  ((looking-at "cf") "combinational form")
	  ((looking-at "cj") "conjunction")
	  ((looking-at "da") "definite article")
	  ((looking-at "ia") "indefinite article")
	  ((looking-at "ij") "interjection")
	  ((looking-at "is") "interjection suffix")
	  ((looking-at "js") "adjective suffix")
	  ((looking-at "nc") "noun combinational form")
	  ((looking-at "np") "noun plural suffix")
	  ((looking-at "ns") "noun suffix")
	  ((looking-at "pf") "prefix")
	  ((looking-at "pn") "pronoun")
	  ((looking-at "pp") "preposition")
	  ((looking-at "sf") "verb suffix")
	  ((looking-at "tm") "trademark")
	  ((looking-at "va") "verbal auxilliary")
	  ((looking-at "vb") "verb")
	  ((looking-at "vc") "verb combinational form")
	  ((looking-at "vi") "verb intransitive")
	  ((looking-at "vm") "verb impersonal")
	  ((looking-at "vp") "verb imperfect")
	  ((looking-at "vs") "verb suffix")
	  ((looking-at "vt") "verb transitive")
	  (t (buffer-substring start end)))))


(defun webster-convert ()
  (goto-char (point-min))
  ;; nuke the continuation lines
  (save-excursion
    (while (re-search-forward "^C:" nil t)
      (forward-char -2)
      (while (looking-at "^C:")
	(forward-line 1))
      (forward-line -1)
      (while (looking-at "^C:")
	(forward-char -1)
	(let ((n (- (point) (save-excursion (beginning-of-line) (point)))))
	  (delete-char 3)
	  ;; What a stupid format!  (example: "fat")
	  (if (= n 79) (insert " "))
	  (beginning-of-line)))))
  (goto-char (point-min))
  (let ((last-type nil)
	(this-type nil)
	(last-part nil))
    (while (not (eobp))
      (setq this-type (following-char))
      (cond
       ((looking-at "^WORD ")
	(let ((p (point)))
	  (end-of-line)
	  (delete-region p (point))))

       ((looking-at "^21[12] ")	; reply to a LIST command; one line.
	(delete-char 4))
       ((looking-at "^220 ")	; reply to a LIST command; intern the results.
	(let ((p (point)))
	  (if (eq (preceding-char) ?\n) (setq p (1- p)))
	  (end-of-line)
	  (delete-region p (point)))
	(insert "\n")
	(while (not (or (eobp) (looking-at "\n\n")))
	  (forward-line 1)
	  (insert "    ")
	  (let (s e)
	    (while (looking-at "[^\n;]+;")
	      (webster-intern (buffer-substring (setq s (match-beginning 0))
						(setq e (1- (match-end 0)))))
	      (goto-char (match-end 0))
	      (insert " ")
	      (if webster-fontify
		  (webster-fontify s e 'webster-bold t)))
	    (if (looking-at "\n")
		nil
	      (webster-intern
	       (buffer-substring (setq s (point))
				 (progn (end-of-line) (setq e (point)))))
	      (if webster-fontify
		  (webster-fontify s e 'webster-bold t)))
	    )))

       ((looking-at "^\n")
	(delete-char 1))

       ((looking-at "^\\(200\\|221\\|PREFIX\\|LIST\\|EXACT\\)[- ]")
	;; just toss these.
	(let ((p (point)))
	  (if (eq (preceding-char) ?\n) (setq p (1- p)))
	  (end-of-line)
	  (delete-region p (point))))

       ((looking-at "^F:")
	;; First record:  F:entname;homono;prefsuf;dots;accents;pos;posjoin;pos2
	(delete-char 2)
	(search-forward ";")
	(let ((p (1- (point)))
	      homonym prefix dots pos posj pos2)
	  (if (looking-at "[0-9]+")
	      (setq homonym (buffer-substring (point) (match-end 0))))
	  (search-forward ";")
	  (if (looking-at "[^;]+")
	      (setq prefix (buffer-substring (point) (match-end 0))))
	  (search-forward ";")
	  (if (looking-at "[0-9]+")
	      (setq dots (append (buffer-substring (point) (match-end 0))
				 nil)))
	  (search-forward ";")
	  ;; ignore accents
	  (search-forward ";")
	  (if (looking-at "[a-z]+")
	      (setq pos (webster-pos (point) (match-end 0))))
	  (search-forward ";")
	  (if (looking-at "[a-z]+")
	      (setq posj (webster-pos (point) (match-end 0))))
	  (if (looking-at "[a-z]+")
	      (setq pos2 (webster-pos (point) (match-end 0))))
	  (end-of-line)
	  (delete-region p (point))
	  (beginning-of-line)
	  (insert " ")
	  (let ((e (save-excursion (end-of-line) (point))))
	    (webster-intern (buffer-substring (point) e))
	    (if webster-fontify
		(webster-fontify (point) e 'webster-bold t)))
	  (beginning-of-line)
	  (if (not homonym)
	      (insert " ")
	    (let ((p (point)))
	      (insert homonym)
	      (if webster-fontify
		  (webster-fontify p (point) 'webster-bold-italic))))
	  (forward-char 1)
	  (while dots
	    (forward-char (- (car dots) ?0))
	    (insert ".")
	    (setq dots (cdr dots)))
	  (end-of-line)
	  (let ((p (point)))
	    (if pos (insert " " pos))
	    (if posj (insert " " posj))
	    (if pos2 (insert " " pos2))
	    (if (and webster-fontify (or pos posj pos2))
		(webster-fontify p (point) 'webster-italic)))
	  (insert "  ")
	  ;; prefix/suffix is "p" or "s"; I don't know what it's for.
	  (setq last-part pos)))

       ((looking-at "^P:")
	;; Pronunciation: P:text
	(delete-char 2) (delete-char -1)
	(insert " \\")
	(let ((p (point))
	      (fill-prefix "     "))
	  (end-of-line)
	  (insert " ")
	  (if webster-fontify
	      (progn
		(webster-fontify (1- p) (1- (point)) 'webster-italic)
		(forward-char -1)))
	  (webster-textify-region p (point))
	  (insert "\\")))

       ((looking-at "E:")
	;; Etymology:  E:text
	(delete-char 2) (insert "   [")
	(let ((fill-prefix "    "))
	  (webster-textify-region (point) (progn (end-of-line) (point))))
	(insert "]"))

       ((looking-at "S:")
	;; Synonym:  S:text
	(delete-char 2) (insert "  ")
	(let ((fill-prefix "      "))
	  (webster-textify-region (point) (progn (end-of-line) (point)))))

       ((looking-at "X:")
	;; Cross Reference:  X:word;wrdsuper;wrdsubs;type;word2
	(setq last-part nil)
	(let (p word super sub type word2)
	  (delete-char 2)
	  (setq p (point))
	  (if (looking-at "[^;]+")
	      (setq word (upcase (buffer-substring (point) (match-end 0)))))
	  (search-forward ";")
	  (if (looking-at "[^;]+")
	      (setq super (buffer-substring (point) (match-end 0))))
	  (search-forward ";")
	  (if (looking-at "[^;]+")
	      (setq sub (buffer-substring (point) (match-end 0))))
	  (search-forward ";")
	  (if (looking-at "[0-9]+")
	      (setq type (string-to-int
			  (buffer-substring (point) (match-end 0)))))
	  (search-forward ";")
	  (if (looking-at  "[^;]+")
	      (setq word2 (upcase (buffer-substring (point) (match-end 0)))))
	  (delete-region p (point))
	  (insert "  ")
	  (cond ((eq type 0) (insert "see (\bM" word ")\bM"))
		((eq type 1) (insert "see (\bM" word ")\bM table"))
		((eq type 2) (insert "### ILLEGAL XREF CODE 2"))
		((eq type 3) (insert "see (\bM" word2 ")\bM at (\bM" word
				     ")\bM table"))
		((eq type 4) (insert "compare (\bM" word ")\bM"))
		((eq type 5) (insert "compare (\bM" word ")\bM table"))
		((eq type 6) (insert "called also (\bM" word ")\bM"))
		((eq type 7) (insert "### ILLEGAL XREF CODE 7"))
		((eq type 8) (insert "(\bYsyn)\bY see in addition (\bM" word
				     ")\bM"))
		((eq type 9) (insert "(\bYsyn)\bY see (\bM" word ")\bM"))
		(t (insert "#### ILLEGAL XREF CODE " (or type "nil"))))
	  (let ((fill-prefix "     "))
	    (webster-textify-region p (point)))))

       ((looking-at "D:")
	;; Definition:  D:snsnumber;snsletter;snssubno;pos;text
	(let (p n sub1 sub2 part)
	  (setq p (point))
	  (forward-char 2)
	  (if (looking-at "[0-9]+")
	      (setq n (buffer-substring (point) (match-end 0))))
	  (search-forward ";")
	  (if (looking-at "[a-z]+")
	      (setq sub1 (buffer-substring (point) (match-end 0))))
	  (search-forward ";")
	  (if (looking-at "[0-9]+")
	      (setq sub2 (buffer-substring (point) (match-end 0))))
	  (search-forward ";")
	  (if (looking-at "[a-z]+")
	      (setq part (webster-pos (point) (match-end 0))))
	  (search-forward ";")
	  (delete-region p (point))
	  (if (and sub2 (not (equal sub2 "1")))
	      (setq sub1 " "))
	  (if (and sub1 (not (equal sub1 "a")))
	      (setq n " "))
	  ;; If a Definition appears after a Label, don't print numbers
	  ;; as the label has done that already.
	  (if (eq last-type ?L)
	      (setq n (and n " ") sub1 (and sub1 " ") sub2 (and sub2 " ")))
	  (if (and part (not (equal part last-part)))
	      (let ((p (point)))
		(insert "   " part "\n")
		(if webster-fontify
		    (webster-fontify p (1- (point)) 'webster-italic))
		(setq last-part part)))
	  (indent-to (- 6 (length n)))
	  (setq p (point))
	  (if (and n (not (equal n "0")))
	      (insert n " "))
	  (if sub1 (insert " " sub1 " "))
	  (if sub2 (insert " (" sub2 ") "))
	  (insert ": ")
	  (if webster-fontify
	      (webster-fontify p (point) 'webster-bold-italic))
	  (setq p (point))
	  (end-of-line)
	  (let ((fill-prefix (make-string (if sub2 17 (if sub1 12 9)) ? )))
	    (webster-textify-region p (point)))))

       ((looking-at "R:")
	;; Run-on:  R:name;dots;accents;pos1;posjoin;pos2
	(delete-char 2)
	(insert "  ")
	(search-forward ";") (delete-char -1)
	(let ((beg (save-excursion (beginning-of-line) (+ (point) 2))))
	  (webster-intern (buffer-substring beg (point)))
	  (if webster-fontify
	      (webster-fontify beg (point) 'webster-bold t)))
	(if (looking-at "[0-9]+")
	    (let* ((dots (append (buffer-substring (point) (match-end 0))
				 nil)))
	      (delete-region (point) (match-end 0))
	      (beginning-of-line)
	      (forward-char 2)
	      (while dots
		(forward-char (- (car dots) ?0))
		(insert ".")
		(setq dots (cdr dots)))))
	(search-forward ";") (delete-char -1)
	;; throw away the accents
	(let ((p (point)))
	  (search-forward ";")
	  (delete-region p (point)))
	(insert " ")
	(if (looking-at "[a-z][a-z]?;")
	    (let* ((start (point))
		   (end (1- (match-end 0)))
		   (pos (webster-pos start end)))
	      (delete-region start end)
	      (insert pos)
	      (if webster-fontify
		  (webster-fontify start (point) 'webster-italic))))
	(search-forward ";") (delete-char -1) (insert " ")
	(search-forward ";") (delete-char -1) (insert " "))

       ((looking-at "L:")
	;; Label:  L:snsnumber;snsletter;snssubno;text
	(let (p n sub1 sub2)
	  (setq p (point))
	  (forward-char 2)
	  (if (looking-at "[0-9]+")
	      (setq n (buffer-substring (point) (match-end 0))))
	  (search-forward ";")
	  (if (looking-at "[a-z]+")
	      (setq sub1 (buffer-substring (point) (match-end 0))))
	  (search-forward ";")
	  (if (looking-at "[0-9]+")
	      (setq sub2 (buffer-substring (point) (match-end 0))))
	  (search-forward ";")
	  (delete-region p (point))
	  (if (and sub2 (not (equal sub2 "1")))
	      (setq sub1 " "))
	  (if (and sub1 (not (equal sub1 "a")))
	      (setq n " "))
	  (indent-to (- 6 (length n)))
	  (setq p (point))
	  (if (not (equal n "0"))
	      (insert (or n " ") " "))
	  (if sub1 (insert " " sub1))
	  (if sub2 (insert " (" sub2 ")"))
	  (insert " ")
	  (if webster-fontify
	      (webster-fontify p (point) 'webster-bold-italic))
	  (setq p (point))
	  (end-of-line)
	  (let ((fill-prefix (make-string (if sub2 17 (if sub1 12 9)) ? )))
	    (webster-textify-region p (point)))))

       ((looking-at "V:")
	;; Variant:  V:name;dots;accents;level1()level2
	(delete-char 2)
	(let ((p (point))
	      beg)
	  (search-forward ";") (delete-char -1)
	  (webster-intern (buffer-substring
			   (save-excursion (beginning-of-line)
					   (setq beg (point)))
			   (point)))
	  (if webster-fontify
	      (webster-fontify beg (point) 'webster-bold t))
	  (if (looking-at "[0-9]+")
	      (let* ((dots (append (buffer-substring (point) (match-end 0))
				   nil)))
		(delete-region (point) (match-end 0))
		(beginning-of-line)
		(while dots
		  (forward-char (- (car dots) ?0))
		  (insert ".")
		  (setq dots (cdr dots)))))
	  (search-forward ";") ; skip accents
	  (delete-region (1- (point))
			 (save-excursion (end-of-line) (point)))
	  (let ((fill-prefix "    "))
	    (webster-textify-region p (point) t)))
	(save-excursion
	  (beginning-of-line)
	  (cond ((eq last-type ?F) (delete-char -1))
		((eq last-type ?V) (delete-char -1) (insert "; "))
		(t (insert "  ")))))

       ((looking-at ".\n")
	(delete-char 1))
       ((looking-at "22[0-9] ")
	(delete-region (point) (save-excursion (end-of-line) (point))))
       ((looking-at "\n")
	nil)
       (t
	(insert "* ")))
      (setq last-type this-type)
      (forward-line 1)
      (while (save-excursion
	       (and (not (bobp))
		    (progn (forward-line -1) (looking-at "\n"))))
	(delete-char -1))
      ))
  (goto-char (point-min))
  (cond ((search-forward "\^H" nil t)
	 (goto-char (point-min))
	 (insert
	  "\n****\tThis definition contains unrecognized font-change codes."
	  "\n****\tPlease tell jwz.\n\n")
	 (goto-char (point-min))))

  ;; lay down the default font; don't put it over the spaces and tabs on
  ;; the beginning of the line so that those space as if it was a fixed
  ;; width font.  There must be a better way than 
  (if webster-fontify
      (save-excursion
	(let (e)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward " \t")
	  ;; avoid extent overlaps; should be able to use extent priorities
	  ;; to obviate this, but it's late.
	  (while (setq e (extent-at (point)))
	    (goto-char (1+ (extent-end-position e))))
	  (setq e (make-extent (point) (progn (forward-line 1) (point))))
	  (set-extent-face e 'webster)))))
  )


;; Codes:
;;
;;	(A		start superscript	catalan
;;	(B		start unknown		mixed number
;;	(G		start greek		alpha
;;	(I		start subscript		alcohol
;;	(J		start subitalic		mixed number
;;	(M		start small		mitten
;;	(Q		start special		mitzvah
;;	(R		start APL		mixed
;;	(X		start italic		everywhere...
;;	(Y		start bold		everywhere...
;;	)A		end superscript		catalan
;;	)B		end unknown		mixed number
;;	)G		end greek		alpha
;;	)I		end subscript		alcohol
;;	)J		end subitalic		mixed number
;;	)M		end small		mitten
;;	)Q		end special		mitzvah
;;	)R		end APL			mixed
;;	)X		end italic		everywhere...
;;	)Y		end bold		everywhere...
;;	"a		a-umlaut		acetoacetic acid
;;	"e		e-umlaut		agio
;;	"i		i-umlaut		alcaic
;;	"o		o-umlaut		ale
;;	"u		u-umlaut		alpenglow
;;	a:		a-umlaut		aardvark
;;	n:		n-umlaut		pogy
;;	o:		o-umlaut		coccyx
;;	s:		s-umlaut		centrifugation
;;	u:		u-umlaut		accouter
;;	w:		w-umlaut		bourgeois
;;	I:		I-umlaut		natural
;;	~a		a-umlaut		alcove
;;	~e		e-umlaut		Boxer
;;	~i		i-umlaut		Cistercian
;;	~o		o-umlaut		alcove
;;	~u		u-umlaut		Boxer
;;	~E		E-umlaut		arris
;;	~O		O-umlaut		prix fixe
;;	>e		e-grave			arriere-pensee
;;	>a		a-grave			pompano
;;	>u		u-grave			coca
;;	>E		E-grave
;;	u)		u-grave
;;	o)		o-grave
;;	i)		i-grave
;;	s)		s-grave
;;	;C		C-cedilla		compendia
;;	;c		c-cedilla		babassu
;;	<E		E-acute
;;	<a		a-acute
;;	<e		e-acute
;;	S(		S-acute
;;	c(		c-acute
;;	i(		i-acute
;;	o(		o-acute
;;	r(		r-acute
;;	s(		s-acute
;;	y(		y-acute
;;	)>		guillemotright		everywhere...
;;	<(		guillemotleft		everywhere...
;;	(<		guillemotleft (?)	come
;;	-m		longdash		pi
;;	n_		nj			babbling
;;	'o		degree			
;;	|)		]
;;	|-		dagger
;;	|=		doubledagger
;;	|S		section
;;	|o		lower-phi
;;	|q		paragraph		paragraph
;;	=\		"$"
;;	(<		"<"
;;	(|		"["
;;	'b		stroke
;;	*o		centerdot
;;	+=		plusminus
;;	-/		\
;;	-3		"..."
;;	-:		division
;;	-X		multiplication
;;	-n		"-"
;;	-x		multiplication
;;	''		' overstrike
;;	::		: overstrike
;;	;;		; overstrike
;;	MM		M overstrike
;;	a-		a-lineover
;;	e-		e-lineover
;;	i-		i-lineover
;;	o-		o-lineover
;;	u-		u-lineover
;;	y-		y-lineover
;;	A-		A-lineover
;;	E-		E-lineover
;;	I-		I-lineover
;;	O-		O-lineover
;;	U-		U-lineover
;;	Q-		Q-lineover2
;;	a.		a-dotover
;;	e.		e-dotover
;;	m.		m-dotover
;;	n.		n-dotover
;;	o.		o-dotover
;;	r.		r-dotover
;;	u.		u-dotover
;;	e_		e-lineunder
;;	h_		h-lineunder
;;	k_		k-lineunder
;;	r-		r-lineunder
;;	r_		r-lineunder
;;	t_		t-lineunder
;;	u_		u-lineunder
;;	k-		k-dotunder

;; t(\bQsub-dot)\bQ		t-dotunder
;; s(\bQsub-dot)\bQ		s-dotunder
;; h(\bQsub-dot)\bQ		h-dotunder		aceldama
;; n(\bQsub-dot)\bQ		n-dotunder
;; r(\bQsub-dot)\bQ		r-dotunder
;; d(\bQsub-dot)\bQ		d-dotunder
;; z(\bQsub-dot)\bQ		z-dotunder
;; l(\bQsub-dot)\bQ		l-dotunder
;; S(\bQsub-dot)\bQ		S-dotunder
;; H(\bQsub-dot)\bQ		H-dotunder
;; o(\bQsub-dot)\bQ		o-dotunder
;; a(\bQsub-dot)\bQ		a-dotunder
;; e(\bQbreve)\bQ		e-breve
;; u(\bQbreve)\bQ		u-breve
;; i(\bQbreve)\bQ		i-breve
;; a(\bQbreve)\bQ		a-breve
;; A(\bQbreve)\bQ		A-breve
;; s(\bQbreve)\bQ		s-breve
;; n(\bQbreve)\bQ		n-breve
;; E(\bQbreve)\bQ		E-breve
;; y(\bQbreve)\bQ		y-breve
;; o(\bQbreve)\bQ		o-breve
;; h(\bQsub-breve)\bQ		h-breve
;; e(\bQhachek)\bQ		e-hachek
;; s(\bQhachek)\bQ		s-hachek
;; z(\bQhachek)\bQ		z-hachek
;; c(\bQhachek)\bQ		c-hachek
;; j(\bQhachek)\bQ		j-hachek
;; i(\bQhachek)\bQ		i-hachek
;; u(\bQhachek)\bQ		u-hachek
;; g(\bQhachek)\bQ		g-hachek
;; r(\bQhachek)\bQ		r-hachek
;; a(\bQhachek)\bQ		a-hachek
;; C(\bQhachek)\bQ		C-hachek
;; a(\bQmacron-tilda)\bQ	a-macrontilda
;; i(\bQmacron-tilda)\bQ	i-macrontilda
;; e(\bQmacron-tilda)\bQ	e-macrontilda
;; a(\bQsup-circle)\bQ		a-circleover
;; A(\bQsup-circle)\bQ		A-circleover
;; e(\bQcidilla)\bQ		e-cedilla
;; o(\bQcidilla)\bQ		o-cedilla
;; a(\bQcidilla)\bQ		a-cedilla
;; z(\bQsub-diaeresis)\bQ	z-umlautunder
;; r(\bQsub-diaeresis)\bQ	r-umlautunder
;; t(\bQsub-macron)\bQ		t-lineunder
;; B(\bQ3 character overbar)\bQ	B-lineover3

;; (\bQa-e)\bQ-		ae-overbar (?)		herring

;; "U			unknown
;; '-			unknown
;; 'a 			unknown
;; (j			unknown
;; )o			unknown
;; - 			unknown
;; -0			unknown
;; ->			unknown
;; -M			unknown
;; -N			unknown
;; -O			unknown
;; -s			unknown
;; ;(			unknown
;; <'			unknown
;; <A			unknown
;; =S			unknown
;; >'			unknown
;; B 			unknown
;; G<			unknown
;; G>			unknown
;; I'			unknown
;; O'			unknown
;; S			unknown
;; c|			unknown
;; e@			unknown
;; eg			unknown
;; en			unknown
;; er			unknown
;; et			unknown
;; i"			unknown
;; l-			unknown
;; m-			unknown
;; n,			unknown
;; nB			unknown
;; o@			unknown
;; os			unknown
;; ot			unknown
;; s,			unknown			chouse
;; u@			unknown
;; | 			unknown

;; /a			unknown			alpha
;; /b			unknown
;; /c			unknown
;; /d			unknown
;; /e			unknown
;; /g			unknown
;; /h			unknown
;; /i			unknown
;; /k			unknown
;; /l			unknown
;; /m			unknown
;; /n			unknown
;; /p			unknown
;; /r			unknown
;; /s			unknown
;; /t			unknown
;; /u			unknown
;; /v			unknown
;; /w			unknown
;; /x			unknown
;; /z			unknown

;; /C			unknown
;; /D			unknown
;; /F			unknown
;; /G			unknown
;; /I			unknown
;; /L			unknown
;; /N			unknown
;; /O			unknown
;; /P			unknown
;; /S			unknown
;; /U			unknown
;; /V			unknown
;; /W			unknown
;; /X			unknown
