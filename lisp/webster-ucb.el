;;; -*- Mode:Emacs-Lisp -*-

;;; Yet Another Webster Protocol.
;;; This one is for talking to the kind of Webster server of which 
;;; pasteur.Berkeley.EDU port 1964 is an instance.
;;;
;;; The interface and much of the process-handling code in this file were
;;; lifted from the Webster client by Jason Glasgow that talks to the kind
;;; of Webster server of which mintaka.lcs.mit.edu port 103 is an instance.
;;;
;;; 13 nov 90  Jamie Zawinski <jwz@lucid.com>  created
;;; 14 sep 91  Jamie Zawinski <jwz@lucid.com>  hacked on some more
;;; 19 feb 91  Jamie Zawinski <jwz@lucid.com>  added Lucid Emacs font support
;;; 15 apr 92  Jamie Zawinski <jwz@lucid.com>  added mouse support

(defvar webster-host "pasteur" "*The host with the webster server")
(defvar webster-port "1964" "*The port on which the webster server listens")

(defvar webster-running nil "Used to determine when connection is established")
(defvar webster-state "closed" "for the modeline")
(defvar webster-process nil "The current webster process")
(defvar webster-process-name "webster" "The current webster process")
(defvar webster-buffer nil "The current webster process")

(defvar webster-start-mark nil)

(defvar webster-fontify (string-match "Lucid" emacs-version)
  "*Set to t to use the Lucid GNU Emacs font-change mechanism.")

(cond ((fboundp 'make-face)
       (or (find-face 'webster)
	   (face-differs-from-default-p (make-face 'webster))
	   (copy-face 'default 'webster))
       (or (find-face 'webster-bold)
	   (face-differs-from-default-p (make-face 'webster-bold))
	   (copy-face 'bold 'webster-bold))
       (or (find-face 'webster-italic)
	   (face-differs-from-default-p (make-face 'webster-italic))
	   (copy-face 'italic 'webster-italic))
       (or (find-face 'webster-bold-italic)
	   (face-differs-from-default-p (make-face 'webster-bold-italic))
	   (copy-face 'bold-italic 'webster-bold-italic))
       (or (find-face 'webster-small)
	   (face-differs-from-default-p (make-face 'webster-small))
	   (copy-face 'webster-bold 'webster-small))
       ))

(defun webster-fontify (start end face &optional highlight)
  (let ((e (make-extent start end (current-buffer))))
    (set-extent-face e face)
    (if highlight (set-extent-attribute e 'highlight))))

;;;
;;; Initial filter for ignoring information until successfully connected
;;;
(defun webster-initial-filter (proc string)
  (let ((this-buffer (current-buffer)))
    ;; don't use save-excursion so tht point moves in webster-buffer
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
	      p)
	  (widen)
	  (goto-char (point-min))
	  (narrow-to-region webster-start-mark (point-max))
	  (webster-convert)
	  (widen)
	  (setq p (marker-position webster-start-mark))
	  (goto-char (point-max))
	  (or (bobp) (insert "\^L\n--------------------\n"))
	  (set-marker webster-start-mark (point-max))
	  (goto-char p)
	  (if webster-window
	      (progn
		(select-window webster-window)
		(goto-char p)
		(recenter 1)
		(select-window window)))))))


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
	  (set-process-filter webster-process 'webster-initial-filter)
	  (process-send-string  webster-process webster-command)
	  (setq webster-running nil)
	  (while (not webster-running)	; wait for feedback
	    (accept-process-output webster-process))))
    (display-buffer webster-buffer nil)
    (process-send-string webster-process (concat kind " " word "\n"))))

(defun webster-xref-word (event)
  "Define the highlighted word under the mouse.
Words which are known to have definitions are highlighted when the mouse
moves over them.  You may define any word by selecting it with the left
mouse button and then clicking middle."
  (interactive "e")
  (let* ((buffer (window-buffer (event-window event)))
	 (extent (extent-at (event-point event) buffer 'highlight))
	 text)
    (cond (extent
	   (setq text (save-excursion
			(set-buffer buffer)
			(buffer-substring
			 (extent-start-position extent)
			 (extent-end-position extent)))))
	  ((x-selection-owner-p) ; the selection is in this emacs process.
	   (setq text (x-get-selection)))
	  (t
	   (error "click on a highlighted word to define")))
    (while (string-match "\\." text)
      (setq text (concat (substring text 0 (match-beginning 0))
			 (substring text (match-end 0)))))
    (message "looking up %s..." (upcase text))
    (webster text)))


(defun webster-quit ()
  "Close connection and quit webster-mode.  Buffer is not deleted."
  (interactive)
  (message "Closing connection to %s..." webster-host)
  (kill-process webster-process)
  (message "Closing connection to %s...done" webster-host)
  (setq webster-state "closed")
  (if (eq (current-buffer) webster-buffer)
      (bury-buffer)))

(defvar webster-mode-map nil)
(if webster-mode-map
    nil
  (setq webster-mode-map (make-sparse-keymap))
  (define-key webster-mode-map "?" 'describe-mode)
  (define-key webster-mode-map "d" 'webster)
  (define-key webster-mode-map "e" 'webster-endings)
  (define-key webster-mode-map "q" 'webster-quit)
  (define-key webster-mode-map "s" 'webster-spell)
  (if (string-match "Lucid" emacs-version)
      (define-key webster-mode-map 'button2 'webster-xref-word))
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

(defconst webster-completion-table (make-vector 511 0))

(defun webster-intern (string)
  (intern (downcase string) webster-completion-table))

(defun webster-textify-region (start end &optional nointern)
  (save-excursion
    (goto-char (1- end))
    (if (looking-at "[^\n]\n") (setq end (1+ end)))
    (save-restriction
      (narrow-to-region start end)
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
      ;; some conversions
      (goto-char (point-min))
      (while (search-forward "\b" nil t)
	(delete-char -1)
	(forward-char -1)
	(cond ((looking-at "<(")
	       (insert "<<")
	       (if webster-fontify
		   (let ((p (point))
			 (e (and (save-excursion (search-forward ")\b>" nil t))
				 (match-beginning 0))))
		     (if e
			 (webster-fontify p e 'webster-italic)))))
	      ((looking-at ")>")
	       (insert ">>"))
	      ((looking-at "[a-z][-.]") ; overstrike
	       (insert (following-char))
	       (if webster-fontify
		   (webster-fontify (- (point) 1) (point) 'webster-bold)))
	      ((looking-at "[a-z][:_]")  ; umlaut or overbar
	       (insert "  ")
	       (forward-char -2))
	      ((looking-at "([MXY]") ; start smallcaps, italic, or bold
	       (cond ((and (not nointern)
			   (looking-at "([MXY]\\([^\)]*\\))"))
		      (webster-intern
		       (buffer-substring (match-beginning 1) (match-end 1)))
		      (if webster-fontify
			  (let ((c (char-after (1- (match-beginning 1)))))
			    (webster-fontify
			     (match-beginning 1) (match-end 1)
			     (cond ((= ?M c) 'webster-bold) ;##
				   ((= ?X c) 'webster-italic)
				   ((= ?Y c) 'webster-bold))
			     (or (= ?M c)))))))
	       nil)
	      ((looking-at ")[MXY]") ; end smallcaps, italic, or bold
	       nil)
	      ((looking-at "[()][ABIJ]") ; start or end superscript/subscript
	       nil)
	      ((looking-at "[()][GRQ]") ; greek, APL, or Symbol
	       nil)
	      ((looking-at
		(format "[%c][%c]" (following-char) (following-char)))
	       nil)
	      ((looking-at "-m")
	       (insert "--"))
	      (t ; ## debug
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
      )))


(defun webster-convert ()
  (goto-char (point-min))
  ;; nuke the continuation lines
  (save-excursion
    (while (re-search-forward "^C:" nil t)
      (backward-char 2)
      (let ((p (point)) n)
	(while (looking-at "^C:")
	  (setq n (- (point) p)
		p (point))
	  (delete-char -1)
	  (delete-char 2)
	  (if (= n 77) (insert " ")) ; what a dumbshit format...
	  (forward-line 1)))))
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
	(while (not (or (eobp) (looking-at "\n\n")))
	  (forward-line 1)
	  (while (looking-at "[^\n;]+;")
	    (webster-intern (buffer-substring (match-beginning 0)
					      (1- (match-end 0))))
	    (goto-char (match-end 0))
	    (insert " "))
	  (or (looking-at "\n")
	      (webster-intern
	       (buffer-substring (point) (progn (end-of-line) (point)))))))

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
	      (setq pos (buffer-substring (point) (match-end 0))))
	  (search-forward ";")
	  (if (looking-at "[a-z]+")
	      (setq posj (buffer-substring (point) (match-end 0))))
	  (if (looking-at "[a-z]+")
	      (setq pos2 (buffer-substring (point) (match-end 0))))
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
	      (setq part (buffer-substring (point) (match-end 0))))
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
	      (progn
		(insert "   " part "\n")
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
  (goto-char (point-min)))
