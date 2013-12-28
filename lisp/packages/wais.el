;;; A GNU Emacs interface to WAIS
;;;
;;;  Jonny Goldman <jonathan@think.com>
;;;
;;; liberally ripped off from various sources, and heavily influenced
;;; by wais-interface.el from Brewster and Bonnie.
;;;
;;; Header: /tmp_mnt/net/quake/proj/wais-8-b5-8-b5/ui/RCS/wais.el.raw,v 1.31 92/03/30 15:48:45 jonathan Exp 
;;; include the following in your .emacs file (without semicolons):
;;; (autoload 'wais "wais"
;;;       "Do something useful for WAIS" t)
;;; (autoload 'wais-select-question "wais"
;;;       "Select a question for WAIS" t)
;;; (autoload 'wais-create-question "wais"
;;;       "Create a new question for WAIS" t)

(provide 'wais)
(eval-when-compile
 (require 'cl))

;;; These are important variables.  Set them appropriately.

(defvar *wais-top-directory* "/cadillac-th2/wais/wais-8-b5/")
(defvar waisq-program (concat *wais-top-directory* "bin/waisq")
  "Location of the waisq executable. 
   This comes in the bin directory of the wais release") 
(defvar waisindex-program (concat *wais-top-directory* "bin/waisindex")
  "Location of the waisindex executable. 
   This comes in the bin directory of the wais release")
(defvar *common-source-directory* (concat *wais-top-directory* "wais-sources/")
  "Where the common sources for you site live.  nil if there are none")

(defvar *wais-maximum-result-documents* 40
  "Maximum number of results to return for a question.  If you change this,
be sure to kill the wais-receiving buffer.")

(defvar *wais-question-directory*
    "~/wais-questions/"
  "User's question directory")
(defvar *wais-source-directory*
    "~/wais-sources/"
  "User's source directory")
(defvar *wais-document-directory*
    "~/wais-documents/"
  "User's document directory")

(defvar wais-version 8)

(defvar *debug* nil)

(defvar *waisq-truncate-mode* t
 "switch to set truncate mode in waisq buffers.  Set to nil for line wrap")

(defvar *wais-document-display-size* 4
  "Number of lines of document headers to display when text is shown")

(defvar *wais-multiple-document-buffers* t
  "If set to nil, use one buffer for all retrieved documents.
A retrieval request will be issued each time a document is
selected for editing.
Otherwise, use Multiple buffers for retrieving Documents.
Retrieval requests are made only once, and the buffer is reused.")

(defvar *wais-document-buffer* "Wais DOC"
  "Name of buffer when *wais-multiple-document-buffers* is nil")

(defvar *x-viewers* 
  (list (list "GIF" (concat *wais-top-directory* "bin/wais-gif-display"))
	(list "TIFF" (concat *wais-top-directory* "bin/wais-tiff-display"))
	(list "PICT" (concat *wais-top-directory* "bin/wais-pict-display")))
  "Alist of Programs to use to view under X
format: ((type viewer) (type viewer) ...)")

(defvar *wais-show-size* t
  "Show size of document in results window")

(defvar *wais-show-date* t
  "Show date of document in results window")

(defun current-line ()
  "return the current line number (in the buffer) of point."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defconst *wais-client-machine* nil
  "if set, this machine will be used to run *wais-binary-pathname* 
   (using rsh)")

(defvar *wais-process* () "the variable that holds the wais process struct")
(defvar *wais-receiving-buffer* "wais-receiving-buffer")

(defun shell-command-fast (string)
  (let ((buf (get-buffer-create "*Shell Command Output*"))
	)
    (save-excursion
      (set-buffer buf)
      (erase-buffer))
    (call-process shell-file-name nil buf nil "-f" "-c" string)
    ))

(defun wais-toggle-multiple-buffers ()
  "Switch between multiple WAIS DOC buffers and one WAIS DOC buffer"
  (interactive)
  (setq *wais-multiple-document-buffers*
	(not *wais-multiple-document-buffers*))
  (message (if *wais-multiple-document-buffers*
	       "Using multiple Document buffers"
	       "Using single Document buffer")))

(defun wais-find-process ()
  "Check status of 'wais' process and start if necessary."
  (cond
   ((not (and *wais-process*
	      (eq (process-status *wais-process*) 'run)))
    (message "Starting new wais process...")
    (and (get-buffer *wais-receiving-buffer*) 
	 (kill-buffer *wais-receiving-buffer*))
    ;; bind process-connection-type to nil
    ;; to avoid allocating a pty. -- taylor, 21 Oct 88
    (let (
	  ;; this doesn't work on the NeXT.  Comment out this line.
	  (process-connection-type nil)
	  )
      (if *wais-client-machine*
	  (setq *wais-process* 
		(start-process "WAIS" *wais-receiving-buffer* 
			       "rsh" *wais-client-machine*
			       waisq-program "-"
			       "-m"
			       (format "%d" *wais-maximum-result-documents*)
			       (if *common-source-directory*
				   "-c" "")
			       (if *common-source-directory*
				   (expand-file-name *common-source-directory*)
				   "")
			       (if *wais-source-directory*
				   "-s" "")
			       (if *wais-source-directory*
				   (expand-file-name *wais-source-directory*)
				   "")))
	  (setq *wais-process* 
		(start-process "WAIS" *wais-receiving-buffer*
			       waisq-program "-"
			       "-m"
			       (format "%d" *wais-maximum-result-documents*)
			       (if *common-source-directory*
				   "-c" "")
			       (if *common-source-directory*
				   (expand-file-name *common-source-directory*)
				   "")
			       (if *wais-source-directory*
				   "-s" "")
			       (if *wais-source-directory*
				   (expand-file-name *wais-source-directory*)
				   "")))))
    (process-kill-without-query *wais-process*)
    (sit-for 3)))
  (let ((b (current-buffer)))
    (set-buffer *wais-receiving-buffer*)
    (emacs-lisp-mode)
    (set-buffer b))
  *wais-process*)

(defvar waisk-mode-map nil)

(if waisk-mode-map
    nil
  (setq waisk-mode-map (copy-keymap text-mode-map))
  (define-key waisk-mode-map "\C-m" 'wais-query)
  (define-key waisk-mode-map "\C-xk" 'wais-kill-buffer))

(defun waisk-mode ()
  "Major mode for editting words for the question.

All the usual text-mode cursor movement works, except

RET     Go for it (answer the Question).

Entering this mode calls value of hook variable waisk-mode-hook."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'waisk-mode)
  (setq mode-name "WaisK")
  (use-local-map waisk-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (run-hooks 'waisk-mode-hook))

(defvar wais-document nil)

(defvar waisd-mode-map nil)

(if waisd-mode-map
    nil
  (setq waisd-mode-map (copy-keymap text-mode-map))
  (define-key waisd-mode-map "?" 'waisd-help)
  (define-key waisd-mode-map "h" 'waisd-help)
  (define-key waisd-mode-map "B" 'waisd-best-line)
  (define-key waisd-mode-map "s" 'wais-add-section)
  (define-key waisd-mode-map "\r" 'wais-query)
  (define-key waisd-mode-map " " 'scroll-up)
  (define-key waisd-mode-map "\C-?" 'scroll-down)
  (define-key waisd-mode-map "q" 'waisd-exit))

(defvar *waisd-mode-string*
    "Major mode in effect in a wais document buffer.

   Movement commands:

   All the usual text-mode cursor movement work.
   In addition the following commands are available:

   B       Go to the best line in the document
   space   Scroll document forward.
   delete  Scroll document backward.

   Action Commands:

   s       Add the marked region as a section to the Relevant Documents.
   ? or h  Show this message (Help).
   q       quit reading this document.  bury this buffer, and the Question
           buffer associated with it.

   When you retrieve a source you will see the source description form in
   the document buffer.  To save this for use in subsequent searches,
   simply use the \"S\" command in the results window, or the standard
   Emacs save-file function (control-x control-s, or M-x save-file).  If
   you use the save-file function, be sure to add the .src suffix so the
   interface will recognize this as a source.  The \"S\" function will add
   the suffix for you.

Entering this mode calls value of hook variable waisd-mode-hook."
)

(defun waisd-mode ()
  "Major mode for WAIS documents.  Use M-x waisd-help for more information."
  (interactive)
  (kill-all-local-variables)
  (make-variable-buffer-local 'wais-document)
  (make-variable-buffer-local 'current-question)
  (make-variable-buffer-local 'current-question-filename)
  (make-variable-buffer-local 'question-name)
  (make-variable-buffer-local 'wais-best-line)
  (setq wais-document t)
  (setq major-mode 'waisd-mode)
  (setq mode-name "Wais DOC")
  (use-local-map waisd-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (run-hooks 'waisd-mode-hook))

(defvar waisq-mode-map nil)

(defun init-waisq-mode-map ()
  (suppress-keymap waisq-mode-map)
  (define-key waisq-mode-map "n" 'wais-edit-next-msg)
  (define-key waisq-mode-map "p" 'wais-edit-previous-msg)
  (define-key waisq-mode-map "\C-n" 'wais-next-msg)
  (define-key waisq-mode-map "\C-p" 'wais-previous-msg)
  (define-key waisq-mode-map "+" 'wais-edit-next-resdoc)
  (define-key waisq-mode-map "-" 'wais-edit-previous-resdoc)
  (define-key waisq-mode-map "a" 'wais-add-reldoc)
  (define-key waisq-mode-map "d" 'wais-delete-reldocs)
  (define-key waisq-mode-map "A" 'wais-add-source)
  (define-key waisq-mode-map "D" 'wais-delete-sources)
  (define-key waisq-mode-map "g" 'wais-query)
  (define-key waisq-mode-map "G" 'wais-query)
  (define-key waisq-mode-map "\r" 'wais-query)
  (define-key waisq-mode-map "q" 'wais-exit)
  (define-key waisq-mode-map "Q" 'wais-quit)
  ;;more to come:
  (define-key waisq-mode-map "e" 'wais-edit)
  (define-key waisq-mode-map "f" 'wais-edit)
  (define-key waisq-mode-map "v" 'wais-edit)
  (define-key waisq-mode-map "h" 'wais-help)
  (define-key waisq-mode-map "?" 'wais-help)
  (define-key waisq-mode-map "N" 'wais-create-question)
  (define-key waisq-mode-map "k" 'wais-goto-keywords)
  (define-key waisq-mode-map "K" 'wais-goto-keywords)
  (define-key waisq-mode-map " " 'wais-scroll-msg-up)
  (define-key waisq-mode-map "" 'wais-scroll-msg-down)
  (define-key waisq-mode-map "s" 'wais-select-question)
  (define-key waisq-mode-map "S" 'wais-save-document)
  (define-key waisq-mode-map "m" 'wais-toggle-multiple-buffers)
  (define-key waisq-mode-map "B" 'waisq-best-line)
  (define-key waisq-mode-map "\C-l" 'wais-redisplay)
  (define-key waisq-mode-map "\C-xk" 'wais-kill-buffer))

(if waisq-mode-map
    nil
  (setq waisq-mode-map (make-keymap))
  (init-waisq-mode-map))

(defvar *waisq-mode-string*
    "Major mode in effect in a wais question buffer.

   Movement commands:

   C-n     Move to next document, or arg documents.
   C-p     Move to previous document, or arg documents.
   e,f,v   Edit, Find or View the current document (all are synonymous).
   n       Edit to next document, or arg documents.
   p       Edit to previous document, or arg documents.
   space   Scroll document in other window forward.
   delete  Scroll document backward.
   B       Go to the best line in the document
   C-l     Refresh Display and reset Question Window.

   Sources:

   A       Add a source to the question.
   D       Delete all sources from the question.

   Relevance Feedback:

   a       Add the current document to the question.
   d       Delete all relevant documents from the question.

   Action Commands:

   k       Replace the 'Find documents on' words
   G,RET   Go for it (submit the query).
   N       Make a new question.
   m       Toggle multiple document buffer mode. 
   s       Select another question.
   S       Save this document to a file.
   ? or h  Show this message (Help).
   q       quit WaisQ, but keep the question's buffer.
   Q       Quit WaisQ and kill this question's buffer.

   New users should try M-x wais-novice.

   Entering this mode calls value of hook variable waisq-mode-hook.

   Some notes on retrieving and saving sources (from the directory of servers,
   or from the help query):

   When you retrieve a source you will see the source description form in
   the document buffer.  To save this for use in subsequent searches,
   simply use the \"S\" command in the results window, or the standard
   Emacs save-file function (control-x control-s, or M-x save-file).  If
   you use the save-file function, be sure to add the .src suffix so the
   interface will recognize this as a source.  The \"S\" function will add
   the suffix for you.
")

(defun wais-help ()
  "Display the special commands available in WaisQ mode"
  (interactive)
  (let ((waisqp (and (boundp 'question-name)
		     question-name)))
    (cond (waisqp
      (wais-redisplay-internal)
      (when wais-split
	(setq wais-split nil)
	(split-window (get-buffer-window (current-buffer))*wais-document-display-size*))
      (other-window 1)))
    (switch-to-buffer (get-buffer-create "*Help*"))
    (erase-buffer)
    (if waisqp
	nil
      (insert "                            Gnu Emacs WAIS.

Use M-x wais, M-x wais-select-question or M-x wais-create-question 
to get into WaisQ mode.

"))
    (insert *waisq-mode-string*)
    (newline 2)
    (insert "  Configuration variables:")
    (newline 2)
    (insert "  Using multiple Document buffers: "
	    (if *wais-multiple-document-buffers*
		"Yes." "No."))
    (goto-char (point-min))
    (if waisqp
	(other-window -1))))

(defun waisd-help ()
  "Display the special commands available in WaisQ mode"
  (interactive)
  (switch-to-buffer (get-buffer-create "*Help*"))
  (erase-buffer)
  (insert *waisd-mode-string*)
  (newline 2)
  (insert "  Configuration variables:")
  (newline 2)
  (insert "  Using multiple Document buffers: "
	  (if *wais-multiple-document-buffers*
	      "Yes." "No."))
  (goto-char (point-min))
  (waisd-mode))

(defun waisq-mode ()
  "Major mode for editting WAIS questions.  Use M-x wais-help to see more"
  (interactive)
  (if (check-init-directories)
      (progn
	(wais-create-question "Quick" "?" "directory-of-servers.src")
	(wais-query))
      (progn
	(setq major-mode 'waisq-mode)
	(setq mode-name "WaisQ")
	(if (eq wais-buffer-type 'keys)
	    (use-local-map waisk-mode-map)
	    (use-local-map waisq-mode-map))
	(make-variable-buffer-local 'wais-buffer-type)
	(make-variable-buffer-local 'question-name)
	(make-variable-buffer-local 'current-question-filename)
	(make-variable-buffer-local 'current-question)
	(make-variable-buffer-local 'headlines)
	(make-variable-buffer-local 'wais-split)
	(setq wais-split t)
	(setq truncate-lines *waisq-truncate-mode*)
	(setq buffer-read-only t)
	(setq tab-width 5)
	(set-syntax-table emacs-lisp-mode-syntax-table)
	(run-hooks 'waisq-mode-hook))))

(defun load-question (file)
  (let ((filename (expand-file-name (concat *wais-question-directory* file))))
    (load-question-internal filename file)))

(defun quiet-replace-string (from-string to-string)
  (while (search-forward from-string nil t)
    (replace-match to-string t t)))

(defun load-question-internal (filename name)
  (find-file filename)
  (emacs-lisp-mode)
  (goto-char (point-min))
  (save-excursion
    (quiet-replace-string "#s(" "("))
  (save-excursion
    (quiet-replace-string "#(" "("))
  (save-excursion
    (quiet-replace-string "d003" ""))
  (save-excursion
    (quiet-replace-string "d004" ""))
  (save-excursion
    (quiet-replace-string "(" ""))
  (save-excursion
    (quiet-replace-string ")" ""))
  (save-excursion
    (quiet-replace-string "\"" "\""))
  (save-excursion
    (quiet-replace-string "" ""))
  (save-excursion
    (quiet-replace-string "" ""))
  (save-excursion
    (quiet-replace-string "
\"" "\""))
  (let ((result (read (current-buffer))))
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))
    result))

(defun dateof (date)
  (if (= (length date) 6)
      (let ((result (make-string 8 ?/)))
	(setf (aref result 0) (aref date 2))
	(setf (aref result 1) (aref date 3))
	(setf (aref result 3) (aref date 4))
	(setf (aref result 4) (aref date 5))
	(setf (aref result 6) (aref date 0))
	(setf (aref result 7) (aref date 1))
	result)
      ""))

(defun any-from-anystring (anystring)
  "return an elisp any from a string that contains an any"
  (let ((l (length anystring)))
    (dotimes (i l)
      (if (= (aref anystring i) ?#)
	  (setf (aref anystring i) 32))))
  (read anystring))

(defun anystring-to-string (anystring)
  "creates a regular old string from an anystring"
  (any-to-string (any-from-anystring anystring)))

(defun any-to-string (any)
  "create a string from an elisp ANY"
  (let* ((size (second (member ':size any)))
	 (bytes (second (member ':bytes any)))
	 (result (make-string size 0))
	 (i 0))
    (dolist (el bytes)
      (setf (aref result i) el)
      (incf i))
    result))

(defun string-to-any (string)
  "create an elisp any from a STRING"
  (let ((l (length string))
	bytes)
    (dotimes (i l)
      (push (aref string i) bytes))
    (list ':any ':size l ':bytes (reverse bytes))))

(defun print-any (any)
  "Returns a string which is the printed representation of an any"
  (let* ((size (second (member ':size any)))
	 (bytes (second (member ':bytes any)))
	 (result (format "(:any :size %d :bytes #( " size)))
    (dolist (el bytes)
      (setq result (concat result (format "%d " el))))
    (concat result ") )")))

(defun get-keys (question)
  (second (member ':seed-words question)))

(defun get-reldocs (question)
  (second (member ':relevant-documents question)))

(defun get-sources (question)
  (second (member ':sources question)))

(defun get-resdocs (question)
  (second (member ':result-documents question)))

(defun get-document (docid)
  (second (member ':document docid)))

(defun get-score (docid)
  (second (member ':score docid)))

(defun get-type (docid)
  (second (member ':type docid)))

(defun get-headline (document)
  (second (member ':headline document)))

(defun get-date (document)
  (second (member ':date document)))

(defun get-size (document)
  (second (member ':number-of-bytes document)))

(defun get-start (docid)
  (second (member ':line-pos
		  (second (member ':start docid)))))

(defun get-end (docid)
  (second (member ':line-pos
		  (second (member ':end docid)))))

(defun headlist (doclist)
  (let ((result nil)
	document)
    (dolist (docid doclist)
      (setq document (get-document docid))
      (push (list (get-score docid)
		  (get-headline document)
		  (get-date document)
		  (get-size document))
	    result))
    (reverse result)))

(defun rellist (doclist)
  (let ((result nil)
	document)
    (dolist (docid doclist)
      (setq document (get-document docid))
      (push (list (get-start docid)
		  (get-end docid)
		  (get-headline (get-document docid))
		  (get-date document))
	    result))
    (reverse result)))

(defun get-sourcename (source)
  (second (member ':filename source)))

(defun sourcelist (sourcelist)
  (let ((result nil))
    (dolist (sid sourcelist)
      (push (get-sourcename sid)
	    result))
    (reverse result)))

(defun find-wais-buffer (name type)
  (let ((result (get-buffer name)))
    (if result
	nil
      (setq result (get-buffer-create name))
      (switch-to-buffer result)
      (setq wais-buffer-type type)
      (waisq-mode))
    (switch-to-buffer result)
    (waisq-mode)
    result))

(defun wais-redisplay-internal ()
  (if (and (boundp 'question-name)
	   question-name)
      (let ((name question-name))
	(setup-wais-display name)
	(if (not (eql name question-name))
	    (display-question name)))
    (error "Not a question buffer.")))

(defun wais-redisplay ()
  "Rebuild the WAISQ display"
  (interactive)
  (wais-redisplay-internal)
  (recenter))

(defun setup-wais-display (name)
  (let ((buff (find-wais-buffer (concat name ": Find Documents On")
				'keys)))
    (delete-other-windows)
    (split-window
     (get-buffer-window buff) 4))
  (setq mode-line-format "-Find Documents On-----%p-%-")
  (setq buffer-read-only nil)
  (other-window 1)
  (split-window
   (get-buffer-window
    (find-wais-buffer (concat name ": On Sources") 'source))
   4)
  (setq mode-line-format '(20 "-On Sources---%p-%-"))
  (other-window 1)
  (find-wais-buffer (concat name ": Results") 'result)
  (setq wais-split t)
  (other-window -1)
  (split-window-horizontally 20)
  (other-window 1)
  (find-wais-buffer (concat name ": Similar To") 'relevant)
  (setq mode-line-format "--Similar To------%p-%-")  
  (other-window 1))

(defun set-buffer-variables (question name filename resheads)
  (setq current-question question)
  (setq question-name name)
  (setq current-question-filename filename)
  (setq headlines resheads)
  (setq default-directory *wais-question-directory*)
  (set-buffer-modified-p nil))

(defun display-question (name &optional file message)
  (let ((q (if file
	       (load-question-internal file name)
	       (load-question name))))
    (display-question-internal 
     (if file file (expand-file-name (concat *wais-question-directory* name)))
     name q message)))

(defun insert-headline (line)
  (insert 
    (concat "" (first line) "	"
	    (if *wais-show-size*
		(if (< (fourth line) 1024)
		    (format "%d\t" (fourth line))
		    (format "%dK\t" (/ (fourth line) 1024)))
		"")
	    (if *wais-show-date*
		(if (string= (third line) "0")
		    " No Date  "
		    (concat "(" (dateof (third line)) ") "))
		"")))
  (let* ((headline (second line))
	 (l (length headline))
	 c)
    (dotimes (i l)
      (setq c (aref headline i))
      (insert c)
      (if (= c 10)
	  (insert "				")))))

(defun display-question-internal (filename name q &optional message)
  (let ((keys (get-keys q))
	(sourcenames (sourcelist (get-sources q)))
	(relheads (rellist (get-reldocs q)))
	(resheads (headlist (get-resdocs q)))
	keybuff sourcebuff relbuff resbuff)
    (setup-wais-display name)
    (if message (message message))
    (setq keybuff (find-wais-buffer (concat name ": Find Documents On") 'keys))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert keys)
    (goto-char (point-min))
    (set-buffer-variables q name filename resheads)
    (setq sourcebuff (find-wais-buffer (concat name ": On Sources") 'source))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq truncate-lines *waisq-truncate-mode*)
    (auto-fill-mode -1)
    (set-buffer-variables q name filename resheads)
    (if sourcenames
	(let ((sorted-sourcenames
	       (sort sourcenames
		     '(lambda (a b) (string< a b)))))
	  (dolist (line sorted-sourcenames)
	    (insert (concat " " line))
	    (newline)))
	(insert " No Sources"))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (setq relbuff (find-wais-buffer (concat name ": Similar To") 'relevant))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq truncate-lines *waisq-truncate-mode*)
    (auto-fill-mode -1)
    (set-buffer-variables q name filename resheads)
    (if relheads
	(dolist (line relheads)
	  (if (first line)
	      (insert (format " [%d,%d] %s%s"
			      (first line) (second line)
			      (if (string= (fourth line) "0")
				  "" (concat "(" (dateof (fourth line)) ") "))
			      (third line)))
	      (insert (concat " "
			      (if (string= (fourth line) "0")
				  "" (concat "(" (dateof (fourth line)) ") "))
			      (third line))))
	  (newline))
	(insert " No documents"))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (setq resbuff (find-wais-buffer (concat name ": Results") 'result))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq truncate-lines *waisq-truncate-mode*)
    (auto-fill-mode -1)
    (set-buffer-variables q name filename resheads)
    (if resheads
	(progn
	  (dolist (line resheads)
	    (insert-headline line)
	    (newline))
	  (delete-char -1))
	(insert "No documents"))
    (goto-char (point-min))
    (setq buffer-read-only t)
    q))

(defun wais-next-line ()
  (while (string-equal (buffer-substring (point) (1+ (point))) "	")
    (next-line 1)
    (beginning-of-line nil)))

(defun wais-prev-line ()  
  (do () 
      ((not (string-equal (buffer-substring (point) (1+ (point))) "	")))
    (next-line -1)
    (beginning-of-line nil)))

(defun wais-next-msg (number)
  "Move the cursor to the next (arg) Document"
  (interactive "p")
  (if (null number) (setq number 1))
  (let ((direction (if (plusp number) 1 -1)))
    (dotimes (i (abs number))
      (next-line direction)
      (if (> direction 0)
	  (wais-next-line)
	  (wais-prev-line)))))

(defun wais-previous-msg (number)
  "Move the cursor to the previous (arg) Document"
  (interactive "p")
  (wais-next-msg (- (if number number 1))))

(defun wais-edit (&optional n)
  "Retrieve the Current Document"
  (interactive "p")
  (wais-edit-next-msg 0))

(defun line-to-doc ()
  (save-excursion
    (beginning-of-line nil)
    (let ((here (point))
	  (result 1))
      (goto-char (point-min))
      (while (< (point) here)
	(wais-next-msg 1)
	(incf result))
      result)))

(defun wais-edit-next-msg (&optional n)
  "Retrieve the next (arg) Document"
  (interactive "p")
  (wais-next-msg n)
  (edit-document current-question-filename (line-to-doc)))

(defun wais-edit-previous-msg (&optional n)
  "Retrieve the previous (arg) Document"
  (interactive "p")
  (wais-edit-next-msg (if n (- n) -1)))

(defun show-dialog (time &optional size message)
  (or size
      (setq size 4))
  (cond ((< (window-height) (+ 2 size))
	 ;;dont split window, too small
	 (save-excursion
	   (switch-to-buffer *wais-receiving-buffer*)
	   (goto-char (point-min))
	   (if (numberp time)
	       (progn (if message
			  (message message))
		      (sit-for time))
	       (read-input (concat (if message message "")
				   " Press return to continue")))))
	(t
	  (save-window-excursion
	    (split-window (get-buffer-window (current-buffer)) 
			  (- (window-height) size))
	    (other-window 1)
	    (switch-to-buffer *wais-receiving-buffer*)
	    (save-excursion
	      (goto-char (point-min))
	      (if (numberp time)
		  (progn (if message
			     (message message))
			 (sit-for time))
		  (read-input (concat (if message message "")
				      " Press return to continue"))))
	    (bury-buffer (current-buffer))))))

(defun wais-query (&optional stuff)
  "Answer this Question"
  (interactive)
  (let (result
	file
	(message "Asking the question..."))
    (update-keywords question-name)
    (wais-redisplay-internal)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq buffer-read-only t)
    (sit-for 0)
    (message message)
    (setq file current-question-filename)
    (setq name question-name)
    (setq result
	  (wais-query-internal file name message))
    (message "Asking the question...done.")
    (if result
	(save-excursion
	  (set-buffer *wais-receiving-buffer*)
	  (goto-char (point-min))
	  (search-forward "Found")
	  (beginning-of-line)
	  (message (buffer-substring (point)
				     (progn
				       (end-of-line)
				       (point)))))
	(display-question name file
			  "Incomplete Transaction.  Question Unmodified."))))

(defun update-keywords (name)
  (save-excursion
    (set-buffer 
      (find-wais-buffer (concat name ": Find Documents On")
			'keys))
    (if current-question-filename
	(wais-replace-keywords (buffer-substring (point-min) (point-max))))))


(defun wais-query-internal (file name &optional message)
  (let (result)
    (update-keywords name)
    (condition-case e
	 (let (command-string)
	   (find-file file)
	   (emacs-lisp-mode)
	   (goto-char (point-min))
	   (if (search-forward ":result-documents" nil t)
	       (setq command-string
		     (concat (buffer-substring (point-min)
					       (progn
						 (forward-char -17)
						 (point))) ")"))
	       (setq command-string
		     (concat (buffer-substring (point-min) (point-max))
			     "
")))
	   (kill-buffer (current-buffer))
	   (message message)
	   (wais-find-process)
	   (set-buffer *wais-receiving-buffer*)
	   (erase-buffer)
	   (process-send-string (wais-find-process) command-string)
	   (accept-process-output (wais-find-process))
	   (if (not (eq (process-status *wais-process*) 'run))
	       (error "WAIS process died. Look in buffer %s for clues."
		      *wais-receiving-buffer*))
	   (goto-char (point-min))
	   (setq result t)
	   (while (not (search-forward " (:question"
				       nil t))
	     (accept-process-output *wais-process*)
	     (goto-char (point-min))
	     (if (or (save-excursion
		       (search-forward "Bad Connection"
				       (save-excursion
					 (if (search-forward " (:question" nil t)
					     (point)))
				       t))
		     (save-excursion
		       (search-forward "Connection refused"
				       (save-excursion
					 (if (search-forward " (:question" nil t)
					     (point)))
				       t)))
		 (progn
		   (setq result nil)
		   (show-dialog t 4 "Looks like a bad connection.")))
	     (if (save-excursion
		   (search-forward "This Question has no sources"
				   (save-excursion
				     (if (search-forward " (:question" nil t)
					 (point)))
				   t))
		 (progn
		   (setq result nil)
		   (show-dialog t 4 "No Source.  Press 'A' to add one. ")))
	     (if (save-excursion
		   (search-forward "Code:"
				   (save-excursion
				     (if (search-forward " (:question" nil t)
					 (point)))
				   t))
		 (progn
		   (show-dialog t 4 "Diagnostic Error")))
	     (if (save-excursion
		   (search-forward "Couldn't find source"
				   (save-excursion
				     (if (search-forward " (:question" nil t)
					 (point)))
				   t))
		 (progn
		   (setq result nil)
		   (show-dialog t 4 "Looks like a bad source spec.")))
	     (if (not (eq (process-status *wais-process*) 'run))
		 (error "WAIS process died. Look in buffer %s for clues."
			*wais-receiving-buffer*))
	     (goto-char (point-min)))
	   (if result
	       (progn
		 (while (not (search-forward "Waisq: Ready for next question."
					     nil t))
		   (accept-process-output *wais-process*)
		   (goto-char (point-min)))
		 (goto-char (point-min))
		 (let ((match "(:question"))
		   (search-forward match)
		   (setq command-string
			 (buffer-substring
			   (- (point) (length match))
			   (let ((end "Waisq: Ready for next question."))
			     (search-forward end)
			     (forward-char (- (length end)))
			     (point))))
		   (find-file file)
		   (erase-buffer)
		   (insert command-string)
		   (let ((require-final-newline nil))
		     (save-buffer 0))
		   (kill-buffer (current-buffer))
		   (setq message (format "%sdone." message))
		   (message message)
		   (display-question name
				     file message)))))
       (error
	 (show-dialog t 6 "Something wrong with query"))
       (quit
	 (display-question name file)
	 (message "Abort Query!")
	 (or *debug*
	   (kill-buffer *wais-receiving-buffer*))))
    result))

(defun get-source-filename (file)
  "Get source file name, adding .src if necessary"
  (interactive "FSource file name: ")
  (if (null file)
      (setq file
	    (read-file-name "Source file name: ")))
  (let ((len (length file)))
    (if (string= (substring file -4) ".src")
	      file
	      (concat file ".src"))))

(defun wais-save-document (&rest foo)
  "Save this document to a file"
  (interactive)
  (wais-edit)
  (condition-case foo
       (progn
	 (other-window 1)
	 (if (string= default-directory *wais-source-directory*)
	     (write-file (get-source-filename nil))
	     (save-buffer))
	 (other-window -1))
     (quit
       (message "Abort!")
       (wais-redisplay))))
  

(defun wais-exit (&optional foo)
  "Leave this Question"
  (interactive)
  (let ((current (current-buffer)))
    (bury-doc-buffers)
    (switch-to-buffer current))
  (wais-redisplay-internal)
  (delete-other-windows)
  (dotimes (i 4)
    (bury-buffer))
  (if (member major-mode 
	      '(waisq-mode waisd-mode waisk-mode))
      (wais-redisplay-internal)))

(defun wais-quit (&optional foo)
  "Kill this Question (and all it's buffers)"
  (interactive)
  (let ((current (current-buffer)))
    (bury-doc-buffers)
    (switch-to-buffer current))
  (wais-redisplay-internal)
  (delete-other-windows)
  (dotimes (i 4)
    (kill-buffer (current-buffer)))
  (and (get-buffer *wais-receiving-buffer*) 
       (kill-buffer *wais-receiving-buffer*))
  (if (member major-mode 
	      '(waisq-mode waisd-mode waisk-mode))
      (wais-redisplay-internal)))

(defun waisd-exit (&optional foo)
  "Burry this Document buffer, and the Question that made it"
  (interactive)
  (other-window -1)
  (let ((current (current-buffer)))
    (bury-doc-buffers)
    (switch-to-buffer current))
  (wais-redisplay-internal)
  (delete-other-windows)
  (dotimes (i 4)
    (bury-buffer))
    (if (member major-mode 
	      '(waisq-mode waisd-mode waisk-mode))
      (wais-redisplay-internal)))

;;; to make kill-buffer a little more tollerant:

(defun wais-kill-buffer (&rest args)
  (interactive)
  (if (eq major-mode 'waisq-mode)
      (if (yes-or-no-p "Really kill this question? ")
	  (wais-quit))))

(defun get-resdoc (num file)
  (let (result)
    (save-excursion
      (find-file file)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (search-forward ":result-documents")
      (search-forward ":document-id" nil t num)
      (let ((loc (- (point) 15)))
	(goto-char loc)
	(forward-char 13)
	(if (search-forward ":document-id" nil t)
	    ;;(forward-sexp 1) - doesn't work correctly.
	    (setq result (buffer-substring loc (- (point) 13)))
	    (goto-char (1- (point-max)))
	    (dotimes (i 2)
	      (while (not (string= (buffer-substring (point) (1+ (point)))
				   ")"))
		(backward-char)))
	    (setq result (buffer-substring loc (- (point) 13)))))
      (kill-buffer (current-buffer)))
    result))

(defun next-or-prev-doc (doc nextp)
  (let ((obuf (current-buffer))
	(buf (get-buffer-create " *wais-resdoc-tmp-buffer"))
	result)
    (switch-to-buffer buf)
    (erase-buffer)
    (insert doc)
    (goto-char (point-min))
    (search-forward ":type")
    (kill-line)
    (insert 
      (if nextp
	  " \"WAIS_NEXT\""
	  " \"WAIS_PREV\""))
    (setq result (buffer-substring (point-min) (point-max)))
    (kill-buffer buf)
    (switch-to-buffer obuf)
    result))

(defun wais-add-reldoc (&optional num)
  "Add the current Document to the Question"
  (interactive)
  (let* ((doc (current-line))
	 (line (second (nth (1- doc) headlines)))
	 (file current-question-filename)
	 (name question-name))
    (update-keywords name)
    (save-excursion
      (let ((string (get-resdoc doc file)))
	(find-file file)
	(goto-char (point-min))
	(search-forward ":relevant-documents")
	(search-forward "( ")
	(insert string))
      (let ((require-final-newline nil))
	(save-buffer 0))
      (kill-buffer (current-buffer))
      (display-question name))
    (find-wais-buffer (concat name ": Results") 'result)
    (goto-line doc)))

(defun wais-delete-reldocs (&optional num)
  "Remove all 'Similar To' Documents from this Question"
  (interactive)
  (let ((doc (current-line))
	(file current-question-filename)
	(name question-name))
    (update-keywords name)
    (find-file file)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (search-forward ":relevant-documents")
    (search-forward "(")
    (backward-char 1)
    (let ((loc (point)))
      (forward-sexp 1)
	 (setq loc (point))
	 (forward-sexp -1)
	 (delete-char (- loc (point)))
	 (insert "(  )
      "))
       (let ((require-final-newline nil))
	 (save-buffer 0))
       (kill-buffer (current-buffer))
       (display-question name current-question-filename)
       (goto-line doc)))

(defun get-doc-type (document)
  (second (member ':type document)))

(defun get-doc-best-line (document)
  (second (member ':best-line document)))

(defun type-from-number (question number)
  (get-doc-type (get-document (nth (1- number)
				   (get-resdocs question)))))

(defun best-line-from-number (question number)
  (get-doc-best-line (get-document (nth (1- number)
					(get-resdocs question)))))

(defun get-filename (string)
  (let ((first-space (do ((i 0 (1+ i)))
			 ((or (= i (length string)) 
			      (= (aref string i) ? )
			      (= (aref string i) ?_))
			  i))))
    (substring string 0 first-space)))

(defun wais-find-viewer (type)
  (do ((e (first *x-viewers*) (first rest))
       (rest (cdr *x-viewers*) (cdr rest)))
      ((or (string= (first e) type)
	   (null rest))
       (if (string= (first e) type)
	   (second e)
	   nil))))

(defun x-view-buffer (name)
  (let ((buffer (get-buffer name)))
    (if (null buffer)
	(generate-new-buffer name)
	(let ((proc (get-buffer-process buffer)))
	  (if (and proc
		   (eq (process-status proc) 'run))
	      (progn 
		(message "Already viewing this file!")
		nil)
	      buffer)))))

(defun view-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 (set-process-buffer proc nil))
	(t 
	  (let ((b (current-buffer)))
	    (set-buffer (process-buffer proc))
	    (goto-char (point-max))
	    (insert "Done.\n")
	    (set-buffer b)))))

(defun x-view-file (fname type)
  (let* ((viewer
	  (wais-find-viewer type))
	 (buffer (x-view-buffer (concat "*xview-" name)))
	 view-process
	 (b (current-buffer))
	 (command (format "%s %s;/bin/rm %s" viewer fname fname)))
    (if viewer
	(if buffer
	    (progn
	      (set-buffer buffer)
	      (make-variable-buffer-local 'wais-document)
	      (setq wais-document 0)
	      (erase-buffer)
	      (insert command "\n")
	      (goto-char (point-max))
	      (set-buffer b)
	      (setq view-process
		    (start-process
		      fname buffer
		      "csh"
		      "-fc" command))
	      (set-process-sentinel view-process 'view-sentinel)
	      buffer)
	    (get-buffer (concat "*xview-" name)))
	(message "unable to view %s, can't find viewer for type: %s" fname type))))

(defun find-doc-buffer (docid)
  (do* ((buffers (buffer-list) (cdr buffers))
	(buff (car buffers) (car buffers)))
       ((or (null buffers)
	    (and buff
		 (equal docid
			(save-excursion
			  (set-buffer buff)
			  wais-document))))
	(if buffers
	    buff nil))))

(defun fix-wais-name (name)
  "Replace TABS in NAME with Space so buffer-select works."
  (dotimes (i (length name))
    (if (or (= (aref name i) ?	)
	    (= (aref name i) ? ))
	(setf (aref name i) ?_)))
  name)

(defun edit-document (filename document-number)
  (let ((buff (current-buffer))
	(result t))
    (condition-case e
	 (let* ((q current-question)
		(f current-question-filename)
		(n question-name)
		(wais-string (concat "(:question :result-documents ( "
				     (get-resdoc document-number
						 current-question-filename)
				     " ) )
"))
		(name (if *wais-multiple-document-buffers*
			  (fix-wais-name (second (nth (1- document-number)
						      headlines)))
			  *wais-document-buffer*))
		(document (get-document (nth (1- document-number)
					     (get-resdocs q))))
		(buffer (find-doc-buffer document))
		(lines headlines)
		(type (type-from-number q document-number))
		(best-line (best-line-from-number q document-number))
		(size (second (member ':number-of-bytes
				      (get-document (nth (1- document-number)
							 (get-resdocs q)))))))
	   (if (plusp size)
	       (if (and buffer *wais-multiple-document-buffers*)
		   (progn
		     (cond (wais-split
		       (setq wais-split nil)
		       (split-window (get-buffer-window (current-buffer))
				     *wais-document-display-size*)))
		     (other-window 1)
		     (switch-to-buffer buffer)
		     (other-window -1))
		   (progn
		     (message "Retrieving Document (%s characters)..." size)
		     (wais-find-process)
		     (set-buffer *wais-receiving-buffer*)
		     (erase-buffer)
		     (process-send-string (wais-find-process) wais-string)
		     (accept-process-output *wais-process*)
		     (if (not (eq (process-status *wais-process*) 'run))
			 (error "WAIS process died. Look in buffer %s for clues."
				*wais-receiving-buffer*))
		     (while (and result
				 (not (save-excursion
					(goto-char (point-min))
					(search-forward "Waisq: Ready for next question." nil t))))
		       ;; check to see if we've got an bad connection
		       (goto-char (point-min))
		       (if (or (save-excursion
				 (search-forward "Connection refused"
						 (save-excursion
						   (if (search-forward "done." nil t)
						       (point)))
						 t))
			       (save-excursion
				 (search-forward "bad connection"
						 (save-excursion
						   (if (search-forward "done." nil t)
						       (point)))
						 t)))
			   (progn
			     (setq result nil)
			     (show-dialog t 4 "Looks like a bad connection.")
			     (top-level)))
		       (if (save-excursion
			     (search-forward "Code:"
					     (save-excursion
					       (if (search-forward "done." nil t)
						   (point)))
					     t))
			   (progn
			     (show-dialog t 4 "Diagnostic Error")
			     (top-level)))
		       (accept-process-output *wais-process*)
		       (if (not (eq (process-status *wais-process*) 'run))
			   (error "WAIS process died. Look in buffer %s for clues."
				  *wais-receiving-buffer*)))
		     (if result
			 (save-excursion
			   (goto-char (point-min))
			   (save-excursion
			     (let* ((end-string "Waisq: Ready for next question.")
				    (size (progn
					    (search-forward end-string)
					    (forward-char (- (1+ (length end-string))))
					    (point))))
			       (message "Received %d bytes...done." size)
			       (setq wais-string
				     (buffer-substring
				       (point-min)
				       size))
			       (switch-to-buffer (if *wais-multiple-document-buffers*
						     (generate-new-buffer name)
						     name))
			       (setq name (buffer-name))
			       (setq buffer-read-only nil)
			       (waisd-mode)
			       (setq wais-document (get-document (nth (1- document-number)
								      (get-resdocs q))))
			       (setq current-question q)
			       (setq current-question-filename f)
			       (setq question-name n)
			       (erase-buffer)
			       (setq wais-best-line best-line)
			       (insert wais-string)
			       (goto-char (point-min))
			       (cond ((and type (string= type "WSRC"))
				      (setq default-directory *wais-source-directory*))
				     ((and type
					   (not (string= type "TEXT"))
					   (not (string= type "WCAT")))
				      (if (getenv "DISPLAY")
					  (let ((buff (current-buffer))
						(fname  (format "%s%s"
								*wais-document-directory*
								(get-filename name))))
					    (set-visited-file-name fname)
					    (set-buffer-modified-p t)
					    (let ((require-final-newline nil))
					      (save-buffer 0))
					    (setq name (x-view-file fname type))
					    (wais-redisplay-internal)
					    (kill-buffer buff))
					  (progn
					    (setq default-directory *wais-document-directory*)
					    (message "Got a %s document I can't display." type))))
				     (t (setq default-directory *wais-document-directory*)
					(goto-char (point-min))
					(if (rmail-p (current-buffer))
					    (wais-rmail-show-message 1))
					(setq buffer-read-only t)))))
			   (switch-to-buffer buff)
			   (progn	;or (and (or (string= type "GIF")
					;	(string= type "TIFF"))
					;   (getenv "DISPLAY"))
			     (cond (wais-split
			       (setq wais-split nil)
			       (split-window (get-buffer-window (current-buffer))
					     *wais-document-display-size*)))
			     (other-window 1)
			     (switch-to-buffer name)
			     (other-window -1)))
			 (show-dialog t 4 "Error retrieving Document"))))
	       (message "Empty Document, nothing to retrieve.")))
       (errors
	 (switch-to-buffer buff)
	 (wais-redisplay-internal)
	 (show-dialog t 6 "Something wrong with retrieval."))
       (quit
	 (switch-to-buffer buff)
	 (wais-redisplay-internal)
	 (message "Abort Retrieval!")
	 (or *debug*
	   (kill-buffer *wais-receiving-buffer*))))
    (switch-to-buffer buff)))

(defun resdoc-from-docret (docret)
  (let* ((b (current-buffer))
	 (c (get-buffer-create "* wais-temp *"))
	 q)
    (set-buffer c)
    (erase-buffer)
    (insert docret)
    (goto-char (point-min))
    (quiet-replace-string "#(" "(")
    (goto-char (point-min))
    (setq q (read c))
    (kill-buffer c)
    (set-buffer b)
    (first (get-resdocs q))))

(defun wais-edit-next-resdoc ()
  "Edit the document cardinally after this document"
  (interactive)
  (edit-next-or-previous-document current-question-filename
				  (current-line) t))

(defun wais-edit-previous-resdoc ()
  "Edit the document cardinally after this document"
  (interactive)
  (edit-next-or-previous-document current-question-filename
				  (current-line) nil))

;;; this mostly works.  It cannot as yet be called from a keystroke.
;;; Need to resolve docid so it doesn't retrieve the document multiple times 
;;; if it's alread in a buffer.  That's pretty close!  I think I'll put it
;;; on a key. How about + an -!

(defun edit-next-or-previous-document (filename document-number nextp)
  (let ((buff (current-buffer))
	(result t))
    (condition-case e
	 (let* ((q current-question)
		(f current-question-filename)
		(n question-name)
		(resdoc (next-or-prev-doc
			  (get-resdoc document-number
				      current-question-filename)
			  nextp))
		(wais-string (concat "(:question :seed-words \"foo\" :relevant-documents ( "
				     resdoc
				     " ) :sources ( "
				     (format "%s ) " (first (get-sources current-question))) " )
")))
	   (progn
	     (wais-find-process)
	     (set-buffer *wais-receiving-buffer*)
	     (erase-buffer)
	     (process-send-string (wais-find-process) wais-string)
	     (accept-process-output *wais-process*)
	     (if (not (eq (process-status *wais-process*) 'run))
		 (error "WAIS process died. Look in buffer %s for clues."
			*wais-receiving-buffer*))
	     (while (and result
			 (not (save-excursion
				(goto-char (point-min))
				(search-forward "Waisq: Ready for next question." nil t))))
	       ;; check to see if we've got an bad connection
	       (goto-char (point-min))
	       (if (or (save-excursion
			 (search-forward "Connection refused"
					 (save-excursion
					   (if (search-forward "Found" nil t)
					       (point)))
					 t))
		       (save-excursion
			 (search-forward "bad connection"
					 (save-excursion
					   (if (search-forward "Found" nil t)
					       (point)))
					 t)))
		   (progn
		     (setq result nil)
		     (show-dialog t 4 "Looks like a bad connection.")
		     (top-level)))
	       (if (save-excursion
		     (search-forward "Code:"
				     (save-excursion
				       (if (search-forward "Found" nil t)
					   (point)))
				     t))
		   (progn
		     (show-dialog t 4 "Diagnostic Error")
		     (top-level)))
	       (accept-process-output *wais-process*)
	       (if (not (eq (process-status *wais-process*) 'run))
		   (error "WAIS process died. Look in buffer %s for clues."
			  *wais-receiving-buffer*)))
	     (if result
		 (save-excursion
		   (goto-char (point-min))
		   (save-excursion
		     (let ((size (1- (progn
				       (goto-char (point-min))
				       (search-forward "Found")
				       (next-line 1)
				       (beginning-of-line)
				       (point)))))
		       (setq wais-string
			     (buffer-substring
			       (point)
			       (progn
				 (search-forward "Waisq: Ready for next question.")
				 (forward-char -31)
				 (point)))))))))
	   (let* ((docid (resdoc-from-docret wais-string))
		  (document (get-document docid))
		  (name (if *wais-multiple-document-buffers*
			    (fix-wais-name (get-headline document))
			    *wais-document-buffer*))
		  (buffer (find-doc-buffer document))
		  (type (get-type docid))
		  (best-line (best-line-from-number q document-number))
		  (size (get-size document)))
	     (if (and buffer *wais-multiple-document-buffers*)
		 (progn
		   (cond (wais-split
		     (setq wais-split nil)
		     (split-window (get-buffer-window (current-buffer))
				   *wais-document-display-size*)))
		   (other-window 1)
		   (switch-to-buffer buffer)
		   (other-window -1))
		 (progn
		   (setq wais-string
			 (format "(:question :result-documents ( %s ) ) "
				 docid))
		   (wais-find-process)
		   (set-buffer *wais-receiving-buffer*)
		   (erase-buffer)
		   (process-send-string (wais-find-process) wais-string)
		   (accept-process-output *wais-process*)
		   (if (not (eq (process-status *wais-process*) 'run))
		       (error "WAIS process died. Look in buffer %s for clues."
			      *wais-receiving-buffer*))
		   (while (and result
			       (not (save-excursion
				      (goto-char (point-min))
				      (search-forward "Waisq: Ready for next question." nil t))))
		     ;; check to see if we've got an bad connection
		     (goto-char (point-min))
		     (if (or (save-excursion
			       (search-forward "Connection refused"
					       (save-excursion
						 (if (search-forward "done." nil t)
						     (point)))
					       t))
			     (save-excursion
			       (search-forward "bad connection"
					       (save-excursion
						 (if (search-forward "done." nil t)
						     (point)))
					       t)))
			 (progn
			   (setq result nil)
			   (show-dialog t 4 "Looks like a bad connection.")
			   (top-level)))
		     (if (save-excursion
			   (search-forward "Code:"
					   (save-excursion
					     (if (search-forward "done." nil t)
						 (point)))
					   t))
			 (progn
			   (show-dialog t 4 "Diagnostic Error")
			   (top-level)))
		     (accept-process-output *wais-process*)
		     (if (not (eq (process-status *wais-process*) 'run))
			 (error "WAIS process died. Look in buffer %s for clues."
				*wais-receiving-buffer*)))
		   (if result
		       (save-excursion
			 (goto-char (point-min))
			 (save-excursion
			   (let ((size (1- (progn
					     (goto-char (point-min))
					     (search-forward "done.")
					     (forward-char 1)
					     (point)))))
			     (save-excursion
			       (word-search-backward "Received")
			       (message "%s...done." (buffer-substring (point) (- size 7))))
			     (setq wais-string
				   (buffer-substring
				     (point)
				     (progn
				       (search-forward "Waisq: Ready for next question.")
				       (forward-char -31)
				       (point))))))
			 (switch-to-buffer (if *wais-multiple-document-buffers*
					       (generate-new-buffer name)
					       name))
			 (setq name (buffer-name))
			 (setq buffer-read-only nil)
			 (waisd-mode)
			 (setq wais-document (get-document docid))
			 (setq current-question q)
			 (setq current-question-filename f)
			 (setq question-name n)
			 (erase-buffer)
			 (setq wais-best-line best-line)
			 (insert wais-string)
			 (goto-char (point-min))
			 (cond ((and type (string= type "WSRC"))
				(setq default-directory *wais-source-directory*))
			       ((and type
				     (not (string= type "TEXT"))
				     (not (string= type "WCAT")))
				(if (getenv "DISPLAY")
				    (let ((buff (current-buffer))
					  (fname  (format "%s%s"
							  *wais-document-directory*
							  (get-filename name))))
				      (set-visited-file-name fname)
				      (set-buffer-modified-p t)
				      (let ((require-final-newline nil))
					(save-buffer 0))
				      (setq name (x-view-file fname type))
				      (wais-redisplay-internal)
				      (kill-buffer buff))
				    (progn
				      (setq default-directory *wais-document-directory*)
				      (message "Got a %s document I can't display." type))))
			       (t (setq default-directory *wais-document-directory*)
				  (goto-char (point-min))
				  (if (rmail-p (current-buffer))
				      (wais-rmail-show-message 1))
				  (setq buffer-read-only t)))
			 (switch-to-buffer buff)
			 (progn
			   (cond (wais-split
			     (setq wais-split nil)
			     (split-window (get-buffer-window (current-buffer))
					   *wais-document-display-size*)))
			   (other-window 1)
			   (switch-to-buffer name)
			   (other-window -1)))
		       (show-dialog t 4 "Error retrieving Document"))))))
       (errors
	 (switch-to-buffer buff)
	 (wais-redisplay-internal)
	 (show-dialog t 6 "Something wrong with retrieval."))
       (quit
	 (switch-to-buffer buff)
	 (wais-redisplay-internal)
	 (message "Abort Retrieval!")
	 (or *debug*
	   (kill-buffer *wais-receiving-buffer*))))
    (switch-to-buffer buff)))

(defun waisd-best-line ()
  (interactive)
  (if (and (boundp 'wais-best-line)
	   wais-best-line)
      (goto-line wais-best-line)))

(defun waisq-best-line ()
  (interactive)
  (other-window 1)
  (waisd-best-line)
  (other-window -1))

(defun wais-add-section ()
  "Add the current region as a section to the document"
  (interactive)
  (let ((here (point))
	(there (mark)))
    (save-window-excursion
      (let ((start-line (current-line))
	    (end-line (save-excursion
			(goto-char there)
			(current-line))))
	(if (> start-line end-line)
	    (let ((temp end-line))
	      (setq end-line start-line)
	      (setq start-line temp)))
	(wais-add-fragment wais-document
			   current-question-filename question-name
			   (1- start-line) (1- end-line))))))

(defun insert-parts (first second)
  (if (and first
	   (listp first))
      (insert-struct first)
      (insert (format "%s " first)))
  (if (and second
	   (listp second)
	   (not (eq (first second) ':any)))
      (progn (newline 1)
	     (insert-struct second))
      (if (stringp second)
	  (progn
	    (insert "\"")
	    (dotimes (i (length second))
	      (if (= (aref second i) ?\")
		  (insert "\\\"")
		(insert (aref second i))))
	    (insert "\""))
	  (if (and (listp second)
		   (eq (first second) ':any))
	      (insert-any second)
	      (insert (format "%s" second))))))

(defun insert-any (any)
  (insert (format "(%s %s %d %s " 
		  (first any) (second any) (third any) (fourth any)))
  (insert "#( ")
  (dolist (n (fifth any))
    (insert (format "%d " n)))
  (insert "
)
)"))

(defun insert-struct (struct)
  (insert "(")
  (if (and (first struct)
	   (listp (first struct)))
      (insert-struct (first struct))
      (insert (format "%s" (first struct)) "
"))
    (do ((first (second struct) (first rest))
	 (second (third struct) (second rest))
	 (rest (cdddr struct) (cddr rest)))
	((null rest)
	 (insert-parts first second))
      (insert-parts first second)
      (newline 1))
    (insert "
)"))

(defun wais-add-fragment (doc file name start end)
  (update-keywords name)
  (let ((reldoc (make-doc-fragment doc start end)))
    (save-excursion
      (find-file file)
      (goto-char (point-min))
      (search-forward ":result-documents")
      (goto-char (point-min))
      (search-forward ":relevant-documents")
      (search-forward "( ")
      (save-excursion
	(insert-struct reldoc))
      (indent-sexp)
      (let ((require-final-newline nil))
	(save-buffer 0))
      (kill-buffer (current-buffer))
      (display-question name))))

(defun make-doc-fragment (doc start end)
  (list ':document-id
	':start (list ':fragment ':line-pos start)
	':end (list ':fragment ':line-pos end)
	':document doc))

(defun wais-delete-all-documents ()
  "Delete all WAIS DOC buffers"
  (interactive)
  (let ((current-buffer (current-buffer)))
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (boundp 'wais-document)
		 wais-document)
	(kill-buffer buf)))))

(defun bury-doc-buffers ()
  (let ((current-buffer (current-buffer)))
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (cond ((and (boundp 'wais-document)
		 wais-document)
	(switch-to-buffer buf)
	(bury-buffer buf))))))

(defun wais-scroll-msg-up (&optional dist)
  "Scroll other window forward."
  (interactive "P")
  (or wais-split
    (condition-case foo
	 (scroll-other-window dist)
       (error (message "Bottom of buffer")))))

(defun wais-scroll-msg-down (&optional dist)
  "Scroll other window backward."
  (interactive "P")
  (or wais-split
    (condition-case foo
	 (scroll-other-window
	   (cond ((eq dist '-) nil)
		 ((null dist) '-)
		 (t (- (prefix-numeric-value dist)))))
       (error (message "Top of buffer")))))

(defvar *rmail-header-regex* "*** EOOH ***")

(defun rmail-p (buffer)
  (save-excursion
    (switch-to-buffer buffer)
    (condition-case rmail-p
	 (re-search-forward *rmail-header-regex*)
       (error nil))))

(defun wais-rmail-show-message (n)
  "Show message in wais."
  (interactive "p")
  (widen)
  (let (blurb)
    (let ((beg (point-min))
	  (end (point-max)))
      (goto-char beg)
      (forward-line 1)
      (if (= (following-char) ?0)
	  (progn
	    (rmail-reformat-message beg end)
	    (rmail-set-attribute "unseen" nil))
	  (search-forward "\n*** EOOH ***\n" end t)
	  (narrow-to-region (point) end))
      (goto-char (point-min))
					;	(rmail-display-labels)
					;	(run-hooks 'rmail-show-message-hook)
      (if blurb
	  (message blurb)))))

(defun waisq (&optional name)
  "Edit a Wais Question"
  (interactive "sEdit an existing question named: ")
  (display-question name))

;; question Menu mode is suitable only for specially formatted data.
(put 'question-menu-mode 'mode-class 'special)

(defun question-menu-mode ()
  "Major mode for editing a list of questions.
   Each line describes one of the questions in Emacs.
   Letters do not insert themselves; instead, they are commands.
   q (or space) -- select question of line point is on.
   Precisely,\\{question-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map question-menu-mode-map)
  (setq truncate-lines *waisq-truncate-mode*)
  (setq question-read-only t)
  (setq major-mode 'question-menu-mode)
  (setq mode-name "Question Menu")
  (run-hooks 'question-menu-mode-hook))

(defun question-menu-select ()
  "Select question described by this line of question menu."
  (interactive)
  (let* ((path (question-menu-get-path)))
    (if (null path)
	(progn
	  (message "No question selected")
	  (bury-buffer))
	(progn
	  (set-buffer-modified-p nil)
	  (switch-to-buffer (other-buffer))
	  (bury-buffer question-menu-buffer-name)
	  (message "Fetching Question %s..." path)
	  (display-question path)
	  (message "Fetching Question %s...done." path)))))

(defun question-menu-get-path ()
  "returns the pathname on this line"
  (if (= (current-line) 1)
      nil
      (progn
	(beginning-of-line)
	(let ((begin (point)))
	  (end-of-line)
	  (let ((answer (buffer-substring begin (point))))
	    (beginning-of-line)
	    (cond ((or (= 0 (length answer))
		       (char-equal (aref "<" 0) (aref answer 0)))
		   (message "No Question on this line")
		   nil)
		  (t answer)))))))

(defun all-questions ()
  "returns a list of the names of questions.  This should look into the 
      question and pull out the name, but that is not in the question struct yet."
  (let ((answer ())
	last-char)
    (let ((directory *wais-question-directory*))
      (dolist (file (directory-files directory))
	(setq last-char (aref file (1- (length file))))
	(if (and (not (file-directory-p (concat directory file)))
		 (not (member file answer))
		 (not (or (string= file ".")
			  (string= file "..")
			  (eq last-char ?~)
			  (eq last-char ?#))))
	    (push file answer))))
    (nreverse answer)))

(defun wais-select-question ()
  "Make a menu of questions so you can select one.  
   Type ? after invocation to get help on commands available.
   Type q immediately to make the question menu go away."
  (interactive)
  (let ((questions (all-questions)))
    (if questions
	(progn
	  (delete-other-windows)
	  (switch-to-buffer "*Question List*")  
	  (question-menu-mode)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert "<<Select a question with <space> or 'q'>>\n")
	  (dolist (question questions)
	    (insert question)
	    (insert "\n"))
	  (delete-char -1)
	  (goto-char (point-min))
	  (forward-line 1)
	  (setq buffer-read-only t)
	  (message
	    "Commands: <space>, q, ? for help.")
	  nil)
	(if (yes-or-no-p
		"You have no questions.  would you like to create one? ")
	  (wais-create-question)))))

(defvar question-menu-mode-map nil "")
(defvar question-menu-buffer-name "*Question List*")

(defun setup-question-mode-map ()
  (suppress-keymap question-menu-mode-map t)
  (define-key question-menu-mode-map "q" 'question-menu-select)
  (define-key question-menu-mode-map "s" 'question-menu-select)
  (define-key question-menu-mode-map " " 'question-menu-select)
  (define-key question-menu-mode-map "n" 'next-line)
  (define-key question-menu-mode-map "p" 'previous-line)
  (define-key question-menu-mode-map "?" 'describe-mode))

(if question-menu-mode-map
    ()
    (progn
      (setq question-menu-mode-map (make-keymap))
      (setup-question-mode-map)))

(defun wais-novice ()
  "Create and run the 'Quick' novice question, and pop up
the novice Help"
  (interactive)
  (make-wais-novice-question))

(defun make-wais-novice-question ()
  (wais-create-question "Quick" "?" "directory-of-servers.src")
  (wais-query)
  (show-novice-wais-help))

(defvar *wais-novice-string* 
"First, I've created a 'Quick' question for you, and hopefully it ran.  You
now see a typical WAIS display.  Let me first tell you that you can scroll
this window by pressing the space bar and the Delete key, in case you can't
see it all.

There are five parts to this display:
1. the Search words
2. the Sources to search
3. some documents that might be similar to your intended document
4. the Resulting documents from the search
5. a document, in this case, this message.

You can now use this Question to ask further questions, or you might wish
to create some questions of your own (they don't all have to be the 'Quick'
question).

The most useful keystrokes for using WAISQ mode are (case is important!):

<space>:  scroll the text in the other window up
  <del>:  scroll the text in the other window down
      k:  switches you to the search word window
  e,f,v:  view the current result document in a window like this one.
      a:  add the current document to the relevant documents list
      d:  delete all the relevant documents
      A:  capital A adds a new source
      D:  captial D deletes all the sources.
  g,RET:  perform the query.
    C-l:  rebuild the display, removing the document view window.
      s:  select a new Question
      q:  quit out of this question, and bury it.
      Q:  capital Q quits out of this question, and kill all its buffers.
      ?:  describe waisq-mode shows all new keystrokes associated with
          WAISQ mode.

You really shouldn't have to use C-x o to switch to other windows, but if
you do, you should go back to the result-documents window and press C-l to
rebuild the display.")

(defun show-novice-wais-help ()
  "Show something useful for a new user of WAIS"
  (interactive)
  (wais-redisplay-internal)
  (cond (wais-split
    (setq wais-split nil)
    (split-window (get-buffer-window (current-buffer))*wais-document-display-size*)))
  (other-window 1)
  (switch-to-buffer (get-buffer-create "*WAIS Novice Help*"))
  (erase-buffer)
  (insert *wais-novice-string*)
  (goto-char (point-min))
  (other-window -1))

(defun wais-create-question (&optional name keywords source)
  "Create a new Question named NAME"
  (interactive)
  (let ((new (check-init-directories)))
    (or name
      (setq name (read-input "Create a new question named: ")))
    (or keywords
      (setq keywords ""))
    (or source
      (setq source (get-source-name)))
    (let ((file (expand-file-name
		  (concat *wais-question-directory*
			  name))))
      (create-question-internal file keywords source)
      (display-question name)
      (wais-goto-keywords))
    (if new
	(message "For more information, try M-x wais-novice"))))

(defun create-question-internal (file keywords source)
  (find-file file)
  (erase-buffer)
  (insert  "(:question 
       :version  2 
       :seed-words \"" keywords "\"
       :sources 
       (  (:source-id 
	   :filename \"" source "\"
	   ) 
	 )
       )
")
  (let ((require-final-newline nil))
    (save-buffer 0))
  (kill-buffer (current-buffer)))

(defun find-documents-on ()
  "Obsolete.  Use M-x wais or M-x wais-create-question"
  (interactive)
  (message "Obsolete.  Use M-x wais or M-x wais-create-question"))

(defun delete-string ()
  (search-forward "\"")
  (let ((here (1- (point))))
    (search-forward "\"")
    (delete-char (- here (point)))))

(defun wais-goto-keywords ()
  "Go to the keyword window for this question"
  (interactive)
  (wais-redisplay-internal)
  (other-window -3))

(defun wais-replace-keywords (&optional keys)
  "Replace the 'Find documents on' words in the current Question"
  (interactive "sFind documents on: ")
  (if (> (length keys) 4999)
      (error "Keys longer than 5000 characters.  I Can't handle that.")
      (let ((file current-question-filename)
	    (name question-name))
	(find-file file)
	(goto-char (point-min))
	(search-forward ":seed-words")
	(delete-string)
	(insert " \"" keys "\"")
	(let ((require-final-newline nil))
	  (save-buffer 0))
	(kill-buffer (current-buffer)))))

(defun wais-delete-sources ()
  "Delete all sources from this question"
  (interactive)
  (let ((doc (current-line))
	(file current-question-filename)
	(name question-name))
    (update-keywords name)
    (find-file file)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (search-forward ":sources")
    (search-forward "(")
    (backward-char 1)
    (let ((loc (point)))
      (forward-sexp 1)
      (setq loc (point))
      (forward-sexp -1)
      (delete-char (- loc (point)))
      (insert "(  )")
      (let ((require-final-newline nil))
	(save-buffer 0))
      (kill-buffer (current-buffer))
      (display-question name current-question-filename)
      (forward-line (1- doc)))))


(defun wais-add-source (&optional source)
  "Add a Source to the current Question"
  (interactive)
  (or source
    (setq source (get-source-name)))
  (let ((doc (current-line))
	(file current-question-filename)
	(name question-name))
    (update-keywords name)
    (find-file file)
    (goto-char (point-min))
    (search-forward ":sources")
    (search-forward "(")
    (insert "  (:source-id :filename \"" source "\" ) 
")
    (let ((require-final-newline nil))
      (save-buffer 0))
    (kill-buffer (current-buffer))
    (display-question name current-question-filename)
    (forward-line (1- doc))))



(defvar *wais-selected-sources* nil
  "A list of selected sources for a default question")

;; source Menu mode is suitable only for specially formatted data.
(put 'source-menu-mode 'mode-class 'special)

(defun source-menu-mode ()
  "Major mode for editing a list of sources.
Each line describes one of the sources in Emacs.
Letters do not insert themselves; instead, there are commands.
q, s, v, or space -- view the source of line point is on.
Precisely,\\{source-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map source-menu-mode-map)
  (setq truncate-lines *waisq-truncate-mode*)
  (setq source-read-only t)
  (setq major-mode 'source-menu-mode)
  (setq mode-name "Source Menu")
  (run-hooks 'source-menu-mode-hook))

(defun source-menu-view ()
  "View source described by this line of source menu."
  (interactive)
  (let* ((path (source-menu-get-path)))
    (if (null path)
	(message "No source selected")
	(if (file-exists-p (concat *wais-source-directory* path))
	    (view-file (concat *wais-source-directory* path))
	    (if (file-exists-p (concat *common-source-directory* path))
		(view-file (concat *common-source-directory* path))))))
  (bury-buffer)
  (if (eq major-mode 'waisq-mode)
      (wais-redisplay-internal)))

(defun source-menu-get-path ()
  "returns the pathname on this line"
  (if (= (current-line) 1)
      nil
      (progn
	(beginning-of-line)
	(let ((begin (point)))
	  (end-of-line)
	  (let ((answer (buffer-substring begin (point))))
	    (beginning-of-line)
	    (cond ((or (= 0 (length answer))
		       (char-equal (aref "<" 0) (aref answer 0)))
		   (message "No Source on this line")
		   nil)
		  (t answer)))))))

(defun all-sources ()
  "returns a list of the names of sources.  This should look into the 
   source and pull out the name, but that is not in the source struct yet."
  (let ((answer ())
	last-char)
    (if (and (stringp *wais-source-directory*)
	     (file-directory-p *wais-source-directory*))
	(dolist (file (directory-files *wais-source-directory*))
	  (setq last-char (aref file (1- (length file))))
	  (if (and (> (length file) 3)
		   (string= (substring file -4) ".src"))
	      (push (cons file file) answer))))
    (if (and *common-source-directory*
	     (stringp *common-source-directory*)
	     (not (eq *common-source-directory* *wais-source-directory*))
	     (file-exists-p *common-source-directory*))
	(dolist (file (directory-files *common-source-directory*))
	  (if (and (> (length file) 3)
		   (string= (substring file -4) ".src"))
	      (push (cons file file) answer))))
    (or (null answer)
      (sort answer '(lambda (a b) (string< (car a) (car b)))))))

(defun wais-view-source ()
  "Make a menu of sources so you can select one to view.  
Type ? after invocation to get help on commands available.
Type q immediately to make the source menu go away."
  (interactive)
  (let ((sources (all-sources)))
    (if sources
	(progn
	  (delete-other-windows)
	  (switch-to-buffer "*Source List*")  
	  (source-menu-mode)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert "<<Select a source with <space> or 'q', view with 'v'>>\n")
	  (dolist (source sources)
	    (insert (car source))
	    (insert "\n"))
	  (delete-char -1)
	  (goto-char (point-min))
	  (forward-line 1)
	  (setq buffer-read-only t)
	  (message
	    "Commands: <space>, q, v or ? for help.")
	  nil)
	(message "No sources.  Something is wrong - see your site administrator"))))

(defvar source-menu-mode-map nil "")
(defvar source-menu-buffer-name "*Source List*")

(defun setup-source-mode-map ()
  (suppress-keymap source-menu-mode-map t)
  (define-key source-menu-mode-map "q" 'source-menu-view)
  (define-key source-menu-mode-map "s" 'source-menu-view)
  (define-key source-menu-mode-map " " 'source-menu-view)
  (define-key source-menu-mode-map "n" 'next-line)
  (define-key source-menu-mode-map "p" 'previous-line)
  (define-key source-menu-mode-map "v" 'source-menu-view)
  (define-key source-menu-mode-map "?" 'describe-mode))

(if source-menu-mode-map
    ()
    (progn
      (setq source-menu-mode-map (make-keymap))
      (setup-source-mode-map)))

(defun get-source-name ()
  (let ((result "")
	(sources (all-sources)))
    (save-window-excursion
      (delete-other-windows)
      (if sources
	  (progn
	    (while (string= result "")
	      (setq result
		    (completing-read "Select Source (press ? for a list of sources): "
				     sources nil t nil)))
	    result)
	  (message
	    "No sources.  Something is wrong - see your site administrator")))))

(defun source-defined-p (source)
  (assoc source (all-sources)))

(defvar index-types '(("groliers" . "groliers")
		      ("mail" . "mail")
		      ("rmail" . "rmail")
		      ("netnews" . "netnews")
		      ("catalog" . "catalog")
		      ("bio" . "bio")
		      ("cmapp" . "cmapp")
		      ("text" . "text")
		      ("para". "para")))

(defun basename (path)
  (do ((loc (1- (length path)) (1- loc)))
      ((or (minusp loc)
	   (eq (aref path loc) ?/))
       (substring path (1+ loc)))))

;;; this will create and index a database
;;; but uses local search (no configuration).

(require 'compile)

(defun wais-index (command)
  "Run waisindex-program, with user-specified args, and collect output in a buffer."
  (interactive "sIndex (with args): ")
  (compile1 (concat waisindex-program command)
	    "Done" "waisindex"))

(defun wais-create-source (source)
  "Create a new Source and a database to go with it"
  (interactive "sName for this Source: ")
  (let ((server-name-or-address nil)
	(port nil))
    (if (yes-or-no-p "Shall I create the index? ")
	(let* ((db (read-file-name "File(s) to index (unix wildcards allowed): "))
	       (type (completing-read "Type (press ? for a list of types, default is Text): "
				      index-types nil t nil))
	       (exportp (yes-or-no-p "Export this Source? "))
	       (index (concat (if exportp
				  *common-source-directory*
				  *wais-source-directory*)
			      source)))
	  (if (string= type "") (setq type "text"))
	  (wais-index (concat (case wais-version
				(7 " -i ")
				(8 " -d ")
				(t " -d "))
			      index " -t " type " "
			      (case wais-version
				(7 "")
				(8 (if exportp
				       " -export "
				       ""))
				(t ""))
			      (expand-file-name db))))
	(progn
	  (setq index (read-file-name "Index for this sources (path prefix, without . suffix): "))
	  (setq server-name-or-address (read-input "On Server: "))
	  (if (not (string= server-name-or-address ""))
	      (setq port (read-input "Using port: ")))
	  (message "Creating source %s, for index %s..." source index)
	  (find-file (concat *wais-source-directory*
			     (if (string= (substring *wais-source-directory* -1) "/")
				 ""
				 "/")
			     source ".src"))
	  (erase-buffer)
	  (insert "(:source
   :version  3
   " (if (and server-name-or-address
	      (not (string= server-name-or-address "")))
	 (concat ":ip-name \"" server-name-or-address "\"
   ")
	 "")
   (if (and port
	    (not (string= "" port)))
       (concat ":tcp-port " port "
   ")
       "")
   ":database-name \"" (expand-file-name index) "\"
   :cost 0.00
   :cost-unit :free
   :description \"Source structure created by GMACS Wais interface, ")
	  (insert-date t)
	  (insert "\"
   )")
	  (let ((require-final-newline nil))
	    (save-buffer 0))
	  (kill-buffer (current-buffer))
	  (message "Creating source %s, for index %s...done." source index)
	  ))))


;;; a simple way to get into "wais"

(defun wais ()
  "Find a wais question and go to it.
First tries to find the question named Quick, then any wais question,
finally it creates a Quick question, prompting for search words and source.
If this is the first time a user tries to use wais, it will create a new
question name Quick, using the directory-of-servers as a source, and submit
a '?' for the query."
  (interactive)
  (let ((quick-buffer (get-buffer "Quick: Results")))
    (if quick-buffer
	(progn
	  (set-buffer quick-buffer)
	  (wais-redisplay-internal))
	 ;;; that means we don't have a quick question around.
	 ;;; let's find what we've got
	(do* ((buffers (buffer-list) (cdr buffers))
	      (buffer (car buffers) (car buffers))
	      (done nil))
	     ((or (null buffers) done) 
	      (or done (wais-create-question "Quick")))
	  (set-buffer buffer)
	  (cond ((and (boundp 'current-question-filename)
		     current-question-filename)
	    (wais-redisplay-internal)
	    (setq done t)))))))

;;; code to create the question directory if it doesn't exist

(defun wais-create-directory (directory)
  "create a directory"
  (if (string= "/" (substring directory -1))
      (setq directory (substring directory 0 -1)))
  (shell-command-fast (concat "/bin/mkdir " directory))
  (if (null (file-attributes directory))
      (error "Could not create directory %s" directory)))

(defun check-init-directories ()
  (let ((result nil))
    (if (not (file-attributes *wais-question-directory*))
	(progn (message "Creating %s" *wais-question-directory*)
	       (wais-create-directory (expand-file-name *wais-question-directory*))
	       (setq result t)))
    (if (not (file-attributes *wais-source-directory*))
	(progn (message "Creating %s" *wais-source-directory*)
	       (wais-create-directory (expand-file-name *wais-source-directory*))
	       (setq result t)))
    (if (not (file-attributes *wais-document-directory*))
	(progn (message "Creating %s" *wais-document-directory*)
	       (wais-create-directory (expand-file-name *wais-document-directory*))
	       (setq result t)))
    result))
