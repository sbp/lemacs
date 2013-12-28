;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

(defun w3-length-after-parsing (str)
  "Returns the length of a string after removing all text between
<>, and resolving all HTML entity references"
  (let ((tmp ""))
    (while (string-match "\\([^<]*\\)<[^>]+>" str)
      (setq tmp (concat tmp (w3-match str 1))
	    str (substring str (match-end 0) nil)))
    (setq tmp (concat tmp str))
    (setq tmp (w3-fix-entities-in-string tmp))
    (length tmp)))

(defun w3-first-n-items (l n)
  "Return the first N items from list L"
  (let ((x 0)
	y)
    (if (> n (length l))
	(setq y l)
      (while (< x n)
	(setq y (nconc y (list (nth x l)))
	      x (1+ x))))
    y))
	       
(defun w3-breakup-menu (menu-desc max-len)
  (if (> (length menu-desc) max-len)
      (cons (cons "More..." (w3-first-n-items menu-desc max-len))
	    (w3-breakup-menu (nthcdr max-len menu-desc) max-len))
    menu-desc))

(defun w3-get-url-at-point (&optional pt)
  "Get the URL closest to point, but don't change your
position. Has a preference for looking backward when not
directly on a symbol."
  ;; Not at all perfect - point must be right in the name.
  (save-excursion
    (if pt (goto-char pt))
    (let ((filename-chars ".a-zA-Z0-9---_/:") start)
      (save-excursion
	;; first see if you're just past a filename
	(if (not (eobp))
	    (if (looking-at "[] \t\n[{}()]") ; whitespace or some parens
		(progn
		  (skip-chars-backward " \n\t\r({[]})")
		  (if (not (bobp))
		      (backward-char 1)))))
	(if (string-match (concat "[" filename-chars "]")
			  (char-to-string (following-char)))
	    (progn
	      (skip-chars-backward filename-chars)
	      (setq start (point))
	      (skip-chars-forward filename-chars))
	  (error "No URL found around point!"))
	(buffer-substring start (point))))))

(defun w3-follow-url-at-point (&optional pt)
  "Follow the URL under PT, defaults to link under (point)"
  (interactive "d")
  (w3-fetch (w3-get-url-at-point pt)))			     

(defun w3-batch-fetch ()
  "Fetch all the URLs on the command line and save them to files in
the current directory.  The first argument after the -f w3-batch-fetch
on the command line should be a string specifying how to save the
information retrieved.  If it is \"html\", then the page will be
unformatted when it is written to disk.  If it is \"text\", then the
page will be formatted before it is written to disk.  If it is
\"binary\" it will not mess with the file extensions, and just save
the data in raw binary format.  If none of those, the default is
\"text\", and the first argument is treated as a normal URL."  
  (if (not noninteractive)
      (error "`w3-batch-fetch' is to be used only with -batch"))
  (let ((fname "")
        (curname "")
	(x 0)
	(args (symbol-value 'command-line-args-left))
	(w3-strict-width 80)
	(retrieval-function 'w3-fetch)
	(file-format "text")
	(file-extn ".txt"))
    (setq file-format (downcase (car args)))
    (cond
     ((string= file-format "html")
      (message "Saving all text as raw HTML...")
      (setq retrieval-function 'w3-retrieve
	    file-extn ".html"
	    args (cdr args)))
     ((string= file-format "binary")
      (message "Saving as raw binary...")
      (setq retrieval-function 'w3-retrieve
	    file-extn ""
	    args (cdr args)))
     ((string= file-format "text")
      (message "Saving all text as formatted...")
      (setq args (cdr args)))
     (t
      (message "Going with default, saving all text as formatted...")))
    (while args
      (funcall retrieval-function (car args))
      (if (string= file-extn "") nil
	(setq fname (w3-file-extension (w3-basepath w3-current-file t) t)))
      (if (string= (w3-strip-leading-spaces fname) "")
	  (setq fname "root"))
      (setq curname fname)
      (while (file-exists-p (concat curname file-extn))
	(setq curname (concat fname x)
	      x (1+ x)))
      (setq fname (concat curname file-extn))
      (write-region (point-min) (point-max) fname)
      (setq args (cdr args)))))

(defun w3-eat-trailing-space (x)
  "Remove spaces/tabs at the end of a string"
  (let ((y (1- (length x)))
	(skip-chars (list ?  ?\t ?\n)))
    (while (and (>= y 0) (memq (aref x y) skip-chars))
      (setq y (1- y)))
    (substring x 0 (1+ y))))

(defun w3-strip-leading-spaces (x)
  "Remove spaces at the front of a string"
  (let ((y (1- (length x)))
	(z 0)
	(skip-chars (list ?  ?\t ?\n)))
    (while (and (<= z y) (memq (aref x z) skip-chars))
      (setq z (1+ z)))
    (substring x z nil)))

(defun w3-reload-all-files ()
  "Reload all w3 files"
  (interactive)
  (let ((x '(w3 w3-auth w3-forms w3-gopher w3-html+ w3-hyperbole
		w3-lists w3-mime w3-misc w3-mosaic w3-parse w3-print
		w3-typecheck w3-url w3-vars w3-viewers w3-emacs
		w3-emacs19 w3-epoch w3-lucid)))
    (while x
      (setq features (delq (car x) features)
	    x (cdr x)))
    (require 'w3)))		

(defun w3-document-source ()
  "View this documents source"
  (interactive)
  (let ((w3-source t)
	(url (w3-view-url t)))
    (if url
	(progn (w3-retrieve (w3-view-url t))
	       (w3-sentinel nil nil))
      (message "Automatically generated hypertext - no source available."))))

(defun w3-mail-current-document ()
  "Mail the current-document to someone"
  (interactive)
  (let* ((format (completing-read
		  "Format: "
		  '(("HTML Source") ("Formatted Text") ("LaTeX Source"))
		  nil t))
	 (url (w3-view-url t))
	 (str 
	  (save-excursion
	    (cond
	     ((equal "HTML Source" format) (w3-retrieve url))
	     ((equal "Formatted Text" format) nil)
	     ((equal "LaTeX Source" format)
	      (w3-retrieve url)
	      (w3-convert-html-to-latex)))
	    (buffer-string))))
    (cond
     ((and w3-mutable-windows (fboundp w3-mail-other-window-command))
      (funcall w3-mail-other-window-command))
     ((fboundp w3-mail-command)
      (funcall w3-mail-command))
     (w3-mutable-windows (mail-other-window))
     (t (mail)))
    (mail-subject)
    (insert (format "%s from URL %s" format url))
    (re-search-forward mail-header-separator nil)
    (forward-char 1)
    (while (< (current-column) 79) (insert "-"))
    (insert "\n" str "\n")
    (while (< (current-column) 79) (insert "-"))
    (mail-to)))

(defun w3-parse-relative-link (url &optional method)
  "Try to resolve a link like \"library/io.html\""
  (let ((resolved (cond ((equal w3-current-type "http")
			 (concat "http://" w3-current-server ":" 
				 w3-current-port))
			((equal w3-current-type "ftp")
			 (concat "file://" w3-current-server "/"))
			(t "file:"))))
    (if (= ?# (string-to-char url))
	(setq resolved url)
      (progn
	(setq url (w3-remove-relative-links url))
	(cond
	 ((equal (string-to-char url) 47)
	  (setq resolved (concat resolved url)))
	 (t (setq resolved (concat resolved
				   (w3-basepath w3-current-file) "/" url))))))
    resolved))

(defun w3-internal-use-history (hist-item)
  "Go to the link in the history"
  (let ((url (nth 0 hist-item))
	(buf (nth 1 hist-item))
	(pnt (nth 2 hist-item)))
    (cond
     ((null buf)			; Find a buffer with same url
      (let ((x (buffer-list))
	    (found nil))
	(while (and x (not found))
	  (save-excursion
	    (set-buffer (car x))
	    (setq found (string= (w3-view-url t) url))
	    (if (not found) (setq x (cdr x)))))
	(cond
	 (found
	  (switch-to-buffer (car x))
	  (if (number-or-marker-p pnt) (goto-char pnt)))
	 (t
	  (w3-fetch url)))))
     ((buffer-name buf)			; Reuse the old buffer if possible
      (switch-to-buffer buf)
      (if (number-or-marker-p pnt) (goto-char pnt))
      (if (and url (= ?# (string-to-char url)))	; Destination link
	  (progn
	    (goto-char (point-min))
	    (w3-find-specific-link (substring url 1 nil)))))
     (url (w3-maybe-relative url))		; Get the link
     (t (message "Couldn't understand whats in the history.")))))

(defun w3-forward-in-history ()
  "Go tot the next link in the history"
  (interactive)
  (cond
   ((null w3-current-next-link) (message "No previous history item."))
   (t (w3-internal-use-history w3-current-next-link))))

(defun w3-backward-in-history ()
  "Go to the previous link in the history"
  (interactive)
  (cond
   ((null w3-current-last-link) (message "No previous history item."))
   (t (w3-internal-use-history w3-current-last-link))))

(defun w3-relative-link (url &optional method data)
  (if (equal "#" (substring url 0 1))
      (progn
	(setq w3-current-last-link (list "" (current-buffer) (point)))
	(setq w3-current-next-link (list url nil nil))
	(goto-char (point-min))
	(w3-find-specific-link (substring url 1 nil)))
    (w3-fetch (w3-parse-relative-link url method) method data)))
  
(defun w3-maybe-eval ()
  "Maybe evaluate a buffer of emacs lisp code"
  (if (funcall w3-confirmation-func "This is emacs-lisp code, evaluate it?")
      (eval-current-buffer)
    (emacs-lisp-mode)))

(defun w3-build-continuation ()
  "Build a series of functions to be run on this file"
  (save-excursion
    (set-buffer w3-working-buffer)
    (let ((cont w3-default-continuation)
	  (extn (w3-file-extension w3-current-file)))
      (if (assoc extn w3-uncompressor-alist)
	  (setq extn (w3-file-extension
		      (substring w3-current-file 0 (- (length extn))))))
      (if w3-source (setq w3-current-mime-viewer
			  (w3-mime-viewer "www/source")))
      (if (not w3-current-mime-viewer)
	  (setq w3-current-mime-viewer
		(w3-mime-viewer (w3-extension-to-mime extn))))
      (if w3-current-mime-viewer
	  (setq cont (append cont '(w3-pass-to-viewer)))
	(setq cont (append cont (list w3-default-action))))
      cont)))

(defun w3-link-info (&optional url no-show)
  "Get more information on a link."
  (interactive)
  (if (not url) (setq url (w3-view-this-url t)))
  (if (not url) (setq url (w3-view-url t)))
  (let (ctype result serv fil type)
    (string-match "^\\([^:]+\\):/*\\([^/]+\\)/*\\(.*\\)" url)
    (setq type (w3-match url 1)
	  serv (w3-match url 2)
	  fil (w3-match url 3))
    (cond
     ((string= type "news")
      (setq ctype (if (string-match "@" serv) "news article"
		    (concat "newsgroup (" serv ")"))
	    type "NNTP site at %s"
	    serv w3-news-server))
     ((and (string= type "file")
	   (not (string-match "^file://" url)))
      (setq serv "local file system."
	    ctype (concat (cdr (assoc (w3-extension-to-mime
				       (w3-file-extension fil))
				      w3-mime-descriptions)) " file")
	    type "%s"))
     ((string= "gopher" type)
      (setq type (concat type " site at %s."))
      (if (= ?0 (string-to-char fil))
	  (setq ctype "gopher document")
	(setq ctype "gopher directory")))
     (t
      (if (string= type "file") (setq type "ftp"))
      (setq ctype (concat (cdr
			   (assoc (w3-extension-to-mime
				   (w3-file-extension fil))
				  w3-mime-descriptions)) " file")
	    type (concat type " site at %s."))))
    (setq result
	  (format "A %s on the %s" ctype (format type serv)))
    (if (not no-show) (message result) result)))

(defun w3-use-links ()
  "Select one of the &lt;LINK&gt; tags from this document and fetch it."
  (interactive)
  (if (not w3-current-links)
      (error "No <LINK> tags in this document.")
    (w3-fetch (cdr (assoc (completing-read "Link with REV="
					   w3-current-links nil t)
			  w3-current-links)))))

(defun w3-fix-fake-urls (st nd)
  "Fix fake urls into real \"<A HREF=...></A>\" type links"
  (interactive "r")
  (save-restriction
    (narrow-to-region st nd)
    (goto-char (point-min))
    (w3-replace-regexp
     (concat w3-nonrelative-link "\\(:[^ >]+\\)") " \\1\\2")
    (w3-replace-regexp
     (concat "[^=\"]+" w3-nonrelative-link "\\(:[^ >\\\n]+\\)")
     "<A HREF=\"\\1\\2\"> \\1\\2 </A>")))

(defun w3-update-source-files (w3-source-directory)
  "Retrieve the latest version of the w3 browser and copy it to`
w3-source-directory"
  (interactive "DWhere to store w3.tar.z? ")
  (let ((newfile (format "%s%sw3.tar.z" w3-source-directory
			 (if (equal "/" (substring w3-source-directory -1 nil))
			     "" "/")))
	(oldfile "/anonymous@cs.indiana.edu:/pub/elisp/w3/w3.tar.z"))
    (if (file-newer-than-file-p oldfile newfile)
	(copy-file oldfile newfile 3))
    (shell-command (format "gunzip %s" newfile))
    (shell-command (format "cd %s ; tar xf %s"
			   w3-source-directory
			   newfile))))

(defun w3-hexify-string (str)
  "Escape characters in a string"
  (let ((str2 "")
	(char 0))
    (while (not (equal str ""))
      (setq char (string-to-char str)
	    str (substring str 1 nil)
	    str2 (format "%s%s" str2
			 (if (or (> char ?z)
				 (< char ?/)
				 (and (< char ?a)
				      (> char ?Z))
				 (and (< char ?A)
				      (> char ?9)))
			     (if (< char 16)
				 (upcase (format "%%0%x" char))
			       (upcase (format "%%%x" char)))
			   (format "%c" char)))))
    str2))

(defun w3-find-this-file ()
  "Do a find-file on the currently viewed html document if it is a file: or
ftp: reference"
  (interactive)
  (cond
   ((and (null w3-current-type)
	 (eq major-mode 'w3-mode))
    (if w3-mutable-windows
	(find-file-other-window w3-current-file)
      (find-file w3-current-file)))
   ((equal w3-current-type "ftp")
    (if w3-mutable-windows
	(find-file-other-window
	 (format "/anonymous@%s:%s" w3-current-server w3-current-file))
      (find-file 
       (format "/anonymous@%s:%s" w3-current-server w3-current-file))))
   (t (message "Sorry, I can't get that file so you can alter it."))))

(defun w3-delete-from-alist (x alist)
  "Remove X from ALIST, return new alist"
  (if (eq (assoc x alist) (car alist)) (cdr alist)
    (delq (assoc x alist) alist)))

(defun w3-count-occurences (regexp)
  "Count # of matches for REGEXP after point. Modified from the how-many
function of emacs19"
  (let ((n 0) opoint)
    (save-excursion
      (while (and (not (eobp))
		  (progn (setq opoint (point))
			 (re-search-forward regexp nil t)))
	(if (= opoint (point))
	    (forward-char 1)
	  (setq n (1+ n)))))
    n))      

(defun w3-insert-this-url (pref-arg)
  "Insert the current url in another buffer, with prefix ARG, insert URL under point"
  (interactive "P")
  (let ((thebuf (get-buffer (read-buffer "Insert into buffer: ")))
	(oldbuf (current-buffer))
	(url (if pref-arg (w3-view-this-url t) (w3-view-url t))))
    (if (not (equal "Not on a link!" url))
	(progn
	  (set-buffer thebuf)
	  (insert url)
	  (set-buffer oldbuf)))))      

(defun w3-show-hotlist ()
  "View the hotlist in hypertext form"
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (not w3-hotlist)
      (error "Sorry, no hotlist is in memory.")
    (let ((tmp w3-hotlist)
	  (buff (current-buffer)))
      (w3-clear-tmp-buffer)
      (insert "<TITLE> Hotlist </TITLE>\n")
      (insert (format "<H1> Hotlist From %s </H1>\n" w3-hotlist-file))
      (insert "<OL>\n")
      (while tmp
	(insert (format "<LI> <A HREF=\"%s\">%s</A>\n"
			(car (cdr (car tmp)))
			(car (car tmp))))
	(setq tmp (cdr tmp)))
      (insert "\n</OL>\n")
      (setq w3-continuation (append w3-continuation '(w3-prepare-buffer))
	    w3-current-type "www"
	    w3-current-file "hotlist.html"
	    w3-current-mime-viewer 'w3-prepare-buffer
	    w3-current-last-buffer buff)
      (w3-prepare-buffer))))

(defun w3-lookup-style (type)
  "Return the physical style of logical style <TYPE>"
  (let ((x (cdr (assoc type w3-style-assoc))))
    (if (symbolp x) (symbol-value x) x)))	

(defun w3-make-sequence (start end)
  "Make a sequence (list) of numbers from START to END"
  (let ((sqnc '()))
    (while (< start end)
      (setq sqnc (cons (int-to-string start) sqnc)
	    start (1+ start)))
    sqnc))

(defun w3-maybe-relative (url &optional method data)
  "Take a url and either fetch it, or resolve relative refs, then fetch it"
  (cond
   ((not
     (string-match w3-nonrelative-link url))
    (w3-relative-link url method data))
   (t (w3-fetch url method data))))
 
(defun w3-in-assoc (elt list)
  "Check to see if ELT matches any of the regexps in the car elements of LIST"
  (let (rslt)
    (while (and list (not rslt))
      (and (car (car list))
	   (string-match (car (car list)) elt)
	   (setq rslt (car list)))
      (setq list (cdr list)))
    rslt))

(defun w3-member (elt list)
  "Function defined so that we are sure member will always use equal, like
its supposed to.  This was pulled from Jamie Zawinskies byte compiler "
  (while (and list (not (equal elt (car list))))
    (setq list (cdr list)))
  list)

(defun w3-goto-last-buffer ()
  "Go to last WWW buffer visited"
  (interactive)
  (if w3-current-last-buffer
      (if w3-mutable-windows
	  (pop-to-buffer w3-current-last-buffer)
	(switch-to-buffer w3-current-last-buffer))
    (message "No previous buffer found.")))

(defun w3-file-extension (fname &optional x)
  "Return the filename extension of FNAME.  If optional variable X is t,
then return the basename of the file with the extension stripped off."
  (if (string-match "\\.[^\\.]+$" fname)
      (if x (substring fname 0 (match-beginning 0))
	(substring fname (match-beginning 0) nil))
    ""))

(defun w3-toggle-telnet ()
  "Toggle telnetting status"
  (interactive)
  (setq w3-use-telnet (not w3-use-telnet)))

(defun w3-basepath (file &optional x)
  "Return the base pathname of FILE, or the actual filename if X is true"
  (if (string-match "\\(.*\\)/\\([^/]*\\)" file)
      (if (not x) (substring file (match-beginning 1) (match-end 1))
	(substring file (match-beginning 2) (match-end 2)))
    file))

(defun w3-replace-regexp (regexp to-string)
  "Quiet replace-regexp."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-string t nil)))

(defun w3-find-highest-link-num ()
  "Find highest NAMEd link, so we can number on from there."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let ((lnkctr 0))
    (while (re-search-forward "<A[ \t\n]+NAME=" (point-max) t)
      (let* ((start (match-end 0))
	     (end (save-excursion
		    (re-search-forward "[ \t\n>]" (point-max) t)
		    (match-beginning 0)))
	     (subst (buffer-substring start end)))
	(and subst
	     (> (string-to-int subst) lnkctr)
	     (setq lnkctr (string-to-int subst)))))
    lnkctr))

(defun w3-clear-tmp-buffer ()
  "Clear the temporary W3 buffer"
  (set-buffer (get-buffer-create w3-working-buffer))
  (if buffer-read-only (toggle-read-only))
  (erase-buffer))  

(defun w3-preview-this-buffer ()
  "See what this buffer will look like when its formatted."
  (interactive)
  (let* ((thebuf (current-buffer)))
    (w3-clear-tmp-buffer)
    (insert-buffer thebuf)
    (setq w3-current-file (buffer-file-name thebuf)
	  w3-current-type nil)
    (w3-sentinel nil nil)))

(defun w3-open-stream (name buffer host service)
  "Open a stream to a host, using either telnet (if w3-use-telnet is t,
or a raw stream (if w3-use-telnet is nil)"
  (if w3-use-telnet
      (let ((proc
	     (start-process name buffer w3-telnet-prog host
			    (int-to-string service)))
	    (tmp nil))
	(save-excursion
	  (set-buffer buffer)
	  (setq tmp (point))
	  (while (not (progn
			(goto-char (point-min))
			(re-search-forward w3-telnet-ready-regexp nil t)))
	    (accept-process-output))
	  (delete-region tmp (point))
	  (goto-char (point-min))
	  (if (re-search-forward "connect:" nil t)
	      (progn
		(condition-case ()
		    (delete-process proc)
		  (error nil))
		(w3-replace-regexp ".*connect:.*" "")
		nil)
	    proc)))
    (let ((tries 1)
	  (x t)
	  (z nil))
      (while (and x
		  (<= tries w3-connection-retries))
	(condition-case ()
	    (setq z (open-network-stream name buffer host service)
		  x nil)
	  (error (progn
		   (message "Failed on connection try #%d" tries)
		   (sit-for 1)
		   (setq x t
			 tries (1+ tries))))))
      (if (> tries w3-connection-retries)
	  (format "Could not establish connection to %s:%d" host service)
	z))))

(defun w3-unhex (x)
  (if (> x ?9)
      (if (>= x ?a)
	  (+ 10 (- x ?a))
	(+ 10 (- x ?A)))
    (- x ?0)))

(defun w3-unhex-string (str)
  "Remove %XXX embedded spaces, etc in a url"
  (let ((tmp ""))
    (while (string-match "%[0-9a-f][0-9a-f]" str)
      (let* ((start (match-beginning 0))
	     (ch1 (w3-unhex (elt str (+ start 1))))
	     (code (+ (* 16 ch1)
		      (w3-unhex (elt str (+ start 2))))))
	(setq tmp
	      (concat 
	       tmp (substring str 0 start)
	       (char-to-string code)))
	(setq str (substring str (match-end 0)))))
    (setq tmp (concat tmp str))
    tmp))

(defun w3-rassoc (key list)
  "An 'rassoc' function - don't want to bother with loading cl just
for this function"
  (let ((found nil))
    (while (and list (not found))
      (if (equal (cdr (car list)) key) (setq found (car list)))
      (setq list (cdr list)))
    found))

(defun w3-insert-entities-in-string (str)
  "Remove special characters in STR and replace them with HTML[+] entities"
  (let ((tmp "")
	(regexp "\\(&\\|<\\|>\\)")
	(temp '(("&lt;" . "<") ("&gt;" . ">") ("&amp;" . "&")))
	(x nil))
    (while (string-match regexp str)
      (setq x (car (w3-rassoc (w3-match str 1) temp))
	    tmp (format "%s%s%s" tmp (substring str 0 (match-beginning 0)) x)
	    str (substring str (match-end 0))))
    (concat tmp str)))

(defun w3-fix-entities-in-string (str)
  "Remove &xxx; entities in string STR"
  (let ((tmp "")
	(regexp (concat "\\(" (mapconcat (function (lambda (x) (car x)))
					 w3-html-entities "\\|") "\\)"))
	(x nil))
    (while (string-match regexp str)
      (setq x (cdr (assoc (w3-match str 1) w3-html-entities))
	    tmp (format "%s%s%s" tmp (substring str 0 (match-beginning 0)) x)
	    str (substring str (match-end 0))))
    (setq tmp (concat tmp str))
    tmp))    
       
(defun w3-edit-source ()
  "Edit the html document just retrieved"
  (set-buffer w3-working-buffer)
  (let ((ttl (format "Editing %s Annotation: %s"
		     (cond
		      ((eq w3-editing-annotation 'group) "Group")
		      ((eq w3-editing-annotation 'personal) "Personal")
		      (t "Unknown"))
		     (w3-basepath w3-current-file t)))
	(str (buffer-string)))
    (set-buffer (get-buffer-create ttl))
    (insert str)
    (kill-buffer w3-working-buffer)))

(defun w3-clean-text ()
  "Clean up a buffer after telnet (trash at beginning, connection closed)"
  (set-buffer w3-working-buffer)
  (if (and w3-use-telnet (equal w3-current-type "http"))
      (progn
	(goto-char (point-min))
	(kill-line w3-telnet-header-length)
	(w3-replace-regexp "Connection.*" ""))))

(defun w3-source ()
  "Show the source of a file"
  (let ((tmp (buffer-name (generate-new-buffer "Document Source"))))
    (set-buffer w3-working-buffer)
    (kill-buffer tmp)
    (rename-buffer tmp)
    (set-buffer-modified-p nil)
    (buffer-enable-undo)
    (if w3-mutable-windows (pop-to-buffer tmp) (switch-to-buffer tmp))))

(defun w3-uncompress ()
  "Uncompress a file"
  (set-buffer w3-working-buffer)
  (let ((extn (w3-file-extension w3-current-file)))
    (if (assoc extn w3-uncompressor-alist)
	(progn
	  (message "Uncompressing")
	  (shell-command-on-region (point-min) (point-max)
				   (cdr (assoc extn w3-uncompressor-alist))
				   t)))))

(defun w3-sentinel (proc string)
  (set-buffer w3-working-buffer)
  (w3-replace-regexp "Process WWW.*" "")
  (let ((x (w3-build-continuation)))
    (while x
      (funcall (car x))
      (setq x (cdr x)))))

(defun w3-show-history-list ()
  "Format the w3-history-list prettily and show it to the user"
  (interactive)
  (if (not w3-history-list)
      (error "Sorry, no history list available now!")
    (let ((urls w3-history-list)
	  (buff (current-buffer)))
      (w3-clear-tmp-buffer)
      (insert "<TITLE> History List For This Session of W3</TITLE>\n")
      (insert "<H1>History List For This Session of W3</H1>\n")
      (insert "<H2>(Oldest items last in list)</H2>\n")
      (insert "<OL>\n")
      (while urls
	(insert (format "<LI> <A HREF=\"%s\">%s</A>\n"
			(car (car urls)) (cdr (car urls))))
	(setq urls (cdr urls)))
      (insert "\n</OL>\n")
      (setq w3-continuation (append w3-continuation '(w3-prepare-buffer))
	    w3-current-type "www"
	    w3-current-file "history.html"
	    w3-current-mime-viewer 'w3-prepare-buffer
	    w3-current-last-buffer buff)
      (w3-prepare-buffer))))

(defun w3-save-as ()
  "Save a document to the local disk"
  (interactive)
  (let ((format (completing-read
		 "Format: "
		 '(("HTML Source") ("Formatted Text") ("LaTeX Source"))
		 nil t))
	(fname (expand-file-name
		(read-file-name "File name: " default-directory))))
    (cond
     ((equal "HTML Source" format)
      (w3-retrieve (w3-view-url t)))	; Get the document
     ((equal "Formatted Text" format)
      nil)				; Do nothing - we have the text already
     ((equal "LaTeX Source" format)
      (w3-retrieve (w3-view-url t))	; Get the file
      (w3-convert-html-to-latex)))	; Convert to LaTeX
    (write-region (point-min) (point-max) fname)))

(defun w3-upcase-region (st nd)
  "Uppercase a region of text, ignoring text within < and >"
  (save-excursion
    (goto-char st)
    (while (re-search-forward "<[^>]+>" nd t)
      (upcase-region st (match-beginning 0))
      (setq st (match-end 0)))
    (upcase-region st nd)))
      

(provide 'w3-misc)
