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

(require 'w3-vars)
(require 'nntp)

(defvar w3-lazy-message-time 0)
(defun w3-lazy-message (&rest args)
  "Just like `message' but is a no-op if called more than once a second."
  (if (= w3-lazy-message-time
	 (setq w3-lazy-message-time (nth 1 (current-time))))
      nil
    (apply 'message args)))

(or (fboundp 'current-time) (fset 'w3-lazy-message 'message))

(defun w3-build-url (protocol)
  "Build a url for PROTOCOL, return it as a string"
  (interactive (list (cdr (assoc (completing-read
				  "Protocol: "
				  w3-acceptable-protocols-alist nil t)
				 w3-acceptable-protocols-alist))))
  (let (user host port file)
    (cond
     ((null protocol) (error "Protocol is unknown to me!"))
     ((string= protocol "news")
      (setq host (read-string "Enter news server name, or blank for default: ")
	    port (read-string "Enter port number, or blank for default: ")
	    file (read-string "Newgroup name or Message-ID: ")))
     ((string= protocol "mailto") (setq file (read-string "E-mail address: ")))
     ((string= protocol "http")
      (setq host (read-string "Enter server name: ")
	    port (read-string "Enter port number, or blank for default: ")
	    file (read-string "Remote file: "))
      (and (string= "" port) (setq port nil))
      (and (string= "" host) (error "Must specify a remote machine!")))
     ((string= protocol "file")
      (if (funcall w3-confirmation-func "Local file?")
	  (setq file (read-file-name "Local File: " nil nil t))
	(setq user (read-string "Login as user (blank=anonymous): ")
	      host (read-string "Remote machine name: "))
	(and (string= user "") (setq user "anonymous"))
	(and (string= host "") (error "Must specify a remote machine!"))
	(setq file (read-file-name "File: " (format "/%s@%s:" user host)
				   nil t)
	      file (substring file (length (format "/%s@%s:" user host))))))
     ((or (string= protocol "telnet")
	  (string= protocol "tn3270"))
      (setq user (read-string "Login as user (blank=none): ")
	    host (read-string "Remote machine name: ")
	    port (read-number "Port number (blank=23): "))
      (and (string= "" port) (setq port nil))
      (and (string= "" user) (setq user nil))
      (and (string= "" host) (error "Must specify a host machine!")))
     ((string= protocol "gopher")
      (setq host (read-string "Enter server name: ")
	    port (read-string "Enter port number, or blank for default: ")
	    file (read-string "Remote file: "))
      (and (string= "" port) (setq port nil))
      (and (string= "" host) (error "Must specify a remote machine!"))))
    (message "%s:%s%s"
	     protocol
	     (if (null host) "" (concat "//" host
					(if (null port) "" (concat ":" port))))
	     (if (= ?/ (string-to-char file)) file (concat "/" file)))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for the different types of urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-http (url &optional method data)
  "Retrieve URL via http.  If SOURCE is non-nil, then don't parse the buffer."
  (let ((lb (current-buffer))
	(x w3-current-server)
	(y w3-current-port))
    (w3-clear-tmp-buffer)
    (setq w3-current-type "http"
	  w3-current-last-buffer lb)
    (if (string-match "http:/\\([^/].*\\)" url)	; Weird url
	(setq url (format "http://%s:%s/%s" x y (substring url
							   (match-beginning 1)
							   (match-end 1)))))
	
    (string-match "http:+/*\\([^:/]*\\):*\\([^/]*\\)/*\\(/.*\\)" url)
    (let* ((server (substring url (match-beginning 1) (match-end 1)))
	   (port   (substring url (match-beginning 2) (match-end 2)))
	   (file   (substring url (match-beginning 3) (match-end 3)))
	   (dest   (if (string-match "#.+$" file)
		       (prog1
			   (substring file (1+ (match-beginning 0))
				      (match-end 0))
			 (setq file (substring file 0 (match-beginning 0))))
		     nil))
	   (parsed-mime nil)
	   (target-size 0))
      (if (and (string= server "")
	       (string= port ""))
	  (progn
	    (string-match "/*\\([^:]+\\):*\\([0-9]*\\)" file)
	    (setq server (substring file (match-beginning 1) (match-end 1))
		  port (substring file (match-beginning 2) (match-end 2))
		  file "/")))
      (if (or (not (w3-member port w3-bad-port-list))
	      (funcall w3-confirmation-func
		       (format
			"Warning!  Trying to connect to port %s - continue? "
			port)))
	  (progn
	    (if (equal port "") (setq port "80"))
	    (if (equal file "") (setq file "/") )
	    (setq w3-current-server server
		  w3-current-port port
		  w3-current-file file
		  w3-find-this-link dest)
	    (message "Fetching: %s %s %s" server port file)
	    (let ((process
		   (w3-open-stream "WWW" w3-working-buffer server
				   (string-to-int port))))
	      (if (stringp process)
		  (progn
		    (set-buffer w3-working-buffer)
		    (erase-buffer)
		    (setq w3-current-mime-type "text/html"
			  w3-current-mime-viewer (w3-mime-viewer "text/html"))
		    (insert "<TITLE>ERROR</TITLE>\n"
			    "<H1>ERROR - Could not establish connection</H1>"
			    "<P>"
			    "The browser could not establish a connection "
			    (format "to %s:%s.<P>" server port)
			    "The server is either down, or the URL"
			    (format "(%s) is malformed.<P>" (w3-view-url t)))
		    (message process))
		(progn
		  (process-kill-without-query process)
		  (process-send-string process
				  (w3-create-mime-request file "" method data))
		  (if nil; w3-be-asynchronous
		      (set-process-sentinel process 'w3-sentinel)
		    (save-excursion
		      (set-buffer w3-working-buffer)
		      (while (memq (process-status process) '(run open))
			(if (and w3-show-http2-transfer
				 (not parsed-mime)
				 (w3-is-mime-response))
			    (progn
			      (goto-char (point-min))
			      (if (re-search-forward
				   "^content-length:[ \\\t]*\\([0-9]+\\)"
				   nil t)
				  (setq target-size
					(string-to-int
					 (buffer-substring
					  (match-beginning 1)
					  (match-end 1)))))
			      (setq parsed-mime t)))
			(if (> target-size 0)
			    (w3-lazy-message "Read %d of %d bytes (%d%%)"
					     (point-max) target-size
					     (/ (* (point-max) 100)
						target-size))
			  (w3-lazy-message "Read %d bytes." (point-max)))
			(accept-process-output))
		      (condition-case ()
			  (delete-process process)
			(error nil))))))))
	(progn
	  (ding)
	  (message "Aborting connection to bad port..."))))))

(defun w3-file (url)
  "Find a link to an ftp site - simple transformation to ange-ftp format"
  (if (not (string-match "//" url)) (w3-open-local-internal url)
    (progn
      (string-match "^\\(file\\|ftp\\):/*\\([^/]*\\)/*\\(/.*\\)" url)
      (let* ((site (substring url (match-beginning 2) (match-end 2)))
	     (file (substring url (match-beginning 3) (match-end 3)))
	     (user "anonymous")
	     (dest   (if (string-match "#.+$" file)
			 (prog1
			     (substring file (1+ (match-beginning 0))
					(match-end 0))
			   (setq file (substring file 0 (match-beginning 0))))
		       nil))
	     (obuf (current-buffer)))
	(if (string-match "@" site)
	    (setq user (substring site 0 (match-beginning 0))
		  site (substring site (1+ (match-beginning 0)) nil)))
	(w3-clear-tmp-buffer)
	(setq w3-current-last-buffer obuf)
	(let ((filename (concat "/" user "@" site ":" file)))
	  (cond
	   ((equal site "localhost") (w3-open-local-internal file))
	   ((file-directory-p filename)
	    (if (eq w3-directory-format 'hypertext)
		(progn
		  (setq w3-current-type "ftp"
			w3-find-this-link dest
			w3-current-last-buffer obuf
			w3-current-server site
			w3-current-file (format
					 "%s%sindex.html" file
					 (if (equal "/"
						    (substring file -1 nil))
					     "" "/")))
		  (w3-format-directory filename))
	      (progn
		(if (get-buffer w3-working-buffer)
		    (kill-buffer w3-working-buffer))
		(find-file filename))))
	   (t
	    (progn
	      (set-buffer (get-buffer-create w3-working-buffer))
	      (setq w3-current-type "ftp")
	      (setq w3-current-server site)
	      (setq w3-current-file file)
	      (insert-file-contents filename nil)))))))))

(defun w3-news-get-header (header)
  "Get header information HEADER out of news article in nntp buffer"
  (set-buffer " *nntpd*")
  (goto-char (point-min))
  (if (re-search-forward (concat "^" header ": +\\(.*\\)") nil t)
      (buffer-substring (match-beginning 1) (match-end 1))
    ""))

(defun w3-news-get-body ()
  "Get body of article from the nntp buffer"
  (set-buffer " *nntpd*")
  (goto-char (point-min))
  (re-search-forward "\\\n\\\n")
  (buffer-substring (match-end 0) (point-max)))

(defun w3-format-news ()
  "Format a news buffer in html"
  (let ((from  (w3-news-get-header "From"))
	(subj  (w3-news-get-header "Subject"))
	(org   (w3-news-get-header "Organization"))
	(date  (w3-news-get-header "Date"))
	(body  (w3-news-get-body)))
    (w3-clear-tmp-buffer)
    (setq w3-current-file ""
	  w3-current-type "")
    (insert
     (format "<TITLE>%s</TITLE>\n" subj)
     (format
      "<ADDRESS>%s</ADDRESS><P>\n<H1>%s</H1><P>\n%s %s<P><P><P>\n<PRE>%s</PRE>"
      from subj org date body))))

(defun w3-format-whole-newsgroup (newsgroup header-list)
  (w3-clear-tmp-buffer)
  (setq w3-current-file ""
	w3-current-type "")
  (insert (format "<TITLE>%s</TITLE>\n<H1>%s</H1>\n<DL>\n" newsgroup
		  newsgroup))
  (while header-list
    (insert
     (format "<DT>%s\n<DD><A HREF=\"news:%s\">%s</A>\n"
	     (nntp-header-from (car header-list))
	     (if (string-match "<\\(.*\\)>"
			       (nntp-header-id (car header-list)))
		 (substring (nntp-header-id (car header-list))
			    (match-beginning 1) (match-end 1)))
	     (nntp-header-subject (car header-list))))
    (setq header-list (cdr header-list))))

(defun w3-parse-news-url (url)
  "Parse out a news url"
  (string-match "/*\\([^/:]*\\):*\\([0-9]*\\)/*\\(.*\\)" url)
  (let (
	(host (substring url (match-beginning 1) (match-end 1)))
	(port (substring url (match-beginning 2) (match-end 2)))
	(art  (substring url (match-beginning 3) (match-end 3))))
    (if (equal port "") (setq port "119"))
    (if (equal host "") (setq host w3-news-server))
    (if (equal art "") (setq art host
			     host w3-news-server))
    (list host port art)))

(defun w3-news (article)
  "Find a news reference"
  (let ((buff (current-buffer)))
    (set-buffer (get-buffer-create w3-working-buffer))
    (setq w3-current-last-buffer buff)
    (set-buffer buff))
  (let* ((info (w3-parse-news-url article))
	 (host (nth 0 info))
	 (port (nth 1 info))
	 (article (nth 2 info)))
    (if (not (equal w3-current-nntp-server host))
	(nntp-close-server))
    (if (not (nntp-server-opened))
	(progn
	  (message "Reopening connection to %s:%s" host port)
	  (if (not (nntp-open-server host (string-to-int port)))
	      (error "News server not responding!"))))
    (if (string-match "@" article);; Its a specific article
	(progn
	  (if (not (equal ">" (substring article -1 nil)));; get in correct
	      (setq article (format "<%s>" article)));; format
	  (if (nntp-request-article article);; did we get it?
	      (w3-format-news);; yes
	    (progn
	      (set-buffer w3-working-buffer)
	      (insert "<TITLE>ERROR</TITLE>\n"
		      "<H1> Error requesting article... </H1>"
		      "The status message returned by the NNTP server was:<BR>"
		      (format "<PRE>%s</PRE><P>" (nntp-status-message))
		      "If you feel this is an error, <A HREF=\""
		      "mailto:wmperry@indiana.edu>send me mail</A>."))))
      (progn;; Its a whole group
	(if (not (nntp-request-group article))
	    (message "%s" (nntp-status-message))
	  (let*
	      ((stat (nntp-status-message))
	       (st (string-to-int (substring stat
					     (match-beginning 1)
					     (match-end 1))))
	       (nd (string-to-int (substring stat
					     (match-beginning 2)
					     (match-end 2)))))
	    (w3-format-whole-newsgroup article
				       (nntp-retrieve-headers
					(w3-make-sequence st nd)))))))
    (setq w3-current-file "newsgroup.html")))
  
(defun w3-open-local (fname)
  "Find a local file, interactively"
  (interactive "FLocal file: ")
  (if (not w3-setup-done) (w3-do-setup))
  (w3-fetch (concat "file:" fname)))

(defun w3-open-local-internal (fname)
  "Find a local file."
  (let ((lb (current-buffer))
	(dest   (if (string-match "#.+$" fname)
		    (prog1
			(substring fname
				   (1+ (match-beginning 0)) (match-end 0))
		      (setq fname (substring fname 0 (match-beginning 0))))
		  nil)))
    (if (string-match "file:" fname)
	(setq fname (substring fname (match-end 0) nil)))
    (setq fname (expand-file-name fname))
    (w3-clear-tmp-buffer)
    (message "Fetching... %s" fname)
    (setq w3-current-type nil
	  w3-find-this-link dest
	  w3-current-last-buffer lb
	  w3-current-file (if (string-match "file:" fname)
			      (substring fname (match-end 0) nil) fname))
    (if (file-directory-p fname)
	(if (eq w3-directory-format 'hypertext)
	    (progn
	      (w3-format-directory fname)
	      (setq w3-current-file (format
				     "%s%s" fname
				     (if (equal "/"
						(substring fname -1 nil))
					 "" "/"))))
	  (progn
	    (if (get-buffer w3-working-buffer)
		(kill-buffer w3-working-buffer))
	    (find-file fname)))
      (insert-file-contents fname))))

(defun w3-format-directory (dir)
  "Format the files in DIR into hypertext"
  (let ((files (directory-files dir)) file
	div attr mod-time size)
    (save-excursion
      (setq div (1- (length files)))
      (set-buffer w3-working-buffer)
      (erase-buffer)
      (insert (format "<TITLE>Index of %s</TITLE>\n" dir))
      (insert (format "<H1> Directory listing of %s</H1>\n<P>" dir))
      (insert "<DL>\n")
      (while files
	(message "Building directory list... (%d%%)"
		 (/ (* 100 (- div (length files))) div))
	(setq file (if (equal "/" (substring dir -1 nil))
		       (format "%s%s" dir (car files))
		     (format "%s/%s" dir (car files))))
	(setq attr (file-attributes file)
	      mod-time (nth 5 attr)
	      size (nth 7 attr))
	(if (or (equal '(0 0) mod-time) ; Set to null if unknown or
                                        ; untranslateable
		(not (or w3-running-lemacs w3-running-new-lucid
			 w3-running-FSF19)))
	    (setq mod-time nil)
	  (setq mod-time (current-time-string mod-time)))
	(if (<= size 0) (setq size nil) (setq size (int-to-string size)))
	(cond
	 ((equal "." (car files)) nil)
	 ((equal ".." (car files)) 
	  (insert (format "<DT> <A HREF=\"%s\">%s</A>"
			  (car files) "Parent Directory")))
	 ((stringp (nth 0 attr))	; Symbolic link handling
	  (insert (format "<DT> <A HREF=\"%s\">%s -&gt; %s</A>"
			  (car files) (car files) (nth 0 attr))))
	 ((nth 0 attr)			; Directory handling
	  (insert (format "<DT> <A HREF=\"%s\">%s/</A>"
			  (car files) (car files))))
	 (t				; Normal file handlnig
	  (insert (format "<DT> <A HREF=\"%s\">%s</A>"
			  (car files) (car files)))))
	(cond
	 ((equal "." (car files)) nil)
	 (t 
	  (if (not (or mod-time size)) nil (insert "\n<DD>"))
	  (if mod-time (insert "Last Mod: " mod-time))
	  (if (and mod-time size) (insert ", "))
	  (if size (insert "Size: " size " bytes"))
	  (insert "\n")))
	(setq files (cdr files)))
      (insert "\n</DL>"))))

(defun w3-telnet (url)
  "Open up a telnet connection"
  (string-match "telnet:/*\\(.*@\\)*\\([^/]*\\)/*" url)
  (let* ((server (substring url (match-beginning 2) (match-end 2)))
	 (name (if (match-beginning 1)
		   (substring url (match-beginning 1) (1- (match-end 1)))
		 nil))
	 (title (format "%s%s" (if name (concat name "@") "") server))
	 (thebuf (string-match ":" server))
	 (port (if thebuf
		   (prog1
		       (substring server (1+ thebuf) nil)
		     (setq server (substring server 0 thebuf))) "23")))
    (if (equal window-system 'x)
	(start-process "htmlsub" nil w3-xterm-command
		       "-title" title
		       "-ut" "-e" w3-telnet-prog server port)
      (terminal-emulator
       (get-buffer-create (format "%s%s:%s" (if name (concat name "@") "")
				  server port))
       w3-telnet-prog
       (list server port)))
    (if name (message "Please log in as %s" name))))

(defun w3-tn3270 (url)
  "Open up a tn3270 connection"
  (string-match "tn3270:/*\\(.*@\\)*\\([^/]*\\)/*" url)
  (let* ((server (substring url (match-beginning 2) (match-end 2)))
	 (name (if (match-beginning 1)
		   (substring url (match-beginning 1) (1- (match-end 1)))
		 nil))
	 (thebuf (string-match ":" server))
	 (port (if thebuf
		   (prog1
		       (substring server (1+ thebuf) nil)
		     (setq server (substring server 0 thebuf))) "23")))
    (if (equal window-system 'x)
	(start-process "htmlsub" nil w3-tn3270-emulator
		       "-title" server "-ut" "-e" w3-tn3270-emulator
		       server port)
      (terminal-emulator
       (get-buffer-create (format "%s%s:%s" (if name (concat name "@") "")
				  server port))
       w3-tn3270-emulator (list server port)))))

(defun w3-mailto (url)
  "Send mail to someone"
  (string-match "mailto:/*\\(.*\\)" url)
  (let ((to (substring url (match-beginning 1) (match-end 1)))
	(url (w3-view-url t)))
    (cond
     ((and w3-mutable-windows (fboundp w3-mail-other-window-command))
      (funcall w3-mail-other-window-command))
     ((fboundp w3-mail-command)
      (funcall w3-mail-command))
     (w3-mutable-windows (mail-other-window))
     (t (mail)))
    (mail-to)
    (insert (format "%s\nX-URL-From: %s" to url))
    (mail-subject)))

(defun w3-grok-gopher-url (url)
  "Return a list of attributes from a gopher url.  List is of the
type: host port selector-string MIME-type extra-info"
  (let (host				; host name
	port				; Port #
	selector			; String to send to gopher host
	type				; MIME type
	extra				; Extra information
	x				; Temporary storage for host/port
	y				; Temporary storage for selector
	)
    (or (string-match "gopher:/*\\([^/]+\\)/*\\(.*\\)" url)
	(error "Can't understand url %s" url))
    (setq x (w3-match url 1)		; The host (and possible port #)
	  y (w3-unhex-string
	     (w3-match url 2)))		; The selector (and possible type)

    ;First take care of the host/port/gopher+ information from the url
    ;A + after the port # (host:70+) specifies a gopher+ link
    ;A ? after the port # (host:70?) specifies a gopher+ ask block
    (if (string-match "^\\([^:]+\\):\\([0-9]+\\)\\([?+]*\\)" x)
	(setq host (w3-match x 1)
	      port (w3-match x 2)
	      extra (w3-match x 3))
      (setq host x
	    port "70"
	    extra nil))
    (cond
     ((equal extra "")  (setq extra nil))
     ((equal extra "?") (setq extra 'ask-block))
     ((equal extra "+") (setq extra 'gopher+)))

    ; Next, get the type/get rid of the Mosaic double-typing. Argh.
    (setq x (string-to-char y)		; Get gopher type
	  selector y			; Get the selector string
	  type (cdr (assoc x w3-gopher-to-mime)))
    
    (list host port (or selector "") type extra)))

(defun w3-gopher (url)
  "Handle gopher URLs"
  (let ((descr (w3-grok-gopher-url url)))
    (cond
     ((or (not (w3-member (nth 1 descr) w3-bad-port-list))
	  (funcall
	   w3-confirmation-func
	   (format "Warning!  Trying to connect to port %s - continue? "
		   (nth 1 descr))))
      (if w3-use-hypertext-gopher
	  (w3-do-gopher descr)
	(gopher-dispatch-object (vector (if (= 0
					       (string-to-char (nth 2 descr)))
					    ?1
					  (string-to-char (nth 2 descr)))
					(nth 2 descr) (nth 2 descr)
					(nth 0 descr)
					(string-to-int (nth 1 descr)))
				(current-buffer))))
     (t (ding) (message "Aborting connection to bad port...")))))

(defun w3-retrieve (url &optional method data)
  "Retrieve a document using any of the supported types.
No parsing is done, just return the HTML (or whatever) text.
Text is left in w3-working-buffer.  Returns whether the resulting
buffer (if any) should be parsed as HTML."
  (if (get-buffer w3-working-buffer)
      (kill-buffer w3-working-buffer))
  (string-match "\\([^:]*\\):" url)
  (let* ((type (substring url (match-beginning 1) (match-end 1))))
    (cond
     ((equal type "mailto") (w3-mailto url))
     ((equal type "news") (w3-news (substring url (match-end 0) nil)))
     ((equal type "local") (w3-open-local url))
     ((equal type "ftp") (w3-file url))
     ((equal type "file") (w3-file url))
     ((equal type "http") (w3-http url method data))
     ((equal type "telnet") (w3-telnet url))
     ((equal type "gopher") (w3-gopher url))
     ((equal type "tn3270") (w3-tn3270 url))
     (t (progn
	   (set-buffer (get-buffer-create w3-working-buffer))
	   (erase-buffer)
	   (insert "<TITLE> Link Error! </TITLE>\n"
		   "<H1> An error has occurred... </H1>\n"
		   (format "The link type <CODE>%s</CODE>" type)
		   " is unrecognized or unsupported at this time.<P>\n"
		   "If you feel this is an error, please"
		   "<A HREF=\"mailto://wmperry@indiana.edu\">send me mail.</A>"
		   "<P><ADDRESS>William Perry</ADDRESS>"
		   "<ADDRESS>wmperry@indiana.edu</ADDRESS>")
	   (setq w3-current-file "error.html"))))
    (cond
     ((not (get-buffer w3-working-buffer)) nil)
     ((w3-is-mime-response) (w3-parse-mime-headers))
     ((w3-member w3-current-server w3-bad-server-list) nil)
     (t
      (setq w3-bad-server-list (cons w3-current-server w3-bad-server-list))))))

(defun w3-fetch (&optional url method data)
  "Function to read in a URL and dispatch it to the appropriate handler."
  (interactive (list (read-string "URL: " (if (eq major-mode 'w3-mode)
					      (w3-view-url t) ""))))
  (let ((x (w3-view-url t))
	(y (current-buffer))
	(z (point))
	(bufs (buffer-list))
	(found nil))
    (if (not w3-setup-done) (w3-do-setup))
    (if (string= "file:nil" x) (setq x nil))
    (while (and bufs (not found))
      (save-excursion
	(set-buffer (car bufs))
	(setq found (if (and
			 (not (equal (buffer-name (car bufs))
				     w3-working-buffer))
			 (equal (w3-view-url t) url)) (car bufs) nil)
	      bufs (cdr bufs))))
    (if (or (not found)
	    (cond
	     ((eq w3-reuse-buffers 'no) t)
	     ((eq w3-reuse-buffers 'yes) nil)
	     (t
	      (if w3-reuse-buffers
		  (progn
		    (ding)
		    (message
		     "Warning: Invalid value for variable w3-reuse-buffers: %s"
		     (prin1-to-string w3-reuse-buffers))
		    (sit-for 2)))
	      (not (funcall w3-confirmation-func
			    (format "URL found in buffer %10s, reuse "
				    (buffer-name found)))))))
	(progn
	  (w3-retrieve url method data)
	  (if (get-buffer w3-working-buffer)
	      (progn
		(setq w3-current-next-link (list url nil nil))
		(set-buffer w3-working-buffer)
		(setq w3-current-last-link (list x y z))
		(w3-sentinel nil nil))))
      (switch-to-buffer found))))
    
(provide 'w3-url)
