;;; LCD Archive Entry:
;;; w3-mode|William M. Perry|wmperry@indiana.edu|
;;; Major mode for browsing World Wide Web nodes|
;;; 94-1-8|1.9.9|Location undetermined
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
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
;;; This is a major mode for browsing documents written in Hypertext Markup ;;;
;;; Language (HTML).  These documents are typicallly part of the World Wide ;;;
;;; Web (WWW), a project to create a global information net in hypertext    ;;;
;;; format.				                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Known bugs: 			                             	    ;;;
;;; 1. No image support for lucid emacs                                     ;;;
;;; 2. Searching in the hypertext gopher doesn't work yet                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Todo:			                                            ;;;
;;; 1. Ability to edit group and personal annotations                       ;;;
;;; 2. Support for more than one annotation server.                         ;;;
;;; 3. Wais support (Gateway or native?)                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)	    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'w3-vars)

(if (not (fboundp 'gopher-dispatch-object))
    (progn
      (autoload 'gopher-dispatch-object "gopher"    "Fetch gopher dir" t)))

(if (not (fboundp 'html-mode))
    (autoload 'html-mode "html-mode" "Edit HTML docs" t))

(if (not noninteractive) (progn
			   (or (featurep 'efs)
			       (featurep 'efs-auto)
			       (require 'ange-ftp))
			   (require 'nntp)))

(defun w3-load-flavors ()
  "Load the correct zone/font info for each flavor of emacs"
  (cond
   (w3-running-new-lucid (require 'w3-new-lucid))
   (w3-running-lemacs (require 'w3-lucid))
   (w3-running-epoch  (require 'w3-epoch))
   (w3-running-FSF19  (require 'w3-emacs19))
   (t                 (require 'w3-emacs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Automatic bug submission.                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-submit-bug ()
  "Function to submit a bug to the programs maintainer"
  (interactive)
  (cond
   ((and w3-mutable-windows (fboundp w3-mail-other-window-command))
    (funcall w3-mail-other-window-command))
   ((fboundp w3-mail-command)
    (funcall w3-mail-command))
   (w3-mutable-windows (mail-other-window))
   (t (mail)))
  (mail-to)
  (insert "wmperry@indiana.edu")
  (mail-subject)
  (insert "Bug found in w3-mode")
  (re-search-forward mail-header-separator nil t)
  (next-line 1)
  (while (< (current-column) 29) (insert "-"))
  (insert "Description of System:")
  (while (< (current-column) 75) (insert "-"))
  (insert "\n")
  (string-match "WWW \\([^ ]*\\) \\(.*\\)" w3-version)
  (insert "WWW Browser Version: "
	  (substring w3-version (match-beginning 1) (match-end 1))
	  ", of "
	  (substring w3-version (match-beginning 2) (match-end 2))
	  "\n"
	  "      Emacs Version: "
	  (substring (emacs-version) 0 (string-match " of" (emacs-version)))
	  (if w3-running-epoch "(Epoch)" "")
	  "\n"
	  "        System Type: "
	  (prin1-to-string system-type) "\n")
  (while (< (current-column) 29) (insert "-"))
  (insert "Description of Problem:")
  (while (< (current-column) 75) (insert "-"))
  (insert "\n\n"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for searching						    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-nuke-spaces-in-search (x)
  "Remove spaces from search strings . . ."
  (let ((new ""))
    (while (not (equal x ""))
      (setq new (concat new (if (= (string-to-char x) 32) "+" 
			      (substring x 0 1)))
	    x (substring x 1 nil)))
    new))

(defun w3-search ()
  "Perform a search, if this is a searchable index."
  (interactive)
  (cond
   ((not w3-current-isindex) (message "Not a searchable index!"))
   ((not (equal w3-current-type "http"))
    (message "Sorry, searching is not implemented on local files yet."))
   (t
    (let ((querystring (w3-nuke-spaces-in-search
			(read-string "Search on (+ separates keywords): "))))
      (w3-fetch (concat (w3-view-url t)
			(if (= ?? (string-to-char (substring w3-current-file
							     -1 nil))) "" "?")
			querystring))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto documentation, etc                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-help ()
  "Print documentation on w3 mode."
  (interactive)
  (let* ((funcs w3-doc-functions)
	 (funcstr "")
	 (vars w3-doc-variables)
	 (varstr "")
	 (keys nil))
    (while funcs
      (if (fboundp (car funcs))
	  (setq keys (where-is-internal (car funcs) w3-mode-map t)
		funcstr (format "%s\n<LI>%5s: %s" funcstr
				(if (null keys)
				    (format "M-x %s" (car funcs))
				  (if w3-running-FSF19
				      (key-description (car keys))
				    (key-description keys)))
				(documentation (car funcs)))))
      (setq funcs (cdr funcs)))
    (while vars
      (if (boundp (car vars))
	  (let* ((thevar (prin1-to-string (car vars)))
		 (doc (documentation-property (car vars)
					      'variable-documentation)))
	    (setq varstr
		  (format "%s\n<LI>%20s: %s\n" varstr thevar
			  (if (> (+ (length thevar) (length doc)) 80)
			      (concat "\n" doc)
			    doc)))))
      (setq vars (cdr vars)))
    (set-buffer (get-buffer-create w3-working-buffer))
    (if buffer-read-only (toggle-read-only))
    (erase-buffer)
    (insert (format "<TITLE>Help For W3 V%s</TITLE>\n" w3-version-number))
    (insert "<H1>Current keybindings:</H1>\n<HR><P>\n"
	    "<UL>" funcstr "</UL>")
    (insert "<P><H1>Modifiable variables:</H1>\n<HR><P>\n"
	    "<UL>" varstr "</UL>")
    (setq w3-current-type "www"
	  w3-current-file "help.html")	  
    (goto-char (point-min))
    (w3-sentinel nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remove . & .. from path names                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-remove-relative-links-helper (name)
  (cond
   ((string-match "\\\.\\\./" name)
    (let ((tmp (substring name (match-end 0) nil)))
      (if (= 0 (match-beginning 0))
	  (concat (w3-basepath (w3-basepath w3-current-file)) "/" tmp)
	(concat (w3-basepath (substring name 0
					(1- (match-beginning 0)))) "/" tmp))))
   ((string-match "\\\./" name)
    (if (= 0 (match-beginning 0))
	(substring name 2 nil)
      (concat
       (substring name 0 (match-beginning 0))
       (substring name (match-end 0) nil))))))

(defun w3-remove-relative-links (name)
  (while (string-match "\\\.+/" name)
    (setq name (w3-remove-relative-links-helper name)))
  name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definition, etc.                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-version ()
  "Show the version # of W3 in the minibuffer"
  (interactive)
  (message w3-version))

(defun w3 ()
  "Start a w3 session.  Goes to w3-default-homepage as home page."
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (not (string-match ".*:.*" w3-default-homepage))
      (w3-fetch (concat "file:" w3-default-homepage))
    (w3-fetch w3-default-homepage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definition							    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-reload-document ()
  "Reload the current document"
  (interactive)
  (let ((tmp (w3-view-url t))
	(pnt (point)))
    (kill-buffer (current-buffer))
    (w3-fetch tmp)
    (goto-char pnt)))

(defun w3-leave-buffer ()
  "Bury this buffer,but don't kill it"
  (interactive)
  (let ((x w3-current-last-buffer))
    (if (and w3-running-FSF19
	     (or (eq window-system 'x)
		 (eq window-system 'pm)))
	(set-variable 'lucid-menu-bar-dirty-flag t))
    (bury-buffer (current-buffer))
    (if (and (bufferp x) (buffer-name x))
	(if w3-mutable-windows (pop-to-buffer x) (switch-to-buffer x)))))  

(defun w3-quit ()
  "Quit WWW mode"
  (interactive)
  (let ((x w3-current-last-buffer))
    (if (and w3-running-FSF19
	     (or (eq window-system 'x)
		 (eq window-system 'pm)))
	(set-variable 'lucid-menu-bar-dirty-flag t))
    (kill-buffer (current-buffer))
    (if (and (bufferp x) (buffer-name x))
	(if w3-mutable-windows (pop-to-buffer x) (switch-to-buffer x)))))
  
(defun w3-view-url (&optional no-show)
  "View the current document's URL"
  (interactive)
  (let ((url ""))
    (cond
     ((equal w3-current-type "gopher")
      (setq url (format "%s://%s%s/%s"
			w3-current-type w3-current-server
			(if (string= "70" w3-current-port) ""
			  (concat ":" w3-current-port))
			w3-current-file)))
     ((equal w3-current-type "http")
      (setq url (format  "%s://%s%s/%s" w3-current-type w3-current-server
			 (if (string= "80" w3-current-port) ""
			   (concat ":" w3-current-port))
			 (if (= ?/ (string-to-char w3-current-file))
			     (substring w3-current-file 1 nil)
			   w3-current-file))))
     ((equal w3-current-type "ftp")
      (setq url (format "%s://%s/%s" w3-current-type w3-current-server 
			(if (= 47 (string-to-char w3-current-file))
			    (substring w3-current-file 1 nil)
			  w3-current-file))))
     ((equal w3-current-type nil)
      (setq url (format "file:%s" w3-current-file)))
     ((equal w3-current-type "www")
      (setq url nil)))
    (if (not no-show) (message url) url)))
    
(defun w3-save-url (under-pt)
  "Save current url in the kill ring"
  (interactive "P")
  (let ((x (if under-pt (w3-view-this-url t) (w3-view-url t))))
    (setq kill-ring (cons x kill-ring))
    (setq kill-ring-yank-pointer kill-ring)
    (if (fboundp 'w3-store-in-x-clipboard) (w3-store-in-x-clipboard x))))

(defun w3-end-of-document ()
  "Go to end of document"
  (interactive)
  (goto-char (point-max)))

(defun w3-start-of-document ()
  "Go to start of document"
  (interactive)
  (goto-char (point-min)))

(defun w3-mail-to-author ()
  "Send mail to the author of this document, if possible."
  (interactive)
  (let ((x (assoc "made" w3-current-links)))
    (if x
	(w3-fetch (cdr x))
      (error "Cannot find the 'made' link for this document, sorry."))))

(defun w3-do-setup ()
  "Do setup - this is to avoid conflict with user settings when W3 is
dumped with emacs."
  ; Read in the ~/.w3 file if it exists - could set up some of these
  ; defaults.  This file is where I will store configuration information
  ; once I write the auto-editing of variables/info, etc.
  (if (file-exists-p (expand-file-name "~/.w3"))
      (load-file (expand-file-name "~/.w3")))

  ; Set the password entry funtion based on user defaults or guess
  ; based on which remote-file-access package they are using.
  (cond
   (w3-passwd-entry-func nil)		; Already been set
   ((or (featurep 'efs)			; Using EFS
	(featurep 'efs-auto))		; or autoloading efs
    (setq w3-passwd-entry-func 'efs-read-passwd))
   ((featurep 'ange-ftp)		; Using ange-ftp
    (setq w3-passwd-entry-func 'ange-ftp-read-passwd))
   (t (error "You must use either ange-ftp or efs!")))

  ; Read in extra mime types that may not be defined in w3-mime-viewers
  (if (file-exists-p (expand-file-name "~/.mailcap")) (w3-parse-mailcap))
  
  ; Load in the hotlist if they haven't set it already
  (or w3-hotlist (w3-parse-hotlist))
  
  ; Load in their personal annotations if they haven't set them already
  (or w3-personal-annotations (w3-parse-personal-annotations))
  
  ; Create the fonts, etc in windowing systems
  (w3-create-faces)
  
  ; Set up the news service if they haven't done so
  (or w3-news-server (setq w3-news-server  (or (getenv "NNTPSERVER")
					       "news")))
  
  ; Set the default home page, honoring their defaults, then
  ; the standard WWW_HOME, then default to the documentation @ IU
  (or w3-default-homepage
      (setq w3-default-homepage
	    (or (getenv "WWW_HOME")
		"http://cs.indiana.edu/elisp/w3/docs.html")))
  
  ; Set up the MIME accept string if they haven't got it hardcoded yet
  (or w3-mime-accept-string
      (setq w3-mime-accept-string (w3-parse-viewer-types)))
  
  ; This isn't used yet, but just in case we ever need it for the
  ; graphics parsing routines - perhaps use this value to determine
  ; value for w3-max-colors?
  (or w3-color-planes (setq w3-color-planes
			    (cond
			     (w3-running-lemacs (x-display-planes))
			     ((and w3-running-FSF19
				   (or (eq window-system 'x)
				       (eq window-system 'pm)))
			      (x-display-planes))
;			     ((and w3-running-FSF19
;				   (eq window-system 'dps))
;			      (dps-display-color-cells))
			     (w3-running-epoch 8)
			     (t nil))))
  
  ; This isn't used yet, but just in case we ever need it for the
  ; graphics parsing routines - perhaps use this value to determine
  ; value for w3-max-colors?
  (or w3-color-display (setq w3-color-display
			     (cond
			      (w3-running-lemacs (x-color-display-p))
			      ((and w3-running-FSF19
				    (or (eq window-system 'x)
					(eq window-system 'pm)))
			       (x-display-color-p))
;			      ((and w3-running-FSF19
;				    (eq window-system 'dps))
;			       (dps-display-color-p))
			      (w3-running-epoch t)
			      (t nil))))
  
  ; Set up the regular expression used to find styles.
  (or w3-style-regexp (setq w3-style-regexp
			    (concat "<\\("
				    (mapconcat 'car w3-style-assoc "\\|")
				    "\\)>")))
  (w3-setup-version-specifics)
  (setq w3-setup-done t))

(defun w3-mode ()
  "Mode for viewing HTML documents.  Will try to bring up the document
specified by w3-default-homepage.
Current keymap is:
\\{w3-mode-map}"
  (interactive)
  (or w3-setup-done (w3-do-setup))
  (let ((tmp (list w3-current-file w3-current-server
		   w3-current-type w3-current-port
		   w3-current-isindex
		   w3-zones-list
		   w3-current-last-buffer
		   w3-current-mime-viewer
		   w3-current-mime-headers
		   w3-current-mime-type
		   w3-current-mime-encoding
		   w3-current-links
		   w3-delayed-images
		   w3-current-next-link
		   w3-current-last-link))) ; keep some variables
    (kill-all-local-variables)
    (use-local-map w3-mode-map)
    (setq major-mode 'w3-mode)
    (setq mode-name "WWW")
    (run-hooks 'w3-mode-hooks)
    (setq				; restore buffer-local variables
     w3-current-file (nth 0 tmp)
     w3-current-server (nth 1 tmp)
     w3-current-type (nth 2 tmp)
     w3-current-port (nth 3 tmp)
     w3-current-isindex (nth 4 tmp)
     w3-zones-list (nth 5 tmp)
     w3-current-last-buffer (nth 6 tmp)
     w3-current-mime-viewer (nth 7 tmp)
     w3-current-mime-headers (nth 8 tmp)
     w3-current-mime-type (nth 9 tmp)
     w3-current-mime-encoding (nth 10 tmp)
     w3-current-links (nth 11 tmp)
     w3-delayed-images (nth 12 tmp)
     w3-current-next-link (nth 13 tmp)
     w3-current-last-link (nth 14 tmp)
     )
    (if w3-running-epoch (use-local-mouse-map w3-mouse-map))
    (if (boundp 'w3-NeXT-mousemap) (use-local-mousemap w3-NeXT-mousemap))
    (if (and w3-running-FSF19 (or (eq window-system 'x)
				  (eq window-system 'pm)))
	(w3-build-FSF19-menu))
    (if (or w3-running-lemacs
	    w3-running-new-lucid)
	(progn
	  (w3-build-lemacs-menu)
	  (if w3-track-mouse (setq mode-motion-hook 'w3-mouse-handler))
	  (add-hook 'activate-menubar-hook 'w3-add-hotlist-menu)))
    (if (and w3-current-isindex (equal w3-current-type "http"))
	(setq mode-line-process "-Searchable"))))

(or (fboundp 'add-hook)
    (defun add-hook (hook-var function &optional at-end)
      "Add a function to a hook.
First argument HOOK-VAR (a symbol) is the name of a hook, second
 argument FUNCTION is the function to add.
Third (optional) argument AT-END means to add the function at the end
 of the hook list instead of the beginning.  If the function is already
 present, this has no effect.
Returns nil if FUNCTION was already present in HOOK-VAR, else new
 value of HOOK-VAR."
      (if (not (boundp hook-var)) (set hook-var nil))
      (let ((old (symbol-value hook-var)))
	(if (or (not (listp old)) (eq (car old) 'lambda))
	    (setq old (list old)))
	(if (w3-member function old)
	    nil
	  (set hook-var
	       (if at-end
		   (append old (list function)) ; don't nconc
		 (cons function old)))))))

(if (not (featurep 'w3)) (run-hooks 'w3-load-hooks))

(provide 'w3)

(require 'w3-url)
(require 'w3-html+)
(require 'w3-mime)
(require 'w3-misc)
(require 'w3-print)
(require 'w3-parse)
(require 'w3-lists)
(require 'w3-mosaic)
(require 'w3-gopher)
(require 'w3-forms)
(require 'w3-typecheck)
(require 'w3-viewers)
(require 'w3-auth)

(w3-load-flavors)

;;;###autoload (autoload 'w3-open-local "w3" "Open local file for WWW browsing" t)
;;;###autoload (autoload 'w3-fetch "w3" "Open remote file for WWW browsing" t)
;;;###autoload (autoload 'w3-use-hotlist "w3" "Use shortcuts to WWW Docs" t)
;;;###autoload (autoload 'w3-preview-this-buffer "w3" "Parse this buffer as HTML" t)
;;;###autoload (autoload 'w3-batch-fetch "w3" "Batch fetch urls")
;;;###autoload (autoload 'w3-retrieve "w3" "Fetch urls")
