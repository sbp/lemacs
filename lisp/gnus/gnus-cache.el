;;; gnus-cache.el - caching extensions to GNUS                 -*-Emacs-Lisp-*-
;;; Copyright (C) 1993 Rick Sladkey <jrs@world.std.com>

;; This file is not part of GNUS but is distributed under
;; the same conditions as GNUS.

;; GNUS Cache is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNUS Cache is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.


;; LCD Archive Entry:
;; gnus-cache|Rick Sladkey|jrs@world.std.com|
;; GNUS speed-up by caching NNTP article headers to disk|
;; 3-Jan-1994|1.8|~/misc/gnus-cache.el.Z|

(defconst gnus-cache-version "GNUS Cache v1.8 - 94-01-03")


;; Overview:

;; This add-on package for GNUS is meant to dramatically speed-up
;; the process of reading netnews using GNUS over NNTP.  It works
;; by caching unread message headers into files.  It is most useful
;; if you have a fast local system with some spare disk space but a
;; slow NNTP server or a slow link to your NNTP server.  It works
;; with either the old GNUS (<= 3.14.4) or the new GNUS (>= 3.15).

;; Never again catch up on a newsgroup just because you dread
;; re-entering it with the same 200 articles unread...

;; Never again leave your NNTP connection open for hours on end
;; just because downloading the active file takes five minutes...

;; Never again wait forever downloading 400 headers from
;; a newsgroup that you just end up catching up on anyway...


;; Usage:

;; To use gnus-cache just put "gnus-cache.el" in your load-path
;; and byte-compile it and then put "(require 'gnus-cache)"
;; in your GNUS setup file or in your gnus-open-server-hook.
;; For GNUS version <= 3.14.4 use gnus-Open-server-hook instead.

;; GNUS Cache works best when you preload the cache with the headers
;; of newsgroups you read most often.  This can be done by either
;; running `gnus-batch-kill' (this kills two birds with one stone)
;; or by running `gnus-cache-preload-cache' after GNUS is started.
;; Using either method is convenient because all header retrieval
;; for all your newgroups happens at one time so you can start it
;; up and do something else until its done.  Then when you go to
;; read your news the subject summaries appear almost instantly.
;; Well, pretty fast, anyway.

;; By default, "G" is bound to `gnus-cache-get-new-news-and-preload-cache'
;; if it is not already defined.


;; Variables:

;; gnus-cache-enabled -- If this is false it should act like the regular NNTP.
;; If you want this to be false I recommend not using gnus-cache.
;; However, it might be useful if you use more than one NNTP server and
;; don't want to cache them all.
;; This is on by default.

;; gnus-cache-verbose -- Controls how chatty gnus-cache is.
;; This is on by default.

;; gnus-cache-save-active-file -- Controls if the active file is cached.
;; It is a `good thing' to cache the active file if you quit and restart
;; GNUS frequently and have a slow NNTP server and a large active file.
;; When the active file is cached you get the "old" active file when
;; you restart GNUS which is very fast.  Use "g" or "G" to re-read it.
;; This is on by default.

;; gnus-cache-save-all-headers -- Controls if deleted articles are cached.
;; You may want this turned on if you frequently reread already caught up
;; groups or if you are sharing the cache directory.  Needs lots of disk space.
;; It is turned off by default.

;; gnus-cache-saved-newsgroups-regexp -- Controls which newsgroups get cached.
;; If you want to cache only certain newsgroups, this is how.
;; If you only want to save the active file you can set this to nil.
;; The default of t caches all newsgroups you read or are subscribed to.

;; gnus-cache-kill-while-preloading -- Controls whether preloading also kills.
;; If you have a fast system and don't use `gnus-expunge' you will probably
;; want this leave this turned off.  However, if it is turned on, you may
;; want to also set gnus-apply-kill-hook (or gnus-Apply-kill-hook) to nil
;; so that articles are killed only a preload time.
;; By default, kill files are processed at read time, not preload time.

;; gnus-cache-apply-kill-hook -- Called to perform kills during preloading.
;; If gnus-cache-kill-while-preloading is t then this hook is called
;; during cache preloading.
;; The default value is gnus-apply-kill-file.

;; gnus-cache-saved-file-maximum-age -- Controls deletions from the cache dir.
;; If this is a number, it specified the number of days a cache file
;; may be kept.  If a cache file is older than this at server open time
;; it is deleted.  If it is nil, cache files are never deleted.
;; If you are using a shared cache directory you may want to turn this
;; off and do expiry from cron.
;; The default value is 7 days.

;; gnus-cache-directory-is-shared -- Controls how carefully files are written.
;; If true, then gnus-cache writes to saved files using temp files and
;; renaming instead of overwriting.  This prevents cache file corruption.
;; Also causes created directories to be chmod'ed to 777 and created files
;; to be chmod'ed to 644.
;; This is off by default.

;; gnus-cache-directory-template -- A format string for the cache directory.
;; This may contain a "%s" for the NNTP server name.
;; If the directory does not exist it is created automatically.
;; The default value is "~/.gnus-cache/%s".

;; gnus-cache-cache-file-name -- Extension to give cache files. See
;; gnus-cache-use-long-file-name.  The default value is "CACHE" but
;; using "CACHE.gz" with a compression package will buy you gzipped
;; cache files.  Idea from Richard Pieri.

;; gnus-cache-active-file-name -- Name to use for the saved active file.
;; The default it is "ACTIVE".

;; gnus-cache-use-long-file-name -- Controls the format of header cache files.
;; If this is turned on then message header cache files look like
;; "gnu.emacs.gnus.CACHE", otherwise like "gnu/emacs/gnus/CACHE".
;; The reason it is a different variable than gnus-use-long-file-name
;; is because you may want to use different values for each.
;; This defaults to value of gnus-use-long-file-name.

;; gnus-cache-setup-hook -- A hook for gnus-cache customization.
;; For example, you could set gnus-cache-directory from this hook.

;; gnus-cache-preloading-completed-hook -- A hook for doing something
;; when preloading is finished.  By default this does nothing but
;; you could make gnus-cache beep at you by setting it to 'ding.


;; Caveats:

;; You can "rm -rf" the cache directory at any time and it will
;; not harm your ability to read news.  Only the speed...

;; Be aware that if you are subscribed to a lot of large newgroups
;; that you don't really read then gnus-cache will take much longer
;; and waste a lot of disk space.

;; If you have, for example, a 100k active file and an total of
;; 2000 unread messages in any number of groups, then gnus-cache
;; might use about 500k of disk space for caching.  If you are
;; more religious about keeping up with all your subscribed
;; groups it could be much less.



(require 'gnus)
(require 'nntp)

(defvar gnus-cache-enabled t
  "*True if gnus-cache should honor the values of
gnus-cache-save-active-file and gnus-cache-saved-newsgroups-regexp.")

(defvar gnus-cache-verbose t
  "*True if gnus-cache should be chatty when using cached files.")

(defvar gnus-cache-save-active-file t
  "*True if gnus-cache should save a copy of the active file
for faster restarts.  If this variable is true then you must
use `g' or `G' after starting GNUS to truly see the latest news.")

(defvar gnus-cache-save-all-headers nil
  "*True if gnus-cache should retain the headers for deleted articles.
You will probably only want to turn this on if the cache directory is shared.")

(defvar gnus-cache-saved-newsgroups-regexp t
  "*A regular expression matching the names of all newsgroups
whose headers you want saved for faster access.  Use t to match
all newsgroups or nil to not match any.")

(defvar gnus-cache-kill-while-preloading nil
  "*True if gnus-cache should apply kill files when performing
`gnus-cache-preload-cache'.  Leave this off if you like to
see killed articles in newsgroup summaries.  That is, if
you don't use `gnus-expunge' at the end of your kill files.")

(defvar gnus-cache-apply-kill-hook 'gnus-apply-kill-file
  "*A hook called to perform kill processing on the current
newsgroup during cache preloading if gnus-cache-kill-while-preloading
is t.  The default value is gnus-apply-kill-file.")

(defvar gnus-cache-saved-file-maximum-age 7
  "*Maximum age of files in the cache directory before they are
deleted.  The default value is 7 days.")

(defvar gnus-cache-directory-is-shared nil
  "*True if gnus-cache should use temp files and renaming when
writing to files in the cache directory.")

(defvar gnus-cache-directory-template "~/.gnus-cache/%s"
  "*Format string used to determine the name of the directory
that cache files are kept in for a given NNTP server.
A \"%s\" is substituted with the server's name.
The default value keeps the cache files in a directory
called \"~/.gnus-cache/SERVER\".  It should be changed if
the cache directory is shared.")

(defvar gnus-cache-cache-file-name "CACHE"
  "*File name of a CACHE file. See \"gnus-cache-use-long-file-name\".")

(defvar gnus-cache-active-file-name "ACTIVE"
  "*File name of the ACTIVE file.")

(defvar gnus-cache-use-long-file-name gnus-use-long-file-name
  "*True if cache files for saved article headers should use names
like gnu.emacs.gnus.CACHE or gnu/emacs/gnus/CACHE.")

(defvar gnus-cache-setup-hook nil
  "*Hooks to run after setting up the gnus-cache directory.
You may set gnus-cache variables such as gnus-cache-directory here.")

(defvar gnus-cache-preloading-completed-hook nil
  "*Hooks to run after preloading is completed.  Useful for doing
attention-grabbing things.  Use your imagination.")



;; Current newsgroup for NNTP.
(defvar gnus-cache-current-newsgroup nil)

;; Minimum active article in current newsgroup.
(defvar gnus-cache-current-min-article nil)

;; True if current group command has been issued to NNTP server.
(defvar gnus-cache-newsgroup-requested nil)

;;True if nntp-request-list has been requested once already.
(defvar gnus-cache-request-list-requested nil)

;; Name of directory holding cached header information from NNTP server.
(defvar gnus-cache-directory nil)

;; List of functions from the nntp package that get overloaded by gnus-cache.
(defconst gnus-cache-overloaded-nntp-functions
  '(nntp-retrieve-headers nntp-request-list nntp-open-server nntp-request-group
    nntp-request-article nntp-request-body nntp-request-head
    nntp-request-last nntp-request-next nntp-request-stat))

;; Are we running the old GNUS package?
(defvar gnus-cache-old-gnus (fboundp 'gnus-Group-startup-message))

(defvar gnus-cache-buffer nil)

;; Do the overloading.
(let ((list gnus-cache-overloaded-nntp-functions)
      (old-function nil)
      (saved-function nil)
      (new-function nil))
  (while list
    (setq old-function (car list)
	  list (cdr list)
	  saved-function (intern (concat "gnus-cache-"
					 (symbol-name old-function)
					 "-original"))
	  new-function (intern (concat "gnus-cache-"
				       (symbol-name old-function))))
    (if (and (fboundp old-function) (not (fboundp saved-function)))
	(progn
	  (fset saved-function (symbol-function old-function))
	  (fset old-function new-function)))))

;; Politely install ourselves in the group mode map.
(if gnus-cache-old-gnus
    (let ((old-command (lookup-key gnus-Group-mode-map "G")))
      (and (or (not old-command)
	       (eq old-command 'undefined))
	   (define-key gnus-Group-mode-map "G"
	     'gnus-cache-get-new-news-and-preload-cache)))
  (let ((old-command (lookup-key gnus-group-mode-map "G")))
    (and (or (not old-command)
	     (eq old-command 'undefined))
	 (define-key gnus-group-mode-map "G"
	   'gnus-cache-get-new-news-and-preload-cache))))



(defun gnus-cache-get-new-news-and-preload-cache ()
  "Reread the active file and preload the cache of GNUS headers."
  (interactive)
  (if gnus-cache-old-gnus
      (gnus-Group-get-new-news)
    (gnus-group-get-new-news))
  (gnus-cache-preload-cache))

(defun gnus-cache-preload-cache (&optional options)
  ;; This function is based almost entirely on gnus-batch-kill by UMEDA.
  "Preload the cache of GNUS headers.   Optional argument OPTIONS
is a newsrc-style options line describing which newsgroups to preload.
In any case, only newsgroups matching gnus-cache-saved-newsgroup-regexp
are preloaded."
  (interactive "sOptions: (default all) ")
  (if (or (not options) (string-equal options ""))
      (setq options "all"))
  (let* ((last-group (if gnus-cache-old-gnus
			 (gnus-Group-group-name)
		       (gnus-group-group-name)))
	 (group nil)
	 (subscribed nil)
	 (newsrc nil)
	 (yes-and-no (gnus-parse-n-options options))
	 (yes (car yes-and-no))
	 (no  (cdr yes-and-no))
	 ;; Disable verbose message.
	 (gnus-novice-user nil)
	 (gnus-large-newsgroup nil)
	 (nntp-large-newsgroup nil))
    (save-window-excursion
      (setq newsrc (copy-sequence gnus-newsrc-assoc))
      (while newsrc
	(setq group (car (car newsrc)))
	(setq subscribed (nth 1 (car newsrc)))
	(setq newsrc (cdr newsrc))
	(if (and subscribed

		 (gnus-cache-saved-newsgroup-p group)
		 (not (zerop (nth 1 (gnus-gethash group gnus-unread-hashtb))))
		 (if yes
		     (string-match yes group) t)
		 (or (null no)
		     (not (string-match no group))))
	    (progn
	      (message "Preloading %s..." group)
	      (if gnus-cache-old-gnus
		  (if gnus-cache-kill-while-preloading
		      (let ((gnus-Apply-kill-hook
			     (or gnus-cache-apply-kill-hook
				 gnus-Apply-kill-hook)))
			(gnus-Subject-read-group group nil t)
			(if (eq (current-buffer)
				(get-buffer gnus-Subject-buffer))
			    (gnus-Subject-exit t)))
		    (gnus-select-newsgroup group nil)
		    (gnus-Subject-exit t))
		(if gnus-cache-kill-while-preloading
		    (let ((gnus-apply-kill-hook
			   (or gnus-cache-apply-kill-hook
			       gnus-apply-kill-hook)))
		      (gnus-summary-read-group group nil t)
		      (if (eq (current-buffer)
			      (get-buffer gnus-summary-buffer))
			  (gnus-summary-exit t)))
		  (gnus-select-newsgroup group nil)
		  (gnus-summary-exit t)))))))
    (or (and last-group
	     (if gnus-cache-old-gnus
		 (gnus-Group-jump-to-group last-group)
	       (gnus-group-jump-to-group last-group)))
	(progn
	  (beginning-of-line)
	  (search-forward ":" nil t)))
    (message "Preloading... done")
    (run-hooks 'gnus-cache-preloading-completed-hook)))
  
(defun gnus-cache-saved-newsgroup-p (group)
  ;; Should this newsgroup get cached?
  (if (stringp gnus-cache-saved-newsgroups-regexp)
      (string-match gnus-cache-saved-newsgroups-regexp group)
    gnus-cache-saved-newsgroups-regexp))

(defun gnus-cache-nntp-open-server (host &optional service)
  ;; Must do our package setup at open server time.
  (gnus-cache-setup host)
  (gnus-cache-nntp-open-server-original host service))

(defun gnus-cache-setup (host)
  ;; Set up the cache directory name and some variables,
  ;; run our setup hook, and clean out the cache directory.
  (if gnus-cache-enabled
      (progn
	(setq gnus-cache-buffer (get-buffer-create " *gnus-cache*"))
	(setq gnus-cache-directory
	      (directory-file-name
	       (expand-file-name
		(format gnus-cache-directory-template host))))
	(setq gnus-cache-request-list-requested nil)
	(run-hooks 'gnus-cache-setup-hook)
	(if (file-directory-p gnus-cache-directory)
	    (and gnus-cache-saved-file-maximum-age
		 (gnus-cache-expire-saved-files))
	  (gnus-cache-make-directory gnus-cache-directory)))))

(defun gnus-cache-expire-saved-files ()
  ;; Delete all old files in the cache directory.
  (let* ((default-directory gnus-cache-directory)
	 (cache-files
	  (if gnus-cache-use-long-file-name
	      (directory-files gnus-cache-directory nil "\\.CACHE$")
	    (gnus-cache-recursive-files-of gnus-cache-directory "/CACHE$")))
	 (expired-cache-files
	  (gnus-cache-files-older-than
	   cache-files gnus-cache-saved-file-maximum-age)))
    (mapcar (if gnus-cache-verbose
		(function
		 (lambda (file)
		   (message "Deleting expired %s" file)
		   (delete-file file)))
	      (function delete-file))
	    expired-cache-files)))

(defun gnus-cache-recursive-files-of (dir-or-file regexp)
  ;; Create a list of all files in or below a directory matching a pattern.
  (if (file-directory-p dir-or-file)
      (apply (function nconc)
	     (mapcar (function
		      (lambda (dir-or-file)
			(gnus-cache-recursive-files-of dir-or-file regexp)))
		     (directory-files dir-or-file t "^[^.]")))
    (and (or (not regexp) (string-match regexp dir-or-file))
	 (list dir-or-file))))

(defun gnus-cache-make-directory (dir)
  ;; Makes leading directory components as necessary.
  ;; Only use existing make-directory if it is a subr.
  ;; Make dir mode 777 if the cache-directory is shared.
  (let ((parent (and (string-match "^.*/" dir)
		     (substring dir 0 (1- (match-end 0))))))
    (if (and parent (not (file-directory-p parent)))
	(gnus-cache-make-directory parent)))
  (prog1
      (if (and (fboundp 'make-directory)
	       (subrp (symbol-function 'make-directory)))
	  (make-directory dir)
	(eq (call-process "mkdir" nil nil nil dir) 0))
    (and gnus-cache-directory-is-shared
	 (set-file-modes dir 511))))	; 511 == 0777

(defun gnus-cache-files-older-than (files age)
  ;; For some versions of Emacs the age in days must be less than about 100.
  (setq age (* age 86400))
  (let* ((ms-age (/ age 65536))
	 (ls-age (- age (* ms-age 65536)))
	 (current-time (gnus-cache-current-time))
	 (ms-time (car current-time))
	 (ls-time (car (cdr current-time)))
	 (file nil)
	 (result nil))
    (while files
      (setq file (car files) files (cdr files))
      (let* ((file-time (nth 5 (file-attributes file)))
	     (ms-diff (- ms-time (car file-time)))
	     (ls-diff (- ls-time (if (numberp (cdr file-time))
				     (cdr file-time)
				   (car (cdr file-time))))))
	(and (< ls-diff 0)
	     (setq ls-diff (+ ls-diff 65536) ms-diff (1- ms-diff)))
	(and (or (> ms-diff ms-age)
		 (and (= ms-diff ms-age)
		      (> ls-diff ls-age)))
	     (setq result (cons file result)))))
    result))

(defun gnus-cache-current-time ()
  ;; Only use existing current-time if it is a subr (FSF 19).
  ;; Only use existing current-time-seconds if it is a subr (Lucid).
  ;; The temp file idea is due to Joe Wells.
  (cond
   ((and (fboundp 'current-time)
	 (subrp (symbol-function 'current-time)))
    (current-time))
   ((and (fboundp 'current-time-seconds)
	 (subrp (symbol-function 'current-time-seconds)))
    (let ((time (current-time-seconds)))
      (list (car time) (cdr time))))
   (t
    (let ((temp (make-temp-name gnus-cache-directory)))
      (unwind-protect
	  (progn
	    (write-region (point-min) (point-min) temp nil 'no-message)
	    (nth 5 (file-attributes temp)))
	(delete-file temp))))))



(defun gnus-cache-nntp-retrieve-headers (sequence)
  ;; If this is a newsgroup to be cached then retrieve headers using the cache.
  (if (and (not (stringp (car sequence)))
	   gnus-cache-enabled
	   (gnus-cache-saved-newsgroup-p gnus-cache-current-newsgroup))
      (gnus-cache-retrieve-headers-using-cache sequence)
    (gnus-cache-retrieve-headers-using-nntp sequence)))

(defun gnus-cache-retrieve-headers-using-cache (sequence)
  ;; This is the workhorse for the header caching feature.
  (save-excursion
    (set-buffer gnus-cache-buffer)
    (erase-buffer)
    (let* ((cache-dir gnus-cache-directory)
	   (cache-file (gnus-cache-saved-file-name
			gnus-cache-current-newsgroup))
	   (cached-headers (and (file-exists-p cache-file)
				(insert-file-contents cache-file)
				(read gnus-cache-buffer)))
	   (cached-sequence (mapcar (function (lambda (header)
						(nntp-header-number header)))
				    cached-headers))
	   (uncached-sequence (gnus-cache-sequence-difference sequence
							      cached-sequence))
	   (uncached-headers (and uncached-sequence
				  (gnus-cache-retrieve-headers-using-nntp
				   uncached-sequence)))
	   (headers (if uncached-sequence
			(gnus-cache-merge-headers sequence
						  cached-headers
						  uncached-headers)
		      cached-headers)))
      (and uncached-sequence
	   (progn
	     (erase-buffer)
	     (print headers gnus-cache-buffer)
	     (gnus-cache-write-file (point-min) (point-max) cache-file)))
      (gnus-cache-trim-headers sequence headers))))

(defun gnus-cache-saved-file-name (group)
  ;; Cache file names end with "CACHE" so that expiry won't accidentally
  ;; clobber important files.  Makes any leading directory components
  ;; if using short file names.
  (if gnus-cache-use-long-file-name
      (concat gnus-cache-directory "/" group "." gnus-cache-cache-file-name)
    (let ((path gnus-cache-directory))
      (while (string-match "\\." group)
	(setq path (concat path "/" (substring group 0 (1- (match-end 0))))
	      group (substring group (match-end 0))))
      (setq path (concat path "/" group))
      (or (file-directory-p path)
	  (gnus-cache-make-directory path))
      (concat path "/" gnus-cache-cache-file-name))))

(defun gnus-cache-sequence-difference (sequence cached-sequence)
  ;; Determine which headers are not in the cache and must be retrieved.
  (let ((uncached-sequence nil)
	(article nil)
	(cached-article nil))
  (while (and sequence cached-sequence)
    (setq article (car sequence) sequence (cdr sequence))
    (while (and cached-sequence
		(> article (setq cached-article (car cached-sequence))))
      (setq cached-sequence (cdr cached-sequence)))
    (if (or (not cached-sequence) (< article cached-article))
	(setq uncached-sequence (cons article uncached-sequence))))
  (nconc (nreverse uncached-sequence) sequence)))

(defun gnus-cache-merge-headers (sequence cached-headers uncached-headers)
  ;; Merge cached and uncached headers to create a superset of the
  ;; requested sequence.  The cached-headers list is modified.
  (let ((old-headers nil)
	(headers nil)
	(article nil))
    ;; Trim expired articles.
    (while (and cached-headers
		(< (nntp-header-number (car cached-headers))
		   gnus-cache-current-min-article))
      (setq cached-headers (cdr cached-headers)))
    ;; Avoid lots of consing for articles cached but not requested.
    (setq old-headers cached-headers
	  article (car sequence))
    (let ((last-cons nil))
      (while (and cached-headers
		  (< (nntp-header-number (car cached-headers)) article))
	(setq last-cons cached-headers
	      cached-headers (cdr cached-headers)))
      (if (eq old-headers cached-headers)
	  (setq old-headers nil)
	(setcdr last-cons nil)))
    ;; Note that cached and uncached sequences don't intersect
    ;; and that the uncached sequence is a subset of the requested sequence.
    (while sequence
      (setq article (car sequence)
	    sequence (cdr sequence))
      (while (and cached-headers
		  (< (nntp-header-number (car cached-headers)) article))
	(setq headers (cons (car cached-headers) headers)
	      cached-headers (cdr cached-headers)))
      (cond
       ((and cached-headers
	     (= (nntp-header-number (car cached-headers)) article))
	(setq headers (cons (car cached-headers) headers)
	      cached-headers (cdr cached-headers)))
       ((and uncached-headers
	     (= (nntp-header-number (car uncached-headers)) article))
	(setq headers (cons (car uncached-headers) headers)
	      uncached-headers (cdr uncached-headers)))
       (t
	(setq headers (cons (vector article) headers)))))
    (if gnus-cache-save-all-headers
	(nconc old-headers (nreverse headers))
      (nreverse headers))))

(defun gnus-cache-trim-headers (sequence headers)
  ;; Remove unrequested or expired headers by modifying list.
  ;; Requested sequence must be a subset of the headers.
  (let ((result (setq headers (cons nil headers)))
	(article (car sequence)))
    (while sequence
      (setq article (car sequence)
	    sequence (cdr sequence))
      (while (< (nntp-header-number (car (cdr headers))) article)
	(setcdr headers (cdr (cdr headers))))
      (if (= (length (car (cdr headers))) 1)
	  (setcdr headers (cdr (cdr headers)))
	(setq headers (cdr headers))))
    (setcdr headers nil)
    (cdr result)))

(defun gnus-cache-write-file (beg end file)
  ;; Maybe replace file instead of overwriting so cache dir can be shared.
  (if gnus-cache-directory-is-shared
      (let ((temp (make-temp-name file)))
	(write-region beg end temp nil 'no-message)
	(set-file-modes temp 420)	; 420 == 0644
	(rename-file temp file t))
    (write-region beg end file nil 'no-message)))

(defun gnus-cache-nntp-request-list ()
  ;; Read the active file, perhaps from the cache.
  (if (and gnus-cache-enabled
	   gnus-cache-save-active-file
	   (not noninteractive))
      (gnus-cache-request-list-using-cache)
    (gnus-cache-nntp-request-list-original)))

(defun gnus-cache-request-list-using-cache ()
  ;; Active file caching.
  (let ((cached-active-file
	 (concat gnus-cache-directory "/" gnus-cache-active-file-name))
	(result nil))
    (if (or gnus-cache-request-list-requested
	    (not (file-exists-p cached-active-file)))
	(if (setq result (gnus-cache-nntp-request-list-original))
	    (save-excursion
	      (set-buffer nntp-server-buffer)
	      (and gnus-cache-verbose
		   (message "Writing saved active file..."))
	      (gnus-cache-write-file (point-min) (point-max)
				     cached-active-file)))
      (save-excursion
	(and gnus-cache-verbose
	     (message "Reading saved active file..."))
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(setq result (insert-file-contents cached-active-file))))
    (setq gnus-cache-request-list-requested t)
    result))

(defun gnus-cache-nntp-request-group (group)
  ;; We defer group requests until the last minute for extra speed.
  (setq gnus-cache-current-newsgroup group
	gnus-cache-current-min-article 0
	gnus-cache-newsgroup-requested nil)
  t)

(defun gnus-cache-request-group-internal ()
  ;; Must be called when we are about the read an article or retrieve headers.
  (if gnus-cache-newsgroup-requested
      t
    (setq gnus-cache-newsgroup-requested t)
    (let* ((result (gnus-cache-nntp-request-group-original
		    gnus-cache-current-newsgroup))
	   (message (and result (nntp-status-message))))
      (and result
	   (stringp message)
	   (string-match "^[0-9]+[ \t]+\\([0-9]+\\)[ \t]+[0-9]+" message)
	   (setq gnus-cache-current-min-article
		 (string-to-int (substring message
					   (match-beginning 1)
					   (match-end 1)))))
      result)))

;; Several functions need to verify that the group is properly set.

(defun gnus-cache-retrieve-headers-using-nntp (sequence)
  (and
   (gnus-cache-request-group-internal)
   (gnus-cache-nntp-retrieve-headers-original sequence)))

(defun gnus-cache-nntp-request-article (id)
  (and
   (gnus-cache-request-group-internal)
   (gnus-cache-nntp-request-article-original id)))

(defun gnus-cache-nntp-request-body (id)
  (and
   (gnus-cache-request-group-internal)
   (gnus-cache-nntp-request-body-original id)))

(defun gnus-cache-nntp-request-head (id)
  (and
   (gnus-cache-request-group-internal)
   (gnus-cache-nntp-request-head-original id)))

(defun gnus-cache-nntp-request-stat (id)
  (and
   (gnus-cache-request-group-internal)
   (gnus-cache-nntp-request-stat-original id)))

(defun gnus-cache-nntp-request-last ()
  (and
   (gnus-cache-request-group-internal)
   (gnus-cache-nntp-request-last-original)))

(defun gnus-cache-nntp-request-next ()
  (and
   (gnus-cache-request-group-internal)
   (gnus-cache-nntp-request-next-original)))

(provide 'gnus-cache)

;;; gnus-cache.el ends here
