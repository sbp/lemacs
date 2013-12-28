;;; -*- mode: emacs-lisp -*-
;;; Newsgroups: gnu.emacs.gnus
;;; From: jrs@world.std.com (Rick Sladkey)
;;; Subject: gnus-tree.el -- thread structure lines for GNUS
;;; Organization: The Internet
;;; Date: Tue, 6 Apr 1993 03:38:03 GMT
;;; 
;;; I had an idea to use the empty indentation space that GNUS uses to
;;; display article threads for displaying thread structure lines.
;;; 
;;; Here is a sample from my *Summary* buffer:
;;; 
;;; D 3306: [Benjamin R. Liblit      ] expiring kill patterns
;;; D  \_ 3307: [Daniel P. Faigin        ]
;;; D      \_ 3309: [Benjamin R. Liblit      ]
;;; D      |   \_ 3308: [Daniel P. Faigin        ]
;;; D      |       \_ 3312: [Benjamin R. Liblit      ]
;;; D      \_ 3310: [Larry Hunter            ]
;;; D      |   \_ 3313: [k p c                   ]
;;; D      |   |   \_ 3321: [Benjamin R. Liblit      ]
;;; D      |   |   \_ 3322: [Benjamin R. Liblit      ]
;;; D      |   |       \_ 3320: [Daniel P. Faigin        ]
;;; D      |   |       \_ 3323: [k p c                   ]
;;; D      |   \_ 3318: [Per Abrahamsen          ]
;;; D      \_ 3329: [Don Wells               ]
;;; D          \_ 3331: [Benjamin R. Liblit      ]
;;; 
;;; I use my *Summary* buffer highlighting package to "gray out" the
;;; thread lines so that they are still present but less obtrusive.  This
;;; provides subtle visual feedback about the structure of threads.
;;; 
;;; Here are the changed functions from UMEDA's latest GNUS 3.14.4 but
;;; they may work in older versions as well.  Only the first function
;;; (gnus-Subject-prepare-threads) is really changed.  All the others just
;;; replace [ \t]+ with [^0-9]* in the regexps.
;;; 
;;; Rick
;;; -----

;;; gnus-tree.el - tree notation for GNUS threads - rick sladkey

(provide 'gnus-tree)

(require 'gnus)

(defvar gnus-show-thread-lines t
  "*Show thread structure lines in Summary Mode if non-nil.")

(defun gnus-Subject-prepare-threads (threads level &optional parent-subject
					     leader)
  "Prepare Summary buffer from THREADS and indentation LEVEL.
THREADS is a list of `(PARENT [(CHILD1 [(GRANDCHILD ...]...) ...]).'
Optional PARENT-SUBJECT specifies the subject of the parent.
Option LEADER specifies the current indentation leader."
  (let ((thread nil)
	(header nil)
	(number nil)
	(subject nil)
	(child-subject nil)
	(parent-subject (or parent-subject ""))
	;; `M Indent NUM: [OPT] SUBJECT'
	(cntl (format "%%s %%s%%%dd: [%%s] %%s\n"
		      (length (prin1-to-string gnus-newsgroup-end)))))
    (while threads
      (setq thread (car threads))
      (setq threads (cdr threads))
      ;; If thread is a cons, hierarchical threads is given.
      ;; Otherwise, thread itself is header.
      (if (consp thread)
	  (setq header (car thread))
	(setq header thread))
      ;; Print valid header only.
      (if (vectorp header)		;Depends on nntp.el.
	  (progn
	    (setq number (nntp-header-number header))
	    (setq subject (nntp-header-subject header))
	    (setq child-subject (gnus-simplify-subject subject 're-only))
	    (insert
	     (format cntl
		     ;; Read or not.
		     (cond ((memq number gnus-newsgroup-marked)  "-")
			   ((memq number gnus-newsgroup-unreads) " ")
			   (t "D"))
		     ;; Thread level.
		     (if gnus-show-thread-lines
			 (if (zerop level) "" (concat leader " \\_ "))
		       (make-string (* level gnus-thread-indent-level) ? ))
		     ;; Article number.
		     number
		     ;; Optional headers.
		     (or (and gnus-optional-headers
			      (funcall gnus-optional-headers header)) "")
		     ;; Its subject string.
		     (concat (if (or (zerop level)
				     (not gnus-thread-hide-subject)
				     ;; Subject is different from the parent.
				     (not (string-equal
					   parent-subject child-subject)))
				 nil
			       (make-string (window-width) ? ))
			     subject)
		     ))
	    ))
      ;; Print subthreads.
      (and (consp thread)
	   (cdr thread)
	   (gnus-Subject-prepare-threads
	    (cdr thread) (1+ level) child-subject
	    (if (zerop level) ""
	      (concat leader (if threads " |  " "    ")))))
      )))

(defun gnus-Subject-subject-string ()
  "Return current subject string or nil if nothing."
  (save-excursion
    ;; It is possible to implement this function using
    ;;  `gnus-Subject-article-number' and `gnus-newsgroup-headers'.
    (beginning-of-line)
    ;; We have to take care of hidden lines.
    (if (looking-at
	 ".[^0-9]*[0-9]+:.\\[[^]\r\n]*\\][ \t]+\\([^\r\n]*\\)[\r\n]")
	(buffer-substring (match-beginning 1) (match-end 1)))
    ))

(defun gnus-Subject-article-number ()
  "Article number around point. If nothing, return current number."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ".[^0-9]*\\([0-9]+\\):")
	(string-to-int
	 (buffer-substring (match-beginning 1) (match-end 1)))
      ;; If search fail, return current article number.
      gnus-current-article
      )))

(defun gnus-Subject-goto-subject (article)
  "Move point to ARTICLE's subject."
  (interactive
   (list
    (string-to-int
     (completing-read "Article number: "
		      (mapcar
		       (function
			(lambda (headers)
			  (list
			   (int-to-string (nntp-header-number headers)))))
		       gnus-newsgroup-headers)
		      nil 'require-match))))
  (let ((current (point)))
    (goto-char (point-min))
    (or (and article (re-search-forward (format "^.[^0-9]*%d:" article) nil t))
	(progn (goto-char current) nil))
    ))

(defun gnus-Subject-search-subject (backward unread subject)
  "Search for article forward.
If 1st argument BACKWARD is non-nil, search backward.
If 2nd argument UNREAD is non-nil, only unread article is selected.
If 3rd argument SUBJECT is non-nil, the article which has
the same subject will be searched for."
  (let ((func
	 (if backward
	     (function re-search-backward) (function re-search-forward)))
	(article nil)
	;; We have to take care of hidden lines.
	(regexp 
	 (format "^%s[^0-9]*\\([0-9]+\\):.\\[[^]\r\n]*\\][ \t]+%s"
		 ;;(if unread " " ".")
		 (cond ((eq unread t) " ") (unread "[ ---]") (t "."))
		 (if subject
		     (concat "\\([Rr][Ee]:[ \t]+\\)*"
			     (regexp-quote (gnus-simplify-subject subject))
			     ;; Ignore words in parentheses.
			     "\\([ \t]*([^\r\n]*)\\)*[ \t]*\\(\r\\|$\\)")
		   "")
		 )))
    (if backward
	(beginning-of-line)
      (end-of-line))
    (if (funcall func regexp nil t)
	(setq article
	      (string-to-int
	       (buffer-substring (match-beginning 1) (match-end 1)))))
    ;; Adjust cursor point.
    (beginning-of-line)
    (search-forward ":" nil t)
    ;; This is the result.
    article
    ))

(defun gnus-Subject-set-current-mark (&optional current-mark)
  "Put `+' at the current article.
Optional argument specifies CURRENT-MARK instead of `+'."
  (save-excursion
    (set-buffer gnus-Subject-buffer)
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      ;; First of all clear mark at last article.
      (if (re-search-forward "^.[^0-9]*[0-9]+:[^ \t]" nil t)
	  (progn
	    (delete-char -1)
	    (insert " ")
	    (goto-char (point-min))))
      (if (re-search-forward (format "^.[^0-9]*%d:" gnus-current-article) nil t)
	  (progn
	    (delete-char 1)
	    (insert (or current-mark "+"))))
      )))

(defun gnus-Subject-first-unread-article ()
  "Select first unread article. Return non-nil if successfully selected."
  (interactive)
  (let ((begin (point)))
    (goto-char (point-min))
    (if (re-search-forward "^ [^0-9]*[0-9]+:" nil t)
	(gnus-Subject-display-article (gnus-Subject-article-number))
      ;; If there is no unread articles, stay there.
      (goto-char begin)
      ;;(gnus-Subject-display-article (gnus-Subject-article-number))
      (message "No more unread articles")
      nil
      )
    ))
