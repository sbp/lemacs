;; -*- Mode: Emacs-Lisp -*-

;; From flee@cs.psu.edu Fri May 22 12:16:20 1992
;; Newsgroups: gnu.emacs.gnus,gnu.emacs.sources
;; From: flee@cs.psu.edu (Felix Lee)
;; Subject: Two GNUS speedups.
;; Organization: Penn State Computer Science
;; Date: Thu, 21 May 1992 04:45:32 GMT
;; 
;; I've posted these speedups before, but here they are all in one place,
;; and cleaned up a bit.  (Upgrading to GNUS 3.14 was an excuse to go
;; back and clean things up.)
;; 
;; gnus-speedups|Felix Lee|flee@cs.psu.edu
;; |Simple performance enhancements for GNUS
;; |1992-05-18|1.2: stable||

;; This package contains two plug-compatible performance enhancements
;; for GNUS 3.13 and 3.14:

;; 1. A faster version of gnus-find-new-newsgroups, for making GNUS
;; start more quickly.

;; 2. Faster subject and date sorting in the *Subject* buffer.

;; This file must be loaded after "gnus.el".  You can do this by
;; adding '(load "gnus-speedups") at the end of your local gnus
;; wrapper, or at the end of "gnus.el".  Or you could just include
;; this file directly.

(require 'gnus)

;;;;;
;; 1. A faster version of gnus-find-new-newsgroups.

;; We reduce an O(N**2) process to O(N) by building a hash table for
;; the list of known newsgroups.  (It's a little strange that GNUS
;; doesn't already have a hash table for this.)

(defun gnus-find-new-newsgroups ()
  "Look for new newsgroups and return names.
`-n' option of options line in .newsrc file is recognized."
  (let ( (group nil)
	 (new-groups nil)
	 (known-groups (gnus-make-hashtable)) )
    ;; Build a table of known newsgroups.
    (mapcar
     (function (lambda (group) (gnus-sethash (car group) t known-groups)))
     gnus-killed-assoc)
    (mapcar
     (function (lambda (group) (gnus-sethash (car group) t known-groups)))
     gnus-newsrc-assoc)
    ;; Compare the active file against what's known.
    (mapatoms
     (function
      (lambda (sym)
	(setq group (symbol-name sym))
	;; Take into account the -n option.
	(and (or (null gnus-newsrc-options-n-no)
		 (not (string-match gnus-newsrc-options-n-no group))
		 (and gnus-newsrc-options-n-yes
		      (string-match gnus-newsrc-options-n-yes group)))
	     (null (gnus-gethash group known-groups))
	     (setq new-groups (cons group new-groups)))
	))
     gnus-active-hashtb)
    new-groups
    ))


;;;;;
;; 2. Faster subject and date sorting in the *Subject* buffer.

;; The basic idea is explained in 'gnus-keyed-sort.

;; Sample hook usage:
;; (setq gnus-Select-group-hook
;;	 (function
;;	  (lambda ()
;;	    (gnus-keyed-sort-headers
;;	     (function gnus-string-lessp)
;;	     (function (lambda (it) (gnus-simplify-subject it 're-only))))
;;	    )))

(defun gnus-keyed-sort (list compare extract)
  "Sort LIST stably and return the sorted list.  Does not modify LIST.
Arguments are (LIST COMPARE EXTRACT).  Elements in the list are
compared as if the predicate were:
	(COMPARE (EXTRACT a) (EXTRACT b))
but EXTRACT is run over each element of the list in a preprocessing
stage for efficiency.  This reduces the number of EXTRACT calls from
O(N log N) to O(N).

Example: (gnus-keyed-sort load-path 'string< 'downcase)
"
  (let ( (keyed-list
	  (mapcar
	   (function (lambda (it) (cons (funcall extract it) it)))
	   list)) )
    (setq keyed-list
	  (sort keyed-list
		(function
		 (lambda (a b) (funcall compare (car a) (car b))))))
    (mapcar (function (lambda (it) (cdr it)))
	    keyed-list)
    ))

(defun gnus-keyed-sort-headers (compare extract)
  "Sort current group's headers by COMPARE and EXTRACT.  Sorting is
done as if the predicate were
	(COMPARE (EXTRACT a) (EXTRACT b))
See 'gnus-keyed-sort for details.
Note: interrupting the sort leaves the headers unsorted.
"
  (setq gnus-newsgroup-headers
	(gnus-keyed-sort
	 gnus-newsgroup-headers
	 compare extract)))

(defun gnus-Subject-keyed-sort-subjects (compare extract &optional reverse)
  "Sort and redisplay the *Subject* buffer by COMPARE and EXTRACT.
Calls 'gnus-keyed-sort-headers to do the sorting.  Optional argument
REVERSE means to do an 'nreverse after sorting.
"
  (let ( (current (gnus-Subject-article-number)) )
    (gnus-keyed-sort-headers compare extract)
    (if reverse
	(setq gnus-newsgroup-headers (nreverse gnus-newsgroup-headers)))
    (gnus-Subject-prepare)
    (gnus-Subject-goto-subject current)
    ))

;; XXX It should be 'gnus-sort-fold-case, not 'case-fold-search
(defun gnus-Subject-sort-by-subject (reverse)
  "Sort *Subject* buffer by subject alphabetically.  Argument REVERSE
means reverse order.  \"Re:\"s are ignored.  If 'case-fold-search, then
case of letters will be ignored.
"
  (interactive "P")
  ;; The main complication here is we try to speed up the sort process
  ;; by hoisting conditions outside the sort.
  (gnus-Subject-keyed-sort-subjects
   'string<
   (if case-fold-search
       (function
	(lambda (it)
	  (downcase (gnus-simplify-subject (nntp-header-subject it) 're-only))))
     (function
      (lambda (it)
	(gnus-simplify-subject (nntp-header-subject it) 're-only)))
     )
   reverse)
  )

;; For backward compatibility with GNUS 3.13
(if (not (fboundp 'gnus-sortable-date))
    (fset 'gnus-sortable-date 'gnus-comparable-date))

(defun gnus-Subject-sort-by-date (reverse)
  "Sort *Subject* buffer by posted date.  Argument REVERSE means
reverse order."
  (interactive "P")
  (gnus-Subject-keyed-sort-subjects
   'string<
   (function
    (lambda (it)
      (gnus-sortable-date (nntp-header-date it))))
   reverse)
  )
