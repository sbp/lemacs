(defvar w3-allow-searching-of
  '("text/plain" "text/html" "text/x-setext"
    "application/x-troff-man" "application/x-troff-me"
    "application/x-troff-ms" "application/rtf"
    "text/richtext" "application/x-wais-source"
    "application/tex" "application/texinfo"
    "application/x-troff")
  "*A list of MIME content types that it is Ok for the automatic
search to descend to.")

(defun w3-do-search (term &optional base hops-limit restriction)
  "Recursively descend all the child links of the current document for TERM.
TERM may be a string, in which case it is treated as a regular expression,
and re-search-forward is used, or a symbol, in which case it is funcalled
with 1 argument, the current URL being searched.

BASE is the URL to start searching from.

HOPS-LIMIT is the maximum number of nodes to descend before they
search dies out.

RESTRICTION is a regular expression or function to call with one
argument, a URL that could be searched.  If RESTRICTION returns
non-nil, then the url is added to the queue, otherwise it is
discarded.  This is useful for restricting searching to either
certain tyes of URLs (only search ftp links), or restricting searching
to one domain (only search stuff in the indiana.edu domain).

For use in functions passed to w3-do-search:
QUEUE is the queue of links to be searched
HOPS is the current number of hops from the root document
RESULTS is an assoc list of (URL . RETVAL), where RETVAL is the value
returned from previous calls to the TERM function (or point if searching
for a regexp"
  (let ((x))
    (or base (setq base (w3-view-url t)))
    (if (setq x (w3-buffer-visiting base))
	(set-buffer x)
      (w3-fetch base))
    (w3-search-internal term hops-limit restriction)))

(defun w3-normalize-url (url)
  "Normalize a URL, removing all '#' references from it, etc."
  (cond
   ((null url) nil)
   ((string-match "#\\(.*\\)" url) (w3-match url 1))
   (t url)))
  
(defun w3-search-internal (term &optional hops-limit restriction)
  "Recursively descend all the child links of the current document for TERM.
TERM may be a string, in which case it is treated as a regular expression,
and re-search-forward is used, or a symbol, in which case it is funcalled
with 1 argument, the current URL being searched.

HOPS-LIMIT is the maximum number of nodes to descend before they
search dies out.

RESTRICTION is a regular expression or function to call with one
argument, a URL that could be searched.  If RESTRICTION returns
non-nil, then the url is added to the queue, otherwise it is
discarded.  This is useful for restricting searching to either
certain tyes of URLs (only search ftp links), or restricting searching
to one domain (only search stuff in the indiana.edu domain).

For use in functions passed to w3-do-search:
QUEUE is the queue of links to be searched
HOPS is the current number of hops from the root document
RESULTS is an assoc list of (URL . RETVAL), where RETVAL is the value
returned from previous calls to the TERM function (or point if searching
for a regexp"
  (setq hops-limit (or hops-limit 5))
  (let ((queue '())
	(visited '())
	(results nil)
	(hops 0))

    ;; Search initial page and stick it in the results list
    (goto-char (point-min))
    (cond
     ((stringp term)
      (setq results (cons (w3-view-url t) (re-search-forward term nil t))))
     ((symbolp term)
      (setq results (cons (w3-view-url t) (funcall term (w3-view-url t))))))

    ;; Build the initial queue of just the links on this page that are
    ;; deemed searchable
    (w3-map-links
     (function
      (lambda (x y)
	(if (and
	     (w3-member (nth 8 (w3-file-attributes (nth 2 x)))
			w3-allow-searching-of)
	     (cond
	      ((null (nth 2 x)) nil)
	      ((stringp restriction) (string-match restriction (nth 2 x)))
	      ((symbolp restriction) (funcall restriction (nth 2 x)))
	      (t t)))
	    (setq queue (nconc queue (list (w3-normalize-url (nth 2 x)))))))))

    (while queue
      (let ((x (car queue)) y)
	(setq visited (cons x visited))
	(if (setq y (w3-buffer-visiting x))
	    (set-buffer y)
	  (w3-retrieve x))
	(cond
	 ((equal (or w3-current-mime-type
		     (w3-extension-to-mime (w3-file-extension
					    w3-current-file))) "text/html")
	  (w3-prepare-buffer t)
	  (w3-map-links
	   (function
	    (lambda (link-data searching-func)
	      (let* ((url (w3-normalize-url (nth 2 link-data)))
		     (info (and
			    (cond
			     ((null url) nil)
			     ((stringp restriction)
			      (string-match restriction url))
			     ((symbolp restriction)
			      (funcall restriction url))
			     (t t))
			    (w3-file-attributes url)))
		     (num-children 0))
		(cond
		 ((null info)
		  (message "Skipping %s (not searchable)" url) nil)
		 ((w3-member (nth 8 info) w3-allow-searching-of)
		  (if (< hops hops-limit)
		      (w3-map-links	; Count the child links
		       (function	; and add them to the queue to 
			(lambda (lnk arg) ; be serviced
			  (setq num-children (1+ num-children))
			  (if (or
			       (w3-member url visited) 	; already seen it
			       (w3-member url queue)) 	; planning on seeing it
			      nil
			    (setq queue (nconc queue (list url))))))))
		  (goto-char (point-min))
		  (cond
		   ((stringp term)
		    (setq results (cons (cons url
					      (re-search-forward term nil t))
					results)))
		   ((symbolp term)
		    (setq results (cons (cons url (funcall term url))
					results)))
		   (t
		    (error "TERM must be a regular expression or symbol."))))
		 (t (message "Skipping %s (why?)" url))))))))
	 (t
	  (goto-char (point-min))
	  (cond
	   ((stringp term)
	    (setq results (cons (cons x (re-search-forward term nil t))
				results)))
	   ((symbolp term)
	    (setq results (cons (cons x (funcall term x)) results)))))))
      (setq queue (cdr queue)))
    results))
