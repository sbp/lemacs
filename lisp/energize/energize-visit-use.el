;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright © 1992-1993 by Lucid, Inc.  All Rights Reserved.

;; Load this file and ^X-\ (back-slash) gets bound to a
;; command that visit all the places where a language element is used.  
;; It can be started from the Le-Browser buffer of that language element
;; or from a toplevel from defining the element.
;; 
;; The visit happens as follows:
;;   - the 1st ^X-\ jumps to the beginning of the definition of the 1sr user.
;;   - then search-forward inside the first user for the name of the
;;     language element that was in the le-browser buffer.
;;   - the 2rd one searches again etc.. until no more match is found inside
;;     the body of the 1 user.
;;   - after the last match is seen the next user of the LE is visited 
;;     the same way.
;; 
;; If you want to start revisiting while a previous visit is not terminated do
;; ^U-^X-\ in another (or the same) le-browser buffer.
;; 
;; If you position the point on a User in the LE Browser buffer before doing
;; ^X-\, that use will become the next one shown.
;;
;; energize-next-use-previous, ^U-1-^X-\, backs up one use
;;
;; It's quite crude and the UI could be improved in different ways.  
;; What about:
;;   - Allow for stacking of visits so that you can recursively 
;;     visit another LE while one is being visited.
;;   - Highlight all the occurences of the LE-name inside the body of the user.
;;   - Deal with *alternatives*

(defvar energize-next-use-show-le-browser t
  "*If t energize-next-use-command will show both the next use and the le-browser buffer")

(defvar energize-next-use-search-by-file nil ;; default is off cause on is slow
  "*If t energize-next-use-command will show all uses in a file before going to
the next file")

(defvar energize-next-use-command-name ())
(defvar energize-next-use-label ())
(defvar energize-next-use-name ())
(defvar energize-next-use-from-mark ())
(defvar energize-next-use-source-start-mark ())
(defvar energize-next-use-source-end-mark ())
(defvar energize-next-use-source-current-mark ())
(defvar energize-next-use-current-file ())
(defvar energize-next-use-file-began-mark ())
(defvar energize-next-use-from-extent-cache ())
(defvar energize-next-use-history ())
(defvar energize-next-use-history-pointer ())
(defvar energize-next-use-marker-heap ())

;; remember markers we allocate so we can clean them up
(defun energize-next-use-make-a-marker ()
  (let ((m (make-marker)))
    (setq energize-next-use-marker-heap
	  (cons m energize-next-use-marker-heap))
    m))

(defun energize-next-use-copy-a-marker (mark)
  (let ((m (copy-marker mark)))
    (setq energize-next-use-marker-heap
	  (cons m energize-next-use-marker-heap))
    m))

(defun energize-next-use-cleanup-markers ()
  (while energize-next-use-marker-heap
    (set-marker (car energize-next-use-marker-heap) nil)
    (setq energize-next-use-marker-heap (cdr energize-next-use-marker-heap))))

(defun extent-after (pos)
  (let ((extent (next-extent (current-buffer))))
    (while (and extent (< (extent-start-position extent) pos))
      (setq extent (next-extent extent)))
    extent))

(defun energize-next-use-start-pos ()
  (save-excursion
    (goto-char (point-min))
    (if (search-forward energize-next-use-label () t)
	(point)
      ())))

;; see if we are in the LE Browser buffer of the object we are looking at
;; uses of.  If so, capture the position as a possible place to find the
;; next user, rather than take the first or next in sequence.
(defun energize-next-use-start-pos-offset ()
  ;; energize-next-use-from-mark is NULL iff this is called from
  ;; energize-next-use-init - in that case assume that if we are in a LEB
  ;; it is the one we will be using
  (if (if (and energize-next-use-from-mark
	       (marker-buffer energize-next-use-from-mark))
	  (eq (current-buffer) (marker-buffer energize-next-use-from-mark))
	(eq major-mode 'energize-browser-mode))
      (save-excursion
	(beginning-of-line)
	(point))))

(defun energize-next-use-get-name (name)
  (let ((colon (- (length name) 1)))
    (while (and (> colon 0) (not (eq (aref name colon) ?:)))
      (setq colon (- colon 1)))
    (if (= (aref name colon) ?:)
	(substring name (+ colon 1))
      name)))

;; cache the extents (and later the results of following them to the
;; source buffer) to avoid calling the server too many times
;; returns nil if the extent does not have a "source" menu option
;; returns the cache entry otherwise
(defun energize-next-use-valid-from-extent-p (buff extent)
  (let ((cache-entry (assoc extent energize-next-use-from-extent-cache)))
    (if (not cache-entry)
	(if (energize-list-menu buff extent ()
				energize-next-use-command-name)
	    (setq energize-next-use-from-extent-cache
		  (cons (setq cache-entry (list extent))
			energize-next-use-from-extent-cache))))
    cache-entry))

;; decide what use to look at next.  current is nil if called from
;; energize-next-use-next, else it is the presumed first extent for
;; energize-next-use-from-mark passed from energize-next-use-init
;; wanted-pos is the position of the cursor in the LE Browser of the
;; object being looked for, or nil if the cursor is not in that buffer
;; if wanted-pos is non-nil it means we will consider restarting the
;; search at the user indicated by that position.  Otherwise, we just
;; return current.  If this is nil too, it means just continue searching
;; as before.  If not, it means continue initializing as before.
;;
;; returns an extent to be used to set energize-next-use-from-mark or nil
(defun energize-next-use-adjust-for-start-offset (current wanted-pos)
  ;; assumes we're in the LE browser buffer (we may not have been when
  ;; the value of wanted-pos was determined)
  (if wanted-pos
      (let ((end-pos (energize-next-use-end-pos))
	    ;; ok will be set to the first extent found that is actually
	    ;; a user
	    (ok nil)
	    (try (or current (extent-after (energize-next-use-start-pos)))))
	;; loop will exit if an actual user extent is found that follows
	;; the wanted-pos - if we go out of bounds, try is set to nil
	;; else it is the desired extent on exit from the loop
	(while (and try
		    (or (< (extent-start-position try) end-pos)
			(setq try nil))
		    (not
		     (and
		      ;; try is a valid user extent if this menu exists
		      (energize-next-use-valid-from-extent-p
		       (current-buffer) try)
		      ;; but it might be before wanted-pos,
		      (or (> (extent-end-position try) wanted-pos)
			  ;; in which case remember it as ok unless
			  ;; ok was already set 
			  (and (setq ok (or ok try)) nil)))))
	  (setq try (next-extent try)))
	;; return try or if we were called from init, return ok
	;; else return nil to mean continue searching in current sequence
	(or try (and current ok)))
    current))

(defun energize-next-use-set-marks (extent buffer)
  (if (not energize-next-use-from-mark)
      (setq energize-next-use-from-mark (make-marker)))
  (set-marker energize-next-use-from-mark 
	      (extent-start-position extent) buffer))

(defun energize-next-use-init (command label)
  ;; start-offset is the position of point if it is in the LEB buffer
  (let ((start-offset (energize-next-use-start-pos-offset)))
    (if (not (eq major-mode 'energize-browser-mode))
	(if (energize-list-menu (current-buffer)
				(energize-extent-at (point)) ()
				"browse_le")
	    (energize-execute-command "browse_le"
				      (energize-extent-at (point)) () t)
	  (error
       "command undefined unless in a LE buffer or at a LE definition form")))
    (setq energize-next-use-command-name command)
    (setq energize-next-use-label label)
    (setq energize-next-use-current-file nil)
    (setq energize-next-use-from-extent-cache nil)
    (setq energize-next-use-history nil)
    (setq energize-next-use-history-pointer nil)
    (let* ((pos (energize-next-use-start-pos))
	   (extent (and pos
			;; start at the beginning, unless start-pos says
			;; to start further along
			(energize-next-use-adjust-for-start-offset
			 (extent-after pos) start-offset))))
      (if (null extent)
	  (error "no uses")
	(energize-next-use-set-marks extent (current-buffer))
	(setq energize-next-use-name
              (save-excursion
                (let ((s (energize-next-use-get-name (buffer-name))))
                  (if (let ((l (length s)))
                        (while (not
                                (and (> l 0)
                                     (progn
                                       (beginning-of-buffer)
                                       (search-forward
                                        (substring s 0 l) nil t))))
                          (setq l (- l 1)))
                        (> l 0))
                      (let (pos)
                        (backward-sexp)
                        (setq pos (point))
                        (forward-sexp)
                        (buffer-substring pos (point)))
                    s))))))
    (energize-next-use-show-both)))

(defun energize-next-use-end-pos ()
  (save-excursion
    (goto-char (point-min))
    (search-forward energize-next-use-label)
    (re-search-forward "^ *[A-Za-z ]*: " () 'end)
    (point)))

;; return the source-entry portion of the extent cache, looking it up
;; if necessary by doing energize-execute-command "source" on the extent
;; source-entry portion is list of
;; a) point marker after executing the "source" command
;; b) a flag saying we've actually visited this use, not just looked it up
;; c) marker for the window-start after executing the "source" command
(defun energize-next-use-from-extent-source-info (from-extent-entry)
  (let ((extent (car from-extent-entry))
	(source-entry (cdr from-extent-entry)))
    (if (not source-entry)
	(save-window-excursion
	  (save-excursion
	    (let ((pre-display-buffer-function nil)) ;; hack for multi-screen
	      (energize-execute-command
	       energize-next-use-command-name extent () t))
	    (setcdr from-extent-entry
		    (setq source-entry
			  (list (energize-next-use-copy-a-marker
				 (point-marker))
				nil
				(set-marker
				 (energize-next-use-make-a-marker)
				 (window-start))))))))
    source-entry))

(defun energize-next-use-from-extent-source-mark (source-entry)
  (car source-entry))

(defun energize-next-use-from-extent-source-start-mark (source-entry)
  (car (cdr (cdr source-entry))))

(defun energize-next-use-from-extent-source-seen-p (source-entry)
  (car (cdr source-entry)))

(defun energize-next-use-from-extent-set-source-seen-p (source-entry)
  (setcar (cdr source-entry) t))

;; goto the next user - or if advance is nil try to re-establish the
;; extent for the current one
(defun energize-next-use-next-to-extent (advance)
  (let ((buff (marker-buffer energize-next-use-from-mark))
	(from-pos (marker-position energize-next-use-from-mark)))
    (and buff from-pos
	 (let ((result nil))
	   (set-buffer buff)
	   (let ((end-pos (energize-next-use-end-pos))
		 (extent (or (energize-extent-at from-pos)
			     (setq advance nil)
			     (extent-after from-pos))))
	     (if (and advance energize-next-use-search-by-file)
		 (if (not energize-next-use-current-file)
		     ;; if searching by file but dont have a current file,
		     ;; set current file to current source buffer
		     (if (setq energize-next-use-current-file
			       (and energize-next-use-source-start-mark
				    (marker-buffer
				     energize-next-use-source-start-mark)))
			 (progn
			   (if (not energize-next-use-file-began-mark)
			       (setq energize-next-use-file-began-mark
				     (make-marker)))
			   (set-marker energize-next-use-file-began-mark
				       from-pos buff))))
	       (setq energize-next-use-current-file nil))
	     (if advance
		 (setq extent (next-extent extent)))
	     (if energize-next-use-search-by-file
		 (message "Searching for next use in current file...")
	       (message "Searching for next use..."))
	     ;; validate the extent
	     (while extent
	       (while
		   (and
		    extent
		    ;; if extent is beyond the end of the "used by" zone
		    ;; set it to nil to terminate loop
		    (or (< (extent-start-position extent) end-pos)
			(setq extent nil))
		    (not
		     (let ((from-extent-entry
			    (energize-next-use-valid-from-extent-p
			     buff extent)))
		       (and
			;; true if the extent has a "source" menu option
			from-extent-entry
			(or
			 (not energize-next-use-search-by-file)
			 ;; see if this is extent points to the current file
			 ;; or a file we haven't looked at yet, if not current
			 ;; file is set now
			 (let* ((source-entry
				 (energize-next-use-from-extent-source-info
				  from-extent-entry))
				(source-marker
				 (energize-next-use-from-extent-source-mark
				  source-entry))
				(buff (marker-buffer source-marker)))
			   (if energize-next-use-current-file
			       (eq buff energize-next-use-current-file)
			     (not
			      (energize-next-use-from-extent-source-seen-p
			       source-entry)))))))))
		 (setq extent (next-extent extent)))
	       (if extent
		   ;; we found one we can use - remember it but set extent
		   ;; to nil to terminate the loop
		   (progn
		     ;; convert the extent to a mark for future reference
		     (energize-next-use-set-marks extent buff)
		     ;; terminate the loop
		     (setq extent nil)
		     ;; saw we succeeded in finding something
		     (setq result t))
		 ;; we didn't find anything valid - if we were looking in
		 ;; the current file, restart the loop now looking for
		 ;; anything we haven't used yet, to make it the current file
		 ;; if we weren't looking in the current file then we really
		 ;; have failed to find anything left, so return nil
		 (if energize-next-use-current-file
		     (progn
		       (setq energize-next-use-current-file nil)
		       (setq extent
			     ;; restart loop from were we started looking at
			     ;; the current file
			     (let ((pos (marker-position
					 energize-next-use-file-began-mark)))
			       (and pos
				    (or (energize-extent-at pos)
					(extent-after pos)))))))))
	     (if energize-next-use-search-by-file
		 (message "Searching for next use in current file...done")
	       (message "Searching for next use...done"))
	     result)))))

(defun energize-next-use-show-from (&optional mark)
  (setq mark (or mark energize-next-use-from-mark))
  (if energize-next-use-show-le-browser
      ;; position the browser to the line of the current user
      (let ((buff (marker-buffer mark))
	    (from-pos (marker-position mark)))
	(and buff from-pos
	     (progn
	       ;; don't worry about pre-display-buffer-function here;
	       ;; we actually want a new screen to be created when
	       ;; displaying the Browser buffer.
	       (pop-to-buffer buff)
	       (goto-char from-pos)
	       (beginning-of-line)
	       (set-window-start (selected-window) (point)))))
    (message "next use of %s" energize-next-use-name)))

(defun energize-next-use-show-to ()
  (let* ((buff (marker-buffer energize-next-use-from-mark))
	 (from-pos (marker-position energize-next-use-from-mark))
	 (from-extent (and buff from-pos (energize-extent-at from-pos buff)))
	 (from-extent-entry
	  ;; get the cache entry for the current user extent in the browser
	  (energize-next-use-valid-from-extent-p buff from-extent))
	 (source-extent nil))
    (and
     from-extent-entry
     (let* ((source-entry
	     (energize-next-use-from-extent-source-info
	      from-extent-entry))
	    (source-marker
	     (energize-next-use-from-extent-source-mark
	      source-entry))
	    (source-buffer (marker-buffer source-marker))
	    (source-pos (marker-position source-marker))
	    ;; if we're allowed to split screens, call pop-to-buffer
	    ;; with no pre-display-buffer-function, so that the current
	    ;; screen (the screen of the Browser buffer) is used.  If
	    ;; we don't split screens, then use the p-d-b-f, so that a
	    ;; new screen can be created for this file.
	    (pre-display-buffer-function
	     (if energize-split-screens-p
		 nil
	       pre-display-buffer-function)))
       (if source-pos
	   (progn
	     ;; position the source window as if we had just executed the
	     ;; command "source" on the from-extent in the browser
	     (pop-to-buffer source-buffer)
	     (set-window-start
	      (selected-window)
	      (marker-position
	       (energize-next-use-from-extent-source-start-mark source-entry))
	      t)
	     (goto-char source-marker)
	     ;; note that we have actually visited this use
	     (energize-next-use-from-extent-set-source-seen-p source-entry)
	     (setq source-extent (energize-extent-at (point))))))
     (let ((start-pos (extent-start-position source-extent)))
       ;; convert positions to markers
       (if (not energize-next-use-source-start-mark)
	   (setq energize-next-use-source-start-mark (make-marker)))
       (if (not energize-next-use-source-end-mark)
	   (setq energize-next-use-source-end-mark (make-marker)))
       (if (not energize-next-use-source-current-mark)
	   (setq energize-next-use-source-current-mark (make-marker)))
       (set-marker energize-next-use-source-start-mark start-pos)
       (set-marker energize-next-use-source-current-mark start-pos)
       (set-marker energize-next-use-source-end-mark
		   (extent-end-position source-extent))))))

(defun energize-next-use-search-for-name ()
  (if (let ((case-fold-search nil)
	    (found nil)
	    (end-pos (marker-position energize-next-use-source-end-mark)))
	(if end-pos
	    (progn
	      (buffer-syntactic-context-flush-cache)
	      ;; case sensitive exact search for token not in comment or string
	      (while
		  (and
		   (setq found
			 (re-search-forward
			  (concat "[^A-Za-z0-9_]"
				  (regexp-quote energize-next-use-name)
				  "[^A-Za-z0-9_]")
			  end-pos t))
		   (save-excursion (buffer-syntactic-context))))))
	found)
      (progn
	;; allow for the delimiter
	(backward-char 1)
	t)
    nil))


;; say this if search can't find the thing that is being used
(defun energize-next-use-not-lexically-apparent ()
  (message "next use of %s is here, but not lexically apparent"
	   energize-next-use-name))

;; called when visiting a new users, not a new use within that user
(defun energize-next-use-show-both ()
  ;; reposition the browser to the line showing the user
  (energize-next-use-show-from)
  ;; position the source to the user definition form
  (energize-next-use-show-to)
  ;; instead of stopping at the beginning of the user form,
  ;; go immediately to the first use found if it is lexically apparent
  (if (and
       energize-next-use-source-start-mark
       (let ((buff (marker-buffer energize-next-use-source-start-mark))
	     (current-pos
	      (marker-position energize-next-use-source-current-mark))
	     (end-pos (marker-position energize-next-use-source-end-mark)))
	 (if (and buff current-pos end-pos)
	     (save-excursion
	       (set-buffer buff)
	       (and (< current-pos end-pos)
		    (progn
		      (goto-char current-pos)
		      (energize-next-use-search-for-name)))))))
      ;; an apparent use is there, so proceed as if going to the next use
      ;; within the user
      (energize-next-use-next)
    ;; no use is apparent, so print a message
    (energize-next-use-not-lexically-apparent)
    ;; record the history entry for a non-apparent use - it is distinguished
    ;; by having no point position from which to make a region to highlight
    (setq energize-next-use-history
	  (cons (list (energize-next-use-copy-a-marker
		       energize-next-use-from-mark)
		      (energize-next-use-copy-a-marker
		       energize-next-use-source-current-mark)
		      (set-marker (energize-next-use-make-a-marker)
				  (window-start)))
		energize-next-use-history))
    (setq energize-next-use-history-pointer
	  energize-next-use-history)))

;; show the next use within the current user - if no more are found
;; go to the next user
(defun energize-next-use-next ()
  ;; new-start will get set to an extent for a user at which to
  ;; reposition the visit, iff the cursor is in the LEB buffer
  ;; of the usee at a valid position in the list of users.
  (let ((new-start (energize-next-use-adjust-for-start-offset
		    nil (energize-next-use-start-pos-offset)))
	(buff (marker-buffer energize-next-use-source-start-mark))
	(current-pos (marker-position energize-next-use-source-current-mark))
	(end-pos (marker-position energize-next-use-source-end-mark)))
    (if (and buff (not (eq buff (current-buffer))))
	(set-buffer buff))
    ;; new-start means we are repositioning due to the point being in the
    ;; user list in the browser.  In this case we will act as if we are
    ;; at the end of the old user
    (if new-start
	(energize-next-use-set-marks
	 new-start
	 (marker-buffer energize-next-use-from-mark)))
    (cond ((and buff current-pos end-pos
		(not new-start) ;; don't stay in current user if repositioning
		(< current-pos end-pos)
		(progn
		  (goto-char current-pos)
		  (energize-next-use-search-for-name)))
	   ;; the 'still in the same user' case
	   (set-marker energize-next-use-source-current-mark (point))
	   ;; redisplay the browser in case it got covered up
	   (energize-next-use-show-from)
	   ;; We know that we're not changing files, so there won't be any
	   ;; need to create a new screen; it would be nice if we reselected
	   ;; the appropriate screen instead of just using the current screen,
	   ;; but that's more work, and this will very rarely do the wrong
	   ;; thing.
	   (let ((pre-display-buffer-function 
		  (if energize-split-screens-p
		      nil
		    pre-display-buffer-function)))
	     ;; display the source buffer
	     (pop-to-buffer buff)
	     ;; had to do this because window-start overrides point
	     ;; in terms of deciding what part of the window to show
	     ;; this seems like a possible bug
	     (set-window-buffer (selected-window) buff)
	     (push-mark (point) t)
	     (backward-char (length energize-next-use-name))
	     (if zmacs-regions
		 (zmacs-activate-region))
	     ;; record the visit in the history - this entry has a point
	     ;; marker, distinguishing it from a non-lexically apparent one
	     (setq energize-next-use-history
		   (cons (list
			  (energize-next-use-copy-a-marker
			   energize-next-use-from-mark)
			  (energize-next-use-copy-a-marker
			   energize-next-use-source-current-mark)
			  (set-marker (energize-next-use-make-a-marker)
				      (window-start))
			  (set-marker (energize-next-use-make-a-marker)
				      (point)))
			 energize-next-use-history))
	     (setq energize-next-use-history-pointer
		   energize-next-use-history)))
	  ((or new-start
	       (energize-next-use-next-to-extent
		(and buff current-pos end-pos)))
	   ;; the 'moved to a new user or repositioned to one' case
	   (energize-next-use-show-both))
	  (t
	   ;; no more users
	   (energize-next-use-terminate)
	   (error "no more")))))


(defun energize-next-use-terminate ()
  (setq energize-next-use-command-name ())
  (setq energize-next-use-label ())
  (setq energize-next-use-name ())
  (if energize-next-use-from-mark
      (set-marker energize-next-use-from-mark nil))
  (if energize-next-use-source-start-mark
      (set-marker energize-next-use-source-start-mark nil))
  (if energize-next-use-source-current-mark
      (set-marker energize-next-use-source-current-mark nil))
  (if energize-next-use-source-end-mark
      (set-marker energize-next-use-source-end-mark nil))
  (if energize-next-use-file-began-mark
      (set-marker energize-next-use-file-began-mark nil))
  (energize-next-use-cleanup-markers))

;; get next use from history list if we have regressed or are regressing
;; if history-pointer == history then we are in the present
;; to go back set history-pointer to cdr history-pointer
;; to go forward set history-pointer to the last cell on history prior to
;; the current value of history-pointer
;; a history entry is just redisplay info for the browser and source buffers
(defun energize-next-use-from-history (backward)
  (if backward
      (setq energize-next-use-history-pointer
	    (cdr energize-next-use-history-pointer))
    (let ((temp energize-next-use-history-pointer))
      (setq energize-next-use-history-pointer
	    energize-next-use-history)
      (while (not (eq (cdr energize-next-use-history-pointer) temp))
	(setq energize-next-use-history-pointer
	      (cdr energize-next-use-history-pointer)))))
  (let* ((history-entry (car energize-next-use-history-pointer))
	 (from-mark (car history-entry))
	 (source-mark (car (cdr history-entry)))
	 (source-start-mark (car (cdr (cdr history-entry))))
	 (source-point-mark (car (cdr (cdr (cdr history-entry))))))
    (energize-next-use-show-from from-mark)
    (if (and (marker-position source-mark)
	     (marker-position source-start-mark))
	(progn
	  (pop-to-buffer (marker-buffer source-mark))
	  (set-window-start
	   (selected-window) (marker-position source-start-mark) t)
	  (goto-char source-mark)
	  (if (and source-point-mark (marker-position source-point-mark))
	      (progn
		(push-mark (point) t)
		(goto-char source-point-mark)
		(if zmacs-regions
		    (zmacs-activate-region)))
	    (energize-next-use-not-lexically-apparent))))))

(defun energize-next-use-go (label command)
  (if (and energize-next-use-from-mark
	   (marker-position energize-next-use-from-mark))
      (if (eq energize-next-use-history-pointer
	      energize-next-use-history)
	  ;; normal case
	  (energize-next-use-next)
	;; in the depths of history but going forward
	(energize-next-use-from-history nil))
    ;; beginning of time
    (energize-next-use-init label command)))

(defun energize-next-use-previous ()
  (if (cdr energize-next-use-history-pointer)
      ;; going back in time
      (energize-next-use-from-history t)
    ;; trying to go beyond the pale
    (error "no prior use")))
	
(defun energize-next-use-command (arg)
  "Show the next place where the current language element is used.
The current language element is the one that point is on for source buffers
or the element displayed in a language element browser buffer.
Repeated calls to this functions visit all the callers in sequence.  

With prefix arg = 1, back up to the last use.

With prefix arg > 1, start a visit from scratch on the current language 
element.

If the point is on a particular user in the language element browser, the
search will be (re)started in that user.

The variable energize-next-use-show-le-browser controls if this command should
display both the language element browser buffer and the next user or just the
next user of the current language element.

The variable energize-next-use-search-by-file controls whether all uses within
a given file should be shown in sequence.  If the value is nil, the uses
are shown in the order in which they appear in the LE Browser.  If the
value is non-nil, the all uses in a given file are shown before proceeding to
the next use in another file."
  (interactive "P")
  (if (eq arg 1)
      (energize-next-use-previous)
    (if arg
	(energize-next-use-terminate))
    (energize-next-use-go "source" "Used By:")))

(define-key global-map '[(control x) \\] 'energize-next-use-command)

(defun energize-next-use-start ()
  "Start visiting the uses of a language element.
If executed in a LE Browser buffer visit the uses of the language element
in the buffer.  In a source buffer visit the uses of the language element
at (point)"
  (interactive)
  (energize-next-use-command 4))
