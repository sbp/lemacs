;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright © 1992-1993 by Lucid, Inc.  All Rights Reserved.

;; Load this file and ^X-\ (back-slash) gets bound to a
;; command that visit all the places where a language element is used.  
;; It can be started from the Le-Browser buffer of that language element
;; or from a toplevel from defining the element.
;; 
;; The visit happens as follows:
;;   - the 1st ^X-\ jumps to the beginning of the definition of the 1sr user.
;;   - the 2nd ^X-\ search-forward inside the first user for the name of the
;;     language element that was in the le-browser buffer.
;;   - the 3rd one searches again etc.. until no more match is found inside
;;     the body of the 1 user.
;;   - after the last match is seen the next user of the LE is visited 
;;     the same way.
;; 
;; If you want to start revisiting while a previous visit is not terminated do
;; ^U-^X-\ in another (or the same) le-browser buffer.
;; 
;; It's quite crude and the UI could be improved in different ways.  
;; What about:
;;   - A command to start the search at another place in the middle of a
;;     le-browser buffer.
;;   - Allow for stacking of visits so that you can recursively 
;;     visit another LE while one is being visited.
;;   - Highlight all the occurences of the LE-name inside the body of the user.

(defvar energize-next-use-show-le-browser t
  "*If t energize-next-use-command will show both the next use and the le-browser buffer")

(defvar energize-next-use-command-name ())
(defvar energize-next-use-label ())
(defvar energize-next-use-from-extent ())
(defvar energize-next-use-name ())
(defvar energize-next-use-to-extent ())
(defvar energize-next-use-position ())

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

(defun energize-next-use-get-name (name)
  (let ((colon (- (length name) 1)))
    (while (and (> colon 0) (not (eq (aref name colon) ?:)))
      (setq colon (- colon 1)))
    (if (= (aref name colon) ?:)
	(substring name (+ colon 1))
      name)))


(defun energize-next-use-init (command label)
  (if (not (eq major-mode 'energize-browser-mode))
      (energize-execute-command "browse_le" (extent-at (point)) () t))
  (setq energize-next-use-command-name command)
  (setq energize-next-use-label label)
  (let ((pos (energize-next-use-start-pos)))
    (if (null pos)
	(error "no uses")
      (setq energize-next-use-from-extent (extent-after pos))
      (setq energize-next-use-name
	    (energize-next-use-get-name (buffer-name)))))
  (energize-next-use-show-both))

(defun energize-next-use-end-pos ()
  (save-excursion
    (goto-char (point-min))
    (search-forward energize-next-use-label)
    (re-search-forward "^ *[A-Za-z ]*: " () 'end)
    (point)))

(defun energize-next-use-next-to-extent ()
  (set-buffer (extent-buffer energize-next-use-from-extent))
  (let ((end-pos (energize-next-use-end-pos)))
    (setq energize-next-use-from-extent
	  (next-extent energize-next-use-from-extent))
    (while (and energize-next-use-from-extent
		(< (extent-start-position energize-next-use-from-extent)
		   end-pos)
		(not
		 (energize-list-menu (current-buffer)
				     energize-next-use-from-extent ()
				     energize-next-use-command-name)))
      (setq energize-next-use-from-extent
	    (next-extent energize-next-use-from-extent)))
    (if (and energize-next-use-from-extent
	     (>= (extent-start-position energize-next-use-from-extent)
		 end-pos))
	(setq energize-next-use-from-extent ()))
    energize-next-use-from-extent))

(defun energize-next-use-show-from ()
  (if energize-next-use-show-le-browser
      (progn
	(set-buffer (extent-buffer energize-next-use-from-extent))
	(let* ((pop-up-windows t)
	       (pre-display-buffer-function nil)
	       (w (display-buffer
		   (extent-buffer energize-next-use-from-extent)))
	       (pos (save-excursion
		      (goto-char
		       (extent-start-position energize-next-use-from-extent))
		      (beginning-of-line)
		      (point))))
	  (set-window-point w pos)
	  (set-window-start w pos)))
    (message "next use of %s"
	     (buffer-name (extent-buffer energize-next-use-from-extent)))))

(defun energize-next-use-show-to ()
  (let ((pre-display-buffer-function nil))
    (energize-execute-command energize-next-use-command-name
			      energize-next-use-from-extent () t))
  (setq energize-next-use-to-extent (extent-at (point)))
  (setq energize-next-use-position
	(extent-start-position energize-next-use-to-extent)))

(defun energize-next-use-show-both ()
  (energize-next-use-show-from)
  (energize-next-use-show-to))

(defun energize-next-use-next ()
  (set-buffer (extent-buffer energize-next-use-to-extent))
  (cond ((and (< energize-next-use-position
		 (extent-end-position energize-next-use-to-extent))
	      (progn
		(goto-char energize-next-use-position)
		(search-forward energize-next-use-name
				(extent-end-position
				 energize-next-use-to-extent) t)))
	 (setq energize-next-use-position (point))
	 (push-mark)
	 (backward-char (length energize-next-use-name))
	 (if zmacs-regions
	     (zmacs-activate-region))
	 (energize-next-use-show-from)
	 (let ((pre-display-buffer-function nil))
	   (pop-to-buffer (extent-buffer energize-next-use-to-extent))))
	((energize-next-use-next-to-extent)
	 (if zmacs-regions
	     (zmacs-deactivate-region))
	 (energize-next-use-show-both))
	(t
	 (energize-next-use-terminate)
	 (error "no more"))))

(defun energize-next-use-terminate ()
  (setq energize-next-use-command-name ())
  (setq energize-next-use-label ())
  (setq energize-next-use-from-extent ())
  (setq energize-next-use-name ())
  (setq energize-next-use-to-extent ())
  (setq energize-next-use-position ()))

(defun energize-next-use-go (label command)
  (if (null energize-next-use-from-extent)
      (energize-next-use-init label command)
    (energize-next-use-next)))

(defun energize-next-use-command (arg)
  "Show the next place where the current language element is used.
The current language element is the one that point is on for source buffers
or the element displayed in a language element browser buffer.
Repeated calls to this functions visit all the callers in sequence.  
The caller function is first displayed then all the places in the caller that
call the language element are visited.

With prefix arg set, start a visit from scratch on the current language 
element. 

The variable energize-next-use-show-le-browser controls if this command should
display both the language element browser buffer and the next user or just the
next user of the current language element."
  (interactive "P")
  (if arg
      (energize-next-use-terminate))
  (energize-next-use-go "source" "Used By:"))

(define-key global-map '[(control x) \\] 'energize-next-use-command)

(defun energize-next-use-start ()
  "Start visiting the uses of a language element.
If executed in a LE Browser buffer visit the uses of the language element
in the buffer.  In a source buffer visit the uses of the language element
at (point)"
  (interactive)
  (energize-next-use-command 4))
