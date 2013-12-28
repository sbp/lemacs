;;; -*- Mode: EMACS-LISP; Syntax: E-Lisp; Base: 10 -*-
;;;
;;;; backtrace-logging.el
;;;
;;; User code for interacting with backtrace logging.
;;;
;;; ***************************************************************************
;;;
;;; Revision:	29-Jan-92 12:14:49
;;;
;;; Programmer: Harlan Sexton
;;;
;;; Edit-History:
;;;
;;; Created: 30-Aug-91 by hbs
;;;
;;; End-of-Edit-History

(defvar *btl-index-counter* 1)
(defvar *btl-data-file* nil)
(defvar *btl-indices-assigned* nil)
(defvar elisp-only-btl nil)
(defvar cadillac-id-tag-save 'cadillac-id-tag-save)

(defun assign-btl-indices (&optional force)
  (if (or force 
	  (not *btl-indices-assigned*))
      (progn
	(mapatoms 'assign-btl-index)
	(setq *btl-indices-assigned* t))))

(defun btl-compiled-p (function)
  (or (subrp function)
      (compiled-function-p function)
      (and (consp function)
	   (eq (car function) 'lambda)
	   (let ((rest (cdr (cdr function)))
		 (quit nil))
	     (while (and rest (not quit)) 
	       (let ((trial (car rest)))
		 (if (consp trial)
		     (setq quit (eq (car trial) 'byte-code)))
		 (setq rest (cdr rest))))
	     quit))))

(defun get-btl-index (symbol)
  (if (symbolp symbol)
      (let ((val (or (get symbol cadillac-id-tag)
		     (get symbol 'cadillac-id-tag-save))))
	(if (and val (integerp val) (> val 0))
	    val
	  (progn
	    (setq val *btl-index-counter*)
	    (setq *btl-index-counter* (1+ *btl-index-counter*))
	    val)))))
      
(defun disable-btl-index (symbol)
  (if (symbolp symbol)
      (let ((val (or (get symbol cadillac-id-tag)
		     (get symbol 'cadillac-id-tag-save))))
	(if (and val (integerp val) (> val 0))
	    (put symbol 'cadillac-id-tag-save val))
	(remprop symbol cadillac-id-tag))))

(defun assign-btl-index (symbol)
  (if (and (symbolp symbol)
	   (fboundp symbol))
      (let* ((function (symbol-function symbol))
	     (subr-function (subrp function))
	     (compiled-function (btl-compiled-p function))
	     (tagged-function (get symbol cadillac-id-tag)))
	(cond ((and elisp-only-btl compiled-function (not tagged-function))
	       (put symbol cadillac-id-tag (get-btl-index symbol)))
	      ((and (not elisp-only-btl) subr-function tagged-function)
	       (disable-btl-index symbol))))))
                    
(defun file-to-btl-data-file (file)
  (let ((base-dir (expand-file-name "")))
    (if (string-equal base-dir file)
	(setq file nil)))
  (setq file (or file *btl-data-file*))
  (let ((go-ahead nil))
    (if (or (not (stringp file))
	    (file-directory-p file))
	(setq go-ahead nil)
      (if (file-exists-p file)
	  (setq go-ahead
		(y-or-n-p (message "File exists -- overwrite %s? "
				   file)))
	(setq go-ahead t)))
    (if (not go-ahead)
	(error "Bad data file.")))
  file)

(defun btl-init (file &optional pc-logging)
  (setq file (file-to-btl-data-file file))
  (let ((log-type (if pc-logging "PC logging" "btl")))
    (message (format "Initializing %s..." log-type))
    (sit-for 0)
    (assign-btl-indices t)
    (setq *btl-data-file* file)
    (if pc-logging
	(initialize-pc-logging-internal file)
      (initialize-backtrace-logging-internal file))
    (message (format "Initializing %s...done." log-type))       
    (sit-for 1)
    (message "")        
    (sit-for 0)))

(defun start-btl (file &optional elisp-flag-val)
  (interactive "FFile in which to write data: ")
  (terminate-logging)
  (setq elisp-only-btl elisp-flag-val)
  (btl-init file)
  (start-logging))

(defun start-btl-elisp (file)
  (interactive "FFile in which to write data: ")
  (start-btl file t))

(defun consing-btl (file &optional elisp-flag-val)
  (interactive "FFile in which to write data: ")
  (terminate-logging)
  (setq elisp-only-btl elisp-flag-val)
  (set-log-signal 0)
  (btl-init file)
  (start-logging))

(defun consing-btl-elisp (file)
  (interactive "FFile in which to write data: ")
  (consing-btl file t))

(defun set-elisp-btl (arg)
  (interactive "p")
  (setq elisp-only-btl (eql arg 1)))

(defun start-pcl (file &optional elisp-flag-val)
  (interactive "FFile in which to write data: ")
  (setq elisp-only-btl elisp-flag-val)
  (btl-init file t)
  (start-logging))

(defun start-pcl-elisp (file)
  (interactive "FFile in which to write data: ")
  (start-pcl file t))

(defun suspend-btl ()
  (interactive)
  (stop-logging))

(defun suspend-pcl ()
  (interactive)
  (stop-logging))

(defun resume-btl ()
  (interactive)
  (start-logging))

(defun resume-pcl ()
  (interactive)
  (start-logging))

(defun stop-btl ()
  (interactive)
  (terminate-logging))

(defun stop-pcl ()
  (interactive)
  (terminate-logging))

(defun show-btl ()
  (interactive)
  (terminate-logging)
  (switch-to-buffer (get-buffer-create "*BTL Log Info*"))
  (goto-char (point-max))
  (buffer-disable-undo (current-buffer))
  (if *btl-data-file*
      (summarize-logging *btl-data-file*)))
