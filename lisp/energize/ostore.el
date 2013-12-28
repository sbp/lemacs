;;; -*- Mode: Emacs-Lisp -*-
;;; Adds a couple of ObjectStore commands to the Energize "Browse" menu.

(defun osbrowser-sentinel (proc msg)
  (cond ((eq (process-status proc) 'exit)
	 (message "Osbrowser subprocess exited"))
	((eq (process-status proc) 'signal)
	 (message "Osbrowser subprocess killed"))))

(defun os-browser () 
  (interactive)
  (message "Launching osbrowser.")
  (set-process-sentinel 
   (start-process "os-browser" "*os-browser*" "osbrowser") 
   'osbrowser-sentinel))

(defun ossd-sentinel (proc msg)
  (cond ((eq (process-status proc) 'exit)
	 (message "OS schema designer subprocess exited"))
	((eq (process-status proc) 'signal)
	 (message "OS schema designer subprocess killed"))))

(defun os-schema-designer () 
  (interactive)
  (message "Launching ossd.")
  (set-process-sentinel 
   (start-process "os-schema-designer" "*os-schema-designer*" "ossd")
   'osbrowser-sentinel))

(add-menu-item '("Browse") "----" nil t)
(add-menu-item '("Browse") "OS Browser" 'os-browser t)
(add-menu-item '("Browse") "OS Schema Designer" 'os-schema-designer t)

(provide 'ostore)
