;;  file-complete.el
;;
;; Code to do completion of $envvar and ~username, within shell buffers
;; or the minibuffer.
;; 
;; Copyright (C) 1991 Free Software Foundation, Inc.
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Written by eirik@theory.tn.cornell.edu.
;;(wmessage "In file-complete.el...") 
(defvar passwd-use-yp nil
  "*If t, do not read the passwd file directly")

(defvar shell-mode-hook
  (function (lambda ()
	      (local-set-key "\^I" 'shell-expand-file-name))))

(or (fboundp 'read-file-name-internal-primitive)
    (fset 'read-file-name-internal-primitive
	  (symbol-function 'read-file-name-internal)))
  
;;  This is unnecessary with #define MAINTAIN_ENVIRONMENT

(if (boundp 'process-environment)
    (or (fboundp 'getenv-primitive)
	(progn
	  (fset 'getenv-primitive (symbol-function 'getenv))
	  (defun getenv (var)
	    "Return the value of environment variable VAR, or the entire environment if VAR is t"
	    (if (eq t var)
		(if envvars envvars
		  (setq envvars
			(mapcar
			 (function
			  (lambda (string)
			    (let ((d (string-match "=" string)))
			      (cons (substring string 0 d)
				    (and d (substring string (1+ d)))))))
			 process-environment)))
	      (getenv-primitive var))))))

(defvar envvars nil
  "A list of the environment variable names and values.")

;; Might as well compute this at load time
(getenv t)

(defun read-file-name-internal (name dir action)
  "A replacement for the primitive read-file-name-internal that expands
partial usernames and environment variable names.

NAME is the filename to complete; DIR is the directory to complete in.
ACTION is nil to complete, t to return list of completions, lambda to
verify final value."

    (let* ((buf (current-buffer))
	   (char (progn
		   (set-buffer (get-buffer-create " *read*"))
		   (erase-buffer)
		   (insert name)
		   (and (re-search-backward "[$~]" nil t)
			(char-after (point)))))
	   (can (and char
		     (or (eq (point) (point-min))
			 (save-excursion (backward-char 1)
					 (looking-at "/")))
		     (not (progn
			    (forward-char 1)
			    (save-excursion
			      (re-search-forward "[^A-Za-z -]"
						 (point-max) t))))
		     (buffer-substring (point) (point-max))))
	   (ignore (set-buffer buf)))
      (if (null can) (read-file-name-internal-primitive name dir action)
	(let ((prefix (substring name 0 (- (length name) (length can) 1))))
	  (cond
	   ((eq char ?~)
	    (let ((s (complete-username can nil action)))
	      (cond ((stringp s)
		     (concat "~" s
			     (and
			      (eq t (complete-username s nil action))
			      (file-directory-p
			       (expand-file-name (concat "~" s)))
			      "/")))
		    ((eq t s) (concat name 
				      (if (file-directory-p
					   (expand-file-name name))
					  "/")))
		    (t s))))
	   ((eq char ?$)
	    (let ((completion-list (all-completions can (getenv t))))
	      (cond
	       ((null action)
		(let* ((un (and (eq (length completion-list) 1)
				(car completion-list)))
		       (unv (and un (getenv un)))
		       (dirp (and unv (> (length unv) 0)
				  (file-directory-p unv))))
		  (if (and un (string-equal un can))
		      (concat prefix unv (if dirp "/"))
		    (let ((s (try-completion can (getenv t))))
		      (cond ((stringp s) (concat prefix "$" s
						 (if dirp "/")))
			    (t s))))))
	       ((eq t action) completion-list)
	       (t (eq 1 (length completion-list)))))))))
      ))

(defun complete-username (string predicate flag)
  "Use /etc/passwd to expand a ~."
  (if (string-match ":" string) nil
    (let ((pwbuf (get-file-buffer "/etc/passwd"))
	  (regexp (concat "^" string "."))
	  (buffer (current-buffer))
	  ret)
      (cond (pwbuf
	     (set-buffer pwbuf)
	     (or passwd-use-yp
		 (verify-visited-file-modtime pwbuf)
		 (revert-buffer t t)))
	    (passwd-use-yp
	     (progn
	       (setq pwbuf (create-file-buffer "/etc/passwd"))
	       (set-buffer pwbuf)
	       (call-process "ypcat" nil pwbuf nil "passwd")
	       (set-visited-file-name "/etc/passwd")
	       (set-buffer-modified-p nil)))
	    (t
	     (setq pwbuf (create-file-buffer "/etc/passwd"))
	     (set-buffer pwbuf)
	     (insert-file-contents "/etc/passwd" t)))
      (save-excursion
	(goto-char (point-min))
	(cond ((eq flag t)
	       (while (and flag (re-search-forward regexp nil t))
		 (let* ((start (progn
				 (beginning-of-line 1)
				 (point)))
			(end (if (search-forward ":" nil t)
				 (1- (point))
			       (setq flag nil)))
			(name (and start end
				   (buffer-substring start end))))
		   (setq ret
			 (nconc ret
				(if predicate
				    (if (funcall predicate name)
					(list name))
				  (list name)))))))
	      ((not flag)
	       (setq ret
		     (let ((list
			     (mapcar 'list
				     (complete-username string nil t))))
			    (let ((match
				   (try-completion
				    string
				    (if list list (list nil)))))
			      (or (and (eq (length list) 1)
				       (complete-username string
							  predicate
							  'lambda))
				  match)))))
	      (t
	       (and (re-search-forward (concat "^" string ":") nil t)
		    (setq ret t)))
	      ))
      (set-buffer buffer)
      ret)))

;; Same, within shell buffers.  It is useful to bind this to a key,
;; e.g., TAB.
;; 
(defun shell-expand-file-name ()
  "Expand the file name before point"
  (interactive)
  (let* (
	 (line (save-excursion (beginning-of-line) (point)))
	 (base (or (save-excursion
		     (and (re-search-backward "[ /]" line t)
			  (point)))
		   (1- line)))
	 (dir (or (save-excursion
		    (and (search-backward " " line t)
			 (1+ (point))))
		  line))
	 (char (and dir (char-after dir)))
	 (dirname (and dir (or
			    (and (eq char ?$)
				 (let* ((s (save-excursion
					     (goto-char dir)
					     (search-forward "/" nil t)
					     (point)))
					(d (and s (getenv
						   (buffer-substring
						    (1+ dir) (1- s))))))
				   (and d (eq (aref d 0) ?/) 
					(concat d
						(buffer-substring (1- s) base)
						))))
			    (and (eq char ?/)
				 (buffer-substring dir (1+ base)))
			    (and (> base dir)
				 (concat default-directory
					 (buffer-substring dir base)))
			    default-directory)))
	 (basename (and base (buffer-substring (1+ base) (point))))
	 (name (and basename dirname
		    (read-file-name-internal basename dirname nil)))
	 )
    (cond
     ((eq t name) (message "[Sole completion]"))
     ((null name) (message "[No match]"))
     ((string-equal name basename)
      (with-output-to-temp-buffer "*Completions*"
	(display-completion-list
	 (read-file-name-internal basename dirname t))))
     (t (delete-region (1+ base) (point))
	(insert name))
     )))

;; eof
