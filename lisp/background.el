;; Fun with background jobs.
;; Copyright (C) 1988 Joe Keane <jk3k+@andrew.cmu.edu>
;; Refer to the GNU Emacs General Public License for copyright info.

;; - Adapted to use comint and cleaned up somewhat. Olin Shivers 5/90
;; - Background failed to set the process buffer's working directory
;;   in some cases. Fixed. Olin 6/14/90
;; - Background failed to strip leading cd's off the command string
;;   after performing them. This screwed up relative pathnames.
;;   Furthermore, the proc buffer's default dir wasn't initialised 
;;   to the user's buffer's default dir before doing the leading cd.
;;   This also screwed up relative pathnames if the proc buffer already
;;   existed and was set to a different default dir. Hopefully we've
;;   finally got it right. The pwd is now reported in the buffer
;;   just to let the user know. Bug reported by Piet Van Oostrum.
;;   Olin 10/19/90
;; - Fixed up the sentinel to protect match-data around invocations.
;;   Also slightly rearranged the cd match code for similar reasons.
;;   Olin 7/16/91

(provide 'background)
(require 'comint)

;; user variables
(defvar background-show t
  "*If non-nil, background jobs' buffers are shown when they're started.")
(defvar background-select nil
  "*If non-nil, background jobs' buffers are selected when they're started.")

(defun background (command)
  "Run COMMAND in the background like csh.  
A message is displayed when the job starts and finishes.  The buffer is in
comint mode, so you can send input and signals to the job.  The process object
is returned if anyone cares.  See also comint-mode and the variables
background-show and background-select."
  (interactive "s%% ")
  (let ((job-number 1)
	(job-name "%1")
	(dir default-directory))
    (while (process-status job-name)
      (setq job-name (concat "%" (setq job-number (1+ job-number)))))
    (if background-select (pop-to-buffer job-name)
      (if background-show (with-output-to-temp-buffer job-name)) ; cute
      (set-buffer (get-buffer-create job-name)))
    (erase-buffer)

    (setq default-directory dir) ; Do this first, in case cd is relative path.
    (if (string-match "^cd[\t ]+\\([^\t ;]+\\)[\t ]*;[\t ]*" command)
	(let ((dir (substring command (match-beginning 1) (match-end 1))))
	   (setq command (substring command (match-end 0)))
	   (setq default-directory
		 (file-name-as-directory (expand-file-name dir)))))

    (insert "--- working directory: " default-directory
	    "\n% " command ?\n)

    (let ((proc (get-buffer-process
		 (comint-exec job-name job-name shell-file-name
			      nil (list "-c" command)))))
      (comint-mode)
      ;; COND because the proc may have died before the G-B-P is called.
      (cond (proc (set-process-sentinel proc 'background-sentinel)
		  (message "[%d] %d" job-number (process-id proc))))
      (setq mode-name "Background")
      proc)))

(defun background-sentinel (process msg)
  "Called when a background job changes state."
  (let ((ms (match-data))) ; barf
    (unwind-protect
	 (let ((msg (cond ((string= msg "finished\n") "Done")
			  ((string-match "^exited" msg)
			   (concat "Exit " (substring msg 28 -1)))
			  ((zerop (length msg)) "Continuing")
			  (t (concat (upcase (substring msg 0 1))
				     (substring msg 1 -1))))))
	   (message "[%s] %s %s" (substring (process-name process) 1)
		    msg
		    (nth 2 (process-command process)))
	   (if (null (buffer-name (process-buffer process)))
	       (set-process-buffer process nil) ; WHY? Olin.
	       (if (memq (process-status process) '(signal exit))
		   (save-excursion
		     (set-buffer (process-buffer process))
		     (let ((at-end (eobp)))
		       (save-excursion
			 (goto-char (point-max))
			 (insert ?\n msg ? 
				 (substring (current-time-string) 11 19) ?\n))
		       (if at-end (goto-char (point-max))))
		     (set-buffer-modified-p nil)))))
      (store-match-data ms))))
