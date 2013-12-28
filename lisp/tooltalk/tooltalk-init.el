;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; Registration of the default Tooltalk patterns and handlers.
;;;
;;; @(#)tooltalk-init.el 1.5 93/11/24


(defvar tooltalk-eval-pattern
  '(category TT_HANDLE
       scope TT_SESSION
          op "emacs-eval"
    callback tooltalk-eval-handler))

(defvar tooltalk-load-file-pattern
  '(category TT_HANDLE
       scope TT_SESSION
          op "emacs-load-file"
    callback tooltalk-load-file-handler))

(defvar tooltalk-make-client-screen-pattern 
  '(category TT_HANDLE
       scope TT_SESSION
          op "emacs-make-client-screen"
    callback tooltalk-make-client-screen-handler))

(defvar tooltalk-status-pattern 
  '(category TT_HANDLE
       scope TT_SESSION
          op "emacs-status"
    callback tooltalk-status-handler))

(defun register-initial-tooltalk-patterns ()
  (mapcar #'register-tooltalk-pattern 
	  (mapcar #'make-tooltalk-pattern 
		  (list tooltalk-eval-pattern
			tooltalk-load-file-pattern
			tooltalk-make-client-screen-pattern
			tooltalk-status-pattern))))



;;; If the string contains embedded nulls (unlikely but possible)
;;; then replace each one with "\000".

(defun tooltalk::prin1-to-string-carefully (form)
  (let ((string (prin1-to-string form)))
    (let ((parts '())
	  index)
      (while (setq index (string-match "\0" string))
	(setq parts 
	      (apply 'list "\\000" (substring string 0 index) parts))
	(setq string (substring string (1+ index))))
      (if (not parts)
	  string
	(setq parts (apply 'list string parts))
	(apply 'concat (nreverse parts))))))


(defun tooltalk::eval-string (str)
  (let ((result (eval (car (read-from-string str)))))
    (tooltalk::prin1-to-string-carefully result)))


(defun tooltalk-eval-handler (msg pat)
  (let ((str (get-tooltalk-message-attribute msg 'arg_val 0))
	(result-str nil)
	(failp t))
    (unwind-protect
	(cond
	 ;; Assume That the emacs debugger will handle errors.
	 ;; If the user throws from the debugger to the cleanup
	 ;; form below, failp will remain t.
	 (debug-on-error   
	  (setq result-str (tooltalk::eval-string str)
		failp nil))

	 ;; If an error occurs as a result of evaluating
	 ;; the string or printing the result, then we'll return 
	 ;; a string version of error-info.
	 (t
	  (condition-case error-info
	      (setq result-str (tooltalk::eval-string str)
		    failp nil)
	    (error 
	     (let ((error-str
		    (tooltalk::prin1-to-string-carefully error-info)))
	       (setq result-str error-str
		     failp t))))))

      ;; If we get to this point and result-str is still nil, the
      ;; user must have thrown out of the debuggger
      (let ((reply-type (if failp 'fail 'reply))
	    (reply-value (or result-str "(debugger exit)")))
	(set-tooltalk-message-attribute reply-value msg 'arg_val 0)
	(return-tooltalk-message msg reply-type)))))



(defun tooltalk-make-client-screen-handler (m p)
  (let ((nargs (get-tooltalk-message-attribute m 'args_count)))
    (if (not (= 3 nargs))
	(progn
	  (set-tooltalk-message-attribute "wrong number of arguments" m 'status_string)
	  (return-tooltalk-message m 'fail))))

  ;; Note: relying on the fact that arg_ival is returned as a string

  (let* ((name   (get-tooltalk-message-attribute m 'arg_val 0))
	 (window (get-tooltalk-message-attribute m 'arg_ival 1))
	 (args (list '(nomenu . "on") (cons 'name name)))
	 (screen (x-create-screen args window)))
    (set-tooltalk-message-attribute (screen-name screen) m 'arg_val 2)
    (return-tooltalk-message m 'reply)))



(defun tooltalk-load-file-handler (m p)
  (let ((path (get-tooltalk-message-attribute m 'file)))
    (condition-case error-info 
	(progn
	  (load-file path)
	  (return-tooltalk-message m 'reply))
      (error 
       (let ((error-string (tooltalk::prin1-to-string-carefully error-info)))
	(set-tooltalk-message-attribute error-string m 'status_string)
	(return-tooltalk-message m 'fail))))))


(defun tooltalk-status-handler (m p)
  (return-tooltalk-message m 'reply))

