;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-rng.el,v 1.18 1993/09/03 02:05:07 ivan Rel $
;;;%Header
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell, ccm@cs.cmu.edu.
;;;
;;; Send mail to ilisp-bug@darwin.bu.edu if you have problems.
;;;
;;; Send mail to ilisp-request@darwin.bu.edu if you want to be on the
;;; ilisp mailing list.
;;;
;;;


;;;
;;; ILISP match ring.
;;;
(defun match-ring (ring regexp start)
  "Return the index in RING of REGEXP starting at START."
  (let ((n 0)
	(len (ring-length ring)))
    (while (and (< n len) 
		(not (string-match regexp (ring-ref ring n))))
      (setq n (1+ n)))
    (if (= n len)
	nil
	n)))

;;;
(defun lisp-match-ring (regexp string &optional no-insert)
  "Match REGEXP in the input-ring of the current buffer and set the
ring variables to look like comint-previous-similar-input if found.
If not found insert STRING, unless NO-INSERT."
  (let ((n (if regexp (match-ring (ilisp-get-input-ring) regexp 0))))
    (if n
	(let ((point (progn (comint-kill-input) (point))))
	  (insert (ring-ref (ilisp-get-input-ring) n))
	  (save-excursion
	    (goto-char (+ point (length string)))
	    (skip-chars-forward "^ \t\n\)")
	    (setq point (point)))
	  (push-mark point)
	  (set-ilisp-input-ring-index n)
	  (setq this-command 'comint-previous-similar-input
		comint-last-similar-string string)
	  t)
	(if (and string (not no-insert))
	    (progn (comint-kill-input) (insert string) t)
	    nil))))

(provide 'ilisp-rng )
