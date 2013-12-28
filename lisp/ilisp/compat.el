;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: compat.el,v 1.25 1993/09/03 02:05:07 ivan Rel $
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
;;;
;;; Compatability between emacs 18 and 19
;;;
;;;
(defconst ilisp-emacs-version-id
  (cond ((string-match "Lucid" emacs-version)
	 (if (string-match "^19.[0-7][^0-9]" emacs-version)
	     'lucid-19
	   'lucid-19-new))
	((string-match "^19" emacs-version)
	 'fsf-19)
	(t 'fsf-18))
  "What version of emacs we are running. Set in compat.el")


      

(if (eq ilisp-emacs-version-id 'fsf-18)
    ;; Hook stuff--this should really be a part of emacs-lisp anyway
    (defun add-hook (hook function)
      "Arguments are HOOK and FUNCTION. Add FUNCTION to HOOK's list.
FUNCTION is not added if it's already on the list."
      (set hook
	   (if (boundp hook)
	       (let ((value (symbol-value hook)))
		 (if (and value (or (not (consp value)) (eq (car value) 'lambda)))
		     (setq value (cons value nil)))
		 (if (not (comint-mem function value))
		     (setq value (append value (list function))))
		 value)
	     (list function)))))


;;;
;;; COMINT 
;;;
;;; FSF, Lucid and 18 use different versions of comint with
;;; incompatible interface variables and functions.  Hooray.
;;;

;;; To avoid compilation warnings, give these defvars some values. ugh.
(if (string-match "2\.03" comint-version)
    (fset 'comint-mem 'member))


(defun ilisp-get-input-ring ()
  "Use instead of get-input-ring coming-input-ring or input-ring."
  (cond ((eq ilisp-emacs-version-id 'lucid-19)
	 (get-input-ring))
	((or (eq ilisp-emacs-version-id 'fsf-19) 
	     (eq ilisp-emacs-version-id 'lucid-19-new))
	 comint-input-ring)
	(t input-ring)))


(defun ilisp-ring-insert (ring input)
  (if (eq ilisp-emacs-version-id 'lucid-19)
      (ring-insert-new ring input)
      (ring-insert ring input)))

(defun ilisp-temp-buffer-show-function-symbol ()
  (if (eq ilisp-emacs-version-id 'fsf-18) 
      'temp-buffer-show-hook
    'temp-buffer-show-function))

(defun set-ilisp-temp-buffer-show-function (val)
  (if (eq ilisp-emacs-version-id 'fsf-18) 
      (setq temp-buffer-show-hook val)
    (setq temp-buffer-show-function val)))

(defun ilisp-temp-buffer-show-function ()
  (if (eq ilisp-emacs-version-id 'fsf-18) 
      temp-buffer-show-hook
    temp-buffer-show-function))

(defun ilisp-input-ring-index ()
  (if (or (eq ilisp-emacs-version-id 'fsf-19)
	  (eq ilisp-emacs-version-id 'lucid-19-new))
      comint-input-ring-index
    input-ring-index))

(defun set-ilisp-input-ring-index (n)
  (if (or (eq ilisp-emacs-version-id 'fsf-19)
	  (eq ilisp-emacs-version-id 'lucid-19-new))
      (setq comint-input-ring-index n)
    (setq input-ring-index n)))

(defun ilisp-input-ring-size ()
  (if (or (eq ilisp-emacs-version-id 'fsf-19)
	  (eq ilisp-emacs-version-id 'lucid-19-new))
      comint-input-ring-size
    input-ring-size))

(defun set-ilisp-input-ring-size (n)
  (if (or (eq ilisp-emacs-version-id 'fsf-19)
	  (eq ilisp-emacs-version-id 'lucid-19-new))
      (setq comint-input-ring-size n)
    (setq input-ring-size n)))

;;;
(provide 'compat)
