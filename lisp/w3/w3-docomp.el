;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autoload statements so we don't get lots of unknown functions when
;;; compiling.  Just for clean-ness.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar html-mode-map nil "")
(defvar *noconv* nil "")
(defvar *internal* nil "")
(defvar mouse-map nil "")
(defvar x-button-middle nil "")
(defvar nntp/connection nil "")
(defvar x-button-c-middle nil "")
(defvar mouse-middle nil "")
(defvar mouse-down nil "")
(defvar hwww:link-follow nil "")
(defvar hwww:url nil "")
(defvar hwww:start nil "")
(defvar *autoconv* nil "")
(defvar *euc-japan* nil "")
(defvar nntp-server-buffer nil "")
(defvar nntp-server-buffer nil "")
(defvar nntp-version nil "")
(defvar nntp-server-buffer nil "")
(defvar ange-ftp-version nil "")
(defvar efs-version nil "")
(defvar file-coding-system nil "")
(defvar mc-flag nil "")
(defvar mc-pgp-path nil "")
(defvar mc-pgp-key-begin-line nil "")
(defvar mc-ripem-pubkeyfile nil "")
(defvar mc-default-scheme nil "")
(defvar current-menubar nil "")
(defvar default-menubar nil "")
(defvar mode-motion-hook nil "")
(defvar standard-display-table nil "")
(defvar nntp-server-name "")
(defvar gnus-default-nntp-server "")
(defvar gnus-nntp-server "")

(load "bytecomp" t t nil)
;; Emacs 19 byte compiler complains about too much stuff by default.
;; Turn off most of the warnings here.
(setq byte-compile-warnings '(free-vars))

(let ((x (get 'extent-data 'byte-obsolete-info))
      (y '(extent-data set-extent-attribute set-extent-data)))
  (cond
   (x					; stifle lucid warnings
    (while y				; when compiling old code in new lucid
      (if (setq x (get (car y) 'byte-obsolete-info))
	  (put (car y) 'byte-compile (cdr x)))
      (setq y (cdr y))))
   (t nil)))
