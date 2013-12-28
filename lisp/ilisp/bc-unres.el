;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: bc-unres.el,v 1.19 1993/09/03 02:05:07 ivan Rel $
;;;%Header
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell, ccm@cs.cmu.edu.
;;;
;;; Send mail to ilisp-bug@darwin.bu.edu if you have problems.
;;;
;;; Send mail to ilisp-request@darwin.bu.edu if you want to be on the
;;; ilisp mailing list.
;;;
;;;
(require 'byte-compile "bytecomp")

(if (eq byte-compile-warnings t)
    (setq byte-compile-warnings byte-compile-default-warnings))

(setq byte-compile-warnings
      (delq 'unresolved byte-compile-warnings))

