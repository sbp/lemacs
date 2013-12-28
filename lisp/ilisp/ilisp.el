;;; -*-Emacs-Lisp-*-
;;;%Header
;;;
;;; Rcs_Info: ilisp.el,v 1.35 1993/09/03 02:05:07 ivan Rel $
;;;
;;; Send mail to ilisp-bug@darwin.bu.edu if you have problems.
;;;
;;; Send mail to ilisp-request@darwin.bu.edu if you want to be on the
;;; ilisp mailing list.
;;;

;;; Inferior LISP interaction package for GNU Emacs.  
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell, ccm@cs.cmu.edu.
;;;
;;; Maintained by Ivan Vazquez ivan@darwin.bu.edu

;;; This file is part of GNU Emacs.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; *****************************************************************
;;; Please read the texinfo file (via m-x info in emacs or tex it and
;;; print it out) for installation instructions.
;;; *****************************************************************

;;; This file defines a generic LISP interface that can be customized
;;; to match a specific LISP dialect.  Support is already provided for
;;; a number of common LISP dialects.  Lucid, Allegro and CMU are
;;; fully supported.  Other LISP dialects are missing features like
;;; arglist and find-source.

;;; Since this is built on top of the general command-interpreter-in-
;;; a-buffer mode (comint mode), it shares a common base
;;; functionality, and a common set of bindings, with all modes
;;; derived from comint mode.  This makes it easier to use.

;;; For documentation on the functionality provided by comint mode,
;;; and the hooks available for customizing it, see the file
;;; comint.el.

;;; Throughout this file you will find comment lines with %'s on them.
;;; These lines define sections for outline mode which I use while
;;; programming to temporarily hide code.

;;; See the documentation for ILISP mode, or read texinfo document for
;;; information.  All of the EMACS function names begin or end with
;;; lisp or ilisp to separate ilisp functions from functions in other
;;; packages.  Functions that work only in lisp buffers or that work
;;; in both lisp buffers and inferior lisp buffers use lisp, all other
;;; functions use ilisp.  If a function is intended to be used
;;; interactively, then the lisp or ilisp comes at the end of the
;;; function name, otherwise at the start.

;;;%%KNOWN BUGS
;;; 
;;; If you type multiple things to the top level before you get a
;;; prompt, the LISP may be running with the status light indicating
;;; ready.  This is because I have no way to distinguish between input
;;; to a program and that to the top level.
;;;
;;; When running a lisp on Ultrix, you need to set ilisp-program to
;;; "/bin/sh -c your/path/your-lisp-image".
;;; 
;;; If you get lisp output breaking up in weird places it almost
;;; certainly means that comint-prompt-regexp is not precise enough.
;;;
;;; I would like to eat Lucid's return from break in the process
;;; filter, but I can't tell how many newlines to eat after.

;;;%Requirements
;;(require 'symlink-fix)	; worthless
(require 'comint)
(require 'compat)
(require 'comint-ipc)
(require 'bridge)
(require 'popper)

(require 'ilisp-def)
(require 'ilisp-el)
(require 'ilisp-sym)
(require 'ilisp-inp)
(require 'ilisp-ind)

(require 'ilisp-prc)
(require 'ilisp-val)
(require 'ilisp-out)
(require 'ilisp-mov)
(require 'ilisp-key)
(require 'ilisp-prn)
(require 'ilisp-low)
(require 'ilisp-doc)
(require 'ilisp-ext)
(require 'ilisp-mod)
(require 'ilisp-dia)
(require 'ilisp-cmt)
(require 'ilisp-rng)
(require 'ilisp-hnd)
(require 'ilisp-utl)
(require 'ilisp-cmp)
(require 'ilisp-kil)
(require 'ilisp-snd)
(require 'ilisp-xfr)
(require 'ilisp-hi)
(require 'ilisp-aut)

;;; Dialects.
;;; 
;;; The user will define their autoloads to load "ilisp" when trying
;;; to run their dialect.  This will load all of the dialects in.
;;;
(require 'ilisp-cl)
(require 'ilisp-cmu)
(require 'ilisp-acl)
(require 'ilisp-kcl)
(require 'ilisp-luc)
(require 'ilisp-sch)

;;; Now run the hooks.

(run-hooks 'ilisp-site-hook)
(run-hooks 'ilisp-load-hook)

(if (not lisp-no-popper) 
    (if (and (boundp 'epoch::version) epoch::version)
	(require 'epoch-pop)
	(require 'popper)))

(if (not ilisp-mode-map) (ilisp-bindings))

(provide 'ilisp)

