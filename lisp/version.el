;; Record version number of Emacs.
;; Copyright (C) 1985, 1991, 1992, 1993, 1994
;; Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;; The following line is modified automatically
;; by loading inc-version.el, each time a new Emacs is dumped.
(defconst emacs-version "19.10" "\
Version numbers of this version of Emacs.")

(setq emacs-version (purecopy (concat emacs-version " Lucid")))

(defconst emacs-build-time (current-time-string) "\
Time at which Emacs was dumped out.")

(defconst emacs-build-system (system-name))

(defconst emacs-major-version
  (progn (or (string-match "^[0-9]+" emacs-version)
	     (error "emacs-version unparsable"))
	 (string-to-number (substring emacs-version
				      (match-beginning 0) (match-end 0))))
  "Major version number of this version of Emacs, as an integer.
Warning, this variable did not exist in emacs versions earlier than:
  FSF Emacs:   19.23
  Lucid Emacs: 19.10")

(defconst emacs-minor-version
  (progn (or (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
	     (error "emacs-version unparsable"))
	 (string-to-number (substring emacs-version
				      (match-beginning 1) (match-end 1))))
  "Minor version number of this version of Emacs, as an integer.
Warning, this variable did not exist in emacs versions earlier than:
  FSF Emacs:   19.23
  Lucid Emacs: 19.10")


(defun emacs-version () "\
Return string describing the version of Emacs that is running."
  (interactive)
  (if (interactive-p)
      (message "%s" (emacs-version))
    (format "GNU Emacs %s of %s %s on %s (%s)"
	    emacs-version
	    (substring emacs-build-time 0
		       (string-match " *[0-9]*:" emacs-build-time))
	    (substring emacs-build-time
		       (string-match "[0-9]*$" emacs-build-time))
	    emacs-build-system system-type)))


;; Put the emacs version number into the `pure[]' array in a form that
;; `what(1)' can extract from the executable or a core file.  We don't
;; actually need this to be pointed to from lisp; pure objects can't
;; be GCed.
(purecopy (concat "\n@" "(#)" (emacs-version)
		  "\n@" "(#)" "Configuration: " system-configuration "\n"))

;;Local variables:
;;version-control: never
;;End:
