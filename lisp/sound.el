;; Basic lisp subroutines for Emacs
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(defun load-sound-file (filename sound-name &optional volume)
  "Read in an audio-file and add it to the sound-alist."
  (interactive "fSound file name: \n\
SSymbol to name this sound: \n\
nVolume (0 for default): ")
  (or (symbolp sound-name) (error "sound-name not a symbol"))
  (or (null volume) (integerp volume) (error "volume not an integer or nil"))
  (let (buf data)
    (unwind-protect
	(save-excursion
	  (set-buffer (setq buf (get-buffer-create " *sound-tmp*")))
	  (erase-buffer)
	  (insert-file-contents filename)
	  (setq data (buffer-string))
	  (erase-buffer))
      (and buf (kill-buffer buf)))
    (let ((old (assq sound-name sound-alist)))
      ;; some conses in sound-alist might have been dumped with emacs.
      (if old (setq sound-alist (delq old (copy-sequence sound-alist)))))
    (setq sound-alist (cons
			(purecopy
			  (if (and volume (not (eq 0 volume)))
			      (list sound-name volume data)
			      (cons sound-name data)))
		       sound-alist)))
  sound-name)

(defun load-default-sounds ()
  "Load and install some sound files as beep-types.
This only works if you're on display 0 of a Sun SparcStation."
  (interactive)
  (message "Loading sounds...")
  (setq sound-alist nil)
  (let ((default-directory exec-directory))
    (load-sound-file "sounds/drum-beep.au"	'drum)
    (load-sound-file "sounds/quiet-beep.au"	'quiet)
    (load-sound-file "sounds/bass-snap.au"	'bass 80)
    (load-sound-file "sounds/whip.au"		'whip 70))
  (setq sound-alist (append '((default		bass)
			      (undefined-key	drum)
			      (undefined-click	drum)
			      (command-error	bass)
			      (no-completion	whip)
			      (y-or-n-p		quiet)
			      (yes-or-no-p	quiet)
			      (isearch-failed	quiet)
			      (isearch-quit	bass)
			      (auto-save-error	whip 100))
			    sound-alist))
  (message "Loading sounds...done")
  (beep nil 'quiet))
