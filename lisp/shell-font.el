;; Makes the shell buffer's prompt be bold (or whatever).
;; Copyright (C) 1992 Free Software Foundation, Inc.

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

;; Do this: (add-hook 'shell-mode-hook 'install-shell-font-prompt) 
;; and the prompt in your shell-buffers will appear in boldface.
;;
;; If you want it to be italic instead, do (copy-face 'italic 'shell-prompt).


(or (find-face 'shell-prompt) (make-face 'shell-prompt))
(or (face-differs-from-default-p 'shell-prompt (selected-screen))
    (copy-face 'bold 'shell-prompt))

(defun shell-face-process-filter (proc string)
  "A process-filter that simply inserts the string into the process's buffer,
to give the illusion of a process with no filter, but then searches backward
for text matching the comint-prompt-regexp of this buffer, and puts it in
the `shell-prompt' face."
  (save-excursion 
    (set-buffer (process-buffer proc))
    (goto-char (process-mark proc))
    (insert-before-markers string)
    (set-marker (process-mark proc) (point))
    (goto-char (point-max))
    (if (re-search-backward comint-prompt-regexp nil t)
	(progn
	  (goto-char (match-end 0))
	  (skip-chars-backward " \t")
	  (set-extent-face (make-extent (match-beginning 0) (point))
			   'shell-prompt)))))

(defun install-shell-font-prompt ()
  "Add this to your shell-mode-hook to make the prompt be printed in boldface.
The prompt uses the face called `shell-prompt'; you can alter the graphical
attributes of that with the normal face-manipulation functions."
  (set-process-filter (get-buffer-process (current-buffer))
		      'shell-face-process-filter))
