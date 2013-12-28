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
(or (face-differs-from-default-p 'shell-prompt)
    (copy-face 'bold 'shell-prompt))

(defun shell-hack-prompt-font (limit)
  "Search backward from point-max for text matching the comint-prompt-regexp,
and put it in the `shell-prompt' face.  LIMIT is the left bound of the search."
  (save-excursion
    (goto-char (point-max))
    (save-match-data
     (cond ((re-search-backward comint-prompt-regexp limit t)
	    (goto-char (match-end 0))
	    (skip-chars-backward " \t")
	    (set-extent-face (make-extent (match-beginning 0) (point))
			     'shell-prompt))))))

(defun shell-face-process-filter (proc string)
  "A process-filter that simply inserts the string into the process's buffer,
to give the illusion of a process with no filter, but then searches backward
for text matching the comint-prompt-regexp of this buffer, and puts it in
the `shell-prompt' face."
  (save-excursion 
    (set-buffer (process-buffer proc))
    (goto-char (process-mark proc))
    (let* ((p (point))
	   (ie (and comint-last-input-end
		    (marker-position comint-last-input-end)))
	   (w (get-buffer-window (current-buffer)))
	   (ws (and w (window-start w))))
      (insert-before-markers string)
      ;; the insert-before-markers may have screwed window-start
      ;; and likely moved comint-last-input-end.  This is why the
      ;; insertion-reaction should be a property of markers, not
      ;; of the function which does the inserting.
      (if ws (set-window-start w ws t))
      (if ie (set-marker comint-last-input-end ie))
      (set-marker (process-mark proc) (point))
      (shell-hack-prompt-font p))))

(defun install-shell-font-prompt ()
  "Add this to your shell-mode-hook to make the prompt be printed in boldface.
The prompt uses the face called `shell-prompt'; you can alter the graphical
attributes of that with the normal face-manipulation functions."
  (set-process-filter (get-buffer-process (current-buffer))
		      'shell-face-process-filter))
