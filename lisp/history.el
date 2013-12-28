;; Copyright (C) 1989 Free Software Foundation, Inc.
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

;; suggested generic history stuff  -- tale

;; This is intended to provided easy access to a list of elements
;; being kept as a history ring.

;; To use, variables for a list and the index to it need to be kept, and
;; a limit to how large the list can grow.  Short wrappers can than be provided
;; to interact with these functions.

;; For example, a typical application of this is in interactive processes,
;; like shell or gdb.  A history can be kept of commands that are sent
;; to the process so that they are easily retrieved for re-inspection or
;; re-use.  Using process "foo" to illustrate:

;; Variable foo-history will be the list.  foo-history-index would be the
;; pointer to the current item within the list; it is based with 0 being
;; the most recent element added to the list.  foo-history-size can be a
;; user-variable which controls how many items are allowed to exist.

;; The following functions could interactive with the list; foo-mark
;; in these examples trackes the end of output from foo-process.

;; (defun foo-history-previous (arg) ;; Suggested binding: C-c C-p
;;   "Retrieve the previous command sent to the foo process.
;; ARG means to select that message out of the list (0 is the first)."
;;   (interactive "P")
;;   (history-fetch 'foo-history 'foo-history-index (or arg 'previous)
;;                  foo-mark (point-max)))

;; foo-history-next would look practically the same, but substituting "next"
;; for "previous".  Suggested binding: C-c C-n

;; (defun foo-history-clear () ;; Suggested binding: C-c C-u
;;   "Clear the input region for the foo-process and reset history location."
;;   (interactive)
;;   (delete-region foo-mark (goto-char (point-max))))

;; To get the history on the stack, an extremely minimal function would look
;; something like this, probably bound to RET:

;; (defun foo-send ()
;;   "Send a command to foo-process."
;;   (interactive)
;;   (let ((str (buffer-substring foo-mark (goto-char (point-max)))))
;;     (insert ?\C-j)
;;     (setq foo-history-index -1) ; reset the index
;;     (set-marker foo-mark (point))
;;     (send-string foo-process str)
;;     (history-add 'foo-history str foo-history-size)))

;; ToDo: history-isearch

(provide 'history)

(defvar history-last-search ""
  "The last regexp used by history-search which resulted in a match.")

(defun history-add (list item size)
  "At the head of LIST append ITEM.  Limit the length of LIST to SIZE elements.
LIST should be the name of the list."
  (set list (append (list item) (eval list)))
  (let ((elist (eval list)))
    (if (> (length elist) size)
	(setcdr (nthcdr (1- size) elist) nil))))

(defun history-fetch (list index dir &optional beg end)
  "Retrieve an entry from LIST, working from INDEX in direction DIR.
LIST should be the name of the list, for message purposes.  INDEX should be
the name of the variable used to index the list, so it can be maintained.
DIR non-nil means to use previous entry, unless it is the symbol ``next''
to get the next entry or a number to get an absolute reference.  DIR
nil is equivalent to ``next''.

If optional numeric argument BEG is preset, it is taken as the point to insert
the entry in the current buffer, leaving point at the start of the entry.
If followed by a numeric END, the region between BEG and END will be deleted
before the entry is inserted."
  (let (str (eind (eval index)) (elist (eval list)))
    (cond
     ((numberp dir)
      (setq str (nth dir elist))
      (if str (set index dir) (message "No entry %d in %s." dir list)))
     ((or (not dir) (eq dir 'next))
      (if (= eind -1)
          (message "No next entry in %s." list)
        (set index (1- eind))
        (setq str (if (zerop eind) "" (nth (1- eind) elist)))))
     (t
      (if (>= (1+ eind) (length elist))
          (message "No previous entry in %s." list)
        (set index (1+ eind))
        (setq str (nth (1+ eind) elist)))))
    (if (not (and (integer-or-marker-p beg) str)) ()
      (if (integer-or-marker-p end) (delete-region beg end))
      (insert str)
      (goto-char beg))
    str))

(defun history-search (list index dir regexp &optional beg end)
  "In history LIST, starting at INDEX and working in direction DIR, find REGEXP.
LIST and INDEX should be their respective symbol names.  DIR nil or 'forward
means to search from the current index toward the most recent history entry.
DIR non-nil means to search toward the oldest entry.  The current entry is
not checked in either case.

If an entry is found and optional numeric argument BEG exists then the entry
will be inserted there and point left at BEG.  If numeric END also exists
then the region will be deleted between BEG and END."
  (let* ((forw (or (not dir) (eq dir 'forward))) str found
	 (eind (eval index))
	 (elist (eval list))
	 (slist (if forw
		    (nthcdr (- (length elist) eind) (reverse elist))
		  (nthcdr (1+ eind) elist))))
    (while (and (not found) slist)
      (if (string-match regexp (car slist))
	  (setq found (car slist)
		history-last-search regexp))
      (setq eind (+ (if forw -1 1) eind)
	    slist (cdr slist)))
    (if (not found)
	(error "\"%s\" not found %s in %s"
	       regexp (if forw "forward" "backward") list)	       
      (set index eind)
      (if (not (integer-or-marker-p beg)) ()
	(if (integer-or-marker-p end) (delete-region beg end))
	(insert found)
	(goto-char beg)))
    found))

(defun history-menu (list buffer &optional notemp)
  "Show the history kept by LIST in BUFFER.
This function will use ``with-output-to-temp-buffer'' unless optional third
argument NOTEMP is non-nil."
  (let ((pop-up-windows t) (line 0) 
	(menu
	 (mapconcat (function (lambda (item)
				(setq line (1+ line))
				(format (format "%%%dd: %%s"
						(int-to-string (length list)))
					line item)))
		    list "\n")))
    (if notemp
	(save-excursion
	  (insert menu)
	  (display-buffer buffer))
      (with-output-to-temp-buffer buffer (princ menu)))))
