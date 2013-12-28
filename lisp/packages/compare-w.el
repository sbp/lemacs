;; Compare text between windows for Emacs.
;; Copyright (C) 1986, 1989, 1993 Free Software Foundation, Inc.

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

(provide 'compare-w)

(defvar compare-windows-whitespace "[ \t\n]+"
  "*Regular expression defining runs of whitespace for \\[compare-windows].
Changes in whitespace are optionally ignored.

The value of `compare-windows-whitespace' may instead be a function; this
function is called in each buffer, with point at the current scanning point.
The function's job is to categorize any whitespace around (including before)
point; it should also advance past any whitespace.
The function is passed one argument, the point where compare-windows
was originally called; it should not consider any text before that point.
If the function returns the same value for both buffers, then the
whitespace is considered to match, and is skipped.")

(defvar compare-ignore-case nil
  "*If the value of this variable evaluates to non-nil, \\[compare-windows]
ignores case differences.  Some useful settings: nil, t or 'case-fold-search,
meaning to track the value of the `case-fold-search' variable.")

(defun compare-windows (&optional ignore-whitespace)
  "Compare text in current window with text in next window.
Compares the text starting at point in each window,
moving over text in each one as far as they match.

A prefix arg means ignore changes in whitespace.
The variable `compare-windows-whitespace' controls how whitespace is skipped.

If `compare-ignore-case' is non-nil, changes in case are also ignored."
  (interactive "P")
  (let* ((p1 (point))
         (opoint1 p1)
         (b1 (current-buffer))
         (w2 (let* ((w (selected-window))
                    (n (next-window w)))
               (if (eq n w) 
                   (error "No other window")
                   n)))
         (p2 (window-point w2))
         (b2 (window-buffer w2))
         (opoint2 p2)
         (success t)
         (compare-ignore-case 
          ;;>>>> ARGGGH
          (eval compare-ignore-case))
         (skip-whitespace (cond ((not ignore-whitespace)
                                 nil)
                                ((stringp compare-windows-whitespace)
                                 (function (lambda (start)
                                   (let ((p (point))
                                         (found nil))
                                     (while (and (looking-at
                                                  compare-windows-whitespace)
                                                 ;; whitespace still covers p
                                                 (<= p (match-end 0))
                                                 (progn (setq p (match-end 0)
                                                              found t)
                                                        (> (point) start)))
                                       ;; keep going back until whitespace
                                       ;; doesn't extend to or past p
                                       (backward-char 1))
                                     (if found (goto-char p))
                                     found))))
                                (t
                                 compare-windows-whitespace))))
    (while success
      (setq success nil)
      ;; if interrupted, show how far we've gotten
      (goto-char p1)
      (set-window-point w2 p2)

      ;; If both buffers have whitespace next to point,
      ;; optionally skip over it.

      (if skip-whitespace
          (save-excursion
            (let* ((result1 (cond ((funcall skip-whitespace opoint1))
                                  ((= p1 opoint1) nil)
                                  (t
                                   (goto-char (1- p1))
                                   (funcall skip-whitespace opoint1))))
                   (p1a (point))
                   (result2 (cond ((progn (set-buffer b2) 
                                          (goto-char p2)
                                          (funcall skip-whitespace opoint2)))
                                  ((= p2 opoint2) nil)
                                  (t
                                   (goto-char (1- p2))
                                   (funcall skip-whitespace opoint2))))
                   (p2a (point)))
              (if (and (eq result1 result2) result1)
                  (setq p1 p1a
                        p2 p2a)))))
      
      (let ((case-fold-search compare-ignore-case))
        (setq success (compare-buffer-substrings b1 p1 nil
                                                 b2 p2 nil))
        (if (< success 0) (setq success (- success)))
        (cond ((= success 0)
               ;; whole buffer matched
               (set-buffer b1)
               (setq p1 (point-max)
                     p2 (save-excursion 
                          (set-buffer b2)
                          (point-max))
                     ;; Can't be any more successful
                     success nil))
              ((= success 1)
               ;; nothing matched
               (setq success nil))
              (t
               (setq p1 (+ p1 success -1))
               (setq p2 (+ p2 success -1))))))

    (goto-char p1)
    (set-window-point w2 p2)
    ;; Beep if nothing matched
    (if (= (point) opoint1)
	(beep))))
