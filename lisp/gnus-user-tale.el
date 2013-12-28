;;; User Contributed Software for GNUS newsreader
;; Copyright (C) 1989 Masanobu UMEDA

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; The program in this file is contributed by tale@pawl.rpi.edu (David
;; C Lawrence), and is not part of the standard distribution of GNUS.
;; This may be included in the future releases of GNUS.  Please do not
;; send me any flame on it.

;;Date: 28 Feb 89 02:41:31 GMT
;;From: tale@pawl.rpi.edu (David C Lawrence)
;;Organization: The Octagon Room
;;Subject: A different article mode-line
;;To: info-gnus-english@cis.ohio-state.edu
;;
;;Well, I've seen at least three other people ask for something like
;;this and I've also wanted it, so I went in and made some changes.
;;Basically, this is what the following code does:
;;
;; o One new function is added; others merely have their current
;;definitions modified to accept the new function.
;;
;; o The exception to the first point is gnus-Article-set-mode-line.
;;The way I have defined it is to show a modeline similar to this:
;;
;;--- GNUS: comp.theory.cell-automata  2 more  5:32pm 2.27[0]  (GNUS Article)--46%
;;
;;Note that comp.theory.cell-automata is one of the longest group names
;;and everything fits into the modeline happily; only one or two group
;;would break that.  
;;
;; o The display-time-string is there so because I like it for both
;;telling time and getting my mail notification.
;; 
;; o The percentage displayed on the mode line is how many lines of the
;;total buffer size are not beyond the bottom of the window, like more(1).
;;
;; o The "2 more" refers to how many unmarked articles remain in the
;;group. Spaces are printed if there are no more unmarked articles.
;;
;;I did the "2 more" thing rather than paging because knowing that the
;;range is {1191-2010} and that I am at 2009 is mostly useless to me;
;;since I follow subjects, which particular article I am on in the group
;;is meaningless as far as helping me predict when I'll be on my way to
;;the next group.
;;
;;This was only given non-rigourous, but seemingly adequate, testing.
;;If you find a problem with it, please let me know so I can improve it.
;;
;;Dave

(provide 'gnus-user-tale)

(setq gnus-Article-mode-hook
      '(lambda ()
         (make-local-variable 'gnus-Article-head-to-window-bottom)
         (kill-local-variable 'global-mode-string)
         (setq mode-line-format
               (list (purecopy "")
                     'mode-line-modified 'mode-line-buffer-identification
                     (purecopy "  ")
                     'global-mode-string
                     (purecopy "  %[(")
                     'mode-name 'minor-mode-alist "%n"
                     (purecopy ")%]--")
                     'gnus-Article-head-to-window-bottom
                     (purecopy "-%-")))))

(defun gnus-Article-set-percent (&optional new-article)
  "Set gnus-Article-head-to-window-bottom as a string which represents the
percentage of total Article lines that are before the bottom of the window.
Also forces mode-line update.  Optional NEW-ARTICLE is necessary when a new
article is selected."
  (save-excursion
    (save-restriction
      (setq gnus-Article-head-to-window-bottom
            (if new-article
                (progn
;;                  (vertical-motion (- (screen-height)
;;                                      gnus-subject-lines-height 3))
                  (vertical-motion (- (screen-height)
				      (save-excursion
					(set-buffer gnus-Subject-buffer)
					(window-height))
				      3))
                  ;; The next bit is in case the last real line is
                  ;; (will be) visible on the screen.
                  (move-to-column (- (screen-width) 3))
                  (if (not (eobp)) (forward-char 1))
                  ;; Might only be at end-of-page
                  (widen)
                  (if (eobp) "All"
                    (concat (/ (* 100 (count-lines (point-min) (point)))
                               (count-lines (point-min) (point-max))) "%")))
              (move-to-window-line (- (window-height) 2))
              ;; Same deal as above
              (move-to-column (- (window-width) 3))
              (if (not (eobp)) (forward-char 1))
              ;; Might only be at end-of-page
              (widen)
              (if (eobp) "Bot"
                (concat (/ (* 100 (count-lines (point-min) (point)))
                           (count-lines (point-min) (point-max))) "%"))))))
  (set-buffer-modified-p t))

(defun gnus-Article-set-mode-line ()
  "Set Article mode line string."
  (setq mode-line-buffer-identification
	(list 17
	      (format "GNUS: %s  %s"
		      gnus-newsgroup-name
                      (let ((unmarked (length (gnus-set-difference
                                               gnus-newsgroup-unreads
                                               gnus-newsgroup-marked))))
                        (if (= 0 unmarked) "      "
                          (concat unmarked " more"))))))
  ;; Even if we did this when narrowing to page, do it again as a
  ;; new article.
  (gnus-Article-set-percent 1)
  (set-buffer-modified-p t))

(defun gnus-Article-next-page (lines)
  "Show next page of current article.
If end of article, return non-nil. Otherwise return nil.
Argument LINES specifies lines to be scrolled up."
  (interactive "P")
  (move-to-window-line -1)
  (if (eobp)
      (if (or (not gnus-break-pages)
              (string-match "All\\|Bot" gnus-Article-head-to-window-bottom)
	      (save-restriction (widen) (eobp))) ;Real end-of-buffer?
	  t
	(gnus-narrow-to-page 1)		;Go to next page.
	nil
	)
    (scroll-up lines)
    (gnus-Article-set-percent)
    nil
    ))

(defun gnus-Article-prev-page (lines)
  "Show previous page of current article.
Argument LINES specifies lines to be scrolled down."
  (interactive "P")
  (move-to-window-line 0)
  (if (and gnus-break-pages
	   (bobp)
	   (not (save-restriction (widen) (bobp)))) ;Real beginning-of-buffer?
      (progn
	(gnus-narrow-to-page -1) ;Go to previous page.
	(goto-char (point-max))
	(recenter -1))
    (scroll-down lines)
    (gnus-Article-set-percent)))

;; I don't want to copy the whole function to my gnus-etc.el, so just
;; rebind it here.  The only change the original function needs,
;; however is to include (gnus-Article-set-percent) as the last 
;; function called by gnus-narrow-to-page.
(or (fboundp 'original-gnus-narrow-to-page)
    (fset 'original-gnus-narrow-to-page
          (symbol-function 'gnus-narrow-to-page)))

(defun gnus-narrow-to-page (&optional arg)
  "Make text outside current page invisible except for page delimiter.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in.
NOTE: This function has been modified to also update the Article buffer
mode-line after narrowing."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (original-gnus-narrow-to-page arg)
  (gnus-Article-set-percent))

;; This is a function which I have as my gnus-Save-newsrc-hook.  I like
;; it because it keeps my .newsrc and .newsrc.el trimmed down by not
;; letting unsubscribed groups build up long strings of marked article
;; ranges because of cross-posted articles.  (This saves over 14k on my
;; average newsrc.el size.)  It also keeps a fairly organized newsrc by
;; sorting unsubscribed groups alphabetically and putting them all at the
;; end of my newsrc.  It doesn't do anything to the order of subscribed
;; groups.   If you only desire one feature or the other you can call it
;; with the appropriate optional arguments.

;;; Based on an idea by David.Detlefs@DLD.AVALON.CS.CMU.EDU
;;; Suitable for calling as a gnus-Save-newsrc-hook.
(defun gnus-reorder-newsrc-file (&optional nosort nocompress)
  (let (gnus-unsub-assoc reordered)
    (save-excursion
      (set-buffer (get-file-buffer gnus-current-startup-file))
      (goto-char (point-max))
      ;; protect against a totally unsubscribed .newsrc
      (if (not (re-search-backward "^.*:" nil t)) nil
        (re-search-forward "^.*! " nil t)
        (setq gnus-unsub-assoc
              (memq (assoc
                     (buffer-substring (- (point) 2)
                                       (progn (beginning-of-line) (point)))
                     gnus-newsrc-assoc) gnus-newsrc-assoc))
        (while (re-search-backward "^.*! " nil t)
          (let ((position (point)) unsub-line)
            (setq gnus-unsub-assoc
                  (cons (assoc
                         (buffer-substring
                          (point) (progn (skip-chars-forward "^!") (point)))
                         gnus-newsrc-assoc) gnus-unsub-assoc)
                  unsub-line (buffer-substring position
                                               (progn (beginning-of-line)
                                                 (next-line 1) (point)))
                  reordered t)
            (delete-region position (point))
            (goto-char (point-max)) (insert unsub-line) (goto-char position)))
        (if (or nosort (not reordered)) nil
          (sort-lines nil (progn (re-search-forward "! " nil t)
                                 (beginning-of-line) (point)) (point-max))
          (setq gnus-unsub-assoc (sort gnus-unsub-assoc 'gnus-assoc-lessp)))
        (if nocompress nil
          (goto-char (point-min))
          (re-search-forward "! " nil t)
          (while (re-search-forward "," nil t)
            (let ((compress-group
                   (assoc (buffer-substring
                           (progn (beginning-of-line) (point))
                           (progn (skip-chars-forward "^!") (point)))
                          gnus-newsrc-assoc)) first-unread)
              (delete-region (progn (skip-chars-forward "^-,") (point))
                             (progn (end-of-line) (skip-chars-backward "^-,")
                                    (point)))
              (insert "-")
              (setcdr (nth 2 compress-group)
                      (cdr (nth (1- (length compress-group)) compress-group)))
              (setcdr (memq (nth 2 compress-group) compress-group) nil))))
        (setq gnus-newsrc-assoc
              (append (gnus-set-difference gnus-newsrc-assoc gnus-unsub-assoc)
                      gnus-unsub-assoc))))))

(defun gnus-assoc-lessp (list1 list2)
  "Returns t if the car of LIST1 (a string) is less than the car of
LIST2 by doing comparison with string-lessp."
  (string-lessp (car list1) (car list2)))
