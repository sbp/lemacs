;; Holiday functions.
;; Copyright (C) 1990 Free Software Foundation, Inc.

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

;; This collection of functions implements the holiday features as described
;; in calendar.el.

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;; Technical details of all of the holiday calculations can be found in
;; ``Calendrical Calculations'' by Edward M. Reingold and Nachum Dershowitz,
;; Report Number UIUCDCS-R-89-1541, Department of Computer Science,
;; University of Illinois, Urbana, Illinois, September, 1989.

(require 'calendar)
(provide 'holidays)

(defun holidays ()
  "Display the holidays for last month, this month, and next month.
This function is suitable for execution in a .emacs file."
  (interactive)
  (save-excursion
    (let* ((date (calendar-current-date))
           (displayed-month (extract-calendar-month date))
           (displayed-year (extract-calendar-year date)))
      (list-calendar-holidays))))

(defun check-calendar-holidays (date)
  "Check the list of holidays for any that occur on DATE.
The value returned is a list of strings of relevant holiday descriptions.
The holidays are those in the list calendar-holidays."
  (let* ((displayed-month (extract-calendar-month date))
         (displayed-year (extract-calendar-year date))
         (h (calendar-holiday-list))
         (holiday-list))
    (while h
      (if (calendar-date-equal date (car (car h)))
          (setq holiday-list (append holiday-list (cdr (car h)))))
      (setq h (cdr h)))
    holiday-list))

(defun calendar-cursor-holidays ()
  "Find holidays for the date specified by the cursor in the calendar window."
  (interactive)
  (message "Checking holidays...")
  (let* ((date (calendar-cursor-to-date))
         (date-string (calendar-date-string date))
         (holiday-list (check-calendar-holidays date))
         (holiday-string (mapconcat 'identity holiday-list ";  "))
         (msg (format "%s:  %s" date-string holiday-string)))
    (if (not holiday-list)
        (message "No holidays known for %s" date-string)
      (if (<= (length msg) (screen-width))
          (message msg)
        (set-buffer (get-buffer-create holiday-buffer))
        (setq buffer-read-only nil)
        (setq mode-line-format
              (format "--------------------------%s%%-"
                      date-string))
        (erase-buffer)
        (insert (mapconcat 'identity holiday-list "\n"))
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (display-buffer holiday-buffer)
        (message "Checking holidays...done")))))

(defun mark-calendar-holidays ()
  "Mark notable days in the calendar window."
  (interactive)
  (setq mark-holidays-in-calendar t)
  (message "Marking holidays...")
  (let ((holiday-list (calendar-holiday-list)))
    (while holiday-list
      (mark-visible-calendar-date
       (car (car holiday-list)) calendar-holiday-marker)
      (setq holiday-list (cdr holiday-list))))
  (message "Marking holidays...done"))

(defun list-calendar-holidays ()
  "Create a buffer containing the holidays for the current calendar window.
The holidays are those in the list calendar-notable-days.  Returns t if any
holidays are found, nil if not."
  (interactive)
  (message "Looking up holidays...")
  (let ((holiday-list (calendar-holiday-list))
        (m1 displayed-month)
        (y1 displayed-year)
        (m2 displayed-month)
        (y2 displayed-year))
    (if (not holiday-list)
        (progn
          (message "Looking up holidays...none found")
          nil)
      (set-buffer (get-buffer-create holiday-buffer))
      (setq buffer-read-only nil)
      (increment-calendar-month m1 y1 -1)
      (increment-calendar-month m2 y2 1)
      (setq mode-line-format
            (format "-------------Notable Dates from %s, %d to %s, %d%%-"
                    (calendar-month-name m1) y1 (calendar-month-name m2) y2))
      (erase-buffer)
      (insert
       (mapconcat
        '(lambda (x) (concat (calendar-date-string (car x))
                             ": " (car (cdr x))))
        holiday-list "\n"))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer holiday-buffer)
      (message "Looking up holidays...done")
      t)))

(defun calendar-holiday-list ()
  "Form the list of holidays that occur on dates in the calendar window.
The holidays are those in the list calendar-holidays."
  (let ((p calendar-holidays)
        (holiday-list))
    (while p
      (let* ((function-name
              (intern (format "calendar-holiday-function-%s" (car (car p)))))
             (holidays
              (if (cdr (car p));; optional arguments
                  (funcall function-name (cdr (car p)))
                (funcall function-name))))
        (if holidays
            (setq holiday-list (append holidays holiday-list))))
      (setq p (cdr p)))
    (setq holiday-list (sort holiday-list 'calendar-date-compare))))

;; Below are the functions that calculate the dates of holidays; these
;; are called by the funcall in the function calendar-holiday-list.  If you
;; write other such functions, be sure to imitate the style used below,
;; including the evaluation of each element in the list that constitutes
;; the argument to the function.  If you don't do this evaluation, the
;; list calendar-holidays cannot contain expressions (as, for example, in
;; the entry for the Islamic new year.  Also remember that each function
;; must return a list of items of the form ((month day year) string);
;; the date (month day year) should be visible in the calendar window.

(defun calendar-holiday-function-fixed (x)
  "Returns the corresponding Gregorian date, if visible in the window, to
month, year where month is (car X) and year is (car (cdr X)).  If it is
visible, the value returned is the list (((month day year) string)) where
string is (car (nthcdr 2 X)).  Returns nil if it is not visible in the
current calendar window."
  (let* ((month (eval (car x)))
         (day (eval (car (cdr x))))
         (string (eval (car (nthcdr 2 x))))
         (m displayed-month)
         (y displayed-year))
    (increment-calendar-month m y (- 11 month))
    (if (> m 9)
      (list (list (list month day y) string)))))

(defun calendar-holiday-function-float (x)
  "Returns the corresponding Gregorian date, if visible in the window, to the
n-th occurrence (negative counts from the end of the month) of dayname in
month, year where month is (car X), year is (car (cdr X)), n is
(car (nthcdr 2 X)).  If it is visible, the value returned is the list
(((month day year) string)) where string is (car (nthcdr 3 X)).
Returns nil if it is not visible in the current calendar window."
  (let* ((month (eval (car x)))
         (dayname (eval (car (cdr x))))
         (n (eval (car (nthcdr 2 x))))
         (string (eval (car (nthcdr 3 x))))
         (m displayed-month)
         (y displayed-year))
    (increment-calendar-month m y (- 11 month))
    (if (> m 9)
      (list (list (calendar-nth-named-day n dayname month y) string)))))

(defun calendar-holiday-function-julian (x)
  "Returns the corresponding Gregorian date, if visible in the window, to the
Julian date month, year where month is (car X) and year is (car (cdr X)).
If it is visible, the value returned is the list (((month day year) string))
where string is (car (nthcdr 2 X)).  Returns nil if it is not visible in the
current calendar window."
  (let* ((month (eval (car x)))
         (day (eval (car (cdr x))))
         (string (eval (car (nthcdr 2 x))))
         (m1 displayed-month)
         (y1 displayed-year)
         (m2 displayed-month)
         (y2 displayed-year)
         (year))
    (increment-calendar-month m1 y1 -1)
    (increment-calendar-month m2 y2 1)
    (let* ((start-date (calendar-absolute-from-gregorian
                        (list m1 1 y1)))
           (end-date (calendar-absolute-from-gregorian
                      (list m2 (calendar-last-day-of-month m2 y2) y2)))
           (julian-start (calendar-julian-from-absolute start-date))
           (julian-end (calendar-julian-from-absolute end-date))
           (julian-y1 (extract-calendar-year julian-start))
           (julian-y2 (extract-calendar-year julian-end)))
      (setq year (if (< 10 month) julian-y1 julian-y2))
      (let ((date (calendar-gregorian-from-absolute
                   (calendar-absolute-from-julian
                    (list month day year)))))
        (if (calendar-date-is-visible-p date)
            (list (list date string)))))))

(defun calendar-holiday-function-islamic (x)
  "Returns the corresponding Gregorian date, if visible in the window, to the
Islamic date month, year where month is (car X) and year is (car (cdr X)).
If it is visible, the value returned is the list (((month day year) string))
where string is (car (nthcdr 2 X)).  Returns nil if it is not visible in
the current calendar window."
  (let* ((month (eval (car x)))
         (day (eval (car (cdr x))))
         (string (eval (car (nthcdr 2 x))))
         (islamic-date (calendar-islamic-from-absolute
                        (calendar-absolute-from-gregorian
                         (list displayed-month 15 displayed-year))))
         (m (extract-calendar-month islamic-date))
         (y (extract-calendar-year islamic-date))
        (date))
    (if (< m 1)
        nil;;   Islamic calendar doesn't apply.
      (increment-calendar-month m y (- 10 month))
      (if (> m 7);;  Islamic date might be visible
          (let ((date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-islamic (list month day y)))))
            (if (calendar-date-is-visible-p date)
                (list (list date string))))))))

(defun calendar-holiday-function-hebrew (x)
  "Returns the corresponding Gregorian date, if visible in the window, to the
Hebrew date month, year where month is (car X) and year is (car (cdr X)).
If it is visible, the value returned is the list (((month day year) string))
where string is (car (nthcdr 2 X)).  Returns nil if it is not visible in
the current calendar window."
  (let* ((month (eval (car x)))
         (day (eval (car (cdr x))))
         (string (eval (car (nthcdr 2 x)))))
    (if (memq displayed-month;;  This test is only to speed things up a bit;
              (list          ;;  it works fine without the test too.
               (if (< 11 month) (- month 11) (+ month 1))
               (if (< 10 month) (- month 10) (+ month 2))
               (if (<  9 month) (- month  9) (+ month 3))
               (if (<  8 month) (- month  8) (+ month 4))))
        (let ((m1 displayed-month)
              (y1 displayed-year)
              (m2 displayed-month)
              (y2 displayed-year)
              (year))
          (increment-calendar-month m1 y1 -1)
          (increment-calendar-month m2 y2 1)
          (let* ((start-date (calendar-absolute-from-gregorian
                              (list m1 1 y1)))
                 (end-date (calendar-absolute-from-gregorian
                            (list m2 (calendar-last-day-of-month m2 y2) y2)))
                 (hebrew-start (calendar-hebrew-from-absolute start-date))
                 (hebrew-end (calendar-hebrew-from-absolute end-date))
                 (hebrew-y1 (extract-calendar-year hebrew-start))
                 (hebrew-y2 (extract-calendar-year hebrew-end)))
            (setq year (if (< 6 month) hebrew-y2 hebrew-y1))
            (let ((date (calendar-gregorian-from-absolute
                         (calendar-absolute-from-hebrew
                          (list month day year)))))
              (if (calendar-date-is-visible-p date)
                  (list (list date string)))))))))

(defun calendar-holiday-function-advent ()
  "Date of Advent, if visible in calendar window."
  (let ((year displayed-year)
        (month displayed-month))
    (increment-calendar-month month year -1)
    (let ((advent (calendar-gregorian-from-absolute
                   (calendar-dayname-on-or-before 0
                    (calendar-absolute-from-gregorian
                     (list 12 3 year))))))
      (if (calendar-date-is-visible-p advent)
          (list (list advent "Advent"))))))

(defun calendar-holiday-function-easter-etc ()
  "List of dates related to Easter, as visible in calendar window."
  (if (> displayed-month 5)
      nil;; Ash Wednesday, Good Friday, and Easter are not visible.
    (let* ((century (1+ (/ displayed-year 100)))
           (shifted-epact        ;; Age of moon for April 5...
            (mod (+ 14 (* 11 (mod displayed-year 19));;     ...by Nicaean rule
                    (-           ;; ...corrected for the Gregorian century rule
                     (/ (* 3 century) 4))
                    (/    ;; ...corrected for Metonic cycle inaccuracy.
                     (+ 5 (* 8 century)) 25)
                    (* 30 century));;              Keeps value positive.
                 30))
           (adjusted-epact       ;;  Adjust for 29.5 day month.
            (if (or (= shifted-epact 0)
                    (and (= shifted-epact 1) (< 10 (mod displayed-year 19))))
                (1+ shifted-epact)
              shifted-epact))
           (paschal-moon       ;; Day after the full moon on or after March 21.
            (- (calendar-absolute-from-gregorian (list 4 19 displayed-year))
               adjusted-epact))
           (abs-easter (calendar-dayname-on-or-before 0 (+ paschal-moon 7)))
           (easter (calendar-gregorian-from-absolute abs-easter))
           (good-friday (calendar-gregorian-from-absolute (- abs-easter 2)))
           (ash-weds (calendar-gregorian-from-absolute (- abs-easter 46)))
           (output-list))
      (if (calendar-date-is-visible-p ash-weds)
          (setq output-list (list (list ash-weds "Ash Wednesday"))))
      (if (calendar-date-is-visible-p good-friday)
          (setq output-list
                (append (list (list good-friday "Good Friday")) output-list)))
      (if (calendar-date-is-visible-p easter)
          (setq output-list
                (append (list (list easter "Easter Sunday")) output-list)))
      output-list)))

(defun calendar-holiday-function-rosh-hashanah-etc ()
  "List of dates related to Rosh Hashanah, as visible in calendar window."
  (if (or (< displayed-month 8)
          (> displayed-month 11))
      nil;; None of the dates are visible
    (let* ((abs-r-h (calendar-absolute-from-hebrew
                      (list 7 1 (+ displayed-year 3761))))
           (rosh-h (calendar-gregorian-from-absolute abs-r-h))
           (yom-k (calendar-gregorian-from-absolute (+ abs-r-h 9)))
           (suc (calendar-gregorian-from-absolute (+ abs-r-h 14)))
           (shemini (calendar-gregorian-from-absolute (+ abs-r-h 21)))
           (torah (calendar-gregorian-from-absolute (+ abs-r-h 22)))
           (output-list))
      (if (calendar-date-is-visible-p rosh-h)
          (setq output-list
                (list (list rosh-h
                            (format "Rosh HaShanah %d"
                                     (+ 3761 displayed-year))))))
      (if (calendar-date-is-visible-p yom-k)
          (setq output-list
                (append (list (list yom-k "Yom Kippur")) output-list)))
      (if (calendar-date-is-visible-p suc)
          (setq output-list
                (append (list (list suc "Sukkot")) output-list)))
      (if (calendar-date-is-visible-p shemini)
          (setq output-list
                (append (list (list shemini "Shemini Azereth")) output-list)))
      (if (calendar-date-is-visible-p torah)
          (setq output-list
                (append (list (list torah "Simhat Torah")) output-list)))
      output-list)))

