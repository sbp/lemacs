;; Calendar functions.
;; Copyright (C) 1988, 1989, 1990 Free Software Foundation, Inc.

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

;; This collection of functions implements a calendar window.  It generates
;; generates a calendar for the current month, together with the previous and
;; coming months, or for any other three-month period.  The calendar can be
;; scrolled forward and backward in the window to show months in the past or
;; future; the cursor can move forward and backward by days, weeks, or months,
;; making it possible, for instance, to jump to the date a specified number of
;; days, weeks, or months from the date under the cursor.  The user can
;; display a list of holidays and other notable days for the period shown; the
;; notable days can be marked on the calendar, if desired.  The user can also
;; specify that dates having corresponding diary entries (in a file that the
;; user specifies) be marked; the diary entries for any date can be viewed in
;; a separate window.  The diary and the notable days can be viewed
;; independently of the calendar.  Dates can be translated from the (usual)
;; Gregorian calendar to the day of the year/days remaining in year, to the
;; ISO commercial calendar, to the Julian (old style) calendar, to the Hebrew
;; calendar, to the Islamic calendar, and to the French Revolutionary calendar.

;; The diary related functions are in diary.el; the holiday related functions
;; are in holiday.el

;; Comments, corrections, and improvements should be sent to
;;  Edward M. Reingold               Department of Computer Science
;;  (217) 333-6733                   University of Illinois at Urbana-Champaign
;;  reingold@cs.uiuc.edu             1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;; GNU Emacs users too numerous to list pointed out a variety of problems
;; with earlier forms of the `infinite' sliding calendar and suggested some
;; of the features included in this package.  Especially significant in this
;; regard was the suggestion of mark-diary-entries and view-diary-entries,
;; together ideas for their implementation, by
;;  Michael S. Littman		     Cognitive Science Research Group
;;  (201) 829-5155                   Bell Communications Research
;;  mlittman@wind.bellcore.com       445 South St. Box 1961 (2L-331)
;;                                   Morristown, NJ  07960

;; The algorithms for the Hebrew calendar are those of the Rambam (Rabbi Moses
;; Maimonides), from his Mishneh Torah, as implemented by
;;  Nachum Dershowitz                Department of Computer Science
;;  (217) 333-4219                   University of Illinois at Urbana-Champaign
;;  nachum@cs.uiuc.edu               1304 West Springfield Avenue
;;                                   Urbana, Illinois 61801

;; Technical details of all the calendrical calculations can be found in
;; ``Calendrical Calculations'' by Nachum Dershowitz and Edward M. Reingold,
;; Software--Practice and Experience, Volume 20, Number 9 (September, 1990),
;; pages 899-928.

(defconst calendar-version "Version 4, released November 20, 1990")

(provide 'calendar)

(defvar view-diary-entries-initially nil
  "*If T, the diary entries for the current date will be displayed on entry.
The diary is displayed in another window when the calendar is first displayed,
if the current date is visible.  The number of days of diary entries displayed
is governed by the variable `number-of-diary-entries'.")

(defvar number-of-diary-entries 1
  "*Specifies how many days of diary entries are to be displayed initially.
This variable affects the diary display when the command M-x diary is used,
or if the value of the variable `view-diary-entries-initially' is t.  For
example, if the default value 1 is used, then only the current day's diary
entries will be displayed.  If the value 2 is used, then both the current
day's and the next day's entries will be displayed.  The value can also be
a vector such as [0 2 2 2 2 4 1]; this value will cause no diary entries to
be displayed on Sunday, the current date's and the next day's diary entries
to be displayed Monday through Thursday, Friday through Monday's entries to
be displayed on Friday, and only Saturday's entries to be displayed on
Saturday.  This variable does not affect the diary display with the `d'
command from the calendar; in that case, the prefix argument controls the
number of days of diary entries displayed.")

(defvar mark-diary-entries-in-calendar nil
  "*If t, dates with diary entries will be marked in the calendar window.
The marking symbol is specified by the variable `diary-entry-marker'.")

(defvar diary-entry-marker "+"
  "*The symbol used to mark dates that have diary entries.")

(defvar view-calendar-holidays-initially nil
  "*If t, the holidays for the current three month period will be displayed
on entry.  The holidays are displayed in another window when the calendar is
first displayed.")

(defvar mark-holidays-in-calendar nil
  "*If t, dates of holidays will be marked in the calendar window.
The marking symbol is specified by the variable `calendar-holiday-marker'.")

(defvar calendar-holiday-marker "*"
  "*The symbol used to mark notable dates in the calendar.")

(defvar all-hebrew-calendar-holidays nil
  "*If nil, the holidays from the Hebrew calendar that are shown will
include only those days of such major interest as to appear on secular
calendars.  If t, the holidays shown in the calendar will include all
special days that would be shown on a complete Hebrew calendar.")

(defvar all-christian-calendar-holidays nil
  "*If nil, the holidays from the Christian calendar that are shown will
include only those days of such major interest as to appear on secular
calendars.  If t, the holidays shown in the calendar will include all
special days that would be shown on a complete Christian calendar.")

(defvar all-islamic-calendar-holidays nil
  "*If nil, the holidays from the Islamic calendar that are shown will
include only those days of such major interest as to appear on secular
calendars.  If t, the holidays shown in the calendar will include all
special days that would be shown on a complete Islamic calendar.")

(defvar initial-calendar-window-hook nil
  "*List of functions to be called when the calendar window is first opened.
The functions invoked are called after the calendar window is opened, but
once opened is never called again.  Leaving the calendar with the `q' command
and reentering it will cause these functions to be called again.")

(defvar today-visible-calendar-hook nil
  "*List of functions called whenever the current date is visible.
This can be used, for example, to replace today's date with asterisks; a
function `calendar-star-date' is included for this purpose:
    (setq today-visible-calendar-hook 'calendar-star-date)
It could also be used to mark the current date with `='; a function is also
provided for this:
    (setq today-visible-calendar-hook 'calendar-mark-today)

The corresponding variable `today-invisible-calendar-hook' is the list of
functions called when the calendar function was called when the current
date is not visible in the window.

Other than the use of the provided functions, the changing of any
characters in the calendar buffer by the hooks may cause the failure of the
functions that move by days and weeks.")

(defvar today-invisible-calendar-hook nil
  "*List of functions called whenever the current date is not visible.

The corresponding variable `today-visible-calendar-hook' is the list of
functions called when the calendar function was called when the current
date is visible in the window.

Other than the use of the provided functions, the changing of any
characters in the calendar buffer by the hooks may cause the failure of the
functions that move by days and weeks.")

(defvar diary-file "~/diary"
  "*Name of the file in which one's personal diary of dates is kept.

The file's entries are lines in any of the forms

            MONTH/DAY
            MONTH/DAY/YEAR
            MONTHNAME DAY
            MONTHNAME DAY, YEAR
            DAYNAME

at the beginning of the line; the remainder of the line is the diary entry
string for that date.  MONTH and DAY are one or two digit numbers, YEAR is
a number and may be written in full or abbreviated to the final two digits.
If the date does not contain a year, it is generic and applies to any year.
DAYNAME entries apply to any date on which is on that day of the week.
MONTHNAME and DAYNAME can be spelled in full, abbreviated to three
characters (with or without a period), capitalized or not.  Any of DAY,
MONTH, or MONTHNAME, YEAR can be `*' which matches any day, month, or year,
respectively.

The European style (in which the day precedes the month) can be used
instead, if you execute `european-calendar' when in the calendar, or set
`european-calendar-style' to t in your .emacs file.  The European forms are

            DAY/MONTH
            DAY/MONTH/YEAR
            DAY MONTHNAME
            DAY MONTHNAME YEAR
            DAYNAME

To revert to the default American style from the European style, execute
`american-calendar' in the calendar.

A diary entry can be preceded by a diary-nonmarking-symbol (ordinarily `&')
to make that entry nonmarking--that is, it will not be marked on dates in
the calendar window but will appear in a diary window.

Multiline diary entries are made by indenting lines after the first with
either a TAB or one or more spaces.

Lines not in one the above formats are ignored.  Here are some sample diary
entries (in the default American style):

     12/22/1988 Twentieth wedding anniversary!!
     &1/1. Happy New Year!
     10/22 Ruth's birthday.
     21: Payday
     Tuesday--weekly meeting with grad students at 10am
              Supowit, Shen, Bitner, and Kapoor to attend.
     1/13/89 Friday the thirteenth!!
     &thu 4pm squash game with Lloyd.
     mar 16 Dad's birthday
     April 15, 1989 Income tax due.
     &* 15 time cards due.

If the first line of a diary entry consists only of the date or day name with
no trailing blanks or punctuation, then that line will not be displayed in the
diary window; only the continuation lines will be shown.  For example, the
single diary entry

     02/11/1989
      Bill Blattner visits Princeton today
      2pm Cognitive Studies Committee meeting
      2:30-5:30 Lizzie at Lawrenceville for `Group Initiative'
      4:00pm Jamie Tappenden
      7:30pm Dinner at George and Ed's for Alan Ryan
      7:30-10:00pm dance at Stewart Country Day School

will appear in the diary window without the date line at the beginning.  This
facility allows the diary window to look neater, but can cause confusion if
used with more than one day's entries displayed.

Diary entries can be based on Lisp sexps.  For example, the diary entry

      %%(diary-block 11 1 1990 11 10 1990) Vacation

causes the diary entry \"Vacation\" to appear from November 1 through November
10, 1990.  Other functions available are `diary-float', `diary-anniversary',
`diary-cyclic', `day-of-year', `iso-date', `commercial-date', `french-date',
`hebrew-date', `islamic-date', `parasha', `omer', and `rosh-hodesh'.  See the
documentation for the function `list-sexp-diary-entries' for more details.

Diary entries based on the Hebrew and/or the Islamic calendar are also
possible, but because these are somewhat slow, they are ignored
unless you set the `nongregorian-diary-listing-hook' and the
`nongregorian-diary-marking-hook' appropriately.  See the documentation
for these functions for details.

Diary files can contain directives to include the contents of other files; for
details, see the documentation for the variable `list-diary-entries-hook'.")

(defvar diary-nonmarking-symbol "&"
  "*The symbol used to indicate that a diary entry is not to be marked in the
calendar window.")

(defvar hebrew-diary-entry-symbol "H"
  "*The symbol used to indicate that a diary entry is according to the
Hebrew calendar.")

(defvar islamic-diary-entry-symbol "I"
  "*The symbol used to indicate that a diary entry is according to the
Islamic calendar.")

(defvar diary-include-string "#include"
  "*The string used to indicate the inclusion of another file of diary entries
in diary-file.  See the documentation for the function
`include-other-diary-files'.")

(defvar sexp-diary-entry-symbol "%%"
  "*The string used to indicate a sexp diary entry in diary-file.
See the documentation for the function `list-sexp-diary-entries'.")

(defvar abbreviated-calendar-year t
  "*Interpret a two-digit year DD in a diary entry as being either 19DD or
20DD, as appropriate, for the Gregorian calendar; similarly for the Hebrew and
Islamic calendars.  If this variable is nil, years must be written in full.")

(defvar european-calendar-style nil
  "*Use the European style of dates in the diary and in any displays.  If this
variable is t, a date 1/2/1990 would be interpreted as February 1, 1990.
The accepted European date styles are

            DAY/MONTH
            DAY/MONTH/YEAR
            DAY MONTHNAME
            DAY MONTHNAME YEAR
            DAYNAME

Names can be capitalized or not, written in full, or abbreviated to three
characters with or without a period.")

(defvar american-date-diary-pattern
  '((month "/" day "[^/0-9]")
    (month "/" day "/" year "[^0-9]")
    (monthname " *" day "[^,0-9]")
    (monthname " *" day ", *" year "[^0-9]")
    (dayname "\\W"))
  "*List of pseudo-patterns describing the American patterns of date used.
See the documentation of diary-date-forms for an explanantion.")

(defvar european-date-diary-pattern
  '((day "/" month "[^/0-9]")
    (day "/" month "/" year "[^0-9]")
    (backup day " *" monthname "\\W+\\<[^*0-9]")
    (day " *" monthname " *" year "[^0-9]")
    (dayname "\\W"))
  "*List of pseudo-patterns describing the European patterns of date used.
See the documentation of diary-date-forms for an explanantion.")

(defvar diary-date-forms
  (if european-calendar-style
      european-date-diary-pattern
    american-date-diary-pattern)
  "*List of pseudo-patterns describing the forms of date used in the diary.
The patterns on the list must be MUTUALLY EXCLUSIVE and must should not match
any portion of the diary entry itself, just the date component.

A pseudo-pattern is a list of regular expressions and the keywords `month',
`day', `year', `monthname', and `dayname'.  The keyword `monthname' will
match the name of the month, capitalized or not, or its three-letter
abbreviation, followed by a period or not; it will also match `*'.
Similarly, `dayname' will match the name of the day, capitalized or not, or
its three-letter abbreviation, followed by a period or not.  The keywords
`month', `day', and `year' will match those numerical values, preceded by
arbitrarily many zeros; they will also match `*'.

The matching of the diary entries with the date forms is done with the
standard syntax table from Fundamental mode, but with the `*' changed so
that it is a word constituent.

If, to be mutually exclusive, a pseudo-pattern must match a portion of the
diary entry itself, the first element of the pattern MUST be `backup'.  This
directive causes the the date recognizer to back up to the beginning of the
current word of the diary entry, so in no case can the pattern match more
than a portion of the first word of the diary entry.")

(defvar european-calendar-display-form
  '(dayname ", " day " " monthname " " year)
  "*The pseudo-pattern that governs the way a Gregorian date is formatted
in the European style.  See the documentation of calendar-date-display-forms
for an explanantion.")

(defvar american-calendar-display-form
  '(dayname ", " monthname " " day ", " year)
  "*The pseudo-pattern that governs the way a Gregorian date is formatted
in the American style.  See the documentation of calendar-date-display-forms
for an explanantion.")

(defvar calendar-date-display-form
  (if european-calendar-style
      european-calendar-display-form
    american-calendar-display-form)
  "*The pseudo-pattern that governs the way a Gregorian date is formatted
as a string by the function `calendar-date-string'.  A pseudo-pattern is a
list of expressions that can involve the keywords `month', `day', and
`year', all numbers in string form, and `monthname' and `dayname', both
alphabetic strings.  For example, the ISO standard would use the pseudo-
pattern

       '(year \"-\" month \"-\" day)

while a typical American form would be

       '(month \"/\" day \"/\" (substring year -2))

and

       '((format \"%9s, %9s %2s, %4s\" dayname monthname day year))

would give the usual American style in fixed-length fields.

See the documentation of the function `calendar-date-string'.")

(defun european-calendar ()
  "Set the interpretation and display of dates to the European style."
  (interactive)
  (setq european-calendar-style t)
  (setq calendar-date-display-form european-calendar-display-form)
  (setq diary-date-forms european-date-diary-pattern)
  (update-calendar-mode-line))

(defun american-calendar ()
  "Set the interpretation and display of dates to the American style."
  (interactive)
  (setq european-calendar-style nil)
  (setq calendar-date-display-form american-calendar-display-form)
  (setq diary-date-forms american-date-diary-pattern)
  (update-calendar-mode-line))

(defvar print-diary-entries-hook
  '(add-diary-heading lpr-buffer (lambda nil (kill-buffer temp-buffer)))
  "*List of functions to be called after a temporary buffer is prepared
with the diary entries currently visible in the diary buffer.  The default
value adds a heading (formed from the information in the mode line of the
diary buffer), does the printing, and kills the buffer.  Other uses might
include, for example, rearranging the lines into order by day and time,
saving the buffer instead of deleting it, or changing the function used to
do the printing.")

(defvar list-diary-entries-hook nil
  "*List of functions to be called after the diary file is culled for
relevant entries. It is to be used for diary entries that are not found in
the diary file.

A function `include-other-diary-files' is provided for use as the value of
this hook.  This function enables you to use shared diary files together
with your own.  The files included are specified in the diary-file by lines
of the form

        #include \"filename\"

This is recursive; that is, #include directives in files thus included are
obeyed.  You can change the \"#include\" to some other string by changing
the variable `diary-include-string'.  When you use `include-other-diary-files'
as part of the list-diary-entries-hook, you will probably also want to use the
function `mark-included-diary-files' as part of the mark-diary-entries-hook.

For example, you could use

     (setq list-diary-entries-hook
           '(include-other-diary-files
             (lambda nil
                     (setq diary-entries-list
                           (sort diary-entries-list 'diary-entry-compare)))))
     (setq diary-display-hook 'fancy-diary-display)

in your .emacs file to cause the fancy diary buffer to be displayed with
diary entries from various included files, each day's entries sorted into
lexicographic order.")

(defvar diary-display-hook 'simple-diary-display
  "*List of functions that handle the display of the diary.

Ordinarily, this just displays the diary buffer (with holidays indicated in
the mode line), if there are any relevant entries.  At the time these
functions are called, the variable `diary-entries-list' is a list, in order
by date, of all relevant diary entries in the form of ((MONTH DAY YEAR)
STRING), where string is the diary entry for the given date.  This can be
used, for example, to handle appointment notification, prepare a different
buffer for display (perhaps combined with holidays), or produce hard copy
output.

A function `fancy-diary-display' is provided as an alternative
choice for this hook; this function prepares a special noneditable diary
buffer with the relevant diary entries that has neat day-by-day arrangement
with headings.  The fancy diary buffer will show the holidays unless the
variable `holidays-in-diary-buffer' is set to nil.  Ordinarily, the fancy
diary buffer will not show days for which there are no diary entries, even
if that day is a holiday; if you want such days to be shown in the fancy
diary buffer, set the variable `diary-list-include-blanks' to t.")

(defvar nongregorian-diary-listing-hook nil
  "*List of functions to be called for the diary file and included files as
they are processed for listing diary entries.  You can use any or all of
`list-hebrew-diary-entries', `yahrzeit-diary-entry', and
`list-islamic-diary-entries'.  The documentation for these functions
describes the style of such diary entries.")

(defvar mark-diary-entries-hook nil
  "*List of functions called after marking diary entries in the calendar.

A function `mark-included-diary-files' is also provided for use as the
mark-diary-entries-hook; it enables you to use shared diary files together
with your own.  The files included are specified in the diary-file by lines
of the form
        #include \"filename\"
This is recursive; that is, #include directives in files thus included are
obeyed.  You can change the \"#include\" to some other string by changing the
variable `diary-include-string'.  When you use `mark-included-diary-files' as
part of the mark-diary-entries-hook, you will probably also want to use the
function `include-other-diary-files' as part of the list-diary-entries-hook.")

(defvar nongregorian-diary-marking-hook nil
  "*List of functions to be called as the diary file and included files are
processed for marking diary entries.  You can use either or both of
mark-hebrew-diary-entries and mark-islamic-diary-entries.  The documentation
for these functions describes the style of such diary entries.")

(defvar diary-list-include-blanks nil
  "*If nil, do not include days with no diary entry in the list of diary
entries.  Such days will then not be shown in the the fancy diary buffer,
even if they are holidays.")

(defvar holidays-in-diary-buffer t
  "*If t, the holidays will be indicated in the mode line of the diary buffer
(or in the fancy diary buffer next to the date).  This slows down the diary
functions somewhat; setting it to nil will make the diary display faster.")

(defvar calendar-holidays
  '(
;;  General Holidays (American)
    (fixed 1 1 "New Year's Day")
    (float 1 1 3 "Martin Luther King Day")
    (fixed 2 2 "Ground Hog Day")
    (fixed 2 14 "Valentine's Day")
    (float 2 1 3 "President's Day")
    (fixed 3 17 "St. Patrick's Day")
    (fixed 4 1 "April Fool's Day")
    (float 4 0 1 "Daylight Savings Time Begins")
    (float 5 0 2 "Mother's Day")
    (float 5 1 -1 "Memorial Day")
    (fixed 6 14 "Flag Day")
    (float 6 0 3 "Father's Day")
    (fixed 7 4 "Independence Day")
    (float 9 1 1 "Labor Day")
    (float 10 1 2 "Columbus Day")
    (float 10 0 -1 "Daylight Savings Time Ends")
    (fixed 10 31 "Halloween")
    (fixed 11 11 "Veteran's Day")
    (float 11 4 4 "Thanksgiving")

;;  Christian Holidays
    (if all-christian-calendar-holidays
        (fixed 1 6 "Epiphany"))
    (easter-etc)
    (if all-christian-calendar-holidays
        (fixed 8 15 "Assumption"))
    (if all-christian-calendar-holidays
        (advent))
    (fixed 12 25 "Christmas")
    (if all-christian-calendar-holidays
        (julian 12 25 "Eastern Orthodox Christmas"))

;;  Jewish Holidays
    (rosh-hashanah-etc)
    (if all-hebrew-calendar-holidays
        (julian 11
                (let* ((m displayed-month)
                       (y displayed-year)
                       (year))
                  (increment-calendar-month m y -1)
                  (let ((year (extract-calendar-year
                               (calendar-julian-from-absolute
                                (calendar-absolute-from-gregorian
                                 (list m 1 y))))))
                    (if (zerop (% (1+ year) 4))
                        22
                      21))) "\"Tal Umatar\" (evening)"))
    (if all-hebrew-calendar-holidays
        (hanukkah)
      (hebrew 9 25 "Hanukkah"))
    (if all-hebrew-calendar-holidays
      (hebrew 10
              (let ((h-year (extract-calendar-year
                             (calendar-hebrew-from-absolute
                              (calendar-absolute-from-gregorian
                               (list displayed-month 28 displayed-year))))))
                (if (= (% (calendar-absolute-from-hebrew (list 10 10 h-year))
                        7)
                       6)
                    11 10))
              "Tzom Teveth"))
    (if all-hebrew-calendar-holidays
        (hebrew 11 15 "Tu B'Shevat"))
    (if all-hebrew-calendar-holidays
        (hebrew
         11
         (let ((m displayed-month)
               (y displayed-year))
           (increment-calendar-month m y 1)
           (let* ((h-year (extract-calendar-year
                           (calendar-hebrew-from-absolute
                            (calendar-absolute-from-gregorian
                             (list m
                                   (calendar-last-day-of-month m y)
                                   y)))))
                  (s-s
                   (calendar-hebrew-from-absolute
                    (if (=
                         (% (calendar-absolute-from-hebrew
                             (list 7 1 h-year))
                            7)
                         6)
                        (calendar-dayname-on-or-before
                         6 (calendar-absolute-from-hebrew
                            (list 11 17 h-year)))
                      (calendar-dayname-on-or-before
                       6 (calendar-absolute-from-hebrew
                          (list 11 16 h-year))))))
                  (day (extract-calendar-day s-s)))
             day))
         "Shabbat Shirah"))
    (passover-etc)
    (if (and all-hebrew-calendar-holidays
             (let* ((m displayed-month)
                    (y displayed-year)
                    (year))
               (increment-calendar-month m y -1)
               (let ((year (extract-calendar-year
                            (calendar-julian-from-absolute
                             (calendar-absolute-from-gregorian
                              (list m 1 y))))))
                 (= 21 (% year 28)))))
        (julian 3 26 "Kiddush HaHamah"))
    (if all-hebrew-calendar-holidays
        (tisha-b-av-etc))

;;  Islamic Holidays
    (islamic 1 1 (format "Islamic New Year %d"
                         (let ((m displayed-month)
                               (y displayed-year))
                           (increment-calendar-month m y 1)
                           (extract-calendar-year
                            (calendar-islamic-from-absolute
                             (calendar-absolute-from-gregorian
                              (list m (calendar-last-day-of-month m y) y)))))))
    (if all-islamic-calendar-holidays
        (islamic 1 10 "Ashura"))
    (if all-islamic-calendar-holidays
        (islamic 3 12 "Mulad-al-Nabi"))
    (if all-islamic-calendar-holidays
        (islamic 7 26 "Shab-e-Mi'raj"))
    (if all-islamic-calendar-holidays
        (islamic 8 15 "Shab-e-Bara't"))
    (islamic 9 1 "Ramadan Begins")
    (if all-islamic-calendar-holidays
        (islamic 9 27 "Shab-e Qadr"))
    (if all-islamic-calendar-holidays
        (islamic 10 1 "Id-al-Fitr"))
    (if all-islamic-calendar-holidays
        (islamic 12 10 "Id-al-Adha")))
  "List of notable days for the command M-x holidays.
Additional holidays are easy to add to the list.  The possible holiday-forms
are as follows:

    (fixed MONTH DAY STRING)   a fixed date on the Gregorian calendar
    (float MONTH DAYNAME K STRING) the Kth DAYNAME in MONTH on the Gregorian
                               calendar (0 for Sunday, etc.); K<0 means
                               count back from the end of the month
    (hebrew MONTH DAY STRING)  a fixed date on the Hebrew calendar
    (islamic MONTH DAY STRING) a fixed date on the Islamic calendar
    (julian MONTH DAY STRING)  a fixed date on the Julian calendar
    (if BOOLEAN HOLIDAY-FORM &optional HOLIDAY-FORM) gives a choice between
                               two holidays based on the value of BOOLEAN
    (FUNCTION &optional ARGS)  dates requiring special computation; ARGS,
                               if any, are passed in a list to the function
                               `calendar-holiday-function-FUNCTION'

For example, to add Bastille Day, celebrated in France on July 14, add

     (fixed 7 14 \"Bastille Day\")

to the list.  To add Hurricane Supplication Day, celebrated in the Virgin
Islands on the fourth Monday in August, add

     (float 8 1 4 \"Hurricane Supplication Day\")

to the list (the last Monday would be specified with `-1' instead of `4').
To add the last day of Hanukah to the list, use

     (hebrew 10 2 \"Last day of Hanukah\")

since the Hebrew months are numbered with 1 starting from Nisan, while to
add the Islamic feast celebrating Mohammed's birthday use

     (islamic 3 12 \"Mohammed's Birthday\")

since the Islamic months are numbered from 1 starting with Muharram.  To
add Thomas Jefferson's birthday, April 2, 1743 (Julian), use

     (julian 4 2 \"Jefferson's Birthday\")

To include a holiday conditionally, use the if form.  For example, to
include American presidential elections, which occur on the first Tuesday
after the first Monday in November of years divisble by 4, add

     (if (zerop (% displayed-year 4))
         (fixed 11
                (extract-calendar-day
                 (calendar-gregorian-from-absolute
                  (1+ (calendar-dayname-on-or-before
                       1 (+ 6 (calendar-absolute-from-gregorian
                               (list 11 1 displayed-year)))))))
                \"US Presidential Election\"))

to the list.  To include the phases of the moon, add

     (lunar-phases)

to the holiday list, where `calendar-holiday-function-lunar-phases' is an
Emacs-Lisp function that you've written to return a (possibly empty) list of
the relevant VISIBLE dates with descriptive strings such as

     (((2 6 1989) \"New Moon\") ((2 12 1989) \"First Quarter Moon\") ... )

The fixed, float, hebrew, islamic, julian and if forms are implemented by
the inclusion of the functions `calendar-holiday-function-fixed',
`calendar-holiday-function-float', `calendar-holiday-function-hebrew',
`calendar-holiday-function-islamic', `calendar-holiday-function-julian',
and `calendar-holiday-function-if', respectively.")


(defconst calendar-buffer "*Calendar*"
  "Name of the buffer used for the calendar.")

(defconst holiday-buffer "*Holidays*"
  "Name of the buffer used for the displaying the holidays.")

(defconst fancy-diary-buffer "*Fancy Diary Entries*"
  "Name of the buffer used for the optional fancy display of the diary.")

(defmacro increment-calendar-month (mon yr n)
  "Move the variables MON and YR to the month and year N months forward
if N is positive or backward if N is negative."
  (` (let (( macro-y (+ (* (, yr) 12) (, mon) -1 (, n) )))
       (setq (, mon) (1+ (% macro-y 12) ))
       (setq (, yr) (/ macro-y 12)))))

(defmacro calendar-for-loop (var from init to final do &rest body)
  "Execute a for loop."
  (` (let (( (, var) (1- (, init)) ))
       (while (>= (, final) (setq (, var) (1+ (, var))))
         (,@ body)))))

(defmacro calendar-sum (index initial condition expression)
  "For INDEX = INITIAL and successive integers, as long as CONDITION holds,
sum EXPRESSION."
  (` (let (( (, index) (, initial))
             (sum 0))
       (while (, condition)
         (setq sum (+ sum (, expression) ))
         (setq (, index) (1+ (, index))))
       sum)))

(defun calendar (&optional arg)
  "Display a three-month calendar in another window.
The three months appear side by side, with the current month in the middle
surrounded by the previous and next months.  The cursor is put on today's date.

This function is suitable for execution in a .emacs file; appropriate setting
of the variable `view-diary-entries-initially' will cause the diary entries for
the current date to be displayed in another window.  The value of the variable
`number-of-diary-entries' controls the number of days of diary entries
displayed upon initial display of the calendar.

An optional prefix argument ARG causes the calendar displayed to be ARG
months in the future if ARG is positive or in the past if ARG is negative;
in this case the cursor goes on the first day of the month.

Once in the calendar window, future or past months can be moved into view.
Arbitrary months can be displayed, or the calendar can be scrolled forward
or backward.

The cursor can be moved forward or backward by one day, one week, one month,
or one year.  All of these commands take prefix arguments which, when negative,
cause movement in the opposite direction.  For convenience, the digit keys
and the minus sign are automatically prefixes.  The window is replotted as
necessary to display the desired date.

Diary entries can be marked on the calendar or displayed in another window.

Use M-x describe-mode for details of the key bindings in the calendar window.

The Gregorian calendar is assumed.

After preparing the calendar window initially, the hooks given by the variable
`initial-calendar-window-hook' are run.

The hooks given by the variable `today-visible-calendar-hook' are run
everytime the calendar window gets scrolled, if the current date is visible
in the window.  If it is not visible, the hooks given by the variable
`today-invisible-calendar-hook' are run.  Thus, for example, setting
`today-visible-calendar-hook' to 'calendar-star-date will cause today's date
to be replaced by asterisks to highlight it whenever it is in the window."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (set-buffer (get-buffer-create calendar-buffer))
  (calendar-mode)
  (setq calendar-window-configuration (current-window-configuration))
  (let ((pop-up-windows t)
        (split-height-threshold 1000))
    (pop-to-buffer calendar-buffer)
    (regenerate-calendar-window arg)
    (let ((date (list current-month current-day current-year)))
      (if (and view-diary-entries-initially (calendar-date-is-visible-p date))
          (view-diary-entries
           (if (vectorp number-of-diary-entries)
               (aref number-of-diary-entries (calendar-day-of-week date))
             number-of-diary-entries))))
    (let* ((diary-buffer (get-file-buffer diary-file))
           (diary-window (if diary-buffer (get-buffer-window diary-buffer)))
           (split-height-threshold (if diary-window 2 1000)))
      (if view-calendar-holidays-initially
          (list-calendar-holidays))))
  (run-hooks 'initial-calendar-window-hook))

(autoload 'view-diary-entries "diary"
  "Prepare and display a buffer with diary entries.
Searches the file diary-file for entries that match ARG days starting with
the date indicated by the cursor position in the displayed three-month
calendar."
  t)

(autoload 'show-all-diary-entries "diary"
  "Show all of the diary entries in the diary-file.
This function gets rid of the selective display of the diary-file so that
all entries, not just some, are visible.  If there is no diary buffer, one
is created."
  t)

(autoload 'mark-diary-entries "diary"
  "Mark days in the calendar window that have diary entries.
Each entry in diary-file visible in the calendar window is marked."
  t)

(autoload 'insert-diary-entry "diary"
  "Insert a diary entry for the date indicated by point."
  t)

(autoload 'insert-weekly-diary-entry "diary"
  "Insert a weekly diary entry for the day of the week indicated by point."
  t)

(autoload 'insert-monthly-diary-entry "diary"
  "Insert a monthly diary entry for the day of the month indicated by point."
  t)

(autoload 'insert-yearly-diary-entry "diary"
  "Insert an annual diary entry for the day of the year indicated by point."
  t)

(autoload 'insert-anniversary-diary-entry "diary"
  "Insert an anniversary diary entry for the date indicated by point."
  t)

(autoload 'insert-block-diary-entry "diary"
  "Insert a block diary entry for the dates indicated by point and mark."
  t)

(autoload 'insert-cyclic-diary-entry "diary"
  "Insert a cyclic diary entry starting at the date indicated by point."
  t)

(autoload 'insert-hebrew-diary-entry "diary"
  "Insert a diary entry for the Hebrew date corresponding to the date
indicated by point."
  t)

(autoload 'insert-monthly-hebrew-diary-entry "diary"
  "Insert a monthly diary entry for the day of the Hebrew month corresponding
to the date indicated by point."
  t)

(autoload 'insert-yearly-hebrew-diary-entry "diary"
  "Insert an annual diary entry for the day of the Hebrew year corresponding
to the date indicated by point."
  t)

(autoload 'insert-islamic-diary-entry "diary"
  "Insert a diary entry for the Islamic date corresponding to the date
indicated by point."
  t)

(autoload 'insert-monthly-islamic-diary-entry "diary"
  "Insert a monthly diary entry for the day of the Islamic month corresponding
to the date indicated by point."
  t)

(autoload 'insert-yearly-islamic-diary-entry "diary"
  "Insert an annual diary entry for the day of the Islamic year corresponding
to the date indicated by point."
  t)

(autoload 'list-calendar-holidays "holidays"
  "Create a buffer containing the holidays for the current calendar window.
The holidays are those in the list `calendar-notable-days'.  Returns t if any
holidays are found, nil if not."
  t)

(autoload 'mark-calendar-holidays "holidays"
  "Mark notable days in the calendar window."
  t)

(autoload 'calendar-cursor-holidays "holidays"
  "Find holidays for the date specified by the cursor in the calendar window."
  t)

(defun regenerate-calendar-window (&optional arg)
  "Generate the calendar window, offset from the current date by ARG months."
  (if (not arg) (setq arg 0))
  (let* ((buffer-read-only nil)
         (today-visible (and (<= arg 1) (>= arg -1)))
         (today (calendar-current-date))
         (month (extract-calendar-month today))
         (day (extract-calendar-day today))
         (year (extract-calendar-year today))
         (day-in-week (calendar-day-of-week today)))
    (update-calendar-mode-line)
    (setq current-month month)
    (setq current-day day)
    (setq current-year year)
    (increment-calendar-month month year arg)
    (generate-calendar month year)
    (calendar-cursor-to-visible-date
     (if today-visible today (list displayed-month 1 displayed-year)))
    (set-buffer-modified-p nil)
    (or (one-window-p t)
        (/= (screen-width) (window-width))
        (shrink-window (- (window-height) 9)))
    (sit-for 0)
    (and mark-holidays-in-calendar
         (mark-calendar-holidays)
         (sit-for 0))
    (unwind-protect
        (if mark-diary-entries-in-calendar (mark-diary-entries))
      (if today-visible
          (run-hooks 'today-visible-calendar-hook)
        (run-hooks 'today-invisible-calendar-hook)))))

(defun generate-calendar (month year)
  "Generate a three-month Gregorian calendar centered around MONTH, YEAR."
  (if (< (+ month (* 12 (1- year))) 2)
      (error "Months before February, 1 AD are not available."))
  (setq displayed-month month)
  (setq displayed-year year)
  (erase-buffer)
  (increment-calendar-month month year -1)
  (calendar-for-loop i from 0 to 2 do
       (generate-calendar-month month year (+ 5 (* 25 i)))
       (increment-calendar-month month year 1)))

(defun generate-calendar-month (month year indent)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted in the buffer starting at the line on which point
is currently located, but indented INDENT spaces.  The indentation is done
from the first character on the line and does not disturb the first INDENT
characters on the line."
  (let* ((first-day-of-month (calendar-day-of-week (list month 1 year)))
         (first-saturday (- 7 first-day-of-month))
         (last (calendar-last-day-of-month month year)))
    (goto-char (point-min))
    (calendar-insert-indented
       (format "   %s %d" (calendar-month-name month) year) indent t)
    (calendar-insert-indented " S  M Tu  W Th  F  S" indent t)
    (calendar-insert-indented "" indent);; Move to appropriate spot on line
    ;; Add blank days before the first of the month
    (calendar-for-loop i from 1 to first-day-of-month do
        (insert "   "))
    ;; Put in the days of the month
    (calendar-for-loop i from 1 to last do
         (insert (format "%2d " i))
         (and (= (% i 7) (% first-saturday 7))
              (/= i last)
              (calendar-insert-indented "" 0 t)    ;; Force onto following line
              (calendar-insert-indented "" indent)))));; Go to proper spot

(defun calendar-insert-indented (string indent &optional newline)
  "Insert STRING at column INDENT.
If the optional parameter NEWLINE is t, leave point at start of next line,
inserting a newline if there was no next line; otherwise, leave point after
the inserted text.  Value is always t."
  ;; Try to move to that column.
  (move-to-column indent)
  ;; If line is too short, indent out to that column.
  (if (< (current-column) indent)
      (indent-to indent))
  (insert string)
  ;; Advance to next line, if requested.
  (if newline
      (progn
	(end-of-line)
	(if (eobp)
            (newline)
	  (forward-line 1))))
  t)

(defun redraw-calendar ()
  "Redraw the calendar display."
  (interactive)
  (regenerate-calendar-window
   (calendar-interval current-month current-year
                      displayed-month displayed-year)))

(defvar calendar-mode-map nil)
(if calendar-mode-map
    nil
  (setq calendar-mode-map (make-sparse-keymap))
  (calendar-for-loop i from 0 to 9 do
       (define-key calendar-mode-map (int-to-string i) 'digit-argument))
  (let ((l (list 'narrow-to-region 'mark-word 'mark-sexp 'mark-paragraph
                 'mark-defun 'mark-whole-buffer 'mark-page 'kill-region
                 'copy-region-as-kill 'downcase-region 'upcase-region
                 'capitalize-region 'write-region)))
    (while (car l)
      (let ((k (where-is-internal (car l))))
        (while (car k)
          (define-key calendar-mode-map (car k) 'calendar-not-implemented)
          (setq k (cdr k)))
        (setq l (cdr l)))))
  (define-key calendar-mode-map "-"     'negative-argument)
  (define-key calendar-mode-map "\C-x>" 'scroll-calendar-right)
  (define-key calendar-mode-map "\ev"   'scroll-calendar-right-three-months)
  (define-key calendar-mode-map "\C-x<" 'scroll-calendar-left)
  (define-key calendar-mode-map "\C-v"  'scroll-calendar-left-three-months)
  (define-key calendar-mode-map "\C-b"  'calendar-backward-day)
  (define-key calendar-mode-map "\C-p"  'calendar-backward-week)
  (define-key calendar-mode-map "\e["   'calendar-backward-month)
  (define-key calendar-mode-map "\C-x[" 'calendar-backward-year)
  (define-key calendar-mode-map "\C-f"  'calendar-forward-day)
  (define-key calendar-mode-map "\C-n"  'calendar-forward-week)
  (define-key calendar-mode-map "\e]"   'calendar-forward-month)
  (define-key calendar-mode-map "\C-x]" 'calendar-forward-year)
  (define-key calendar-mode-map "\C-a"  'calendar-beginning-of-week)
  (define-key calendar-mode-map "\C-e"  'calendar-end-of-week)
  (define-key calendar-mode-map "\ea"   'calendar-beginning-of-month)
  (define-key calendar-mode-map "\ee"   'calendar-end-of-month)
  (define-key calendar-mode-map "\e<"   'calendar-beginning-of-year)
  (define-key calendar-mode-map "\e>"   'calendar-end-of-year)
  (define-key calendar-mode-map "\C-@"  'calendar-set-mark)
  (define-key calendar-mode-map "\C-x\C-x" 'calendar-exchange-point-and-mark)
  (define-key calendar-mode-map "\e="   'calendar-count-days-region)
  (define-key calendar-mode-map "gd"    'calendar-goto-date)
  (define-key calendar-mode-map "gJ"    'calendar-goto-julian-date)
  (define-key calendar-mode-map "gH"    'calendar-goto-hebrew-date)
  (define-key calendar-mode-map "gI"    'calendar-goto-islamic-date)
  (define-key calendar-mode-map "gC"    'calendar-goto-iso-date)
  (define-key calendar-mode-map " "     'scroll-other-window)
  (define-key calendar-mode-map "\C-c\C-l" 'redraw-calendar)
  (define-key calendar-mode-map "c"     'calendar-current-month)
  (define-key calendar-mode-map "o"     'calendar-other-month)
  (define-key calendar-mode-map "q"     'exit-calendar)
  (define-key calendar-mode-map "a"     'list-calendar-holidays)
  (define-key calendar-mode-map "h"     'calendar-cursor-holidays)
  (define-key calendar-mode-map "x"     'mark-calendar-holidays)
  (define-key calendar-mode-map "u"     'calendar-unmark)
  (define-key calendar-mode-map "m"     'mark-diary-entries)
  (define-key calendar-mode-map "d"     'view-diary-entries)
  (define-key calendar-mode-map "s"     'show-all-diary-entries)
  (define-key calendar-mode-map "D"     'cursor-to-calendar-day-of-year)
  (define-key calendar-mode-map "C"     'cursor-to-iso-calendar-date)
  (define-key calendar-mode-map "J"     'cursor-to-julian-calendar-date)
  (define-key calendar-mode-map "H"     'cursor-to-hebrew-calendar-date)
  (define-key calendar-mode-map "I"     'cursor-to-islamic-calendar-date)
  (define-key calendar-mode-map "F"     'cursor-to-french-calendar-date)
  (define-key calendar-mode-map "\C-cd" 'insert-diary-entry)
  (define-key calendar-mode-map "\C-cw" 'insert-weekly-diary-entry)
  (define-key calendar-mode-map "\C-cm" 'insert-monthly-diary-entry)
  (define-key calendar-mode-map "\C-cy" 'insert-yearly-diary-entry)
  (define-key calendar-mode-map "\C-ca" 'insert-anniversary-diary-entry)
  (define-key calendar-mode-map "\C-cb" 'insert-block-diary-entry)
  (define-key calendar-mode-map "\C-cc" 'insert-cyclic-diary-entry)
  (define-key calendar-mode-map "\C-cHd" 'insert-hebrew-diary-entry)
  (define-key calendar-mode-map "\C-cHm" 'insert-monthly-hebrew-diary-entry)
  (define-key calendar-mode-map "\C-cHy" 'insert-yearly-hebrew-diary-entry)
  (define-key calendar-mode-map "\C-cId" 'insert-islamic-diary-entry)
  (define-key calendar-mode-map "\C-cIm" 'insert-monthly-islamic-diary-entry)
  (define-key calendar-mode-map "\C-cIy" 'insert-yearly-islamic-diary-entry)
  (define-key calendar-mode-map "?"      'describe-calendar-mode))

(defun describe-calendar-mode ()
  "Create a help buffer with a brief description of the calendar-mode."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ
     (format
      "Calendar Mode:\nFor a complete description, type %s\n%s\n"
      (substitute-command-keys
       "\\<calendar-mode-map>\\[describe-mode] from within the calendar")
      (substitute-command-keys "\\{calendar-mode-map}")))
    (print-help-return-message)))

;; Calendar mode is suitable only for specially formatted data.
(put 'calendar-mode 'mode-class 'special)

(defvar calendar-mode-line-format
  (substitute-command-keys
    "\\<calendar-mode-map>\\[scroll-calendar-left]      Calendar    \\[describe-calendar-mode] help/\\[calendar-other-month] other/\\[calendar-current-month] current    %17s       \\[scroll-calendar-right]")
  "The mode line of the calendar buffer.")

(defun calendar-mode ()
  "A major mode for the sliding calendar window and diary.

The commands for cursor movement are:\\<calendar-mode-map>

       \\[calendar-forward-day]  one day forward           \\[calendar-backward-day]  one day backward
       \\[calendar-forward-week]  one week forward          \\[calendar-backward-week]  one week backward
       \\[calendar-forward-month]  one month forward       \\[calendar-backward-month]  one month backward
       \\[calendar-forward-year]  one year forward        \\[calendar-backward-year]  one year backward
       \\[calendar-beginning-of-week]  beginning of week         \\[calendar-end-of-week]  end of week
       \\[calendar-beginning-of-month]  beginning of month      \\[calendar-end-of-month]  end of month
       \\[calendar-beginning-of-year]  beginning of year       \\[calendar-end-of-year]  end of year
       \\[calendar-goto-date]  go to date                \\[calendar-goto-julian-date]  go to Julian date
       \\[calendar-goto-hebrew-date]  go to Hebrew date         \\[calendar-goto-islamic-date]  go to Islamic date
       \\[calendar-goto-iso-date]  go to ISO date

You can mark a date in the calendar and switch the point and mark:
       \\[calendar-set-mark]  mark date                 \\[calendar-exchange-point-and-mark]  exchange point and mark
You can determine the number of days (inclusive) between the point and mark by
       \\[calendar-count-days-region]  count days in the region

The commands for calendar movement are:

       \\[scroll-calendar-right]  scroll one month right   \\[scroll-calendar-left]  scroll one month left
       \\[scroll-calendar-right-three-months]  scroll 3 months right    \\[scroll-calendar-left-three-months]  scroll 3 months left
       \\[calendar-current-month]  display current month        \\[calendar-other-month]  display another month

Whenever it makes sense, the above commands take prefix arguments that
multiply their affect.  For convenience, the digit keys and the minus sign
are bound to digit-argument, so they need not be prefixed with ESC.

If the calendar window somehow becomes corrupted, it can be regenerated with

       \\[redraw-calendar]  redraw the calendar

The following commands deal with holidays and other notable days:

       \\[calendar-cursor-holidays]  give holidays for the date specified by the cursor
       \\[mark-calendar-holidays]  mark notable days
       \\[calendar-unmark]  unmark dates
       \\[list-calendar-holidays]  display notable days

The command M-x holidays causes the notable dates for the current month, and
the preceding and succeeding months, to be displayed, independently of the
calendar.

The following commands control the diary:

       \\[mark-diary-entries]  mark diary entries          \\[calendar-unmark]  unmark dates
       \\[view-diary-entries]  display diary entries       \\[show-all-diary-entries]  show all diary entries
       \\[print-diary-entries]  print diary entries

Displaying the diary entries causes the diary entries from the diary-file
(for the date indicated by the cursor in the calendar window) to be
displayed in another window.  This function takes an integer argument that
specifies the number of days of calendar entries to be displayed, starting
with the date indicated by the cursor.

The command \\[print-diary-entries] prints the diary buffer (as it appears)
on the line printer.

The command M-x diary causes the diary entries for the current date to be
displayed, independently of the calendar.  The number of days of entries is
governed by number-of-diary-entries.

The format of the entries in the diary file is described in the
documentation string for the variable diary-file.

When diary entries are in view in the window, they can be edited.  It is
important to keep in mind that the buffer displayed contains the entire
diary file, but with portions of it concealed from view.  This means, for
instance, that the forward-char command can put the cursor at what appears
to be the end of the line, but what is in reality the middle of some
concealed line.  BE CAREFUL WHEN EDITING THE DIARY ENTRIES! (Inserting
additional lines or adding/deleting characters in the middle of a visible
line will not cause problems; watch out for end-of-line, however--it may
put you at the end of a concealed line far from where the cursor appears to
be!)  BEFORE EDITING THE DIARY IT IS BEST TO DISPLAY THE ENTIRE FILE WITH
show-all-diary-entries.  BE SURE TO WRITE THE FILE BEFORE EXITING FROM THE
CALENDAR.

The following commands assist in making diary entries:

       \\[insert-diary-entry]  insert a diary entry for the selected date
       \\[insert-weekly-diary-entry]  insert a diary entry for the selected day of the week
       \\[insert-monthly-diary-entry]  insert a diary entry for the selected day of the month
       \\[insert-yearly-diary-entry]  insert a diary entry for the selected day of the year
       \\[insert-block-diary-entry]  insert a diary entry for the block days between point and mark
       \\[insert-anniversary-diary-entry]  insert an anniversary diary entry for the selected date
       \\[insert-cyclic-diary-entry]  insert a cyclic diary entry

There are corresponding commands to assist in making Hebrew- or Islamic-date
diary entries:

       \\[insert-hebrew-diary-entry]  insert a diary entry for the Hebrew date corresponding
                to the selected date
       \\[insert-monthly-hebrew-diary-entry]  insert a diary entry for the day of the Hebrew month
                corresponding to the selected day
       \\[insert-yearly-hebrew-diary-entry]  insert a diary entry for the day of the Hebrew year
                corresponding to the selected day
       \\[insert-islamic-diary-entry]  insert a diary entry for the Islamic date corresponding
                to the selected date
       \\[insert-monthly-islamic-diary-entry]  insert a diary entry for the day of the Islamic month
                corresponding to the selected day
       \\[insert-yearly-islamic-diary-entry]  insert a diary entry for the day of the Islamic year
                corresponding to the selected day

All of the diary entry commands make nonmarking entries when given a prefix
argument; with no prefix argument, the diary entries are marking.

The day number in the year and the number of days remaining in the year can be
determined by

       \\[cursor-to-calendar-day-of-year]  show day number and the number of days remaining in the year

Equivalent dates on the ISO commercial, Julian, Hebrew, Islamic and French
Revolutionary calendars can be determined by

       \\[cursor-to-iso-calendar-date]  show equivalent date on the ISO commercial calendar
       \\[cursor-to-julian-calendar-date]  show equivalent date on the Julian calendar
       \\[cursor-to-hebrew-calendar-date]  show equivalent date on the Hebrew calendar
       \\[cursor-to-islamic-calendar-date]  show equivalent date on the Islamic calendar
       \\[cursor-to-french-calendar-date]  show equivalent date on the French Revolutionary calendar

To exit from the calendar use

       \\[exit-calendar]  exit from calendar

The variable `view-diary-entries-initially', whose default is nil, can be
set to to t cause diary entries for the current date will be displayed in
another window when the calendar is first displayed, if the current date is
visible.  The variable `number-of-diary-entries' controls number of days of
diary entries that will be displayed initially or with the command M-x
diary.  For example, if the default value 1 is used, then only the current
day's diary entries will be displayed.  If the value 2 is used, both the
current day's and the next day's entries will be displayed.  The value can
also be a vector: If the value is [0 2 2 2 2 4 1] then no diary entries
will be displayed on Sunday, the current date's and the next day's diary
entries will be displayed Monday through Thursday, Friday through Monday's
entries will be displayed on Friday, while on Saturday only that day's
entries will be displayed.

The variable `view-calendar-holidays-initially' can be set to t to cause
the holidays for the current three month period will be displayed on entry
to the calendar.  The holidays are displayed in another window.

The variable `mark-diary-entries-in-calendar' can be set to t to cause any
dates visible with calendar entries to be marked with the symbol specified
by the variable `diary-entry-marker', normally a plus sign.

The variable `initial-calendar-window-hook', whose default value is nil,
is list of functions to be called when the calendar window is first opened.
The functions invoked are called after the calendar window is opened, but
once opened is never called again.  Leaving the calendar with the `q' command
and reentering it will cause these functions to be called again.

The variable `today-visible-calendar-hook', whose default value is nil,
is the list of functions called after the calendar buffer has been prepared
with the calendar when the current date is visible in the window.
This can be used, for example, to replace today's date with asterisks; a
function calendar-star-date is included for this purpose:
    (setq today-visible-calendar-hook 'calendar-star-date)
It could also be used to mark the current date with `*'; a function is also
provided for this:
    (setq today-visible-calendar-hook 'calendar-mark-today)

The variable `today-invisible-calendar-hook', whose default value is nil,
is the list of functions called after the calendar buffer has been prepared
with the calendar when the current date is not visible in the window.

The variable `list-diary-entries-hook' is the list of functions called
after the diary buffer is prepared.  The default value simply displays the
diary file using selective-display to conceal irrelevant diary entries.  An
alternative function `prepare-fancy-diary-buffer' is provided that, when
used as the `list-diary-entries-hook', causes a noneditable buffer to be
prepared with a neatly organized day-by-day listing of relevant diary
entries, together with any known holidays.  The inclusion of the holidays
slows this fancy display of the diary; to speed it up, set the variable
`holidays-in-diary-buffer' to nil.

The variable `print-diary-entries-hook' is the list of functions called
after a temporary buffer is prepared with the diary entries currently
visible in the diary buffer.  The default value of this hook adds a heading
(composed from the diary buffer's mode line), does the printing with the
command lpr-buffer, and kills the temporary buffer.  Other uses might
include, for example, rearranging the lines into order by day and time.

The Gregorian calendar is assumed."

  (kill-all-local-variables)
  (setq major-mode 'calendar-mode)
  (setq mode-name "Calendar")
  (use-local-map calendar-mode-map)
  (setq buffer-read-only t)
  (setq indent-tabs-mode nil)
  (make-local-variable 'calendar-window-configuration);; Windows on entry.
  (make-local-variable 'calendar-mark-ring)
  (make-local-variable 'current-month)  ;;  Current month.
  (make-local-variable 'current-day)    ;;  Current day.
  (make-local-variable 'current-year)   ;;  Current year.
  (make-local-variable 'displayed-month);;  Month in middle of window.
  (make-local-variable 'displayed-year));;  Year in middle of window.

(defun update-calendar-mode-line ()
  "Update the calendar mode line with the current date and date style."
  (if (bufferp (get-buffer calendar-buffer))
      (save-excursion
        (set-buffer calendar-buffer)
        (setq mode-line-format
              (format calendar-mode-line-format
                      (calendar-date-string (calendar-current-date) t))))))

(defun exit-calendar ()
  "Get out of the calendar window and destroy it and related buffers."
  (interactive)
  (let ((diary-buffer (get-file-buffer diary-file))
        (d-buffer (get-buffer fancy-diary-buffer))
        (h-buffer (get-buffer holiday-buffer)))
    (if (not diary-buffer)
        (progn
          (set-window-configuration calendar-window-configuration)
          (kill-buffer calendar-buffer)
          (if d-buffer (kill-buffer d-buffer))
          (if h-buffer (kill-buffer h-buffer)))
      (if (or (not (buffer-modified-p diary-buffer))
              (yes-or-no-p "Diary modified; do you really want to exit the calendar? "))
          (progn
            (set-window-configuration calendar-window-configuration)
            (kill-buffer calendar-buffer)
            (if d-buffer (kill-buffer d-buffer))
            (if h-buffer (kill-buffer h-buffer))
            (set-buffer diary-buffer)
            (set-buffer-modified-p nil)
            (kill-buffer diary-buffer))))))

(defun calendar-current-month ()
  "Reposition the calendar window so the current date is visible."
  (interactive)
  (let ((today (calendar-current-date)));; The date might have changed.
    (if (not (calendar-date-is-visible-p today))
        (regenerate-calendar-window)
      (update-calendar-mode-line)
      (calendar-cursor-to-visible-date today))))

(defun calendar-forward-month (arg)
  "Move the cursor forward ARG months.
Movement is backward if ARG is negative."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((cursor-date (or (calendar-cursor-to-date)
                          (error "Cursor is not on a date!")))
         (month (extract-calendar-month cursor-date))
         (day (extract-calendar-day cursor-date))
         (year (extract-calendar-year cursor-date)))
    (increment-calendar-month month year arg)
    (let ((last (calendar-last-day-of-month month year)))
      (if (< last day)
        (setq day last)))
    ;; Put the new month on the screen, if needed, and go to the new date.
    (let ((new-cursor-date (list month day year)))
      (if (not (calendar-date-is-visible-p new-cursor-date))
          (calendar-other-month month year))
      (calendar-cursor-to-visible-date new-cursor-date))))

(defun calendar-forward-year (arg)
  "Move the cursor forward by ARG years.
Movement is backward if ARG is negative."
  (interactive "p")
  (calendar-forward-month (* 12 arg)))

(defun calendar-backward-month (arg)
  "Move the cursor backward by ARG months.
Movement is forward if ARG is negative."
  (interactive "p")
  (calendar-forward-month (- arg)))

(defun calendar-backward-year (arg)
  "Move the cursor backward ARG years.
Movement is forward is ARG is negative."
  (interactive "p")
  (calendar-forward-month (* -12 arg)))

(defun scroll-calendar-left (arg)
  "Scroll the displayed calendar left by ARG months.
If ARG is negative the calendar is scrolled right.  Maintains the relative
position of the cursor with respect to the calendar as well as possible."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let ((old-date (calendar-cursor-to-date))
        (today (calendar-current-date)))
    (if (/= arg 0)
        (progn
          (regenerate-calendar-window
           (+ arg (calendar-interval current-month current-year
                                     displayed-month displayed-year)))
          (calendar-cursor-to-visible-date
           (cond
            ((calendar-date-is-visible-p old-date) old-date)
            ((calendar-date-is-visible-p today) today)
            (t (list displayed-month 1 displayed-year))))))))

(defun scroll-calendar-right (arg)
  "Scroll the displayed calendar window right by ARG months.
If ARG is negative the calendar is scrolled left.  Maintains the relative
position of the cursor with respect to the calendar as well as possible."
  (interactive "p")
  (scroll-calendar-left (- arg)))

(defun scroll-calendar-left-three-months (arg)
  "Scroll the displayed calendar window left by 3*ARG months.
If ARG is negative the calendar is scrolled right.  Maintains the relative
position of the cursor with respect to the calendar as well as possible."
  (interactive "p")
  (scroll-calendar-left (* 3 arg)))

(defun scroll-calendar-right-three-months (arg)
  "Scroll the displayed calendar window right by 3*ARG months.
If ARG is negative the calendar is scrolled left.  Maintains the relative
position of the cursor with respect to the calendar as well as possible."
  (interactive "p")
  (scroll-calendar-left (* -3 arg)))

(defun calendar-current-date ()
  "Returns the current date in a list (month day year).
If in the calendar buffer, also sets the current date local variables."
  (let* ((date (current-time-string))
         (garbage
          (string-match
           "^\\([A-Z][a-z]*\\) *\\([A-Z][a-z]*\\) *\\([0-9]*\\) .* \\([0-9]*\\)$"
           date))
         (month
          (cdr (assoc 
                (substring date (match-beginning 2) (match-end 2))
                (calendar-make-alist
                 calendar-month-name-array
                 1
                 '(lambda (x) (substring x 0 3))))))
         (day
          (string-to-int (substring date (match-beginning 3) (match-end 3))))
         (year
          (string-to-int (substring date (match-beginning 4) (match-end 4)))))
    (if (equal (current-buffer) (get-buffer calendar-buffer))
      (progn
        (setq current-month month)
        (setq current-day day)
        (setq current-year year)))
    (list month day year)))

(defun calendar-cursor-to-date ()
  "Returns a list of the month, day, and year of current cursor position.
Returns nil if the cursor is not on a specific day."
  (if (and (looking-at "[*0-9]")
           (< 2 (count-lines (point-min) (point))))
      (save-excursion
        (re-search-backward "[^0-9]")
        (forward-char 1)
        (let*
            ((day (string-to-int (buffer-substring (point) (+ 3 (point)))))
             (day (if (= 0 day) current-day day));; Starred date.
             (segment (/ (current-column) 25))
             (month (% (+ displayed-month segment -1) 12))
             (month (if (= 0 month) 12 month))
             (year
              (cond
               ((and (=  12 month) (= segment 0)) (1- displayed-year))
               ((and (=   1 month) (= segment 2)) (1+ displayed-year))
               (t displayed-year))))
          (list month day year)))))

(defun calendar-cursor-to-nearest-date ()
  "Move the cursor to the closest date.
The position of the cursor is unchanged if it is already on a date.
Returns the list (month day year) giving the cursor position."
  (let ((date (calendar-cursor-to-date))
        (column (current-column)))
    (if date
        date
      (if (> 3 (count-lines (point-min) (point)))
          (progn
            (goto-line 3)
            (move-to-column column)))
      (if (not (looking-at "[0-9]"))
          (if (and (not (looking-at " *$"))
                   (or (< column 25)
                       (and (> column 27)
                            (< column 50))
                       (and (> column 52)
                            (< column 75))))
              (progn
                (re-search-forward "[0-9]" nil t)
                (backward-char 1))
            (re-search-backward "[0-9]" nil t)))
      (calendar-cursor-to-date))))

(defun calendar-forward-day (arg)
  "Move the cursor forward ARG days.
Moves backward if ARG is negative."
  (interactive "p")
  (if (/= 0 arg)
      (let*
          ((cursor-date (calendar-cursor-to-date))
           (cursor-date (if cursor-date
                            cursor-date
                          (if (> arg 0) (setq arg (1- arg)))
                          (calendar-cursor-to-nearest-date)))
           (new-cursor-date
            (calendar-gregorian-from-absolute
             (+ (calendar-absolute-from-gregorian cursor-date) arg)))
           (new-display-month (extract-calendar-month new-cursor-date))
           (new-display-year (extract-calendar-year new-cursor-date)))
        ;; Put the new month on the screen, if needed, and go to the new date.
        (if (not (calendar-date-is-visible-p new-cursor-date))
            (calendar-other-month new-display-month new-display-year))
        (calendar-cursor-to-visible-date new-cursor-date))))

(defun calendar-backward-day (arg)
  "Move the cursor back ARG days.
Moves forward if ARG is negative."
  (interactive "p")
  (calendar-forward-day (- arg)))

(defun calendar-forward-week (arg)
  "Move the cursor forward ARG weeks.
Moves backward if ARG is negative."
  (interactive "p")
  (calendar-forward-day (* arg 7)))

(defun calendar-backward-week (arg)
  "Move the cursor back ARG weeks.
Moves forward if ARG is negative."
  (interactive "p")
  (calendar-forward-day (* arg -7)))

(defun calendar-beginning-of-week (arg)
  "Move the cursor back ARG Sundays."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let ((day (calendar-day-of-week (calendar-cursor-to-date))))
    (calendar-backward-day
     (if (= day 0) (* 7 arg) (+ day (* 7 (1- arg)))))))

(defun calendar-end-of-week (arg)
  "Move the cursor forward ARG Saturdays."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let ((day (calendar-day-of-week (calendar-cursor-to-date))))
    (calendar-forward-day
     (if (= day 6) (* 7 arg) (+ (- 6 day) (* 7 (1- arg)))))))

(defun calendar-beginning-of-month (arg)
  "Move the cursor backward ARG month beginnings."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date)))
    (if (= day 1)
        (calendar-backward-month arg)
      (calendar-cursor-to-visible-date (list month 1 year))
      (calendar-backward-month (1- arg)))))

(defun calendar-end-of-month (arg)
  "Move the cursor forward ARG month ends."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date))
         (last-day (calendar-last-day-of-month month year)))
    (if (/= day last-day)
        (progn
          (calendar-cursor-to-visible-date (list month last-day year))
          (setq arg (1- arg))))
    (increment-calendar-month month year arg)
    (let ((last-day (list
                     month
                     (calendar-last-day-of-month month year)
                     year)))
      (if (not (calendar-date-is-visible-p last-day))
          (calendar-other-month month year)
      (calendar-cursor-to-visible-date last-day)))))

(defun calendar-beginning-of-year (arg)
  "Move the cursor backward ARG year beginnings."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date))
         (jan-first (list 1 1 year)))
    (if (and (= day 1) (= 1 month))
        (calendar-backward-month (* 12 arg))
      (if (and (= arg 1)
               (calendar-date-is-visible-p jan-first))
          (calendar-cursor-to-visible-date jan-first)
        (calendar-other-month 1 (- year (1- arg)))))))

(defun calendar-end-of-year (arg)
  "Move the cursor forward ARG year beginnings."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date))
         (year (extract-calendar-year date))
         (dec-31 (list 12 31 year)))
    (if (and (= day 31) (= 12 month))
        (calendar-forward-month (* 12 arg))
      (if (and (= arg 1)
               (calendar-date-is-visible-p dec-31))
          (calendar-cursor-to-visible-date dec-31)
        (calendar-other-month 12 (- year (1- arg)))
        (calendar-cursor-to-visible-date (list 12 31 displayed-year))))))

(defun extract-calendar-month (date)
  "Extract the month part of DATE which has the form (month day year)."
  (car date))

(defun extract-calendar-day (date)
  "Extract the day part of DATE which has the form (month day year)."
  (car (cdr date)))

(defun extract-calendar-year (date)
  "Extract the year part of DATE which has the form (month day year)."
  (car (cdr (cdr date))))

(defun calendar-gregorian-from-absolute (date)
  "Compute the list (month day year) corresponding to the absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((approx (/ date 366));; Approximation from below.
         (year                ;; Search forward from the approximation.
          (+ approx
             (calendar-sum y approx
                 (>= date (calendar-absolute-from-gregorian (list 1 1 (1+ y))))
                  1)))
         (month         ;; Search forward from January.
          (1+ (calendar-sum m 1
                   (> date
                      (calendar-absolute-from-gregorian
                       (list m (calendar-last-day-of-month m year) year)))
                   1)))
         (day           ;; Calculate the day by subtraction.
          (- date
             (1- (calendar-absolute-from-gregorian (list month 1 year))))))
    (list month day year)))

(defun calendar-cursor-to-visible-date (date)
  "Move the cursor to DATE that is on the screen."
    (let ((month (extract-calendar-month date))
          (day (extract-calendar-day date))
          (year (extract-calendar-year date)))
      (goto-line (+ 3
                    (/ (+ day -1
                          (calendar-day-of-week (list month 1 year)))
                       7)))
      (move-to-column (+ 6
                         (* 25
                            (1+ (calendar-interval
                                 displayed-month displayed-year month year)))
                         (* 3 (calendar-day-of-week date))))))

(defun calendar-other-month (month year)
  "Display a three-month calendar centered around MONTH and YEAR."
  (interactive
   (let* ((completion-ignore-case t)
          (month (cdr (assoc
                       (capitalize
                        (completing-read
                         "Month name: "
                         (mapcar 'list (append calendar-month-name-array nil))
                         nil t))
                       (calendar-make-alist calendar-month-name-array))))
          (year (calendar-read
                 "Year (>0): "
                 '(lambda (x) (> x 0))
                 (int-to-string current-year))))
     (list month year)))
  (if (and (= month displayed-month)
           (= year displayed-year))
      nil
    (let ((old-date (calendar-cursor-to-date))
          (today (calendar-current-date)))
      (regenerate-calendar-window
       (calendar-interval current-month current-year month year))
      (calendar-cursor-to-visible-date
       (cond
        ((calendar-date-is-visible-p old-date) old-date)
        ((calendar-date-is-visible-p today) today)
        (t (list month 1 year)))))))

(defun calendar-set-mark (arg)
  "Mark the date under the cursor, or jump to marked date.
With no prefix argument, push current date onto marked date ring.
With argument, jump to mark, pop it, and put point at end of ring."
  (interactive "P")
  (let ((date (or (calendar-cursor-to-date)
                  (error "Cursor is not on a date!"))))
    (if (null arg)
        (progn
          (setq calendar-mark-ring (cons date calendar-mark-ring))
          ;; Since the top of the mark ring is the marked date in the
          ;; calendar, the mark ring in the calendar is one longer than
          ;; in other buffers to get the same effect.
          (if (> (length calendar-mark-ring) (1+ mark-ring-max))
              (setcdr (nthcdr mark-ring-max calendar-mark-ring) nil))
          (message "Mark set"))
      (if (null calendar-mark-ring)
          (error "No mark set in this buffer")
        (calendar-goto-date (car calendar-mark-ring))
        (setq calendar-mark-ring
              (cdr (nconc calendar-mark-ring (list date))))))))

(defun calendar-exchange-point-and-mark ()
  "Exchange the current cursor position with the marked date."
  (interactive)
  (let ((mark (car calendar-mark-ring))
        (date (or (calendar-cursor-to-date)
                  (error "Cursor is not on a date!"))))
    (if (null mark)
        (error "No mark set in this buffer")
      (setq calendar-mark-ring (cons date (cdr calendar-mark-ring)))
      (calendar-goto-date mark))))

(defun calendar-count-days-region ()
  "Count the number of days (inclusive) between point and the mark."
  (interactive)
  (let* ((days (- (calendar-absolute-from-gregorian
                   (or (calendar-cursor-to-date)
                       (error "Cursor is not on a date!")))
                  (calendar-absolute-from-gregorian
                   (or (car calendar-mark-ring)
                       (error "No mark set in this buffer")))))
         (days (1+ (if (> days 0) days (- days)))))
    (message "Region has %d day%s (inclusive)"
             days (if (> days 1) "s" ""))))

(defun calendar-not-implemented ()
  "Not implemented."
  (interactive)
  (error "%s not available in the calendar"
         (global-key-binding (this-command-keys))))

(defun calendar-read (prompt acceptable &optional initial-contents)
  "Return an object read from the minibuffer.
Prompt with the string PROMPT and use the function ACCEPTABLE to decide if
entered item is acceptable.  If non-nil, optional third arg INITIAL-CONTENTS
is a string to insert in the minibuffer before reading."
  (let ((value (read-minibuffer prompt initial-contents)))
    (while (not (funcall acceptable value))
      (setq value (read-minibuffer prompt initial-contents)))
    value))

(defun calendar-goto-date (date)
  "Move cursor to DATE."
  (interactive
   (let* ((year (calendar-read
                 "Year (>0): "
                 '(lambda (x) (> x 0))
                 (int-to-string current-year)))
          (month-array calendar-month-name-array)
          (completion-ignore-case t)
          (month (cdr (assoc
                       (capitalize
                        (completing-read
                         "Month name: "
                         (mapcar 'list (append month-array nil))
                         nil t))
                       (calendar-make-alist month-array 1 'capitalize))))
          (last (calendar-last-day-of-month month year))
          (day (calendar-read
                (format "Day (1-%d): " last)
                '(lambda (x) (and (< 0 x) (<= x last))))))
     (list (list month day year))))
  (let ((month (extract-calendar-month date))
        (year (extract-calendar-year date)))
    (if (not (calendar-date-is-visible-p date))
        (calendar-other-month
         (if (and (= month 1) (= year 1))
             2
           month)
         year)))
  (calendar-cursor-to-visible-date date))

(defun calendar-goto-julian-date (date &optional noecho)
  "Move cursor to Julian DATE; echo Julian date unless NOECHO is t."
  (interactive
   (let* ((year (calendar-read
                 "Julian calendar year (>0): "
                 '(lambda (x) (> x 0))
                 (int-to-string
                  (extract-calendar-year
                   (calendar-julian-from-absolute
                    (calendar-absolute-from-gregorian
                     (list current-month 1 current-year)))))))
          (month-array calendar-month-name-array)
          (completion-ignore-case t)
          (month (cdr (assoc
                       (capitalize
                        (completing-read
                         "Julian calendar month name: "
                         (mapcar 'list (append month-array nil))
                         nil t))
                       (calendar-make-alist month-array 1 'capitalize))))
          (last 
           (if (and (zerop (% year 4)) (= month 2))
               29
             (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month))))
          (day (calendar-read
                (format "Julian calendar day (%d-%d): "
                        (if (and (= year 1) (= month 1)) 3 1) last)
                '(lambda (x) 
                   (and (< (if (and (= year 1) (= month 1)) 2 0) x)
                        (<= x last))))))
     (list (list month day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-julian date)))
  (or noecho (cursor-to-julian-calendar-date)))

(defun calendar-goto-hebrew-date (date &optional noecho)
  "Move cursor to Hebrew DATE; echo Hebrew date unless NOECHO is t."
  (interactive
   (let* ((year (calendar-read
                 "Hebrew calendar year (>3760): "
                 '(lambda (x) (> x 3760))
                 (int-to-string
                  (extract-calendar-year
                   (calendar-hebrew-from-absolute
                    (calendar-absolute-from-gregorian
                     (list current-month 1 current-year)))))))
          (month-array (if (hebrew-calendar-leap-year-p year)
                           calendar-hebrew-month-name-array-leap-year
                         calendar-hebrew-month-name-array-common-year))
          (completion-ignore-case t)
          (month (cdr (assoc
                       (capitalize
                        (completing-read
                         "Hebrew calendar month name: "
                         (mapcar 'list (append month-array nil))
                         (if (= year 3761)
                             '(lambda (x)
                                (let ((m (cdr
                                          (assoc
                                           (car x)
                                           (calendar-make-alist
                                            month-array)))))
                                  (< 0
                                     (calendar-absolute-from-hebrew
                                      (list m
                                            (hebrew-calendar-last-day-of-month
                                             m year)
                                            year))))))
                                 
                         t))
                       (calendar-make-alist month-array 1 'capitalize))))
          (last (hebrew-calendar-last-day-of-month month year))
          (first (if (and (= year 3761) (= month 10))
                     18 1))
          (day (calendar-read
                (format "Hebrew calendar day (%d-%d): "
                        first last)
                '(lambda (x) (and (<= first x) (<= x last))))))
     (list (list month day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-hebrew date)))
  (or noecho (cursor-to-hebrew-calendar-date)))

(defun calendar-goto-islamic-date (date &optional noecho)
  "Move cursor to Islamic DATE; echo Islamic date unless NOECHO is t."
  (interactive
   (let* ((year (calendar-read
                 "Islamic calendar year (>0): "
                 '(lambda (x) (> x 0))
                 (int-to-string
                  (extract-calendar-year
                   (calendar-islamic-from-absolute
                    (calendar-absolute-from-gregorian
                     (list current-month 1 current-year)))))))
          (month-array calendar-islamic-month-name-array)
          (completion-ignore-case t)
          (month (cdr (assoc
                       (capitalize
                        (completing-read
                         "Islamic calendar month name: "
                         (mapcar 'list (append month-array nil))
                         nil t))
                       (calendar-make-alist month-array 1 'capitalize))))
          (last (islamic-calendar-last-day-of-month month year))
          (day (calendar-read
                (format "Islamic calendar day (1-%d): " last)
                '(lambda (x) (and (< 0 x) (<= x last))))))
     (list (list month day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-islamic date)))
  (or noecho (cursor-to-islamic-calendar-date)))

(defun calendar-goto-iso-date (date &optional noecho)
  "Move cursor to ISO DATE; echo ISO date unless NOECHO is t."
  (interactive
   (let* ((year (calendar-read
                 "ISO calendar year (>0): "
                 '(lambda (x) (> x 0))
                 (int-to-string current-year)))
          (no-weeks (extract-calendar-month
                     (calendar-iso-from-absolute
                      (1-
                       (calendar-dayname-on-or-before
                        1 (calendar-absolute-from-gregorian
                           (list 1 4 (1+ year))))))))
          (week (calendar-read
                 (format "ISO calendar week (1-%d): " no-weeks)
                 '(lambda (x) (and (> x 0) (<= x no-weeks)))))
          (day (calendar-read
                "ISO day (1-7): "
                '(lambda (x) (and (<= 1 x) (<= x 7))))))
     (list (list week day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-absolute-from-iso date)))
  (or noecho (cursor-to-iso-calendar-date)))

(defun calendar-interval (mon1 yr1 mon2 yr2)
  "The number of months difference between the two specified months."
  (+ (* 12 (- yr2 yr1))
     (- mon2 mon1)))

(defun calendar-leap-year-p (year)
  "Returns t if YEAR is a Gregorian leap year."
  (or
    (and (=  (% year   4) 0)
         (/= (% year 100) 0))
    (= (% year 400) 0)))

(defun calendar-day-number (date)
  "Return the day number within the year of the date DATE.
For example, (calendar-day-number '(1 1 1987)) returns the value 1,
while (calendar-day-number '(12 31 1980)) returns 366."
;;
;; An explanation of the calculation can be found in PascAlgorithms by
;; Edward and Ruth Reingold, Scott-Foresman/Little, Brown, 1988.
;;
    (let* ((month (extract-calendar-month date))
           (day (extract-calendar-day date))
           (year (extract-calendar-year date))
         (day-of-year (+ day (* 31 (1- month)))))
      (if (> month 2)
          (progn
            (setq day-of-year (- day-of-year (/ (+ 23 (* 4 month)) 10)))
            (if (calendar-leap-year-p year)
                (setq day-of-year (1+ day-of-year)))))
      day-of-year))

(defun calendar-day-name (date)
  "Returns a string with the name of the day of the week of DATE."
  (aref calendar-day-name-array (calendar-day-of-week date)))

(defconst calendar-day-name-array
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

(defun calendar-last-day-of-month (month year)
  "The last day in MONTH during YEAR."
  (if (and (calendar-leap-year-p year) (= month 2))
      29
    (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month))))

(defconst calendar-month-name-array
  ["January" "February" "March"     "April"   "May"      "June"
   "July"    "August"   "September" "October" "November" "December"])

(defun calendar-make-alist (sequence &optional start-index filter)
  "Make an assoc list corresponding to SEQUENCE.
Start at index 1, unless optional START-INDEX is provided.
If FILTER is provided, apply it to each item in the list."
  (let ((index (if start-index (1- start-index) 0)))
    (mapcar
     '(lambda (x)
        (setq index (1+ index))
        (cons (if filter (funcall filter x) x)
              index))
     (append sequence nil))))

(defun calendar-month-name (month)
  "The name of MONTH."
  (aref calendar-month-name-array (1- month)))

(defun calendar-day-of-week (date)
  "Returns the day-of-the-week index of DATE, 0 for Sunday, 1 for Monday, etc."
  (% (calendar-absolute-from-gregorian date) 7))

(defun calendar-absolute-from-gregorian (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let ((month (extract-calendar-month date))
        (day (extract-calendar-day date))
        (year (extract-calendar-year date)))
    (+ (calendar-day-number date);; Days this year
       (* 365 (1- year));;        + Days in prior years
       (/ (1- year) 4);;          + Julian leap years
       (- (/ (1- year) 100));;    - century years
       (/ (1- year) 400))));;     + Gregorian leap years

(defun calendar-unmark ()
  "Delete the diary and holiday marks from the calendar."
  (interactive)
  (setq mark-diary-entries-in-calendar nil)
  (setq mark-holidays-in-calendar nil)
  (save-excursion
    (goto-line 3)
    (beginning-of-line)
    (let ((buffer-read-only nil)
          (start (point))
          (star-date (search-forward "**" nil t))
          (star-point (point)))
      (if star-date
          (progn    ;;  Don't delete today as left by calendar-star-date
            (subst-char-in-region start (- star-point 2)
                                  (string-to-char diary-entry-marker) ?  t)
            (subst-char-in-region start (- star-point 2)
                                 (string-to-char calendar-holiday-marker) ?  t)
            (subst-char-in-region star-point (point-max)
                                  (string-to-char diary-entry-marker) ?  t)
            (subst-char-in-region star-point (point-max)
                                (string-to-char calendar-holiday-marker) ?  t))
        (subst-char-in-region start (point-max)
                              (string-to-char diary-entry-marker) ?  t)
        (subst-char-in-region start (point-max)
                              (string-to-char calendar-holiday-marker) ?  t))
      (set-buffer-modified-p nil))))

(defun calendar-date-is-visible-p (date)
  "Returns t if DATE is legal and is visible in the calendar window."
  (let ((gap (calendar-interval
              displayed-month displayed-year
              (extract-calendar-month date) (extract-calendar-year date))))
    (and (calendar-date-is-legal-p date) (> 2 gap) (< -2 gap))))

(defun calendar-date-is-legal-p (date)
  "Returns t if DATE is a legal date."
  (let ((month (extract-calendar-month date))
        (day (extract-calendar-day date))
        (year (extract-calendar-year date)))
    (and (<= 1 month) (<= month 12)
         (<= 1 day) (<= day (calendar-last-day-of-month month year))
         (<= 1 year))))

(defun calendar-date-equal (date1 date2)
  "Returns t if the DATE1 and DATE2 are the same."
  (and
   (= (extract-calendar-month date1) (extract-calendar-month date2))
   (= (extract-calendar-day date1) (extract-calendar-day date2))
   (= (extract-calendar-year date1) (extract-calendar-year date2))))

(defun mark-visible-calendar-date (date &optional mark)
  "Leave mark DATE with MARK.  MARK defaults to diary-entry-marker."
  (if (calendar-date-is-legal-p date)
      (save-excursion
        (set-buffer calendar-buffer)
        (calendar-cursor-to-visible-date date)
        (forward-char 1)
        (let ((buffer-read-only nil))
          (delete-char 1)
          (insert (if mark mark diary-entry-marker))
          (forward-char -2))
        (set-buffer-modified-p nil))))

(defun calendar-star-date ()
  "Replace the date under the cursor in the calendar window with asterisks.
This function can be used with the today-visible-calendar-hook run after the
calendar window has been prepared."
  (let ((buffer-read-only nil))
    (forward-char 1)
    (delete-char -2)
    (insert "**")
    (backward-char 1)
    (set-buffer-modified-p nil)))

(defun calendar-mark-today ()
  "Mark the date under the cursor in the calendar window with an equal sign.
This function can be used with the today-visible-calendar-hook run after the
calendar window has been prepared."
  (let ((buffer-read-only nil))
    (forward-char 1)
    (delete-char 1)
    (insert "=")
    (backward-char 2)
    (set-buffer-modified-p nil)))

(defun calendar-date-compare (date1 date2)
  "Returns t if DATE1 is before DATE2, nil otherwise.
The actual dates are in the car of DATE1 and DATE2."
  (< (calendar-absolute-from-gregorian (car date1))
     (calendar-absolute-from-gregorian (car date2))))

(defun calendar-date-string (date &optional abbreviate nodayname)
  "A string form of DATE, driven by the variable `calendar-date-display-form'.
An optional parameter ABBREVIATE, when t, causes the month and day names to be
abbreviated to three characters.  An optional parameter NODAYNAME, when t,
omits the name of the day of the week."
  (let* ((dayname
          (if nodayname
              ""
            (if abbreviate
                (substring (calendar-day-name date) 0 3)
              (calendar-day-name date))))
         (month (extract-calendar-month date))
         (monthname
          (if abbreviate
              (substring
               (calendar-month-name month) 0 3)
            (calendar-month-name month)))
         (day (int-to-string (extract-calendar-day date)))
         (month (int-to-string month))
         (year (int-to-string (extract-calendar-year date))))
    (mapconcat 'eval calendar-date-display-form "")))

(defun calendar-dayname-on-or-before (dayname date)
  "Returns the absolute date of the DAYNAME on or before absolute DATE.
DAYNAME=0 means Sunday, DAYNAME=1 means Monday, and so on.

Note: Applying this function to d+6 gives us the DAYNAME on or after an
absolute day d.  Similarly, applying it to d+3 gives the DAYNAME nearest to
absolute date d, applying it to d-1 gives the DAYNAME previous to absolute
date d, and applying it to d+7 gives the DAYNAME following absolute date d."
  (- date (% (- date dayname) 7)))

(defun calendar-nth-named-day (n dayname month year)
  "Returns the date of the  Nth DAYNAME in MONTH, YEAR.
A DAYNAME of 0 means Sunday, 1 means Monday, and so on.  If N<0, the
date returned is the Nth DAYNAME from the end of MONTH, YEAR (that is, -1 is
the last DAYNAME, -2 is the penultimate DAYNAME, and so on."
  (calendar-gregorian-from-absolute
   (if (> n 0)
       (+ (calendar-dayname-on-or-before
           dayname (calendar-absolute-from-gregorian (list month 7 year)))
          (* 7 (1- n)))
     (+ (calendar-dayname-on-or-before
         dayname
         (calendar-absolute-from-gregorian
          (list month (calendar-last-day-of-month month year) year)))
        (* 7 (1+ n))))))

(defun cursor-to-calendar-day-of-year ()
  "Show the day number in the year and the number of days remaining in the
year for the date under the cursor."
  (interactive)
  (let* ((date (or (calendar-cursor-to-date)
                   (error "Cursor is not on a date!")))
         (year (extract-calendar-year date))
         (day (calendar-day-number date))
         (days-remaining (- (calendar-day-number (list 12 31 year)) day)))
    (message "Day %d of %d; %d day%s remaining in the year"
             day year days-remaining (if (= days-remaining 1) "" "s"))))

(defun calendar-absolute-from-iso (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and
DATE.  The `ISO year' corresponds approximately to the Gregorian year, but
weeks start on Monday and end on Sunday.  The first week of the ISO year is
the first such week in which at least 4 days are in a year.  The ISO
commercial DATE has the form (week day year) in which week is in the range
1..52 and day is in the range 0..6 (1 = Monday, 2 = Tuesday, ..., 0 =
Sunday).  The The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let* ((week (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date)))
    (+ (calendar-dayname-on-or-before
        1 (+ 3 (calendar-absolute-from-gregorian (list 1 1 year))))
       (* 7 (1- week))
       (if (= day 0) 6 (1- day)))))

(defun calendar-iso-from-absolute (date)
  "Compute the `ISO commercial date' corresponding to the absolute DATE.
The ISO year corresponds approximately to the Gregorian year, but weeks
start on Monday and end on Sunday.  The first week of the ISO year is the
first such week in which at least 4 days are in a year.  The ISO commercial
date has the form (week day year) in which week is in the range 1..52 and
day is in the range 0..6 (1 = Monday, 2 = Tuesday, ..., 0 = Sunday).  The
absolute date is the number of days elapsed since the (imaginary) Gregorian
date Sunday, December 31, 1 BC."
  (let* ((approx (extract-calendar-year
                  (calendar-gregorian-from-absolute (- date 3))))
         (year (+ approx
                  (calendar-sum y approx
                      (>= date (calendar-absolute-from-iso (list 1 1 (1+ y))))
                      1))))
    (list
     (1+ (/ (- date (calendar-absolute-from-iso (list 1 1 year))) 7))
     (% date 7)
     year)))

(defun cursor-to-iso-calendar-date ()
  "Show the equivalent date on the `ISO commercial calendar' for the date
under the cursor."
  (interactive)
  (let* ((greg-date
          (or (calendar-cursor-to-date)
              (error "Cursor is not on a date!")))
         (day (% (calendar-absolute-from-gregorian greg-date) 7))
         (iso-date (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian greg-date))))
    (message "ISO date: Day %s of week %d of %d."
             (if (zerop day) 7 day)
             (extract-calendar-month iso-date)
             (extract-calendar-year iso-date))))

(defun calendar-julian-from-absolute (date)
  "Compute the Julian (month day year) corresponding to the absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((approx (/ (+ date 2) 366));; Approximation from below.
         (year        ;; Search forward from the approximation.
          (+ approx
             (calendar-sum y approx
                (>= date (calendar-absolute-from-julian (list 1 1 (1+ y))))
                1)))
         (month       ;; Search forward from January.
          (1+ (calendar-sum m 1
                 (> date
                    (calendar-absolute-from-julian
                     (list m
                           (if (and (= m 2) (= (% year 4) 0))
                               29
                             (aref [31 28 31 30 31 30 31 31 30 31 30 31]
                                   (1- m)))
                           year)))
                 1)))
         (day         ;; Calculate the day by subtraction.
          (- date (1- (calendar-absolute-from-julian (list month 1 year))))))
    (list month day year)))

(defun calendar-absolute-from-julian (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let ((month (extract-calendar-month date))
        (day (extract-calendar-day date))
        (year (extract-calendar-year date)))
    (+ (calendar-day-number date)
       (if (and (= (% year 100) 0)
                (/= (% year 400) 0)
                (> month 2))
           1 0);; Correct for Julian but not Gregorian leap year.
       (* 365 (1- year))
       (/ (1- year) 4)
       -2)))

(defun cursor-to-julian-calendar-date ()
  "Show the Julian calendar equivalent of the date under the cursor."
  (interactive)
  (let ((calendar-date-display-form
         (if european-calendar-style
             '(day " " monthname " " year)
           '(monthname " " day ", " year))))
    (message "Julian date: %s"
             (calendar-date-string
              (calendar-julian-from-absolute
               (calendar-absolute-from-gregorian
                (or (calendar-cursor-to-date)
                    (error "Cursor is not on a date!"))))))))

(defun islamic-calendar-leap-year-p (year)
  "Returns t if YEAR is a leap year on the Islamic calendar."
  (memq (% year 30)
        (list 2 5 7 10 13 16 18 21 24 26 29)))

(defun islamic-calendar-last-day-of-month (month year)
  "The last day in MONTH during YEAR on the Islamic calendar."
  (cond
   ((memq month (list 1 3 5 7 9 11)) 30)
   ((memq month (list 2 4 6 8 10)) 29)
   (t (if (islamic-calendar-leap-year-p year) 30 29))))

(defun islamic-calendar-day-number (date)
  "Return the day number within the year of the Islamic date DATE."
    (let* ((month (extract-calendar-month date))
           (day (extract-calendar-day date)))
      (+ (* 30 (/ month 2))
         (* 29 (/ (1- month) 2))
         day)))

(defun calendar-absolute-from-islamic (date)
  "Absolute date of Islamic DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date))
         (y (% year 30))
         (leap-years-in-cycle
          (cond
           ((< y 3) 0)  ((< y 6) 1)  ((< y 8) 2)  ((< y 11) 3) ((< y 14) 4)
           ((< y 17) 5) ((< y 19) 6) ((< y 22) 7) ((< y 25) 8) ((< y 27) 9)
           (t 10))))
    (+ (islamic-calendar-day-number date);; days so far this year
       (* (1- year) 354)                 ;; days in all non-leap years
       (* 11 (/ year 30))                ;; leap days in complete cycles
       leap-years-in-cycle               ;; leap days this cycle
       227014)))                         ;; days before start of calendar

(defun calendar-islamic-from-absolute (date)
  "Compute the Islamic date (month day year) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (if (< date 227015)
      (list 0 0 0);; pre-Islamic date
    (let* ((approx (/ (- date 227014) 355));; Approximation from below.
           (year           ;; Search forward from the approximation.
            (+ approx
               (calendar-sum y approx
                             (>= date (calendar-absolute-from-islamic
                                       (list 1 1 (1+ y))))
                             1)))
           (month          ;; Search forward from Muharram.
            (1+ (calendar-sum m 1
                              (> date
                                 (calendar-absolute-from-islamic
                                  (list m
                                        (islamic-calendar-last-day-of-month
                                         m year)
                                        year)))
                              1)))
           (day            ;; Calculate the day by subtraction.
            (- date
               (1- (calendar-absolute-from-islamic (list month 1 year))))))
      (list month day year))))

(defconst calendar-islamic-month-name-array
  ["Muharram" "Safar" "Rabi I" "Rabi II" "Jumada I" "Jumada II"
   "Rajab" "Sha'ban" "Ramadan" "Shawwal" "Dhu al-Qada" "Dhu al-Hijjah"])

(defun cursor-to-islamic-calendar-date ()
  "Show the Islamic calendar equivalent of the date under the cursor."
  (interactive)
  (let ((calendar-date-display-form
           (if european-calendar-style
               '(day " " monthname " " year)
             '(monthname " " day ", " year)))
        (calendar-month-name-array calendar-islamic-month-name-array)
        (islamic-date (calendar-islamic-from-absolute
                       (calendar-absolute-from-gregorian
                        (or (calendar-cursor-to-date)
                            (error "Cursor is not on a date!"))))))
    (if (< (extract-calendar-year islamic-date) 1)
        (message "Date is pre-Islamic")
      (message "Islamic date: %s" (calendar-date-string islamic-date nil t)))))

(defun calendar-hebrew-from-absolute (date)
  "Compute the Hebrew date (month day year) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((greg-date (calendar-gregorian-from-absolute date))
         (month (aref [9 10 11 12 1 2 3 4 7 7 7 8]
                 (1- (extract-calendar-month greg-date))))
         (day)
         (year (+ 3760 (extract-calendar-year greg-date))))
    (while (>= date (calendar-absolute-from-hebrew (list 7 1 (1+ year))))
        (setq year (1+ year)))
    (let ((length (hebrew-calendar-last-month-of-year year)))
      (while (> date
                (calendar-absolute-from-hebrew
                 (list month
                       (hebrew-calendar-last-day-of-month month year)
                       year)))
        (setq month (1+ (% month length)))))
    (setq day (1+
               (- date (calendar-absolute-from-hebrew (list month 1 year)))))
    (list month day year)))

(defun hebrew-calendar-leap-year-p (year)
  "t if YEAR is a Hebrew calendar leap year."
  (< (% (1+ (* 7 year)) 19) 7))

(defun hebrew-calendar-last-month-of-year (year)
  "The last month of the Hebrew calendar YEAR."
  (if (hebrew-calendar-leap-year-p year)
      13
    12))

(defun hebrew-calendar-last-day-of-month (month year)
  "The last day of MONTH in YEAR."
  (if (or (memq month (list 2 4 6 10 13))
          (and (= month 12) (not (hebrew-calendar-leap-year-p year)))
          (and (= month 8) (not (hebrew-calendar-long-heshvan-p year)))
          (and (= month 9) (hebrew-calendar-short-kislev-p year)))
      29
    30))

(defun hebrew-calendar-elapsed-days (year)
  "Number of days elapsed from the Sunday prior to the start of the Hebrew
calendar to the mean conjunction of Tishri of Hebrew YEAR."
  (let* ((months-elapsed
          (+ (* 235 (/ (1- year) 19));; Months in complete cycles so far.
             (* 12 (% (1- year) 19))      ;; Regular months in this cycle
             (/ (1+ (* 7 (% (1- year) 19))) 19)));; Leap months this cycle
         (parts-elapsed (+ 204 (* 793 (% months-elapsed 1080))))
         (hours-elapsed (+ 5
                           (* 12 months-elapsed)
                           (* 793 (/ months-elapsed 1080))
                           (/ parts-elapsed 1080)))
         (parts                                  ;; Conjunction parts
          (+ (* 1080 (% hours-elapsed 24)) (% parts-elapsed 1080)))
         (day                                    ;; Conjunction day
          (+ 1 (* 29 months-elapsed) (/ hours-elapsed 24)))
         (alternative-day
          (if (or (>= parts 19440)    ;; If the new moon is at or after midday,
                  (and (= (% day 7) 2);; ...or is on a Tuesday...
                       (>= parts 9924)  ;;    at 9 hours, 204 parts or later...
                       (not (hebrew-calendar-leap-year-p year)));; of a
                                                                ;; common year,
                  (and (= (% day 7) 1);; ...or is on a Monday...
                       (>= parts 16789) ;;   at 15 hours, 589 parts or later...
                       (hebrew-calendar-leap-year-p (1- year))));; at the end
                                                     ;; of a leap year
       ;; Then postpone Rosh HaShanah one day
              (1+ day)
       ;; Else
            day)))
    (if ;; If Rosh HaShanah would occur on Sunday, Wednesday, or Friday
        (memq (% alternative-day 7) (list 0 3 5))
  ;; Then postpone it one (more) day and return        
        (1+ alternative-day)
  ;; Else return        
      alternative-day)))

(defun hebrew-calendar-days-in-year (year)
  "Number of days in Hebrew YEAR."
  (- (hebrew-calendar-elapsed-days (1+ year))
     (hebrew-calendar-elapsed-days year)))

(defun hebrew-calendar-long-heshvan-p (year)
  "t if Heshvan is long in Hebrew YEAR."
  (= (% (hebrew-calendar-days-in-year year) 10) 5))

(defun hebrew-calendar-short-kislev-p (year)
  "t if Kislev is short in Hebrew YEAR."
  (= (% (hebrew-calendar-days-in-year year) 10) 3))

(defun calendar-absolute-from-hebrew (date)
  "Absolute date of Hebrew DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((month (extract-calendar-month date))
         (day (extract-calendar-day date))
         (year (extract-calendar-year date)))
    (+ day                            ;; Days so far this month.
       (if (< month 7);; before Tishri
     ;; Then add days in prior months this year before and after Nisan
           (+ (calendar-sum
               m 7 (<= m (hebrew-calendar-last-month-of-year year))
               (hebrew-calendar-last-day-of-month m year))
              (calendar-sum
               m 1 (< m month)
               (hebrew-calendar-last-day-of-month m year)))
     ;; Else add days in prior months this year
         (calendar-sum
          m 7 (< m month)
          (hebrew-calendar-last-day-of-month m year)))
    (hebrew-calendar-elapsed-days year);; Days in prior years.
    -1373429)))                        ;; Days elapsed before absolute date 1.

(defconst calendar-hebrew-month-name-array-common-year
  ["Nisan" "Iyar" "Sivan" "Tammuz" "Av" "Elul" "Tishri"
   "Heshvan" "Kislev" "Teveth" "Shevat" "Adar"])

(defconst calendar-hebrew-month-name-array-leap-year
  ["Nisan" "Iyar" "Sivan" "Tammuz" "Av" "Elul" "Tishri"
   "Heshvan" "Kislev" "Teveth" "Shevat" "Adar I" "Adar II"])

(defun cursor-to-hebrew-calendar-date ()
  "Show the Hebrew calendar equivalent of the date under the cursor."
  (interactive)
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " monthname " " year)
            '(monthname " " day ", " year)))
         (hebrew-date (calendar-hebrew-from-absolute
                       (calendar-absolute-from-gregorian
                        (or (calendar-cursor-to-date)
                            (error "Cursor is not on a date!")))))
         (calendar-month-name-array
          (if (hebrew-calendar-leap-year-p (extract-calendar-year hebrew-date))
              calendar-hebrew-month-name-array-leap-year
            calendar-hebrew-month-name-array-common-year)))
    (message "Hebrew date: %s" (calendar-date-string hebrew-date nil t))))

(defun french-calendar-leap-year-p (year)
  "True if YEAR is a leap year on the French Revolutionary calendar.
For Gregorian years 1793 to 1805, the years of actual operation of the
calendar, uses historical practice based on equinoxes is followed (years 3, 7,
and 11 were leap years; 15 and 20 would have been leap years).  For later
years uses the proposed rule of Romme (never adopted)--leap years fall every
four years except century years not divisible 400 and century years that are
multiples of 4000."
  (or (memq year '(3 7 11));; Actual practice--based on equinoxes
      (memq year '(15 20)) ;; Anticipated practice--based on equinoxes
      (and (> year 20)     ;; Romme's proposal--never adopted
           (zerop (% year 4))
           (not (memq (% year 400) '(100 200 300)))
           (not (zerop (% year 4000))))))

(defun french-calendar-last-day-of-month (month year)
  "Last day of MONTH, YEAR on the French Revolutionary calendar.
The 13th month is not really a month, but the 5 (6 in leap years) day period of
`sansculottides' at the end of the year."
  (if (< month 13)
      30
    (if (french-calendar-leap-year-p year)
        6
      5)))

(defun calendar-absolute-from-french (date)
  "Absolute date of French Revolutionary DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let ((month (extract-calendar-month date))
        (day (extract-calendar-day date))
        (year (extract-calendar-year date)))
    (+ (* 365 (1- year));; Days in prior years
       ;; Leap days in prior years
       (if (< year 20)
           (/ year 4);; Actual and anticipated practice (years 3, 7, 11, 15)
         ;; Romme's proposed rule (using the Principle of Inclusion/Exclusion)
         (+ (/ (1- year) 4);; Luckily, there were 4 leap years before year 20
            (- (/ (1- year) 100))
            (/ (1- year) 400)
            (- (/ (1- year) 4000))))
       (* 30 (1- month));; Days in prior months this year
       day;; Days so far this month
       654414)));; Days before start of calendar (September 22, 1792).

(defun calendar-french-from-absolute (date)
  "Compute the French Revolutionary date (month day year) corresponding to
absolute DATE.  The absolute date is the number of days elapsed since the
(imaginary) Gregorian date Sunday, December 31, 1 BC."
  (if (< date 654415)
      (list 0 0 0);; pre-French Revolutionary date
    (let* ((approx (/ (- date 654414) 366));; Approximation from below.
           (year                ;; Search forward from the approximation.
            (+ approx
               (calendar-sum y approx
                 (>= date (calendar-absolute-from-french (list 1 1 (1+ y))))
                 1)))
           (month               ;; Search forward from Vendemiaire.
            (1+ (calendar-sum m 1
                  (> date
                     (calendar-absolute-from-french
                      (list m
                            (french-calendar-last-day-of-month m year)
                            year)))
                  1)))
           (day                   ;; Calculate the day by subtraction.
            (- date
               (1- (calendar-absolute-from-french (list month 1 year))))))
    (list month day year))))

(defun cursor-to-french-calendar-date ()
  "Show the French Revolutionary calendar equivalent of the date under the
cursor."
  (interactive)
  (let* ((french-date (calendar-french-from-absolute
                       (calendar-absolute-from-gregorian
                        (or (calendar-cursor-to-date)
                            (error "Cursor is not on a date!")))))
         (y (extract-calendar-year french-date))
         (m (extract-calendar-month french-date))
         (d (extract-calendar-day french-date)))
    (if (< y 1)
        (message "Date is pre-French Revolution")
      (if (= m 13)
          (message "Jour %s de l'Annee %d de la Revolution"
                   (aref french-calendar-special-days-array (1- d))
                   y)
        (message "Decade %s, %s de %s de l'Annee %d de la Revolution"
                 (make-string (1+ (/ (1- d) 10)) ?I)
                 (aref french-calendar-day-name-array (% (1- d) 10))
                 (aref french-calendar-month-name-array (1- m))
                 y)))))

(defconst french-calendar-month-name-array
  ["Vendemiaire" "Brumaire" "Frimaire" "Nivose" "Pluviose" "Ventose" "Germinal"
   "Floreal" "Prairial" "Messidor" "Thermidor" "Fructidor"])

(defconst french-calendar-day-name-array
  ["Primidi" "Duodi" "Tridi" "Quartidi" "Quintidi" "Sextidi" "Septidi"
   "Octidi" "Nonidi" "Decadi"])

(defconst french-calendar-special-days-array
  ["de la Vertu" "du Genie" "du Labour" "de la Raison" "de la Recompense"
   "de la Revolution"])
