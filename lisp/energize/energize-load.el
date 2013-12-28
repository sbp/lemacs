;;; load the elisp side of Energize

;;; Remember, if you add any files here, you have to mention them in
;;; ymakefile or their docstrings won't be available.

(load "c++-mode")
(load "view-less")
(load "evi")
(load "energize/energize-init")
(load "energize/energize-windows")
(load "energize/energize-mode")
(load "energize/energize-menus")
(load "energize/energize-shell")
(load "energize/energize-visit-use")
(load "energize/energize-vi")
(load "energize/energize-advise")

(if (fboundp (intern-soft "initialize-backtrace-logging-internal"))
    (load "energize/backtrace-logging"))

(provide 'energize)
