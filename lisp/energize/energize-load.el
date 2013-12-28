;;; load the elisp side of Energize

;;; Remember, if you add any files here, you have to mention them in
;;; ymakefile or their docstrings won't be available.

(load "c++-mode")
(load "view-less")
(load "userlock")
(load "comint")
(load "shell")
(load "compile")
(load "gdb")
(load "evi")
(load "energize/energize-init")
(load "energize/energize-windows")
(load "energize/energize-mode")
(load "energize/energize-menus")
(load "energize/energize-shell")
(load "energize/energize-visit-use")
(load "energize/energize-vi")
(load "energize/energize-advice")
(load "energize/energize-font-lock")

(autoload 'energize-annotate-print-ps "energize-annoprint" t)
(autoload 'energize-set-font-family "energize-font-size" t)
(autoload 'energize-set-font-size "energize-font-size" t)
(autoload 'energize-set-font-boldness "energize-font-size" t)
(autoload 'blink-paren-init "blink-paren" t)

(if (fboundp (intern-soft "initialize-backtrace-logging-internal"))
    (load "energize/backtrace-logging"))

(provide 'energize)
