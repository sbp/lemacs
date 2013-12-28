; always take .el files.
; don't put stuff into pure segment to avoid pure-space-exceeded error.

(let ((load-ignore-elc-files t)
      (purify-flag nil))
  (load "loadup.el"))

(load "loadup.el")
