;; edebug-lucid.el  -*- Syntax: Emacs-Lisp; Mode: emacs-lisp -*- ;; 
;; Date: Fri, 26 Mar 93 18:03:24 +0100
;; From: Guido Bosch <Guido.Bosch@loria.fr>

(defvar edebug-mode-menu
  '("Edebug Commands"
    "----"
    ["Step" edebug-step-mode t] ;; was edebug-step-through-mode
    ["Next" edebug-next-mode t]
    ["Trace" edebug-trace-mode t]
    ["Continue" edebug-continue-mode t]
    ["Go" edebug-go-mode t]
    ["Stop" edebug-stop t]
    "----"
    ("More Motion"
     ["Trace Fast" edebug-Trace-fast-mode t]
     ["Continue Fast" edebug-Continue-fast-mode t]
     ["Go Nonstop" edebug-Go-nonstop-mode t]
     "----"
     ["Forward Sexp" edebug-forward-sexp t]
     ["Step In" edebug-step-in t] 
     ["Step Out" edebug-step-out t]
     ["Goto Here" edebug-goto-here t])
    ("Breakpoints"
     ["Set Breakpoint" edebug-set-breakpoint t]
     ["Unset Breakpoint" edebug-unset-breakpoint t]
     ["Set Conditional Breakpoint" edebug-set-conditional-breakpoint t]
     ["Set Global Break Condition" edebug-set-global-break-condition t]
     ["Show Next Breakpoint" edebug-next-breakpoint t])
    ("Examinating"
     ["Previous Result" edebug-previous-result t]
     ["Bounce Point" edebug-bounce-point t]
     ["View Outside" edebug-view-outside t]
     ["Toggle Save Windows" edebug-toggle-save-windows t]
     ["Eval Expression" edebug-eval-expression t]
     ["Eval Last Sexp" edebug-eval-last-sexp t]
     ["Show Backtrace" edebug-backtrace t])
    ("Miscellaneous"
     ;; ["Help" edebug-help t]
     ["Where" edebug-where t]
     ["Display Freq Count" edebug-display-freq-count t]
     ["Visit Eval List" edebug-visit-eval-list t])
    "----"
    ["Top Level"  top-level t]
    ["Top Level Nonstop" edebug-top-level-nonstop t]
    ["Abort" abort-recursive-edit t]
    ))


(defun edebug-mode-menu (event)
  (interactive "@e")
  (popup-menu edebug-mode-menu))

(define-key edebug-mode-map 'button3 'edebug-mode-menu)	

(provide 'edebug-lucid)
