;; gmhist-cmp.el

;; Gmhist support for completer.el by ccm@CS.CMU.EDU (Christopher McConnell).
;; This is known to work with version 3.01 of completer.el.

;; You only need this when you don't like it that TAB and SPC complete
;; partially and rather want M-TAB and M-SPC do that, leaving normal
;; completion on TAB and SPC.

;; Do partial filename completion only with M-SPC and M-TAB (SPC and
;; TAB do usual completion) within gmhist's version of read-file-name.

(require 'completer)			; let it mung the keymaps

;; Establish a filename key map separate from the other gmhist maps:
(setq completer-complete-filenames t
      gmhist-filename-completion-map 'gmhist-completer-filename-completion-map
      gmhist-filename-must-match-map 'gmhist-completer-filename-must-match-map)

;; Fill the map with completer and gmhist key bindings:
(setq gmhist-completer-filename-must-match-map
      (copy-keymap minibuffer-local-must-match-map)
      gmhist-completer-filename-completion-map
      (copy-keymap minibuffer-local-completion-map))
(mapcar
 '(lambda (map)
    (gmhist-define-keys map)
    (define-key map "\e\t" 'completer-complete)
    (define-key map "\e " 'completer-word)
    (define-key map "\t" 'minibuffer-complete)
    (define-key map " " 'minibuffer-complete-word))
 (list gmhist-completer-filename-completion-map
       gmhist-completer-filename-must-match-map))
