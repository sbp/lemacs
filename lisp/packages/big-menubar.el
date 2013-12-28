;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE                 
;;    fib-menubar.el
;;    Source: /usr/junk/emacs-19.3/lisp/local/fib-menubar.el,v 
;;                      
;; PURPOSE              
;;    Lucid EMACS menus
;;                      
;; PROGRAMMER           
;;    Dror Caspi (Author: dror ) (Locker: dror )
;;    slightly modified by jwz
;;                      
;; REVISION             
;;    Revision: 1.11 
;;
;; DATE
;;    Date: 1993/01/31 13:29:35 
;;
;; REMARKS
;;                                                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst big-menubar
  '(("File"
     ["Open File..."               find-file                         t]
     ["Open File, New Screen..."   find-file-new-screen              t]
     ["Open Alternate File..."     find-alternate-file               t]
     "-----"
     ["Include File..."            insert-file                       t]
     "-----"
     ["Toggle Read Only"           toggle-read-only                  t]
     "-----"
     ["Save Buffer"                save-buffer                       t]
     ["Save Buffer As..."          write-file                        t]
     ["Save Some Buffers"          save-some-buffers                 t]
     ["Save Region As..."          write-region                      t]
     ["Revert Buffer"              revert-buffer                     t]
     "-----"
     ["Print Buffer"               lpr-buffer                        t]
     ["Print Region"               lpr-region                        t]
     "-----"
     ["Kill Buffer..."             kill-buffer                       t]
     ["Exit Emacs"                 save-buffers-kill-emacs           t]
     )
    ("Screen"
     ["New Screen"                 x-new-screen                      t]
     ["Delete Screen"              delete-screen                     t]
     ["One Screen"                 one-screen                        t]
     "-----"
     ["Split"                      split-window-vertically           t]
     ["Un-split (Keep This)"       delete-other-windows              t]
     ["Un-split (Keep Others)"     delete-window                     t]
     ["Save Configuration..."      window-config-to-register         t]
     ["Restore Configuration..."   register-to-window-config         t]
     )
    ("Edit"
     ["Undo"                       advertised-undo                   t]
     ["Redo"                       electric-command-history          t]
     "-----"
     ["Cut"                        x-kill-primary-selection          t]
     ["Copy"                       x-copy-primary-selection          t]
     ["Paste"                      x-yank-clipboard-selection        t]
     ["Clear"                      x-delete-primary-selection        t]
     "-----"
     ["Copy to Register..."        copy-to-register                  t]
     ["Paste Register..."          insert-register                   t]
     "-----"
     ["Toggle Overwrite/Insert"    overwrite-mode                    t]
     ["Quoted Insert..."           quoted-insert                     t]
     "-----"
     ("Mark"
      ["Here"                       set-mark-command                  t]
      ["Word"                       mark-word                         t]
      ["Sentence"                   mark-end-of-sentence              t]
      ["Paragraph"                  mark-paragraph                    t]
      ["Page"                       mark-page                         t]
      ["Balanced Expression"        mark-sexp                         t]
      ["Lisp Function"              mark-defun                        t]
      ["C Function"                 mark-c-function                   t]
      ["Whole Buffer"               mark-whole-buffer                 t]
      )
     "-----"
     ("Search"
      ["Forward..."                 isearch-forward                   t]
      ["Backward..."                isearch-backward                  t]
      "-----" 
      ["Regexp Forward..."          isearch-forward-regexp            t]
      ["Regexp Backward..."         isearch-backward-regexp           t]
      "-----"
      ["Words Forward..."           word-search-forward               t]
      ["Words Backward..."          word-search-backward              t]
      )
     ("Replace"
      ["Query..."                   query-replace                     t]
      ["Regexp Query..."            query-replace-regexp              t]
      "-----"
      ["All..."                     replace-string                    t]
      ["Regexp All..."              replace-regexp                    t]
      )
     "-----"
     ("Transpose"
      ["Characters"                 transpose-chars                   t]
      ["Words"                      transpose-words                   t]
      ["Lines"                      transpose-lines                   t]
      ["Sentencess"                 transpose-sentences               t]
      ["Paragraphs"                 transpose-paragraphs              t]
      ["Balanced Expressions"       transpose-sexps                   t]
      )
     ("Sort"
      ["Lines"                      sort-lines                        t]
      ["Paragraphs"                 sort-paragraphs                   t]
      ["Pages"                      sort-pages                        t]
      ["Columns"                    sort-columns                      t]
      ["Regexp..."                  sort-regexp-fields                t]
      )
     ("Center"
      ["Line"                       center-line                       t]
      ["Paragraph"                  center-paragraph                  t]
      ["Region"                     center-region                     t]
      )
     ("Indent"
      ["As Previous Line"           indent-relative                   t]
      ["To Column..."               indent-to-column                  t]
      "-----"
      ["Region"                     indent-region                     t]
      ["Balanced Expression"        indent-sexp                       t]
      ["C Expression"               indent-c-exp                      t]
      )
     "-----"
     ("Narrow"
      ["To Region"                  narrow-to-region                  t]
      ["To Page"                    narrow-to-page                    t]
      "-----"
      ["Cancel"                     widen                             t]
      )
     )
    ("Motion"
     ["Goto Mark"                       exchange-point-and-mark           t]
     ["Goto Line..."                    goto-line                         t]
     "-----"
     ["End of Balanced Parentheses ()"       forward-list            t]
     ["Beginning of Balanced Parentheses ()" backward-list           t]
     ["Next Openning Parenthesis ("          down-list               t]
     ["Previous Openning Parenthesis ("      backward-up-list        t]
     ["Next Closing Parenthesis )"           up-list                 t]
     "-----"
     ["End of Balanced Expression"           forward-sexp            t]
     ["Beginning of Balanced Expression"     backward-sexp           t]
     "-----"
     ["End of Function"            end-of-defun                      t]
     ["Beginning of Function"      beginning-of-defun                t]
     "-----"
     ["Next Page"                  forward-page                      t]
     ["Previous Page"              backward-page                     t]
     "-----"
     ["End of Buffer"              end-of-buffer                     t]
     ["Beginning of Buffer"        beginning-of-buffer               t]
     "-----"
     ["Save Current Position..."   point-to-register                 t]
     ["Goto Saved Position..."     register-to-point                 t]
     "-----"
     ["Set Marker..."              set-user-marker                   t]
     ["Goto Marker..."             goto-user-marker                  t]
     ["List Markers"               list-markers                      t]
     "-----"
     ["Set Goal Column"            set-goal-column                   t]
     ["Cancel Goal Column"         (set-goal-column t)               t]
     )
    ("Buffers"   "")
    ("Run"
     ["Compile..."                 compile                           t]
     ["Kill Compilation"           kill-compilation                  t]
     "-----"
     ["Next Error"                 next-error                        t]
     ["Previous Error"             previous-error                    t]
     ["Goto Error"                 compile-goto-error                t]
     "-----"
     ["GDB Debuger"                gdb                               t]
     )
    ("Utilities"
     ["Shell"                      shell                             t]
     "-----"
     ("Mail"
      ["Send"                       mail                              t]
      ["Read"                       rmail                             t]
      ["Read Folder..."             rmail-input                       t]
      )
     ["Dired..."                   dired                             t]
     "-----"
     ["Grep..."                    grep                              t]
     ("Tags"
      ["Set Tags Table File..."     visit-tags-table                  t]
      "-----"
      ["Show Occurance..."          find-tag                          t]
      ["Show Occurance (Other)..."  find-tag-other-window             t]
      ["Next Occurance"             (find-tag nil)                    t]
      ["Next Occurance (Other)"     (find-tag-other-window nil)       t]
      "-----"
      ["Search by Tags..."          tags-search                       t]
      ["Query Replace by Tags..."   tags-query-replace                t]
      ["Continue Search/Replace"    tags-loop-continue                t]
      "-----"
      ["Next File"                  next-file                         t]
      "-----"
      ["List Tags in File..."       list-tags                         t]
      ["List Tags by Regexp..."     tags-apropos                      t]
      )
     "-----"
     ("Spell Check"
      ["Word"                       ispell-word                       t]
      ["Region"                     ispell-region                     t]
      ["Whole Buffer"               ispell-buffer                     t]
      )
     "-----"
     ("Compare Windows"
      ["Exact Match"                 compare-windows                   t]
      ["Ignore White Space"          (compare-windows t)		 t]
      )
     "-----"
     ["Hex Edit File..."           hexl-find-file                     t]
     )
    ("Macro"
     ["Start Macro Recording"      start-kbd-macro                    t]
     ["End Macro Recording"        end-kbd-macro                      t]
     ["Name Last Macro..."         name-last-kbd-macro                t]
     ["Insert Macro in Buffer..."  insert-kbd-macro                   t]
     ["Execute Last Macro"         call-last-kbd-macro                t]
     )
    nil
    ("Help"
     ["Info"                       info                               t]
     ["Describe Mode"              describe-mode                      t]
     ["Command Apropos..."         command-apropos                    t]
     ["Full Apropos..."            apropos                            t]
     ["List Keybindings"           describe-bindings                  t]
     ["Describe Key..."            describe-key                       t]
     ["Describe Function..."       describe-function                  t]
     ["Describe Variable..."       describe-variable                  t]
     "-----"
     ["Unix Manual..."             manual-entry                       t]
     ["Emacs Tutorial"             help-with-tutorial                 t]
     ["Emacs News"                 view-emacs-news                    t]
     "-----"
     ["Total Frustration"          doctor                             t]
     )))

(set-menubar big-menubar)
