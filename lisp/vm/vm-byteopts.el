;; get the compiler loaded so we can undo some of the things that
;; happen when it's loaded.
(load "bytecomp" t t nil)
;; Emacs 19 byte compiler complains about too much stuff by default.
;; Turn off most of the warnings here.
(setq byte-compile-warnings '(free-vars))
;; need to use these variables for v18 support.
;; stifle the compiler.
(put 'inhibit-local-variables 'byte-obsolete-variable nil)
