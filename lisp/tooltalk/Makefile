# @(#)Makefile 1.3 92/12/16

EMACS=../../src/xemacs

TOOLTALK.el  = tooltalk-macros.el tooltalk-init.el tooltalk-util.el
TOOLTALK.elc = $(TOOLTALK.el:.el=.elc)

all: $(TOOLTALK.elc)

.INIT: load-tooltalk.el $(TOOLTALK.el) 

.SUFFIXES: .elc .el

.el.elc:
	$(EMACS) -batch -q -f batch-byte-compile $(@:.elc=.el)

clean: 
	$(RM) $(TOOLTALK.elc)
