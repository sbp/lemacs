# @(#)Makefile 1.4 94/04/19

EMACS=../../src/xemacs

TOOLTALK.el  = tooltalk-macros.el tooltalk-init.el tooltalk-util.el
TOOLTALK.elc = $(TOOLTALK.el:.el=.elc)

all: $(TOOLTALK.elc)

.INIT: tooltalk-load.el $(TOOLTALK.el) 

.SUFFIXES: .elc .el

.el.elc:
	$(EMACS) -batch -q -f batch-byte-compile $(@:.elc=.el)

clean: 
	$(RM) $(TOOLTALK.elc)
