# what emacs is called on your system
EMACS = ../../src/xemacs

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q -no-site-file

# files that contain variables and macros that everything else depends on
CORE = eos-common.el

OBJECTS = \
	eos-browser.elc eos-common.elc eos-debugger-extra.elc  \
	eos-debugger.elc eos-editor.elc eos-init.elc eos-menubar.elc

SOURCES = \
	eos-browser.el eos-common.el eos-debugger-extra.el  \
	eos-debugger.el eos-editor.el eos-init.el eos-menubar.el

all:	$(OBJECTS)

clean:
	rm -f $(OBJECTS)

eos-browser.elc: eos-browser.el $(CORE)
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile eos-browser.el

eos-debugger.elc: eos-debugger.el $(CORE)
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile eos-debugger.el

eos-debugger-extra.elc: eos-debugger-extra.el $(CORE)
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile eos-debugger-extra.el

eos-editor.elc: eos-editor.el $(CORE)
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile eos-editor.el

eos-menubar.elc: eos-menubar.el $(CORE)
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile eos-menubar.el

eos-common.elc: eos-common.el
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile eos-common.el

eos-init.elc: eos-init.el
	${EMACS} ${BATCHFLAGS} -f batch-byte-compile eos-init.el

