# what Emacs version to build VM for.
# allowed values are 18 and 19.
# can't build for both, sorry.
EMACS_VERSION = 19

# what emacs is called on your system
EMACS = ../../src/xemacs

# where the Info file should go
INFODIR = ../../info

# where the vm.elc, tapestry.elc, etc. files should go
#LISPDIR = /usr/local/lib/emacs/site-lisp

############## no user servicable parts beyond this point ###################

# have to preload the files contains macro or the byte compiler
# will compile everything that references them incorrectly.
PRELOADS = -l ./vm-message.el -l ./vm-misc.el -l ./vm-vars.el

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q

# files that key macro definitions.  almost everything depends on
# them becasue teh byte-compiler inlines macro expansions.
CORE = vm-message.el vm-misc.el

OBJECTS = \
    vm-delete.elc vm-digest.elc vm-edit.elc vm-folder.elc \
    vm-license.elc vm-mark.elc vm-message.elc vm-misc.elc \
    vm-motion.elc vm-page.elc vm-pop.elc vm-reply.elc vm-save.elc \
    vm-search.elc vm-sort.elc vm-summary.elc vm-undo.elc vm-vars.elc \
    vm-version.elc vm-virtual.elc vm-window.elc \
    vm-lucid.elc

SOURCES = \
    vm-delete.el vm-digest.el vm-edit.el vm-folder.el \
    vm-license.el vm-mark.el vm-message.el vm-misc.el \
    vm-motion.el vm-page.el vm-pop.el vm-reply.el vm-save.el \
    vm-search.el vm-sort.el vm-summary.el vm-undo.el vm-vars.el \
    vm-version.el vm-virtual.el vm-window.el \
    vm-lucid.el

vm:	vm.elc sets.elc tapestry.elc

vm.elc:	$(OBJECTS)
	cat $(OBJECTS) > vm.elc

all:	vm.info vm

debug:	$(SOURCES)
	cat $(SOURCES) > vm.elc

#install:	all
#	cp vm.info $(INFODIR)/vm.info
#	cp vm.elc sets.elc tapestry.elc $(LISPDIR)

clean:
	rm -f $(OBJECTS)

vm.info:	vm.texinfo
	$(EMACS) $(BATCHFLAGS) vm.texinfo -l texinfmt -f texinfo-format-buffer -f save-buffer

vm-delete.elc:	vm-delete.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-delete.el

vm-digest.elc:	vm-digest.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-digest.el

vm-edit.elc:	vm-edit.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-edit.el

vm-folder.elc:	vm-folder.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-folder.el

vm-lucid.elc:	vm-lucid.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-lucid.el

vm-license.elc:	vm-license.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-license.el

vm-mark.elc:	vm-mark.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-mark.el

vm-message.elc:	vm-message.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-message.el

vm-misc.elc:	vm-misc.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-misc.el

vm-motion.elc:	vm-motion.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-motion.el

vm-page.elc:	vm-page.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-page.el

vm-pop.elc:	vm-pop.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-pop.el

vm-reply.elc:	vm-reply.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-reply.el

vm-save.elc:	vm-save.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-save.el

vm-search.el:	Makefile vm-search$(EMACS_VERSION).el
	{ test -f vm-search.el && \
	  cmp -s vm-search.el vm-search$(EMACS_VERSION).el && \
	  touch vm-search.el ; } || \
	cp vm-search$(EMACS_VERSION).el vm-search.el

vm-search.elc:	vm-search.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-search.el

vm-sort.elc:	vm-sort.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-sort.el

vm-summary.elc:	vm-summary.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-summary.el

vm-undo.elc:	vm-undo.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-undo.el

vm-vars.elc:	vm-vars.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-vars.el

vm-version.elc:	vm-version.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-version.el

vm-virtual.elc:	vm-virtual.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-virtual.el

vm-window.elc:	vm-window.el $(CORE)
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-window.el

tapestry.elc:	tapestry.el
	$(EMACS) $(BATCHFLAGS) -f batch-byte-compile tapestry.el

sets.elc:	sets.el
	$(EMACS) $(BATCHFLAGS) -f batch-byte-compile sets.el
