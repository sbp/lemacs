# what Emacs version to build VM for.
# Allowed values are 18 and 19.
#
# Currently only vm-isearch-forward depends on this being
# correct.  You can use the same VM .elc files under v18 and v19
# Emacs if you don't care about vm-isearch-forward.
#
# Note that .elc files compiled with the v19 byte compiler won't
# work under v18 Emacs, but v18 .elcs will work under v19.  So
# point this at your v18 Emacs binary if you want compatible .elc
# files.
EMACS_VERSION = 19

# what emacs is called on your system
EMACS = ../../src/xemacs

# where the Info file should go
INFODIR = ../../info

# where the vm.elc, tapestry.elc, etc. files should go
#LISPDIR = /usr/local/lib/emacs/site-lisp

# if you have a newer version of reporter.el and you know that it
# works with VM, remove references to reporter from these two
# lines.  Ditto for timezone.el.
#SUPPORT_EL  = reporter.el  timezone.el
#SUPPORT_ELC = reporter.elc timezone.elc

############## no user servicable parts beyond this point ###################

# no csh please
SHELL = /bin/sh

# have to preload the files that contain macro definitions or the
# byte compiler will compile everything that references them
# incorrectly.  also preload a file that sets byte compiler options.
PRELOADS = -l ./vm-byteopts.el -l ./vm-message.el -l ./vm-misc.el -l ./vm-vars.el -l ./vm-version.el

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q

# files that contain key macro definitions.  almost everything
# depends on them because the byte-compiler inlines macro
# expansions.  everything also depends on the byte compiler
# options file since this might do odd things like turn off
# certain compiler optimizations.
CORE = vm-message.el vm-misc.el vm-byteopts.el

OBJECTS = \
    vm-delete.elc vm-digest.elc vm-edit.elc vm-folder.elc \
    vm-license.elc vm-mark.elc vm-message.elc vm-minibuf.elc vm-misc.elc \
    vm-motion.elc vm-page.elc vm-pop.elc vm-reply.elc vm-save.elc \
    vm-search.elc vm-sort.elc vm-summary.elc vm-startup.elc vm-thread.elc \
    vm-undo.elc vm-vars.elc vm-version.elc vm-virtual.elc vm-window.elc \
    vm-lucid.elc

SOURCES = \
    vm-delete.el vm-digest.el vm-edit.el vm-folder.el \
    vm-license.el vm-mark.el vm-message.el vm-minibuf.el vm-misc.el \
    vm-motion.el vm-page.el vm-pop.el vm-reply.el vm-save.el \
    vm-search.el vm-sort.el vm-startup.el vm-summary.el vm-thread.el \
    vm-undo.el vm-vars.el vm-version.el vm-virtual.el vm-window.el \
    vm-lucid.el

vm:	vm.elc

vm.elc:	autoload

noautoload:	$(OBJECTS) tapestry.elc $(SUPPORT_ELC)
	@echo "building vm.elc (with all modules included)..."
	@cat $(OBJECTS) tapestry.elc $(SUPPORT_ELC) > vm.elc

autoload:	$(OBJECTS) tapestry.elc $(SUPPORT_ELC)
	@echo "building vm.elc (with all modules set to autoload)..."
	@cat vm-startup.elc vm-vars.elc vm-version.elc > vm.elc
	@$(EMACS) $(BATCHFLAGS) -l ./make-autoloads -f print-autoloads $(SOURCES) >> vm.elc

all:	vm.info vm

debug:	$(SOURCES) tapestry.el $(SUPPORT_EL)
	@echo "building vm.elc (uncompiled, no autoloads)..."
	@cat $(SOURCES) tapestry.el $(SUPPORT_EL) > vm.elc

#install:	all
#	cp vm.info $(INFODIR)/vm
#	cp *.elc $(LISPDIR)

clean:
	rm -f $(OBJECTS) tapestry.elc $(SUPPORT_ELC)

vm.info:	vm.texinfo
	@echo "making vm.info..."
	@$(EMACS) $(BATCHFLAGS) vm.texinfo -l texinfmt -f texinfo-format-buffer -f save-buffer

vm-delete.elc:	vm-delete.el $(CORE)
	@echo compiling vm-delete.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-delete.el

vm-digest.elc:	vm-digest.el $(CORE)
	@echo compiling vm-digest.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-digest.el

vm-edit.elc:	vm-edit.el $(CORE)
	@echo compiling vm-edit.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-edit.el

vm-folder.elc:	vm-folder.el $(CORE)
	@echo compiling vm-folder.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-folder.el

vm-license.elc:	vm-license.el $(CORE)
	@echo compiling vm-license.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-license.el

vm-lucid.elc:	vm-lucid.el $(CORE)
	@echo compiling vm-lucid.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-lucid.el

vm-mark.elc:	vm-mark.el $(CORE)
	@echo compiling vm-mark.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-mark.el

vm-message.elc:	vm-message.el $(CORE)
	@echo compiling vm-message.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-message.el

vm-minibuf.elc:	vm-minibuf.el $(CORE)
	@echo compiling vm-minibuf.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-minibuf.el

vm-misc.elc:	vm-misc.el $(CORE)
	@echo compiling vm-misc.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-misc.el

vm-motion.elc:	vm-motion.el $(CORE)
	@echo compiling vm-motion.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-motion.el

vm-page.elc:	vm-page.el $(CORE)
	@echo compiling vm-page.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-page.el

vm-pop.elc:	vm-pop.el $(CORE)
	@echo compiling vm-pop.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-pop.el

vm-reply.elc:	vm-reply.el $(CORE)
	@echo compiling vm-reply.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-reply.el

vm-save.elc:	vm-save.el $(CORE)
	@echo compiling vm-save.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-save.el

vm-search.el:	Makefile vm-search$(EMACS_VERSION).el
	@echo making vm-search.el...
	@{ test -f vm-search.el && \
	  cmp -s vm-search.el vm-search$(EMACS_VERSION).el && \
	  touch vm-search.el ; } || \
	 { rm -f vm-search.el; cp vm-search$(EMACS_VERSION).el vm-search.el ; }

vm-search.elc:	vm-search.el $(CORE)
	@echo compiling vm-search.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-search.el

vm-sort.elc:	vm-sort.el $(CORE)
	@echo compiling vm-sort.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-sort.el

vm-startup.elc:	vm-startup.el $(CORE)
	@echo compiling vm-startup.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-startup.el

vm-summary.elc:	vm-summary.el $(CORE)
	@echo compiling vm-summary.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-summary.el

vm-thread.elc:	vm-thread.el $(CORE)
	@echo compiling vm-thread.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-thread.el

vm-undo.elc:	vm-undo.el $(CORE)
	@echo compiling vm-undo.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-undo.el

vm-vars.elc:	vm-vars.el $(CORE)
	@echo compiling vm-vars.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-vars.el

vm-version.elc:	vm-version.el $(CORE)
	@echo compiling vm-version.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-version.el

vm-virtual.elc:	vm-virtual.el $(CORE)
	@echo compiling vm-virtual.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-virtual.el

vm-window.elc:	vm-window.el $(CORE)
	@echo compiling vm-window.el...
	@$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile vm-window.el

tapestry.elc:	tapestry.el
	@echo compiling tapestry.el...
	@$(EMACS) $(BATCHFLAGS) -f batch-byte-compile tapestry.el

timezone.elc:	timezone.el
	@echo compiling timezone.el...
	@$(EMACS) $(BATCHFLAGS) -f batch-byte-compile timezone.el

reporter.elc:	reporter.el
	@echo compiling reporter.el...
	@$(EMACS) $(BATCHFLAGS) -f batch-byte-compile reporter.el
