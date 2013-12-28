
EMACS=../../src/xemacs

.SUFFIXES: .elc .el

BINS=   vm-delete.elc vm-digest.elc vm-edit.elc vm-folder.elc vm-group.elc  \
        vm-license.elc vm-mark.elc vm-message.elc vm-misc.elc vm-motion.elc \
        vm-page.elc vm-reply.elc vm-save.elc vm-search.elc vm-summary.elc   \
        vm-undo.elc vm-vars.elc vm-version.elc vm-virtual.elc vm-window.elc \
        vm-sort.elc vm-lucid.elc

VM:	vm.elc

DEPS=   -l ./vm-vars.el -l ./vm-message.el -l ./vm-misc.el

.el.elc:
	$(EMACS) -batch -q $(DEPS) -f batch-byte-compile $(@:.elc=.el)

vm.elc: $(BINS)
	cat $(BINS) > $@

vm:	vm.texinfo
	$(EMACS) -batch -q vm.texinfo -f texinfo-format-buffer -f save-buffer

all:	VM vm
