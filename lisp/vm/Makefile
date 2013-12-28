EMACS=lemacs

VM:
	$(EMACS) -batch -q -l ./vm-message.el -l ./vm-misc.el -f batch-byte-compile .
	cat vm-*.elc > vm.elc

vm:	vm.texinfo
	$(EMACS) -batch -q vm.texinfo -f texinfo-format-buffer -f save-buffer

all:	VM vm
