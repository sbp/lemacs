vm:	vm.elc
	emacs -batch -q -l ./vm.elc -f batch-byte-compile .

all:	vm.elc vm.info
	emacs -batch -q -l ./vm.elc -f batch-byte-compile .

vm.elc:	vm.el
	emacs -batch -q -f batch-byte-compile vm.el
	emacs -batch -q -l ./vm.elc -f batch-byte-compile vm-*.el

vm.info:	vm.texinfo
	emacs -batch -q vm.texinfo -f texinfo-format-buffer -f save-buffer
