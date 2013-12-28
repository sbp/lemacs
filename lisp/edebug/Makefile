# Makefile for the edebug manual.
#
# Sep 1993

# The version of edebug.
VERSION = 3.2

# Redefine `TEX' if `tex' does not invoke plain TeX. For example:
# TEX=platex

TEX=tex

FILES = README Makefile edebug.el cust-print.el edebug-history \
	edebug-lucid.el edebug-emacs19.el \
	cl-specs.el cl-read.el edebug-cl-read.el \
	edebug.tex edebug.texi edebug.info permute-index

# I include the cl package for now.
CLFILES = cl.el cl-extra.el cl-macs.el cl-seq.el cl-compat.el cl.texinfo

SUBDIR = edebug-${VERSION}

all:	edebug.dvi

edebug.cp:	edebug.tex
	# First shot to define xrefs and produce permuted index.
	$(TEX) edebug.tex

edebug.cps:	edebug.cp
	./permute-index edebug cp
	mv permuted.cps edebug.cps

edebug.dvi:	edebug.cps
	# Produce the final dvi.
	$(TEX) edebug.tex

dist edebug.tar.Z:	${FILES}
	rm -rf edebug.tar edebug.tar.Z ${SUBDIR}
	mkdir ${SUBDIR}
	mv ${FILES} ${SUBDIR}
	tar chf edebug.tar ${SUBDIR}
	mv ${SUBDIR}/* .
	compress edebug.tar
	rm -r ${SUBDIR}

cl-dist:	${CLFILES}
	rm -rf cl.tar cl.tar.Z
	tar cf cl.tar ${CLFILES}
	compress cl.tar

mostlyclean clean:
	rm -f edebug.dvi edebug.log edebug.toc
	rm -f edebug.cp edebug.fn edebug.ky edebug.pg edebug.tp edebug.vr

distclean realclean: clean
	rm -f edebug.??s edebug.aux
	rm -f edebug.elc
