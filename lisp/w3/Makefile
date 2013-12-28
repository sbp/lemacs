# what emacs is called on your system
EMACS = ../../src/xemacs

# where the Info file should go
INFODIR = ../../info

# where the w3 lisp files should go
#LISPDIR = ~/lisp

# Change this to be where your .emacs file is stored
DOTEMACS      = $$HOME/.emacs

# Change this to be how to convert texinfo files into info files
# examples:
#	${EMACS} -batch -q -f batch-texinfo-format
#	makeinfo
MAKEINFO      = makeinfo

############## no user servicable parts beyond this point ###################
# Have to preload a few things to get a nice clean compile
DEPS = -l ./w3-docomp.el -l ./w3-vars.el

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q

# files that contain variables and macros that everything else depends on
CORE = w3-docomp.el

OBJECTS = \
	w3.elc w3-vars.elc w3-emacs.elc w3-emacs19.elc w3-epoch.elc	\
	w3-hyperbole.elc w3-lemacs.elc w3-mule.elc			\
	w3-next.elc w3-mac.elc w3-search.elc

SOURCES = \
	w3.el w3-vars.el w3-emacs.el w3-emacs19.el w3-epoch.el		\
	w3-hyperbole.el w3-lemacs.el w3-mule.el				\
	w3-next.el w3-search.el w3-mac.el

EMACSTEMPLATE = add-to-dot-emacs

DISTFILES     = ${EMACSTEMPLATE} Makefile ChangeLog ${SOURCES}          \
		w3.texinfo w3-docomp.el

w3:	w3-docomp.el $(OBJECTS)

all:	w3.info w3 emacs

#install: all
#	echo Installing in ${LISPDIR}
#	cp ${SOURCES} ${OBJECTS} ${LISPDIR}

mailcap:
	cp mailcap ~/.mailcap

emacs:	${EMACSTEMPLATE}
	echo Creating ${DOTEMACS} file
	touch ${DOTEMACS}
	cat ${EMACSTEMPLATE} | sed 's@THELISPDIR@${LISPDIR}@g' >> ${DOTEMACS}

clean:
	rm -f $(OBJECTS)

w3.info:	w3.texinfo
	@rm -f w3.texi
	@cat w3.texinfo | 		\
	sed 's@setfilename ../info@setfilename ${INFODIR}@g' > w3.texi
	@${MAKEINFO} w3.texi
	@rm -f w3.texi

w3.dvi:		w3.texinfo
	tex w3.texinfo
	texindex w3.cp  w3.fn  w3.ky  w3.pg  w3.tp  w3.vr
	tex w3.texinfo
	rm -f 	w3.cp  w3.fn  w3.ky  w3.pg  w3.tp  w3.vr 	\
		w3.cps w3.fns w3.kys w3.pgs w3.tps w3.vrs	\
		w3.log w3.toc w3.aux

w3-emacs.elc:	w3-emacs.el
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile w3-emacs.el

w3-mac.elc:	w3-mac.el
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile w3-mac.el

w3-emacs19.elc:	w3-emacs19.el
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile w3-emacs19.el

w3-epoch.elc:	w3-epoch.el
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile w3-epoch.el

w3-hyperbole.elc:	w3-hyperbole.el
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile w3-hyperbole.el

w3-lemacs.elc:	w3-lemacs.el
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile w3-lemacs.el

w3-next.elc:	w3-next.el
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile w3-next.el

w3-mule.elc:	w3-mule.el
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile w3-mule.el

w3-vars.elc:	w3-vars.el
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile w3-vars.el

w3.elc:	w3.el
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile w3.el

w3-search.elc: w3-search.el
	${EMACS} ${BATCHFLAGS} ${DEPS} -f batch-byte-compile w3-search.el
