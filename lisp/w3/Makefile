# Makefile for w3-mode
# Written by William M. Perry
# wmperry@indiana.edu, 3/27/93

# Change this to be the emacs on your system.
EMACS         = lemacs

# Change this to be where you want the lisp files installed.
# This will be run through expand-file-name, so '~' will work.
LISPDIR	      = ~/lisp

# Change this to be where your .emacs file is stored
DOTEMACS      = $$HOME/.emacs

# Change this to be how to convert texinfo files into info files
# examples:
#	${EMACS} -batch -q -f batch-texinfo-format
#	makeinfo
MAKEINFO      = makeinfo

#
# --- You should not need to touch anything below here
#

.IGNORE:
.SILENT:
.SUFFIXES: .elc .el .info .texi .html .texi

AUXFILES      = extras/
W3FILES	      = w3-vars.el w3-emacs.el w3-emacs19.el		\
		w3-epoch.el w3-forms.el w3-gopher.el		\
		w3-html+.el w3-hyperbole.el w3-lists.el		\
		w3-lucid.el w3-mime.el w3-misc.el		\
		w3-mosaic.el w3-parse.el w3-typecheck.el	\
		w3-print.el w3-url.el w3.el w3-viewers.el	\
		w3-auth.el w3-new-lucid.el w3-cache.el		\
		w3-next.el w3-mule.el

W3BINS	      = w3-vars.elc w3-misc.elc w3-emacs.elc		\
		w3-emacs19.elc w3-epoch.elc w3-forms.elc 	\
		w3-gopher.elc w3-html+.elc w3-hyperbole.elc	\
		w3-lists.elc w3-lucid.elc w3-mime.elc 		\
		w3-mosaic.elc w3-parse.elc w3-typecheck.elc	\
		w3-print.elc w3-url.elc w3.elc w3-viewers.elc	\
		w3-auth.elc w3-new-lucid.elc w3-cache.elc	\
		w3-next.elc w3-mule.elc

DEPS	      = -l w3-docomp.el -l w3-vars.el

EMACSTEMPLATE = add-to-dot-emacs

DISTFILES     = ${EMACSTEMPLATE} Makefile ChangeLog ${W3FILES}          \
		w3.texi w3-docomp.el

.el.elc:
	${EMACS} -batch -q ${DEPS} -f batch-byte-compile $(@:.elc=.el)

.texi.info:
	${MAKEINFO} $(@:.info=.texi)

.texi.html:
	texi2html -glossary -split -verbose $(@:.html=.texi)

w3:	 w3-docomp.el ${W3BINS}

all: emacs install

emacs:
	echo Creating ${DOTEMACS} file
	touch ${DOTEMACS}
	cat ${EMACSTEMPLATE} | sed 's@THELISPDIR@${LISPDIR}@g' >> ${DOTEMACS}

install: w3
	echo Installing in ${LISPDIR}
	cp ${W3FILES} ${W3BINS} ${LISPDIR}

version: w3.texi
	echo "Creating new version."
	rm -f ChangeLog
	rcs2log -h indiana.edu -i 0 > ChangeLog
	cat OChangeLog >> ChangeLog
	mkdir w3
	co -q ${W3FILES} w3-docomp.el
	cp ${DISTFILES} w3/
	rm -f w3/w3.el w3/w3-vars.el
	./change.pl w3.el yes > w3/w3.el
	./change.pl w3-vars.el no > w3/w3-vars.el
	touch w3/THIS-IS-VERSION-`cat .version`Beta

w3.TAGS: ${W3FILES}
	etags -o w3.TAGS ${W3FILES}

w3.dvi: w3.texi
	tex w3.texi
	texindex w3.cp w3.fn w3.ky w3.pg w3.tp w3.vr
	tex w3.texi
	rm -f 	w3.aux w3.cp w3.cps w3.fn w3.fns w3.ky w3.kys w3.log	\
		w3.pg w3.pgs w3.toc w3.tp w3.tps w3.vr w3.vrs
tar:	version
	echo Making tar files and moving to moose
	tar czf /tmp/w3.tar.gz w3
	rsh mango "rm -f /nfs/moose/u/ftp/pub/elisp/w3/THIS-*"
	rcp /tmp/w3.tar.gz mango:/nfs/moose/u/ftp/pub/elisp/w3/w3.tar.gz
	rcp w3/THIS-IS-VERSION* mango:/nfs/moose/u/ftp/pub/elisp/w3
	rm -fr w3 ChangeLog *.el /tmp/w3.tar.gz

clean: 
	echo Removing intermediate files and backups.
	rm -f *.elc *~ \#* *.tar.Z *.uu *.shar
