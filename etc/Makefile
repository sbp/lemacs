DESTDIR=
LIBDIR=/usr/local/lib
BINDIR=/usr/local/bin
MANDIR=/usr/man/man1
MANEXT=1

# make sure executables are linked statically.
# add -DUSG for SysV movemail and timer
CFLAGS= -g -Bstatic
# For Xenix.  Needed for movemail
#  LOADLIBES= -lx

EXECUTABLES = test-distrib etags ctags wakeup make-docfile \
   digest-doc sorted-doc movemail cvtmail fakemail yow env \
   emacsserver emacsclient b2m hexl

#Executables to install in BINDIR.
#(The others can be left in the emacs/etc subdir; Emacs can find them there.)
INSTALL = wakeup etags ctags b2m

all: ${EXECUTABLES}

install: all
	install -c -s wakeup ${DESTDIR}${LIBDIR}/etc/wakeup
	install -c -s etags ${DESTDIR}${BINDIR}/etags
	install -c -s ctags ${DESTDIR}${BINDIR}/ctags
	install -c -s b2m ${DESTDIR}${BINDIR}/b2m
	install -c -m 444 emacs.1 ${MANDIR}/emacs.${MANEXT}

install.sysv: all
	-cp wakeup ${DESTDIR}${BINDIR}/wakeup.new
	-chmod 755 ${DESTDIR}${BINDIR}/wakeup.new
	-chgrp bin ${DESTDIR}${BINDIR}/wakeup.new
	-chown bin ${DESTDIR}${BINDIR}/wakeup.new
	-mv ${DESTDIR}${BINDIR}/wakeup.new ${DESTDIR}${BINDIR}/wakeup
	-cp etags ${DESTDIR}${BINDIR}/etags.new
	-chmod 755 ${DESTDIR}${BINDIR}/etags.new
	-chgrp bin ${DESTDIR}${BINDIR}/etags.new
	-chown bin ${DESTDIR}${BINDIR}/etags.new
	-mv ${DESTDIR}${BINDIR}/etags.new ${DESTDIR}${BINDIR}/etags
	-cp ctags ${DESTDIR}${BINDIR}/ctags.new
	-chmod 755 ${DESTDIR}${BINDIR}/ctags.new
	-chgrp bin ${DESTDIR}${BINDIR}/ctags.new
	-chown bin ${DESTDIR}${BINDIR}/ctags.new
	-mv ${DESTDIR}${BINDIR}/ctags.new ${DESTDIR}${BINDIR}/ctags
	-cp b2m ${DESTDIR}${BINDIR}/b2m.new
	-chmod 755 ${DESTDIR}${BINDIR}/b2m.new
	-chgrp bin ${DESTDIR}${BINDIR}/b2m.new
	-chown bin ${DESTDIR}${BINDIR}/b2m.new
	-mv ${DESTDIR}${BINDIR}/b2m.new ${DESTDIR}${BINDIR}/b2m
	-cp emacs.1 ${MANDIR}/emacs.${MANEXT}.new
	-chmod 444 ${MANDIR}/emacs.${MANEXT}.new
	-mv ${MANDIR}/emacs.${MANEXT}.new ${MANDIR}/emacs.${MANEXT}

install.xenix:
	cp wakeup ${DESTDIR}${LIBDIR}/etc/wakeup
	chmod 755 ${DESTDIR}${LIBDIR}/etc/wakeup
	cp etags ctags ${DESTDIR}${BINDIR}
	chmod 755 ${DESTDIR}${BINDIR}/etags ${DESTDIR}${BINDIR}/ctags
	cp b2m ${DESTDIR}${BINDIR}
	chmod 755 ${DESTDIR}${BINDIR}/b2m
	cp emacs.1 ${MANDIR}/emacs.${MANEXT}
	chmod 444 ${MANDIR}/emacs.${MANEXT}


distclean:
	-rm -f ${EXECUTABLES} *~ \#* DOC* core

clean:
	-rm -f ${EXECUTABLES} core

# This justs verifies that the non-ASCII characters
# in the file `testfile' have no been clobbered by
# whatever means were used to copy and distribute Emacs.
# If they were clobbered, all the .elc files were clobbered too.
test-distrib: test-distrib.c
	$(CC) -o test-distrib ${CFLAGS} test-distrib.c
	./test-distrib

etags: etags.c
	$(CC) -o etags ${CFLAGS} -DETAGS etags.c $(LOADLIBES)

ctags: etags.c
	$(CC) -o ctags ${CFLAGS} -DCTAGS etags.c $(LOADLIBES)

wakeup: wakeup.c
	$(CC) -o wakeup ${CFLAGS} wakeup.c $(LOADLIBES)

make-docfile: make-docfile.c
	$(CC) -o make-docfile ${CFLAGS} make-docfile.c $(LOADLIBES)

digest-doc: digest-doc.c
	$(CC) -o digest-doc ${CFLAGS} digest-doc.c $(LOADLIBES)

sorted-doc: sorted-doc.c
	$(CC) -o sorted-doc ${CFLAGS} sorted-doc.c $(LOADLIBES)

#
movemail: movemail.c ../src/config.h
	$(CC) -o movemail ${CFLAGS} movemail.c $(LOADLIBES)

cvtmail: cvtmail.c
	$(CC) -o cvtmail ${CFLAGS} cvtmail.c $(LOADLIBES)

fakemail: fakemail.c ../src/config.h
	$(CC) -o fakemail ${CFLAGS} fakemail.c $(LOADLIBES)

yow: yow.c ../src/paths.h
	$(CC) -o yow ${CFLAGS} yow.c $(LOADLIBES)

# this is silly -- just use emacs to edit this file!
# (in any case this program doesn't preserve alphabetical ordering,
#  which is why I'm removing it)
#addyow: addyow.c
#	$(CC) -o addyow ${CFLAGS} addyow.c

env: env.c ../src/config.h
	$(CC) -o env -DEMACS ${CFLAGS} env.c $(LOADLIBES)

emacsserver: emacsserver.c ../src/config.h
	$(CC) -o emacsserver ${CFLAGS} emacsserver.c $(LOADLIBES)

emacsclient: emacsclient.c ../src/config.h
	$(CC) -o emacsclient ${CFLAGS} emacsclient.c $(LOADLIBES)

hexl: hexl.c
	$(CC) -o hexl ${CFLAGS} hexl.c $(LOADLIBES)

# This one is NOT included in EXECUTABLES.
# See ../src/ymakefile.
emacstool: emacstool.c
	$(CC) emacstool.c -o emacstool -g -lsuntool -lsunwindow -lpixrect $(LOADLIBES)
