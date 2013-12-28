CPP = $(CC) -E
# For Solaris2, use:
# CPP = $(CC) -E -Xs
MAKE = make  # BSD doesn't have it as a default.
#Note: an alternative is  CPP = /lib/cpp

all: xmakefile doall

doall:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} all

#This is used in making a distribution.
#Do not use it on development directories!
distclean:
	-rm -f paths.h config.h emacs-* xemacs temacs cemacs pure_cemacs pure_cemacs.* xmakefile core *~ \#* *.o
	-cd lwlib && $(MAKE) clean

clean:
	-rm -f xemacs temacs cemacs pure_cemacs pure_cemacs.* xmakefile core \#* *.o
	-cd lwlib && $(MAKE) clean

xemacs: xmakefile doxemacs

doxemacs:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} xemacs

temacs: xmakefile dotemacs

dotemacs:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} temacs

release: xmakefile dorelease

dorelease:
	$(MAKE) $(MAKEOVERRIDES) -f xmakefile ${MFLAGS} release

# If you have a problem with cc -E here, changing
# the definition of CPP above may fix it.
xmakefile: ymakefile config.h
	rm -f xmakefile
	cp ymakefile junk.c
	$(CPP) junk.c | sed -e 's/^#.*//' -e 's/^[ \f\t][ \f\t]*$$//' -e 's/^ /	/' -e 's/\//' | \
	sed -n -e '/^..*$$/p' > xmakefile
	rm -f junk.c

tags TAGS::
	etags [a-zA-Z]*.h [a-zA-Z]*.c \
      ./lwlib/[a-zA-Z]*.h ./lwlib/[a-zA-Z]*.c \
      ../lisp/[a-z]*.el ../lisp/*/[a-z]*.el

cemacs: xmakefile docemacs

docemacs:
	${MAKE} -f xmakefile "EMACS_ENERGIZE_CONNECT=-Xez" cemacs
