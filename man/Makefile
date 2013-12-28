CFLAGS = -g
CPP_CFLAGS = $(CFLAGS) -I../src -DHAVE_CONFIG_H -DHAVE_STDARG_H -DHAVE_STRING_H -Dxfree=free
OBJS = makeinfo.o getopt.o getopt1.o alloca.o

# Subdirectories to make recursively.
SUBDIR = lemacs lispref

# The makefiles of the directories in $SUBDIR.
SUBDIR_MAKEFILES = lemacs/Makefile lispref/Makefile

.c.o:
	${CC} -c ${CPP_CFLAGS} $<

../info/%.info : %.texinfo
	-makeinfo $<

srcs = ange-ftp cl dired evi external-widget forms gnus ilisp info ispell \
       pcl-cvs supercite termcap texinfo vip vm w3 standards
info = $(srcs:%=../info/%.info)

all : ${SUBDIR} $(info)

.RECURSIVE: ${SUBDIR}

lemacs: FRC.lemacs
lispref: FRC.lispref

${SUBDIR}: ${SUBDIR_MAKEFILES} makeinfo FRC
	cd $@; $(MAKE) all $(MFLAGS) \
		CC='${CC}' CFLAGS='${CFLAGS}' MAKE='${MAKE}'

makeinfo : $(OBJS)
	$(CC) -o makeinfo $(OBJS)

texindex : texindex.o
	cc -o texindex texindex.o

alloca.c :
	[ -h alloca.c ] || ln -s ../src/alloca.c .

getopt.h :
	[ -h getopt.h ] || ln -s ../lib-src/getopt.h .

getopt.c :
	[ -h getopt.c ] || ln -s ../lib-src/getopt.c .

getopt1.c :
	[ -h getopt1.c ] || ln -s ../lib-src/getopt1.c .

mostlyclean:
	rm -f *.log *.cp *.fn *.ky *.pg *.vr *.o core

clean: mostlyclean
	rm -f *.dvi
	rm -f getopt1.c getopt.c getopt.h alloca.c makeinfo

# These are needed because they are present in the other makefiles.
distclean: clean
realclean: distclean
extraclean: distclean
	-rm -f *~ \#*

makeinfo.o : getopt.h
getopt.o : getopt.h
getopt1.o : getopt.h
texindex.o : getopt.h

FRC FRC.lemacs FRC.lispref:

