CFLAGS = -DHAVE_STRING_H -Dxfree=free
OBJS = makeinfo.o getopt.o getopt1.o alloca.o

all : makeinfo

makeinfo : $(OBJS)
	cc -o makeinfo $(OBJS)

alloca.c :
	ln -s ../src/alloca.c .

getopt.c :
	ln -s ../etc/getopt.c .
	ln -s ../etc/getopt.h .

getopt1.c :
	ln -s ../etc/getopt.c .
