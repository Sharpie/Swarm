MODULE = misc
CURRENT = 2
REVISION = 0
AGE = 1
miscdir = $(includedir)/misc

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/..

lib_LTLIBRARIES = libmisc.la

EXTRA_DIST = memchr.c memcpy.c memset.c \
	realpath.c \
	stpcpy.c strchr.c strcmp.c strdup.c strlen.c strncmp.c \
	strndup.c strnlen.c strpbrk.c strsep.c

SRC = getopt.c getopt1.c xmalloc.c find_executable.c \
	argp-ba.c argp-eexst.c argp-fmtstream.c argp-fs-xinl.c  \
	argp-help.c argp-parse.c argp-pv.c argp-pvh.c argp-xinl.c \
	dropdir.c

noinst_HEADERS = argp-fmtstream.h argp-namefrob.h mem.h

libmisc_la_SOURCES = $(SRC)
libmisc_la_LIBADD = @LIBOBJS@
libmisc_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

misc_HEADERS = argp.h getopt.h
