MODULE = misc
CURRENT = 0
REVISION = 0
AGE = 0
miscdir = $(includedir)/misc

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/..

lib_LTLIBRARIES = libmisc.la

SRC = getopt.c getopt1.c \
	strdup.c strndup.c strsep.c stpcpy.c strnlen.c \
	xmalloc.c realpath.c find_executable.c \
	argp-ba.c argp-eexst.c argp-fmtstream.c argp-fs-xinl.c  \
	argp-help.c argp-parse.c argp-pv.c argp-pvh.c argp-xinl.c

noinst_HEADERS = argp-fmtstream.h argp-namefrob.h

libmisc_la_SOURCES = $(SRC)
libmisc_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

misc_HEADERS = argp.h getopt.h
