MODULE = misc
miscdir = $(includedir)/misc

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/..

lib_LIBRARIES = libmisc.a

libmisc_a_SOURCES = getopt.c getopt1.c \
	strdup.c strndup.c strsep.c stpcpy.c strnlen.c \
	xmalloc.c realpath.c find_executable.c \
	argp-ba.c argp-eexst.c argp-fmtstream.c argp-fs-xinl.c  \
	argp-help.c argp-parse.c argp-pv.c argp-pvh.c argp-xinl.c

misc_HEADERS = argp.h getopt.h
