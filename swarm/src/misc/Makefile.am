MODULE = misc
CURRENT = 2
REVISION = 3
AGE = 1
miscdir = $(includedir)/Swarm

include $(top_srcdir)/src/Makefile.rules 

INCLUDES = -I$(srcdir)/.. -I$(top_srcdir)/libobjc

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libmisc.la

ARGP_SRC = argp-ba.c argp-eexst.c argp-fmtstream.c argp-fs-xinl.c argp-help.c argp-parse.c argp-pv.c argp-pvh.c argp-xinl.c getopt1.c getopt.c

EXTRA_DIST = memchr.c memcpy.c memset.c realpath.c stpcpy.c strchr.c strcmp.c strlen.c strncmp.c strndup.c strnlen.c strpbrk.c strsep.c strtoll.c argp.h $(ARGP_SRC)

EXTRA_HEADERS = argp.h

SRC = xmalloc.c find_executable.c dropdir.c avl.c avltr.c debugabort.c strdup.c quicksort.c

noinst_HEADERS = argp-fmtstream.h argp-namefrob.h mem.h

libmisc_la_SOURCES = $(SRC)
libmisc_la_LIBADD = @LTLIBOBJS@
libmisc_la_LDFLAGS = @inst_LDFLAGS@

misc_HEADERS = $(MODULE).h @ARGP_H@ getopt.h avl.h avltr.h
