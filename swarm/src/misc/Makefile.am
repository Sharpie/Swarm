MODULE = misc
miscdir = $(includedir)/misc

include $(top_srcdir)/Makefile.rules

lib_LIBRARIES = libmisc.a

libmisc_a_SOURCES = getopt.c getopt1.c strdup.c strndup.c strsep.c stpcpy.c \
	xmalloc.c realpath.c find_executable.c

misc_HEADERS = argp.h getopt.h

include_HEADERS = $(MODULE).h
