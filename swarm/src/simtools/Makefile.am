MODULE = simtools
CURRENT = 3
REVISION = 0
AGE = 0
simtoolsdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(top_builddir)/libobjc -I$(top_srcdir)/libobjc -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../random -I$(srcdir)/../simtoolsgui -I$(srcdir)/../misc -I.. -I$(srcdir)/..

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libsimtools.la

SRC = InFile.m OutFile.m AppendFile.m \
	ObjectLoader.m ObjectSaver.m \
	NSelect.m QSort.m \
	UName.m simtools.m

libsimtools_la_SOURCES = $(SRC)
libsimtools_la_LDFLAGS = @inst_LDFLAGS@

simtools_HEADERS = InFile.h OutFile.h AppendFile.h \
	ObjectLoader.h ObjectSaver.h \
	NSelect.h QSort.h \
	UName.h
