MODULE = simtools
CURRENT = 1
REVISION = 0
AGE = 0
simtoolsdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../random -I$(srcdir)/../simtoolsgui -I$(srcdir)/../misc -I.. -I$(srcdir)/..

lib_LTLIBRARIES = libsimtools.la

SRC = InFile.m OutFile.m AppendFile.m \
	Archiver.m \
	ObjectLoader.m ObjectSaver.m \
	ListShuffler.m NSelect.m QSort.m \
	UName.m simtools.m

libsimtools_la_SOURCES = $(SRC)
libsimtools_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

simtools_HEADERS = InFile.h OutFile.h AppendFile.h \
	Archiver.h \
	ObjectLoader.h ObjectSaver.h \
	ListShuffler.h NSelect.h QSort.h \
	UName.h
