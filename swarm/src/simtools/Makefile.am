MODULE = simtools
CURRENT = 0
REVISION = 0
AGE = 0
simtoolsdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../random -I$(srcdir)/../simtoolsgui -I$(srcdir)/../misc -I.. -I$(srcdir)/..

lib_LIBRARIES = libsimtools.a
lib_LTLIBRARIES = libsimtools.la

SRC = ActiveOutFile.m AppendFile.m Archiver.m InFile.m \
	NSelect.m Object ObjectLoader.m ObjectSaver.m OutFile.m QSort.m \
	UName.m simtools.m

libsimtools_a_SOURCES = $(SRC)
libsimtools_la_SOURCES = $(SRC)
libsimtools_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

simtools_HEADERS = ActiveOutFile.h AppendFile.h Archiver.h InFile.h NSelect.h \
	ObjectLoader.h ObjectSaver.h OutFile.h QSort.h UName.h
