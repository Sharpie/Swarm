MODULE = simtools
simtoolsdir = $(includedir)/$(MODULE)

include $(top_srcdir)/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../random -I$(srcdir)/../misc -I.. -I$(srcdir)/..

lib_LIBRARIES = libsimtools.a

libsimtools_a_SOURCES = ActiveOutFile.m AppendFile.m Archiver.m InFile.m \
	NSelect.m Object ObjectLoader.m ObjectSaver.m OutFile.m QSort.m
	UName.m simtools.m

simtools_HEADERS = ActiveOutFile.h AppendFile.h Archiver.h InFile.h NSelect.h \
	ObjectLoader.h ObjectSaver.h OutFile.h QSort.h UName.h
