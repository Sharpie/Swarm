MODULE = simtools
CURRENT = 4
REVISION = 0
AGE = 0
simtoolsdir = $(includedir)/Swarm

include $(top_srcdir)/src/Makefile.rules

INCLUDES = $(LIBOBJCINCLUDES) -I.. -I$(srcdir)/.. -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../random $(GUIDIRINCLUDE) $(SIMTOOLSGUIINCLUDE) -I$(srcdir)/../misc $(GNUSTEPLIBINCLUDES)

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libsimtools.la

SRC = InFile.m OutFile.m AppendFile.m ObjectLoader.m ObjectSaver.m NSelect.m QSort.m UName.m

libsimtools_la_SOURCES = $(SRC)
libsimtools_la_LDFLAGS = @inst_LDFLAGS@

simtools_HEADERS = $(MODULE).h InFile.h OutFile.h AppendFile.h ObjectLoader.h ObjectSaver.h NSelect.h QSort.h UName.h initSwarm.h
