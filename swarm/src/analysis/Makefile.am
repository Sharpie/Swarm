MODULE = analysis
CURRENT = 3
REVISION = 0
AGE = 0
analysisdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(top_builddir)/libobjc -I$(top_srcdir)/libobjc -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../gui -I$(srcdir)/../simtools -I$(srcdir)/../simtoolsgui -I$(srcdir)/../misc -I.. -I$(srcdir)/..

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libanalysis.la

SRC = ActiveGraph.m ActiveOutFile.m Averager.m EZBin.m EZDistribution.m \
	EZGraph.m Entropy.m FunctionGraph.m

libanalysis_la_SOURCES = $(SRC)
libanalysis_la_LDFLAGS = @inst_LDFLAGS@

analysis_HEADERS =  ActiveOutFile.h ActiveGraph.h Averager.h EZBin.h \
	EZDistribution.h EZGraph.h Entropy.h FunctionGraph.h

