MODULE = analysis
CURRENT = 2
REVISION = 0
AGE = 1
analysisdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../gui -I$(srcdir)/../simtools -I$(srcdir)/../simtoolsgui -I$(srcdir)/../misc -I.. -I$(srcdir)/..

lib_LTLIBRARIES = libanalysis.la

SRC = ActiveGraph.m ActiveOutFile.m Averager.m EZBin.m EZDistribution.m \
	EZGraph.m Entropy.m FunctionGraph.m

libanalysis_la_SOURCES = $(SRC)
libanalysis_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE) $(VERSION)

analysis_HEADERS =  ActiveOutFile.h ActiveGraph.h Averager.h EZBin.h \
	EZDistribution.h EZGraph.h Entropy.h FunctionGraph.h

