MODULE = analysis
analysisdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../gui -I$(srcdir)/../simtools -I$(srcdir)/../simtoolsgui -I$(srcdir)/../misc -I.. -I$(srcdir)/..

lib_LIBRARIES = libanalysis.a

libanalysis_a_SOURCES = Averager.m EZBin.m EZDistribution.m EZGraph.m Entropy.m

analysis_HEADERS = Averager.h EZBin.h EZDistribution.h EZGraph.h Entropy.h
