MODULE = analysis
CURRENT = 4
REVISION = 0
AGE = 0
analysisdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = $(LIBOBJCINCLUDES) -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase $(GUIDIRINCLUDE) -I$(srcdir)/../random -I$(srcdir)/../simtools $(SIMTOOLSGUIINCLUDE) -I$(srcdir)/../misc -I.. -I$(srcdir)/.. $(FFIINCLUDES) $(GNUSTEPLIBINCLUDES)

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libanalysis.la

if DISABLE_GUI
SRC = ActiveOutFile.m Averager.m Entropy.m
else
SRC = ActiveGraph.m ActiveOutFile.m Averager.m EZBin.m EZDistribution.m EZGraph.m Entropy.m FunctionGraph.m
endif

libanalysis_la_SOURCES = $(SRC)
libanalysis_la_LDFLAGS = @inst_LDFLAGS@

analysis_HEADERS =  ActiveOutFile.h ActiveGraph.h Averager.h EZBin.h EZDistribution.h EZGraph.h Entropy.h FunctionGraph.h

