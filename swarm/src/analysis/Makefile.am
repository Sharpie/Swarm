MODULE = analysis
CURRENT = 4
REVISION = 0
AGE = 0
analysisdir = $(includedir)/Swarm

include $(top_srcdir)/src/Makefile.rules

INCLUDES = $(LIBOBJCINCLUDES) -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase $(GUIDIRINCLUDE) -I$(srcdir)/../random -I$(srcdir)/../simtools $(SIMTOOLSGUIINCLUDE) -I$(srcdir)/../misc -I.. -I$(srcdir)/.. $(FFIINCLUDES) $(GNUSTEPLIBINCLUDES)

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libanalysis.la

if ENABLE_OPENSTEP
GUI_SRC =
GUI_HDRS =
else
GUI_SRC = ActiveGraph.m EZBin.m EZDistribution.m EZGraph.m FunctionGraph.m
GUI_HDRS = ActiveGraph.h EZBin.h EZDistribution.h EZGraph.h FunctionGraph.h
endif

if DISABLE_GUI
SRC = ActiveOutFile.m Averager.m Entropy.m
else
SRC = ActiveOutFile.m Averager.m Entropy.m $(GUI_SRC)
endif

libanalysis_la_SOURCES = $(SRC)
libanalysis_la_LDFLAGS = @inst_LDFLAGS@

if DISABLE_GUI
analysis_HEADERS =  $(MODULE).h ActiveOutFile.h Averager.h Entropy.h
else
analysis_HEADERS =  $(MODULE).h ActiveOutFile.h Averager.h Entropy.h $(GUI_HDRS)
endif

