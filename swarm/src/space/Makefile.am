MODULE = space
CURRENT = 2
REVISION = 1
AGE = 0
spacedir = $(includedir)/Swarm

include $(top_srcdir)/src/Makefile.rules

INCLUDES = $(LIBOBJCINCLUDES) -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../random $(GUIDIRINCLUDE) -I$(srcdir)/../simtools $(SIMTOOLSGUIINCLUDE) -I$(srcdir)/../misc -I.. -I$(srcdir)/.. $(JAVAINCLUDES) $(GNUSTEPLIBINCLUDES)

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libspace.la

if ENABLE_OPENSTEP
GUI_SRC =
GUI_HDRS =
else
GUI_SRC = Object2dDisplay.m Value2dDisplay.m
GUI_HRDS = Object2dDisplay.h Value2dDisplay.h
endif

if DISABLE_GUI
SRC = Ca2d.m ConwayLife2d.m DblBuffer2d.m Diffuse2d.m Discrete2d.m Grid2d.m Int2dFiler.m
else
SRC = Ca2d.m ConwayLife2d.m DblBuffer2d.m Diffuse2d.m Discrete2d.m Grid2d.m Int2dFiler.m $(GUI_SRC)
endif

libspace_la_SOURCES = $(SRC)
libspace_la_LDFLAGS = @inst_LDFLAGS@

if DISABLE_GUI
space_HEADERS = $(MODULE).h Ca2d.h ConwayLife2d.h DblBuffer2d.h Diffuse2d.h Discrete2d.h Grid2d.h Int2dFiler.h
else
space_HEADERS = $(MODULE).h Ca2d.h ConwayLife2d.h DblBuffer2d.h Diffuse2d.h Discrete2d.h Grid2d.h Int2dFiler.h $(GUI_HDRS)
endif

EXTRA_DIST = Object2dDisplay.h Value2dDisplay.h
