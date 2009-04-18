MODULE = openstep
CURRENT = 1
REVISION = 0
AGE = 0
swarmgstepdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../objectbase -I.. -I$(srcdir)/.. -I$(srcdir)/../misc -I$(srcdir)/../activity -I$(srcdir)/../space -I$(srcdir)/../openstep $(XINCLUDES) $(XPMINCLUDES) $(PNGINCLUDES) $(ZLIBINCLUDES) $(JAVAINCLUDES) $(GNUSTEPLIBINCLUDES)

override OBJCFLAGS += $(X_CFLAGS) @CFLAGS_NOWARN_IMPLICIT_INT@

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libswarmopenstep.la

SRC = OpenStepSwarm.m Raster.m ZoomRaster.m openstep.m Additions.m Object2dDisplay.m Value2dDisplay.m

libswarmgstep_la_SOURCES = $(SRC)
libswarmgstep_la_LDFLAGS = @inst_LDFLAGS@

swarmgstep_HEADERS = OpenStepSwarm.h Raster.h ZoomRaster.h Additions.h Object2dDisplay.h Value2dDisplay.h openstep.h
