MODULE = swarmgstep
CURRENT = 1
REVISION = 0
AGE = 0
swarmgstepdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

#TCLFILES = analysis.tcl simtools.tcl
#XPMFILES = schedule.xpm trigger.xpm

#EXTRA_DIST = $(TCLFILES) $(XPMFILES)

XPMINCLUDES = @XPMINCLUDES@
TCLINCLUDES = @TCLINCLUDES@
TKINCLUDES = @TKINCLUDES@
ZLIBINCLUDES = @ZLIBINCLUDES@
PNGINCLUDES = @PNGINCLUDES@
XINCLUDES = @XINCLUDES@

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../objectbase -I.. -I$(srcdir)/.. -I$(srcdir)/../gui -I$(srcdir)/../simtoolsgui -I$(srcdir)/../misc -I$(srcdir)/../activity $(TKINCLUDES) $(XINCLUDES) $(TCLINCLUDES) $(XPMINCLUDES) $(PNGINCLUDES) $(ZLIBINCLUDES) $(JAVAINCLUDES) $(GNUSTEPLIBINCLUDES)

override OBJCFLAGS += $(X_CFLAGS) @CFLAGS_NOWARN_IMPLICIT_INT@

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libswarmgstep.la

SRC = GNUstepSwarm.m Raster.m ZoomRaster.m swarmgstep.m

libswarmgstep_la_SOURCES = $(SRC)
libswarmgstep_la_LDFLAGS = @inst_LDFLAGS@

swarmgstep_HEADERS = GNUstepSwarm.h Raster.h ZoomRaster.h

swarmgstep.lo: $(foreach f, $(TCLFILES), $(basename $(f))_tcl.x)
