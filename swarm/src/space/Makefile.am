MODULE = space
CURRENT = 2
REVISION = 0
AGE = 0
spacedir = $(includedir)/space

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../random -I$(srcdir)/../gui -I$(srcdir)/../simtools -I$(srcdir)/../simtoolsgui -I$(srcdir)/../misc -I.. -I$(srcdir)/..

lib_LTLIBRARIES = libspace.la

SRC = Ca2d.m ConwayLife2d.m DblBuffer2d.m Diffuse2d.m \
	Discrete2d.m Grid2d.m Int2dFiler.m Object2dDisplay.m Value2dDisplay.m

libspace_la_SOURCES = $(SRC)
libspace_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

space_HEADERS = Ca2d.h ConwayLife2d.h DblBuffer2d.h Diffuse2d.h Discrete2d.h \
	Grid2d.h Int2dFiler.h Object2dDisplay.h Value2dDisplay.h

