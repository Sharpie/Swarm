MODULE = space
spacedir = $(includedir)/space

include $(top_srcdir)/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../random -I$(srcdir)/../gui -I$(srcdir)/../simtools -I$(srcdir)/../simtoolsgui -I.. -I$(srcdir)/..

lib_LIBRARIES = libspace.a

libspace_a_SOURCES = Ca2d.m ConwayLife2d.m DblBuffer2d.m Diffuse2d.m \
	Discrete2d.m Grid2d.m Int2dFiler.m Object2dDisplay.m Value2dDisplay.m

space_HEADERS = Ca2d.h ConwayLife2d.h DblBuffer2d.h Diffuse2d.h Discrete2d.h \
	Grid2d.h Int2dFiler.h Object2dDisplay.h Value2dDisplay.h

include_HEADERS = $(MODULE).h
