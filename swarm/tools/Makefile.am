bin_SCRIPTS = print-hdf5
SCHEME = print-table.scm swarm.scm
EXTRA_DIST = as print-hdf5.in $(SCHEME)

pkgdata_DATA = $(SCHEME)

noinst_PROGRAMS = findImp

findImp_SOURCES = findImp.m
# force use of libtool to work around dependency generation bug
OBJEXT = lo
# force use of special symbols because execute will work from .o
AM_CPPFLAGS =  -DDLL_EXPORT

swarm_srcdir = $(top_srcdir)/src

INCLUDES = $(LIBOBJCINCLUDES) -I$(top_builddir)/src -I$(swarm_srcdir) -I$(swarm_srcdir)/collections -I$(swarm_srcdir)/defobj -I$(swarm_srcdir)/activity -I$(swarm_srcdir)/objectbase -I$(swarm_srcdir)/random -I$(swarm_srcdir)/simtools -I$(swarm_srcdir)/gui -I$(swarm_srcdir)/simtoolsgui -I$(swarm_srcdir)/misc -I. $(GNUSTEPLIBINCLUDES)

USEDLL=@USEDLL@
findImp_LDFLAGS = -L$(top_builddir)/src $(LIBOBJCLDFLAGS) $(GNUSTEPLIBLDFLAGS)
findImp_LDADD = @SWARMLIBS@ $(GNUSTEPLIBLIBS)
