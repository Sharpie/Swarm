bin_SCRIPTS = print-hdf5
SCHEME = print-table.scm swarm.scm
EXTRA_DIST = print-hdf5.in $(SCHEME)

pkgdata_DATA = $(SCHEME)

noinst_PROGRAMS = findImp$(EXEEXT)

findImp__EXEEXT__SOURCES = findImp.m
# force use of libtool to work around dependency generation bug
OBJEXT = lo
# force use of special symbols because execute will work from .o
CPPFLAGS = @CPPFLAGS@ -DDLL

swarm_srcdir = $(top_srcdir)/src

INCLUDES = -I$(top_builddir)/src -I$(swarm_srcdir) -I$(top_builddir)/libobjc -I$(top_srcdir)/libobjc -I$(swarm_srcdir)/collections -I$(swarm_srcdir)/defobj -I$(swarm_srcdir)/activity -I$(swarm_srcdir)/objectbase -I$(swarm_srcdir)/random -I$(swarm_srcdir)/simtools -I$(swarm_srcdir)/gui -I$(swarm_srcdir)/simtoolsgui -I$(swarm_srcdir)/misc -I. 

USEDLL=@USEDLL@
findImp__EXEEXT__LDFLAGS = -L$(top_builddir)/src -L$(top_builddir)/libobjc
findImp__EXEEXT__LDADD = @SWARMLIBS@
