bin_SCRIPTS = print-hdf5
EXTRA_DIST = print-hdf5.in

pkgdata_DATA = print-table.scm swarm.scm

noinst_PROGRAMS = findImp

findImp_SOURCES = findImp.m

swarm_srcdir = $(top_srcdir)/src

INCLUDES = -I$(top_builddir)/src -I$(swarm_srcdir) -I$(top_builddir)/libobjc -I$(top_srcdir)/libobjc -I$(swarm_srcdir)/collections -I$(swarm_srcdir)/defobj -I$(swarm_srcdir)/activity -I$(swarm_srcdir)/objectbase -I$(swarm_srcdir)/random -I$(swarm_srcdir)/simtools -I$(swarm_srcdir)/gui -I$(swarm_srcdir)/simtoolsgui -I$(swarm_srcdir)/misc -I. 

USEDLL=@USEDLL@
findImp_LDFLAGS = -L$(top_builddir)/src -L$(top_builddir)/libobjc
 ifeq ($(USEDLL),no)
 findImp_LDADD = -lswarm
 else
 CPPFLAGS = -DDLL
 findImp_LDADD = -lswarmdll -lobjcdll
 endif