MODULE = activity
CURRENT = 5
REVISION = 0
AGE = 0
activitydir = $(includedir)/$(MODULE)
JAVAINCLUDES = @JAVAINCLUDES@

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I.. -I$(srcdir)/.. -I$(top_builddir) -I$(top_builddir)/libobjc -I$(top_srcdir)/libobjc -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../misc $(JAVAINCLUDES) $(FFIINCLUDES)

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libactivity.la

CLEAN_FILES = $(TYPEINFO)
BUILT_SOURCES = $(TYPEINFO)

EXTRA_DIST = ActionHomogeneous.m macros.h

SRC = Action.m ActionGroup.m CompoundAction.m Schedule.m SwarmProcess.m XActivity.m activity.m

libactivity_la_SOURCES = $(SRC)
libactivity_la_LDFLAGS = @inst_LDFLAGS@

activity_HEADERS = Action.h ActionGroup.h CompoundAction.h Schedule.h SwarmProcess.h XActivity.h

install-data-local:
	$(INSTALL_DATA) classes.h $(DESTDIR)$(activitydir)
	$(INSTALL_DATA) types.h $(DESTDIR)$(activitydir)
