MODULE = activity
CURRENT = 0
REVISION = 0
AGE = 1
activitydir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I.. -I$(srcdir)/..

lib_LTLIBRARIES = libactivity.la

module_headers = classes.h types.h

BUILT_SOURCES = $(module_headers)

SRC = Action.m ActionGroup.m CompoundAction.m Schedule.m \
	SwarmProcess.m XActivity.m activity.m

libactivity_la_SOURCES = $(SRC)
libactivity_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

activity_HEADERS = Action.h ActionGroup.h CompoundAction.h Schedule.h \
	SwarmProcess.h XActivity.h

install-data-local:
	$(INSTALL_DATA) $(module_headers) $(activitydir)
