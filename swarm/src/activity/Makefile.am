MODULE = activity
CURRENT = 0
REVISION = 0
AGE = 0
activitydir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I.. -I$(srcdir)/..

lib_LIBRARIES = libactivity.a
lib_LTLIBRARIES = libactivity.la

class_headers = classes.h types.h

BUILT_SOURCES = $(class_headers)

SRC = Action.m ActionGroup.m CompoundAction.m Schedule.m \
	SwarmProcess.m XActivity.m activity.m

libactivity_a_SOURCES = $(SRC)
libactivity_la_SOURCES = $(SRC)
libactivity_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

activity_HEADERS = Action.h ActionGroup.h CompoundAction.h Schedule.h \
	SwarmProcess.h XActivity.h activity.h $(class_headers)
