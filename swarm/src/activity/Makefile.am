MODULE = activity
activitydir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I.. -I$(srcdir)/..

lib_LIBRARIES = libactivity.a

class_headers = classes.h types.h

BUILT_SOURCES = $(class_headers)

libactivity_a_SOURCES = Action.m ActionGroup.m CompoundAction.m Schedule.m \
	SwarmProcess.m XActivity.m activity.m

activity_HEADERS = Action.h ActionGroup.h CompoundAction.h Schedule.h \
	SwarmProcess.h XActivity.h activity.h $(class_headers)
