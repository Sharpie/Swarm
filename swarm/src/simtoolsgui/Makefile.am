MODULE = simtoolsgui
CURRENT = 0
REVISION = 0
AGE = 0
simtoolsguidir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../gui -I$(srcdir)/../simtools -I$(srcdir)/../misc -I.. -I$(srcdir)/..

lib_LIBRARIES = libsimtoolsgui.a
lib_LTLIBRARIES = libsimtoolsgui.la

SRC = ActionCache.m ActionHolder.m ActiveGraph.m \
	ClassDisplayWidget.m CommonProbeDisplay.m CompleteProbeDisplay.m \
	ControlPanel.m GUIComposite.m GUISwarm.m MessageProbeWidget.m \
	ProbeDisplay.m ProbeDisplayManager.m SimpleProbeDisplay.m \
	VarProbeWidget.m simtoolsgui.m

libsimtoolsgui_a_SOURCES = $(SRC)
libsimtoolsgui_la_SOURCES = $(SRC)
libsimtoolsgui_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

simtoolsgui_HEADERS = ActionCache.h ActionHolder.h ActiveGraph.h \
	ClassDisplayWidget.h CommonProbeDisplay.h CompleteProbeDisplay.h \
	ControlPanel.h GUIComposite.h GUISwarm.h MessageProbeWidget.h \
	ProbeDisplay.h ProbeDisplayManager.h SimpleProbeDisplay.h \
	VarProbeWidget.h global.h
