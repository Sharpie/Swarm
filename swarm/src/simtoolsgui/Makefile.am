MODULE = simtoolsgui
simtoolsguidir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../gui -I$(srcdir)/../simtools -I$(srcdir)/../misc -I.. -I$(srcdir)/..

lib_LIBRARIES = libsimtoolsgui.a

libsimtoolsgui_a_SOURCES = ActionCache.m ActionHolder.m ActiveGraph.m \
	ClassDisplayWidget.m CommonProbeDisplay.m CompleteProbeDisplay.m \
	ControlPanel.m GUIComposite.m GUISwarm.m MessageProbeWidget.m \
	ProbeDisplay.m ProbeDisplayManager.m SimpleProbeDisplay.m \
	VarProbeWidget.m simtoolsgui.m

simtoolsgui_HEADERS = ActionCache.h ActionHolder.h ActiveGraph.h \
	ClassDisplayWidget.h CommonProbeDisplay.h CompleteProbeDisplay.h \
	ControlPanel.h GUIComposite.h GUISwarm.h MessageProbeWidget.h \
	ProbeDisplay.h ProbeDisplayManager.h SimpleProbeDisplay.h \
	VarProbeWidget.h global.h
