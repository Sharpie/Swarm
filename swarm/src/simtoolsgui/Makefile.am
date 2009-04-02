MODULE = simtoolsgui
CURRENT = 5
REVISION = 0
AGE = 0
simtoolsguidir = $(includedir)/Swarm

include $(top_srcdir)/src/Makefile.rules

INCLUDES = $(LIBOBJCINCLUDES) -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../objectbase -I$(srcdir)/../gui -I$(srcdir)/../simtools -I$(srcdir)/../misc -I.. -I$(srcdir)/.. $(JAVAINCLUDES)

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libsimtoolsgui.la

SRC = ActionCache.m ActionHolder.m ClassDisplayWidget.m CommonProbeDisplay.m CompleteProbeDisplay.m ControlPanel.m GUIComposite.m GUISwarm.m MessageProbeWidget.m MultiVarProbeDisplay.m MultiVarProbeWidget.m ProbeDisplay.m ProbeDisplayManager.m SimpleProbeDisplay.m SingleProbeDisplay.m VarProbeWidget.m simtoolsgui.m

libsimtoolsgui_la_SOURCES = $(SRC)
libsimtoolsgui_la_LDFLAGS = @inst_LDFLAGS@

simtoolsgui_HEADERS = $(MODULE).h ActionCache.h ActionHolder.h ClassDisplayWidget.h CommonProbeDisplay.h CompleteProbeDisplay.h ControlPanel.h GUIComposite.h GUISwarm.h MessageProbeWidget.h MultiVarProbeDisplay.h MultiVarProbeWidget.h ProbeDisplay.h ProbeDisplayManager.h SimpleProbeDisplay.h SingleProbeDisplay.h VarProbeWidget.h
