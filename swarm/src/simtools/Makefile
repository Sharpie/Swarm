SWARMHOME=../..
LIBNAME=simtools
OBJECTS=ControlPanel.o GUISwarm.o simtools.o ProbeDisplay.o \
	SimpleProbeDisplay.o CompleteProbeDisplay.o VarProbeWidget.o \
	MessageProbeWidget.o ClassDisplayWidget.o ProbeDisplayManager.o
HEADERS=ControlPanel.h GUISwarm.h global.h ProbeDisplay.h \
	SimpleProbeDisplay.h CompleteProbeDisplay.h VarProbeWidget.h \
	MessageProbeWidget.h ClassDisplayWidget.h ProbeDisplayManager.h

include $(SWARMHOME)/Makefile.lib

ControlPanel.o: ControlPanel.m ControlPanel.h
simtools.o: simtools.m global.h
ProbeDisplay.o: ProbeDisplay.m ProbeDisplay.h
SimpleProbeDisplay.o: SimpleProbeDisplay.m SimpleProbeDisplay.h
CompleteProbeDisplay.o: CompleteProbeDisplay.m CompleteProbeDisplay.h
VarProbeWidget.o: VarProbeWidget.m VarProbeWidget.h
MessageProbeWidget.o: MessageProbeWidget.m MessageProbeWidget.h
ClassDisplayWidget.o: ClassDisplayWidget.m ClassDisplayWidget.h
ProbeDisplayManager.o: ProbeDisplayManager.m ProbeDisplayManager.h
GUISwarm.o: GUISwarm.m GUISwarm.h
