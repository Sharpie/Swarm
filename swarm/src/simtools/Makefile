SWARMHOME=../..
LIBNAME=simtools
OBJECTS=ControlPanel.o GUISwarm.o simtools.o ProbeDisplay.o \
	SimpleProbeDisplay.o CompleteProbeDisplay.o VarProbeWidget.o \
	MessageProbeWidget.o ClassDisplayWidget.o ProbeDisplayManager.o \
	UName.o InFile.o ObjectLoader.o OutFile.o ObjectSaver.o \
	ActiveGraph.o ActiveOutFile.o QSort.o NSelect.o
HEADERS=ControlPanel.h GUISwarm.h global.h ProbeDisplay.h \
	SimpleProbeDisplay.h CompleteProbeDisplay.h VarProbeWidget.h \
	MessageProbeWidget.h ClassDisplayWidget.h ProbeDisplayManager.h \
	UName.h InFile.h ObjectLoader.h OutFile.h ObjectSaver.h \
	ActiveGraph.h ActiveOutFile.h QSort.h NSelect.h

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
UName.o: UName.m UName.h
InFile.o: InFile.m InFile.h
ObjectLoader.o: ObjectLoader.m ObjectLoader.h
OutFile.o: OutFile.m OutFile.h
ObjectSaver.o: ObjectSaver.m ObjectSaver.h
ActiveGraph.o: ActiveGraph.m ActiveGraph.h
ActiveOutFile.o: ActiveOutFile.m ActiveOutFile.h
QSort.o: QSort.m QSort.h
NSelect.o: NSelect.m NSelect.h
