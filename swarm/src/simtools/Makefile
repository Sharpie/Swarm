SWARMHOME=../..
LIBNAME=simtools
OBJECTS=ControlPanel.o GUISwarm.o simtools.o ProbeDisplay.o \
	CommonProbeDisplay.o SimpleProbeDisplay.o CompleteProbeDisplay.o \
        VarProbeWidget.o MessageProbeWidget.o \
	ClassDisplayWidget.o ProbeDisplayManager.o \
	UName.o InFile.o ObjectLoader.o OutFile.o AppendFile.o ObjectSaver.o \
	ActiveGraph.o ActiveOutFile.o QSort.o NSelect.o \
	ActionCache.o ActionHolder.o Archiver.o GUIComposite.o
HEADERS=ControlPanel.h GUISwarm.h global.h ProbeDisplay.h \
	CommonProbeDisplay.h SimpleProbeDisplay.h CompleteProbeDisplay.h \
        VarProbeWidget.h MessageProbeWidget.h \
	ClassDisplayWidget.h ProbeDisplayManager.h \
	UName.h InFile.h ObjectLoader.h OutFile.h AppendFile.h ObjectSaver.h \
	ActiveGraph.h ActiveOutFile.h QSort.h NSelect.h \
	ActionCache.h ActionHolder.h Archiver.h GUIComposite.h

include $(SWARMHOME)/Makefile.lib

ControlPanel.o: ControlPanel.m ControlPanel.h
simtools.o: simtools.m global.h
ProbeDisplay.o: ProbeDisplay.m ProbeDisplay.h
CommonProbeDisplay.o: CommonProbeDisplay.m CommonProbeDisplay.h
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
AppendFile.o:AppendFile.m AppendFile.h 
ObjectSaver.o: ObjectSaver.m ObjectSaver.h
ActiveGraph.o: ActiveGraph.m ActiveGraph.h
ActiveOutFile.o: ActiveOutFile.m ActiveOutFile.h
QSort.o: QSort.m QSort.h
NSelect.o: NSelect.m NSelect.h
ActionCache.o: ActionCache.h ActionCache.m
ActionHolder.o: ActionHolder.h ActionHolder.m
Archiver.o: Archiver.h Archiver.m
GUIComposite.o: GUIComposite.h GUIComposite.m
