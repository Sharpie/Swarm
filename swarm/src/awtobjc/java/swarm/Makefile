SWARMHOME=../../../..
.SUFFIXES: .java .class

JAVAC=$(JAVA_HOME)/bin/javac

SWARMCLASSES=JavaControlPanel.class Frame.class Raster.class JavaInput.class \
	Graph.class Histogram.class RotText.class

SUPPORTSWARMCLASSES=Action.class MemCanvas.class Mutex.class

CLASSES=$(SWARMCLASSES) $(SUPPORTSWARMCLASSES)

.java.class:
	$(JAVAC) -g -classpath .:..:$(CLASSPATH) $<

install-misc:: $(CLASSES)
	test -d $(SWARMHOME)/lib/swarm || mkdir $(SWARMHOME)/lib/swarm
	cp $(CLASSES) $(SWARMHOME)/lib/swarm
