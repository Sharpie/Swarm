SWARMHOME=../../..
.SUFFIXES: .java .class

all: install-misc
	$(MAKE) -C swarm
	$(MAKE) -C ptplot

JAVAC=$(JAVA_HOME)/bin/javac

GUICLASSES=AlignmentOriginConstraint.class FractionalLayout.class OriginConstraint.class

.java.class:
	$(JAVAC) -g -classpath .:$(CLASSPATH) $<

install-misc:: $(GUICLASSES)
	cp $(GUICLASSES) $(SWARMHOME)/lib
