JAVA_CLASSES = \
HeatbugModelSwarm.class \
Heatbug.class \
HeatbugBatchSwarm.class \
HeatbugObserverSwarm.class \
HeatSpace.class \
StartHeatbugs.class



all: executable

run : executable 

executable : $(JAVA_CLASSES)

clean :
	rm -f *.class

scrub :
	@echo
	@echo
	@echo "Caution: this removes *.html."
	@echo
	@echo "Maybe you would want to name any HTML files you create \"*.htm\""
	@echo "and leave \"*.html\" for javadoc and \"make scrub\"."
	@echo
	@echo
	sleep 3
	make clean
	rm -f *.bak
	rm -f *dump*
	rm -f *junk*
	rm -f *.out
	rm -f *.output
	rm -f *.html
	rm -f package-list



.SUFFIXES: .java-ifdef .java .class .applet .applic

# Setup-free applic from .java, for versions of make that do support
# empty target suffixes:
.java :
	make $*.class
	$(SWARMHOME)/bin/javaswarm $*

# Setup-free applic from .class, for versions of make that do support
# empty target suffixes:
.class :
	$(SWARMHOME)/bin/javaswarm $*

# Setup-free applet from .java, for versions of make that do not support
# empty target suffixes:
.java.applet :
	make $*.class
	appletviewer $*.java

# Setup-free applic from .java, for versions of make that do not support
# empty target suffixes:
.java.applic :
	make $*.class
	$(SWARMHOME)/bin/javaswarm $*

# Setup-free .class from .java:
.java.class :
	$(SWARMHOME)/bin/javacswarm $<

.java-ifdef :
	make $*.class
	$(SWARMHOME)/bin/javaswarm $*

.java-ifdef.applet : 
	make $*.class
	appletviewer $*.java-ifdef

.java-ifdef.class :
	cpp -E $< | sed -e '/^#/d;' >tmp/$*.java
	cd tmp && $(SWARMHOME)/bin/javacswarm -d .. $*.java
