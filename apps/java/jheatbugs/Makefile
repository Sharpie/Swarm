JAVA_CLASSES = \
HeatbugModelSwarm.class \
Heatbug.class \
HeatbugBatchSwarm.class \
HeatbugObserverSwarm.class \
HeatSpace.class \
StartHeatbugs.class



all: executable

run : executable runcurrent

runbatch : executable runcurrentbatch

runcurrent :
	$(SWARMHOME)/bin/javaswarm StartHeatbugs
# ... To invoke with command-line arguments, see current.ksh. 

runcurrentbatch :
	$(SWARMHOME)/bin/javaswarm StartHeatbugs -b
# ... To invoke with command-line arguments, see current.ksh. 

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

wrapup :
	sh wrapup.ksh

# Here we edit HeatbugModelSwarm.java depending on the translation of 
# $SWARMHOME; we have to be sure that HeatbugModelSwarm.java is first in the
# list $JAVA_CLASSES (otherwise another rule may compile it without editing 
# it):
HeatbugModelSwarm.class : HeatbugModelSwarm.java
	echo $(SWARMHOME) | grep    "swarm-[12]\.[01]" \
	 && (grep "new FArgumentsImpl (this, sel)" $< >/dev/null && \
		 perl -pi.bak -e's/new FArgumentsImpl \(this, sel\)/new FArgumentsImpl (this, sel, true)/' $< \
		) || :
	echo $(SWARMHOME) | grep -v "swarm-[12]\.[01]" \
	 && (grep "new FArgumentsImpl (this, sel, true)" $< >/dev/null && \
		 perl -pi.bak -e's/new FArgumentsImpl \(this, sel, true\)/new FArgumentsImpl (this, sel)/' $< \
		) || :
	$(SWARMHOME)/bin/javacswarm $<



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
