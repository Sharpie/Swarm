.SUFFIXES: .java .class
SWARMHOME=../../../..

INSDIR=$(SWARMHOME)/lib
PTPLOT=$(INSDIR)/ptplot
LIBNAME=ptplot

JAVAC=javac

CLASSES=CmdLineArgException.class Message.class Plot.class PlotApplet.class \
	PlotBox.class PlotDataException.class PlotLive.class \
	PlotPoint.class Pxgraph.class

HEADERS=ptplot.h

# all: $(CLASSES)
# include $(SWARMHOME)/Makefile.lib

install-misc:: $(PTPLOT) $(CLASSES)
	cp $(CLASSES) $(INSDIR)/ptplot

$(PTPLOT):
	-mkdir $(PTPLOT)

# directory is ptplot, so go up
.java.class:
	$(JAVAC) -g -classpath ..:$(CLASSPATH) $<

