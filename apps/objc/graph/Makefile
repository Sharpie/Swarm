ifeq ($(SWARMHOME),)
SWARMHOME=../swarm-1.0.5
endif
LIBNAME=graph
TARDIR=GraphLib-0.3
OBJECTS=DiGraph.o DiGraphLink.o DiGraphNode.o graph.o
HEADERS=DiGraph.h DiGraphLink.h DiGraphNode.h 

include $(SWARMHOME)/Makefile.lib
graph.o: graph.m graph.h
DiGraph.o: DiGraph.m DiGraph.h DiGraphLink.h DiGraphNode.h
DiGraphLink.o: DiGraphLink.m DiGraphLink.h 
DiGraphNode.o: DiGraphNode.m DiGraphNode.h 

dist:
	cd ..; ln -s GraphLib $(TARDIR); tar zcvf $(TARDIR).tar.gz $(TARDIR)/DiGraph.m $(TARDIR)/DiGraph.h $(TARDIR)/DiGraphLink.m $(TARDIR)/DiGraphLink.h $(TARDIR)/DiGraphNode.m $(TARDIR)/DiGraphNode.h $(TARDIR)/Makefile $(TARDIR)/graph.m $(TARDIR)/graph.h $(TARDIR)/ChangeLog; mv $(TARDIR).tar.gz GraphLib; rm $(TARDIR); cd GraphLib
