ifeq ($(SWARMHOME),)
SWARMHOME=../swarm-1.0.5
endif
LIBNAME=graph
OBJECTS=DiGraph.o DiGraphLink.o DiGraphNode.o graph.o
HEADERS=DiGraph.h DiGraphLink.h DiGraphNode.h 

# new variables to support a generic "make dist" target
CURDIR=GraphLib     
TARDIR=GraphLib-0.3 
SRCS=DiGraph.m DiGraphLink.m DiGraphNode.m 
# all the files to put into archive
DISTSRCS=Makefile $(HEADERS) $(SRCS) ChangeLog graph.h graph.sym

include $(SWARMHOME)/Makefile.lib
graph.o: graph.m graph.h
DiGraph.o: DiGraph.m DiGraph.h DiGraphLink.h DiGraphNode.h
DiGraphLink.o: DiGraphLink.m DiGraphLink.h 
DiGraphNode.o: DiGraphNode.m DiGraphNode.h 

dist:
	cd ..; ln -s $(CURDIR) $(TARDIR); rm -f $(TARDIR).tar.gz; for f in $(DISTSRCS); do (tar rvf $(TARDIR).tar $(TARDIR)/$$f) done; gzip $(TARDIR).tar; mv $(TARDIR).tar.gz $(CURDIR); rm $(TARDIR); cd $(CURDIR)


