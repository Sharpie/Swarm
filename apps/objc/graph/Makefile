ifeq ($(SWARMHOME),)
SWARMHOME=../swarm-1998-08-20
endif
LIBNAME=graph
OBJECTS=DiGraph.o DiGraphLink.o DiGraphNode.o graph.o
HEADERS=DiGraph.h DiGraphLink.h DiGraphNode.h 

include $(SWARMHOME)/etc/swarm/Makefile.lib
graph.o: graph.m graph.h
DiGraph.o: DiGraph.m DiGraph.h DiGraphLink.h DiGraphNode.h
DiGraphLink.o: DiGraphLink.m DiGraphLink.h 
DiGraphNode.o: DiGraphNode.m DiGraphNode.h 

