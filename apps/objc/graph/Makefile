SWARMHOME=../swarm-1.0.4
LIBNAME=graph
OBJECTS=DiGraph.o DiGraphLink.o DiGraphNode.o graph.o
HEADERS=DiGraph.h DiGraphLink.h DiGraphNode.h 

include $(SWARMHOME)/Makefile.lib
graph.o: graph.m graph.h
DiGraph.o: DiGraph.m DiGraph.h DiGraphLink.h DiGraphNode.h
DiGraphLink.o: DiGraphLink.m DiGraphLink.h 
DiGraphNode.o: DiGraphNode.m DiGraphNode.h 
