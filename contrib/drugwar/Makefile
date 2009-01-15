ifeq ($(SWARMHOME),)
SWARMHOME=/opt/SDGswarm/2.0.1
endif
APPLICATION=drugwar
OBJECTS=Agent.o MemeFactory.o Society.o GUI.o main.o \
	graph/DiGraph.o graph/DiGraphLink.o graph/DiGraphNode.o graph/graph.o

APPLIBS=
OTHERCLEAN=
DATAFILES = GUI-Data.001 Society-Data.001

APPVERSION=0.0.1
BUGADDRESS=gepr@agent-based-modeling.com

include $(SWARMHOME)/etc/swarm/Makefile.appl

.m.o:
	$(OBJC) -o $*.o -c $(OBJCFLAGS) $(CPPFLAGS) $(DLLCPPFLAGS) $(EXTRACPPFLAGS) $(SWARMINCLUDES) $< 

main.o: main.m Society.h Agent.h MemeFactory.h
Agent.o: Agent.m Agent.h
MemeFactory.o: MemeFactory.m MemeFactory.h
Society.o: Society.h Society.m
GUI.o: GUI.h GUI.m Society.h MemeFactory.h Agent.h
graph/DiGraph.o: graph/graph.h graph/DiGraph.[hm]
graph/DiGraphLink.o: graph/graph.h graph/DiGraphLink.[hm]
graph/DiGraphNode.o: graph/graph.h graph/DiGraphNode.[hm]
graph/graph.o: graph/graph.[hm] graph/graph.sym
