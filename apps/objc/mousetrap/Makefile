ifeq ($(SWARMHOME),)
SWARMHOME=../swarm-1.0.5
endif
APPLICATION=mousetrap
OBJECTS=main.o Mousetrap.o MousetrapModelSwarm.o MousetrapObserverSwarm.o MousetrapBatchSwarm.o
APPLIBS=-lspace

include $(SWARMHOME)/Makefile.appl

main.o: main.m MousetrapObserverSwarm.h MousetrapBatchSwarm.h
Mousetrap.o: Mousetrap.m Mousetrap.h
MousetrapModelSwarm.o: MousetrapModelSwarm.m MousetrapModelSwarm.h
MousetrapObserverSwarm.o: MousetrapObserverSwarm.h MousetrapObserverSwarm.m
MousetrapBatchSwarm.o: MousetrapBatchSwarm.h MousetrapBatchSwarm.m
