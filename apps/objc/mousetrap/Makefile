SWARMHOME=../swarm
APPLICATION=mousetrap
OBJECTS=main.o Mousetrap.o MousetrapModelSwarm.o MousetrapObserverSwarm.o
APPLIBS=-lspace

include $(SWARMHOME)/Makefile.appl

main.o: main.m MousetrapObserverSwarm.h
Mousetrap.o: Mousetrap.m Mousetrap.h
MousetrapModelSwarm.o: MousetrapModelSwarm.m MousetrapModelSwarm.h
MousetrapObserverSwarm.o: MousetrapObserverSwarm.h MousetrapObserverSwarm.m
