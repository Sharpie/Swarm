ifeq ($(SWARMHOME),)
SWARMHOME=../swarm-2.0
endif
APPLICATION=mousetrap
APPVERSION=2.0
BUGADDRESS=bug-swarm@santafe.edu
OBJECTS=main.o Mousetrap.o MousetrapModelSwarm.o MousetrapObserverSwarm.o MousetrapBatchSwarm.o
DATAFILES = batch.scm

include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m MousetrapObserverSwarm.h MousetrapBatchSwarm.h
Mousetrap.o: Mousetrap.m Mousetrap.h
MousetrapModelSwarm.o: MousetrapModelSwarm.m MousetrapModelSwarm.h
MousetrapObserverSwarm.o: MousetrapObserverSwarm.h MousetrapObserverSwarm.m
MousetrapBatchSwarm.o: MousetrapBatchSwarm.h MousetrapBatchSwarm.m
