ifeq ($(SWARMHOME),)
SWARMHOME=../swarm-1998-08-20
endif
APPLICATION=mousetrap
OBJECTS=main.o Mousetrap.o MousetrapModelSwarm.o MousetrapObserverSwarm.o MousetrapBatchSwarm.o
DATAFILES = model.setup batch.setup

include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m MousetrapObserverSwarm.h MousetrapBatchSwarm.h
Mousetrap.o: Mousetrap.m Mousetrap.h
MousetrapModelSwarm.o: MousetrapModelSwarm.m MousetrapModelSwarm.h
MousetrapObserverSwarm.o: MousetrapObserverSwarm.h MousetrapObserverSwarm.m
MousetrapBatchSwarm.o: MousetrapBatchSwarm.h MousetrapBatchSwarm.m
