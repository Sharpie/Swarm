SWARMHOME=../../../../swarm
APPLICATION=humbug
OBJECTS=main.o HumbugBatchSwarm.o HumbugModelSwarm.o 

APPLIBS=-lspace
OTHERCLEAN=batch.data visual.data errorlist

include $(SWARMHOME)/Makefile.appl

HumbugModelSwarm.o: HumbugModelSwarm.m HumbugModelSwarm.h 

HumbugBatchSwarm.o: HumbugBatchSwarm.m HumbugBatchSwarm.h \
	HumbugModelSwarm.h

main.o: main.m HumbugBatchSwarm.h

