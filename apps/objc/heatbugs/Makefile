ifeq ($(SWARMHOME),)
SWARMHOME=../swarm-1998-08-18
endif
APPLICATION=heatbugs
OBJECTS=Heatbug.o HeatSpace.o main.o HeatbugModelSwarm.o \
	HeatbugObserverSwarm.o HeatbugBatchSwarm.o
DATAFILES = ant.png

include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m Heatbug.h HeatSpace.h HeatbugObserverSwarm.h HeatbugBatchSwarm.h
Heatbug.o: Heatbug.m Heatbug.h
HeatSpace.o: HeatSpace.m HeatSpace.h
HeatbugObserverSwarm.o: HeatbugObserverSwarm.m HeatbugObserverSwarm.h HeatbugModelSwarm.h
HeatbugModelSwarm.o: HeatbugModelSwarm.m HeatbugModelSwarm.h
HeatbugBatchSwarm.o: HeatbugBatchSwarm.m HeatbugBatchSwarm.h HeatbugModelSwarm.h






