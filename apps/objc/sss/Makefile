ifeq ($(SWARMHOME),)
SWARMHOME=/usr
endif
APPLICATION=sss
APPVERSION=2.2
BUGADDRESS=bug-swarm@swarm.org
OBJECTS=main.o SugarAgent.o SugarSpace.o ModelSwarm.o ObserverSwarm.o 

#DATAFILES = $(foreach f,1 2 3,$(foreach type,obs model,parameters/II-$(f).$(type))) sugarspace.pgm

DATAFILES = sugarspace.pgm

include $(SWARMHOME)/etc/swarm/Makefile.appl

