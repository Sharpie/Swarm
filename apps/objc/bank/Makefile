ifeq ($(SWARMHOME),)
SWARMHOME=../swarm-1998-08-20
endif
APPLICATION=bank
OBJECTS=FEntity.o FLink.o ILink.o BLink.o FNet.o main.o BankModelSwarm.o \
	BankObserverSwarm.o BankBatchSwarm.o
APPLIBS=-lgraph
OTHERCLEAN=banks.data

include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m BankObserverSwarm.h BankBatchSwarm.h
FEntity.o: FEntity.m FEntity.h
FNet.o: FNet.m FNet.h
FLink.o: FLink.m FLink.h
ILink.o: ILink.m ILink.h FLink.h
BLink.o: BLink.m BLink.h FLink.h
BankModelSwarm.o: BankModelSwarm.m BankModelSwarm.h FNet.h FEntity.h
BankObserverSwarm.o: BankObserverSwarm.m BankObserverSwarm.h BankModelSwarm.h
BankBatchSwarm.o: BankBatchSwarm.m BankBatchSwarm.h BankModelSwarm.h
