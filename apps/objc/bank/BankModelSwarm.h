// Copyright (C) 1996-1998 Santa Fe Institute.
#import <objectbase/Swarm.h>
#import "FEntity.h"
#import "FNet.h"

@interface BankModelSwarm: Swarm
{
  int population;
  int averageIncome;
  double probIOP;
  double probIOPSuccess;
  double IOPmultiplier;
  double probEncounter;

  id modelActions;
  id modelSchedule;

  id graphCanvas;
  id theFNet;
  id entityList;
}

+ createBegin: aZone;
- createEnd;		

- (double)getProbEncounter;	
- (double)getProbIOP;	
- (double)getProbIOPSuccess;	
- (double)getIOPmultiplier;	
- getRandomFEntity;
- getTheFNet;
- getEntityList;

- setGraphCanvas: aCanvas;
- buildObjects;
- buildActions;
- activateIn: swarmContext;

@end
