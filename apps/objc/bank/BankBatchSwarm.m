// Copyright (C) 1996-1998 Santa Fe Institute.
#import "BankBatchSwarm.h"
#import "BankModelSwarm.h"
#import <activity.h>

@implementation BankBatchSwarm

// createBegin: here we set up the default observation parameters.
+ createBegin: aZone
{
  BankBatchSwarm *obj;

  obj = [super createBegin: aZone];

  obj->displayFrequency = 1;
  return obj;
}

- buildObjects
{
  id modelZone;

  [super buildObjects];

  modelZone = [Zone create: [self getZone]];
  bankModelSwarm = [BankModelSwarm create: modelZone];

  [bankModelSwarm buildObjects];

#if 0
  unhappinessAverager = [Averager createBegin: [self getZone]];
  [unhappinessAverager setList: [bankModelSwarm getBankList]];
  [unhappinessAverager setProbedSelector: M(getUnhappiness)];
  unhappinessAverager = [unhappinessAverager createEnd];
#endif

  // And open a file for writing (see -writeData for comments)
  outputFile = fopen ("banks.data", "w");

  return self;
}  

- buildActions
{
  [super buildActions];
  
  [bankModelSwarm buildActions];
  
#if 0
  displayActions = [ActionGroup create: [self getZone]];
  // Now schedule the update of the unhappiness graph
  [displayActions createActionTo: unhappinessAverager message: M(update)];
  [displayActions createActionTo: self message: M(writeData)];

  // the displaySchedule controls how often we write data out.
  displaySchedule = [Schedule createBegin: [self getZone]];
  [displaySchedule setRepeatInterval: displayFrequency];
  displaySchedule = [displaySchedule createEnd];
  [displaySchedule at: 0 createAction: displayActions];
#endif

  stopSchedule = [Schedule create: [self getZone]];
  [stopSchedule at: 250 createActionTo: self message: M(stopRunning)];
  
  return self;
}  

- activateIn: swarmContext
{
  [super activateIn: swarmContext];


  [bankModelSwarm activateIn: self];

#if 0
  [displaySchedule activateIn: self];
#endif
  [stopSchedule activateIn: self];

  return [self getSwarmActivity];
}

- go
{
  printf("No DISPLAY environment variable was set, so we're running without graphics.\n");
  printf("Bank is running for 250 time steps and writing data to banks.data.\n");

  fprintf(outputFile,"Starting simulation!\n") ;

  [[self getActivity] run];
  return [[self getActivity] getStatus];
}

- writeData
{
#if 0
  fprintf(outputFile, "%d %g\n", getCurrentTime(),
	  [unhappinessAverager getAverage]);
#endif
  return self;
}

- stopRunning
{
  [getTopLevelActivity() terminate];
  fclose (outputFile);
  return self;
}

@end
