#import "BatchSwarm.h"
#import "ModelSwarm.h"
#import <collections.h>
#import <objectbase.h>
#import "Parameters.h"

@implementation BatchSwarm


+ createBegin: (id)aZone 
{
  BatchSwarm * obj;
   
  obj = [super createBegin: aZone];

  obj->displayFrequency = 1;

  return obj;
}


- createEnd 
{
  experimentDuration = getInt(arguments,"experimentDuration");
  return [super createEnd];
}

- buildObjects 
{
  [super buildObjects];

  modelSwarm = [ModelSwarm create: self];
  
  [modelSwarm buildObjects];

  return self;
}  

- buildActions 
{
  [super buildActions];

  [modelSwarm buildActions];

  displayActions = [ActionGroup create: self ];

  displaySchedule = [Schedule create: self setRepeatInterval: 1];
  [displaySchedule at: 0 createActionTo: self message: M(checkToStop)];
  
  return self;
}  

- activateIn: (id)swarmContext 
{
  [super activateIn: swarmContext];

  [displaySchedule activateIn: self];

  //Put model swarm after the display in the schedule, so that
  //we can see & save the "raw state" at time 0.
  [modelSwarm activateIn: self];

  return [self getSwarmActivity];
}


- go
{
  printf ("You typed -b so we're running without graphics.\n");

  //printf ("The program is running for a maximum of %d timesteps.\n",experimentDuration) ;

  [[self getActivity] run];
  return [[self getActivity] getStatus];
}



- checkToStop
{
  long currentTime = getCurrentTime();

  [(Parameters *)arguments setCurrentTime: currentTime];

  if (currentTime >= getInt ((Parameters *)arguments, "experimentDuration") )
    {
      [self stopRunning];
    }

  else if ([modelSwarm checkToStop] == YES)
    {
      [self stopRunning];
    }

  return self;
}

- stopRunning
{
  [getTopLevelActivity() terminate]; // Terminate the simulation.
  return self;
}



- (void)drop
{
  [modelSwarm drop];
}




@end
