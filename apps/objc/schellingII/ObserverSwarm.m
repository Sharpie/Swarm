#import "ObserverSwarm.h"
#import "ModelSwarm.h"
#import <collections.h>
#import <objectbase.h>
#import "Parameters.h"

@implementation ObserverSwarm

+ createBegin: (id)aZone 
{
  ObserverSwarm * obj;
  id <ProbeMap> probeMap;
  
  obj = [super createBegin: aZone];

  // Fill in the relevant parameters (only one, in this case).
  obj->displayFrequency = 1;

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "displayFrequency"
				    inClass: [self class]]];

  [probeLibrary setProbeMap: probeMap For: [self class]];

  return obj;
}


- createEnd 
{
  return [super createEnd];
}

// Create the objects used in the display of the model. 
- buildObjects 
{
  id modelZone;					  // zone for model.

  [super buildObjects];

  [controlPanel setStateStopped];

  modelZone = [Zone create: self];
  modelSwarm = [ModelSwarm create: modelZone];
  
  // Note what happens if you uncomment this. pj 2005-02-11
  // CREATE_ARCHIVED_PROBE_DISPLAY (modelSwarm);
  CREATE_ARCHIVED_PROBE_DISPLAY (self);


  // Check now if the user hit the quit button: if so, abort.
  if ([controlPanel getState] == ControlStateQuit) return self;

  [modelSwarm buildObjects];


  return self;
}  

- buildActions 
{
  [super buildActions];

  // Tell the model Swarm to build it's schedules
  [modelSwarm buildActions];

  // Then create a group of actions for the observer
  displayActions = [ActionGroup create: self ];

  [displayActions createActionTo: probeDisplayManager message: M(update)];
  [displayActions createActionTo: actionCache         message: M(doTkEvents)];

  [displayActions createActionTo: self                message: M(checkToStop)];

  // Put these actions on a schedule to be repeated at a certain frequency
  displaySchedule = [Schedule createBegin: self];
  [displaySchedule setRepeatInterval: displayFrequency]; // note frequency!
  displaySchedule = [displaySchedule createEnd];
  [displaySchedule at: 0 createAction: displayActions];
  
  return self;
}  

- activateIn: (id)swarmContext 
{
  [super activateIn: swarmContext];

  [displaySchedule activateIn: self];

  [modelSwarm activateIn: self];

  return [self getSwarmActivity];
}



- checkToStop
{
  
  long currentTime = getCurrentTime();
  [(Parameters *)arguments setCurrentTime: currentTime];

  if(currentTime == getInt((Parameters*)arguments,"experimentDuration"))
    {
      [controlPanel setStateStopped];
    }
  return self;
}


- (void)drop
{
  [modelSwarm drop];
}



@end
