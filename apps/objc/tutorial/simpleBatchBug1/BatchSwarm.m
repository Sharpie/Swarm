// BatchSwarm.m

#import "BatchSwarm.h"
#import "ModelSwarm.h"
#import <activity.h>
#import <simtoolsgui.h>
#import "Parameters.h"

@implementation BatchSwarm

+ createBegin: aZone
{
  BatchSwarm *obj;
 
  obj = [super createBegin: aZone];

  return obj;
}

- createEnd
{
  return [super createEnd];
}

- buildObjects
{
  [super buildObjects];

  if ((modelSwarm = 
       [lispAppArchiver getWithZone: self key: "modelSwarm"]) == nil)
    raiseEvent(InvalidOperation,
               "Can't find the modelSwarm parameters");

  [modelSwarm buildObjects];


  return self;
}


- stopRunning
{
  printf ("I'm the BatchSwarm and I've just run your model. \n That was iteration %lu \n", getCurrentTime());
  [getTopLevelActivity() terminate]; // Terminate the simulation.
  return self;
}


- buildActions
{


  [super buildActions];

  [modelSwarm buildActions];

   
  stopSchedule = [Schedule create: self];
  [stopSchedule at: getInt ((Parameters *)arguments, "experimentDuration") 
                createActionTo: self 
                message: M(stopRunning)];

 
  return self;
}

- activateIn: swarmContext
{

  [super activateIn: swarmContext];


  [modelSwarm activateIn: self];

  [stopSchedule activateIn: self];


  return [self getSwarmActivity];
}




- go 
{
  printf ("You typed `./bug -b' or `bug --batch', so we're running without graphics.\n");

  printf ("bug is running for %d timesteps.\n",getInt(arguments,"experimentDuration") ) ;
 
  //  if (loggingFrequency)
  //  printf ("It is logging data every %d timesteps to: unhappiness.output.\n" loggingFrequency);
  
  [[self getActivity] run];
  return [[self getActivity] getStatus];
}



@end








