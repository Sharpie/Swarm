#import <objectbase.h>
#import <activity.h>
#import <collections.h>
#import <simtools.h>
#import <stdio.h>
#import "BankModelSwarm.h"

@interface BankBatchSwarm : Swarm {
  int displayFrequency;

  id displayActions;	
  id displaySchedule;
  id stopSchedule;

  BankModelSwarm * bankModelSwarm;
  /*
  Averager * unhappinessAverager;	
  */
  FILE * outputFile;			
}

+createBegin: (id) aZone;
-buildObjects;
-buildActions;
-activateIn: (id) swarmContext;
-go;

// special message on ourselves to stop running.
-stopRunning;

// special data write method
-writeData;

@end
