// Heatbugs application. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// The HeatbugBatchSwarm is an alternative to the HeatbugObserverSwarm
// It's also a toplevel swarm that directs execution of a model, but it
// operates without any graphics - just does file I/O to store data.

#import <stdio.h>
#import <swarmobject.h>
#import <space.h>
#import <activity.h>
#import <collections.h>
#import <simtools.h>
#import "HeatbugModelSwarm.h"

@interface HeatbugBatchSwarm : Swarm {
  int displayFrequency;				  // one parameter: update freq

  id displayActions;				  // schedule data structs
  id displaySchedule;
  id stopSchedule;

  HeatbugModelSwarm * heatbugModelSwarm;	  // the Swarm we're observing

  Averager * unhappinessAverager;		  // data collection object
  FILE * outputFile;				  // and a file to store data
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
