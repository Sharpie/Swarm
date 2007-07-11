// Heatbugs application. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// The HeatbugBatchSwarm is an alternative to the HeatbugObserverSwarm
// It's also a toplevel swarm that directs execution of a model, but it
// operates without any graphics - just does file I/O to store data.

#import <SwarmOSX/GNUstepSwarm.h>
#import "HeatbugModelSwarm.h"

@interface HeatbugBatchSwarm: GNUstepSwarm
{
  int loggingFrequency;	       		  // Frequency of fileI/O

  int experimentDuration;                 // When to Stop the Sim

  id displayActions;			  // schedule data structs
  id displaySchedule;
  id stopSchedule;

  HeatbugModelSwarm *heatbugModelSwarm;	  // the Swarm we're observing

                                          // The EZGraph will be used 
  id unhappyGraph;                        // in FileI/O mode rather 
                                          // than the usual Graphics 
                                          // mode...

  id <Value2dDisplay> heatDisplay;              // display the heat
  id <Object2dDisplay> heatbugDisplay;          // display the heatbugs
}

+ createBegin: aZone;
- buildObjects;
- buildActions;
- activateIn: swarmContext;
- go;

// special message on ourselves to stop running.
- stopRunning;

- (HeatbugModelSwarm *)getModel;
- (id <Object2dDisplay>)getHeatbugDisplay;
- (id <Value2dDisplay>)getHeatDisplay;

@end
