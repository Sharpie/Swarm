// Mousetrap application. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// The MousetrapBatchSwarm is an alternative to the MousetrapObserverSwarm
// It's also a toplevel swarm that directs execution of a model, but it
// operates without any graphics - just does file I/O to store data.

#import <stdio.h>
#import <swarmobject.h>
#import <simtools.h>
#import <analysis.h>
#import <space.h>
#import <activity.h>
#import <collections.h>

#import "MousetrapModelSwarm.h"

@interface MousetrapBatchSwarm: Swarm
{
  int loggingFrequency;		       		 // Frequency of fileI/O

  id displayActions;				 // schedule data structs
  id displaySchedule;

  MousetrapModelSwarm * mousetrapModelSwarm;	 // the Swarm we're observing

                                                 // The EZGraph will be used 
  id triggerGraph;                               // in FileI/O mode rather 
                                                 // than the usual Graphics 
                                                 // mode...
}

+ createBegin: aZone;
- buildObjects;
- buildActions;
- activateIn: (id) swarmContext;
- go;						 // Batch mode needs its own

- checkToStop;					 // Special method to end sim. 

@end

