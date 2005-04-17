
#import <objectbase.h>
#import <simtoolsgui.h>
#import <simtoolsgui/GUISwarm.h>
#import <space.h>
#import <activity.h>

#import <collections.h>
#import <simtools.h>
#import "ModelSwarm.h"

@interface ObserverSwarm : GUISwarm {
  int displayFrequency;				

  id displayActions;				
  id displaySchedule;

  ModelSwarm * modelSwarm;	 

}

+ createBegin: (id)aZone;
- createEnd;
- buildObjects;
- buildActions;
- (id <Activity>)activateIn: (id)swarmContext;
- checkToStop;
- (void)drop;

@end
