// ObserverSwarm.h

#import "ModelSwarm.h"
#import <objectbase.h>
#import <objectbase/Swarm.h>

@interface BatchSwarm: Swarm
{

  id stopSchedule;


  ModelSwarm * modelSwarm;       
}

+ createBegin: aZone;
- createEnd;
- buildObjects;

- stopRunning;
- buildActions;
- activateIn: swarmContext;
- go ;
@end

