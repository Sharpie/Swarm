#import <objectbase.h>
#import <objectbase/Swarm.h>
#import <space.h>
#import <analysis.h>
#import <collections.h>
#import <simtools.h>
#import "ModelSwarm.h"
#import "Parameters.h"

@interface BatchSwarm : Swarm 
{
  int displayFrequency;				
  int experimentDuration;
  
  id displayActions;				
  id displaySchedule;
  
  ModelSwarm * modelSwarm;	 
  id <Colormap> colormap;
  id <ZoomRaster> worldRaster;
  id <Object2dDisplay> worldDisplay;
  id <EZGraph> moveGraph;

}

// Methods overriden to make the Swarm.
+ createBegin: (id)aZone;
- createEnd;
- buildObjects;
- buildActions;
- activateIn: (id)swarmContext;
- go;
- checkToStop;
- stopRunning;

- (void)drop;

@end
