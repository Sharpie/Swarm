#import <objectbase.h>
#import <space.h>
#import <activity.h>
#import <collections.h>
#import <simtools.h>
#import "ForestModelSwarm.h"
#import "Species.h"

@interface ForestBatchSwarm : Swarm {
  int metricFrequency ;
  int forestFrequency ;
  int experimentDuration ;

  int speciesNumber ;
  id speciesList ;

  id metricSchedule;
  id forestSchedule;
  id durationSchedule;

  ForestModelSwarm * forestModelSwarm;

  id speciesGraph, entropyGraph ;
  id speciesEntropy ;
  id forestFiler ;
  id forestNamer ;
}

// Methods overriden to make the Swarm.
+createBegin: (id) aZone;
-createEnd;
-buildObjects;
-buildActions;
-activateIn: (id) swarmContext;
-go;
-snapShot ;
-(void) drop ;
@end




