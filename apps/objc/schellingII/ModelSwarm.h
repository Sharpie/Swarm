#import "Person.h"
#import "SchellingWorld.h"
#import <objectbase.h>
#import <space.h>
#import <activity.h>
#import <collections.h>
#import <random.h>
#import <string.h>
#import <objectbase/Swarm.h>

@interface ModelSwarm : Swarm {
  char * neighborhood_type;
  int radius;
  int numRaces;
  BOOL edgeWrap;
  BOOL synchronous;
  int worldXSize, worldYSize;
  double fractionVacant;
  double fractionBlue, fractionRed;
  double blueToleranceLower, blueToleranceUpper;
  double redToleranceLower, redToleranceUpper;
  double otherToleranceLower, otherToleranceUpper;

  unsigned randomSeed;

  BOOL randomize;

  id modelSchedule;

  id <UniformDoubleDist> uniformDouble;
  id <UniformIntegerDist> uniformInteger;
  
  id <List> agentList;
  id world;
  id output;
}

+ createBegin: aZone;		
- createEnd;
				
- buildObjects;
- buildActions;
- stepThroughList;



- activateIn: swarmContext;
- getAgentList;
- getWorld;
- (int)getWorldXSize;
- (int)getWorldYSize;
- (double)getRandomDoubleMin: (double)min Max: (double)max;
- (double)getRandomDouble;
- (int)getRandomIntMin: (int)min Max: (int)max;


- (void)drop;
- (BOOL)checkToStop;

@end
