// Schellings Segregation Model 
// Code by Benedikt Stefansson, <benedikt@ucla.edu>. 
// First version July 1997
// Second version February 1998

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
  // int numAgents;
  char * neighborhood_type;
  int radius;
  int numRaces;
  BOOL edgeWrap;
  BOOL synchronous;
  int worldSize;
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
  
}

+ createBegin: aZone;		
- createEnd;
				
- buildObjects;
- buildActions;
- stepThroughList;

- (BOOL)randomizeList;

- activateIn: swarmContext;
- getAgentList;
- getWorld;
- (int)getWorldSize;
- (double)getRandomDoubleMin: (double)min Max: (double)max;
- (double)getRandomDouble;
- (int)getRandomIntMin: (int)min Max: (int)max;

@end
