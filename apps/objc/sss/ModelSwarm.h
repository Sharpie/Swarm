// Sugarscape in Swarm. Copyright © 1997 Nelson Minar
// This program is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/Swarm.h>
#import "SugarSpace.h"
#import "SugarAgent.h"
#import  <collections.h> // for ListShuffler


// The model swarm encapsulates all the objects that go into the Sugarscape
// model. In the simplest case, this is basically a list of agents, a
// sugarspace world, and various parameters.

@interface ModelSwarm: Swarm
{
  // Parameters for the model
  int numAgents;
  int alpha;					  // growth rate for sugar
  int replacement;				  // use replacement rule?
  int maxVision, maxMetabolism;
  int minInitialSugar, maxInitialSugar;
  int deathAgeMin, deathAgeMax;
  int worldXSize, worldYSize;
  char *datafile;

  // Objects in the list
  id agentList;
  id <ListShuffler> shuffler;
  SugarSpace *sugarSpace;
  id reaperQueue;

  // Schedule stuff

  id modelSchedule;
}

// Methods to handle the agents in the world
- addNewRandomAgent;
- agentBirth: (SugarAgent *)agent;
- agentDeath: (SugarAgent *)agent;

// Accessor functions
- (SugarSpace *)getSugarSpace;
- getAgentList;

// Scheduling methods
- buildObjects;
- buildActions;
- activateIn: swarmContext;

@end
