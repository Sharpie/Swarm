// Sugarscape in Swarm. Copyright © 1997 Nelson Minar
// This program is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "ModelSwarm.h"
//#import <objectbase.h>
#import <random.h>  //uniformUnsRand uniformIntRand

#include <misc.h> // strdup
@implementation ModelSwarm

// Code to create the model swarm parameter set.
// Note that we don't make probes for all the variables - some variables,
// like world size, aren't likely to be modified.
+ createBegin: aZone
{
  ModelSwarm *obj;
  id <ProbeMap> probeMap;

  obj = [super createBegin: aZone];

  // Parameters for the simulation
  obj->numAgents = 400;
  obj->alpha = 1;
  obj->replacement = 0;
  obj->maxVision = 6;
  obj->maxMetabolism = 4;
  obj->minInitialSugar = 5;
  obj->maxInitialSugar = 25;
  obj->deathAgeMin = 99998;
  obj->deathAgeMax = 100000;
  obj->worldXSize = 50;
  obj->worldYSize = 50;
  obj->datafile = strdup("sugarspace.pgm");


  // The creation of a probe map - these parameters are the ones that
  // are easy to modify in the GUI.
  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "numAgents"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "alpha"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "replacement"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "maxMetabolism"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "maxVision"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "minInitialSugar"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "maxInitialSugar"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "deathAgeMin"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "deathAgeMax"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "datafile"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForMessage: "addNewRandomAgent"
				    inClass: [self class]]];

  [probeLibrary setProbeMap: probeMap For: [self class]];
  
  return obj;
}

// Build objects is the code where the model swarm creates all the
// agents and other objects in the simulation model.
- buildObjects
{
  int i;
  
  [super buildObjects];

  // First, set up the object that is the sugarspace - the environment.
  sugarSpace = [SugarSpace createBegin: self];
  [sugarSpace setSizeX: worldXSize Y: worldYSize];
  [sugarSpace setMaxSugarDataFile: datafile];
  sugarSpace = [sugarSpace createEnd];
  [sugarSpace setSugarGrowRate: alpha];

  // create a list to store all the agents
  agentList = [List create: self];

  // And create a bunch of agents to live in the world.
  for (i = 0; i < numAgents; i++)
    [self addNewRandomAgent];

  // Create a "reaper queue" to manage agent deaths
  reaperQueue = [List create: self];

  // And create a suffler object to randomize agent order
  shuffler = [ListShuffler createBegin: self];
  [shuffler setUniformRandom: uniformUnsRand];
  shuffler = [shuffler createEnd];
  
  return self;
}

// Now it's time to create the schedule.
- buildActions
{

  id modelActions;  // holds together a sequence of actions for each timestep
  [super buildActions];

  // One time tick, a set of several actions:
  //   randomize the order of agent updates (to be fair)
  //   update all the agents
  //   kill off the agents who just died
  //   update the sugar on the world
  modelActions = [ActionGroup create: self];
  [modelActions createActionTo: sugarSpace message: M(updateSugar)];
  [modelActions createActionTo: shuffler message: M(shuffleWholeList:) : agentList];
  [modelActions createActionForEach: agentList message: M(step)];
  [modelActions createActionTo: self message: M(reapAgents)];

  // The schedule is just running our actions over and over again
  modelSchedule = [Schedule createBegin: self];
  [modelSchedule setRepeatInterval: 1];
  modelSchedule = [modelSchedule createEnd];
  [modelSchedule at: 0 createAction: modelActions];

  return self;
}

// Create a new agent at random and put it in the world.
- addNewRandomAgent
{
  int x, y;
  SugarAgent *agent;

  // turn off these warnings.
  [[sugarSpace getAgentGrid] setOverwriteWarnings: 0];

  // Create the agent object
  agent = [SugarAgent createBegin: self];
  [agent setModelSwarm: self];
  agent = [agent createEnd];

  // Give the agent a random initial position and parameters.
  x = [uniformIntRand getIntegerWithMin: 0 withMax: [sugarSpace getSizeX]];
  y = [uniformIntRand getIntegerWithMin: 0 withMax: [sugarSpace getSizeY]];
  [sugarSpace addAgent: agent atX: x Y: y];
  [agent setCurrentSugar: 
	   [uniformIntRand getIntegerWithMin: minInitialSugar
			   withMax: maxInitialSugar]];
  [agent setMetabolism:
	   [uniformIntRand getIntegerWithMin: 1 withMax: maxMetabolism]];
  [agent setVision: 
	   [uniformIntRand getIntegerWithMin: 1 withMax: maxVision]];
  [agent setDeathAge:
	   [uniformIntRand getIntegerWithMin: deathAgeMin
			   withMax: deathAgeMax]];
  
  [self agentBirth: agent];

  // turn the warnings back on
  [[sugarSpace getAgentGrid] setOverwriteWarnings: 1];
  return self;
}

// Methods to handle the birth and death of agents
- agentBirth: (SugarAgent *)agent
{
  [agentList addLast: agent];
  return self;
}

- agentDeath: (SugarAgent *)agent
{
  [reaperQueue addLast: agent];
  if (replacement)				  // Replacement rule R (p.32)
    [self addNewRandomAgent];
  return self;
}

// remove all the agents on the reaperQueue from the agentList
// This allows us to defer the death of an agent until it's safe to
// remove it from the list.
- reapAgents
{
  id index, agent;
  
  index = [reaperQueue begin: self];
  while ((agent = [index next]))
    {
      [agentList remove: agent];
      [agent drop];
    }
  [reaperQueue removeAll];
  return self;
}

// A technical detail of scheduling
- activateIn: swarmContext
{
  [super activateIn: swarmContext];
  [modelSchedule activateIn: self];
  return [self getActivity];
}

// accessor methods
- (SugarSpace *)getSugarSpace
{
  return sugarSpace;
}

- getAgentList
{
  return agentList;
}

@end
