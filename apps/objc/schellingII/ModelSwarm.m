#import "ModelSwarm.h"
#import <simtools.h>

@implementation ModelSwarm

// createBegin: here we set up the default simulation parameters.
+ createBegin: aZone 
{
  ModelSwarm * obj;
  id <ProbeMap> probeMap;

  obj = [super createBegin: aZone];

  // Now fill in various simulation parameters with default values.
  obj->worldSize = 50;
  obj->numRaces = 2;
  obj->neighborhood_type = strdup("vonneuman");
  obj->radius = 1;
  obj->edgeWrap = YES;
  obj->synchronous = NO;
  obj->fractionVacant = 0.2;
  obj->fractionBlue = 0.5;
  obj->fractionRed = 0.5;
  obj->blueToleranceUpper=0.50;
  obj->blueToleranceLower=0.25;
  obj->redToleranceUpper=0.50;
  obj->redToleranceLower=0.25;
  obj->otherToleranceUpper=0.50;
  obj->otherToleranceLower=0.25;
  obj->randomize = NO;
  
  // Then create an EmptyProbeMap to hold the individual
  // probes for each simulation parameter
  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  // Add in a bunch of probes one per simulation parameter
  [probeMap addProbe: [probeLibrary getProbeForVariable: "worldSize"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "numRaces"
				  inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "neighborhood_type"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "radius"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "edgeWrap"
				     inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "synchronous"
				   inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "fractionVacant"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "fractionBlue"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "fractionRed"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "blueToleranceUpper"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "blueToleranceLower"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "redToleranceUpper"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "redToleranceLower"
				    inClass: [self class]]];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "otherToleranceUpper"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "otherToleranceLower"
				    inClass: [self class]]];



  [probeMap addProbe: [probeLibrary getProbeForVariable: "randomSeed"
				    inClass: [self class]]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "saveParameters:"
			                        inClass: [self class]]
			                  setHideResult: 1]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "loadParameters:"
			                        inClass: [self class]]
			                  setHideResult: 1]];

   [probeMap addProbe: [[probeLibrary getProbeForMessage: "randomizeList"
			                        inClass: [self class]]
			                  setHideResult: 0]];



  // Now install our custom probeMap into the probeLibrary.
  [probeLibrary setProbeMap: probeMap For: [self class]];
  
  return obj;
}

- createEnd
{
  return [super createEnd];
}

// Methods associated with probe above, to 
// save or load the parameters from a file
- saveParameters: (char*) fn
{
  [ObjectSaver save: self toFileNamed: fn];
  return self;
}
  
- loadParameters: (char*) fn 
{
  [ObjectLoader load: self fromFileNamed: fn];
  return self;
}


- buildObjects 
{
  int x,y;
  int color = 99;  
  double tolerance = 0;
  int nhoodType = 0;
  int numAgents = 1;

  [super buildObjects];
  
  agentList = [List create: self];  // list for the agents
  
  if(neighborhood_type[0] == 'v') nhoodType = 1;
  else nhoodType = 2;

  // Now create a 2d array to hold their location info
  world = [SchellingWorld createBegin: self ];
  [world setSizeX: worldSize Y: worldSize];
 
  [world setNhoodRadius: radius NhoodType: nhoodType EdgeWrap: edgeWrap Sync: synchronous];
  [world setRaces: numRaces]; 
  [world setModelSwarm: self];
  world = [world createEnd];

  // Also some random number generators
  uniformDouble = [UniformDoubleDist create: self setGenerator: randomGenerator];
  uniformInteger = [UniformIntegerDist create: self setGenerator: randomGenerator];
  
 
  // Then proceed to create the people in the world
  for (x = 0; x < worldSize; x++) 
    {
      for(y = 0; y < worldSize; y++) 
	{
	  Person * person;
	  // With probability 1-fractionVacant put an
	  // agent on the square and with probability
	  // fractionBlue make an agent of that color
	  if ([self getRandomDouble] >= fractionVacant) 
	    {
	      double scaleScore = [self getRandomDouble];
	      if ( scaleScore <= fractionBlue) 
		{
		  // Here we initialize variables for a blue agent
		  color = 0;
		  tolerance = [self getRandomDoubleMin: blueToleranceLower
				    Max: blueToleranceUpper];
		}
	      else if ( scaleScore <= fractionRed + fractionBlue)
		{
		  // Here  we initialize variables for a red agent
		  color = 1;
		  tolerance = [self getRandomDoubleMin: redToleranceLower
				    Max: redToleranceUpper];
		}
	      else //not blue, not red...
		{
		  int i = 0; 
		  double otherFraction = (1 - fractionRed - fractionBlue)/(double)(numRaces-2);
		  scaleScore = scaleScore - fractionBlue - fractionRed;
		  for (i = 0; i < numRaces - 2; i++)
		    {
		      if (scaleScore <= otherFraction)
			{
			  color = 2 + i;
			  tolerance = [self getRandomDoubleMin: otherToleranceLower
					    Max: otherToleranceUpper];
			  break;
			}
		      else
			scaleScore = scaleScore - otherFraction;
		    }
		}

	      // Finally we create the actual person
	      person=[Person createBegin: self];
	      [person setWorld: world];
	      person = [person createEnd];
	      [person setX: x Y: y];
	      [person setColor: color];
	      [person setIDNumber: numAgents];
	      numAgents++;
	      [person setTolerance: tolerance];
	      [person setNhoodType: nhoodType];
	      [person setNhoodRadius: radius];
	      [person setEdgeWrap: edgeWrap];
	      // Now that the person has been created and
	      // initialized we have to add it to the world
	      // and the agentList which is used by the schedule
	      [world addObject: person atX: x Y: y];
	      [agentList addLast: person];	
	    }
	}
    }
  
  return self;
}


- getAgentList 
{
  return agentList;
}

- getWorld 
{
  return world;
}

- (int)getWorldSize 
{
  return worldSize;
}


- (double)getRandomDoubleMin: (double)min Max: (double)max 
{
  double num;

  if (min == max)
    num = max;
  else
    num = [uniformDouble getDoubleWithMin: min withMax: max];
  
  return num;
}

- (double)getRandomDouble 
{
  double num;
  
  num = [uniformDouble getDoubleWithMin: 0.0 withMax: 1.0];
  
  return num;
}

- (int)getRandomIntMin: (int)min Max: (int)max 
{
  int num;

  if(min == max)
    num = max;
  else
    num = [uniformInteger getIntegerWithMin: min withMax: max];

  return num;
}

		
- buildActions 
{
  [super buildActions];
      
  modelSchedule = [Schedule createBegin: self];
  [modelSchedule setRepeatInterval: 1];
  modelSchedule = [modelSchedule createEnd];
  
  [modelSchedule at: 0 createActionTo: self message: M(stepThroughList)];
  if (synchronous)
    [modelSchedule at: 0 createActionTo: world message: M(stepRule)];

  return self;
}


- stepThroughList
{
  id shuffler = [ListShuffler create: self];
  id <Index> index;
  id anObject;


  if (randomize == YES)
    {
      [shuffler shuffleWholeList: agentList];
    }

  index = [agentList begin: self];

  for (anObject = [index next]; [index getLoc]==Member; anObject = [index next])
    {
      [anObject step];
    }
  [shuffler drop];
  [index drop];
  return self;
}


- (BOOL)randomizeList
{
  if (randomize == YES) randomize = NO;
  else randomize = YES;
  return randomize;
}

- activateIn: swarmContext 
{
  [super activateIn: swarmContext];

  [modelSchedule activateIn: self];

  return [self getSwarmActivity];
}

@end











