#import "ModelSwarm.h"

#import "Output.h"
#import "Parameters.h"

@implementation ModelSwarm


+ createBegin: aZone 
{
  ModelSwarm * obj;

  obj = [super createBegin: aZone];

  obj->worldXSize = getInt(arguments,"worldXSize");
  obj->worldYSize = getInt(arguments,"worldYSize");
  obj->numRaces = getInt(arguments,"numRaces");
  obj->neighborhood_type = [(Parameters*)arguments getNeighborhoodType];
  obj->radius = getInt(arguments,"radius");
  obj->edgeWrap = getInt(arguments,"edgeWrap");
  obj->synchronous = NO;
  obj->fractionVacant = getDbl(arguments,"fractionVacant");
  obj->fractionBlue = getDbl(arguments,"fractionBlue");
  obj->fractionRed = getDbl(arguments,"fractionRed");
  obj->blueToleranceUpper= getDbl(arguments,"blueToleranceUpper");
  obj->blueToleranceLower=getDbl(arguments,"blueToleranceLower") ;
  obj->redToleranceUpper= getDbl(arguments,"redToleranceUpper");
  obj->redToleranceLower= getDbl(arguments,"redToleranceLower");
  obj->otherToleranceUpper=getDbl(arguments,"otherToleranceUpper");
  obj->otherToleranceLower=getDbl(arguments,"otherToleranceLower");
  obj->randomize = getInt(arguments,"randomize");
  
  return obj;
}

- createEnd
{
  fprintf(stderr,"radius %d edgeWrap %d \n", radius, edgeWrap);

  return [super createEnd];
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

  

  world = [SchellingWorld createBegin: self ];
  [world setSizeX: worldXSize Y: worldYSize];
 
  [world setNhoodRadius: radius NhoodType: nhoodType EdgeWrap: edgeWrap Sync: synchronous];
  [world setRaces: numRaces]; 
  [world setModelSwarm: self];
  world = [world createEnd];


  // Also some random number generators
  uniformDouble = [UniformDoubleDist create: self setGenerator: randomGenerator];
  uniformInteger = [UniformIntegerDist create: self setGenerator: randomGenerator];
  
 
  // Then proceed to create the people in the world
  for (x = 0; x < worldXSize; x++) 
    {
      for(y = 0; y < worldYSize; y++) 
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
  
  
  output =  [Output create: self];
  [output setAgentList: agentList];
  [output setModelSwarm: self]; //could set that via create method, but tricky.
  [output buildObjects];

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

- (int)getWorldXSize 
{
  return worldXSize;
}

- (int)getWorldYSize
{
  return worldYSize;
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
  id modelActions = [ActionGroup create: self];
  [modelActions createActionTo: output message: M(step)];

  [modelActions createActionTo: self message: M(stepThroughList)];
  if (synchronous)
    [modelActions createActionTo: world message: M(stepRule)];

    
  modelSchedule = [Schedule createBegin: self];
  [modelSchedule setRepeatInterval: 1];
  modelSchedule = [modelSchedule createEnd];
  

  [modelSchedule at: 0 createAction: modelActions];
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


- activateIn: swarmContext 
{
  [super activateIn: swarmContext];

  [modelSchedule activateIn: self];

  return [self getSwarmActivity];
}

- (void)drop
{
  [output drop];
}


- (BOOL)checkToStop
{
  if ([output checkToStop] == YES) return YES;
  return NO;
}


@end











