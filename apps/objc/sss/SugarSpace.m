// Sugarscape in Swarm. Copyright © 1997 Nelson Minar
// This program is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Here we implement the class SugarSpace.
#import "SugarSpace.h"
#import "SugarAgent.h"

#import <simtools.h> // InFile
#import <misc.h> // strdup

@implementation SugarSpace

//// The creation of the SugarSpace world.
//// Here we create a lot of objects to represent the world space.
- createEnd
{
  [super createEnd];

  // Check that we have a reasonable size.
  if (xsize <= 0 || ysize <= 0)
    [InvalidCombination raiseEvent:
                          "SugarSpace was created with an invalid size\n"];
  // and a datafile
  if (maxSugarDataFile == NULL)
    [InvalidCombination
      raiseEvent:
        "SugarSpace was created without a data file for max sugar.\n"];

  // Create the array to represent the sugar itself.
  sugar = [Discrete2d createBegin: [self getZone]];
  [sugar setSizeX: xsize Y: ysize];
  sugar = [sugar createEnd];

  [self setSugarGrowRate: 1];

#ifndef USE_HDF5  
  // Now create an object to represent the maximum sugar values
  maxSugar = [Discrete2d createBegin: [self getZone]];
  [maxSugar setSizeX: xsize Y: ysize];
  maxSugar = [maxSugar createEnd];
 
  //the space library's setDiscrete2d:toFile: method returns the max value
  globalMaxSugar = [maxSugar setDiscrete2d: maxSugar toFile: maxSugarDataFile];

 
#else
  {
    int findGlobalMaxSugar (id discrete2d)
      {
        int maxval;
        unsigned x, y;
        
        maxval = [discrete2d getValueAtX: 0 Y: 0];
        for (y = 0; y < ysize; y++)
          for (x = 0; x < xsize; x++)
            {
              int val = [discrete2d getValueAtX: x Y: y];
              
              if (val > maxval)
                maxval = val;
            }
        return maxval + 1;
      }
    
    maxSugar = [hdf5AppArchiver getObject: "maxSugarDiscrete2d"];
    if (!maxSugar)
      {
        maxSugar = [Discrete2d createBegin: [self getZone]];
        [maxSugar setSizeX: xsize Y: ysize];
        maxSugar = [maxSugar createEnd];
      }
    globalMaxSugar = findGlobalMaxSugar (maxSugar);
  }
#endif

  // Start the sugar out at maximum
  [maxSugar copyDiscrete2d: maxSugar toDiscrete2d: sugar];

  // Finally, set up the grid used to represent agent position
  agentGrid = [Grid2d createBegin: [self getZone]];
  [agentGrid setSizeX: xsize Y: ysize];
  agentGrid = [agentGrid createEnd];
  
  return self;
}


//// Update the sugar - this is the dynamics of the sugarspace world. It
//// is the rule G_alpha explained on page 26.
- updateSugar
{
  unsigned int x, y;
  // loop through the world
  for (x = 0; x < xsize; x++)
    {
      for (y = 0; y < ysize; y++)
        {
          int sugarHere = [sugar getValueAtX: x Y: y];
          int maxSugarHere = [maxSugar getValueAtX: x Y: y];
	  
          if (sugarHere + sugarGrowRate < maxSugarHere)
            sugarHere = sugarHere + sugarGrowRate;
          else
            sugarHere = maxSugarHere;
          [sugar putValue: sugarHere atX: x Y: y];
        }
    }
  
  return self;
}


//// Handle manipulating the sugar in the world.

// Read how much sugar is at a particular spot.
- (SugarValue)getSugarAtX: (int)x Y: (int)y
{
  x = [self xnorm: x];
  y = [self ynorm: y];
  return (SugarValue) [sugar getValueAtX: x Y: y];
}

// Take sugar from a spot in the world.
- (SugarValue)takeSugarAtX: (int)x Y: (int)y
{
  SugarValue sugarHere;
  
  x = [self xnorm: x];
  y = [self ynorm: y];
  sugarHere = [sugar getValueAtX: x Y: y];
  [sugar putValue: 0 atX: x Y: y];

  return sugarHere;
}


//// Code to manage agent positions in the sugar scape. Two technical points:
////   All coordinates are normalized first. This enforces wraparound world
////   No effort is made here to make sure two agents don't collide.

// Return the agent at a particular spot.
- (SugarAgent *)getAgentAtX: (int)x Y: (int)y
{
  return [agentGrid getObjectAtX: [self xnorm: x] Y: [self ynorm: y]];
}

// Add a new agent to the world. Note, this code updates the agent's
// own idea of where it is. We're violating OO encapsulation here,
// but that's ok.
- addAgent: (SugarAgent *)agent atX: (int)x Y: (int)y
{
  x = [self xnorm: x];
  y = [self ynorm: y];
  agent->x = x;
  agent->y = y;
  [agentGrid putObject: agent atX: x Y: y];

  return self;
}

// Remove an agent from the world.
- removeAgent: (SugarAgent *)agent
{
  int x, y;

  x = [self xnorm: agent->x];
  y = [self ynorm: agent->y];
  if ([self getAgentAtX: x Y: y] == agent)
    [agentGrid putObject: nil atX: x Y: y];

  return self;
}

// Move the agent.
- moveAgent: (SugarAgent *)agent toX: (int)x Y: (int)y
{
  [self removeAgent: agent];
  [self addAgent: agent atX: x Y: y];

  return self;
}

//// The code below here is not very interesting - it has to do with
//// the technical details of managing the data in the model, not the
//// modelling itself. You can safely ignore it until you want to know
//// the details of how the program works.

// handle the size of the world
- setSizeX: (int)x Y: (int)y
{
  xsize = x;
  ysize = y;

  return self;
}

- (int)getSizeX
{
  return xsize;
}

- (int)getSizeY
{
  return ysize;
}

- (SugarValue)getGlobalMaxSugar
{
  return globalMaxSugar;
}

- (id <Grid2d>)getAgentGrid
{
  return agentGrid;
}

- (id <Discrete2d>)getSugarValues
{
  return sugar;
}

// accessor for sugar growth rate
- setSugarGrowRate: (int)r
{
  sugarGrowRate = r;
  return self;
}

- (int)getSugarGrowRate
{
  return sugarGrowRate;
}

// Copy the filename into our object
- setMaxSugarDataFile: (const char *)s
{
  maxSugarDataFile = strdup (s);
  return self;
}

// normalize coordinates
- (int)xnorm: (int)x
{
  if (x < 0)					  // negative?
    return (x + xsize * 128) % xsize;		  // make positive, round
  else if (x >= (int)xsize)				  // too big? round.
    return x % xsize;
  else
    return x;					  // just right..
}

- (int)ynorm: (int)y
{
  if (y < 0)
    return (y + ysize * 128) % ysize;
  else if (y >= (int)ysize)
    return y % ysize;
  else
    return y;
}



@end
