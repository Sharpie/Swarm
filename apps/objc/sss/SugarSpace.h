// Sugarscape in Swarm. Copyright © 1997 Nelson Minar
// This program is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// The SugarSpace is the object that represents the environment of the model.
// Its main component is the amount of sugar in the world.
// It also enforces dynamics on the sugar space (growing new sugar) and
// keeps track of where all the agents are.

#import <objectbase/SwarmObject.h>
#import <space.h>

@class SugarAgent;

// A SugarValue is a measurement of how much sugar is at the world.
// We define a new type here, but it's basically just an integer
typedef int SugarValue;

// Here is the SugarSpace object itself. It inherits very little behaviour,
// but contains various objects to store properties of the world.
@interface SugarSpace: SwarmObject
{
  unsigned int xsize, ysize;				  // grid size
  
  id <Discrete2d> sugar;			  // sugar at each spot
  int sugarGrowRate;				  // alpha, from the book
  
  const char *maxSugarDataFile;			  // filename for max sugar
  id <Discrete2d> maxSugar;			  // max sugar at each spot
  SugarValue globalMaxSugar;			  // absolute maximum for space
  
  id <Grid2d> agentGrid;			  // agent positions
}

// Use this method at object creation - set the size of the world.
- setSizeX: (int)x Y: (int)y;
- (int)getSizeX;
- (int)getSizeY;
// Also set the maximum sugar datafile
- setMaxSugarDataFile: (const char *)s;

// Dynamics; add more sugar to the world. This is rule G_alpha of the book.
// there are also methods here to change the grow rate.
- updateSugar;
- setSugarGrowRate: (int)r;
- (int)getSugarGrowRate;

// This code handles the main property of a SugarSpace: how much sugar
// there is at each spot in the world, and taking that sugar.
- (SugarValue)getSugarAtX: (int)x Y: (int)y;
- (SugarValue)takeSugarAtX: (int)x Y: (int)y;
- (id <Discrete2d>)getSugarValues;

// Read out the maximum value of sugar from the space
- (SugarValue)getGlobalMaxSugar;

// This code handles the position of all the agents in the sugarspace.
// Look for agents in the space, add, move, and remove them.
- (SugarAgent *)getAgentAtX: (int)x Y: (int)y;
- addAgent: (SugarAgent *)agent atX: (int)x Y: (int)y;
- removeAgent: (SugarAgent *)agent;
- moveAgent: (SugarAgent *)agent toX: (int)x Y: (int)y;

// Get the array of all the agents in the space.
- (id <Grid2d>)getAgentGrid;

// coordinate normalization. This handles coordinates out of range by
// wrapping them around.
- (int)xnorm: (int)x;
- (int)ynorm: (int)y;

@end
