// Sugarscape in Swarm. Copyright © 1997 Nelson Minar
// This program is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "SugarAgent.h"
#import <simtools.h>
#import "ModelSwarm.h"
#import <gui.h>

@implementation SugarAgent

// One "step" for an agent. Depending on the rules in effect, this step
//   could have a lot of different meanings.
- step
{
  // The agent moves to a nearby spot.
  [self moveToBestOpenSpot];

  // Aspects of an agents lifecycle - eating, metabolism, death.
  // Eat the sugar at the current spot
  currentSugar += [sugarSpace takeSugarAtX: x Y: y];

  // Spend the sugar we need to stay alive
  currentSugar -= metabolism;

  // I'm now one year older
  age++;

  // Check if I'm dying
  if (currentSugar <= 0 || age >= deathAge)
    {
      [sugarSpace removeAgent: self];
      [modelSwarm agentDeath: self];
    }
  
  return self;
}

// This is rule "M" described in the Sugarscape book.
//   Briefly: look around for the closest, empty spot with the most food
//     that is within our vision range and move there.
//   The algorithm for this behaviour is complicated because we want
//     to make sure that if there are several spots that are equally good,
//     that we give them all an equal chance.

// A dumb macro to get the absolute value of an integer
#define intabs(a) ((a) < 0 ? -(a) : (a))

- moveToBestOpenSpot
{
  int xLook, yLook;
  SugarValue bestSugar;
  int bestDistance;
  int goodSpots;
  int goodX[16], goodY[16];			  // 4 should be adequate
  int chosenSpot, newX, newY;
  
  // prime the algorithm with out of range values
  bestSugar = -1;
  goodSpots = 0;
  bestDistance = 999999;			  // big number

  // First, look in the X direction for good spots.
  yLook = y;
  for (xLook = x - vision; xLook <= x + vision; xLook++)
    {
      // Is the spot currently empty?
      if ([sugarSpace getAgentAtX: xLook Y: yLook] == nil)
        {
          // is the spot we're looking at the best we've ever seen?
          if ([sugarSpace getSugarAtX: xLook Y: yLook] > bestSugar)
            {
              // yes, best spot ever. Forget everything else, record this
              // as the only good spot
              bestSugar = [sugarSpace getSugarAtX: xLook Y: yLook];
              bestDistance = intabs(x - xLook);
              goodSpots = 0;
              goodX[0] = xLook;
              goodY[0] = yLook;
              goodSpots++;
            }
          else if ([sugarSpace getSugarAtX: xLook Y: yLook] == bestSugar)
            {
              // No, it's only as good as anything else we've seen. Is it closer
              // than any other spot we've seen with this sugar?
              if (intabs(x - xLook) < bestDistance) {
                // Yes, forget all the rest - this is the only good spot
                bestDistance = intabs(x - xLook);
                goodSpots = 0;
                goodX[0] = xLook;
                goodY[0] = yLook;
                goodSpots++;
              }
              else if (intabs (x - xLook) == bestDistance)
                {
                  // No, this spot is as good as some other one. Just add this
                  // one on as a good spot.
                  goodX[goodSpots] = xLook;
                  goodY[goodSpots] = yLook;
                  goodSpots++;
                }
            } 
        }
    }
  
  // Now repeat the same choice in the Y axis.
  xLook = x;
  for (yLook = y - vision; yLook <= y + vision; yLook++)
    {
      if ([sugarSpace getAgentAtX: xLook Y: yLook] == nil)
        {
          if ([sugarSpace getSugarAtX: xLook Y: yLook] > bestSugar)
            {
              bestSugar = [sugarSpace getSugarAtX: xLook Y: yLook];
              bestDistance = intabs(y - yLook);
              goodSpots = 0;
              goodX[0] = xLook;
              goodY[0] = yLook;
              goodSpots++;
            }
          else if ([sugarSpace getSugarAtX: xLook Y: yLook] == bestSugar)
            {
              if (intabs(y - yLook) < bestDistance)
                {
                  bestDistance = intabs(y - yLook);
                  goodSpots = 0;
                  goodX[0] = xLook;
                  goodY[0] = yLook;
                  goodSpots++;
                }
              else if (intabs(y - yLook) == bestDistance)
                {
                  goodX[goodSpots] = xLook;
                  goodY[goodSpots] = yLook;
                  goodSpots++;
                }
            } 
        }
    }
  
  // A bit of debug printing to make sure the agents are behaving sensibly.
  // (turned off normally)
#ifdef DEBUG
  {
    int i;
    printf("Found %d good spots\n", goodSpots);
    for (i = 0; i < goodSpots; i++)
      printf("  (%d,%d) = %d\n", goodX[i], goodY[i],
	     [sugarSpace getSugarAtX: goodX[i] Y: goodY[i]]);
  }
#endif
  
  // Alright, goodX[] and goodY[] record the best spots we've found.
  // Let's figure out where to move.

  if (goodSpots == 0)				  // No spots are good
    ;						  // don't even move
  else
    {
      if (goodSpots == 1)				  // only one good spot
        chosenSpot = 0;
      else					  // pick a random spot
        chosenSpot = [uniformIntRand getIntegerWithMin: 0 withMax: goodSpots-1];
      newX = goodX[chosenSpot];			  // get the coordinate
      newY = goodY[chosenSpot];			  // and move there!
      [sugarSpace moveAgent: self toX: newX Y: newY];
    }
  return self;
}

//// The code below here is not very interesting - it has to do with
//// the technical details of managing the data in the model, not the
//// modelling itself. You can safely ignore it until you want to know
//// the details of how the program works.

// set methods for various values
- setModelSwarm: s
{
  modelSwarm = s;
  sugarSpace = [s getSugarSpace];
  return self;
}

- (SugarValue)getCurrentSugar
{
  return currentSugar;
}

- setCurrentSugar: (SugarValue)cs
{
  currentSugar = cs;
  return self;
}

- (int)getMetabolism
{
  return metabolism;
}

- setMetabolism: (int)m
{
  metabolism = m;
  return self;
}

- (int)getVision
{
  return vision;
}

- setVision: (int)v
{
  vision = v;
  return self;
}

- (int)getAge
{
  return age;
}

- setDeathAge: (int)s
{
  deathAge = s;
  return self;
}

// Graphics code - how to draw oneself (hardcoded colour value here. If
// agents have different properties, these colour should be different.)
- drawSelfOn: (id <Raster>)r
{
  [r drawPointX: x Y: y Color: 100];
  return self;
}

@end
