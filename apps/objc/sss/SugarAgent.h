// Sugarscape in Swarm. Copyright © 1997 Nelson Minar
// This program is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space.h>				  // we use Space features
#import <gui.h>
#import "SugarSpace.h"

// The definition of a SugarAgent object. We inherit code from the generic
// SwarmObject, which provides memory allocation and other niceties. It
// does not provide any sort of agent behaviour, though, that's up to us.
// First, there are a lot of state variables

@interface SugarAgent: SwarmObject
{
  SugarValue currentSugar;			  // how much sugar I hold
  SugarValue metabolism;			  // how much sugar I need
  int vision;					  // how far I can see
  int age;					  // how old I am
  int deathAge;					  // how old I can get
  
  id modelSwarm;				  // my swarm
  SugarSpace *sugarSpace;			  // the sugarspace I live in
  @public
  int x, y;					  // my position
  // I won't change this myself
}

// The main behaviour of an object - do one 'time step', one action.
- step;

// The agent movement rule M.
- moveToBestOpenSpot;

// data accessor functions.
- (SugarValue)getMetabolism;
- (int)getVision;
- (int)getAge;
- (SugarValue)getCurrentSugar;
- setModelSwarm: s;
- setCurrentSugar: (SugarValue)cs;
- setMetabolism: (SugarValue)m;
- setVision: (int)v;
- setDeathAge: (int)a;

// display code so you can see the agents moving.
- drawSelfOn: (id <Raster>)r;

@end
