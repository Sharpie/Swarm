// DrugWar model. Copyright © 2000 Swarm Development Group
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.
#import <collections.h>
#import <objectbase.h>
#import "graph/graph.h"
#import "Agent.h"
#include <misc.h> // exit

@implementation Agent

+designProbeMap: (id) aZone {
  id <ProbeMap> probeMap;

  probeMap = [CustomProbeMap create: aZone forClass: [self class]
			     withIdentifiers: "membership",
			     "factValue", "coercionValue", 
			     "socialPressureValue",":",
			     "setMembershipTo:", NULL];
  [probeLibrary setProbeMap: probeMap For: [self class]];
  return self;
}
//creating

+createBegin: (id) aZone {
  Agent * newMe;
  newMe = [super createBegin: aZone];
  newMe->agentZone = [Zone create: aZone];
  newMe->cycle = 0;
  newMe->deadTimeStart = -1;
  newMe->deadPriorState = -2;
  newMe->paroleTime = 5; // cycles

  return newMe;
}

-setInitialStateToFact: (BOOL) factVal 
	      coercion: (BOOL) coercionVal
              socialPressure: (BOOL) pressureVal
{
  id <Index> statNdx;

  state = [Array create: agentZone setCount: 3];
  statNdx = [state begin: agentZone];
  factValue = factVal;
  [statNdx put: (void *)(int) factVal];
  [statNdx next];
  coercionValue = coercionVal;
  [statNdx put: (void *)(int)coercionVal];
  [statNdx next];
  socialPressureValue = pressureVal;
  [statNdx put: (void *)(int)pressureVal];

  [statNdx drop];

  return state;
}

-setMembershipTo: (int) memShip {

  if (canvas) {
    switch (memShip) {
    case -1:
      [nodeItem setColor: "black"];
      deadTimeStart = cycle;
      deadPriorState = membership;
      break;
    case 0:
      [nodeItem setColor: "red"];
      deadTimeStart = -1;
      break;
    case 1:
      [nodeItem setColor: "blue"];
      deadTimeStart = -1;
      break;
    default:
      (void) fprintf(stderr, "Membership value invalid!\n");
    }
    GUI_UPDATE_IDLE_TASKS();
  }

  membership = memShip;

  return self;
}

// the loyalty list creation/addition method.  This is used
// to design types of agents.  Right now the types are really
// based on having only two MemeFactories.
- setLoyaltyTo: (id)aMemeFactory withValue: (long unsigned)aValue
{
  if (agentLoyalty == nil)
    {
      agentLoyalty = [Map createBegin: agentZone];
      [agentLoyalty createEnd];
    }
  [agentLoyalty at: (void *) aValue insert: aMemeFactory];
  return self;
}

-setFactRatioTo: (float) aRatio {
  if (aRatio >= 1.0) {
    fprintf(stderr, "FactRatio must be less than 1.0.\n");
    exit(1);
  }

  factRatio = aRatio;
  return self;
}

-setSocialPressureRatioTo: (float) aRatio {
  if (aRatio >= 1.0) {
    fprintf(stderr, "PressureRatio must be less than 1.0.\n");
    exit(1);
  }

  pressureRatio = aRatio;
  return self;
}

-toAwarenessAdd: (id) anAgent {
  if (awarenessNet == nil) {
    awarenessNet = [Set createBegin: agentZone];
    //[awarenessNet setDupOption: KeepAllDups];
    //[awarenessNet setKeyFunction: (id (*)(id))aFunction]; // keyed by name
    awarenessNet = [awarenessNet createEnd];
  }
  [awarenessNet add: anAgent];
  return self;
}

- setName: (const char *)aName
{
  label = aName;
  return self;
}

-createEnd {  
  id loyaltyIndex;
  int firstValue, lastValue;

  [super createEnd];

  // Agent Types calculation/check
  if (agentLoyalty != nil) {
    loyaltyIndex = [agentLoyalty begin: agentZone];
    // the map is supposed to be sorted, so the ratio of 
    //   the highest loyalty to the lowest loyalty can 
    //   be calculated thusly:
    [loyaltyIndex setLoc: Start]; 
    firstValue = (long) [loyaltyIndex getKey];
    [loyaltyIndex setLoc: End]; 
    lastValue = (long) [loyaltyIndex getKey];
    [loyaltyIndex drop];

    if ((firstValue >= 0L) && (lastValue >= 0L)) {
      factRatio = (lastValue/firstValue) * 0.50;
      pressureRatio = factRatio;
    }
    else fprintf(stderr, "Error in agentLoyalty list.\n");
  }

  // State Array check
  if (state == nil) fprintf(stderr, "Error: CreateEnd called before"
			    " State Initialized.\n");

  // Misc Init
  incentiveRatio = 0.0;

  nodeItem = [nodeItem setX: canvasPosX Y: canvasPosY];

  return self;
}
//using
-act {

  // get out of jail (or the cartel's dungeon)
  if ( (membership == -1) 
       && ((cycle - deadTimeStart) >= paroleTime) ) {
    [self setMembershipTo: deadPriorState];
  }
  cycle++;

  return self;
}

- setParoleTime: (unsigned) pt {
  paroleTime = pt;
  return self;
}

// Call this after createEnd so that we override Manor's default
-setCanvasPositionToX: (unsigned) x Y: (unsigned) y {

  [[self getNodeItem]
    moveX: (x - [[self getNodeItem] getX]) 
    Y: (y - [[self getNodeItem] getY])];

  canvasPosX = x;
  canvasPosY = y;

  return self;
}

-(const char *) getInstanceName {
  return label;
}

- evaluate
{
  if (membership != -1) {
    factValue = [self calcFact: awarenessNet];
    socialPressureValue = [self calcPressure: awarenessNet];
    coercionValue = [self calcCoercion: awarenessNet];

    /* The following is the boolean function derived straight from
     *  the truth table:
     *         f(0,0,1) = 1
     *         f(1,0,1) = 1
     *         f(1,1,1) = 1
     *         f(0,1,1) = 0
     *         f(0,0,0) = 0
     *         f(0,1,0) = 0
     *         f(1,1,0) = 1
     *         f(1,0,0) = 1
     */
    //membership = (int) (factValue || (!coercionValue && socialPressureValue));

    /* The following is the algebraic function derived from the
     * following  "three-linear" function:
     *  X = p0 + p1*x1 + p2*x2 + p3*x3 + p4*x1*x2 + p5*x1*x3 +
     *                                   p6*x2*x3 + p7*x1*x2*x3
     *
     * and which should be recognizable as a polynomial in three dimensions
     * with all the cross terms [i.e. the result of the multiplication of
     *   (x + k1)*(y+k2)*(z+k3) ]
     *
     * The algebraic expression [X= x1 + (1 - x1 -x2 + x1*x2)*x3] is
     * obtained by substituting the arguments to f(.) in the truth
     * table above, getting a system of 8 equations in pi and solving
     * that system of equations, then re-substituting those values of
     * the pi into the polynomial.
     * (Ref: Lefebvre, "Algebra of Consciousness", 1980, 1982)
     */

    [self setMembershipTo: (int) (factValue + 
				  (1 - factValue - coercionValue + 
				   factValue * coercionValue) * 
				  socialPressureValue)];

    [state atOffset: 0 put: (void *)(int) factValue];
    [state atOffset: 0 put: (void *)(int) coercionValue];
    [state atOffset: 0 put: (void *)(int) socialPressureValue];
  }
  return self;
}  

-setIncentiveRatio: (float) aRatio {
  incentiveRatio = aRatio;
  return self;
}

-(BOOL) calcFact: (id) aList {
  id awareNdx, friend;
  unsigned total=0;
  BOOL coeff;

  awareNdx = [aList begin: agentZone];
  while (([awareNdx getLoc] != End) &&
	 ((friend = [awareNdx next]) != nil)) {
    // total is incremented if friend is inactive
    total += (int) ([friend getMembership] == -1L);
  }
  [awareNdx drop];

  // If more than factRatio of my friends are inactive
  //   offset by the current incentiveRatio being offerred by the
  //   other team
  coeff = (total > ((factRatio * [aList getCount]) +
		    incentiveRatio * ([aList getCount] - 
				      (factRatio * [aList getCount]))));

  // new factValue = percentage of friends busted
  return (coeff); 
}

-(BOOL) getFactValue {
  return factValue;
}

- (BOOL) calcPressure: aList
{
  id awareNdx, friend;
  unsigned total = 0;
  BOOL coeff;

  awareNdx = [aList begin: agentZone];
  while (([awareNdx getLoc] != End) &&
	 ((friend = [awareNdx next]) != nil)) {
    // total is incremented if they recognize pressure
    total += (int) [friend getPressureValue];
  }
  [awareNdx drop];

  // If more than pressureRatio of my friends recognize pressure
  coeff = (total > (pressureRatio * [aList getCount]));

  // new pressureValue = if most friends recognize pressure
  return (coeff); 
}

-(BOOL) getPressureValue {
  return socialPressureValue;
}

-(BOOL) calcCoercion: (id) aList {
  // new coercion = (newfactval + newsocialpress)
  return (factValue || socialPressureValue);
}

-(BOOL) getCoercionValue {
  return coercionValue;
}

-(int) getMembership {
  return membership;
}

-getAwarenessNet {
  return awarenessNet;
}
@end
