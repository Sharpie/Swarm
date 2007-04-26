// DrugWar model. Copyright © 2000 Swarm Development Group
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.
#import "graph/DiGraphNode.h"

@interface Agent : DiGraphNode {
  id state;         // array of bool 
  BOOL factValue, coercionValue, socialPressureValue;
  id awarenessNet;  // list of known agents
  int membership;   // 1=normal; 0=aberrant; -1=inactive

  // These ratios are numbers by which to compare how permissive
  // or resistant an agent is to the facts and social pressures 
  // in it's awareness network.   They are closely linked to the
  // boolean functions for each channel.
  float factRatio, pressureRatio;
  float incentiveRatio;
  // Map specifying which MemeFactory the agent is loyal to, and
  // keyed off the loyalty parameter
  id agentLoyalty;

  int cycle;
  int deadPriorState;
  int deadTimeStart;
  int paroleTime;

  // administrative
  id agentZone;

  // gui
  unsigned canvasPosX, canvasPosY;
}
+designProbeMap: (id) aZone;
+createBegin: (id) aZone;
-setInitialStateToFact: (BOOL) factVal
	      coercion: (BOOL) coercionVal
	socialPressure: (BOOL) pressureVal;
-setMembershipTo: (int) memShip;
-setLoyaltyTo: (id) aMemeFactory withValue: (long unsigned) aValue;
-setFactRatioTo: (float) aRatio;
-setSocialPressureRatioTo: (float) aRatio;
-setIncentiveRatio: (float) aRatio;
-toAwarenessAdd: (id) anAgent;
-setName: (const char *) aName;
-createEnd;

-act;
-setParoleTime: (unsigned) pt;
-setCanvasPositionToX: (unsigned) x Y: (unsigned) y;
-(const char *) getInstanceName;
-evaluate;
-setIncentiveRatio: (float) aRatio;
// the calculation routines are declared so as to remain generic 
// despite the optimization that can be taken advantage of when 
// inputs to one calculation use the output of another calculation
// e.g. calcCoercion = f(calcFact, calcPressure, ...)
-(BOOL) calcFact: (id) aList;
-(BOOL) getFactValue;
-(BOOL) calcPressure: (id) aList;
-(BOOL) getPressureValue;
-(BOOL) calcCoercion: (id) aList;
-(BOOL) getCoercionValue;
-(int) getMembership;
-getAwarenessNet;
@end
