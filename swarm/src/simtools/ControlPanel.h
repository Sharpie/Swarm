// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Control Panel class, for use in GUIs. Controls the state of running
// activities, provides the "stop" and "go" buttons. Also handles updating
// the Tk event loop.

#import <swarmobject/SwarmObject.h>

// these tokens are the various states of the ControlPanel. "getState"
// returns one of these four values.
extern id ControlStateRunning, ControlStateStopped;
extern id ControlStateStepping, ControlStateNextTime, ControlStateQuit;

@interface ControlPanel : SwarmObject {
  id state;					  // uses activity.h symbols
}

-createEnd;
-(id) getPanel;  // deprecated, use this on ActionCache
-(id) getState;
-setState: (id) s;
-waitForControlEvent; // deprecated, use this message on ActionCache
-doTkEvents;  // deprecated, use this message on ActionCache

-startInActivity: (id) activityID;
-setStateRunning;
-setStateStopped;
-setStateStepping;
-setStateQuit;
-setStateNextTime;

@end
