// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Control Panel class, for use in GUIs. Controls the state of running
// activities, provides the "stop" and "go" buttons. Also handles updating
// the Tk event loop.

#import <swarmobject/SwarmObject.h>
#import <tkobjc.h>

// these tokens are the various states of the ControlPanel. "getState"
// returns one of these four values.
extern id ControlStateRunning, ControlStateStopped;
extern id ControlStateStepping, ControlStateNextTime, ControlStateQuit;

@interface ControlPanel : SwarmObject {
  ButtonPanel * panel;
  id state;					  // uses activity.h symbols
}

-createEnd;
-(ButtonPanel *) getPanel;
-(id) getState;
-setState: (id) s;
-waitForControlEvent;
-doTkEvents;

-setStateRunning;
-setStateStopped;
-setStateStepping;
-setStateQuit;
-setStateNextTime;

@end
