// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Control Panel class, for use in GUIs. Controls the state of running
// activities, provides the "stop" and "go" buttons. Also handles updating
// the Tk event loop.

#import <objectbase/SwarmObject.h>

@interface ControlPanel : SwarmObject
{
  id state;					  // uses activity.h symbols
}

- createEnd;
- getState;
- setState: s;
- waitForControlEvent; // deprecated, use this message on ActionCache

- startInActivity: activityID;
- setStateRunning;
- setStateStopped;
- setStateSave;
- setStateStepping;
- setStateQuit;
- setStateNextTime;

// Deprecated Methods
- doTkEvents;  // deprecated, use this message on ActionCache
- getPanel;    // deprecated, use this on ActionCache
@end
