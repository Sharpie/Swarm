// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Control Panel class, for use in GUIs. Controls the state of running
// activities, provides the "stop" and "go" buttons. Also handles updating
// the Tk event loop.

#import <objectbase/SwarmObject.h>

@interface ControlPanel: SwarmObject <ControlPanel>
{
  id <Symbol> state;
#ifdef USE_JAVA
  id ctlObj;
#endif
}

- createEnd;
- (id <Symbol>)getState;
- setState: (id <Symbol>)s;

- startInActivity: (id <SwarmActivity>)activityID;
- setStateRunning;
- setStateStopped;
- setStateSave;
- setStateStepping;
- setStateQuit;
- setStateNextTime;

#ifdef USE_JAVA
- setCtlObj: ctl;
#endif

@end

extern id <Symbol> ControlStateRunning, ControlStateStopped, ControlStateStepping, ControlStateNextTime, ControlStateQuit;
