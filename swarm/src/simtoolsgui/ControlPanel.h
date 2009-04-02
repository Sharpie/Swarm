// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

// Control Panel class, for use in GUIs. Controls the state of running
// activities, provides the "stop" and "go" buttons. Also handles updating
// the Tk event loop.

#import <Swarm/SwarmObject.h>

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
