// Swarm library. Copyright © 1997-2000 Swarm Development Group.
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

#import <Swarm/simtoolsgui.h>  // ActionCache
#import <Swarm/GUIComposite.h>

#import <Swarm/collections.h> // List
#import <Swarm/gui.h> // ButtonPanel

@interface ActionCache: GUIComposite <ActionCache>
{
  id <List> actionCache;
  id <Schedule> destinationSchedule;
  id <ControlPanel> ctrlPanel;
  
  // widget IVar
  id <ButtonPanel> panel;
}

// Create Phase methods
- setControlPanel: (id <ControlPanel>)cp;
- createEnd;
- (id <ButtonPanel>)createProcCtrl;

// Use phase methods
- setScheduleContext: (id <Swarm>)context;
- insertAction: actionHolder;
- deliverActions;
// generic send method underlying the specific send methods
- sendActionOfType: (id <Symbol>)type toExecute: (const char *)cmd;
- sendStartAction;
- sendStopAction;
- sendStepAction;
- sendNextAction;
- sendQuitAction;
- verifyActions;

- waitForControlEvent;

// widget methods
- (id <ButtonPanel>)getPanel;
- doTkEvents;
- (void)drop;
@end

