// Swarm library. Copyright (C) 1997-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui.h>  // ActionCache
#import <simtoolsgui/GUIComposite.h>

#import <collections.h> // List
#import <gui.h> // ButtonPanel

@interface ActionCache: GUIComposite <ActionCache>
{
  id <List> actionCache;
  id <Schedule> destinationSchedule;
  id ctrlPanel;
  
  // widget IVar
  id <ButtonPanel> panel;
}

// Create Phase methods
- setControlPanel: cp;
- createEnd;
- (id <ButtonPanel>)createProcCtrl;

// Use phase methods
- setScheduleContext: context;
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

