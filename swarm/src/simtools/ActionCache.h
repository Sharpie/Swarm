// Swarm Library. Copyright (C) 1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections.h>
#import <activity.h>
#import <tkobjc.h>
#import <swarmobject/SwarmObject.h>

// Type Symbols
extern id <Symbol> Control, Probing, Spatial;
// Error symbols
extern id <Symbol> InvalidActionType, ActionTypeNotImplemented;

@interface ActionCache : SwarmObject {
  id <List> actionCache;
  id <Schedule> destinationSchedule;
  id ctrlPanel;

  // widget IVar
  ButtonPanel * panel;
}

// Create Phase methods
-setControlPanel: (id) cp;
-setScheduleContext: (id) context;
-createEnd;

// Use phase methods
-insertAction: (id) actionHolder;
-deliverActions;
// generic send method underlying the specific send methods
-sendActionOfType: (id <Symbol>) type toExecute: (char *) cmd;
-sendStartAction;
-sendStopAction;
-sendStepAction;
-sendNextAction;
-sendQuitAction;
-verifyActions;

// widget methods
-(ButtonPanel *) createProcCtrl ;
-(ButtonPanel *) getPanel;
-doTkEvents;


@end

