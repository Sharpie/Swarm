// Template application. Copyright (C) 1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools.h>
#import <tkobjc/control.h>
#import <simtools/ActionCache.h>
#import <simtools/ActionHolder.h>
#import <simtools/Archiver.h>

// Type Symbols
id <Symbol> Control, Probing, Spatial;
// Error symbols
id <Symbol> InvalidActionType, ActionTypeNotImplemented;

@implementation ActionCache

// Create Phase methods
-setControlPanel: (id) cp {
  ctrlPanel = cp;
  return self;
}

-setControlPanelGeometryRecordName : (const char *)theName
{
  controlPanelGeometryRecordName = theName;
  return self;
}

-setScheduleContext: (id) context {
  [destinationSchedule activateIn: context];
  return self;
}

-createEnd {
  [super createEnd];

  //Create the data structure
  actionCache = [List createBegin: [self getZone]];
  actionCache = [actionCache createEnd];

  //Create the group to be activated in the swarm
  destinationSchedule = [Schedule createBegin: [self getZone]];
  //[destinationSchedule setRepeatInterval: 1];
  [destinationSchedule setAutoDrop: YES];
  //  [destinationSchedule setRepeatInterval: 1];
  destinationSchedule = [destinationSchedule createEnd];
  [destinationSchedule at: 0 createActionTo: self message: M(deliverActions)];
  //[destinationSchedule at: 1
  //	       createActionCall: xprint
  //	       : destinationSchedule];


  // Create the panel widget that will send the mouse control events to me.
  panel = [self createProcCtrl];

  //
  // Symbols
  //

  // various types of actions
  defsymbol(Control);
  defsymbol(Probing);
  defsymbol(Spatial);
  // various errors for actioncache processing
  deferror(InvalidActionType, "Action Name not recognized.\n");
  deferror(ActionTypeNotImplemented, "Action type not yet implemented.\n");

  return self;
}

// Use phase methods
-insertAction: (id) actionHolder
{
  id <Symbol> actionType;
  char * actionName;

  actionType = [actionHolder getType];
  if (actionType == Control) {
    actionName = [actionHolder getActionName];
    [actionHolder setActionTarget: ctrlPanel];
    if (strcmp(actionName, "Start") == 0)
      [actionHolder setSelector: M(setStateRunning)];
    else if (strcmp(actionName, "Step") == 0)
      [actionHolder setSelector: M(setStateStepping)];
    else if (strcmp(actionName, "Next") == 0)
      [actionHolder setSelector: M(setStateStepping)];
    else if (strcmp(actionName, "Stop") == 0)
      [actionHolder setSelector: M(setStateStopped)];
    else if (strcmp(actionName, "Save") == 0)
      [actionHolder setSelector: M(setStateStoppedAndSave)];
    else if (strcmp(actionName, "Quit") == 0) {
      [actionHolder setSelector: M(setStateQuit)];
    }
    else
      [InvalidActionType
	raiseEvent: "Control Action Name: [%s] not recognized in insertAction",
	actionName];
    [actionCache addLast: actionHolder];
  }
  else if (actionType == Probing) {
    // target should already be set to the particular probe?
    // [actionCache addLast: actionHolder];
    [ActionTypeNotImplemented raiseEvent];
  }
  else if (actionType == Spatial) {
    // [actionCache addLast: actionHolder];
    [ActionTypeNotImplemented raiseEvent];
  }
  else
    [InvalidActionType raiseEvent: "The ActionType Symbol "
		       "embedded in action 0x%0p was not found.\n", 
		       actionHolder];
  return self;
}

-deliverActions
{
  id <Symbol> actionType;
  char * actionName;
  id <Index> cacheIndex;
  id actionHolder;
  
  cacheIndex = [actionCache begin: scratchZone];
  while ((actionHolder = [cacheIndex next]) != nil) {
    actionType = [actionHolder getType];
    if (actionType == Control) {
      actionName = [actionHolder getActionName];
      //[actionHolder setActionTarget: schedControl];
      // if "Quit" then schedule a quit to control panel
      // if "Stop" then schedule a "stop" to the activitycontrol
      if ((strcmp(actionName, "Quit") == 0) ||
	  (strcmp(actionName, "Stop") == 0) ||
          (strcmp(actionName, "Save") == 0)) {
	[destinationSchedule at: 
			       getCurrentTime()+1
			     createActionTo: [actionHolder getActionTarget] 
			     message: [actionHolder getSelector]];
	
      }
      // if "Start" send a message directly to activitycontroller
      // if "Step" send a message directly to activitycontroller
      else if ((strcmp(actionName, "Step") == 0) ||
	       (strcmp(actionName, "Start") == 0)) {
	[[actionHolder getActionTarget] perform: [actionHolder getSelector]];
      }
      else
	[InvalidActionType
	  raiseEvent: "Control Action Name: [%s] not recognized in deliverActions",
	  actionName];
    }
    [cacheIndex remove];
    [actionHolder drop];
    
  }
  [cacheIndex drop];

  // reschedule myself for next cycle
  [destinationSchedule at: getCurrentTime()+1 
		       createActionTo: self 
		       message: M(deliverActions)];

  return self;
}


// I suffered some code bloat because I didn't have time to
// figure out how to pass arguments via the "simctl" Tk command.
// Since this work is not really for Tk, but for Java, I'm not
// going to worry about it. -- gepr
-sendActionOfType: (id <Symbol>) type toExecute: (char *) cmd {
  id anAction;

  // if in waitForControlEvent, then reset the control panel
  // state and insert the start action onto the cache
  // when control finally gets back to controlpanel, it will
  // fall out of the busy wait loop and continue at the point
  // from which waitForControlEvnt was called.
  if (([ctrlPanel getState] == ControlStateStopped) && 
      (strcmp(cmd, "Stop") != 0))
    [ctrlPanel setState: ControlStateRunning];

  // create a 'cmd' action
  anAction = [[ActionHolder createBegin: [self getZone]] createEnd];
  [anAction setActionName: cmd];
  [anAction setType: type];
  
  // insert the action
  return [self insertAction: anAction];
}

-sendStartAction {
  return [self sendActionOfType: Control toExecute: "Start"];
}

-sendStopAction {
  return [self sendActionOfType: Control toExecute: "Stop"];
}

-sendStepAction {
  return [self sendActionOfType: Control toExecute: "Step"];
}

-sendNextAction {
  return [self sendActionOfType: Control toExecute: "Step"];
}

-sendSaveAction
{
  return [self sendActionOfType: Control toExecute: "Save"];
}

-sendQuitAction {
  return [self sendActionOfType: Control toExecute: "Quit"];
}

// perform any reasoning over the cache in this method
-verifyActions {
  return self;
}


// Widget methods
-(ButtonPanel * ) createProcCtrl
{
  ButtonPanel * panelWidget;

  // Make a command for ourselves
  registerCommand (self, "simctl");
  
  // These methods are bound to the Tk buttons. They get invoked directly
  // by the Tk interpreter (via tclobjc). ObserverSwarm uses these states
  // to control simulation execution.

  // make a widget for us, too. Bind buttons to messages to ourself.
  panelWidget = [ButtonPanel createBegin: [self getZone]];
  [panelWidget setWindowGeometryRecordName: controlPanelGeometryRecordName];
  [panelWidget setTargetName: "simctl"];
  panelWidget = [panelWidget createEnd];
  [panelWidget addButtonName: "Start" actionName: "sendStartAction"];
  [panelWidget addButtonName: "Stop" actionName: "sendStopAction"];
  [panelWidget addButtonName: "Step" actionName: "sendStepAction"];
  [panelWidget addButtonName: "Next" actionName: "sendNextAction"];
  [panelWidget addButtonName: "Save" actionName: "sendSaveAction"];
  [panelWidget addButtonName: "Quit" actionName: "sendQuitAction"];
  [panelWidget setWindowTitle: "ProcCtrl"];

  return panelWidget;
}

-(ButtonPanel *) getPanel {
  return panel;
}

// Do the Tk Events.
// First, let Tk process all the events around.
// Second, if we're stopped and not ready to quit, keep processing events
//   until one of those conditions changes. (This lets the user press the
//   Go or Quit buttons to quit.)
// Finally, return a status that tells whether we need to quit.
-doTkEvents
{
  // do all events pending, but don't block.
  while (doOneEventAsync ()) {}
  return self;
}

@end

