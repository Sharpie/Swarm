// Swarm library. Copyright (C) 1997-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools.h>
#import <simtools/ActionCache.h>
#import <simtools/ActionHolder.h>
#import <simtools/Archiver.h>
#import <gui.h>

#ifdef USE_JAVA
#import <simtools/ControlPanel.h>
#import <awtobjc/JavaControlPanel.h>
#import <awtobjc/JavaInput.h>
#endif

// Type Symbols
id <Symbol> Control, Probing, Spatial;
// Error symbols
id <Symbol> InvalidActionType, ActionTypeNotImplemented;

@implementation ActionCache

// Create Phase methods

- setControlPanel: cp
{
  ctrlPanel = cp;
  return self;
}

- createEnd
{
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
  
#ifndef USE_JAVA
  // Create the panel widget that will send the mouse control events to me.
  panel = [self createProcCtrl];
#else
  {
    id buttons;

    // make a control button GUI object
    buttons = [JavaControlPanel create: [self getZone]];
    // the Swarm controller needs to know about it
    [ctrlPanel setCtlObj: buttons];
    
    // and, the control panel needs to know about the action cache
    // so that it can queue up commands for it
    [buttons setActionCache: self];
  }
#endif
#ifdef USE_JAVA
  // create an object to represent the global input queue
  inputQueue = [[[JavaInput create: [self getZone]] init] createEnd]; 
#endif
  
  //
  // Symbols
  //

  // various types of actions
  defsymbol (Control);
  defsymbol (Probing);
  defsymbol (Spatial);
  // various errors for actioncache processing
  deferror (InvalidActionType, "Action Name not recognized.\n");
  deferror (ActionTypeNotImplemented, "Action type not yet implemented.\n");

  return self;
}

// Use phase methods

- setScheduleContext: context
{
  [destinationSchedule activateIn: context];
  return self;
}

- insertAction: actionHolder
{
  id <Symbol> actionType;
  const char *actionName;
  
  actionType = [actionHolder getType];
  if (actionType == Control)
    {
      actionName = [actionHolder getActionName];
      [actionHolder setActionTarget: ctrlPanel];
      if (strcmp (actionName, "Start") == 0)
        [actionHolder setSelector: M(setStateRunning)];
      else if (strcmp (actionName, "Step") == 0)
        [actionHolder setSelector: M(setStateStepping)];
      else if (strcmp (actionName, "Next") == 0)
        [actionHolder setSelector: M(setStateStepping)];
      else if (strcmp (actionName, "Stop") == 0)
        [actionHolder setSelector: M(setStateStopped)];
      else if (strcmp (actionName, "Save") == 0)
        [actionHolder setSelector: M(setStateSave)];
      else if (strcmp (actionName, "Quit") == 0)
        [actionHolder setSelector: M(setStateQuit)];
      else
        [InvalidActionType
          raiseEvent: 
            "Control Action Name: [%s] not recognized in insertAction",
          actionName];
      [actionCache addLast: actionHolder];
    }
  else if (actionType == Probing)
    // target should already be set to the particular probe?
    // [actionCache addLast: actionHolder];
    [ActionTypeNotImplemented raiseEvent];
  else if (actionType == Spatial)
    // [actionCache addLast: actionHolder];
    [ActionTypeNotImplemented raiseEvent];
  else
    [InvalidActionType raiseEvent: "The ActionType Symbol "
		       "embedded in action 0x%0p was not found.\n", 
		       actionHolder];
  return self;
}

- deliverActions
{
  id <Symbol> actionType;
  const char *actionName;
  id <Index> cacheIndex;
  id actionHolder;
  
  cacheIndex = [actionCache begin: scratchZone];
  while ((actionHolder = [cacheIndex next]) != nil)
    {
      actionType = [actionHolder getType];
      if (actionType == Control)
        {
          actionName = [actionHolder getActionName];
          //[actionHolder setActionTarget: schedControl];
          // if "Stop" then schedule a "stop" to the activitycontrol
          if (strcmp (actionName, "Stop") == 0)
            {
              if (_activity_current)
                [destinationSchedule 
                  at:
                    getCurrentTime () + 1
                  createActionTo: [actionHolder getActionTarget]
                  message: [actionHolder getSelector]];
              else
                // Assume we are being called directly, and do it.
                [[actionHolder getActionTarget]
                  perform: [actionHolder getSelector]];
            }
          // if "Save", "Start", "Step", or "Quit" send a message directly
          // to activitycontroller
          else if (strcmp (actionName, "Step") == 0
                   || (strcmp (actionName, "Start") == 0)
                   // Save is here because otherwise archiving won't run
                   // until execution resumes.  I (mgd) think this is bad: if
                   // you push on a button it should do something.  
                   // Also, as long as Quit is immediate, Save needs
                   // to be immediate as well or it could get ignored.
                   || strcmp (actionName, "Save") == 0
                   // Quit is here because otherwise there will be
                   // a timestep the user may observe before the application
                   // actually exits.  This probably ought to become a
                   // scheduled action for when there are non-GUI 
                   // ControlPanels being frobbed by other Swarms.
                   || strcmp (actionName, "Quit") == 0)
            [[actionHolder getActionTarget]
              perform: [actionHolder getSelector]];
          else
            [InvalidActionType
              raiseEvent:
                "Control Action Name: [%s] not recognized in deliverActions",
              actionName];
      }
      [cacheIndex remove];
      [actionHolder drop];
      
    }
  [cacheIndex drop];

  // Reschedule myself for next cycle, otherwise do nothing, assuming
  // we are being called directly.
  if (_activity_current)
    [destinationSchedule at: getCurrentTime () + 1 
                         createActionTo: self 
                         message: M(deliverActions)];
  
  return self;
}

// I suffered some code bloat because I didn't have time to
// figure out how to pass arguments via the "simctl" Tk command.
// Since this work is not really for Tk, but for Java, I'm not
// going to worry about it. -- gepr
- sendActionOfType: (id <Symbol>) type toExecute: (const char *)cmd
{
  id anAction;
  
  // if in waitForControlEvent, then reset the control panel
  // state and insert the start action onto the cache
  // when control finally gets back to controlpanel, it will
  // fall out of the busy wait loop and continue at the point
  // from which waitForControlEvnt was called.
  if (([ctrlPanel getState] == ControlStateStopped)
      && !(strcmp (cmd, "Stop") == 0
           || strcmp (cmd, "Save") == 0))
    [ctrlPanel setState: ControlStateRunning];
  
  // create a 'cmd' action
  anAction = [[ActionHolder createBegin: [self getZone]] createEnd];
  [anAction setActionName: cmd];
  [anAction setType: type];
  
  // insert the action
  return [self insertAction: anAction];
}

- sendStartAction
{
  return [self sendActionOfType: Control toExecute: "Start"];
}

- sendStopAction
{
  return [self sendActionOfType: Control toExecute: "Stop"];
}

- sendStepAction
{
  return [self sendActionOfType: Control toExecute: "Step"];
}

- sendNextAction
{
  return [self sendActionOfType: Control toExecute: "Step"];
}

- sendSaveAction
{
  return [self sendActionOfType: Control toExecute: "Save"];
}

- sendQuitAction
{
  return [self sendActionOfType: Control toExecute: "Quit"];
}

// perform any reasoning over the cache in this method
- verifyActions
{
  return self;
}

// Widget methods
- (id <ButtonPanel>)createProcCtrl
{
  id <ButtonPanel> panelWidget;
  
  // These methods are bound to the Tk buttons. They get invoked directly
  // by the Tk interpreter (via tclobjc). ObserverSwarm uses these states
  // to control simulation execution.

  // make a widget for us, too. Bind buttons to messages to ourself.
  panelWidget = [ButtonPanel createBegin: [self getZone]];
  SET_WINDOW_GEOMETRY_RECORD_NAME (panelWidget);
  [panelWidget setButtonTarget: self];
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

- (id <ButtonPanel>)getPanel
{
  return panel;
}

// What does a "waitForControlEvent" mean to a generic Swarm?  It's
// obvious what it means to a guiSwarm, and since we only instantiate
// a controlpanel with guiswarms, currently, we should provide some
// kind of passthrough from the gui to here.  I'm going to make this
// method block until the ControlPanel state changes.... via a call
// on a controlpanel method from some other object.  So, if the 
// gui isn't on it's own thread, nothing will happen here because 
// on a serial process, this busy wait loop LOCKS everything up.
//   Note also that this is how we do all the probe manipulations
// for the set up of runs.  The probe actions won't get done unless
// some polling action occurs here.  I.e. we don't want the schedule
// to move forward without doing the probe actions first.
// So, for the Tk version we still need a Tk poller; but in the Java
// version we don't as long as we have some mechanism other than the
// schedule for changing ObjC object state in response to Java
// events. 
- waitForControlEvent
{
  [ctrlPanel setState: ControlStateStopped];
  while ([ctrlPanel getState] == ControlStateStopped)
    GUI_EVENT_SYNC ();
  return nil;
}

// Do the Tk Events.
// First, let Tk process all the events around.
// Second, if we're stopped and not ready to quit, keep processing events
//   until one of those conditions changes. (This lets the user press the
//   Go or Quit buttons to quit.)
// Finally, return a status that tells whether we need to quit.
- doTkEvents
{
#ifndef USE_JAVA
  // do all events pending, but don't block.
  while (GUI_EVENT_ASYNC ()) {}
#else
  fprintf (stderr,"doTkEvents / checkEvents\n");
  [inputQueue checkEvents];
#endif

  return self;
}

@end
