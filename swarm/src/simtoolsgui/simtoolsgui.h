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

//S: GUI-related features for simulation.

#import <Swarm/objectbase.h>
#import <Swarm/activity.h> // Activity
#import <Swarm/gui.h> // Frame
#include <Swarm/externvar.h>

@protocol WindowGeometryRecordName
//S: Protocol for archiving window geometry.

//D: Classes that allow for window geometry archiving must conform
//D: this protocol.

CREATING
//M: This method is used to give an
//M: instance ProbeDisplay a name, which will used by the Archiver
//M: when recording its geometry information.
- setWindowGeometryRecordName: (const char *)windowGeometryRecordName;

- setSaveSizeFlag: (BOOL)saveSizeFlag;

//#: This macro uses the instance name of theWidget to set its name
//#: in the window geometry record.
#define SET_WINDOW_GEOMETRY_RECORD_NAME(theWidget) \
  [theWidget setWindowGeometryRecordName: #theWidget]
@end

@protocol CompositeWindowGeometryRecordName <WindowGeometryRecordName>
//S: Protocol for archiving objects with several GUI components.

//D: Protocol for assigning archiving names to components of an object
//D: with several GUI components.
CREATING
//M: Update the list of components, and compute the derived archiving name.
- setWindowGeometryRecordNameForComponent: (const char *)componentName
                                   widget: widget;

#define SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME_FOR(obj, theWidget) \
  [(obj) setWindowGeometryRecordNameForComponent: #theWidget widget: theWidget]

#define SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME(theWidget) \
  SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME_FOR (self,theWidget)

@end


@protocol ControlPanel <SwarmObject, RETURNABLE>
//S: Class to control the top level SwarmProcess

//D: ControlPanel keeps track of the users requests to run, stop, quit, or
//D: time step the simulation. It cooperates with the GUISwarm to control
//D: the execution of activities in Swarm.
CREATING

USING
//M: Get the current button state of the controlpanel.  Is one of
//M: ControlStateRunning, ControlStateStopped, ControlStateStepping,
//M: ControlStateNextTime, or ControlStateQuit.
- (id <Symbol>)getState;

- setState: (id <Symbol>)s;

- startInActivity: (id <SwarmActivity>)activityID;

//M: Sets the state to `running'.
- setStateRunning;

//M: The -setStateStopped message is particularly useful since it will
//M: cause the simulation to stop until the user interactively
//M: sets it back in motion (in other words, this method is useful
//M: in generating a software-triggered pause).
- setStateStopped;

//M: Stop the running activity, and then set state to `ControlStateStepping'.
- setStateStepping;

//M: Terminate activities, and set state to `ControlStateQuit'.
- setStateQuit;

//M: Stop the running activity, and then set state to `ControlStateNextTime'.
- setStateNextTime;

//M: Saves the objects that are registered for archiving.
- setStateSave;

@end

@protocol ActionCache <CompositeWindowGeometryRecordName, SwarmObject, RETURNABLE>
//S: A class to manage threads and Swarms.

//D: A class that provides a smart bag into which actions can be
//D: thrown by other threads and Swarms intended for insertion on
//D: it's Swarm's schedule.
CREATING
- setControlPanel: (id <ControlPanel>)cp;
- createProcCtrl;

USING
- setScheduleContext: (id <Swarm>)context;
- insertAction: actionHolder;
- deliverActions;
- sendActionOfType: (id <Symbol>)type toExecute: (const char *)cmd;
- sendStartAction;
- sendStopAction;
- sendStepAction;
- sendNextAction;
- sendQuitAction;
- verifyActions;

- getPanel;

//M: A message that processes any input or output events of the Tk
//M: toolkit. Scheduling -doTkEvents ensures Tk keeps the user interface
//M: up-to-date.  Without scheduling it as part of the GUI code, the Tk
//M: events would just queue up and never get processed, resulting in a
//M: static, unresponsive user interface.
- doTkEvents;  // should change to pollGUI or something
- waitForControlEvent;
@end

//G: Type Symbols for ActionCache
externvar id <Symbol> Control, Probing, Spatial;

//G: Error Symbols for ActionCache
externvar id <Symbol> InvalidActionType, ActionTypeNotImplemented;


@protocol CommonProbeDisplay <SwarmObject, WindowGeometryRecordName>
//S: A protocol underlying ProbeDisplay and CompleteProbeDisplay
//D: This protocol provides the common interface to all kinds of ProbeDisplays.

USING
//M: This method maintains consistency between the values of the
//M: probedObject's variables and the values which are displayed in
//M: the ProbeDisplay. Ideally, this method should be called every
//M: time the object is modified by the simulation. In practice, the
//M: user schedules an update on the probeDisplayManager which in
//M: turn communicates to all the active ProbeDisplays in the
//M: system.
- (void)update;

- (BOOL)getMarkedForDropFlag;
- getTopLevel;
@end

@protocol SingleProbeDisplay <CommonProbeDisplay>
//S: An abstract protocol underlying single-object probe displays.

//D: This protocol is common to CompleteProbeDisplay and ProbeDisplay.
CREATING
//M: This method must be called.
- setProbedObject: anObject;

USING
//M: Gets the probed object.
- getProbedObject;
@end

@protocol SimpleProbeDisplay <SingleProbeDisplay, RETURNABLE>
//S: 

//D: 
CREATING
//M: 
- setProbeMap: (id <ProbeMap>)probeMap;
USING
@end

@protocol ProbeDisplay <SingleProbeDisplay, CREATABLE>
//S: A class to display ProbeMaps

//D: A class which generates a GUI to a ProbeMap of probes applied to a 
//D: given target object.

CREATING
//M: This is an optional create phase method - if no probeMap is specified
//M: the ProbeDisplay will ask the probedObject for a ProbeMap using the
//M: getProbeMap method described below... The default behaviour of this
//M: method will be to return the probeLibrary's copy of the probeMap for
//M: the class of the target object.
- setProbeMap: (id <ProbeMap>)probeMap;

USING
@end

@protocol CompleteProbeDisplay <SingleProbeDisplay, RETURNABLE>
//S: A class that generates a complete ProbeMap for an object.

//D: A class which generates a GUI to a complete ProbeMap of probes applied 
//D: to a given target object (by complete we mean that all the probes for
//D: the target object's class and its superclasses are included)...
@end

@protocol MultiVarProbeDisplay <CommonProbeDisplay, CREATABLE>
//S: A display for displaying a ProbeMap across a number of objects.

//D: This ProbeDisplay extracts all the variable probes from a probe map
//D: and creates a variable probe entry for each object in the list
//D: provided by the user.
CREATING
//M: Sets the list of objects to display.
- setObjectList: (id <List>)objectList;

//M: Sets the probe map (i.e. list of fields) to display.
- setProbeMap: (id <ProbeMap>)probeMap;

//M: Sets the selector to send for labeling the object.
- setObjectNameSelector: (SEL)objectNameSelector;

USING
@end


@protocol ProbeDisplayManager <SwarmObject, CREATABLE>
//S: The ProbeDisplay manager.

//D: A (singleton) class whose instance is used to manage all the 
//D: ProbeDisplays created by the user during a GUI run of the 
//D: simulation.
USING
- (BOOL)getDropImmediatelyFlag;

- (id <ProbeDisplay>)createProbeDisplayFor: anObject;

- (id <ProbeDisplay>)createArchivedProbeDisplayFor: anObject variableName: (const char *)variableName;

- (id <ProbeDisplay>)createDefaultProbeDisplayFor: anObject;

- (id <ProbeDisplay>)createArchivedDefaultProbeDisplayFor: anObject 
                                             variableName: (const char *)variableName;

- (id <CompleteProbeDisplay>)createCompleteProbeDisplayFor: anObject;

- (id <CompleteProbeDisplay>)createArchivedCompleteProbeDisplayFor: anObject variableName: (const char *)variableName;

//M: Add a probe display to be managed by the ProbeDisplayManager.
- addProbeDisplay: (id <CommonProbeDisplay>)probeDisplay;

//M: Remove a probe display from management by the ProbeDisplayManager.
- removeProbeDisplay: (id <CommonProbeDisplay>)probeDisplay;

//M: Remove and drop probe displays associated with a given object.
- dropProbeDisplaysFor: anObject;

//M: This method will recursively send an update message to all the
//M: Probe Displays managed by the ProbeDisplayManager. 
- (void)update;

- setDropImmediatelyFlag: (BOOL)dropImmediateFlag;

//#: This macro creates a probe display for the given object
#define CREATE_PROBE_DISPLAY(anObject) \
  _createProbeDisplay(anObject)

//#: This macro creates a complete probe display for the given object
#define CREATE_COMPLETE_PROBE_DISPLAY(anObject) \
  _createCompleteProbeDisplay(anObject)

//#: This macro creates the probe display for the given object, to be saved
//#: by the window archiver
#define CREATE_ARCHIVED_PROBE_DISPLAY(anObject) \
  createArchivedProbeDisplayNamed(anObject,#anObject)

//#: This macro creates a complete probe display for the given
//#: object, to be saved  by the window archiver
#define CREATE_ARCHIVED_COMPLETE_PROBE_DISPLAY(anObject) \
  createArchivedCompleteProbeDisplayNamed(anObject,#anObject)

#ifdef __cplusplus
extern "C" {
#endif

extern id <ProbeDisplay> _createProbeDisplay (id obj);
extern id <CompleteProbeDisplay> _createCompleteProbeDisplay (id obj);

extern id <ProbeDisplay> createArchivedProbeDisplayNamed (id obj, const char *name);
extern id <CompleteProbeDisplay> createArchivedCompleteProbeDisplayNamed (id obj, const char *name);

extern const char *buildWindowGeometryRecordName (const char *baseName,
                                                  const char *componentName);
#ifdef __cplusplus
}
#endif
@end

@protocol GUIComposite <CompositeWindowGeometryRecordName>
//S: Base class for objects that use several GUI components.
//D: Base class for objects that use several GUI components.

USING
- (void)enableDestroyNotification: notificationTarget
               notificationMethod: (SEL)notificationMethod;
- (void)disableDestroyNotification;
@end

@protocol GUISwarm <Swarm, WindowGeometryRecordName, CREATABLE>
//S: A version of the Swarm class which is graphics aware. 

//D: GUISwarm is a subclass of Swarm that is used as a toplevel Swarm for
//D: simulations running with a graphical user interface. The GUISwarm
//D: creates a ControlPanel automatically for you and defines a -go method
//D: that interprets the state of the ControlPanel to keep things running
//D: in response to user input. Users subclass GUISwarm much like they
//D: subclass a normal Swarm, implementing the same kind of buildObjects,
//D: buildActions, and activateIn methods. When you are done building your
//D: Observer Swarm, start it as a toplevel via [myGUISwarm go].

//D: The control panel places a few responsibilities on the GUISwarm
//D: subclass author. In particular, a message to [controlPanel doTkEvents]
//D: should be scheduled fairly frequently - only when that method is
//D: executed will the user interface update (and the stop button be
//D: checked). Also, it is often useful to use [controlPanel
//D: setStateStopped] to wait for the user to indicate they're ready for
//D: execution to proceed.

USING
//M: Start the activity running, and also handle user requests via the
//M: control panel. Returns either Completed (the model ran until
//M: requested to terminate) or ControlStateQuit (the user pressed
//M: the quit button).
- go;
GETTERS
- (id <ActionCache>)getActionCache;
- (id <ControlPanel>)getControlPanel;
@end

@protocol MessageProbeWidget <CREATABLE>
//S: A widget for editing the arguments of a MessageProbe.

//D: A widget for editing the arguments of a MessageProbe.
CREATING
- setParent: (id <Frame>)parent;
- setObject: object;
- setProbe: (id <Probe>)probe;

USING
- (void)pack;
@end

@protocol MultiVarProbeWidget <SwarmObject, CREATABLE>
//S: A widget for displaying multiple objects across multiple fields.

//D: A widget for displaying multiple objects across multiple fields.
CREATING
- setParent: (id <Frame>)parent;

//M: Determines if the fields (probes) are labeled (horizontal).
- setFieldLabelingFlag: (BOOL)labelingFlag;
//M: Sets the method to use get the label for each object (vertical).
- setObjectNameSelector: (SEL)objectNameSelector;
//M: Sets the objects to show on the widget.
- setObjectList: (id <List>)objectList;
//M: Sets the fields (probes) to show on the widget.
- setProbeMap: (id <ProbeMap>)probeMap;

USING
- (void)update;
- (void)pack;
@end

//G: Manager that keeps track of active probes to be updated
externvar id <ProbeDisplayManager> probeDisplayManager;

//G: State Symbols for the ControlPanel.
externvar id <Symbol> ControlStateRunning, ControlStateStopped,
  ControlStateStepping, ControlStateNextTime, ControlStateQuit;

//F: Initialize the library and create a ProbeDisplayManager.
extern void initSimtoolsGUI (void);

@class ControlPanel;
@class ActionCache;
@class ProbeDisplay;
@class SingleProbeDisplay;
@class SimpleProbeDisplay;
@class CompleteProbeDisplay;
@class GUIComposite;
@class MultiVarProbeDisplay;
@class ProbeDisplayManager;
@class GUISwarm;
@class MessageProbeWidget;
@class MultiVarProbeWidget;
