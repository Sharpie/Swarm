// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.    

//S: GUI-related features for simulation.

#import <objectbase.h>
#import <activity.h> // SwarmProcess

@protocol WindowGeometryRecordName <SwarmObject>
//S: Protocol for archiving window geometry.

//D: Classes that allow for window geometry archiving must conform
//D: this protocol.

CREATING
//M: This method is used to give an
//M: instance ProbeDisplay a name, which will used by the Archiver
//M: when recording its geometry information.
- setWindowGeometryRecordName: (const char *)windowGeometryRecordName;

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


@protocol ControlPanel <SwarmObject>
//S: Class to control the top level SwarmProcess

//D: ControlPanel keeps track of the users requests to run, stop, quit, or
//D: time step the simulation. It cooperates with the GUISwarm to control
//D: the execution of activities in Swarm.
CREATING
- createEnd;

USING
//M: Get the current button state of the controlpanel.  Is one of
//M: ControlStateRunning, ControlStateStopped, ControlStateStepping,
//M: ControlStateNextTime, or ControlStateQuit.
- getState;

- setState: s;

- startInActivity: activityID;

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

@protocol ActionCache <CompositeWindowGeometryRecordName>
//S: A class to manage threads and Swarms.

//D: A class that provides a smart bag into which actions can be
//D: thrown by other threads and Swarms intended for insertion on
//D: it's Swarm's schedule.
CREATING
- setControlPanel: cp;
- createEnd;
- createProcCtrl;

USING
- setScheduleContext: context;
- insertAction: actionHolder;
- deliverActions;
- sendActionOfType: (id <Symbol>) type toExecute: (const char *)cmd;
- sendStartAction;
- sendStopAction;
- sendStepAction;
- sendNextAction;
- sendQuitAction;
- verifyActions;

- getPanel;
- doTkEvents;  // should change to pollGUI or something
- waitForControlEvent;
@end

//G: Type Symbols for ActionCache
extern id <Symbol> Control, Probing, Spatial;

//G: Error Symbols for ActionCache
extern id <Symbol> InvalidActionType, ActionTypeNotImplemented;

@protocol CommonProbeDisplay <WindowGeometryRecordName>
//S: A protocol underlying ProbeDisplay and CompleteProbeDisplay
//D: This protocol provides the common interface to all kinds of ProbeDisplays.

CREATING
//M: This method must be called.
- setProbedObject: anObject;

USING
//M: Gets the probed object.
- getProbedObject;

//M: This method maintains consistency between the values of the
//M: probedObject's variables and the values which are displayed in
//M: the ProbeDisplay. Ideally, this method should be called every
//M: time the object is modified by the simulation. In practice, the
//M: user schedules an update on the probeDisplayManager which in
//M: turn communicates to all the active ProbeDisplays in the
//M: system.
- update;

- (BOOL)getMarkedForDropFlag;
@end


@protocol ProbeDisplay <CommonProbeDisplay>
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

- createEnd;

USING
//M: Gets the probedMap.
- (id <ProbeMap>)getProbeMap;

@end

@protocol CompleteProbeDisplay <CommonProbeDisplay>
//S: A class that generates a complete ProbeMap for an object.

//D: A class which generates a GUI to a complete ProbeMap of probes applied 
//D: to a given target object (by complete we mean that all the probes for
//D: the target object's class and its superclasses are included)...
@end

extern id <ProbeDisplay> _createProbeDisplay (id obj);
extern id <CompleteProbeDisplay> _createCompleteProbeDisplay (id obj);

extern id <ProbeDisplay> createArchivedProbeDisplayNamed (id obj, const char *name);
extern id <CompleteProbeDisplay> createArchivedCompleteProbeDisplayNamed (id obj, const char *name);


@protocol ProbeDisplayManager <SwarmObject>
//S: The ProbeDisplay manager.

//D: A (singleton) class whose instance is used to manage all the 
//D: ProbeDisplays created by the user during a GUI run of the 
//D: simulation.
USING
- (id <ProbeDisplay>)createProbeDisplayFor: anObject;

- (id <ProbeDisplay>)createArchivedProbeDisplayFor: anObject variableName: (const char *)variableName;

- (id <ProbeDisplay>)createDefaultProbeDisplayFor: anObject;

- (id <ProbeDisplay>)createArchivedDefaultProbeDisplayFor: anObject 
                                             variableName: (const char *)variableName;

- (id <CompleteProbeDisplay>)createCompleteProbeDisplayFor: anObject;

- (id <CompleteProbeDisplay>)createArchivedCompleteProbeDisplayFor: anObject variableName: (const char *)variableName;

//M: Add a probe display to be managed by the ProbeDisplayManager.
- addProbeDisplay: probeDisplay;

//M: Remove a probe display from management by the ProbeDisplayManager.
- removeProbeDisplay: probeDisplay;

//M: Remove a probe display associated with an object 
//M: from management by the ProbeDisplayManager.
- removeProbeDisplayFor: anObject;

//M: Remove and drop probe displays associated with a given object.
- dropProbeDisplaysFor: anObject;

//M: This method will recursively send an update message to all the
//M: Probe Displays managed by the ProbeDisplayManager. 
- update;

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

@end

@protocol GUIComposite <CompositeWindowGeometryRecordName>
//S: Base class for objects that use several GUI components.

//D: Base class for objects that use several GUI components.
USING
- enableDestroyNotification: notificationTarget
         notificationMethod: (SEL)notificationMethod;
- disableDestroyNotification;
@end

@protocol GUISwarm <SwarmProcess, WindowGeometryRecordName>
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
@end

@protocol ActiveGraph <MessageProbe>
//S: Provides a continuous data feed between Swarm and the GUI.

//D: An active graph object is the glue between a MessageProbe (for reading
//D: data) and a BLTGraph GraphElement. ActiveGraphs are created and told
//D: where to get data from and send it to, and then are scheduled to
//D: actually do graphic functions. This class is used by EZGraph, and we
//D: expect to see less direct usage of it by end-users as more analysis
//D: tools (such as EZGraph) internalize its functionality.
USING
//M: Sets the graph element used to draw on.
- setElement: ge;

//M: Sets the object that will be probed for data.
- setDataFeed: d;

//M: Fires the probe, reads the value from the object, and draws it
//M: on the graph element. The X value is implicitly the current
//M: simulation time. Y is the value read. 
- step;
@end

@protocol MessageProbeWidget
//S: A widget for editing the arguments of a MessageProbe.

//D: A widget for editing the arguments of a MessageProbe.
+ createBegin: aZone;
- setParent: parent;
- setObject: object;
- createEnd;
- setProbe: (id <Probe>)probe;
- pack;
@end


//G: Manager that keeps track of active probes to be updated
extern id <ProbeDisplayManager> probeDisplayManager;

//G: State Symbols for the ControlPanel.
extern id ControlStateRunning, ControlStateStopped, ControlStateStepping,
  ControlStateNextTime, ControlStateQuit;

//F: Initialize the library and create a ProbeDisplayManager.
extern void initSimtoolsGUI (void);

@class ControlPanel;
@class ActionCache;
@class ProbeDisplay;
@class CompleteProbeDisplay;
@class ProbeDisplayManager;
@class GUISwarm;
@class ActiveGraph;
@class MessageProbeWidget;
