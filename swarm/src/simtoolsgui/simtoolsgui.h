// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.    

/*
Name:            simtoolsgui.h
Description:     miscellaneous widgetry
Library:         simtoolsgui
*/

#import <objectbase.h>

#define SET_WINDOW_GEOMETRY_RECORD_NAME(theWidget) \
  [theWidget setWindowGeometryRecordName: #theWidget]

@protocol WindowGeometryRecordName <SwarmObject>
- setWindowGeometryRecordName: (const char *)windowGeometryRecordName;
@end

#define SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME_FOR(obj, theWidget) \
  [(obj) setWindowGeometryRecordNameForComponent: #theWidget widget: theWidget]

#define SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME(theWidget) \
  SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME_FOR (self,theWidget)

@protocol CompositeWindowGeometryRecordName <WindowGeometryRecordName>
- setWindowGeometryRecordNameForComponent: (const char *)componentName
                                   widget: widget;
@end

//
// ControlPanel --
//   an class used to control the top-level SwarmProcess.  It 
//   accepts manipulations both by the Swarm it's controlling and
//   the ActionCache.
//
@protocol ControlPanel <SwarmObject>
CREATING
- createEnd;
USING
- getState;
- setState: s;

- startInActivity: activityID;
- setStateRunning;
- setStateStopped;
- setStateStepping;
- setStateQuit;
- setStateNextTime;

@end
// State Symbols for the ControlPanel.
extern id ControlStateRunning, ControlStateStopped;
extern id ControlStateStepping, ControlStateNextTime, ControlStateQuit;

//
// ActionCache --
//   a class that provides a smart bag into which actions can be
//   thrown by other threads and Swarms intended for insertion on
//   it's Swarm's schedule.
//
@protocol ActionCache <CompositeWindowGeometryRecordName>
CREATING
- setControlPanel: cp;
- createEnd;
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

- createProcCtrl;
- getPanel;
- doTkEvents;  // should change to pollGUI or something
- waitForControlEvent;
@end
// Type Symbols for ActionCache
extern id <Symbol> Control, Probing, Spatial;
// Error Symbols for ActionCache
extern id <Symbol> InvalidActionType, ActionTypeNotImplemented;


//
// ProbeDisplay --
//   a class which generates a GUI to a ProbeMap of probes applied to a 
//   given target object...
//
@protocol ProbeDisplay <WindowGeometryRecordName>
CREATING
- setProbedObject: anObject;
- setProbeMap: (ProbeMap *)probeMap;
USING
- getProbedObject;
- getProbeMap;
- update;  // implemented in SimpleProbeDisplay...
@end

//
// CompleteProbeDisplay --
//   a class which generates a GUI to a complete ProbeMap of probes applied 
//   to a given target object (by complete we mean that all the probes for
//   the target object's class and its superclasses are included)...
//
@protocol CompleteProbeDisplay <WindowGeometryRecordName>
CREATING
- setProbedObject: anObject;
USING
- getProbedObject;
- update;
- getMarkedForDropFlag;
@end

//
// ProbeDisplayManager --
//   a (Singleton) class whose instance is used to manage all the 
//   ProbeDisplays created by the user during a GUI run of the 
//   simulation.
//
void _createProbeDisplay (id obj);
void _createCompleteProbeDisplay (id obj);

void createArchivedProbeDisplayNamed (id obj, const char *name);
void createArchivedCompleteProbeDisplayNamed (id obj, const char *name);

#define CREATE_PROBE_DISPLAY(anObject) \
  _createProbeDisplay(anObject)

#define CREATE_COMPLETE_PROBE_DISPLAY(anObject) \
  _createCompleteProbeDisplay(anObject)

#define CREATE_ARCHIVED_PROBE_DISPLAY(anObject) \
  createArchivedProbeDisplayNamed(anObject,#anObject)

#define CREATE_ARCHIVED_COMPLETE_PROBE_DISPLAY(anObject) \
  createArchivedCompleteProbeDisplayNamed(anObject,#anObject)


@protocol ProbeDisplayManager <SwarmObject>
USING
- createProbeDisplayFor: anObject;
- createArchivedProbeDisplayFor: anObject variableName: (const char *)variableName;
- createCompleteProbeDisplayFor: anObject;
- createArchivedCompleteProbeDisplayFor: anObject variableName: (const char *)variableName;

- addProbeDisplay: probeDisplay;
- removeProbeDisplayFor: anObject;
- removeProbeDisplay: probeDisplay;
- dropProbeDisplaysFor: anObject;
- setDropImmediatelyFlag: (BOOL)dropImmediateFlag;
- update;
@end

@protocol GUIComposite <CompositeWindowGeometryRecordName>
- enableDestroyNotification: notificationTarget
         notificationMethod: (SEL)notificationMethod;
- disableDestroyNotification;
@end

//
// GUISwarm --
//   a version of the Swarm class which is graphics aware. This is 
//   known to be somewhat awkwardly designed...
//
@protocol GUISwarm <SwarmProcess, WindowGeometryRecordName> @end

//
// ActiveGraph --
//   provides the continuous data feed between Swarm and the GUI
//
@protocol ActiveGraph <MessageProbe>
USING
- setElement: ge;
- setDataFeed: d;
- step;
@end

// Manager that keeps track of active probes to be updated
extern id <ProbeDisplayManager> probeDisplayManager;

void initSimtoolsGUI (void);

const char *buildWindowGeometryRecordName (const char *baseWindowGeometryRecordName,
                                           const char *componentName);

@class ControlPanel;
@class ActionCache;
@class ProbeDisplay;
@class CompleteProbeDisplay;
@class ProbeDisplayManager;
@class GUISwarm;
@class ActiveGraph;

#if 0
// These are the base classes for some of the Simtools objects.  They
// have been put in the library header file so as not to break any user
// apps that relied upon the old simtools.h, which simply included
// all the simtools/*.h files.  The general rule is that you must
// #import the header file of any class you intend to subclass from.
//
#import <simtools/GUISwarm.h>
#endif
