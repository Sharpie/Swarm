/*
Name:            simtools.h
Description:     miscellaneous widgetry
Library:         simtools
Authors:         Manor Askenazi
Date:            1996-19-12

Changes:
* 1997/11/13     Glen E. Ropella
                 Installed in Swarm distrib.  Added ActionCache.
*/

#import <swarmobject.h>
#import <random.h>      // for global RNGs...
#import <tkobjc.h>	// some methods return graphics stuff...
//
// ControlPanel --
//   an class used to control the top-level SwarmProcess.  It 
//   accepts manipulations both by the Swarm it's controlling and
//   the ActionCache.
//
@protocol ControlPanel <SwarmObject>
CREATING
-		createEnd;
USING
-		getState;
-		setState: (id) s;
-		waitForControlEvent;

-               startInActivity: (id) activityID;
-		setStateRunning;
-		setStateStopped;
-		setStateStepping;
-		setStateQuit;
-		setStateNextTime;

// Deprecated methods
-		doTkEvents;
-               getPanel;
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
@protocol ActionCache <SwarmObject>
CREATING
-               setControlPanel: (id) cp;
-               setScheduleContext: (id) context;
-               createEnd;

USING
-               insertAction: (id) actionHolder;
-               deliverActions;
-               sendActionOfType: (id <Symbol>) type 
		       toExecute: (char *) cmd;
-               sendStartAction;
-               sendStopAction;
-               sendStepAction;
-               sendNextAction;
-               sendQuitAction;
-               verifyActions;

-               createProcCtrl ;
-               getPanel;
-               doTkEvents;  //should change to pollGUI or something
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
@protocol ProbeDisplay <SwarmObject>
CREATING
-		setProbedObject: anObject;
-		setProbeMap: (ProbeMap *) probeMap;
USING
-		getProbedObject ;
-		getProbeMap ;
-		update ;  // implemented in SimpleProbeDisplay...
@end

//
// CompleteProbeDisplay --
//   a class which generates a GUI to a complete ProbeMap of probes applied 
//   to a given target object (by complete we mean that all the probes for
//   the target object's class and its superclasses are included)...
//
@protocol CompleteProbeDisplay <SwarmObject>
CREATING
-		setProbedObject: anObject;
USING
-		getProbedObject;
-	        update;
-               getMarkedForDropFlag;
@end

//
// ProbeDisplayManager --
//   a (Singleton) class whose instance is used to manage all the 
//   ProbeDisplays created by the user during a GUI run of the 
//   simulation.
//
@protocol ProbeDisplayManager <SwarmObject>
USING
-		createProbeDisplayFor: anObject;
-               createProbeDisplayFor             : anObject
                       setWindowGeometryRecordName: (const char *)windowGeometryRecordName;

-		createCompleteProbeDisplayFor: anObject;
-               createCompleteProbeDisplayFor     : anObject
                       setWindowGeometryRecordName: (const char *)windowGeometryRecordName;

-		addProbeDisplay: probeDisplay;
-		removeProbeDisplayFor: anObject ;
-		removeProbeDisplay: probeDisplay;
-               setDropImmediatelyFlag: (BOOL)dropImmediateFlag;
-		update;
@end

//
// GUISwarm --
//   a version of the Swarm class which is graphics aware. This is 
//   known to be somewhat awkwardly designed...
//
@protocol GUISwarm <SwarmProcess> @end

//
// UName --
//   a class used to generate unique names (e.g. "critter1", "critter2" etc.)
//
@protocol UName <SwarmObject>
CREATING
+		create: aZone setBaseName: (char *) aString ;
+		create: aZone setBaseNameObject: aStringObject ;

-		setBaseName: (char *) aString ;
-		setBaseNameObject: aStringObject ;
USING
- (char *)	getNewName ;
-		getNewNameObject ;
@end

//
// InFile --
//   a class which was intended to support file input. There have been 
//   justified requests from our userbase to re-design this interface.
//
@protocol InFile <SwarmObject>
CREATING
+		create: aZone withName: (char *) theName ;
USING
- (int)		getWord: (char *) aWord ;
- (int)		getInt: (int *) anInt ;
- (int)         getUnsigned: (unsigned *) anUnsigned;
- (int)         getLong: (long *) aLong ;
- (int)         getUnsignedLong: (unsigned long *) anUnsLong ;
- (int)		getDouble: (double *) aDouble ;
- (int)		getFloat: (float *) aFloat ;
- (int)		getChar: (char *) aChar ;
- (int)		unGetChar: (char) aChar ;
- (int)		skipLine ;
@end

//
// OutFile --
//   a class which was intended to support file output. There have been 
//   justified requests from our userbase to re-design this interface.
//
@protocol OutFile <SwarmObject>
CREATING
+		create: aZone withName: (char *) theName ;
USING
-		putString: (char *) aString ;
-		putInt: (int) anInt ;
-               putUnsigned: (unsigned) anUnsigned;
-               putLong: (long) aLong ;
-               putUnsignedLong: (unsigned long) anUnsLong ;
-		putDouble: (double) aDouble ;
-		putFloat: (float) aFloat ;
-		putChar: (char) aChar ;
-		putTab ;
-		putNewLine ;
@end

//
// AppendFile --
//   a class which was intended to support (appended) file output. There have been 
//   justified requests from our userbase to re-design this interface.
//
@protocol AppendFile <SwarmObject>
CREATING
+		create: aZone withName: (char *) theName ;
USING
-		putString: (char *) aString ;
-		putInt: (int) anInt ;
-               putUnsigned: (unsigned) anUnsigned;
-               putLong: (long) aLong ;
-               putUnsignedLong: (unsigned long) anUnsLong ;
-		putDouble: (double) aDouble ;
-		putFloat: (float) aFloat ;
-		putChar: (char) aChar ;
-		putTab ;
-		putNewLine ;
@end

//
// ObjectLoader --
//   a particularly bad attempt to design some form of standard for object
//   loading 
//
@protocol ObjectLoader <SwarmObject>
USING
+		load: anObject from: aFileObject ;
+		load: anObject fromFileNamed: (const char *) aFileName ;

-		setFileObject: aFileObject ;
-		loadObject: anObject ;
// In case the same class is being loaded multiple times...
-		updateCache: exampleTarget ; 
@end

//
// ObjectSaver --
//   a particularly bad attempt to design some form of standard for object
//   saving 
//   The ProbeMap argument is used to specify what variables get saved.
//
@protocol ObjectSaver <SwarmObject>
USING
+		save: anObject to: aFileObject ;
+		save: anObject to: aFileObject withTemplate: aProbeMap ;
+		save: anObject toFileNamed: (const char *) aFileName ;
+		save: anObject toFileNamed: (const char *) aFileName 
			      withTemplate: (ProbeMap *) aProbeMap ;

-		setFileObject: aFileObject ;
-		setTemplateProbeMap: aProbeMap ;
-		saveObject: anObject ;
@end

//
// QSort --
//   a class (not to be instantiated) wrapper around the C sort routine.
//
@protocol QSort <SwarmObject>
USING
+ (void)	sortObjectsIn: aCollection ;
+ (void)        sortObjectsIn: aCollection using: (SEL) aSelector ;
+ (void)	sortNumbersIn: aCollection ;
+ (void)        sortNumbersIn: aCollection
			using: (int(*)(const void*,const void*)) comp_fun ;
+ (void)        reverseOrderOf: aCollection;
@end

//
// NSelect --
//   a class (not to be instantiated) wrapper around a Knuth algorithm
//   for the selection of exactly N elements form a collection without
//   repetition.
//
@protocol NSelect <SwarmObject>
USING
+ (void)	select: (int) n from: aCollection into: bCollection ;
@end

//
// ActiveGraph --
//   provides the continuous data feed between Swarm and the GUI
//
@protocol ActiveGraph <MessageProbe>
USING
-               setElement: (id) ge;
-               setDataFeed: (id) d;
-               step;
@end

//
// ActiveOutFile --
//   provides the continuous data feed between Swarm and a File
//
@protocol ActiveOutFile <MessageProbe>
USING
-               setFileObject: (id) aFileObj ;
-               setDataFeed: (id) d;
-               step;
@end

// Manager that keeps track of active probes to be updated
extern id <ProbeDisplayManager> probeDisplayManager;

void initSwarm(int argc, char ** argv);

// Flag for whether we're in graphics mode or not. Default is 1.
extern int swarmGUIMode;


@class ControlPanel;
@class ActionCache;
@class ProbeDisplay;
@class CompleteProbeDisplay;
@class ProbeDisplayManager;
@class GUISwarm;
@class UName;
@class InFile;
@class OutFile;
@class AppendFile;
@class ObjectLoader;
@class ObjectSaver;
@class QSort;
@class NSelect;
@class ActiveGraph;
@class ActiveOutFile;

//
// These are the base classes for some of the Simtools objects.  They
// have been put in the library header file so as not to break any user
// apps that relied upon the old simtools.h, which simply included
// all the simtools/*.h files.  The general rule is that you must
// #import the header file of any class you intend to subclass from.
//
#import <simtools/GUISwarm.h>
