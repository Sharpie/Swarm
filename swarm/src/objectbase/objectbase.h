// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.
/*
Name:            objectbase.h
Description:     support for swarmobjects and probing   
Library:         swarmobject
Authors:         Manor Askenazi
Date:            1996-12-12
*/

#import <activity.h>

//
//  Swarmobject errors
//
//id <Error> ReferenceError;  // dangling pointer to removed object

//
// SwarmObject --
//   Superclass of most objects in a Swarm simulation.
//   Provides support for probing.
//
@protocol SwarmObject <Create, Drop>
USING
- (const char *)getInstanceName;
- 		getProbeMap;
-		getCompleteProbeMap;
-		getProbeForVariable: (const char *)aVariable;
-		getProbeForMessage: (const char *)aMessage;
@end

// 
// ProbeConfig --
//   Protocol for configuration of probes, probemaps, and 
//   the probelibrary.
//
@protocol ProbeConfig
-              setObjectToNotify: anObject;
-              getObjectToNotify;
@end

//
// Probe --
//   Abstract superclass of both VarProbe and MessageProbe.
//
@protocol Probe <SwarmObject, ProbeConfig>
CREATING
-		setProbedClass ;
SETTING
-		setSafety ;     // There should be a setSafety: BOOL
-		unsetSafety ;   // and getSafety methods instead...
USING
-		clone: aZone ;
- (Class)	getProbedClass ;
- (const char *)getProbedType;
/* 

   These are currently implemented here but belong in VarProbe...

- setStringReturnType: returnType ;      
- (void *)probeRaw: anObject ;
- (void *)probeAsPointer: anObject ;
- (int)   probeAsInt: anObject ;
- (double)probeAsDouble: anObject ;
- (char *)probeAsString: anObject Buffer: (char *) buffer ;

*/
@end

//
// VarProbe --
//   Allows the user to inspect a given variable in any candidate
//   which is an instance of, or inherits from, a given class
//
@protocol VarProbe <Probe>
CREATING
-		setProbedVariable: (const char *)aVariable;
SETTING
-		setStringReturnType: returnType;
USING
- (const char *)getProbedVariable;
-               setFloatFormat: (const char *)format;
//   Currently, setNonInteractive must be used *after* create phase
//   In any case these should also become setInteractive: BOOL etc.

-		setNonInteractive; 
- (int)		isInteractive;

- (void *)	probeRaw: anObject;
- (void *)	probeAsPointer: anObject;
- (int)		probeAsInt: anObject;
- (double)	probeAsDouble: anObject;
- (const char *)probeAsString: anObject Buffer: (char *)buffer;
- (const char *)probeAsString: anObject Buffer: (char *)buf
            withFullPrecision: (int) precision;
-		setData: anObject To: (void *) newValue;  // pass by reference.
- (int)		setData: anObject ToString: (const char *)s; 
@end

//
// MessageProbe --
//   Allows the user to call a given message on any candidate
//   which is an instance of, or inherits from, a given class
//

typedef struct val {
  char type;
  union {
    id object;
    SEL selector;
    char _char;
    unsigned char _uchar;
    int _int;
    float _float;
    double _double;
  } val;
} val_t;

@protocol MessageProbe <Probe>
CREATING
-		setProbedSelector: (SEL)aSel;
-		setHideResult: (BOOL)val;
USING
- (BOOL)	isResultId;                  // I doubt that a user will 
- (BOOL)	isArgumentId: (int)which;    // ever need these.

- (const char *)getProbedMessage;
- (int)		getArgCount;
- (const char *)getArg: (int)which;
- (const char *)getArgName: (int)which;
- (BOOL)	getHideResult;

- setArg: (int)which ToString: (const char *)what;

- (val_t)       dynamicCallOn: target;
@end

//
// ProbeMap --
//   A container for Probes. Used to specify the contents of a ProbeDisplay.
//
@protocol ProbeMap <SwarmObject, ProbeConfig>
CREATING
-		setProbedClass: (Class) class;
USING
- (int)		getNumEntries;
- (Class)	getProbedClass;
-		getProbeForVariable: (const char *)aVariable;
-		getProbeForMessage: (const char *)aMessage;

-		addProbe: aProbe;
-		addProbeMap: aProbeMap;

// These should use the word remove not drop.
-		dropProbeForVariable: (const char *)aVariable; 
-		dropProbeForMessage: (const char *)aMessage;  
-		dropProbeMap: aProbeMap;

-		begin: aZone; // returns an index to the underlying Map.
-		clone: aZone;
@end

/*
        The following classes should really become create-phase options 
        on the simple ProbeMap (i.e. ProbeMap should do a switcheroo).
*/

//
// DefaultProbeMap --
//   A subclass of ProbeMap, whose initial state contains all the VarProbes 
//   of the requested target class and also those of all its
//   superclasses.
//
@protocol DefaultProbeMap <ProbeMap, CREATABLE> @end

//
// CustomProbeMap & EmptyProbeMap --
//   A subclass of ProbeMap, whose initial state is empty unlike the default
//   probeMap initial state which contains all the VarProbes and MessageProbes
//   of the requested target class.
//
@protocol CustomProbeMap <ProbeMap, CREATABLE> 
+ create: aZone forClass: (Class) aClass withIdentifiers: (char *) vars, ...;
@end
@protocol EmptyProbeMap <CustomProbeMap, CREATABLE> @end

//
// CompleteProbeMap --
//   A subclass of ProbeMap, whose initial state contains the VarProbes and 
//   MessageProbes of the requested target class but also those of all its
//   superclasses.
//
@protocol CompleteProbeMap <ProbeMap> @end

//
// CompleteVarMap --
//   A subclass of ProbeMap, whose initial state contains no MessageProbes,
//   but does contain all the VarProbes of the requested target class and 
//   those of all its superclasses.
//
@protocol CompleteVarMap <ProbeMap> @end

//
// ProbeLibrary --
//   A (singleton) Class, whose instance is used as a container for aglobal 
//   mapping between classnames and their 'default' ProbeMaps. These defaults 
//   can be changed by the user, thus allowing him/her to customize the default
//    contents of the ProbeDisplays generated when probing objects.
//
@protocol ProbeLibrary <Create, Drop, ProbeConfig> 
USING
-               setDisplayPrecision: (int) nSigDisplay;
- (int)         getDisplayPrecision;
-               setSavedPrecision: (int) nSigSaved;
- (int)         getSavedPrecision;
- (BOOL)        isProbeMapDefinedFor: (Class) aClass;
-		getProbeMapFor: (Class) aClass;
-		getCompleteProbeMapFor: (Class) aClass;
-		getCompleteVarMapFor: (Class) aClass;
-		getProbeForVariable: (const char *) aVar
                            inClass: (Class) aClass;
-		getProbeForMessage: (const char *) aMessage
                           inClass: (Class) aClass;
-		setProbeMap: aMap For: (Class) aClass; 
@end

extern id  <ProbeLibrary> probeLibrary;


//
// ActivityControl --
//
@protocol ActivityControl <SwarmObject>
CREATING
- attachToActivity: anActivity;
USING
- run;
- stop;
- next;
- step;
- stepUntil: (timeval_t)stopTime;
- (void)terminateActivity;
- updateStateVar;
- getStatus;
- (const char *)getInstanceName;
@end

@class Probe;
@class VarProbe;
@class MessageProbe;
@class ProbeMap;
@class CustomProbeMap;
@class EmptyProbeMap;
@class CompleteProbeMap;
@class CompleteVarMap;
@class ProbeLibrary;
@class ActivityControl;

// 
// These are the base classes for all Swarm objects (agents and
// Swarms).  They have been put in the library header file so
// as not to break any user apps that relied upon the old
// swarmobject.h, which simply included all the swarmobject/*.h files.
// The general rule is that you must #import the header file of any
// class you intend to subclass from.
//
#import <objectbase/SwarmObject.h>
#import <objectbase/Swarm.h>
