// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         defobj.h
Description:  standard objects for GNU Objective C extensions
Library:      defobj
*/

#import <defobj/deftype.h>

//
// DefinedObject -- object with defined type and implementation
//
@deftype DefinedObject
-		getZone;
- (BOOL)	respondsTo: (SEL)aSel;
-		getClass;

- (ref_t)	addRef: (notify_t)notifyFunction withArgument: (void *)arg;
- (void)	removeRef: (ref_t)refVal;

- (int)		compare: anObject;

-		perform: (SEL)aSel;
-		perform: (SEL)aSel with: anObject1;
-		perform: (SEL)aSel with: anObject1 with: anObj2;
-		perform: (SEL)aSel with: anObject1 with: anObj2 with: anObj3;

- (void)	setDisplayName: (const char *)displayName;
- (const char *)getDisplayName;

- (void)	describe: outputCharStream;
- (void)	describeID: outputCharStream;
- (void)	xprint;
- (void)	xprintid;
@end

//
// Create -- create an instance of a type with optional customization
//
@deftype Create <DefinedObject>
CREATING
+		create: aZone;
+		createBegin: aZone;
-		createEnd;

+		customizeBegin: aZone;
-		customizeEnd;
-		customizeCopy: aZone;
@end

//
// Drop -- deallocate an object allocated within a zone
//
@deftype Drop
- (void)	drop;
@end


//
// Miscellaneous operations for mixing into other types.
//

//
// Copy -- copy all state defined as part of object
//
@deftype Copy
-		copy: aZone;
@end

//
// GetName - get name which identifies object in its context of use
//
@deftype GetName
+ (const char *)getName;
@end

//
// GetOwner - get object on which existence of object depends
//
@deftype GetOwner
-		getOwner;
@end

//
// SetInitialValue -- create using initial value from an existing object
//
@deftype SetInitialValue
CREATING
+		create: aZone setInitialValue: initialValue;
+		create: aZone setReadOnlyValue: readOnlyValue;

- (void)	setInitialValue: initialValue;
- (void)	setReadOnlyValue: readOnlyValue;
SETTING
- (void)	setReadOnly: (BOOL)readOnly;
USING
- (BOOL)	getReadOnly;
@end


//
// Symbol -- object defined as a distinct global id constant
//
@deftype Symbol <Create, GetName, CREATABLE>
CREATING
+		create: aZone setName: (const char *)name;
- (void)	setName: (const char *)name;
@end

//
// EventType -- a report of some condition detected during program execution
//
@deftype EventType <Symbol>
- (void)	raiseEvent;
- (void)	raiseEvent: (const void *)eventData, ...;
@end

//
// Warning -- a condition of possible concern to a program developer   
//
@deftype Warning <EventType, CREATABLE>
- (void)	setMessageString: (const char *)messageString;
- (const char *)getMessageString;
@end

//
// Error -- a condition which prevents further execution
//
@deftype Error <Warning, CREATABLE>
@end

//
// raiseEvent() -- macro to raise Warning or Error with source location strings
//
#define raiseEvent( eventType, formatString, args... ) \
[eventType raiseEvent: \
"\r" __FUNCTION__, __FILE__, __LINE__, formatString , ## args]


//
// Zone -- modular unit of storage allocation
//
@deftype Zone <Create, Drop, CREATABLE>
CREATING
- (void)	setReclaimPolicy: reclaimPolicy;
- (void)	setStackedSubzones: (BOOL)stackedSubzones;
- (void)	setPageSize: (int)pageSize;
USING
-		getReclaimPolicy;
- (BOOL)	getStackedSubzones;
- (int)		getPageSize;

-		allocIVars: aClass;
-		copyIVars: anObject;
- (void)	freeIVars: anObject;

-		allocIVarsComponent: aClass;
-		copyIVarsComponent: anObject;
- (void)	freeIVarsComponent: anObject;

-		getComponentZone;

- (void *)	alloc: (size_t)size;
- (void)	free: (void *)aBlock;

- (void *)	allocBlock: (size_t)size;
- (void)	freeBlock: (void *)aBlock blockSize: (size_t)size;

-               getPopulation;
-		getSubzones;
- (void)	mergeWithOwner;

- (BOOL)	containsAlloc: (void *)alloc;
-		getSubzone: (void *)alloc;

- (void)	reclaimStorage;
- (void)	releaseStorage;

- (void)	describeForEach: outputCharStream;
- (void)	describeForEachID: outputCharStream;
- (void)	xfprint;
- (void)	xfprintid;
@end

//
// symbol values for ReclaimPolicy option
//
extern id <Symbol>  ReclaimImmediate, ReclaimDeferred,
                    ReclaimFrontierInternal, ReclaimInternal, ReclaimFrontier;


//
// DefinedClass -- class which implements an interface of a type
//
@deftype DefinedClass <DefinedObject, GetName>
+		getSuperclass;
+ (BOOL)	isSubclass: aClass;

+ (void)	setTypeImplemented: aType;
+		getTypeImplemented;

+ (IMP)		getMethodFor: (SEL)aSel;
@end

//
// CreatedClass -- class with variables and/or methods defined at runtime 
//
@deftype CreatedClass <Create, DefinedClass>
CREATING
- (void)	setName: (const char *)name;
- (void)	setClass: aClass;
- (void)	setSuperclass: aClass;
- (void)	setDefiningClass: aClass;

- (void)	at: (SEL)aSel addMethod: (IMP)aMethod;
USING
-		getDefiningClass;
@end

//
// BehaviorPhase -- created class which implements a phase of object behavior
//
@deftype BehaviorPhase <CreatedClass>
CREATING
- (void)	setNextPhase: aClass;
USING
-		getNextPhase;
@end


//
// standard errors
//
extern id <Error> 
  SourceMessage,          // message in the source defines error
  NotImplemented,         // requested behavior not implemented by object
  SubclassMustImplement,  // requested behavior must be implemented by subclass
  InvalidCombination,     // invalid combination of set messages for create
  InvalidOperation,       // invalid operation for current state of receiver
  InvalidArgument,        // argument value not valid
  CreateSubclassing,      // improper use of Create subclassing framework
  CreateUsage,            // incorrect sequence of Create protocol messages
  OutOfMemory,            // no more memory available for allocation
  InvalidAllocSize,       // no more memory available for allocation
  InternalError,          // unexpected condition encountered in program
  BlockedObjectAlloc,     // method from Object with invalid allocation
  BlockedObjectUsage;     // method inherited from Object superclass

//
// standard warnings
//
extern id <Warning>
  WarningMessage,         // message in the source defines warning
  ResourceAvailability,   // resource from runtime environment not available
  LibraryUsage,           // invalid usage of library interface
  DefaultAssumed,         // non-silent use of default
  ObsoleteFeature,        // using feature which could be removed in future
  ObsoleteMessage;

//
// predefined type descriptors for allocated blocks
//
extern id <Symbol>  t_ByteArray, t_LeafObject, t_PopulationObject;

//
// M() -- macro to abbreviate @selector()
//
#define M( messageName ) @selector( messageName )

//
// PTR_... -- macro settings for machine-dependent pointer options
//
#ifndef PTRSIZE
#define PTRSIZE 4
#endif

#ifndef PTRFMT
#if PTRSIZE == 4
#define PTRFMT "%0#8lx"
#else
#define PTRFMT "%0#16lx"
#endif
#endif

//
// _obj_formatIDString() --
//   function to generate object id string in standard format
//
// (Up to 78 characters of the supplied buffer argument could be filled.)
//
extern void _obj_formatIDString (char *buffer, id anObject);

//
// declaration to enable use of @class declaration for message receiver without
// compile error (discovered by trial and error; the declaration appears in
// <objc/objc-api.h> which is also sufficient to suppress the error)
//
extern Class objc_get_class (const char *name);  // for class id lookup

//
// type objects generated for module
//
#import <defobj/types.h>


//
// global data and functions
//

//
// initModule() -- module initialization macro
//
#define initModule(module) _obj_initModule(_##module##_)
extern void _obj_initModule( void *module );

extern id _obj_globalZone;   // global storage zone
extern id _obj_scratchZone;  // scratch zone

#define globalZone       _obj_globalZone
#define scratchZone      _obj_scratchZone

#ifndef _obj_debug
extern BOOL _obj_debug;       // if true then perform all debug error checking
#endif

extern FILE *_obj_xerror;     // output file for error messages
extern FILE *_obj_xdebug;     // output file for debugging messages

extern void xsetname (id anObject, const char *name); // debug set display name
extern void xprint (id anObject);             // debug object print
extern void xprintid (id anObject);           // debug object id print
extern void xfprint (id anObject);            // debug foreach object print
extern void xfprintid (id anObject)  ;              // debug foreach id print
extern void xexec (id anObject, const char *name);  // debug method exec
extern void xfexec (id anObject, const char *name); // debug foreach method exec

extern id nameToObject (const char *name);

//
// old stuff for compatibility during the transition
//

//
// macros used to create and initialize warning and error symbols
// (obsolete once module system in use)
//

#define defsymbol( name ) \
name = [Symbol create: globalZone setName: #name]

#define defwarning( name, message ) \
[(name = [Warning create: globalZone setName: #name]) \
setMessageString: message]

#define deferror( name, message ) \
[(name = [Error create: globalZone setName: #name]) \
setMessageString: message]
