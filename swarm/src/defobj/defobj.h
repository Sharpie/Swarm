// Swarm library. Copyright (C) 1996 Santa Fe Institute.
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
-		getType;
-		getClass;
- (BOOL)	respondsTo: (SEL)aSel;

-		describe: anOutputStream;
-		createIDString: aZone;
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
-		getZone;
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
- (char *)	getName;
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
// Zone -- modular unit of storage allocation
//
@deftype Zone <Create, Drop, GetOwner, CREATABLE>
CREATING
- (void)	setObjectCollection: (BOOL)objectCollection;
- (void)	setPageSize: (int)pageSize;
USING
- (BOOL)	getObjectCollection;
- (int)		getPageSize;

-		allocIVars: aClass;
-		copyIVars: anObject;
- (void)	freeIVars: anObject;

-               getObjects;

- (void *)	alloc: (size_t)size;
- (void)	free: (void *)aBlock;

- (void *)	allocBlock: (size_t)size;
- (void *)	copyBlock: (void *)aBlock blockSize: (size_t)size;
- (void)	freeBlock: (void *)aBlock blockSize: (size_t)size;

-		createAllocSize: (size_t)size;

-		getSubzones;
- (void)	mergeWithOwner;

- (BOOL)	isSubzoneAlloc: (void *)pointer;
-		getAllocSubzone: (void *)pointer;
@end

// Inline functions for use of AllocSize objects:
//   void *allocSize( AllocSize *anAllocSize );
//   void *freeSize( AllocSize *anAllocSize, void *aBlock );


//
// RecordType -- definition of a linear collection of typed member values
//
@deftype RecordType <Create, Drop>
CREATING
-		defineMember: aDataType;
-		defineAppendedData: (BOOL)appendedData;
-		definePrependedData: (BOOL)prependedData;
USING
-		getMemberDefinitions;
- (int)		getMemberDataSize;

- (BOOL)	getAppendedData;
- (BOOL)	getPrependedData;
@end

//
// MemberDefinition -- definition of a typed member of a structure
//
@deftype MemberDefinition <DefinedObject, GetOwner>
-		getDataType;
- (int)		getDefinitionOffset;
- (int)		getByteOffset;
@end

//
// Record -- a linear collection of typed member values
//
@deftype Record <Create, Drop, SetInitialValue>
CREATING
+		create: aZone setRecordType: aRecordType, ...;
+		create: aZone setValues: aRecordType, ...;

- (void)	setRecordType: aRecordType;
- (void)	setValues: aRecordType, ...;

- (void)	setAppendedSize: (int)appendedSize;
- (void)	setPrependedSize: (int)prependedSize;

- (void)	setMemberBlock: (void *)memberBlock;
- (void)	setPrependedBlock: (void *)prependedBlock;
USING
- (int)		getAppendedSize;
- (int)		getPrependedSize;

- (void *)	getMemberBlock;
- (void *)	getAppendedBlock;
- (void *)	getPrependedBlock;

- (void)	setValue: aMemberDefinition to: (void *)value;
-		getValue: aMemberDefinition;

- (void)	setValue: aMemberDefinition buffer: (void *)valueBuffer;
- (void)	getValue: aMemberDefinition buffer: (void *)valueBuffer;
@end


//
// Symbol -- object defined as a distinct global id constant
//
@deftype Symbol <Create, GetName, CREATABLE>
CREATING
+		create: aZone setName: (char *)name;
- (void)	setName: (char *)name;
@end

//
// EventType -- a report of some condition detected during program execution
//
@deftype EventType <Symbol, RecordType>
- (void)	raiseEvent;
- (void)	raiseEvent: (void *)eventData, ...;
@end

//
// Warning -- a condition of possible concern to a program developer   
//
@deftype Warning <EventType, CREATABLE>
- (void)	setMessageString: (char *)messageString;
- (char *)	getMessageString;
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
// Event, WarningEvent, ErrorEvent -- occurrences of standard event types
//
@deftype Event <Record>
@end
@deftype WarningEvent <Record>
@end
@deftype ErrorEvent <Record>
@end

// metaobjects defining all loaded elements of a running program

//
// InterfaceIdentifier -- unique label used to identify interface of a type
//
@deftype InterfaceIdentifier <Symbol>
@end

// standard interfaces

extern id <InterfaceIdentifier>  Creating, Setting, Using;

//
// Interface -- a collection of messages which implements part of a type
//
@deftype Interface <GetOwner>
-		getInterfaceIdentifier;
-		getImplementingClass;

-		getMessages;
-		getAttributes;
-		getAllMessages;
-		getAllAttributes;
@end

//
// Type -- a collection of messages comprising the life history of an object
//
@deftype Type <Interface>
-		getSupertypes;
-		getInterfaces;

- (BOOL)	isSubtype: aType;

- (BOOL)	getCreatable;
-		getImplementation;
@end

//
// ProgramModule -- all elements defined by a single loaded program module
//
@deftype ProgramModule <DefinedObject, GetName>
-		getModules;
-		getTypes;
-		getClasses;
-		getSymbols;
@end

//
// Program -- module which includes all loaded modules of a program
//
extern id <ProgramModule>  Program;


//
// DefinedClass -- class which implements an interface of a type
//
@deftype DefinedClass <DefinedObject, GetOwner, GetName>
-		getSuperclass;
- (BOOL)	isSubclass: aClass;

- (void)	setTypeImplemented: aType;
-		getTypeImplemented;

-		getMethods;
-		getIVarDefs;

-		getAllMethods;
-		getAllIVarDefs;

- (IMP)		getMethodFor: (SEL)aSel;
@end

//
// CreatedClass -- class with variables and/or methods defined at runtime 
//
@deftype CreatedClass <Create, DefinedClass, CREATABLE>
CREATING
- (void)	setName: (char *)name;
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
@deftype BehaviorPhase <CreatedClass, CREATABLE>
CREATING
- (void)	setNextPhase: aClass;
USING
-		getNextPhase;
@end



//
// ActionType -- an executable process optionally yielding a result
//
@deftype ActionType <EventType>
CREATING
-		defineParameter: aDataType;
-		defineResult: aDataType;
USING
-		getParameterDefs;
-		getResultDef;
@end

//
// ActionEvent -- an event recording an occurrence of an ActionType
//
@deftype ActionEvent <Event>
-		getStatus;
-		getSubevents;
@end

//
// Message -- a procedure that may be performed on a receiver object
//
@deftype Message <EventType>
- (SEL)		getSelector;
@end

//
// MessageEvent -- an event recording the execution of a message
//
@deftype MessageEvent <ActionEvent>
@end


//
// standard errors
//
extern id <Error>
  SourceMessage,          // message in the source defines error
  NotImplemented,         // requested behavior not implemented by object
  SubclassMustImplement,  // requested behavior must be implemented by subclass
  InvalidCombination,     // invalid combination of set messages for create
  InvalidArgument,        // argument value not valid
  CreateSubclassing,      // improper use of Create subclassing framework
  CreateUsage,            // incorrect sequence of Create protocol messages
  OutOfMemory,            // no more memory available for allocation
  InvalidAllocSize,       // no more memory available for allocation
  InternalError;          // unexpected condition encountered in program

//
// standard warnings
//
extern id <Warning>
  WarningMessage,         // message in the source defines warning
  LibraryUsage,           // invalid usage of library interface
  DefaultAssumed,         // non-silent use of default
  ObsoleteMessage,        // using feature which could be removed in future
  ObsoleteFeature,        // using feature which could be removed in future
  ResourceAvailability;   // resource from runtime environment not available

//
// M() -- macro to abbreviate @selector()
//
#define M( messageName ) @selector( messageName )

//
// func_t -- function pointer type
//
typedef void (*func_t)( void );

//
// type objects generated for module
//
#import <defobj/types.h>

//
// initModule() -- module initialization macro
//
#define initModule(module) _obj_initModule(_##module##_)
extern void _obj_initModule( void *module );

//
// global data and functions
//

extern id _obj_globalZone;        // global storage zone
extern id _obj_scratchZone;       // scratch zone
extern id _obj_initZone;          // zone for init function allocations
extern id _obj_probeZone;         // zone for probe object allocations

#define globalZone       _obj_globalZone
#define scratchZone      _obj_scratchZone

#ifndef _obj_debug
extern BOOL _obj_debug;       // if true then perform all debug error checking
#endif

extern FILE *_obj_xerror;     // output file for error messages
extern FILE *_obj_xdebug;     // output file for debugging messages

extern void xprint(  id anObject );             // debugger object print
extern void xexec(   id anObject, char *name ); // debugger method exec
extern void xfprint( id anObject );             // debugger foreach object print
extern void xfexec(  id anObject, char *name ); // debugger foreach method exec

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

//
// declarations of obsolete message names
//
@protocol _defobj_compatibility
- (void)        dropFrom: aZone;  // now simply drop
@end
