// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         DefObject.m
Description:  top-level superclass to provide standard services
Library:      defobj
*/

#import <defobj/DefObject.h>
#import <defobj/DefClass.h>
#import <defobj/Program.h>
#import <collections.h>
#import <collections/Map.h>  //!! for at:memberSlot (until replaced)

#import <objc/objc-api.h>
#import <objc/sarray.h>

#define __USE_FIXED__PROTOTYPES__ // for gcc headers
#include <stdio.h>
#include <string.h>

extern id _obj_implModule;  // defined in Program.m

//
// suballocPrototype --
//   prototype of an ordered set initialized for tracking of suballocations
//   within an object 
//
static id suballocPrototype;

//
// _obj_DisplayNameMap -- map of display names by object id
//
id _obj_DisplayNameMap;


@implementation Object_s

//
// getName -- return name of class
//
+ (char *) getName
{
  return (char *)((Class)self)->name;
}

//
// respondsTo: -- return true if message valid for instance
//
+ (BOOL) respondsTo: (SEL)aSel
{
  return respondsTo( self, aSel );
}
- (BOOL) respondsTo: (SEL)aSel
{
  return respondsTo( self, aSel );
}

//
// getClass -- get class object that implements behavior of object
//
+ getClass
{
  return getClass( self );
}
- getClass
{
  return getClass( self );
}

//
// getZone -- return zone in which object allocated
//
- getZone
{
  return (id)( zbits & ~0x7 );
}

//
// _obj_dropAlloc() --
//   function to free each mapped allocation, including nested allocations
//
void _obj_dropAlloc( mapalloc_t mapalloc, BOOL objectAllocation )
{
  // drop object as an internal component of its zone

  if ( objectAllocation ) {
    [(id)mapalloc->alloc dropAllocations: 1];

  // drop block using zone and size provided along with its descriptor

  } else if ( mapalloc->descriptor == t_ByteArray ) {
    [mapalloc->zone freeBlock: mapalloc->alloc blockSize: mapalloc->size];

  // if member of zone population then avoid drop as a component allocation

  } else if ( mapalloc->descriptor == t_PopulationObject ) {
    [(id)mapalloc->alloc dropAllocations: 0];

  // if leaf object then unset the MappedAlloc bit to suppress further mapping

  } else if ( mapalloc->descriptor == t_LeafObject ) {
    unsetMappedAlloc( (Object_s *)mapalloc->alloc );
    [(id)mapalloc->alloc dropAllocations: 1];

  } else {
    raiseEvent( InvalidArgument,
      "unrecognized descriptor of allocated block in mapAlloc() call\n" );
  }
}

//
// dropAllocations: --
//   internal method to drop all storage allocated for an object
//
// This is the default superclass implementation of the standard internal
// method used to drop allallocations made for an object.  It recursively
// drops all component objects and any internal storage blocks within them,
// as identified by the mapAllocations: method for the object.
//
// A subclass must always execute this superclass method to complete the
// drop process.  A subclass should not override this method except to to
// remove references to the object being dropped, or to free resources other
// than internal allocations.  If audits need to be performed to check whether
// a drop request by an external caller is valid, these checks should be
// performed in a caller-visible drop method, rather than the internal
// dropAllocations method.
//
// If an object contains internal allocations that need to be freed, the
// subclass should make sure the MappedAlloc bit is set on the object, which
// will result in the mapAllocations: method being called for the object.
//
- (void) dropAllocations: (BOOL)componentAlloc
{
  id               zone, suballocList = nil, index =/*-O*/nil;
  suballocEntry_t  suballocEntry;
  struct mapalloc  mapalloc;

  // notify any dependent references of the impending deallocation, and
  // remove and free their entries from the suballocations list

  zone = getZone( self );
  if ( getBit( zbits, BitSuballocList ) ) {
    suballocList = getSuballocList( self );
    index = [suballocList begin: scratchZone];
    [index setLoc: End];
    while ( (suballocEntry = (suballocEntry_t)[index prev]) &&
            suballocEntry->notifyFunction ) {
      suballocEntry->notifyFunction( self, nil, suballocEntry->argument );
      [index remove];
      [zone freeBlock: suballocEntry blockSize: sizeof *suballocEntry];
    }
    if ( ! suballocEntry ) {
      [index drop];
      setBit( zbits, BitSuballocList, 0 );
      [suballocList drop];
    }
  }

  // free any internal allocations mapped explicitly by the object

  //
  //!! This code assumes that the zone requires that all internal allocations,
  //!! including internal storage blocks, must be mapped to assure release of
  //!! object resources.  Later, when deferred reclaim versions of zone have
  //!! implemented, this code should be replaced by a no-op provided that no
  //!! debug checking of user non-referenceability assertions is to be done.
  //

  if ( getBit( zbits, BitMappedAlloc ) ) {
    mapalloc.mappingFunction = _obj_dropAlloc;
    mapalloc.zone            = zone;
    mapalloc.descriptor      = t_ByteArray;
    [(id)self mapAllocations: &mapalloc];
  }

  // free any internal allocations remaining in the suballocations list

  if ( suballocList ) {
    [index setLoc: Start];
    while ( (suballocEntry = (suballocEntry_t)[index next]) ) {
      [getZone( self ) freeBlock: suballocEntry->argument
        blockSize: ((suballocHeader_t)suballocEntry->argument)->suballocSize];
      [index remove];
      [zone freeBlock: suballocEntry blockSize: sizeof *suballocEntry];
    }
    [index drop];
    [getSuballocList( self ) drop];
  }

  // free the local instance variables for the object

  if ( componentAlloc )
    [zone freeIVarsComponent: self];
  else
    [zone freeIVars: self];
}

//
// drop -- default superclass method to drop a free-standing created object
//
- (void) drop
{
  [self dropAllocations: 0];
}

//
// addRef: -- 
//   register a dependent reference to the object (or other suballoation if
//   notifyFunction is nil) that will be notified on any reallocation or
//   deallocation of the object
//
- (ref_t) addRef: (notify_t)notifyFunction withArg: (void *)arg;
{
  id               zone, suballocList, index;
  suballocEntry_t  suballocEntry, nextEntry;

  // initialize prototype for suballocList if not done already

  if ( ! suballocPrototype ) {
    suballocPrototype = [OrderedSet createBegin: globalZone];
    [suballocPrototype
      setIndexFromMemberLoc: offsetof( struct suballocEntry, links )];
    suballocPrototype = [suballocPrototype createEnd];
  }

  // if suballoc list not yet established for object then do it now

  zone = getZone( self );
  if ( ! getBit( zbits, BitSuballocList ) ) {
    suballocList =
      [zone allocBlock: getClass( suballocPrototype )->instance_size];
    memcpy( suballocList, suballocPrototype,
              getClass( suballocPrototype )->instance_size );
    ((Object_s *)suballocList)->zbits = (unsigned)zone;
    self->zbits = (unsigned)suballocList & ( self->zbits & 0x7 );
  } else {
    suballocList = getSuballocList( self );
  }

  // initialize new entry for suballocations list

  suballocEntry = [zone allocBlock: sizeof( *suballocEntry )];
  suballocEntry->notifyFunction = notifyFunction;
  suballocEntry->argument = arg;

  // if notify function specified then add reference to end of list

  if ( notifyFunction ) {
    [suballocList addLast: (id)suballocEntry];

  // else insert at point in sort order defined by key

  } else {
    index = [suballocList begin: scratchZone];
    while ( (nextEntry = (suballocEntry_t)[index next]) &&
            ! nextEntry->notifyFunction &&
            ((suballocHeader_t)arg)->suballocKey <
              ((suballocHeader_t)nextEntry->argument)->suballocKey );
    [index addBefore: (id)suballocEntry];
    [index drop];
  }
  return (ref_t)suballocEntry;
}

//
// removeRef: -- 
//   remove a dependent reference previously added by addRef:
//
- (void) removeRef: (ref_t)refVal
{
  id  index, suballocList;

  suballocList = getSuballocList( self );

  if ( _obj_debug && ! suballocList )
    raiseEvent( InvalidOperation,
  "object from which reference to be removed does not have any references" );

  index = [suballocList createIndex: scratchZone fromMember: (id)refVal];
  [index remove];
  [index drop];
}

//
// getSuperclass -- return class for [super ...] dispatch
//
+ getSuperclass
{
  return ((Class)self)->super_class;
}

//
// isSubclass: -- return true if self is a subclass of argument class
//
+ (BOOL) isSubclass: aClass
{
  Class  superclass;

  superclass = (Class)self;
  while ( 1 ) {
    if ( superclass == (Class)aClass ) return 1;
    if ( ! superclass->super_class ) return 0;
    superclass = superclass->super_class;
  }
}

//
// setTypeImplemented: -- initialize class as implementation of type
//
+ (void) setTypeImplemented: aType
{
  classData_t  classData;

  if ( _obj_implModule == nil )
    raiseEvent( SourceMessage,
  "setTypeImplemented: implementating classes for types can only be declared\n"
  "from a module \"_implement\" function\n" );

  if ( ! aType )
    raiseEvent( InvalidArgument,
      "setTypeImplemented: argument is nil\n"
      "(argument may be an uninitialized type from an uninitialized module)\n"
      "Module currently being initialized is: %s\n",
      [_obj_implModule getName] );

  if ( getClass( aType ) != id_Type_c )
    raiseEvent( InvalidArgument,
      "setTypeImplemented: argument is not a type object\n" );

  classData = _obj_getClassData( self );

  if ( classData->owner != _obj_implModule )
    raiseEvent( SourceMessage,
      "setTypeImplemented: class %s in module %s does not belong to module\n"
      "currently being initialized (%s)\n",
      ((Class)self)->name, [classData->owner getName],
      [_obj_implModule getName] );

  if ( classData->typeImplemented &&
       *(id *)classData->typeImplemented != self )
    raiseEvent( SourceMessage,
      "setTypeImplemented: class %s, requested to implement the type %s,\n"
      "has already been specified as the implementation of type %s\n",
      ((Class)self)->name, [aType getName],
      [classData->typeImplemented getName] );

  classData->typeImplemented = aType;
}

//
// getTypeImplemented
//
+ getTypeImplemented
{
  return _obj_getClassData( self )->typeImplemented;
}

//
// getOwner -- get module in which class defined
//
+ getOwner
{
  return _obj_getClassData( self )->owner;
}

//
// getMethodFor: -- return method defined for message to instance, if any
//
+ (IMP) getMethodFor: (SEL)aSel
{
  return sarray_get( ((Class)self)->dtable, (size_t)aSel->sel_id );
}  

//
// getDefiningClass -- return class which defines ivar structure of CreatedClass
//
+ getDefiningClass
{
  return nil;  //!! need to check if CreatedClass class
}

//
// getNextPhase -- return BehaviorPhase class for next succeeding phase
//
+ getNextPhase
{
  return nil;  //!! need to check if BehaviorPhase class
}

//
// self -- return id of class object
//
+ self
{
  return self;
}

//
// getType -- return type which defines message interface of object
//
- getType
{
  return _obj_getClassData( *(Class_s **)self )->typeImplemented;
}

//
// compare: -- default compare: method that compares addresses of two objects
//
- (int) compare: anObject
{
  if ( self < anObject ) return -1;
  return ( self > anObject );
}

//
// perform:[with:[with:[with:]]] --
//   method to perform a message on an object with arguments, defined locally
//   for optional inheritance from the Object superclass, and to define the
//   three-argument form
//

- perform: (SEL)aSel
{
  IMP  mptr;

  mptr = objc_msg_lookup( self, aSel );
  if ( ! mptr ) raiseEvent( InvalidArgument, "message selector not valid\n" );
  return mptr( self, aSel );
}

- perform: (SEL)aSel with: anObject1
{
  IMP  mptr;

  mptr = objc_msg_lookup( self, aSel );
  if ( ! mptr ) raiseEvent( InvalidArgument, "message selector not valid\n" );
  return mptr( self, aSel, anObject1 );
}

- perform: (SEL)aSel with: anObject1 with: anObject2
{
  IMP  mptr;

  mptr = objc_msg_lookup( self, aSel );
  if ( ! mptr ) raiseEvent( InvalidArgument, "message selector not valid\n" );
  return mptr( self, aSel, anObject1, anObject2 );
}

- perform: (SEL)aSel with: anObject1 with: anObject2 with: anObject3
{
  IMP  mptr;

  mptr = objc_msg_lookup( self, aSel );
  if ( ! mptr ) raiseEvent( InvalidArgument, "message selector not valid\n" );
  return mptr(self, aSel, anObject1, anObject2, anObject3);
}

//
// copy -- method to disable the method inherited from the Object superclass
//         that does not use the defobj allocation conventions
//
- copy
{
  raiseEvent( SubclassMustImplement, nil );
  return nil;
}

//
// setDisplayName: -- record a string that identifies an object for external
//                    display purposes
//
- (void) setDisplayName: (char *)aName
{
  char  *displayName, buffer[100];
  id    *memPtr;

  if ( ! _obj_DisplayNameMap) {
    _obj_DisplayNameMap = [Map createBegin: globalZone];
    [_obj_DisplayNameMap setCompareFunction: compareIDs];
    _obj_DisplayNameMap = [_obj_DisplayNameMap createEnd];
  }

  if ( ! aName ) {
    sprintf( buffer, "%0#8x: %.64s",
             (unsigned int)self, getClass( self )->name );
    aName = buffer;
  }

  displayName = [globalZone alloc: strlen( aName ) + 1];
  strcpy( displayName, aName );

  memPtr = (id *)&displayName;
  if ( ! [_obj_DisplayNameMap at: self memberSlot: &memPtr] )
    [_obj_globalZone free: displayName];

  //!! later: register a reference to the object so that the string entry can
  //   be removed when it's later dropped
}

//
// getDisplayName -- return a string that identifies an object for external
//                   display purposes, either from a previously assigned
//                   string or an identification string default
//
- (char *) getDisplayName
{
  char  *displayName;

  if ( ! _obj_DisplayNameMap ) [self setDisplayName: (char *)0];

  displayName = (char *)[_obj_DisplayNameMap at: self];
  if ( displayName ) return displayName;
  [self setDisplayName: (char *)0];
  return (char *)[_obj_DisplayNameMap at: self];
}

//
// describe: -- concatenate a string describing an object to an output stream
//
- (void) describe: outputCharStream
{
  char  buffer[100], *displayName;

  sprintf( buffer, "%0#8x: %.64s",
           (unsigned int)self, getClass( self )->name );
  [outputCharStream catC: buffer];

  displayName = [self getDisplayName];

  if ( strcmp( displayName, buffer ) != 0 ) {
    [outputCharStream catC: ", display name: "];
    [outputCharStream catC: displayName];
  }
  [outputCharStream catC: "\n"];
}

//
// xprint -- print description of object on debug output stream
//
- (void) xprint
{
  id  describeString;

  describeString = [String create: scratchZone];
  [self describe: describeString];
  fprintf( _obj_xdebug, [describeString getC] );
  [describeString drop];
}

@end

//
// respondsTo() -- function to test if object responds to message  
//
BOOL respondsTo( id anObject, SEL aSel )
{
  return sarray_get( getClass( anObject )->dtable, (size_t)aSel->sel_id ) != 0;
}

//
// getMethodFor() --
//   function to look up the method that implements a message within a class
//
IMP getMethodFor( Class aClass, SEL aSel )
{
  return sarray_get( aClass->dtable, (size_t)aSel->sel_id );
}

//
// xprint() -- function to print the debug description for an object
//
void xprint( id anObject )
{
  if ( anObject ) {
    [anObject xprint];
  } else {
    fprintf( _obj_xdebug, "object is nil\n" );
  }
}

//
// xfprint() --
//   function to print the debug description for each member of a collection
//
void xfprint( id anObject )
{
  if ( anObject ) {
    [anObject xfprint];
  } else {
    fprintf( _obj_xdebug, "object is nil\n" );
  }
}

//
// xexec() -- debug function to perform message on an object
//
void xexec( id anObject, char *msgName )
{
  SEL  sel;

  if ( anObject ) {
    sel = sel_get_any_uid( msgName );
    if ( sel ) {
      if ( [anObject respondsTo: sel] ) {
	[anObject perform: sel];
      } else {
        fprintf( _obj_xdebug,
                "Object %0#8x: %.64s does not respond to message %s\n",
              (unsigned int)anObject, [[anObject getClass] getName], msgName );
      }
    } else {
      fprintf( _obj_xdebug, "message \"%s\" is not defined\n", msgName );
    }
  } else {
    fprintf( _obj_xdebug, "object is nil" );
  }
}

//
// xfexec() -- debug function to perform message on each member of a collection
//
void xfexec( id anObject, char *msgName )
{
  fprintf( stderr, "function not yet implemented\n" );
}
