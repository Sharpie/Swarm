// Swarm library. Copyright (C) 1996 Santa Fe Institute.
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

#import <objc/objc-api.h>
#import <objc/sarray.h>

#define __USE_FIXED__PROTOTYPES__ // for gcc headers
#include <stdio.h>

extern id _obj_implModule;  // defined in Program.m


@implementation Object_s

//
// getName -- return name of class
//
+ (char *) getName
{
  return (char *)((Class)self)->name;
}

//
// getClass -- get class object which implements behavior of object
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
// getMethods -- get set of all methods in any interface in local class
//
+ getMethods
{
  classData_t  classData;

  classData = _obj_getClassData( self );

  // if ( classData->metaobjects ...
  return nil;
}

//
// getIVarDefs -- get structure type defining local instance variables
//
+ getIVarDefs
{
  return nil;
}

//
// getAllMethods -- get set of all local and inherited methods
//
+ getAllMethods
{
  return nil;
}

//
// getAllIVarDefs -- get structure type defining all local and inherited ivars
//
+ getAllIVarDefs
{
  return nil;
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
// createIDString: -- create string which identifies object
//
- createIDString: aZone
{
  char  buffer[100];

  sprintf( buffer, "%0#8x: %.64s",
           (unsigned int)self, (*(Class *)self)->name );
  return [String create: aZone setC: buffer];
}

//
// perform:[with:with:with:] -- perform message on object and arguments
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

- describe: aZone
{
  id    valueString;
  char  *buffer;

  valueString = [self createIDString: scratchZone];
  buffer = [scratchZone alloc: 20 + strlen( [valueString getC] )];
  sprintf( buffer, "object is %s\n", [valueString getC] );
  return [String create: aZone setC: buffer];
}

@end


//
// respondsTo -- return true if object responds to message  
//
BOOL respondsTo( id anObject, SEL aSel )
{
  return sarray_get( (*(Class *)anObject)->dtable, (size_t)aSel->sel_id ) != 0;
}


//
// methodFor -- lookup method for message on object
//
IMP methodFor( id anObject, SEL aSel )
{
  return sarray_get( (*(Class *)anObject)->dtable, (size_t)aSel->sel_id );
}
