// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Customize.m
Description:  superclass to implement create-phase customization
Library:      defobj
*/

#import <defobj/Customize.h>
#import <defobj/DefClass.h>
#import <defobj/Program.h>
#import <defobj/defalloc.h>
#import <collections.h>
#import <objc/objc-api.h>

#include <misc.h>

extern void _obj_splitPhases (Class_s  *class);

extern id _obj_initZone;  // currently receives generated classes

//
// inline functions to save field in copy of class structure used as wrapper
//

static inline void
setWrapperCreateBy (Class wrapper, CreateBy_c *createBy)
{
  wrapper->version = (long)createBy;
}

static inline CreateBy_c *
getWrapperCreateBy (Class wrapper)
{
  return (CreateBy_c *)wrapper->version;
}

//
// initCustomizeWrapper -- common routine to set up customize wrapper
//
static void
initCustomizeWrapper( id aZone, id anObject )
{
  Class       wrapper;
  CreateBy_c  *createBy;

  // allocate wrapper class (copy of self class) for instance being customized

  wrapper = (Class)[aZone copyIVars: getClass( anObject )];
  wrapper->info |= _CLS_CUSTOMIZEWRAPPER;

  // allocate a new CreateBy instance and store id in wrapper

  createBy = (CreateBy_c *)[aZone allocIVars: [CreateBy_c self]];
  setMappedAlloc( createBy );
  setWrapperCreateBy( wrapper, createBy );

  // save original self class in CreateBy object until customizeEnd

  createBy->createReceiver = (id)getClass( anObject );  // save original class

  // save zone in CreateBy object until customizeEnd

  createBy->recustomize = aZone;

  // reset self class to point to the wrapper

  setClass( anObject, wrapper );
}


//
// Customize_s -- superclass to implement create-phase customization
//

@implementation Customize_s

PHASE(Creating)

//
// customizeBegin: -- begin customization to define future create
//
+ customizeBegin: aZone
{
  id           newObject;

  // allocate object at initial location using createBegin

  newObject = [self createBegin: [aZone getComponentZone]];

  // wrap instance for customization and return allocated object

  initCustomizeWrapper( aZone, newObject );
  return newObject;
}

//
// customizeEnd -- finalize a customization to define future create
//
- customizeEnd
{
  CreateBy_c  *createBy;
  Class       wrapper, selfClass;

  // check that customization in progress

  if ( ! _obj_customize( self ) )
    raiseEvent( CreateUsage,
       "> class %s: customizeEnd may only follow customizeBegin\n",
       [[self getClass] getName] );

  // get information from self before any possible changes by createEnd

  wrapper   = getClass( self );
  createBy  = getWrapperCreateBy( wrapper );
  selfClass = createBy->createReceiver;

  // execute createEnd to set subclass to handle future create

  [(id)self createEnd];  // rely on createEnd to set createBy action

  // check that a create selection was made 

  if ( [getClass( createBy ) superClass] != [CreateBy_c self] ) {
    raiseEvent( CreateSubclassing,
      "> class %s: createEnd did not select a createBy action when called by\n"
      "> customizeEnd to save a customization\n",
      [selfClass getName] );
  }

  // free self if not retained and not required by CreateBy_c object

  if ( ( getClass( createBy ) == [Create_bycopy self] ||
         getClass( createBy ) == [Create_byboth self] ) &&
       createBy->createReceiver != self &&
       (wrapper->info & _CLS_RETAINSELF) ) {
    memset( self, 0, wrapper->instance_size );    // wipe out all content
    [createBy->recustomize freeIVars: self];  // free from saved zone
    // !! should use [self dropFrom: createBy->recustomize] ??

  // else keep self but reset class pointer if still pointing to wrapper

  } else if ( getClass( self ) == wrapper ) {
    setClass( self, selfClass );
  }

  // check for valid message selector and cache method for receiver

  if ( createBy->createMessage ) {
    createBy->createMethod = getMethodFor(
      getClass( createBy->createReceiver ), createBy->createMessage );
    if ( ! respondsTo( createBy->createReceiver, createBy->createMessage ) ) {
      raiseEvent( CreateSubclassing,
	"> class %s, setCreateByMessage: or setCreateByMessage:to:\n"
	"> receiver object: %0#8x: %.64s\n"
	"> message selector name: \"%s\"\n"
	"> message selector not valid for receiver\n",
	[[self getClass] getName],
	createBy->createReceiver, getClass( createBy->createReceiver )->name,
	sel_get_name( createBy->createMessage ) );
    }
  }

  // free wrapper class

  [createBy->recustomize freeIVars: wrapper];

  // reset recustomize field where zone was stored and return customization
  // (Subclass can reset recustomize field after default is set.)

  createBy->recustomize = nil;
  return createBy;
}

//
// customizeCopy: -- copy an object being customized to define future create
//
- customizeCopy: aZone
{
  CreateBy_c  *createBy;
  id          newObject;

  // check that customization in progress

  if ( _obj_customize( self ) )
    raiseEvent( CreateUsage,
       "> class %s: customizeCopy must follow customizeBegin\n",
       [[self getClass] getName] );

  // make shallow copy of self with original class restored

  createBy  = getWrapperCreateBy( getClass( self ) );
  newObject = [aZone copyIVars: self];
  setClass( newObject, createBy->createReceiver );

  // save zone in new wrapper

  initCustomizeWrapper( aZone, newObject );
  createBy = getWrapperCreateBy( getClass( newObject ) );
  createBy->recustomize = aZone;
  return newObject;
}

//
// customizeBeginEnd: -- customize to defaults but with recustomization enabled
//
+ customizeBeginEnd: aZone
{
  id          newObject;
  CreateBy_c  *createBy;

  newObject = [self customizeBegin: aZone];
  createBy = (CreateBy_c *)[newObject customizeEnd];
  createBy->recustomize = self;
  return createBy;
}

//
// _setCreateBy_: common routine used by setCreateBy_c methods below
//
- _setCreateBy_: (Class)subclass message: (SEL)messageSelector to: anObject
{
  Class       wrapper;
  CreateBy_c  *createBy;

  // check that customization in progress

  if ( ! _obj_customize( self ) )
    raiseEvent( CreateUsage,
      "> class %s: customizeEnd must follow customizeBegin\n"
      "> (If classes coded properly, error raised by a createBy... macro\n"
      "> in a createEnd method.)\n", 
      [[self getClass] getName] );

  // install subclass as class of CreateBy instance

  wrapper  = getClass( self );
  createBy = getWrapperCreateBy( wrapper );
  setClass( createBy, subclass );

  // if requested, set values for message send in create data block

  if ( messageSelector ) {
    createBy->createReceiver = anObject;
    createBy->createMessage  = messageSelector;
  }
  return createBy; 
}

//
// _setCreateByCopy_ -- create by shallow copy of self
//
- (void) _setCreateByCopy_
{
  CreateBy_c  *createBy;

  // start with standard checks 

  createBy = (CreateBy_c *)
    [self _setCreateBy_: [Create_bycopy self] message: (SEL)0 to: nil];

  // set values for shallow copy in create data block

  createBy->createReceiver = self;
}

//
// _setCreateByMessage_:to: -- create by message to object
//
- (void) _setCreateByMessage_: (SEL)messageSelector to: anObject
{
  CreateBy_c *createBy;
  const char *messageName;

  // install wrapper class

  createBy = (CreateBy_c *)[self _setCreateBy_: [Create_bysend self]
                 message: messageSelector to: anObject];

  // confirm valid message selector and return customization wrapper class

  messageName = (const char *)sel_get_name( messageSelector );
  if (!messageName
      || !strchr (messageName, ':')
      || strchr(messageName, ':') - messageName != strlen(messageName) - 1)
    raiseEvent (CreateSubclassing,
                "> class %s: setCreateByMessage:to: message selector name: \"%s\"\n"
                "> message selector must accept one argument (for create zone)\n",
                [[self getClass] getName]);
}

//
// _setCreateByMessage_:toCopy: -- create by message to shallow copy
//
- (void) _setCreateByMessage_: (SEL)messageSelector toCopy: anObject
{
  CreateBy_c *createBy;
  const char *messageName;

  // install subclass as wrapper

  createBy = (CreateBy_c *)[self _setCreateBy_: [Create_byboth self]
               message: (SEL)messageSelector to: anObject];

  // confirm valid message selector and return customization wrapper class

  messageName = (const char *)sel_get_name( messageSelector );
  if ( ! messageName ||
       ( strchr(messageName, ':') &&
         strchr(messageName, ':') - messageName != strlen(messageName) - 1 ) ) {
    raiseEvent( CreateSubclassing,
      "> class %s: setCreateByMessage:to: message selector name: \"%s\"\n"
      "> message selector must accept at most one argument\n",
      [[self getClass] getName] );
  }
}

//
// _setRecustomize_: -- set receiver for any more customize/createBegin messages
//
- (void) _setRecustomize_: anObject
{
  Class       wrapper;
  CreateBy_c  *createBy;

  if ( ! respondsTo( anObject, M(createBegin:) ) )
    raiseEvent( InvalidArgument,
    "> setRecustomize receiver argument does not respond to createBegin:\n" );

  // install subclass as class of CreateBy instance

  wrapper  = getClass( self );
  createBy = getWrapperCreateBy( wrapper );
  createBy->recustomize = anObject;
}

//
// setTypeImplemented: -- implement type using split-phase classes
//
+ (void) setTypeImplemented: aType
{
  [super setTypeImplemented: aType];
  _obj_splitPhases( (Class_s *)self );
}

//
// _obj_splitPhases -- split defining class into class object for each phase
//
void
_obj_splitPhases (Class_s *class)
{
  classData_t classData, superclassData = 0;
  BehaviorPhase_s *classCreating, *classUsing;
  char *classNameBuf;
  methodDefs_t mdefs;
  Method_t mnext;

  // return if classes have already been created

  classData = _obj_getClassData( class );
  if ( classData->initialPhase ) return;

  // split classes for superclass if not done already

  if ( (id)class != id_Customize_s ) {
    superclassData = _obj_getClassData( class->superclass );
    if ( ! superclassData->initialPhase )
      _obj_splitPhases( class->superclass );
  }

  // generate chain of contiguous methods by interface

  _obj_initMethodInterfaces( class );  // (creates temporary chain of
                                       // defs in classData->metaobjects)

  // create class for methods in Creating phase

  classCreating = nil;
  if (!(classData->metaobjects
        && ((methodDefs_t)classData->metaobjects)->interfaceID == UsingOnly))
    {
      classCreating = [id_BehaviorPhase_s createBegin: _obj_initZone];
      
      classNameBuf = _obj_initAlloc (strlen (class->name) + 10);
      strcpy (classNameBuf, class->name);
      strcat (classNameBuf, ".Creating");
      
      [(id)classCreating setName: classNameBuf];
      [(id)classCreating setClass: getClass (class)];
      [(id)classCreating setDefiningClass: class];
    }

  // create class for methods in Using phase

  classUsing = nil;
  if ( ! ( classData->metaobjects &&
       ((methodDefs_t)classData->metaobjects)->interfaceID == CreatingOnly ) ) {

    classUsing = [id_BehaviorPhase_s createBegin: _obj_initZone];
  
    [(id)classUsing setName: class->name];
    [(id)classUsing setClass: getClass( id_Object_s )];
    [(id)classUsing setDefiningClass: class];
  }

  if ( class == id_Customize_s ) {
    if ( classCreating ) [(id)classCreating setSuperclass: id_Object_s];
    if ( classUsing )    [(id)classUsing    setSuperclass: id_Object_s];

  } else {
    if ( classCreating ) {
      if ( superclassData->initialPhase->nextPhase == UsingOnly ) {
        do {
          superclassData = 
           _obj_getClassData( superclassData->initialPhase->superclass );
        }
        while ( superclassData->initialPhase->nextPhase == UsingOnly );

        [(id)classCreating setSuperclass: superclassData->initialPhase];
        superclassData = _obj_getClassData( class->superclass );

      } else {
        [(id)classCreating setSuperclass: superclassData->initialPhase];
      }
    }
    if ( classUsing ) {
      if ( superclassData->initialPhase->nextPhase == CreatingOnly )
        do {
          superclassData =
            _obj_getClassData( superclassData->initialPhase->superclass );
        }
        while ( superclassData->initialPhase->nextPhase == CreatingOnly );

      if ( superclassData->initialPhase->nextPhase == UsingOnly )
	[(id)classUsing setSuperclass: superclassData->initialPhase];
      else
        [(id)classUsing setSuperclass: superclassData->initialPhase->nextPhase];
    }
  }

  // install methods in whichever phase each method belongs

  for ( mdefs = (methodDefs_t)classData->metaobjects; mdefs;
        mdefs = mdefs->next ) {

    if ( mdefs->interfaceID == Creating ||
         ( mdefs->interfaceID == CreatingOnly &&
           mdefs == (methodDefs_t)classData->metaobjects ) ) {
      for ( mnext = mdefs->firstEntry;
            mnext < mdefs->firstEntry + mdefs->count; mnext++ )
        [(id)classCreating at: mnext->method_name addMethod: mnext->method_imp];

    } else if ( mdefs->interfaceID == Using ||
                ( mdefs->interfaceID == UsingOnly &&
                  mdefs == (methodDefs_t)classData->metaobjects ) ) {
      for ( mnext = mdefs->firstEntry;
            mnext < mdefs->firstEntry + mdefs->count; mnext++ )
        [(id)classUsing at: mnext->method_name addMethod: mnext->method_imp];

    } else if ( mdefs->interfaceID == CreatingOnly ||
                mdefs->interfaceID == UsingOnly ) {
        raiseEvent( SourceMessage,
          "> setTypeImplemented: class %s: cannot specify any other phase\n"
          "> in combination with CreatingOnly or UsingOnly\n" );

    } else if ( mdefs->interfaceID == Setting ) {
      for ( mnext = mdefs->firstEntry;
            mnext < mdefs->firstEntry + mdefs->count; mnext++ ) {
        [(id)classCreating at: mnext->method_name addMethod: mnext->method_imp];
        [(id)classUsing    at: mnext->method_name addMethod: mnext->method_imp];
      }

    } else {
      raiseEvent( SourceMessage,
        "> setTypeImplemented: invalid phase marker in class %s\n",
        class->name );
    }        
  }

  // finalize created classes and root them in the defining class

  if ( classCreating ) {
    classCreating = [(id)classCreating createEnd];
    classCreating->nextPhase = classUsing ? classUsing : CreatingOnly;
    classData->initialPhase  = classCreating;
  } else {
    classData->initialPhase = classUsing;
    classUsing->nextPhase = UsingOnly;
  }
  if ( classUsing ) {
    classUsing = [(id)classUsing createEnd];
    if ( classData->classID ) *classData->classID = classUsing;
  } else {
    if ( classData->classID ) *classData->classID = classCreating;
  }
}

@end  // end of Create superclass


//
// CreateBy_c -- superclass of customized create action
//

@implementation CreateBy_c

- createBegin: aZone
{
  // send message to recustomization class, if any

  if ( ! recustomize )
    raiseEvent( CreateUsage,
      "> class %s: createBegin not supported after customization already\n"
      "> completed a first time by customizeBegin/End\n",
      [[self getClass] getName] );

  return [recustomize createBegin: aZone];
}

- customizeBegin: aZone
{
  // send message to recustomization class, if any

  if ( ! recustomize )
    raiseEvent( CreateUsage,
      "> class %s: customizeBegin not supported after customization already\n"
      "> completed a first time by customizeBegin/End\n",
      [[self getClass] getName] );

  return [recustomize customizeBegin: aZone];
}

//
// mapAllocations: -- standard method to map internal allocations
//
- (void) mapAllocations: (mapalloc_t)mapalloc
{
  mapObject( mapalloc, createReceiver );
}

@end

//
// Create_bycopy -- class to create instance by making a shallow copy
//

@implementation Create_bycopy

- create: aZone
{
  // Create new instance by copying previously customized instance.

  return [aZone copyIVars: createReceiver];
}

@end

//
// Create_bysend -- class to create instance by sending a message to an object
//

@implementation Create_bysend

- create: aZone
{

  // Create new instance by sending message to receiver.

  return [createReceiver perform: createMessage with: aZone];
  // return createMethod( createReceiver, createMessage, aZone );
}

@end


//
// Create_byboth -- class to create instance by sending message to shallow copy
//

@implementation Create_byboth

- create: aZone
{
  id           newObject;

  // Create new instance by sending message to shallow copy.

  newObject = [aZone copyIVars: createReceiver];
  [newObject perform: createMessage with: aZone];
  return newObject;

  // (extra zone argument is harmless if not referenced)
}

@end

//
// _obj_customize() -- return true if customization in progress
//
extern BOOL _obj_customize( id anObject )
{
  return ( getClass( anObject )->info & _CLS_CUSTOMIZEWRAPPER ) != 0;
}

//
// compiled versions of extern inline functions
//

//
// getNextPhase() -- return class which implements next phase of object
//
extern Class getNextPhase( id aClass )
{
  return ((Class *)aClass)[_obj_NEXTCLASS];
}

//
// setNextPhase() -- change behavior of object to next succeeding phase
//
extern void setNextPhase( id anObject )
{
  *(Class *)anObject = (*(Class **)anObject)[_obj_NEXTCLASS];
}
