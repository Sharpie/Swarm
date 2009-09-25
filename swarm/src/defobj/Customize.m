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
#import <defobj/swarm-objc-api.h>
#import <objc/Protocol.h>

#include <misc.h> // strchr, stpcpy, strlen

extern void _obj_splitPhases (Class class);

extern id _obj_initZone;  // currently receives generated classes

//
// inline functions to save field in copy of class structure used as wrapper
//
#if 0
static inline void
setWrapperCreateBy (Class wrapper, CreateBy_c *createBy)
{
#if SWARM_OBJC_TODO
  wrapper->version = (long) createBy;
#endif
}

static inline CreateBy_c *
getWrapperCreateBy (Class wrapper)
{
#if SWARM_OBJC_TODO
  return (CreateBy_c *) wrapper->version;
#else
  return nil;
#endif
}
#endif

//
// initCustomizeWrapper -- common routine to set up customize wrapper
//
static void
initCustomizeWrapper (id aZone, Customize_s *anObject)
{
#if SWARM_OBJC_DONE
  Class wrapper;
  CreateBy_c *createBy;

  // allocate wrapper class (copy of self class) for instance being customized

  wrapper = (Class) [aZone copyIVars: getClass (anObject)];
  wrapper->info |= _CLS_CUSTOMIZEWRAPPER;

  // allocate a new CreateBy instance and store id in wrapper

  createBy = (CreateBy_c *) [aZone allocIVars: [CreateBy_c self]];
  setMappedAlloc (createBy);
  setWrapperCreateBy (wrapper, createBy);

  // save original self class in CreateBy object until customizeEnd

  createBy->createReceiver = (id) getClass (anObject);  // save original class

  // save zone in CreateBy object until customizeEnd

  createBy->recustomize = aZone;

  // reset self class to point to the wrapper

  setClass (anObject, wrapper);
#else
  CreateBy_c *createBy;

  // allocate customization wrapper for instance being customized

  anObject->wrapper = xmalloc(sizeof(struct customizeWrapper));
  anObject->wrapper->flags |= _CLS_CUSTOMIZEWRAPPER;
  
  // allocate a new CreateBy instance and store id in wrapper

  createBy = (CreateBy_c *) [aZone allocIVars: [CreateBy_c self]];
  setMappedAlloc (createBy);
  anObject->wrapper->createBy = createBy;

  // save original self class in CreateBy object until customizeEnd

  createBy->createReceiver = (id) getClass (anObject);  // save original class

  // save zone in CreateBy object until customizeEnd

  createBy->recustomize = aZone;
#endif
}


//
// Customize_s -- superclass to implement create-phase customization
//

@implementation Customize_s

PHASE(Creating)


+ createBegin: aZone
{
  [self subclassResponsibility: @selector(createBegin:)];
  return nil; 
}


//
// customizeBegin: -- begin customization to define future create
//
+ customizeBegin: aZone
{
  id newObject;

  // allocate object at initial location using createBegin

  newObject = [self createBegin: [aZone getComponentZone]];

  // wrap instance for customization and return allocated object

  initCustomizeWrapper (aZone, newObject);
  return newObject;
}

//
// customizeEnd -- finalize a customization to define future create
//
- customizeEnd
{
  CreateBy_c *createBy;
#if SWARM_OBJC_DONE
  Class wrapper, selfClass;
#else
  Class selfClass;
#endif

  // check that customization in progress

  if (!_obj_customize (self))
    raiseEvent (CreateUsage,
                "> class %s: customizeEnd may only follow customizeBegin\n",
                [[self getClass] getName]);
  
  // get information from self before any possible changes by createEnd
#if SWARM_OBJC_DONE  
  wrapper = getClass (self);
  createBy = getWrapperCreateBy (wrapper);
#else
  createBy = wrapper->createBy;
#endif
  selfClass = createBy->createReceiver;
  
  // execute createEnd to set subclass to handle future create
  
  [(id) self createEnd];  // rely on createEnd to set createBy action
  
  // check that a create selection was made 
  
  if (swarm_class_getSuperclass(getClass (createBy)) != [CreateBy_c self])
    {
      raiseEvent (CreateSubclassing,
                  "> class %s: createEnd did not select a createBy action when called by\n"
                  "> customizeEnd to save a customization\n",
                  [selfClass getName]);
    }
  
  // free self if not retained and not required by CreateBy_c object
  if ((getClass (createBy) == [Create_bycopy self]
       || getClass (createBy) == [Create_byboth self])
      && createBy->createReceiver != self
      && (wrapper->flags & _CLS_RETAINSELF))
    {
#if SWARM_OBJC_TODO
      abort();
#else
      memset (self, 0, wrapper->instance_size);    // wipe out all content
      [createBy->recustomize freeIVars: self];  // free from saved zone
#endif
      // !! should use [self dropFrom: createBy->recustomize] ??
    }
  // else keep self but reset class pointer if still pointing to wrapper
#if SWARM_OBJC_DONE
  else if (getClass (self) == wrapper)
    setClass (self, selfClass);
#endif

  // check for valid message selector and cache method for receiver  
  if (createBy->createMessage)
    {
      createBy->createMethod =
        getMethodFor (getClass (createBy->createReceiver),
                      createBy->createMessage);
      if (!respondsTo (createBy->createReceiver, createBy->createMessage))
        raiseEvent (CreateSubclassing,
                    "> class %s, setCreateByMessage: or setCreateByMessage:to:\n"
                    "> receiver object: %0#8x: %.64s\n"
                    "> message selector name: \"%s\"\n"
                    "> message selector not valid for receiver\n",
                    [[self getClass] getName],
                    createBy->createReceiver,
		    swarm_class_getName (getClass (createBy->createReceiver)),
                    swarm_sel_getName (createBy->createMessage));
    }
  
  // free wrapper class
#if SWARM_OBJC_DONE  
  [createBy->recustomize freeIVars: wrapper];
#else
  xfree(wrapper);
  wrapper = NULL;
#endif

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
  CreateBy_c *createBy;
  Customize_s *newObject;

  // check that customization in progress

  if (_obj_customize (self))
    raiseEvent (CreateUsage,
                "> class %s: customizeCopy must follow customizeBegin\n",
                [[self getClass] getName]);
  
  // make shallow copy of self with original class restored
#if SWARM_OBJC_DONE  
  createBy = getWrapperCreateBy (getClass (self));
#else
  createBy = wrapper->createBy;
#endif
  newObject = [aZone copyIVars: self];
  setClass (newObject, createBy->createReceiver);
  
  // save zone in new wrapper
  
  initCustomizeWrapper (aZone, newObject);
#if SWARM_OBJC_DONE
  createBy = getWrapperCreateBy (getClass (newObject));
#else
  createBy = newObject->wrapper->createBy;
#endif
  createBy->recustomize = aZone;
  return newObject;
}

//
// customizeBeginEnd: -- customize to defaults but with recustomization enabled
//
+ customizeBeginEnd: aZone
{
  id newObject;
  CreateBy_c *createBy;

  newObject = [self customizeBegin: aZone];
  createBy = (CreateBy_c *) [newObject customizeEnd];
  createBy->recustomize = self;
  return createBy;
}

//
// _setCreateBy_: common routine used by setCreateBy_c methods below
//
- _setCreateBy_: (Class)subclass message: (SEL)messageSelector to: anObject
{
#if SWARM_OBJC_DONE
  Class wrapper;
#endif
  CreateBy_c *createBy;

  // check that customization in progress

  if (!_obj_customize (self))
    raiseEvent (CreateUsage,
                "> class %s: customizeEnd must follow customizeBegin\n"
                "> (If classes coded properly, error raised by a createBy... macro\n"
                "> in a createEnd method.)\n", 
                [[self getClass] getName]);
  
  // install subclass as class of CreateBy instance
#if SWARM_OBJC_DONE
  wrapper  = getClass (self);
  createBy = getWrapperCreateBy (wrapper);
#else
  createBy = wrapper->createBy;
#endif
  setClass (createBy, subclass);
  
  // if requested, set values for message send in create data block
  
  if (messageSelector)
    {
      createBy->createReceiver = anObject;
      createBy->createMessage = messageSelector;
    }
  return createBy; 
}

//
// _setCreateByCopy_ -- create by shallow copy of self
//
- (void)_setCreateByCopy_
{
  CreateBy_c *createBy;

  // start with standard checks 

  createBy = (CreateBy_c *)
    [self _setCreateBy_: [Create_bycopy self] message: (SEL)NULL to: nil];
  
  // set values for shallow copy in create data block
  
  createBy->createReceiver = self;
}

//
// _setCreateByMessage_:to: -- create by message to object
//
- (void)_setCreateByMessage_: (SEL)messageSelector to: anObject
{
  CreateBy_c *createBy;
  const char *messageName;
  
  // install wrapper class
  
  createBy = (CreateBy_c *) [self _setCreateBy_: [Create_bysend self]
                                  message: messageSelector
                                  to: anObject];
  
  // confirm valid message selector and return customization wrapper class
  
  messageName = (const char *) swarm_sel_getName (messageSelector);
  if (!messageName
      || !strchr (messageName, ':')
      || ((unsigned)(strchr (messageName, ':') - messageName)
	  != strlen (messageName) - 1))
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

  createBy = (CreateBy_c *) [self _setCreateBy_: [Create_byboth self]
                                  message: (SEL)messageSelector
                                  to: anObject];

  // confirm valid message selector and return customization wrapper class

  messageName = (const char *) swarm_sel_getName (messageSelector);
  if (!messageName ||
      (strchr (messageName, ':')
       && ((unsigned)(strchr (messageName, ':') - messageName)
	   != strlen (messageName) - 1)))
    raiseEvent (CreateSubclassing,
                "> class %s: setCreateByMessage:to: message selector name: \"%s\"\n"
                "> message selector must accept at most one argument\n",
                [[self getClass] getName]);
}

//
// _setRecustomize_: -- set receiver for any more customize/createBegin messages
//
- (void)_setRecustomize_: anObject
{
#if SWARM_OBJC_DONE
  Class wrapper;
#endif
  CreateBy_c *createBy;

  if (!respondsTo (anObject, M(createBegin:)))
    raiseEvent (InvalidArgument,
                "> setRecustomize receiver argument does not respond to createBegin:\n");
  
  // install subclass as class of CreateBy instance
#if SWARM_OBJC_DONE
  wrapper  = getClass (self);
  createBy = getWrapperCreateBy (wrapper);
#else
  createBy = wrapper->createBy;
#endif
  createBy->recustomize = anObject;
}

//
// setTypeImplemented: -- implement type using split-phase classes
//
+ (void)setTypeImplemented: aType
{
  [super setTypeImplemented: aType];
  _obj_splitPhases (self);
}

//
// _obj_splitPhases -- split defining class into class object for each phase
//
void
_obj_splitPhases (Class class)
{
  classData_t classData, newClassData, superclassData = 0;
  BehaviorPhase_s *classCreating, *classUsing;
  Class creatingClass = NULL, usingClass = NULL;
  Class superClass;
  char *classNameBuf;
  methodDefs_t mdefs, instance_mdefs, class_mdefs;
#if SWARM_OBJC_DONE
  Method_t mnext;
#else
  ObjcMethod *mnext;
  int j;
#endif

  // return if classes have already been created
  classData = _obj_getClassData (class);
  if (classData->initialPhase)
    return;
  
  // split classes for superclass if not done already
  superClass = swarm_class_getSuperclass(class);
  if ((id) class != id_Customize_s)
    {
      superclassData = _obj_getClassData (superClass);
      if (!superclassData->initialPhase)
        _obj_splitPhases (superClass);
   }
  
  // generate chain of contiguous methods by interface
  instance_mdefs = _obj_initMethodInterfaces (class);
  class_mdefs = _obj_initMethodInterfaces (swarm_class_getMetaclass (class));

  // create class for methods in Creating phase
  classCreating = nil;
  if (!(instance_mdefs
        && (instance_mdefs->interfaceID == UsingOnly)))
    {
      classCreating = [id_BehaviorPhase_s createBegin: _obj_initZone];
      
      classNameBuf = _obj_initAlloc (strlen (swarm_class_getName(class)) + 10);
      stpcpy (stpcpy (classNameBuf, swarm_class_getName(class)), ".Creating");
      
      creatingClass = swarm_objc_allocateClassPair(superClass, classNameBuf, 0);

      // copy the instance variables
      swarm_class_copyIvars(class, creatingClass);

      [(id) classCreating setName: classNameBuf];
      [(id) classCreating setClass: getClass (class)];
      [(id) classCreating setDefiningClass: creatingClass];
    }
  
  // create class for methods in Using phase
  classUsing = nil;
  if (!(instance_mdefs
        && (instance_mdefs->interfaceID == CreatingOnly)))
    {
      classUsing = [id_BehaviorPhase_s createBegin: _obj_initZone];
      
      classNameBuf = _obj_initAlloc (strlen (swarm_class_getName(class)) + 7);
      stpcpy (stpcpy (classNameBuf, swarm_class_getName(class)), ".Using");

      usingClass = swarm_objc_allocateClassPair(superClass, classNameBuf, 0);

      // copy the instance variables
      swarm_class_copyIvars(class, usingClass);

      [(id) classUsing setName: classNameBuf];
      [(id) classUsing setClass: getClass (id_Object_s)];
      [(id) classUsing setDefiningClass: usingClass];
    }

  // SWARM_OBJC_TODO - Eliminate superclass from BehaviorPhase?
  // set up proper superclass for creatingClass and usingClass
  if (class == id_Customize_s)
    {
      if (classCreating)
        [(id) classCreating setSuperclass: id_Object_s];
      if (classUsing)
        [(id) classUsing setSuperclass: id_Object_s];
      
    }
  else
    {
      if (classCreating)
        {
          if (superclassData->initialPhase->nextPhase == UsingOnly)
            {
              do {
                superclassData = 
                  _obj_getClassData (swarm_class_getSuperclass (superclassData->initialPhase->definingClass));
              }
              while (superclassData->initialPhase->nextPhase == UsingOnly);
              
              [(id) classCreating setSuperclass: superclassData->initialPhase];
              superclassData = _obj_getClassData (superClass);
            }
          else
            [(id) classCreating setSuperclass: superclassData->initialPhase];
        }
      if (classUsing)
        {
          if (superclassData->initialPhase->nextPhase == CreatingOnly)
            do {
              superclassData =
                _obj_getClassData (swarm_class_getSuperclass (superclassData->initialPhase->definingClass));
            }
            while (superclassData->initialPhase->nextPhase == CreatingOnly);
          
	  if (superclassData->initialPhase->nextPhase == UsingOnly)
	    [(id) classUsing setSuperclass: superclassData->initialPhase];
	  else
	    [(id) classUsing setSuperclass: superclassData->initialPhase->nextPhase];
        }
    }
  
  // install methods in whichever phase each method belongs
  
  for (mdefs = (methodDefs_t) instance_mdefs; mdefs; 
       mdefs = mdefs->next)
    {
      if ( mdefs->interfaceID == Creating
           || (mdefs->interfaceID == CreatingOnly &&
               mdefs == instance_mdefs))
        {
	  for (j = 0, mnext = mdefs->firstEntry; j < mdefs->count; ++j) {
#if SWARM_OBJC_DONE
	    swarm_class_addMethod(creatingClass, (ObjcSEL)mnext->method_name,
				  (ObjcIMP)mnext->method_imp, mnext->method_types);
	    [(id) classCreating at: mnext->method_name addMethod: mnext->method_imp];
#else
	    swarm_class_addMethod(creatingClass, (ObjcSEL)swarm_method_getName(mnext[j]),
				  (ObjcIMP)swarm_method_getImplementation(mnext[j]),
				  swarm_method_getTypeEncoding(mnext[j]));
	    //[(id) classCreating at: swarm_method_getName(mnext)
	    //  addMethod: swarm_method_getImplementation(mnext)];
#endif
	  }          
        }
      else if (mdefs->interfaceID == Using
               || (mdefs->interfaceID == UsingOnly &&
                   mdefs == instance_mdefs))
        {
	  for (j = 0, mnext = mdefs->firstEntry; j < mdefs->count; ++j) {
#if SWARM_OBJC_DONE
	    swarm_class_addMethod(usingClass, (ObjcSEL)mnext->method_name,
				  (ObjcIMP)mnext->method_imp, mnext->method_types);
	    [(id) classUsing at: mnext->method_name addMethod: mnext->method_imp];
#else
	    swarm_class_addMethod(usingClass, (ObjcSEL)swarm_method_getName(mnext[j]),
				  (ObjcIMP)swarm_method_getImplementation(mnext[j]),
				  swarm_method_getTypeEncoding(mnext[j]));
	    //[(id) classUsing at: swarm_method_getName(mnext)
	    //  addMethod: swarm_method_getImplementation(mnext)];
#endif
	  }
        }
      else if (mdefs->interfaceID == CreatingOnly ||
               mdefs->interfaceID == UsingOnly)
        {
          raiseEvent (SourceMessage,
                      "> setTypeImplemented: class %s: cannot specify any other phase\n"
                      "> in combination with CreatingOnly or UsingOnly\n");
          
        }
      else if (mdefs->interfaceID == Setting)
        {
	  for (j = 0, mnext = mdefs->firstEntry; j < mdefs->count; ++j) {
#if SWARM_OBJC_DONE
	      swarm_class_addMethod(creatingClass, (ObjcSEL)mnext->method_name,
				    (ObjcIMP)mnext->method_imp, mnext->method_types);
	      swarm_class_addMethod(usingClass, (ObjcSEL)mnext->method_name,
				    (ObjcIMP)mnext->method_imp, mnext->method_types);
	      [(id) classCreating at: mnext->method_name addMethod: mnext->method_imp];
	      [(id) classUsing at: mnext->method_name addMethod: mnext->method_imp];
#else
	      swarm_class_addMethod(creatingClass, (ObjcSEL)swarm_method_getName(mnext[j]),
				    (ObjcIMP)swarm_method_getImplementation(mnext[j]),
				    swarm_method_getTypeEncoding(mnext[j]));
	      swarm_class_addMethod(usingClass, (ObjcSEL)swarm_method_getName(mnext[j]),
				    (ObjcIMP)swarm_method_getImplementation(mnext[j]),
				    swarm_method_getTypeEncoding(mnext[j]));
              //[(id) classCreating at: swarm_method_getName(mnext)
	      //    addMethod: swarm_method_getImplementation(mnext)];
              //[(id) classUsing at: swarm_method_getName(mnext)
	      //    addMethod: swarm_method_getImplementation(mnext)];
#endif
            }
          
        }
      else
        {
          raiseEvent (SourceMessage,
                      "> setTypeImplemented: invalid phase marker in class %s\n",
                      swarm_class_getName (class));
        }        
    }

  // Class methods

  for (mdefs = (methodDefs_t) class_mdefs; mdefs; 
       mdefs = mdefs->next)
    {
      if ( mdefs->interfaceID == Creating
           || (mdefs->interfaceID == CreatingOnly &&
               mdefs == class_mdefs))
        {
	  for (j = 0, mnext = mdefs->firstEntry; j < mdefs->count; ++j) {
	    swarm_class_addMethod(creatingClass, (ObjcSEL)swarm_method_getName(mnext[j]),
				  (ObjcIMP)swarm_method_getImplementation(mnext[j]),
				  swarm_method_getTypeEncoding(mnext[j]));
	    swarm_class_addMethod(swarm_class_getClass (creatingClass),
				  (ObjcSEL)swarm_method_getName(mnext[j]),
				  (ObjcIMP)swarm_method_getImplementation(mnext[j]),
				  swarm_method_getTypeEncoding(mnext[j]));
	    //swarm_class_addMethod(creatingClass, (ObjcSEL)mnext->method_name,
	    //		  (ObjcIMP)mnext->method_imp, mnext->method_types);
	    //swarm_class_addMethod(creatingClass->class_pointer, (ObjcSEL)mnext->method_name,
	    //		  (ObjcIMP)mnext->method_imp, mnext->method_types);
	    //[(id) classCreating at: mnext->method_name addMethod: mnext->method_imp];
	  }          
        }
      else if (mdefs->interfaceID == Using
               || (mdefs->interfaceID == UsingOnly &&
                   mdefs == class_mdefs))
        {
	  for (j = 0, mnext = mdefs->firstEntry; j < mdefs->count; ++j) {
	    swarm_class_addMethod(usingClass, (ObjcSEL)swarm_method_getName(mnext[j]),
				  (ObjcIMP)swarm_method_getImplementation(mnext[j]),
				  swarm_method_getTypeEncoding(mnext[j]));
	    swarm_class_addMethod(swarm_class_getClass (usingClass),
				  (ObjcSEL)swarm_method_getName(mnext[j]),
				  (ObjcIMP)swarm_method_getImplementation(mnext[j]),
				  swarm_method_getTypeEncoding(mnext[j]));
	    //swarm_class_addMethod(usingClass, (ObjcSEL)mnext->method_name,
	    //		  (ObjcIMP)mnext->method_imp, mnext->method_types);
	    //swarm_class_addMethod(usingClass->class_pointer, (ObjcSEL)mnext->method_name,
	    //		  (ObjcIMP)mnext->method_imp, mnext->method_types);
	    //[(id) classUsing at: mnext->method_name addMethod: mnext->method_imp];
	  }
        }
      else if (mdefs->interfaceID == CreatingOnly ||
               mdefs->interfaceID == UsingOnly)
        {
          raiseEvent (SourceMessage,
                      "> setTypeImplemented: class %s: cannot specify any other phase\n"
                      "> in combination with CreatingOnly or UsingOnly\n");
          
        }
      else if (mdefs->interfaceID == Setting)
        {
	  for (j = 0, mnext = mdefs->firstEntry; j < mdefs->count; ++j) {
	      swarm_class_addMethod(creatingClass, (ObjcSEL)swarm_method_getName(mnext[j]),
				    (ObjcIMP)swarm_method_getImplementation(mnext[j]),
				    swarm_method_getTypeEncoding(mnext[j]));
	      swarm_class_addMethod(swarm_class_getClass (creatingClass),
				    (ObjcSEL)swarm_method_getName(mnext[j]),
				    (ObjcIMP)swarm_method_getImplementation(mnext[j]),
				    swarm_method_getTypeEncoding(mnext[j]));
	      swarm_class_addMethod(usingClass, (ObjcSEL)swarm_method_getName(mnext[j]),
				    (ObjcIMP)swarm_method_getImplementation(mnext[j]),
				    swarm_method_getTypeEncoding(mnext[j]));
	      swarm_class_addMethod(swarm_class_getClass (usingClass),
				    (ObjcSEL)swarm_method_getName(mnext[j]),
				    (ObjcIMP)swarm_method_getImplementation(mnext[j]),
				    swarm_method_getTypeEncoding(mnext[j]));
	      /*
	      swarm_class_addMethod(creatingClass, (ObjcSEL)mnext->method_name,
				    (ObjcIMP)mnext->method_imp, mnext->method_types);
	      swarm_class_addMethod(creatingClass->class_pointer, (ObjcSEL)mnext->method_name,
				    (ObjcIMP)mnext->method_imp, mnext->method_types);
	      swarm_class_addMethod(usingClass, (ObjcSEL)mnext->method_name,
				    (ObjcIMP)mnext->method_imp, mnext->method_types);
	      swarm_class_addMethod(usingClass->class_pointer, (ObjcSEL)mnext->method_name,
				    (ObjcIMP)mnext->method_imp, mnext->method_types);
	      */
	      //[(id) classCreating at: mnext->method_name addMethod: mnext->method_imp];
	      //[(id) classUsing at: mnext->method_name addMethod: mnext->method_imp];
            }
          
        }
      else
        {
          raiseEvent (SourceMessage,
                      "> setTypeImplemented: invalid phase marker in class %s\n",
                      swarm_class_getName(class));
        }        
    }

  // install protocols for each phase class

  Protocol **protoList;
  unsigned int outCount, i;
  protoList = (Protocol **)swarm_class_copyProtocolList(class, &outCount);
  if (protoList) {
    //printf("%d protocols for class %s\n", outCount, class->name);
    for (i = 0; i < outCount; ++i) {
      //printf("%s\n", protoList[i]->protocol_name);
      if (creatingClass) swarm_class_addProtocol(creatingClass, (ObjcProtocol *)protoList[i]);
      if (usingClass) swarm_class_addProtocol(usingClass, (ObjcProtocol *)protoList[i]);
    }
  }

  // finalize created classes and root them in the defining class
  
  if (classCreating)
    {
      classCreating = [(id) classCreating createEnd];
      classCreating->nextPhase = classUsing ? classUsing : (BehaviorPhase_s *)CreatingOnly;
      classData->initialPhase  = classCreating;

      swarm_objc_registerClassPair(creatingClass);
      newClassData = _obj_getClassData (creatingClass);
      newClassData->initialPhase = classCreating;
      newClassData->owner = classData->owner;
      newClassData->typeImplemented = classData->typeImplemented;
    }
  else
    {
      classData->initialPhase = classUsing;
      classUsing->nextPhase = UsingOnly;
    }
  if (classUsing)
    {
      classUsing = [(id) classUsing createEnd];
      //if (classData->classID)
      //classData->classID = usingClass;

      swarm_objc_registerClassPair(usingClass);
      newClassData = _obj_getClassData (usingClass);
      newClassData->initialPhase = classUsing;
      newClassData->owner = classData->owner;
      newClassData->typeImplemented = classData->typeImplemented;
    }
  //else if (classData->classID)
  //classData->classID = creatingClass;
}

@end  // end of Create superclass


//
// CreateBy_c -- superclass of customized create action
//

@implementation CreateBy_c

- createBegin: aZone
{
  // send message to recustomization class, if any

  if (!recustomize)
    raiseEvent (CreateUsage,
                "> class %s: createBegin not supported after customization already\n"
                "> completed a first time by customizeBegin/End\n",
                [[self getClass] getName]);
  
  return [recustomize createBegin: aZone];
}

- customizeBegin: aZone
{
  // send message to recustomization class, if any
  
  if (!recustomize)
    raiseEvent (CreateUsage,
                "> class %s: customizeBegin not supported after customization already\n"
                "> completed a first time by customizeBegin/End\n",
                [[self getClass] getName]);
  
  return [recustomize customizeBegin: aZone];
}

//
// mapAllocations: -- standard method to map internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
  mapObject (mapalloc, createReceiver);
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
  // return createMethod (createReceiver, createMessage, aZone);
}

@end


//
// Create_byboth -- class to create instance by sending message to shallow copy
//

@implementation Create_byboth

- create: aZone
{
  id newObject;

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
BOOL
_obj_customize (Customize_s *anObject)
{
#if SWARM_OBJC_DONE
  return (getClass (anObject)->info & _CLS_CUSTOMIZEWRAPPER) != 0;
#else
  if ((anObject->wrapper) && (anObject->wrapper->flags & _CLS_CUSTOMIZEWRAPPER))
    return YES;
  return NO;
#endif
}

//
// compiled versions of extern inline functions
//

//
// getNextPhase() -- return class which implements next phase of object
//
Class
getNextPhase (Class aClass)
{
#if SWARM_OBJC_DONE
  return (Class) ((BehaviorPhase_s *) aClass)->nextPhase;
#else
  classData_t classData = _obj_getClassData (aClass);
  if (classData->initialPhase->nextPhase)
    return classData->initialPhase->nextPhase->definingClass;
  else
    return nil;
#endif
}

//
// setNextPhase() -- change behavior of object to next succeeding phase
//
void
setNextPhase (id anObject)
{
#if SWARM_OBJC_DONE
  *(Class *) anObject = (Class) (*(BehaviorPhase_s **) anObject)->nextPhase;
#else
  classData_t classData = _obj_getClassData (swarm_object_getClass (anObject));
  if (classData->initialPhase->nextPhase)
    swarm_object_setClass(anObject, classData->initialPhase->nextPhase->definingClass);
#endif
}
