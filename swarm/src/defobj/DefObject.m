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
Name:         DefObject.m
Description:  top-level superclass to provide standard services
Library:      defobj
*/

#import <defobj/DefObject.h>
#import <defobj/DefClass.h>
#import <defobj/Program.h>
#import <defobj/defalloc.h>
#import <collections.h>
#import <collections/Map.h>  //!! for at:memberSlot (until replaced)
#import <defobj/HDF5Object.h>
#import "internal.h" // process_array, map_object_ivars
                     // lisp_output_type, lisp_process_array

#import <defobj/macros.h>

#import <defobj/swarm-objc-api.h>

#include <misc.h> // strcpy, strlen, sprintf, isDigit
#include <collections/predicates.h> // arrayp, keywordp, archiver_list_p, stringp

#ifdef GNUSTEP
#include <Foundation/NSMethodSignature.h>
#else
#ifdef USE_MFRAME
#if SWARM_OBJC_TODO
#include <objc/mframe.h>
#endif
#endif
#endif

#ifdef HAVE_JDK
#include "java.h"
#endif

#import "COM.h"

#import <defobj.h> // FCall, FArguments


#import <defobj/directory.h>

extern id _obj_implModule;  // defined in Program.m

//
// _obj_displayNameMap -- map of display names by object id
//
id _obj_displayNameMap;

//
// _obj_sessionZone --
//   zone used for session-wide allocations that are not intended as part of
//   the program itself
// (Used in DefObject.m to hold the display name map.)
//
extern id _obj_sessionZone;

//
// suballocPrototype --
//   prototype of collections used to hold a list of suballocations for an
//   object
//
static id suballocPrototype;

//
// describeStream --
//   output file stream on which describe messages to be printed
//
static id describeStream;

@implementation Object_s

PHASE(Creating)

- lispInCreate: expr
{
  return self;
}

- hdf5InCreate: (id <HDF5>)hdf5Obj
{
  return self;
}

PHASE(Setting)
- lispIn: expr
{
  id <Index> li = [expr begin: [expr getZone]];
  id key, val;

  while ((key = [li next]) != nil)
    {
      const char *ivarname;

      if (!keywordp (key))
        raiseEvent (InvalidArgument, "expecting keyword [%s]", [key name]);

      if (!(val = [li next]))
        raiseEvent (InvalidArgument, "missing value");
      
      ivarname = [key getKeywordName];
      object_setVariableFromExpr (self, ivarname, val);
    }
  [li drop];
  return self;
}

- hdf5In: (id <HDF5>)hdf5Obj
{
  if ([hdf5Obj getDatasetFlag])
    [hdf5Obj shallowLoadObject: self];
  else
    {
      int process_object (id component)
        {
          [component assignIvar: self];
          return 0;
        }
      [hdf5Obj iterate: process_object];
    }
  return self;
}

PHASE(Using)

+ (const char *)getName
{
#if SWARM_OBJC_DONE
  return ((Class) self)->name;
#else
  return swarm_class_getName(self);
#endif
}

//
// getName -- return name of class
//
- (const char *)getName
{
  return swarm_directory_language_independent_class_name_for_objc_object (self);
}

//
// respondsTo: -- return true if message valid for instance
//
+ (BOOL)respondsTo: (SEL)aSel
{
  return respondsTo (self, aSel);
}

- (BOOL)respondsTo: (SEL)aSel
{
  return respondsTo (self, aSel);
}

+ (BOOL)conformsTo: (Protocol *)protocol
{
#if SWARM_OBJC_DONE
  if (getBit (((Class) self)->info, _CLS_DEFINEDCLASS))
    return [((CreatedClass_s *) self)->definingClass
                                     conformsTo: protocol];
#else
  if (swarm_class_getDefinedClassBit(swarm_object_getClass(self)))
    return swarm_class_conformsToProtocol (self, (ObjcProtocol *)protocol);
#endif
  else
#if SWARM_OSX
	return [super conformsToProtocol: protocol];
#else
    return [super conformsTo: protocol];
#endif
}

//
// getClass -- get class object that implements behavior of object
//
+ (Class)getClass
{
  return getClass (self);
}

- (Class)getClass
{
  return SD_GETCLASS (self);
}

//
// getZone -- return zone in which object allocated
//
- getZone
{
  return getZone (self);
}

//
// _obj_dropAlloc() --
//   function to free each mapped allocation, including nested allocations
//
void
_obj_dropAlloc (mapalloc_t mapalloc, BOOL objectAllocation)
{
  // drop object as an internal component of its zone

  if (objectAllocation)
    [(id) mapalloc->alloc dropAllocations: YES];
  
  // drop block using zone and size provided along with its descriptor
  
  else if (mapalloc->descriptor == t_ByteArray)
    [mapalloc->zone freeBlock: mapalloc->alloc blockSize: mapalloc->size];

  // if member of zone population then avoid drop as a component allocation
  else if (mapalloc->descriptor == t_PopulationObject)
    [(id) mapalloc->alloc dropAllocations: NO];

  // if leaf object then unset the MappedAlloc bit to suppress further mapping

  else if (mapalloc->descriptor == t_LeafObject)
    {
      unsetMappedAlloc ((Object_s *) mapalloc->alloc);
      [(id) mapalloc->alloc dropAllocations: YES];
      
    }
  else
    raiseEvent (InvalidArgument,
                "> unrecognized descriptor of allocated block\n"
                "> in mapAlloc() call\n");
}

//
// drop --
//   standard method to drop all storage allocated for an object
//
// This is the default superclass implementation of the standard method used
// to drop all allocations made for an object.  It recursively drops all
// component objects and any internal storage blocks within them, as
// identified by the mapAllocations: method for the object.
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
- (void)drop
{
  id zone, suballocList, index = /*-O*/nil;
  suballocEntry_t  suballocEntry;
  struct mapalloc  mapalloc;

  // notify any dependent references of the impending deallocation, and
  // remove and free their entries from the suballocations list

  zone = getZone (self);
  suballocList = getSuballocList (self);
  if (suballocList)
    {
      index = [suballocList begin: scratchZone];
      [index setLoc: End];
      while ((suballocEntry = (suballocEntry_t) [index prev])
             && suballocEntry->notifyFunction)
        {
          suballocEntry->notifyFunction (self, nil, suballocEntry->argument);
          [index remove];
          [zone freeBlock: suballocEntry blockSize: sizeof *suballocEntry];
        }
      [index drop];
      if (!suballocEntry)
        {
          setBit (zbits, BitSuballocList, 0);
          [zone freeBlock: suballocList
#if SWARM_OBJC_DONE
                blockSize: getClass (suballocList)->instance_size];
#else
                blockSize: swarm_class_getInstanceSize(swarm_object_getClass(suballocList))];
#endif
          suballocList = nil;
        }
    }

  // free any internal allocations mapped explicitly by the object
  
  //
  //!! This code assumes that the zone requires that all internal allocations,
  //!! including internal storage blocks, must be mapped to assure release of
  //!! object resources.  Later, when deferred reclaim versions of zone have
  //!! implemented, this code should be replaced by a no-op except when
  //!! debug checking of user non-referenceability assertions is to be done.
  //
  
  if (getBit (zbits, BitMappedAlloc))
    {
      mapalloc.mappingFunction = _obj_dropAlloc;
      mapalloc.zone = zone;
      mapalloc.descriptor = t_ByteArray;
      [(id) self mapAllocations: &mapalloc];
    }
  
  // free any internal allocations remaining in the suballocations list
  
  if (suballocList)
    {
      [index setLoc: Start];
      while ((suballocEntry = (suballocEntry_t) [index next]))
        {
          [zone freeBlock: suballocEntry->argument
                blockSize: ((suballocHeader_t) suballocEntry->argument)->suballocSize];
          [index remove];
          [zone freeBlock: suballocEntry blockSize: sizeof *suballocEntry];
        }
      [index drop];
      [zone freeBlock: suballocList
#if SWARM_OBJC_DONE
            blockSize: getClass (suballocList)->instance_size];
#else
            blockSize: swarm_class_getInstanceSize(swarm_object_getClass(suballocList))];
#endif
    }
  
  // free the local instance variables for the object
  
  if (getBit (zbits, BitComponentAlloc))
    FREEIVARSCOMPONENT (zone, self);
  else
    [zone freeIVars: self];
}

//
// dropAllocations: --
//   obsolete method formerly used as an internal drop method
//   (Should use the simple, generic drop instead, which uses BitComponentAlloc
//   to free using the correct method.)
//
- (void)dropAllocations: (BOOL)componentAlloc
{
  if (getBit (zbits, BitComponentAlloc) && !componentAlloc)
    raiseEvent (InvalidOperation,
                "object was allocated as a component allocation but dropAllocations: "
                "requested drop as a free-standing object\n");
  
  else if (!getBit (zbits, BitComponentAlloc) && componentAlloc)
    raiseEvent (InvalidOperation,
                "object was allocated as a free-standing object but dropAllocations: "
                "requested drop as a component allocation\n");
  
   DROP (self);
}

//
// addRef:withArgument: -- 
//   register a dependent reference to the object (or other suballoation if
//   notifyFunction is nil) that will be notified on any reallocation or
//   deallocation of the object
//
- (ref_t)addRef: (notify_t)notifyFunction withArgument: (void *)arg
{
  id zone, suballocList, index;
  suballocEntry_t  suballocEntry, nextEntry;
  
  // initialize prototype for suballocList if not done already
  
  if (!suballocPrototype)
    {
      suballocPrototype = [OrderedSet createBegin: globalZone];
      [suballocPrototype
        setIndexFromMemberLoc: offsetof (struct suballocEntry, links)];
      suballocPrototype = [suballocPrototype createEnd];
    }
  
  // if suballoc list not yet established for object then do it now
  
  if (!getBit (zbits, BitSuballocList))
    {
      zone = getZone (self);
      suballocList =
#if SWARM_OBJC_DONE
        [zone allocBlock: getClass (suballocPrototype)->instance_size];
#else
        [zone allocBlock: swarm_class_getInstanceSize(swarm_object_getClass(suballocPrototype))];
#endif
      memcpy (suballocList,
              suballocPrototype,
#if SWARM_OBJC_DONE
              getClass (suballocPrototype)->instance_size);
#else
              swarm_class_getInstanceSize(swarm_object_getClass(suballocPrototype)));
#endif
      ((Object_s *) suballocList)->zbits = (unsigned long) zone;
      self->zbits =
        (unsigned long) suballocList | (self->zbits & 0x7) | BitSuballocList;
      zone = getZone (self);
    }
  else
    {
      suballocList = getSuballocList (self);
      zone = getZone ((Object_s *) suballocList);
    }

  // initialize new entry for suballocations list

  suballocEntry = [zone allocBlock: sizeof (*suballocEntry)];
  suballocEntry->notifyFunction = notifyFunction;
  suballocEntry->argument = arg;

  // if notify function specified then add reference to end of list

  if (notifyFunction)
    [suballocList addLast: (id) suballocEntry];
  else // else insert at point in sort order defined by key
    {
      index = [suballocList begin: scratchZone];
      while ((nextEntry = (suballocEntry_t)[index next])
             && !nextEntry->notifyFunction
             && ((suballocHeader_t) arg)->suballocKey <
             ((suballocHeader_t) nextEntry->argument)->suballocKey);
      [index addBefore: (id) suballocEntry];
      [index drop];
    }
  return (ref_t) suballocEntry;
}

//
// removeRef: -- 
//   remove a dependent reference previously added by addRef:
//
- (void)removeRef: (ref_t)refVal
{
  id index, suballocList;
  
  suballocList = getSuballocList (self);
  
  if (_obj_debug && ! suballocList)
    raiseEvent(InvalidOperation,
               "> object from which reference to be removed does not have any references");
  
  index = [suballocList createIndex: scratchZone fromMember: (id) refVal];
  [index remove];
  [index drop];
}

//
// getSuperclass -- return class for [super ...] dispatch
//
+ getSuperclass
{
#if SWARM_OBJC_DONE
  return ((Class) self)->super_class;
#else
  return swarm_class_getSuperclass(self);
#endif
}

//
// isSubclass: -- return true if self is a subclass of argument class
//
+ (BOOL)isSubclass: aClass
{
  Class superclass;

  superclass = (Class) self;
  while (YES)
    {
      if (superclass == (Class) aClass)
        return YES;
#if SWARM_OBJC_DONE
      if (!superclass->super_class)
        return NO;
      superclass = superclass->super_class;
#else
      if (!swarm_class_getSuperclass(superclass))
        return NO;
      superclass = swarm_class_getSuperclass(superclass);
#endif
    }
}

//
// setTypeImplemented: -- initialize class as implementation of type
//
+ (void)setTypeImplemented: aType
{
  classData_t classData;
  
  if (_obj_implModule == nil)
    raiseEvent (SourceMessage,
                "> setTypeImplemented: implementating classes for types can only be declared\n"
                "> from a module \"_implement\" function\n");
  
  if (!aType)
    raiseEvent (InvalidArgument,
                "> setTypeImplemented: argument is nil\n"
                "> (argument may be an uninitialized type from an uninitialized module)\n"
                "> Module currently being initialized is: %s\n",
                [_obj_implModule getName]);
  
  if (getClass (aType) != id_Type_c)
    raiseEvent (InvalidArgument,
                "> setTypeImplemented: argument is not a type object\n");
  
  classData = _obj_getClassData (self);
  
  if (classData->owner != _obj_implModule)
    raiseEvent (SourceMessage,
                "> setTypeImplemented: class %s in module %s does not belong to module\n"
                "> currently being initialized (%s)\n",
                swarm_class_getName(self),
                [classData->owner getName],
                [_obj_implModule getName]);
  
  if (classData->typeImplemented
      && *(id *) classData->typeImplemented != self)
    raiseEvent (SourceMessage,
                "> setTypeImplemented: class %s, requested to implement the type %s,\n"
                "> has already been specified as the implementation of type %s\n",
                swarm_class_getName(self), [aType getName],
                [classData->typeImplemented getName] );
  
  classData->typeImplemented = aType;
}

//
// getTypeImplemented
//
+ getTypeImplemented
{
  return _obj_getClassData (self)->typeImplemented;
}

//
// getOwner -- get module in which class defined
//
+ getOwner
{
  return _obj_getClassData (self)->owner;
}

//
// getMethodFor: -- return method defined for message to instance, if any
//
+ (IMP)getMethodFor: (SEL)aSel
{
#if SWARM_OBJC_DONE
  return sarray_get (((Class) self)->dtable, (size_t) aSel->sel_id);
#else
  return swarm_class_getMethodImplementation(self, aSel);
#endif
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
  return _obj_getClassData (self->isa)->typeImplemented;
}

//
// compare: -- default compare: method that compares addresses of two objects
//
- (int)compare: anObject
{
  if ((id)self < (id)anObject)
    return -1;
  return ((id)self > (id)anObject);
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

  mptr = swarm_class_getMethodImplementation (swarm_object_getClass (self), aSel);
  if (!mptr)
    raiseEvent (InvalidArgument, "> message selector not valid\n");
  return mptr (self, aSel);
}

- perform: (SEL)aSel with: anObject1
{
  IMP  mptr;
  
  mptr = swarm_class_getMethodImplementation (swarm_object_getClass (self), aSel);
  if (!mptr)
    raiseEvent (InvalidArgument, "> message selector not valid\n");
  return mptr (self, aSel, anObject1);
}

- perform: (SEL)aSel with: anObject1 with: anObject2
{
  IMP  mptr;
  
  mptr = swarm_class_getMethodImplementation (swarm_object_getClass (self), aSel);
  if (!mptr)
    raiseEvent (InvalidArgument, "> message selector not valid\n");
  return mptr (self, aSel, anObject1, anObject2);
}

- perform: (SEL)aSel with: anObject1 with: anObject2 with: anObject3
{
  IMP  mptr;
  
  mptr = swarm_class_getMethodImplementation (swarm_object_getClass (self), aSel);
  if (!mptr)
    raiseEvent (InvalidArgument, "> message selector not valid\n");
  return mptr (self, aSel, anObject1, anObject2, anObject3);
}

#if SWARM_OBJC_TODO
#ifdef USE_MFRAME
- (retval_t)forward: (SEL)aSel : (arglist_t)argFrame
{
  NSArgumentInfo info;
  id <FArguments> fa;
  id <FCall> fc;
  types_t val;
  const char *type = swarm_sel_getTypeEncoding (aSel);
#ifdef HAVE_JDK
  jobject jObj;
#endif
  COMobject cObj;
  id <Zone> aZone = getCZone (getZone (self));

  if (!type)
    {
      aSel = swarm_sel_getUid (swarm_sel_getName (aSel));
      type = swarm_sel_getTypeEncoding (aSel);
      if (!type)
        abort ();
    }


  fa = [FArguments createBegin: aZone];

  if ((cObj = SD_COM_FIND_OBJECT_COM (self)))
    {
      COMselector cSel; 

      if (!(cSel = SD_COM_FIND_SELECTOR_COM (aSel)))
        raiseEvent (InvalidArgument,
                    "unable to find COM selector `%s' in objc:`%s' %p\n",
                    swarm_sel_getName (aSel),
                    [self name],
                    self,
                    cObj);
      {
        id <Symbol> language = (COM_selector_is_javascript (cSel)
                                ? LanguageJS
                                : LanguageCOM);
        [fa setLanguage: language];
      }
    }
#ifdef HAVE_JDK
  else if ((jObj = SD_JAVA_FIND_OBJECT_JAVA (self)))
    {
      jobject jSel;
      jclass jClass = (*jniEnv)->GetObjectClass (jniEnv, jObj);

      jSel = SD_JAVA_ENSURE_SELECTOR_JAVA (jClass, aSel);
      (*jniEnv)->DeleteLocalRef (jniEnv, jClass);

#if 0      
      if (!jSel)
        raiseEvent (InvalidArgument,
                    "unable to find Java selector `%s' in objc:`%s' %p java: %p hash: %d\n",
                    swarm_sel_getName (aSel),
                    [self name],
                    self,
                    jObj,
                    swarm_directory_java_hash_code (jObj));
#endif
      
      if (jSel)
        {
          const char *sig = java_ensure_selector_type_signature (jSel);
          
          [fa setJavaSignature: sig];
          [scratchZone free: (void *) sig];
        }
    }
#endif
  else
    {
      [fa drop];
      [self doesNotRecognize: aSel];
      return NULL;
    }
  
  type = mframe_next_arg (type, &info);
  mframe_get_arg (argFrame, &info, &val);
  [fa setObjCReturnType: *info.type];
  /* skip object and selector */
  type = mframe_next_arg (type, &info);
  type = mframe_next_arg (type, &info);
  while ((type = mframe_next_arg (type, &info)))
    {
      mframe_get_arg (argFrame, &info, &val);
      [fa addArgument: &val ofObjCType: *info.type];
    }
  fa = [fa createEnd];

  fc = [FCall create: aZone
              target: self
              selector: aSel
              arguments: fa];
  
  if (fc)
    {
      [fc performCall];
      {
        types_t typebuf;
        extern void *alloca (size_t);
        retval_t retValBuf = alloca (MFRAME_RESULT_SIZE);
        retval_t retVal;
        
        retVal = [fc getRetVal: retValBuf buf: &typebuf];
        [fc drop];
        [fa drop];
        return retVal;
      }
    }
  else
    {
      [fa drop];
      [self doesNotRecognize: aSel];
      return NULL;
    }
}
#endif
#endif

//
// methods inherited from Object that are blocked because they do not support
// the defined model of zone-based allocation
//
#ifdef INHERIT_OBJECT
- copy         { raiseEvent (BlockedObjectAlloc, nil); return nil; }
+ alloc        { raiseEvent (BlockedObjectAlloc, nil); return nil; }
- free         { raiseEvent (BlockedObjectAlloc, nil); return nil; }
- shallowCopy  { raiseEvent (BlockedObjectAlloc, nil); return nil; }
- deepen;      { raiseEvent (BlockedObjectAlloc, nil); return nil; }
- deepCopy;    { raiseEvent (BlockedObjectAlloc, nil); return nil; }
#endif

//
// methods inherited from Object that are not officially supported in public
// interface but still supported with warnings until some future release
//
#ifdef INHERIT_OBJECT_WITH_ERRORS
// (suppress automatic declaration of methods by indenting from column 1)
 - (Class)init
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - (Class)class
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - (Class)superClass
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - (MetaClass)metaClass
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - (const char *)name
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 - self
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - (unsigned int)hash
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 - (BOOL)isEqual:anObject
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 - (BOOL)isMetaClass
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 - (BOOL)isClass
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 - (BOOL)isInstance
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 - (BOOL)isKindOf:(Class)aClassObject
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 - (BOOL)isMemberOf:(Class)aClassObject
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 - (BOOL)isKindOfClassNamed:(const char *)aClassName
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 - (BOOL)isMemberOfClassNamed:(const char *)aClassName
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 + (BOOL)instancesRespondTo:(SEL)aSel
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 + (IMP)instanceMethodFor:(SEL)aSel
 { raiseEvent (BlockedObjectUsage, nil); return (IMP)0; }
 + (BOOL) conformsTo: (Protocol*)aProtocol
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 - (BOOL) conformsTo: (Protocol*)aProtocol
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 + (struct objc_method_description *)descriptionForInstanceMethod:(SEL)aSel
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 - (struct objc_method_description *)descriptionForMethod:(SEL)aSel
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 + poseAs:(Class)aClassObject
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - (Class)transmuteClassTo:(Class)aClassObject
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - subclassResponsibility:(SEL)aSel
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - notImplemented:(SEL)aSel
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - shouldNotImplement:(SEL)aSel
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - doesNotRecognize:(SEL)aSel
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - error:(const char *)aString, ...
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 + (int)version
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 + setVersion:(int)aVersion
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 + (int)streamVersion: (TypedStream*)aStream
 { raiseEvent (BlockedObjectUsage, nil); return 0; }
 - read: (TypedStream*)aStream
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - write: (TypedStream*)aStream
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
 - awake
 { raiseEvent (BlockedObjectUsage, nil); return nil; }
#else
- doesNotRecognize: (SEL)sel
{
  raiseEvent (InvalidArgument, "%s does not recognize %s\n",
              [self name], swarm_sel_getName (sel));
  return self;
}
#endif
//
// notifyDisplayName() --
//  function to maintain display name on change of object allocation
//
static void
notifyDisplayName (id object, id reallocAddress, void *arg)
{
  const char *displayName;
  
  displayName = (const char *) [_obj_displayNameMap removeKey: object];
  if (reallocAddress)
    [_obj_displayNameMap at: object insert: (id) displayName];
  else
    [_obj_sessionZone
      freeBlock: (void *) displayName blockSize: strlen (displayName) + 1];
}

//
// setDisplayName: -- record a string that identifies an object for external
//                    display purposes
//
- (void)setDisplayName: (const char *)aName
{
  char buffer[100], *displayName;
  id *memPtr;

  // allocate a display name map if not done already

  if (!_obj_displayNameMap)
    {
      _obj_displayNameMap = [Map createBegin: _obj_sessionZone];
      [_obj_displayNameMap setCompareFunction: compareIDs];
      _obj_displayNameMap = [_obj_displayNameMap createEnd];
    }
  
  // set display name to the default if no name string given

  if (!aName)
    {
      sprintf (buffer, PTRHEXFMT ": %.64s",
               self, swarm_class_getName(swarm_object_getClass(self)));
      aName = buffer;
    }
  
  // allocate a new display name and initialize its name string
  
  displayName = [_obj_sessionZone allocBlock: strlen (aName) + 1];
  strcpy (displayName, aName);

  // insert display name into the global display name map, if not already there

  memPtr = (id *) &displayName;
  if (![_obj_displayNameMap at: self memberSlot: &memPtr])
    {
      [_obj_sessionZone freeBlock: displayName blockSize: strlen (aName) + 1];
      
      // if new display name, then register a dependent reference from the
      // display name to the object
      
    }
  else
    [self addRef: notifyDisplayName withArgument: nil];
}

//
// getDisplayName -- return a string that identifies an object for external
//                   display purposes, either from a previously assigned
//                   string or an identification string default
//
- (const char *)getDisplayName
{
  const char *ret;

  ret = (_obj_displayNameMap
         ? (const char *) [_obj_displayNameMap at: self]
         : NULL);
      
  if (!ret)
    {
      const char *name = [self getName];
      
      [self setDisplayName: name];
      SFREEBLOCK (name);
      
      ret = (const char *) [_obj_displayNameMap at: self];
    }
  
  return ret;
}

#define ATDELIMCHAR '@'
- (const char *)getObjectName
{
  static char name[512];
  
  if (self)
    {
      sprintf (name, "%s%c" PTRHEXFMT, [self name], ATDELIMCHAR, self);
      return name;
    }
  return "nil"; 
}

- (const char *)getTypeName
{
  id type = [self getType];
  
  return (type
          ? [type getName]
          : swarm_directory_language_independent_class_name_for_objc_object (self));
}

//
// _obj_formatIDString() --
//   function to generate object id string in standard format
//
void
_obj_formatIDString (char *buffer, id anObject)
{
  sprintf (buffer, PTRHEXFMT ": %.64s",
           anObject, swarm_class_getName(swarm_object_getClass(anObject)));
}

//
// describeID: -- concatenate a standard object ID string to an output stream
//
// This method should not be overridden in subclasses, so that the basic
// describeID: method remains available for use in describe: methods.
//
- (void)describeID: outputCharStream
{
  char  buffer[100];
  const char *displayName;

  _obj_formatIDString (buffer, self);
  [outputCharStream catC: buffer];

  displayName = [self getDisplayName];
  if (displayName)
    {
      [outputCharStream catC: ", display name: "];
      [outputCharStream catC: displayName];
    }
  [outputCharStream catC: "\n"];
}

//
// describe: -- concatenate a description of an object to an output stream
//
// This method can overridden as desired in subclasses, to provide as
// specific a description of the object as desired for debug purposes.
//
- (void)describe: outputCharStream
{
  [self describeID: outputCharStream];
}

//
// initDescribeStream() --
//   internal function to initialize output file stream on which debug
//   descriptions to be printed
//
static void
initDescribeStream (void)
{
  describeStream = [OutputStream createBegin: _obj_sessionZone];
  [describeStream setFileStream: _obj_xdebug];
  describeStream = [describeStream createEnd];
}

//
// xprint -- print description of object on debug output stream
//
- (void)xprint
{
  if (!describeStream)
    initDescribeStream ();
  [self describe: describeStream];
}

//
// xprintid -- print only the id string for an object on debug output stream
//
- (void)xprintid
{
  if (!describeStream)
    initDescribeStream();
  [self describeID: describeStream];
}

//
// xfprint --
//   print description for each member of a collection on debug output stream
//
- (void)xfprint
{
  if (!describeStream)
    initDescribeStream ();
  if (!respondsTo (self, M(describeForEach:)))
    {
      [describeStream
        catC:
          "xfprint: object does not respond to describeForEach:\n"
        "> object is: "];
      [self describeID: describeStream];
      return;
    }
  [(id) self describeForEach: describeStream];
}


- (void)lispOutVars: stream deep: (BOOL)deepFlag
{
  void store_object (const char *name, fcall_type_t type,
                     void *ptr, unsigned rank, unsigned *dims)
    {
      [stream catSeparator];
      [stream catKeyword: name];
      [stream catSeparator];
      if (rank > 0)
        lisp_process_array (rank, dims, type, ptr,
                            NULL, stream, deepFlag);
      else
        lisp_output_type (type,
                          ptr,
                          0,
                          NULL,
                          stream,
                          deepFlag);
        
    }
  map_object_ivars (self, store_object);
}

// Sometimes the desired effect is not found by saving all variables
// deep or shallow. One can individually save variables in lisp format
// with these methods.


- (void)lispSaveStream: stream Boolean: (const char *)aName Value: (int)val
{
  [stream catSeparator];
  [stream catKeyword: aName ];
  [stream catSeparator];
  [stream catBoolean: val];
}


- (void)lispSaveStream: stream Char: (const char*)aName Value: (char)val
{
  [stream catSeparator];
  [stream catKeyword: aName ];
  [stream catSeparator];
  [stream catChar: val];
}


- (void)lispSaveStream: stream Short: (const char*)aName Value: (short)val
{
  [stream catSeparator];
  [stream catKeyword: aName ];
  [stream catSeparator];
  [stream catShort: val];
}


- (void)lispSaveStream: stream UnsignedShort: (const char*)aName Value: (unsigned short)val
{
  [stream catSeparator];
  [stream catKeyword: aName ];
  [stream catSeparator];
  [stream catUnsignedShort: val];
}


- (void)lispSaveStream: stream Integer: (const char*)aName Value: (int)val
{
  [stream catSeparator];
  [stream catKeyword: aName ];
  [stream catSeparator];
  [stream catInt: val];
}


- (void)lispSaveStream: stream Unsigned: (const char*) aName Value: (unsigned int)val
{
  [stream catSeparator];
  [stream catKeyword: aName ];
  [stream catSeparator];
  [stream catUnsigned: val];
}



- (void)lispSaveStream: stream Long: (const char*)aName Value: (long int)val
{
  [stream catSeparator];
  [stream catKeyword: aName ];
  [stream catSeparator];
  [stream catLong: val];
}


- (void)lispSaveStream: stream UnsignedLong: (const char*)aName Value: (unsigned long int)val
{
  [stream catSeparator];
  [stream catKeyword: aName ];
  [stream catSeparator];
  [stream catUnsignedLong: val];
}

- (void)lispSaveStream: stream LongLong: (const char*)aName Value: (long long int)val
{
  [stream catSeparator];
  [stream catKeyword: aName ];
  [stream catSeparator];
  [stream catLongLong: val];
}


- (void)lispSaveStream: stream UnsignedLongLong: (const char*) aName Value: (unsigned long long int)val
{
  [stream catSeparator];
  [stream catKeyword: aName ];
  [stream catSeparator];
  [stream catUnsignedLongLong: val];
}



- (void)lispSaveStream: stream Float: (const char*) aName Value: (double)val
{
  [stream catSeparator];
  [stream catKeyword: aName ];
  [stream catSeparator];
  [stream catFloat: val];
}


- (void)lispSaveStream: stream Double: (const char*) aName Value: (double)val
{
  [stream catSeparator];
  [stream catKeyword: aName ];
  [stream catSeparator];
  [stream catDouble: val];
}





// Go in order through fcall_type_* types in internal.m and and enable
// storage of arrays of each

- (void)lispStoreBooleanArray: (BOOL *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream 
{
  [stream catSeparator];
  [stream catKeyword: keyword];
  [stream catSeparator];
  lisp_process_array (rank, dims,fcall_type_boolean,ptr,NULL,stream,NO);
}

- (void)lispStoreCharArray: (char *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream
{
  [stream catSeparator];
  [stream catKeyword: keyword];
  [stream catSeparator];
  lisp_process_array (rank, dims,fcall_type_schar,ptr,NULL,stream,NO);
}

//I'm not doing uchar. Don't understand it.

- (void)lispStoreShortArray: (short int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream
{
  [stream catSeparator];
  [stream catKeyword: keyword];
  [stream catSeparator];
  lisp_process_array (rank, dims,fcall_type_sshort,ptr,NULL,stream,NO);
}

- (void)lispStoreIntegerArray: (int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream
{
  [stream catSeparator];
  [stream catKeyword: keyword];
  [stream catSeparator];
  lisp_process_array (rank, dims,fcall_type_sint,ptr,NULL,stream,NO);
}

- (void)lispStoreUnsignedArray: (unsigned int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream
{
  [stream catSeparator];
  [stream catKeyword: keyword];
  [stream catSeparator];
  lisp_process_array (rank, dims,fcall_type_uint,ptr,NULL,stream,NO);
}

- (void)lispStoreLongArray: (long int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream
{
  [stream catSeparator];
  [stream catKeyword: keyword];
  [stream catSeparator];
  lisp_process_array (rank, dims,fcall_type_slong,ptr,NULL,stream,NO);
}

- (void)lispStoreUnsignedLongArray: (unsigned long int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream
{
  [stream catSeparator];
  [stream catKeyword: keyword];
  [stream catSeparator];
  lisp_process_array (rank, dims,fcall_type_ulong,ptr,NULL,stream,NO);
}

- (void)lispStoreLongLongArray: (long long int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream
{
  [stream catSeparator];
  [stream catKeyword: keyword];
  [stream catSeparator];
  lisp_process_array (rank, dims,fcall_type_slonglong,ptr,NULL,stream,NO);
}

- (void)lispStoreUnsignedLongLongArray: (unsigned long long int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream
{
  [stream catSeparator];
  [stream catKeyword: keyword];
  [stream catSeparator];
  lisp_process_array (rank, dims,fcall_type_ulonglong,ptr,NULL,stream,NO);
}

- (void)lispStoreFloatArray: (float *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream
{
  [stream catSeparator];
  [stream catKeyword: keyword];
  [stream catSeparator];
  lisp_process_array (rank, dims,fcall_type_float,ptr,NULL,stream,NO);
}

- (void)lispStoreDoubleArray: (double *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream
{
  [stream catSeparator];
  [stream catKeyword: keyword];
  [stream catSeparator];
  lisp_process_array (rank, dims,fcall_type_double,ptr,NULL,stream,NO);
}

- (void)lispStoreLongDoubleArray: (long double *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream
{
  [stream catSeparator];
  [stream catKeyword: keyword];
  [stream catSeparator];
  lisp_process_array (rank, dims,fcall_type_long_double,ptr,NULL,stream,NO);
}


- (void)_lispOut_: stream deep: (BOOL)deepFlag
{
  [stream catStartMakeInstance: [self getTypeName]];
  [stream catSeparator];
  [self lispOutVars: stream deep: deepFlag];
  [stream catEndMakeInstance];
}

- (void)lispOutShallow: stream
{
  [self _lispOut_: stream deep: NO];
}

- (void)lispOutDeep: stream
{
  [self _lispOut_: stream deep: YES];
}

- (void)hdf5OutDeep: hdf5Obj
{
  void store_object (const char *name,
                     fcall_type_t type,
                     void *ptr,
                     unsigned rank,
                     unsigned *dims)
    {
      if (type == fcall_type_object)
        {
          id obj = *((id *) ptr);
          
          if (obj != nil)
            {
              id group = [[[HDF5 createBegin: [hdf5Obj getZone]]
                              setWriteFlag: YES]
                             setParent: hdf5Obj];
			  group = [(HDF5_c *)group setName: name];
			  group = [group createEnd];
              
              [obj hdf5OutDeep: group];
              [group drop];
            }
        }
      else
        [hdf5Obj storeAsDataset: name typeName: NULL
                 type: type rank: rank dims: dims 
                 ptr: ptr];
    }
  [hdf5Obj storeTypeName: [self getTypeName]];
  map_object_ivars (self, store_object);
}

- (void)hdf5OutShallow: hdf5Obj
{
  if ([hdf5Obj getCompoundType])
    [hdf5Obj shallowStoreObject: self];
  else
    {
      id cType = [HDF5CompoundType createBegin: getZone (self)];
	  cType = [(HDF5CompoundType_c *)cType setPrototype: self];
	  cType = [cType createEnd];
      const char *objName = [hdf5Obj getHDF5Name];

      id cDataset = [HDF5 createBegin: getZone (self)];
	  cDataset = [(HDF5_c *)cDataset setName: objName];
	  cDataset = [[[[cDataset setWriteFlag: YES]
                        setParent: hdf5Obj]
                       setCompoundType: cType]
                      createEnd];
      
      [cDataset storeTypeName: [self getTypeName]];
      [cDataset shallowStoreObject: self];
      
      // for R
      [cDataset nameRecord: 0 name: objName];
      [cDataset writeRowNames];
      [cDataset writeLevels];
      
      [cDataset drop];
      [cType drop];
    }
}

- (void)updateArchiver: archiver
{
#ifdef HAVE_JDK
  jobject jobj = SD_JAVA_FIND_OBJECT_JAVA (self);

  if (jobj)
    {
      id fa = [FArguments createBegin: getCZone (scratchZone)];
      id fc;

      [fa setLanguage: LanguageJava];
      
      [fa addObject: archiver];
      [fa setObjCReturnType: _C_VOID];
      fa = [fa createEnd];
      
      fc = [FCall createBegin: getCZone (scratchZone)];
      [fc setArguments: fa];
      [fc setJavaMethodFromName: "updateArchiver" inObject: jobj];
      fc = [fc createEnd];
      [fc performCall];
      [fc drop];
      [fa drop];
    }
#else
  raiseEvent (SubclassMustImplement, "updateArchiver");
#endif
}

//
// xfprintid -- print id for each member of a collection on debug output stream
//
- (void)xfprintid
{
  if (!describeStream)
    initDescribeStream ();
  if (!respondsTo (self, M(describeForEachID:)))
    {
      [describeStream
        catC:
          "xfprintid: object does not respond to describeForEachID:\n"
        "> object is: "];
      [self describeID: describeStream];
      return;
    }
  [(id) self describeForEachID: describeStream];
}

@end

//
// respondsTo() -- function to test if object responds to message  
//
BOOL
respondsTo (id anObject, SEL aSel)
{
#if SWARM_OBJC_DONE
  return sarray_get (getClass (anObject)->dtable, (size_t) aSel->sel_id) != 0;
#else
  return swarm_class_respondsToSelector(swarm_object_getClass(anObject), (ObjcSEL *)aSel);
#endif
}

//
// getMethodFor() --
//   function to look up the method that implements a message within a class
//
IMP
getMethodFor (Class aClass, SEL aSel)
{
#if SWARM_OBJC_DONE
  return sarray_get (aClass->dtable, (size_t) aSel->sel_id);
#else
  return swarm_class_getMethodImplementation(aClass, aSel);
#endif
}

//
// xsetname() -- debug function to set display name for an object
//
void
xsetname (id anObject, const char *displayName)
{
  if (anObject)
    {
      if (respondsTo (anObject, M(setDisplayName:)))
        [anObject setDisplayName: displayName];
      else
        fprintf (_obj_xdebug,
                 "xsetname: object "
                 PTRHEXFMT 
                 "does not respond to setDisplayName:\n",
                 anObject);
    }
  else
    fprintf (_obj_xdebug, "xsetname: object is nil\n");
}

//
// xprint() -- debug function to print the debug description for an object
//
void 
xprint (id anObject)
{
  if (anObject)
    [anObject xprint];
  else
    fprintf (_obj_xdebug, "xprint: object is nil\n");
}

//
// xprintid() -- debug function to print the id string for an object
//
void
xprintid (id anObject)
{
  if (anObject)
    [anObject xprintid];
  else
    fprintf (_obj_xdebug, "xprintid: object is nil\n");
}

//
// xfprint() --
//   debug function to print the debug description for each member of a
//   collection
//
void
xfprint (id anObject)
{
  if (anObject)
    [anObject xfprint];
  else
    fprintf (_obj_xdebug, "xfprint: object is nil\n");
}

//
// xfprintid() --
//   debug function to print the id string for each member of a collection
//
void
xfprintid (id anObject)
{
  if (anObject)
    [anObject xfprintid];
  else
    fprintf (_obj_xdebug, "xfprintid: object is nil\n");
}

//
// xexec() -- debug function to perform message on an object
//
void
xexec (id anObject, const char *msgName)
{
  SEL  sel;
  
  if (anObject)
    {
      sel = swarm_sel_getUid (msgName);
      if (sel)
        {
          if ([anObject respondsTo: sel])
            [anObject perform: sel];
          else
            fprintf (_obj_xdebug,
                     "Object " PTRHEXFMT
                     ": %.64s does not respond to message %s\n",
                     anObject, [[anObject getClass] getName], msgName);
        }
      else
        fprintf (_obj_xdebug, "message \"%s\" is not defined\n", msgName);
    }
  else
    fprintf (_obj_xdebug, "object is nil");
}

//
// xfexec() -- debug function to perform message on each member of a collection
//
void
xfexec (id anObject, const char *msgName)
{
  id  index, member;
  
  if (anObject)
    {
      if (!respondsTo (anObject, M(begin:)))
        fprintf (_obj_xdebug,
                 "object " PTRHEXFMT ": %s does not respond to begin:\n"
                 "(begin: is required by xfexec to enumerate the members of a collection)\n",
                 anObject, swarm_class_getName(swarm_object_getClass(anObject)));
      else
        {
          index = [anObject begin: scratchZone];
          for (member = [index next];
               [index getLoc] == Member;
               member = [index next])
            {
              xexec (member, msgName);
              anObject = nil;
            }
          if (anObject)
            fprintf (_obj_xdebug, "collection has no members\n");
          [index drop];
        }
    }
  else
    fprintf (_obj_xdebug, "object is nil");
}

