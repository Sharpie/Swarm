// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
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
#import <defobj/defalloc.h>
#import <collections.h>
#import <collections/Map.h>  //!! for at:memberSlot (until replaced)
#import <defobj/HDF5Object.h>
#import <defobj/internal.h> // process_array, map_ivars

#import <objc/objc-api.h>
#import <objc/sarray.h>

#include <misc.h> // strcpy, strlen, isprint, sprintf
#include <collections/predicates.h> // arrayp, keywordp, listp, stringp

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

- hdf5InCreate: expr
{
  return self;
}

PHASE(Using)

//
// getName -- return name of class
//
+ (const char *)getName
{
  return (const char *) ((Class) self)->name;
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
  if (getBit (((Class) self)->info, _CLS_DEFINEDCLASS))
    return [((CreatedClass_s *) self)->definingClass
                                     conformsTo: protocol];
  else
    return [super conformsTo: protocol];
}

//
// getClass -- get class object that implements behavior of object
//
+ getClass
{
  return getClass (self);
}

- getClass
{
  return getClass (self);
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
      if (!suballocEntry)
        {
          [index drop];
          setBit (zbits, BitSuballocList, 0);
          [zone freeBlock: suballocList
                blockSize: getClass (suballocList)->instance_size];
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
            blockSize: getClass (suballocList)->instance_size];
    }
  
  // free the local instance variables for the object
  
  if (getBit (zbits, BitComponentAlloc))
    [zone freeIVarsComponent: self];
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
  
  // execute internal drop method directly, to avoid any version of drop
  // method in subclasses (temporary hack until this whole method eliminated)
  
  _i_Object_s__drop (self, M(drop));
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
        [zone allocBlock: getClass (suballocPrototype)->instance_size];
      memcpy (suballocList,
              suballocPrototype,
              getClass (suballocPrototype)->instance_size);
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
  return ((Class) self)->super_class;
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
      if (!superclass->super_class)
        return NO;
      superclass = superclass->super_class;
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
                ((Class) self)->name,
                [classData->owner getName],
                [_obj_implModule getName]);
  
  if (classData->typeImplemented
      && *(id *) classData->typeImplemented != self)
    raiseEvent (SourceMessage,
                "> setTypeImplemented: class %s, requested to implement the type %s,\n"
                "> has already been specified as the implementation of type %s\n",
                ((Class) self)->name, [aType getName],
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
  return sarray_get (((Class) self)->dtable, (size_t) aSel->sel_id);
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
  return _obj_getClassData (*(Class_s **) self)->typeImplemented;
}

//
// compare: -- default compare: method that compares addresses of two objects
//
- (int)compare: anObject
{
  if (self < anObject)
    return -1;
  return (self > anObject);
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

  mptr = objc_msg_lookup (self, aSel);
  if (!mptr)
    raiseEvent (InvalidArgument, "> message selector not valid\n");
  return mptr (self, aSel);
}

- perform: (SEL)aSel with: anObject1
{
  IMP  mptr;
  
  mptr = objc_msg_lookup (self, aSel);
  if (!mptr)
    raiseEvent (InvalidArgument, "> message selector not valid\n");
  return mptr (self, aSel, anObject1);
}

- perform: (SEL)aSel with: anObject1 with: anObject2
{
  IMP  mptr;
  
  mptr = objc_msg_lookup (self, aSel);
  if (!mptr)
    raiseEvent (InvalidArgument, "> message selector not valid\n");
  return mptr (self, aSel, anObject1, anObject2);
}

- perform: (SEL)aSel with: anObject1 with: anObject2 with: anObject3
{
  IMP  mptr;
  
  mptr = objc_msg_lookup (self, aSel);
  if (!mptr)
    raiseEvent (InvalidArgument, "> message selector not valid\n");
  return mptr (self, aSel, anObject1, anObject2, anObject3);
}

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
  raiseEvent (InvalidArgument, "%s does not recognize %s",
              [self name], sel_get_name (sel));
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
      sprintf (buffer, PTRFMT ": %.64s",
               self, getClass (self)->name);
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
  if (!_obj_displayNameMap)
    return NULL;
  
  return (const char *) [_obj_displayNameMap at: self];
}

- (const char *)getIdName
{
  return [self name];
}

#define ATDELIMCHAR '@'
- (const char *)getObjectName
{
  static char name[512];
  
  if (self)
    {
#ifdef POINTER_FMT_HEX_PREFIX
      sprintf (name, "%s%c%p", [self name], ATDELIMCHAR, self);
#else
      sprintf (name, "%s%c0x%p", [self name], ATDELIMCHAR, self);
#endif
      return name;
    }
  return "nil"; 
}

- (const char *)getTypeName
{
  id type = [self getType];
  
  return type ? [type getName] : [self name];
}

//
// _obj_formatIDString() --
//   function to generate object id string in standard format
//
void
_obj_formatIDString (char *buffer, id anObject)
{
  sprintf (buffer, PTRFMT ": %.64s",
           anObject, getClass (anObject)->name);
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

static void
lisp_output_type (const char *type,
                  const void *ptr,
                  unsigned offset,
                  void *data,
                  id <OutputStream> stream,
                  BOOL deepFlag);

static void
lisp_process_array (const char *type,
                    const void *ptr, void *data,
                    id <OutputStream> stream,
                    BOOL deepFlag)
{
  const char *space;
  
  void lisp_setup_array (unsigned rank, unsigned *dims, const char *baseType)
    {
      char buf[1 + rank + 1]; // always big enough
      
      sprintf (buf, "#%u", rank);
      [stream catC: buf];
    }
  void lisp_start_dim (unsigned dim)
    {
      [stream catC: "("];
      space = "";
    }
  void lisp_end_dim (void)
    {
      [stream catC: ")"];
    }
  void lisp_start_element (void)
    {
      [stream catC: space];
    }
  void lisp_end_element (void)
    {
      space = " ";
    }
  void lisp_array_output_type (const char *type,
                               unsigned offset,
                               void *data)
    {
      lisp_output_type (type, ptr, offset, data, stream, deepFlag);
    }
    
  process_array (type,
                 lisp_setup_array,
                 lisp_start_dim,
                 lisp_end_dim,
                 lisp_start_element,
                 lisp_end_element,
                 lisp_array_output_type,
                 ptr,
                 data);
}

static void
lisp_output_type (const char *type,
                  const void *ptr,
                  unsigned offset,
                  void *data,
                  id <OutputStream> stream,
                  BOOL deepFlag)
{
  char buf[22];  // 2^64

  switch (*type)
    {
    case _C_ID:
      {
        id obj = ((id *) ptr)[offset];

        if (obj == nil || !deepFlag)
          [stream catC: "nil"];
        else
          [obj lispOutDeep: stream];
        break;
      }
    case _C_CLASS:
      raiseEvent (NotImplemented, "Classes not supported [%s]", type);
      break;
    case _C_SEL:
      raiseEvent (NotImplemented, "Selectors not supported");
      break;
    case _C_CHR: 
    case _C_UCHR: 
      [stream catC: "#\\"];
      {
        unsigned char ch = ((unsigned char *) ptr)[offset];
        
        if (isprint (ch))
          {
            buf[0] = ch;
            buf[1] = '\0';
          }
        else
          sprintf (buf, "%03o", (unsigned) ch);
      }
      [stream catC: buf];
      break;
    case _C_SHT: 
      sprintf (buf, "%hd", ((short *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_USHT: 
      sprintf (buf, "%hu", ((unsigned short *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_INT:
      sprintf (buf, "%d", ((int *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_UINT:
      sprintf (buf, "%u", ((unsigned *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_LNG:
      sprintf (buf, "%ld", ((long *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_ULNG:
      sprintf (buf, "%lu", ((unsigned long *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_FLT:
      sprintf (buf, "%fF0", ((float *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_DBL:
      sprintf (buf, "%fD0", ((double *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_BFLD:
      raiseEvent (NotImplemented, "Bit fields not supported [%s]", type);
      break;
    case _C_VOID:
      abort ();
      break;
    case _C_UNDEF: 
      abort ();
      break;
    case _C_PTR:
      raiseEvent (NotImplemented, "Pointers not supported [%s]", type);
      break;
    case _C_CHARPTR:
      [stream catC: "\""];
      [stream catC: ((const char **) ptr)[offset]];
      [stream catC: "\""];
      break;
    case _C_ATOM:
      raiseEvent (NotImplemented, "Atoms not supported");
      break;
    case _C_ARY_B:
      lisp_process_array (type, ptr, data, stream, deepFlag);
      break;
    case _C_ARY_E:
      abort ();
      break;
    case _C_UNION_B:
      raiseEvent (NotImplemented, "Unions not supported [%s]", type);
      break;
    case _C_UNION_E:
      abort ();
      break;
    case _C_STRUCT_B:
      raiseEvent (NotImplemented, "Structures not supported [%s]", type);
      break;
    case _C_STRUCT_E:
      abort ();
      break;
    default:
      abort ();
      break;
    }
}

- _lispOut_: stream deep: (BOOL)deepFlag
{
  [stream catC: "(" MAKE_INSTANCE_FUNCTION_NAME " '"];
  [stream catC: [self getTypeName]];
  {
    void store_object (struct objc_ivar *ivar)
      {
        [stream catC: " #:"];
        [stream catC: ivar->ivar_name];
        [stream catC: " "];
        lisp_output_type (ivar->ivar_type,
                          (void *) self + ivar->ivar_offset,
                          0,
                          NULL,
                          stream,
                          deepFlag);
      }
    map_ivars (getClass (self)->ivars, store_object);
  }
  [stream catC: ")"];
  return self;
}

- lispOutShallow: stream
{
  return [self _lispOut_: stream deep: NO];
}

- lispOutDeep: stream
{
  return [self _lispOut_: stream deep: YES];
}

- hdf5OutDeep: hdf5Obj
{
  void store_object (struct objc_ivar *ivar)
    {
      const char *name = ivar->ivar_name;
      const char *type = ivar->ivar_type;
      void *ptr = (void *)self + ivar->ivar_offset;
      
      if (*type == _C_ID)
        {
          id obj = *((id *) ptr);
          
          if (obj != nil)
            {
              id group = [[[[[HDF5 createBegin: [hdf5Obj getZone]]
                              setCreateFlag: YES]
                             setParent: hdf5Obj]
                            setName: name]
                           createEnd];
              
              [obj hdf5OutDeep: group];
              [group drop];
            }
        }
      else
        [hdf5Obj storeAsDataset: name typeName: NULL type: type ptr: ptr];
    }
  [hdf5Obj storeTypeName: [self getTypeName]];
  map_ivars (getClass (self)->ivars, store_object);
  return self;
}

- hdf5OutShallow: hdf5Obj
{
  if ([hdf5Obj getCompoundType])
    [hdf5Obj shallowStoreObject: self];
  else
    {
      id aZone = [self getZone];
      id cType = [[[HDF5CompoundType createBegin: aZone]
                    setClass: [self class]]
                   createEnd];
      const char *objName = [hdf5Obj getName];

      id cDataset = [[[[[[HDF5 createBegin: aZone]
                          setName: objName]
                         setCreateFlag: YES]
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
  return self;
}

- updateArchiver
{
  raiseEvent (SubclassMustImplement, "updateArchiver");
  return self;
}

- lispIn: expr
{
  id <Index> li = [expr begin: [expr getZone]];
  id key, val;

  while ((key = [li next]) != nil)
    {
      const char *ivarname;
      struct objc_ivar *ivar;
      void *ptr;

      if (!keywordp (key))
        raiseEvent (InvalidArgument, "expecting keyword [%s]", [key name]);

      if ((val = [li next]) == nil)
        raiseEvent (InvalidArgument, "missing value");
      
      ivarname = [key getKeywordName];
      ivar = find_ivar (self, ivarname);
      
      if (ivar == NULL)
        raiseEvent (InvalidArgument, "could not find ivar `%s'", ivarname);
      
      ptr = (void *) self + ivar->ivar_offset;

      if (arrayp (val))
        memcpy (ptr, [val getData], 
                [val getElementCount] * [val getElementSize]);
      else if (valuep (val))
        {
          char ntype = [val getValueType];

          switch (ntype)
            {
            case _C_ID:
              *((id *) ptr) = [val getObject];
              break;
            case _C_DBL:
              *((double *) ptr) = [val getDouble];
              break;
            case _C_FLT:
              *((float *) ptr) = [val getFloat];
              break;
            case _C_INT:
              {
                char itype = *ivar->ivar_type;
                int ival = [val getInteger];

                if (itype == _C_INT || itype == _C_UINT)
                  *((int *) ptr) = ival;
                else if (itype == _C_SHT || itype == _C_USHT)
                  *((short *) ptr) = ival;
                else if (itype == _C_LNG || itype == _C_ULNG)
                  *((long *) ptr) = ival;
                else
                  abort ();
              }
            case _C_UCHR:
              *((unsigned char *) ptr) = [val getChar];
              break;
            default:
              raiseEvent (InvalidArgument, "Unknown value type `%c'", ntype);
              break;
            }
          }
      else if (stringp (val))
        *((const char **) ptr) = strdup ([val getC]);
      else if (listp (val))
        {
          id first = [val getFirst];

          if (stringp (first))
            {
              const char *funcName = [first getC];

              if (strcmp (funcName, MAKE_INSTANCE_FUNCTION_NAME) == 0
                  || strcmp (funcName, MAKE_CLASS_FUNCTION_NAME) == 0) 
                *((id *) ptr) = lispIn ([self getZone], val);
              else
                raiseEvent (InvalidArgument, "function not %s",
                            MAKE_INSTANCE_FUNCTION_NAME
                            " or "
                            MAKE_CLASS_FUNCTION_NAME);
            }
          else
            raiseEvent (InvalidArgument, "argument not a string");
        }
      else
        raiseEvent (InvalidArgument, "Unknown type `%s'", [val name]);
    }
  return self;
}

- hdf5In: hdf5Obj
{
  if ([hdf5Obj getDatasetFlag])
    [hdf5Obj shallowLoadObject: self];
  else
    {
      int process_object (id component)
        {
          const char *ivarName = [component getName];
          void *ptr = ivar_ptr (self, ivarName);

          if (ptr == NULL)
            raiseEvent (InvalidArgument,
                        "could not find ivar `%s'", ivarName);

          if ([component getDatasetFlag])
            [component loadDataset: ptr];
          else
            *(id *) ptr = hdf5In ([self getZone], component);
          return 0;
        }
      [hdf5Obj iterate: process_object];
    }
  return self;
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
  return sarray_get (getClass (anObject)->dtable, (size_t) aSel->sel_id) != 0;
}

//
// getMethodFor() --
//   function to look up the method that implements a message within a class
//
IMP
getMethodFor (Class aClass, SEL aSel)
{
  return sarray_get (aClass->dtable, (size_t) aSel->sel_id);
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
                 PTRFMT 
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
      sel = sel_get_any_uid (msgName);
      if (sel)
        {
          if ([anObject respondsTo: sel])
            [anObject perform: sel];
          else
            fprintf (_obj_xdebug,
                     "Object " PTRFMT
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
                 "object " PTRFMT ": %s does not respond to begin:\n"
                 "(begin: is required by xfexec to enumerate the members of a collection)\n",
                 anObject, getClass (anObject)->name);
      else
        {
          index = [anObject begin: scratchZone];
          while ((member = [index next]))
            {
              xexec (member, msgName);
              anObject = nil;
            }
          if (anObject)
            fprintf (_obj_xdebug, "collection has no members\n");
        }
    }
  else
    fprintf (_obj_xdebug, "object is nil");
}

