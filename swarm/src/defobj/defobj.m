// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         defobj.m
Description:  global data and functions for Swarm kernel
Library:      defobj
*/

#import <defobj.h>

#include "defobj.xm"
#import <defobj/HDF5Object.h>

#include <objc/objc-api.h> // objc_lookup_class, _objc_lookup_class
#include <misc.h> // strcmp, sscanf
#include <collections/predicates.h> // keywordp, archiver_list_p, stringp

#import "internal.h" // class_generate_name
#include <swarmconfig.h> // HAVE_HDF5, HAVE_JDK

#import <defobj/directory.h> // swarm_directory_ensure_class_named
#ifdef HAVE_JDK
#import <defobj/JavaProxy.h> // -createJavaCounterpart
#endif


BOOL _warning_dropFrom = YES;

externvardef FILE *_obj_xerror, *_obj_xdebug;
externvardef BOOL _obj_debug = YES;

Class *localClasses;
unsigned localClassCount = 0;

externvardef id lispArchiver;
externvardef id lispAppArchiver;
externvardef id hdf5AppArchiver;
externvardef id hdf5Archiver;

//
// _defobj_implement() -- generate implementations for defobj module
//
void
_defobj_implement (void)
{
  [id_Zone_c setTypeImplemented: Zone];
  [id_Symbol_c setTypeImplemented: Symbol];
  [id_Warning_c setTypeImplemented: Warning];
  [id_Error_c setTypeImplemented: Error];
  [id_Arguments_c setTypeImplemented: Arguments];
  [id_LispArchiver_c setTypeImplemented: LispArchiver];
  [id_HDF5Archiver_c setTypeImplemented: HDF5Archiver];
  [id_HDF5_c setTypeImplemented: HDF5];
  [id_HDF5CompoundType_c setTypeImplemented: HDF5CompoundType];
  [id_FCall_c setTypeImplemented: FCall];
  [id_FArguments_c setTypeImplemented: FArguments];
}

//
// _defobj_initialize() -- initialize global data for defobj module
//
void
_defobj_initialize (void)
{
  // initialize error messages

  [InvalidCombination setMessageString:
"> Customization messages sent to a type are incompatible with each other\n"
"> or with other requirements of the type.\n" ];

  [InvalidArgument setMessageString:
"> Invalid argument value passed in call.\n"];

  [OutOfMemory setMessageString:
"> No more memory available from the system.  Value of sbrk: %0#8x\n"];

  [InvalidAllocSize setMessageString:
"> Requested allocation size must be at least one byte.\n"
"> (Requested allocation size was zero.)\n"];

  [BlockedObjectAlloc setMessageString:
"> Requested operation is defined by the Object superclass of the GNU\n"
"> Objective C runtime system, but is blocked from usage because its default\n"
"> implementation is incompatible with the model of zone-based allocation\n"
"> established by the defobj package.  To allocate, free, or copy objects\n"
"> that use the defined model of zone-based allocation, one of the messages\n"
"> create:, createBegin:/End, drop, or copy: must be used instead.\n"];

  [BlockedObjectUsage setMessageString:
"> Requested operation is implemented by the Object superclass of the GNU\n"
"> Objective C runtime system, but is blocked from usage because it is not\n"
"> part of the standard public view established by the defobj package.\n"
"> See documentation for an explanation of the supported public view,\n"
"> including functional equivalents for most messages defined by the Object\n"
"> superclass.  This error may be avoided by compiling the defobj library\n"
"> without the -DINHERIT_OBJECT_WITH_ERRORS compile-time flag set.\n"];

  [ProtocolViolation setMessageString:
"> This object does not comply with an expected protocol\n"];

}

static void
registerLocalClass (Class class)
{
  if (localClassCount == 0)
    localClasses = xmalloc (sizeof (Class));
  else
    localClasses = xrealloc (localClasses, localClassCount + 1);
  localClasses[localClassCount++] = class;
}

static Class
findLocalClass (const char *name)
{
  unsigned i;

  for (i = 0; i < localClassCount; i++)
    {
      if (strcmp (localClasses[i]->name, name) == 0)
        return localClasses[i];
    }
  return Nil;
}

static Class
findTypeOrLocalClass (const char *name)
{
  Class class = defobj_lookup_type (name);

  if (class == Nil)
    class = findLocalClass (name);

  return class;
}

void
initDefobj (int argc, const char **argv,
            const char *appName,
            const char *version,
            const char *bugAddress,
            Class argumentsClass,
            struct argp_option *options,
            int (*optionFunc) (int key, const char *arg),
            BOOL inhibitExecutableSearchFlag)
{
  arguments = [argumentsClass ?: [Arguments_c class]
                              createArgc: argc
                              Argv: argv
                              appName: appName
                              version: version
                              bugAddress: bugAddress
                              options: options
                              optionFunc: optionFunc
                              inhibitExecutableSearchFlag:
                                inhibitExecutableSearchFlag];
  _objc_lookup_class = findTypeOrLocalClass;
  {
    BOOL inhibitLoadFlag =
      ([arguments getInhibitArchiverLoadFlag] |
       (getenv ("SWARM_INHIBIT_ARCHIVER_LOAD") != NULL));

#ifdef HAVE_HDF5
    hdf5Archiver = [[[[[HDF5Archiver createBegin: globalZone]
                        setDefaultPath]
                       setSystemArchiverFlag: YES]
                      setInhibitLoadFlag: inhibitLoadFlag]
                     createEnd];
    hdf5AppArchiver = [[[[HDF5Archiver createBegin: globalZone]
                          setDefaultAppPath]
                         setInhibitLoadFlag: inhibitLoadFlag]
                        createEnd];
#else
    hdf5Archiver = nil;
    hdf5AppArchiver = nil;
#endif
    lispArchiver = [[[[[LispArchiver createBegin: globalZone]
                        setDefaultPath]
                       setSystemArchiverFlag: YES]
                      setInhibitLoadFlag: inhibitLoadFlag]
                     createEnd];
    lispAppArchiver = [[[[LispArchiver createBegin: globalZone]
                          setDefaultAppPath]
                         setInhibitLoadFlag: inhibitLoadFlag]
                        createEnd];
  }
}

static id
collectRemaining (id makeExprIndex)
{
  id obj;
  id newList = [List create: [makeExprIndex getZone]];
  
  while ((obj = [makeExprIndex next]))
    [newList addLast: obj];
  
  return newList;
}

BOOL
lispInBoolean (id index)
{
  id val = [index next];
  
  if (!valuep (val))
    raiseEvent (InvalidArgument, "expected ArchiverValue");
  
  if ([val getValueType] != _C_UCHR)
    raiseEvent (InvalidArgument, "expected boolean ArchiverValue");
  
  return [val getBoolean];
}

int
lispInInteger (id index)
{
  id val = [index next];
  
  if (!valuep (val))
    raiseEvent (InvalidArgument, "expected ArchiverValue");
  
  return [val getInteger];
}

const char *
lispInString (id index)
{
  id val = [index next];

  if (!stringp (val))
    raiseEvent (InvalidArgument, "expected String");

  return [val getC];
}

id
lispInKeyword (id index)
{
  id val = [index next];

  if (!keywordp (val))
    raiseEvent (InvalidArgument, "expected ArchiverKeyword");
  
  return val;
}

id
lispIn (id aZone, id expr)
{
  if (!archiver_list_p (expr))
    raiseEvent (InvalidArgument, "> expr not an archiver list");
  {    
    id makeExprIndex = [expr begin: scratchZone];
    BOOL classFlag = NO;
    
    {
      id makeExprObj = [makeExprIndex next];
      
      if (!stringp (makeExprObj))
        raiseEvent (InvalidArgument, "> makeExprObj not a string");
      {
        const char *funcName = [makeExprObj getC];
        
        if (strcmp (funcName, MAKE_CLASS_FUNCTION_NAME) == 0)
          classFlag = YES;
        else if (strcmp (funcName, MAKE_INSTANCE_FUNCTION_NAME) != 0
                 && strcmp (funcName, "make-objc") != 0)
          raiseEvent (InvalidArgument, "> makeExprObj not \""
                      MAKE_INSTANCE_FUNCTION_NAME
                      "\" or \""
                      MAKE_CLASS_FUNCTION_NAME
                      "\" (%s)\n", funcName);
      }
    }
    
    {
      id typeNameString;
      id typeObject;
      id obj;
      
      typeNameString = [[makeExprIndex next] getQuotedObject];

      if (!stringp (typeNameString))
        raiseEvent (InvalidArgument, "> argument not a string");
      
      {
        id argexpr = collectRemaining (makeExprIndex);
        const char *typeName = [typeNameString getC];
        
        if (classFlag)
          {
            obj = type_create (aZone, typeName);
            obj = [obj lispInCreate: argexpr];
            obj = [obj createEnd];
            registerLocalClass (obj);
          }
        else
          {
            if (!(typeObject =
                  swarm_directory_ensure_class_named (typeName)))
              raiseEvent (InvalidArgument, "> type `%s' not found",
                          typeName);

#ifdef HAVE_JDK
            if (!object_is_class (typeObject)
                && [typeObject respondsTo: M(isJavaProxy)])
              {
                obj = [JavaProxy createBegin: aZone];
                [obj createJavaCounterpart: typeName];
              }
            else
#endif
              obj = [typeObject createBegin: aZone];
            
            obj = [obj lispInCreate: argexpr];
            obj = [obj createEnd];
            [obj lispIn: argexpr];
          }
        [argexpr drop];
      }
    [makeExprIndex drop];
      return obj;
    }
  }
}

id
hdf5In (id aZone, id hdf5Obj)
{
  id obj;
  id typeObject;
  const char *typeName = [hdf5Obj getAttribute: ATTRIB_TYPE_NAME];
  
  if (typeName)
    {
      if (!(typeObject = swarm_directory_ensure_class_named (typeName)))
        {
          id typeObj = type_create (aZone, typeName);
          id newTypeObj = [typeObj hdf5InCreate: hdf5Obj];
          
          newTypeObj = [newTypeObj createEnd];
          registerLocalClass (newTypeObj);
          typeObject = newTypeObj;
        }
    }
  else
    {
      if ([hdf5Obj getDatasetFlag] && [hdf5Obj getCount] > 1)
        typeObject = [List self];
      else
        {
          id typeObj;
          id newTypeObj;

          typeName = class_generate_name ();
          typeObj = type_create (aZone, typeName);
          newTypeObj = [typeObj hdf5InCreate: hdf5Obj];
          
          newTypeObj = [newTypeObj createEnd];
          registerLocalClass (newTypeObj);
          typeName = [newTypeObj name];
          typeObject = newTypeObj;
        }
    }
  if (typeObject == nil)
    raiseEvent (LoadError,
                "Failed to find or create class for HDF5 object `%s'",
                [hdf5Obj getName]);
  
#ifdef HAVE_JDK
  if (!object_is_class (typeObject)
      && [typeObject respondsTo: M(isJavaProxy)])
    {
      obj = [JavaProxy createBegin: aZone];
      [obj createJavaCounterpart: typeName];
    }
  else
#endif
    obj = [typeObject createBegin: aZone];
  obj = [obj hdf5InCreate: hdf5Obj];
  obj = [obj createEnd];
  [obj hdf5In: hdf5Obj];

  return obj;
}

id
nameToObject (const char *name)
{
  id object;
  void *val;
  const char *p = name;
  
  while (*p != '@' && *p != '\0')
    p++;
  if ((*p) && (sscanf (p + 3, "%p", &val) == 1))
    return (id) val;
  else if ((!strcmp (name, "nil"))
           || (!strcmp (name, "Nil"))
           || (!strcmp (name, "0x0")))
    return nil;
  else if ((object = (id) swarm_directory_ensure_class_named (name)))
    return object;
  abort ();
}
