// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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
#import <defobj/Archiver.h>

#include <objc/objc-api.h> // objc_lookup_class
#include <misc.h> // strcmp, sscanf
#include <collections/predicates.h> // keywordp, listp, stringp

id  t_Object, t_ByteArray;
BOOL _warning_dropFrom = YES;
BOOL _obj_debug = YES;
FILE *_obj_xerror, *_obj_xdebug;

Class *localClasses;
unsigned localClassCount = 0;

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
  [id_Archiver_c setTypeImplemented: Archiver];
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
    {
      localClassCount++;
      localClasses = xrealloc (localClasses, localClassCount + 1);
    }
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

void
initDefobj (int argc, const char **argv,
            const char *version,
            const char *bugAddress,
            Class argumentsClass,
            struct argp_option *options,
            int (*optionFunc) (int key, const char *arg))
{
  arguments = [argumentsClass ?: [Arguments_c class]
                              createArgc: argc
                              Argv: argv
                              version: version
                              bugAddress: bugAddress
                              options: options
                              optionFunc: optionFunc];
  _objc_lookup_class = findLocalClass;
  archiver = [Archiver create: globalZone];
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
  
  if ([val getValueType] != _C_INT)
    raiseEvent (InvalidArgument, "expected integer ArchiverValue");
  
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
  if (!listp (expr))
    raiseEvent (InvalidArgument, "> expr not a list");
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
        else if (strcmp (funcName, MAKE_INSTANCE_FUNCTION_NAME) != 0)
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
      
      typeNameString = [makeExprIndex next];
      if (!stringp (typeNameString))
        raiseEvent (InvalidArgument, "> argument not a string");
      
      {
        id argexpr = collectRemaining (makeExprIndex);
        const char *typeName = [typeNameString getC];
        
        if (classFlag)
          {
            Class newClass = [CreateDrop class];
            obj = [id_BehaviorPhase_s createBegin: aZone];

            [obj setName: strdup (typeName)];
            [obj setClass: getClass (newClass)];
            [obj setDefiningClass: newClass];
            [obj setSuperclass: newClass];
            obj = [obj lispInCreate: argexpr];
            [obj lispIn: argexpr];
            obj = [obj createEnd];
            registerLocalClass (obj);
          }
        else
          {
            if ((typeObject = defobj_lookup_type (typeName)) == Nil)
              if ((typeObject = objc_lookup_class (typeName)) == Nil)
                raiseEvent (InvalidArgument, "> type `%s' not found",
                            typeName);

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
  else if ((object = (id) objc_lookup_class (name)))
    return object;
  abort ();
}

#if ((__GNUC__ == 2) && (__GNUC_MINOR__ == 8)) && (__GNUC__ > 2)
id
nil_method (id receiver, SEL op, ...)
{
  [NotImplemented raiseEvent:  "The message `%s' was sent to nil.\n",
                  sel_get_name (op)];

  return nil;
}
#endif
