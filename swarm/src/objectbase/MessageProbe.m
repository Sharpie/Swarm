// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "MessageProbe.h"

#include <swarmconfig.h>

#ifdef USE_AVCALL
#include <avcall.h>
#else
#undef PACKAGE
#undef VERSION
#include <ffi.h>
#undef PACKAGE
#undef VERSION
#endif

#import "swarm_rts_routines.h"
#import <objectbase.h> // val_t
#include <misc.h>  // xmalloc, strdup, atoi, strtod

#import <defobj.h> // FCall, FArguments

#ifdef HAVE_JDK
#include <directory.h>
#endif

@implementation MessageProbe
PHASE(Creating)

+ createBegin: aZone
{
  MessageProbe *obj;

  obj = [super createBegin: aZone];
  obj->objectToNotify = nil;
  return obj;
}

- setProbedSelector: (SEL)aSel
{
  probedSelector = aSel;
  return self;
}

- createEnd
{
  [super createEnd];
  
  probedSelector = sel_get_any_typed_uid (sel_get_name (probedSelector));
  
  if (!probedSelector)
    {
      raiseEvent (WarningMessage, "Typed selector does not exist");
      [self drop]; 
      return nil;
    }

  if (!sel_get_type (probedSelector))
    {
      raiseEvent (WarningMessage, "Type for selector does not exist");
      [self drop]; 
      return nil;
    }
  
  probedType = strdup (sel_get_type (probedSelector));

  {
    int argCount = [self getArgCount];

    if (argCount > 0)
      {
        val_t empty_val;
        int i;
        
        empty_val.type = '\0';
        arguments = (val_t *)xmalloc (argCount * sizeof (val_t));
        
        for (i = 0; i < argCount; i++)
          arguments[i] = empty_val;
      }
    else
      arguments = NULL;
  }
  return self;
}

PHASE(Setting)
- setHideResult: (BOOL)theHideResultFlag
{
  hideResultFlag = theHideResultFlag;
  return self;
}

PHASE(Using)

- clone: aZone
{
  MessageProbe *new_probe;
  
  new_probe = [MessageProbe createBegin: aZone];
  [new_probe setProbedClass: probedClass];
  [new_probe setProbedSelector: probedSelector];
  if (objectToNotify != nil) 
    [new_probe setObjectToNotify: objectToNotify];
  new_probe = [new_probe createEnd];

  return new_probe;
}

static const char
nth_type (const char *type, int which)
{
  int i;

  type = skip_argspec (type);  // result 
  type = skip_argspec (type);  // object
  type = skip_argspec (type);  // selector  

  for (i = 0; i < which; i++)
    type = skip_argspec (type);
  
  return *type;
}

- (const char *)getProbedMessage
{
  return sel_get_name (probedSelector);
}

- (int)getArgCount
{
  return get_number_of_arguments (probedType) - 2;
}

- (val_t)getArg: (int)which
{
  return arguments[which];
}

- setArg: (int)which ToString: (const char *)what
{
  switch (nth_type (probedType, which))
    {
    case _C_ID:
      arguments[which].type = _C_ID;
      arguments[which].val.object = nameToObject (what);
      break;
    case _C_CHR:
      arguments[which].type = _C_CHR;
      arguments[which].val.schar = (char) atoi (what);
      break;
    case _C_UCHR:
      arguments[which].type = _C_UCHR;
      arguments[which].val.uchar = (unsigned char) strtoul (what, NULL, 10);
      break;
    case _C_SHT:
      arguments[which].type = _C_SHT;
      arguments[which].val.sshort = atoi (what);
      break;
    case _C_USHT:
      arguments[which].type = _C_USHT;
      arguments[which].val.ushort = (unsigned short) strtoul (what, NULL, 10);
      break;
    case _C_INT:
      arguments[which].type = _C_INT;
      arguments[which].val.sint = atoi (what);
      break;
    case _C_UINT:
      arguments[which].type = _C_UINT;
      arguments[which].val.uint = (unsigned int) strtoul (what, NULL, 10);
      break;
    case _C_LNG:
      arguments[which].type = _C_LNG;
      arguments[which].val.slong = strtol (what, NULL, 10);
      break;
    case _C_ULNG:
      arguments[which].type = _C_ULNG;
      arguments[which].val.ulong = strtoul (what, NULL, 10);
      break;
    case _C_FLT:
      arguments[which].type = _C_FLT;
      arguments[which].val._float = strtod (what, NULL);
      break;
    case _C_DBL:
      arguments[which].type = _C_DBL;
      arguments[which].val._double = strtod (what, NULL);
      break;
    case _C_CHARPTR:
      arguments[which].type = _C_CHARPTR;
      arguments[which].val.string = strdup (what);
      break;
    default:
      abort ();
    }
  return self;
}

static const char *
copy_to_nth_colon (const char *str, int n)
{
  int count = -1;
  int beginning,end,i;
  char *new_str;

  for (i = 0; i < n; i++)
    while (str[++count] != ':');

  count++;

  beginning = count;

  while (str[count] != ':')
    count++;

  count++;

  end = count;

  new_str = xmalloc ((end - beginning) + 1);

  count = 0;
  for (i = beginning; i < end; i++)
    new_str[count++] = str[i];
  new_str[count] = '\0';

  return new_str;
} 

- (const char *)getArgName: (int)which
{
  return copy_to_nth_colon (sel_get_name (probedSelector), which);
}

- (BOOL)isResultId
{
  return (probedType[0] == _C_ID);
}

- (BOOL)isArgumentId: (int)which
{
  return (nth_type (probedType, which) == _C_ID);
}

#ifdef USE_AVCALL
static void
dynamicCallOn (const char *probedType,
               id target,
               SEL probedSelector, 
               val_t *arguments,
               val_t *retVal)
{
  const char *type = probedType;
  IMP imp;
  val_t objectVal, selectorVal;
  unsigned i;

  av_alist alist;

  void push_argument (val_t *arg)
    {
      switch (arg->type)
        {
        case _C_ID:
          av_ptr (alist, id, arg->val.object);
          break;
        case _C_SEL:
          av_ptr (alist, SEL, arg->val.selector);
          break;
        case _C_CHR:
          av_char (alist, arg->val.schar);
          break;
        case _C_UCHR:
          av_uchar (alist, arg->val.uchar);
          break;
        case _C_SHT:
          av_short (alist, arg->val.sshort);
          break;
        case _C_USHT:
          av_ushort (alist, arg->val.ushort);
          break;
        case _C_INT:
          av_int (alist, arg->val.sint);
          break;
        case _C_UINT:
          av_uint (alist, arg->val.uint);
          break;
        case _C_LNG:
          av_long (alist, arg->val.slong);
          break;
        case _C_ULNG:
          av_ulong (alist, arg->val.ulong);
          break;
        case _C_FLT:
          av_float (alist, arg->val._float);
          break;
        case _C_DBL:
          av_double (alist, arg->val._double);
          break;
        case _C_CHARPTR:
          av_ptr (alist, const char *, arg->val.string);
          break;
        default:
          abort ();
        }
    }

  imp = [target methodFor: probedSelector];
  if (!imp)
    abort ();
  
  switch (*type)
    {
    case _C_ID: 
      av_start_ptr (alist, imp, id, &retVal->val.object);
      break;
    case _C_SEL:
      av_start_ptr (alist, imp, SEL, &retVal->val.selector);
      break;
    case _C_CHR:
      av_start_char (alist, imp, &retVal->val.schar);
      break;
    case _C_UCHR:
      av_start_uchar (alist, imp, &retVal->val.uchar);
      break;
    case _C_SHT:
      av_start_short (alist, imp, &retVal->val.sshort);
      break;
    case _C_USHT:
      av_start_ushort (alist, imp, &retVal->val.ushort);
      break;
    case _C_INT:
      av_start_int (alist, imp, &retVal->val.sint);
      break;
    case _C_UINT:
      av_start_uint (alist, imp, &retVal->val.uint);
      break;
    case _C_LNG:
      av_start_long (alist, imp, &retVal->val.slong);
      break;
    case _C_ULNG:
      av_start_ulong (alist, imp, &retVal->val.ulong);
      break;
    case _C_FLT:
      av_start_float (alist, imp, &retVal->val._float);
      break;
    case _C_DBL:
      av_start_double (alist, imp, &retVal->val._double);
      break;
    case _C_CHARPTR:
      av_start_ptr (alist, imp, const char *, &retVal->val.string);
      break;
    case _C_VOID:
      av_start_void (alist, imp);
      break;
    default:
      abort ();
    }
  retVal->type = *type;

  type = skip_argspec (type);
  
  if (*type != _C_ID)
    abort ();
  objectVal.type = _C_ID;
  objectVal.val.object = target;
  push_argument (&objectVal);
  
  type = skip_argspec (type);
  if (*type != _C_SEL)
    abort ();
  
  selectorVal.type = _C_SEL;
  selectorVal.val.selector = probedSelector;
  push_argument (&selectorVal);
  
  for (i = 0, type = skip_argspec (type);
       type;
       type = skip_argspec (type), i++)
    push_argument (&arguments[i]);
  
  av_call (alist);
}

#else

static void
dynamicCallOn (const char *probedType,
               id target,
               SEL probedSelector, 
               val_t *arguments,
               val_t *retVal)
{
  unsigned i;
  const char *type = probedType;
  id aZone = [target getZone];
  BOOL javaFlag;
  id fa = [FArguments createBegin: aZone];
  id <FCall> fc;

  javaFlag = ([target isKindOfClassNamed: "JavaProxy"] || 
              ![target respondsTo: probedSelector]);

  [fa setJavaFlag: javaFlag];
  retVal->type = *type;
  [fa setObjCReturnType: retVal->type];
  type = skip_argspec (type);
  type = skip_argspec (type);
  for (i = 0, type = skip_argspec (type);
       type; 
       type = skip_argspec (type), i++)
    [fa addArgument: &arguments[i].val ofObjCType: *type];
  fa = [fa createEnd];
  
  fc = [[FCall createBegin: aZone] setArguments: fa];
  
#ifdef HAVE_JDK
  if (javaFlag)
    {
      char *selname = strdup (sel_get_name (probedSelector));
      char *p;

      p = strchr (selname, ':');
      if (p)
        *p = '\0';

      [fc setJavaMethod: selname inObject: JFINDJAVA (target)];
    }
  else
#endif
    [fc setMethod: probedSelector inObject: target];
  fc = [fc createEnd];
  [fc performCall];
  retVal->val = *(types_t *) [fc getResult];
#ifdef HAVE_JDK
  if (javaFlag)
    {
      if (retVal->type == _C_CHARPTR)
        retVal->val.string = java_copy_string (jniEnv, retVal->val.object);
      else if (retVal->type == _C_ID)
        retVal->val.object = JFINDOBJC (jniEnv, retVal->val.object);
    }
#endif
  [fc drop];
  [fa drop];
}

#endif

- (val_t)dynamicCallOn: target
{
  val_t retVal;

  dynamicCallOn (probedType, target, probedSelector, arguments, &retVal);

  return retVal;
}

- (double)doubleDynamicCallOn: target
{
  val_t val = [self dynamicCallOn: target];

  if (val.type == _C_SHT)
#ifdef USE_AVCALL
    return (double) val.val.sshort;
#else
    return (double) val.val.sint; // short return is broken in libffi-1.18
#endif
  else if (val.type == _C_USHT)
#ifdef USE_AVCALL
    return (double) val.val.ushort; // short return is broken in libffi-1.18
#else
    return (double) val.val.uint; // short return is broken in libffi-1.18
#endif
  else if (val.type == _C_INT)
    return (double) val.val.sint;
  else if (val.type == _C_UINT)
    return (double) val.val.uint;
  else if (val.type == _C_LNG)
    return (double) val.val.slong;
  else if (val.type == _C_ULNG)
    return (double) val.val.ulong;
  else if (val.type == _C_CHR)
#ifdef USE_AVCALL
    return (double) val.val.sint; // character return is broken in libffi-1.18
#else
    return (double) val.val.schar;
#endif
  else if (val.type == _C_UCHR)
#ifdef USE_AVCALL
    return (double) val.val.uchar; // character return is broken in libffi-1.18
#else
    return (double) val.val.uint; // character return is broken in libffi-1.18
#endif
  else if (val.type == _C_FLT)
    return (double) val.val._float;
  else if (val.type == _C_DBL)
    return val.val._double;
  abort ();
}

- (int)intDynamicCallOn: target
{
  val_t val = [self dynamicCallOn: target];

  if (val.type == _C_SHT)
#ifdef USE_AVCALL
    return (int) val.val.sshort;
#else
    return (int) val.val.sint; // short return is broken in libffi-1.18
#endif
  else if (val.type == _C_USHT)
#ifdef USE_AVCALL
    return (int) val.val.ushort; // short return is broken in libffi-1.18
#else
    return (int) val.val.uint; // short return is broken in libffi-1.18
#endif
  else if (val.type == _C_INT)
    return val.val.sint;
  else if (val.type == _C_UINT)
    return (int) val.val.uint;
  else if (val.type == _C_LNG)
    return (int) val.val.slong;
  else if (val.type == _C_ULNG)
    return (int) val.val.ulong;
  else if (val.type == _C_CHR)
#ifdef USE_AVCALL
    return (int) val.val.sint; // character return is broken in libffi-1.18
#else
    return (int) val.val.schar;
#endif
  else if (val.type == _C_UCHR)
#ifdef USE_AVCALL
    return (int) val.val.uchar; // character return is broken in libffi-1.18
#else
    return (int) val.val.uint; // character return is broken in libffi-1.18
#endif
  else if (val.type == _C_FLT)
    return (int) val.val._float;
  else if (val.type == _C_DBL)
    return (int) val.val._double;
  abort ();
}

- (const char *)stringDynamicCallOn: target
{
  val_t val = [self dynamicCallOn: target];

  if (val.type != _C_CHARPTR)
    abort ();
  return val.val.string;
}

- objectDynamicCallOn: target
{
  val_t val = [self dynamicCallOn: target];

  if (val.type != _C_ID)
    abort ();
  return val.val.object;
}

- (BOOL)getHideResult
{
  return hideResultFlag;
}

@end

#if 0
  // since other routines call this so-called internal 
  // method _trueDynamicCallOn_, I have to put this here.  If and when
  // the probing is brought in line with our coding standards, this
  // hook section should be moved.
  if (objectToNotify != nil)
    {
      if ([objectToNotify respondsTo: M(forEach:)])
        {
          id index, tempObj;

          index = [objectToNotify begin: scratchZone];
          while ((tempObj = [index next]) != nil)
            {
              [tempObj eventOccurredOn: target
                       via: self
                       withProbeType: "MessageProbe"
                       on: probedMessage
                       ofType: probedType[0]
                       withData: (void *)cmd];
            }
          [index drop];
        }
      else 
        [objectToNotify eventOccurredOn: target
                        via: self
                        withProbeType: "MessageProbe"
                        on: probedMessage
                        ofType: probedType[0]
                        withData: cmd];
#endif

