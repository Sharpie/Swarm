// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/MessageProbe.h>

#import "swarm_rts_routines.h"
#import <objectbase.h> // val_t
#include <misc.h>  // xmalloc, atoi, strtod

#import <defobj.h> // FCall, FArguments, STRDUP, ZSTRDUP
#import <defobj/defalloc.h> // getZone

#include <swarmconfig.h>
#ifdef HAVE_JDK
#import "../defobj/java.h" // JAVA_COPY_STRING, SD_JAVA_FIND_OBJECT_OBJC
#endif

#import "../defobj/COM.h" // SD_COM_FIND_OBJECT_COM, COM_is_javascript

#import "../defobj/internal.h"

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
  probedMethodName = STRDUP (sel_get_name (aSel));
  return self;
}

- setProbedMethodName: (const char *)theMethodName
{
  probedMethodName = STRDUP (theMethodName);
  probedSelector = NULL;
  return self;
}

- createEnd
{
  BOOL dynamicArgumentsFlag = NO;
  
  [super createEnd];

  probedSelector = sel_get_any_typed_uid (probedMethodName);
  if (probedObject)
    {
      COMobject cObject = SD_COM_FIND_OBJECT_COM (probedObject);

      dynamicArgumentsFlag = COM_is_javascript (cObject);
      probedType = NULL;
    }
  if (!dynamicArgumentsFlag)
    {
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
      probedType = sel_get_type (probedSelector);
    }
  {
    unsigned argCount = [self getArgCount];

    if (argCount > 0)
      {
        val_t empty_val;
        unsigned i;
        
        empty_val.type = '\0';
        arguments = 
          (val_t *) [getZone (self) alloc: argCount * sizeof (val_t)];
        
        for (i = 0; i < argCount; i++)
          arguments[i] = empty_val;
      }
    else
      arguments = NULL;
  }
  return self;
}

+ create: aZone setProbedSelector: (SEL)aSel
{
  return [[[self createBegin: aZone] setProbedSelector: aSel] createEnd];
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
nth_type (const char *type, unsigned which)
{
  unsigned i;

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

- (unsigned)getArgCount
{
  return get_number_of_arguments (probedType) - 2;
}

- (val_t)getArg: (unsigned)which
{
  return arguments[which];
}

- setArg: (unsigned)which ToUnsigned: (unsigned)val
{
  arguments[which].type = fcall_type_uint;
  arguments[which].val.uint =  val;

  return self;
}

- setArg: (unsigned)which ToString: (const char *)what
{
  switch (nth_type (probedType, which))
    {
    case _C_ID:
      arguments[which].type = fcall_type_object;
      arguments[which].val.object = nameToObject (what);
      break;
    case _C_CHR:
      arguments[which].type = fcall_type_schar;
      arguments[which].val.schar = (char) atoi (what);
      break;
    case _C_UCHR:
      arguments[which].type = fcall_type_uchar;
      arguments[which].val.uchar = (unsigned char) strtoul (what, NULL, 10);
      break;
    case _C_SHT:
      arguments[which].type = fcall_type_sshort;
      arguments[which].val.sshort = atoi (what);
      break;
    case _C_USHT:
      arguments[which].type = fcall_type_ushort;
      arguments[which].val.ushort = (unsigned short) strtoul (what, NULL, 10);
      break;
    case _C_INT:
      arguments[which].type = fcall_type_sint;
      arguments[which].val.sint = atoi (what);
      break;
    case _C_UINT:
      arguments[which].type = fcall_type_uint;
      arguments[which].val.uint = (unsigned int) strtoul (what, NULL, 10);
      break;
    case _C_LNG:
      arguments[which].type = fcall_type_slong;
      arguments[which].val.slong = strtol (what, NULL, 10);
      break;
    case _C_ULNG:
      arguments[which].type = fcall_type_ulong;
      arguments[which].val.ulong = strtoul (what, NULL, 10);
      break;
    case _C_LNG_LNG:
      arguments[which].type = fcall_type_slonglong;
      arguments[which].val.slonglong = (long long) strtol (what, NULL, 10);
      break;
    case _C_ULNG_LNG:
      arguments[which].type = fcall_type_ulonglong;
      arguments[which].val.ulonglong = (unsigned long long) strtoul (what, NULL, 10);
      break;
    case _C_FLT:
      arguments[which].type = fcall_type_float;
      arguments[which].val._float = strtod (what, NULL);
      break;
    case _C_DBL:
      arguments[which].type = fcall_type_double;
      arguments[which].val._double = strtod (what, NULL);
      break;
    case _C_LNG_DBL:
      arguments[which].type = fcall_type_long_double;
      arguments[which].val._long_double = (long double) strtod (what, NULL);
      break;
    case _C_CHARPTR:
      arguments[which].type = fcall_type_string;
      arguments[which].val.string = STRDUP (what);
      break;
    default:
      abort ();
    }
  return self;
}

static const char *
copy_to_nth_colon (const char *str, int n)
{
  int count = 0;
  int beginning,end,i;
  char *new_str;

  for (i = 0; i < n; i++)
    {
      while (str[count] && str[count] != ':')
        count++;
      if (!str[count])
        break;
      else
        count++;
    }

  if (i == n)
    {
      if (str[count])
        {
          beginning = count;
          
          while (str[count] && str[count] != ':')
            count++;
          
          count++;
          
          end = count;
          
          new_str = [scratchZone alloc: (end - beginning) + 1];
          
          count = 0;
          for (i = beginning; i < end; i++)
            new_str[count++] = str[i];
          new_str[count] = '\0';
          
          return new_str;
        }
      else
        return NULL;
    }
  return NULL;
} 

- (const char *)getArgName: (unsigned)which
{
  return copy_to_nth_colon (sel_get_name (probedSelector), which);
}

- (BOOL)isResultId
{
  return (probedType[0] == _C_ID);
}

- (BOOL)isArgumentId: (unsigned)which
{
  return (nth_type (probedType, which) == _C_ID);
}


- (val_t)dynamicCallOn: target
{
  val_t retVal;
  unsigned i;
  const char *type = probedType;
  id aZone = [target getZone];
  id fa = [FArguments createBegin: getCZone (aZone)];
  id <FCall> fc;

  retVal.type = fcall_type_for_objc_type (*type);

  if ([target respondsTo: M(isJavaProxy)])
    [fa setLanguage: LanguageJava];
  if (probedSelector)
    [fa setSelector: probedSelector];
  type = skip_argspec (type);
  type = skip_argspec (type);
  for (i = 0, type = skip_argspec (type);
       type; 
       type = skip_argspec (type), i++)
    [fa addArgument: &arguments[i].val ofObjCType: *type];
  fa = [fa createEnd];

  fc = [FCall create: getCZone (aZone)
	      target: target
	      methodName: probedMethodName
	      arguments: fa];
  [fc performCall];
  if (retVal.type != fcall_type_void)
    retVal.val = *(types_t *) [fc getResult];
#ifdef HAVE_JDK
  if ([fa getLanguage] == LanguageJava)
    {
      if (retVal.type == fcall_type_string)
        retVal.val.string =
          JAVA_COPY_STRING ((jstring) retVal.val.object);
      else if (retVal.type == fcall_type_object)
        retVal.val.object =
          SD_JAVA_FIND_OBJECT_OBJC ((jobject) retVal.val.object);
    }
#endif
  [fc drop];
  [fa drop];

  return retVal;
}

- (double)doubleDynamicCallOn: target
{
  val_t val = [self dynamicCallOn: target];
  double ret = 0.0;

  CONVERT (val.type, double, &val.val);
  return ret;
}

- (long)longDynamicCallOn: target
{
  val_t val = [self dynamicCallOn: target];
  long ret= 0;

  CONVERT (val.type, long, &val.val);
  return ret;
}

- (const char *)stringDynamicCallOn: target
{
  val_t val = [self dynamicCallOn: target];

  if (val.type != fcall_type_string)
    abort ();
  return val.val.string;
}

- objectDynamicCallOn: target
{
  val_t val = [self dynamicCallOn: target];

  if (val.type == fcall_type_object)
    return val.val.object;
  else if (val.type == fcall_type_selector)
    return (id) val.val.selector;
  else
    abort ();
}

- (BOOL)getHideResult
{
  return hideResultFlag;
}

- (void)describe: stream
{
  [super describe: stream];
  [stream catC: "selector: "];
  [stream catC: sel_get_name (probedSelector)];
  [stream catC: "\n"];
}

- (void)drop
{
  FREEBLOCK (probedMethodName);
  [getZone (self) free: arguments];
  [super drop];
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

