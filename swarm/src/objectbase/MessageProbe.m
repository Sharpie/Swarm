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

#import <defobj/FCall.h>
#import <defobj/FArguments.h>

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
  probedMethodName = STRDUP (swarm_sel_getName (aSel));
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

  probedSelector = swarm_sel_getUidWithType (probedMethodName);
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
      if (!swarm_sel_getTypeEncoding (probedSelector))
        {
          raiseEvent (WarningMessage, "Type for selector does not exist");
          [self drop]; 
          return nil;
        }
      probedType = GSTRDUP (swarm_sel_getTypeEncoding (probedSelector));
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
  call = nil;
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

- clone: (id <Zone>)aZone
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
  return swarm_sel_getName (probedSelector);
}

- (unsigned)getArgCount
{
  if (probedType)
    return get_number_of_arguments (probedType) - 2;
  else if (probedObject)
    {
      COMobject cObj = SD_COM_FIND_OBJECT_COM (probedObject);
      
      if (COM_is_javascript (cObj))
        return JS_method_arg_count (cObj, probedMethodName);
      else
        abort ();
    }
  else
    abort ();
}

- (val_t)getArg: (unsigned)which
{
  return arguments[which];
}

- setArg: (unsigned)i ToUnsigned: (unsigned)val
{
  arguments[i].type = fcall_type_uint;
  arguments[i].val.uint =  val;

  return self;
}

- setArg: (unsigned)i ToString: (const char *)what
{
  if (i >= [self getArgCount])
    abort ();
  if (probedType)
    {
      arguments[i].type = fcall_type_for_objc_type (nth_type (probedType, i));

      switch (arguments[i].type)
        {
        case fcall_type_object:
          arguments[i].val.object = nameToObject (what);
          break;
        case fcall_type_schar:
          arguments[i].val.schar = (char) atoi (what);
          break;
        case fcall_type_uchar:
          arguments[i].val.uchar = (unsigned char) strtoul (what, NULL, 10);
          break;
        case fcall_type_sshort:
          arguments[i].val.sshort = atoi (what);
          break;
        case fcall_type_ushort:
          arguments[i].val.ushort = (unsigned short) strtoul (what, NULL, 10);
          break;
        case fcall_type_sint:
          arguments[i].val.sint = atoi (what);
          break;
        case fcall_type_uint:
          arguments[i].val.uint = (unsigned int) strtoul (what, NULL, 10);
          break;
        case fcall_type_slong:
          arguments[i].val.slong = strtol (what, NULL, 10);
          break;
        case fcall_type_ulong:
          arguments[i].val.ulong = strtoul (what, NULL, 10);
          break;
        case fcall_type_slonglong:
          arguments[i].val.slonglong = (long long) strtol (what, NULL, 10);
          break;
        case fcall_type_ulonglong:
          arguments[i].val.ulonglong = (unsigned long long) strtoul (what, NULL, 10);
          break;
        case fcall_type_float:
          arguments[i].val._float = strtod (what, NULL);
          break;
        case fcall_type_double:
          arguments[i].val._double = strtod (what, NULL);
          break;
        case fcall_type_long_double:
          arguments[i].val._long_double = (long double) strtod (what, NULL);
          break;
        case fcall_type_string:
          arguments[i].val.string = STRDUP (what);
          break;
        default:
          abort ();
        }
    }
  else
    arguments[i] = [self guessValue: what];

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
  return copy_to_nth_colon (swarm_sel_getName (probedSelector), which);
}

- (BOOL)isResultId
{
  return (probedType[0] == _C_ID);
}

- (BOOL)isArgumentId: (unsigned)which
{
  return (nth_type (probedType, which) == _C_ID);
}

- (FCall_c *)_createFCall_: target
{
  id fa;
  id aZone = getZone (self);

  [self setProbedObject: target];
  fa = [FArguments createBegin: getCZone (aZone)];

  if (probedType)
    [fa setReturnType: fcall_type_for_objc_type (*probedType)];
  
  if (probedSelector)
    [fa setSelector: probedSelector];  // will set Language
  else if ([target respondsTo: M(isJavaProxy)])
    [fa setLanguage: LanguageJava];
  else if ([target respondsTo: M(isCOMProxy)])
    [fa setLanguage: LanguageCOM];
  {
    unsigned i;
    unsigned argCount = [self getArgCount];

    for (i = 0; i < argCount; i++)
      [fa addArgument: &arguments[i].val ofType: arguments[i].type];
  }
  fa = [fa createEnd];
  return [FCall create: getCZone (aZone)
                target: target
                methodName: probedMethodName
                arguments: fa];
}

- (val_t)dynamicCallOn: target
{
  FArguments_c *fa;
  FCall_c *fc;

  if (call)
    {
      fc = (FCall_c *)call;
      updateTarget (fc, target);
    }
  else
    {
      fc = [self _createFCall_: target];

      if (fc->fargs->assignedArgumentCount == 0)
        call = fc;
    }

  fa = fc->fargs;
      
  [fc performCall];
  {
    val_t retVal = [fa getRetVal];
#ifdef HAVE_JDK
    if (fa->language == LanguageJava)
      {
        if (retVal.type == fcall_type_string)
          retVal.val.string =
            JAVA_COPY_STRING ((jstring) retVal.val.object);
        else if (retVal.type == fcall_type_object)
          retVal.val.object =
            SD_JAVA_FIND_OBJECT_OBJC ((jobject) retVal.val.object);
      }
#endif
    if (!call)
      {
        [fc drop];
        [fa drop];
      }
    return retVal;
  }
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
  [stream catC: swarm_sel_getName (probedSelector)];
  [stream catC: "\n"];
}

- (void)drop
{
  FREEBLOCK (probedMethodName);
  if (arguments)
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

