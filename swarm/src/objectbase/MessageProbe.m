// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdio.h>
#import <stdlib.h>
#import <string.h>

#import <avcall.h>

#import "MessageProbe.h"
#import "swarm_rts_routines.h"
#import <objectbase.h> // val_t

@implementation MessageProbe

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

- (const char *)getProbedMessage
{
  return sel_get_name (probedSelector);
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
    val_t empty_val;
    int argCount = [self getArgCount];
    int i;

    empty_val.type = '\0';
    arguments = (val_t *)malloc (argCount * sizeof (val_t));

    for (i = 0; i < argCount; i++)
      arguments[i] = empty_val;
  }
  return self;
}

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
    case _C_INT:
      arguments[which].type = _C_INT;
      arguments[which].val._int = atoi (what);
      break;
    case _C_FLT:
      arguments[which].type = _C_FLT;
      arguments[which].val._float = strtod (what, NULL);
      break;
    case _C_DBL:
      arguments[which].type = _C_DBL;
      arguments[which].val._double = strtod (what, NULL);
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

  new_str = malloc ((end - beginning) + 1);

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


- (val_t)dynamicCallOn: target
{
  av_alist alist;
  const char *type = probedType;
  IMP imp;
  val_t val;
  val_t ret;
  int i;

  void push_argument (val_t arg)
    {
      switch (arg.type)
        {
        case _C_ID:
          av_ptr (alist, id, arg.val.object);
          break;
          
        case _C_SEL:
          av_ptr (alist, SEL, arg.val.selector);
          break;
          
        case _C_INT:
          av_int (alist, arg.val._int);
          break;
          
        case _C_FLT:
          av_float (alist, arg.val._float);
          break;
          
        case _C_DBL:
          av_double (alist, arg.val._double);
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
      av_start_ptr (alist, imp, id, &ret.val.object);
      break;
    case _C_SEL:
      av_start_ptr (alist, imp, SEL, &ret.val.selector);
      break;
    case _C_INT:
      av_start_int (alist, imp, &ret.val._int);
      break;
    case _C_FLT:
      av_start_float (alist, imp, &ret.val._float);
      break;
    case _C_DBL:
      av_start_double (alist, imp, &ret.val._double);
      break;
    default:
      abort ();
    }
  ret.type = *type;
  type = skip_argspec (type);
  
  if (*type != _C_ID)
    abort ();
  val.type = _C_ID;
  val.val.object = target;
  push_argument (val);
  
  type = skip_argspec (type);
  if (*type != _C_SEL)
    abort ();
  
  val.type = _C_SEL;
  val.val.selector = probedSelector;
  push_argument (val);
  
  for (i = 0, type = skip_argspec (type);
       type;
       type = skip_argspec (type), i++)
    push_argument (arguments[i]);
  
  av_call (alist);
  
  return ret;
}

- (double)doubleDynamicCallOn: target
{
  val_t val = [self dynamicCallOn: target];
  
  if (val.type != _C_DBL)
    abort ();
  return val.val._double;
}

- setHideResult: (BOOL)theHideResultFlag
{
  hideResultFlag = theHideResultFlag;
  return self;
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

