// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdlib.h> // atoi, strtod
#import <misc.h>  // xmalloc, strdup

#ifdef USE_AVCALL
#import <avcall.h>
#else
#import <ffi.h>
#endif

#import "MessageProbe.h"
#import "swarm_rts_routines.h"
#import <objectbase.h> // val_t

//S: A class that allows the user to call a given message on any candidate
//S: that is an instance of, or inherits from, a given class.
//D: This is a specialized subclass of the abstract class Probe. It completes 
//D: the specification of a probe that refers to a message element of
//D: an object. 
@implementation MessageProbe

+ createBegin: aZone
{
  MessageProbe *obj;

  obj = [super createBegin: aZone];
  obj->objectToNotify = nil;
  return obj;
}

//M: The setProbedSelector: method sets the message to be probed given the 
//M: selector. 
- setProbedSelector: (SEL)aSel
{
  probedSelector = aSel;
  return self;
}

//M: The getProbedMessage method returns the string matching the identifier of
//M: the message being probed.
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
    arguments = (val_t *)xmalloc (argCount * sizeof (val_t));

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

//M: The getArg: method returns a string representation of the nth argument.
- (val_t)getArg: (int)which
{
  return arguments[which];
}

//M: The setArg:ToString: method sets the nth argument of the message. 
//M: The argument must be provided in string form.
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
    case _C_CHARPTR:
      arguments[which].type = _C_CHARPTR;
      arguments[which].val.string = what;
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

//M: The getArgName: method returns a string representation of the argument
//M: with the given name.
- (const char *)getArgName: (int)which
{
  return copy_to_nth_colon (sel_get_name (probedSelector), which);
}

//M: The isResultId method returns 1 if the return value of the message is of
//M: type object, and returns 0 otherwise.
- (BOOL)isResultId
{
  return (probedType[0] == _C_ID);
}

//M: The isArgumentId: method returns 1 if a given argument of the message
//M: is of type object, and returns 0 otherwise.
- (BOOL)isArgumentId: (int)which
{
  return (nth_type (probedType, which) == _C_ID);
}

#ifdef USE_AVCALL

//M: The dynamicCallOn: method generates a dynamic message call on the target
//M: object. This method does not return a result.
- (val_t)dynamicCallOn: target
{
  const char *type = probedType;
  IMP imp;
  val_t objectVal, selectorVal, retVal;
  int i;

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

        case _C_UCHR:
          av_uchar (alist, arg->val._uchar);
          break;
          
        case _C_INT:
          av_int (alist, arg->val._int);
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
      av_start_ptr (alist, imp, id, &retVal.val.object);
      break;
    case _C_SEL:
      av_start_ptr (alist, imp, SEL, &retVal.val.selector);
      break;
    case _C_UCHR:
      av_start_uchar (alist, imp, &retVal.val._uchar);
      break;
    case _C_INT:
      av_start_int (alist, imp, &retVal.val._int);
      break;
    case _C_FLT:
      av_start_float (alist, imp, &retVal.val._float);
      break;
    case _C_DBL:
      av_start_double (alist, imp, &retVal.val._double);
      break;
    case _C_CHARPTR:
      av_start_ptr (alist, imp, const char *, &retVal.val.string);
      break;
    default:
      abort ();
    }
  retVal.type = *type;

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
  
  return retVal;
}

#else

- (val_t)dynamicCallOn: target
{
  const char *type = probedType;
  IMP imp;
  val_t objectVal, selectorVal, retVal;
  int i;
  int acnt = get_number_of_arguments (probedType);
  ffi_cif cif;
  ffi_type *fret;
  
  struct alist
    {
      ffi_type **type_pos;
      void **value_pos;
    };
  
  typedef struct alist *av_alist;
  
  struct alist alist_buf;
  av_alist alist = &alist_buf;
  ffi_type *types_buf[acnt];
  void *values_buf[acnt];
  void *ret_addr;

  void push_argument (val_t *arg)
    {
      switch (arg->type)
        {
        case _C_ID:
          *alist->type_pos = &ffi_type_pointer;
          *alist->value_pos = &arg->val.object;
          break;
          
        case _C_SEL:
          *alist->type_pos = &ffi_type_pointer;
          *alist->value_pos = &arg->val.selector;
          break;

        case _C_UCHR:
          *alist->type_pos = &ffi_type_uchar;
          *alist->value_pos = &arg->val._uchar;
          break;
          
        case _C_INT:
          *alist->type_pos = &ffi_type_sint;
          *alist->value_pos = &arg->val._int;
          break;
      
        case _C_FLT:
          *alist->type_pos = &ffi_type_float;
          *alist->value_pos = &arg->val._float;
          break;
          
        case _C_DBL:
          *alist->type_pos = &ffi_type_double;
          *alist->value_pos = &arg->val._double;
          break;
          
        case _C_CHARPTR:
          *alist->type_pos = &ffi_type_pointer;
          *alist->value_pos = &arg->val.string;
          break;

        default:
          abort ();
        }
      alist->type_pos++;
      alist->value_pos++;
    }
  
  alist->type_pos = types_buf;
  alist->value_pos = values_buf;

  retVal.type = *type;

  type = skip_argspec (type);
  objectVal.type = *type;
  if (objectVal.type != _C_ID)
    abort ();
  objectVal.val.object = target;
  push_argument (&objectVal);

  type = skip_argspec (type);
  selectorVal.type = *type;
  if (selectorVal.type != _C_SEL)
    abort ();
  selectorVal.val.selector = probedSelector;
  push_argument (&selectorVal);
  
  for (i = 0, type = skip_argspec (type);
       type;
       type = skip_argspec (type), i++)
    push_argument (&arguments[i]);
  
  switch (retVal.type)
    {
    case _C_ID: 
      fret = &ffi_type_pointer;
      ret_addr = &retVal.val.object;
      break;
    case _C_SEL:
      fret = &ffi_type_pointer;
      ret_addr = &retVal.val.selector;
      break;
    case _C_UCHR:
      fret = &ffi_type_uchar;
      ret_addr = &retVal.val._uchar;
      break;
    case _C_INT:
      fret = &ffi_type_sint;
      ret_addr = &retVal.val._int;
      break;
    case _C_FLT:
      fret = &ffi_type_float;
      ret_addr = &retVal.val._float;
      break;
    case _C_DBL:
      fret = &ffi_type_double;
      ret_addr = &retVal.val._double;
      break;
    case _C_CHARPTR:
      fret = &ffi_type_pointer;
      ret_addr = &retVal.val.string;
      break;

    default:
      abort ();
    }

  if (ffi_prep_cif (&cif, FFI_DEFAULT_ABI, acnt, fret, types_buf) != FFI_OK)
    abort ();

  imp = [target methodFor: probedSelector];
  if (!imp)
    abort ();

  ffi_call (&cif, (void *)imp, ret_addr, values_buf);
  
  return retVal;
}

#endif

//M: The doubleDynamicCallOn: method generates a dynamic message call on the 
//M: target object. This method assumes the user knows the type to be double 
//M: and would like a direct translation into type double.
- (double)doubleDynamicCallOn: target
{
  val_t val = [self dynamicCallOn: target];
  
  if (val.type == _C_INT)
    return (double)val.val._int;
  else if (val.type == _C_UCHR)
#ifdef USE_AVCALL
    return (double)val.val._uchar;
#else
    return (double)val.val._int;
#endif
  else if (val.type == _C_FLT)
    return (double)val.val._float;
  else if (val.type == _C_DBL)
    return val.val._double;
  abort ();
}

//M: The setHideResult: method is used to set the visibility of the result
//M: field. When set to 1, the user is indicating that the result field in 
//M: a graphical representation of the message probe should not be shown.
- setHideResult: (BOOL)theHideResultFlag
{
  hideResultFlag = theHideResultFlag;
  return self;
}

//M: The getHideResult method returns 1 if the result field is "hidden".
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

