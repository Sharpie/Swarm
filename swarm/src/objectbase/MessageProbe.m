// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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
#import <misc.h>  // xmalloc, strdup, atoi, strtod

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
      arguments[which].val._char = (char) atoi (what);
      break;
    case _C_UCHR:
      arguments[which].type = _C_UCHR;
      arguments[which].val._uchar = (unsigned char) strtoul (what, NULL, 10);
      break;
    case _C_SHT:
      arguments[which].type = _C_SHT;
      arguments[which].val._short = atoi (what);
      break;
    case _C_USHT:
      arguments[which].type = _C_USHT;
      arguments[which].val._ushort = (unsigned short) strtoul (what, NULL, 10);
      break;
    case _C_INT:
      arguments[which].type = _C_INT;
      arguments[which].val._int = atoi (what);
      break;
    case _C_UINT:
      arguments[which].type = _C_UINT;
      arguments[which].val._uint = (unsigned int) strtoul (what, NULL, 10);
      break;
    case _C_LNG:
      arguments[which].type = _C_LNG;
      arguments[which].val._long = strtol (what, NULL, 10);
      break;
    case _C_ULNG:
      arguments[which].type = _C_ULNG;
      arguments[which].val._ulong = strtoul (what, NULL, 10);
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
               id target, SEL probedSelector, 
               val_t *arguments,
               val_t *retVal)
{
  const char *type = probedType;
  IMP imp;
  val_t objectVal, selectorVal;
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
        case _C_CHR:
          av_char (alist, arg->val._char);
          break;
        case _C_UCHR:
          av_uchar (alist, arg->val._uchar);
          break;
        case _C_SHT:
          av_short (alist, arg->val._short);
          break;
        case _C_USHT:
          av_ushort (alist, arg->val._ushort);
          break;
        case _C_INT:
          av_int (alist, arg->val._int);
          break;
        case _C_UINT:
          av_uint (alist, arg->val._uint);
          break;
        case _C_LNG:
          av_long (alist, arg->val._long);
          break;
        case _C_ULNG:
          av_ulong (alist, arg->val._ulong);
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
      av_start_char (alist, imp, &retVal->val._char);
      break;
    case _C_UCHR:
      av_start_uchar (alist, imp, &retVal->val._uchar);
      break;
    case _C_SHT:
      av_start_short (alist, imp, &retVal->val._short);
      break;
    case _C_USHT:
      av_start_ushort (alist, imp, &retVal->val._ushort);
      break;
    case _C_INT:
      av_start_int (alist, imp, &retVal->val._int);
      break;
    case _C_UINT:
      av_start_uint (alist, imp, &retVal->val._uint);
      break;
    case _C_LNG:
      av_start_long (alist, imp, &retVal->val._long);
      break;
    case _C_ULNG:
      av_start_ulong (alist, imp, &retVal->val._ulong);
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
               id target, SEL probedSelector, 
               val_t *arguments,
               val_t *retVal)
{
  const char *type = probedType;
  IMP imp;
  val_t objectVal, selectorVal;
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
        case _C_CHR:
          *alist->type_pos = &ffi_type_schar;
          *alist->value_pos = &arg->val._char;
          break;
        case _C_UCHR:
          *alist->type_pos = &ffi_type_uchar;
          *alist->value_pos = &arg->val._uchar;
          break;
        case _C_SHT:
          *alist->type_pos = &ffi_type_sshort;
          *alist->value_pos = &arg->val._short;
          break;
        case _C_USHT:
          *alist->type_pos = &ffi_type_ushort;
          *alist->value_pos = &arg->val._ushort;
          break;
        case _C_INT:
          *alist->type_pos = &ffi_type_sint;
          *alist->value_pos = &arg->val._int;
          break;
        case _C_UINT:
          *alist->type_pos = &ffi_type_uint;
          *alist->value_pos = &arg->val._uint;
          break;
        case _C_LNG:
          *alist->type_pos = &ffi_type_slong;
          *alist->value_pos = &arg->val._long;
          break;
        case _C_ULNG:
          *alist->type_pos = &ffi_type_ulong;
          *alist->value_pos = &arg->val._ulong;
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

  retVal->type = *type;

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
  
  switch (retVal->type)
    {
    case _C_ID: 
      fret = &ffi_type_pointer;
      break;
    case _C_SEL:
      fret = &ffi_type_pointer;
      break;
    case _C_CHR:
      // character return is broken in libffi-1.18
      fret = &ffi_type_sint;
      break;
    case _C_UCHR:
      // character return is broken in libffi-1.18
      fret = &ffi_type_uint;
      break;
    case _C_SHT:
      fret = &ffi_type_sshort;
      break;
    case _C_USHT:
      fret = &ffi_type_ushort;
      break; 
    case _C_INT:
      fret = &ffi_type_sint;
      break;
    case _C_UINT:
      fret = &ffi_type_uint;
      break; 
    case _C_LNG:
      fret = &ffi_type_slong;
      break;
    case _C_ULNG:
      fret = &ffi_type_ulong;
      break; 
   case _C_FLT:
      fret = &ffi_type_float;
      break;
    case _C_DBL:
      fret = &ffi_type_double;
      break;
    case _C_CHARPTR:
      fret = &ffi_type_pointer;
      break;

    default:
      abort ();
    }

  if (ffi_prep_cif (&cif, FFI_DEFAULT_ABI, acnt, fret, types_buf) != FFI_OK)
    abort ();

  imp = [target methodFor: probedSelector];
  if (!imp)
    abort ();

  {
    long long ret = 0;
    
    ffi_call (&cif, (void *)imp, &ret, values_buf);
    
#ifdef __mips64
#define VAL(type, var) (*((type *)(((void *)&var)+(sizeof(var)-sizeof(type)))))
#else
#define VAL(type, var) (*((type *)&var))
#endif
    
    switch (retVal->type)
      {
      case _C_ID: 
        retVal->val.object = VAL(id, ret);
        break;
      case _C_SEL:
          retVal->val.selector = VAL(SEL, ret);
          break;
      case _C_CHR:
        // character return is broken in libffi-1.18
        retVal->val._int = VAL(int, ret);  
        break;
      case _C_UCHR:
        // character return is broken in libffi-1.18
        retVal->val._uint = VAL(unsigned int, ret);  
        break;
      case _C_INT:
        retVal->val._int = VAL(int, ret);
        break;
      case _C_UINT:
        retVal->val._uint = VAL(unsigned int, ret);
        break;
      case _C_SHT:
        // short return is broken in libffi-1.18
        retVal->val._int = VAL(int, ret);
        break;
      case _C_USHT:
        // short return is broken in libffi-1.18
        retVal->val._uint = VAL(unsigned int, ret);
        break;
      case _C_LNG:
        retVal->val._long = VAL(long, ret);
        break;
      case _C_ULNG:
        retVal->val._ulong = VAL(unsigned long, ret);
        break;
      case _C_FLT:
        retVal->val._float = VAL(float, ret);
        break;
      case _C_DBL:
        retVal->val._double = VAL(double, ret);
        break;
      case _C_CHARPTR:
        retVal->val.string = VAL(const char *, ret);
        break;
        
      default:
        abort ();
      }
  }
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
    return (double)val.val._int; // short return is broken in libffi-1.18
  else if (val.type == _C_USHT)
    return (double)val.val._uint; // short return is broken in libffi-1.18
  else if (val.type == _C_INT)
    return (double)val.val._int;
  else if (val.type == _C_UINT)
    return (double)val.val._uint;
  else if (val.type == _C_LNG)
    return (double)val.val._long;
  else if (val.type == _C_ULNG)
    return (double)val.val._ulong;
  else if (val.type == _C_CHR)
    return (double)val.val._int; // character return is broken in libffi-1.18
  else if (val.type == _C_UCHR)
    return (double)val.val._uint; // character return is broken in libffi-1.18
  else if (val.type == _C_FLT)
    return (double)val.val._float;
  else if (val.type == _C_DBL)
    return val.val._double;
  abort ();
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

