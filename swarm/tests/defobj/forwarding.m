#import <simtools.h> // initSwarmBatch
#import <defobj.h> // FArguments, FCall
#import <defobj/Create.h>
#include <misc.h> // printf
#include <objc/mframe.h>

@interface DelegateObject: CreateDrop
- (const char *)m1: (int)num float: (float)val double: (double)val2;
- (const char *)m2: (int)num double: (double)val float: (float)val2;
- (const char *)m3: (float)val double: (double)val2 int: (int)num; 
- (const char *)m4: (double)val float: (float)val2 int: (int)num;
- (const char *)m5: (float)val double: (double)val2 int: (int)num int2: (int)num2; 
- (const char *)m6: (float)fval float2: (float)fval2 double: (double)dval int: (int)num int2: (int)num2; 

- (int)im1i: (int)val1;
- (float)fm1i: (int)val1;
- (double)dm1i: (int)val1;

@end

@implementation DelegateObject
- (const char *)m1: (int)num float: (float)val1 double: (double)val2
{
  static char buf[20];

  sprintf (buf, "%d %.2f %.2f", num, val1, val2);
  printf ("m1: [%s]\n", buf);
  return buf;
}

- (const char *)m2: (int)num double: (double)val1 float: (float)val2
{
  static char buf[20];

  sprintf (buf, "%d %.2f %.2f", num, val2, val1);
  printf ("m2: [%s]\n", buf);
  return buf;
}

- (const char *)m3: (float)val1 double: (double)val2 int: (int)num
{
  static char buf[20];

  sprintf (buf, "%d %.2f %.2f", num, val1, val2);
  printf ("m3: [%s]\n", buf);
  return buf;
}

- (const char *)m4: (double)val1 float: (float)val2 int: (int)num
{
  static char buf[20];

  sprintf (buf, "%d %.2f %.2f", num, val2, val1);
  printf ("m4: [%s]\n", buf);
  return buf;
}

- (const char *)m5: (float)val1 double: (double)val2 int: (int)num int2: (int)num2
{
  static char buf[30];

  sprintf (buf, "%d %d %.2f %.2f", num, num2, val1, val2);
  printf ("m5: [%s]\n", buf);
  return buf;
}

- (const char *)m6: (float)fval1 float2: (float)fval2 double: (double)dval int: (int)num int2: (int)num2
{
  static char buf[40];

  sprintf (buf, "%d %d %.2f %.2f %.2f", num, num2, fval1, fval2, dval);
  printf ("m6: [%s]\n", buf);
  return buf;
}

- (char)cm1i: (int)val1
{
  char result = 'A' + (char)val1;

  printf ("cm1i: `%c'\n", result);
  return result;
}

- (short)sm1i: (int)val1
{
  short result = val1 + 1;

  printf ("sm1i: %hd\n", result);
  return result;
}

- (int)im1i: (int)val1
{
  int result = val1 + 2;
  
  printf ("im1i: %d\n", result);
  return result;
}

- (long)lm1i: (int)val1
{
  long result = val1 + 3;

  printf ("lm1i: %ld\n", result);
  return result;
}

- (float)fm1i: (int)val1
{
  float result = val1 + 4;

  printf ("fm1i: %f\n", result);
  return result;
}

- (double)dm1i: (int)val1
{
  double result = val1 + 5;

  printf ("dm1i: %f\n", result);
  return result;
}

@end

@interface BaseObject: CreateDrop
{
  id delegateObject;
}
- setDelegateObject: delegateObject;
- (retval_t)forward: (SEL)sel :(arglist_t)argFrame;
@end

@implementation BaseObject
- setDelegateObject: anObject
{
  delegateObject = anObject;
  return self;
}

#define isnumeric(c) (isdigit ((int) c) || c == '+' || c == '-')

static const char *
strip_type_sig (const char *sig)
{
  size_t i;
  size_t newlen = 0;
  
  for (i = 0; sig[i]; i++)
    if (!isnumeric (sig[i]))
      newlen++;
  
  {
    char *newsig = xmalloc (newlen + 1);
    
    newlen = 0;
    for (i = 0; sig[i]; i++)
      if (!isnumeric (sig[i]))
        newsig[newlen++] = sig[i];
    newsig[newlen] = '\0';
    return newsig;
  }
}

- (retval_t)forward: (SEL)aSel :(arglist_t)argFrame
{
  id fa, fc;
  const char *type = sel_get_type (aSel);  
  NSArgumentInfo info;  
  types_t val;
  const char *stripped_type;

  fa = [FArguments createBegin: globalZone];

  if (!type)
    {
      aSel = sel_get_any_typed_uid (sel_get_name (aSel));
      type = sel_get_type (aSel);
      if (!type)
        abort ();
    }

  stripped_type = strip_type_sig (type);
  {
    const char *sig = mframe_build_signature (stripped_type, NULL, NULL, NULL);

    printf ("{%s} {%s}{%s}\n\n", stripped_type, type, sig);
    sig = mframe_next_arg (sig, &info);
    mframe_get_arg (argFrame, &info, &val);
    [fa setObjCReturnType: *info.type];
    /* skip object and selector */
    sig = mframe_next_arg (sig, &info);
    sig = mframe_next_arg (sig, &info);
    while ((sig = mframe_next_arg (sig, &info)))
      {
        mframe_get_arg (argFrame, &info, &val);
		[fa addArgument: &val ofObjCType: *info.type];
      }
    XFREE (sig);
  }
  fa = [fa createEnd];

  fc = [FCall createBegin: scratchZone];
  fc = [fc setArguments: fa];
  fc = [fc setMethod: aSel inObject: delegateObject];
  fc = [fc createEnd];

  [fc performCall];
  
  {
    types_t retBuf;
    retval_t retValBuf = alloca (MFRAME_RESULT_SIZE);
    retval_t ret = [fc getRetVal: retValBuf buf: &retBuf];
   
    [fc drop];
    [fa drop];
    
    return ret;
  }
}

@end

int
main (int argc, const char **argv)
{
  id obj;

  initSwarmBatch (argc, argv);
  
  obj = [[BaseObject createBegin: globalZone]
             setDelegateObject: [DelegateObject create: globalZone]];

  if (strcmp ([obj m1: 1 float: 2.0 double: 3.0], "1 2.00 3.00") != 0)
    return 1;
  if (strcmp ([obj m2: 4 double: 5.0 float: 6.0], "4 6.00 5.00") != 0)
    return 1;
  if (strcmp ([obj m3: 7.0 double: 8.0 int: 9], "9 7.00 8.00") != 0)
    return 1;
  if (strcmp ([obj m4: 10.0 float: 11.0 int: 12], "12 11.00 10.00") != 0)
    return 1;
  if (strcmp ([obj m5: 13.0 double: 14.0 int: 15 int2: 16], "15 16 13.00 14.00") != 0)
    return 1;
  if (strcmp ([obj m6: 17.0 float2: 18.0 double: 19.0 int: 20 int2: 21], "20 21 17.00 18.00 19.00") != 0)
    return 1;

  {
    char ret = [obj cm1i: 1];
    
    if (ret != 'B')
      {
        fprintf (stderr, "got: `%c' (%d)\n", ret, (int) ret);
        return 1;
      }
  }

  {
    short ret = [obj sm1i: 2];

    if (ret != 3)
      {
        fprintf (stderr, "got: %hd\n", ret);
        return 1;
      }
  }

  if ([obj im1i: 3] != 5)
    return 1;

  if ([obj lm1i: 4] != 7)
    return 1;

  if ([obj fm1i: 5] != 9)
    return 1;

  if ([obj dm1i: 6] != 11)
    return 1;

  return 0;
}
