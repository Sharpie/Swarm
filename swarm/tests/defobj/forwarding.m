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

- (retval_t)forward: (SEL)aSel :(arglist_t)argFrame
{
  id fa, fc;
  const char *type = sel_get_type (aSel);  
  NSArgumentInfo info;  
  types_t val;

  fa = [FArguments createBegin: globalZone];

  if (!type)
    {
      aSel = sel_get_any_typed_uid (sel_get_name (aSel));
      type = sel_get_type (aSel);
      if (!type)
        abort ();
    }
  
  {
    const char *sig = mframe_build_signature (type, NULL, NULL, NULL);

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
  return 0;
}
