#include "JavaProxy.h"
#include <objc/objc-api.h>
#include <objc/mframe.h>

#import <defobj/FArguments.h>
#import <defobj.h> // FCall

#include <jni.h>

@implementation JavaProxy

- (int)try: (const char *)str
{
  printf ("try:\n");
  return 3;
}

- (double)tryDouble: (const char *)str
{
  return 2.5;
}

- (retval_t)forward: (SEL)aSel : (arglist_t)argFrame
{
  NSArgumentInfo info;
  FArguments *fa;
  id <FCall> fc;
  types_t val;
  id aZone = [self getZone];
  const char *type = sel_get_type (aSel);
  
  if (!type)
    {
      aSel = sel_get_any_typed_uid (sel_get_name (aSel));
      type = sel_get_type (aSel);
      if (!type)
        abort ();
    }
  fa = [FArguments createBegin: aZone];
  type = mframe_next_arg (type, &info);
  mframe_get_arg (argFrame, &info, &val);
  [fa setObjCReturnType: *info.type];
  /* skip object and selector */
  type = mframe_next_arg (type, &info);
  type = mframe_next_arg (type, &info);
  while ((type = mframe_next_arg (type, &info)))
    {
      mframe_get_arg (argFrame, &info, &val);
      [fa addArgument: &val ofObjCType: *info.type];
    }
  fa = [fa createEnd];
  
  fc = [[[[FCall createBegin: aZone] setArguments: fa]
          setMethod: @selector (tryDouble:) inObject: self] createEnd];
  printf ("before call\n");
  [fc performCall];
  printf ("after call\n");
  {
    types_t typebuf;
    retval_t retval = [fc getRetVal: argFrame buf: &typebuf];

    printf ("val:[%f %f]\n", typebuf._double, *(double *) [fc getResult]);
    [fc drop];
    [fa drop];
    return retval;
  }
}
@end
