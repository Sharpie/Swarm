#include "JavaProxy.h"
#include <objc/objc-api.h>
#include <objc/mframe.h>

#import <defobj/FArguments.h>
#import <defobj.h> // FCall

@implementation JavaProxy

- (retval_t)forward: (SEL)aSel : (arglist_t)argFrame
{
  NSArgumentInfo info;
  FArguments *fa;
  id <FCall> fc;

  union {
    char charVal;
    short shortVal;
    int intVal;
    long longVal;
    float floatVal;
    double doubleVal;
    const char *str;
    id obj;
  } val;
  id aZone = [self getZone];

  const char *type = sel_get_type (aSel);
  
  if (!type)
    {
      aSel = sel_get_any_typed_uid (sel_get_name (aSel));
      type = sel_get_type (aSel);
      if (!type)
        abort ();
    }
  printf ("sel `%s' type: `%s'\n", sel_get_name (aSel), type);
  fa = [FArguments createBegin: aZone];
  type = mframe_next_arg (type, &info);
  mframe_get_arg (argFrame, &info, &val);
  [fa setObjCReturnType: *info.type];
  while ((type = mframe_next_arg (type, &info)))
    {
      mframe_get_arg (argFrame, &info, &val);
      [fa addArgument: &val ofObjCType: *type];
    }
  fa = [fa createEnd];
  
  fc = [[[[FCall createBegin: aZone] setArguments: fa]
          setMethod: aSel inObject: self] createEnd];
  [fc performCall];
  {
    retval_t ret = [fc getReturnVal];
    [fc drop];
    [fa drop];
    
    return ret;
  }
}
@end
