#include "JavaProxy.h"
#include <objc/objc-api.h>
#include <objc/mframe.h>

#import <defobj/FArguments.h>

@implementation JavaProxy
- (retval_t)forward: (SEL)aSel : (arglist_t)argFrame
{
  NSArgumentInfo info;
  FArguments *fa;

  const char *type = sel_get_type (aSel);
  
  if (!type)
    {
      aSel = sel_get_any_typed_uid (sel_get_name (aSel));
      type = sel_get_type (aSel);
      if (!type)
        abort ();
    }
  fa = [FArguments createBegin: [self getZone]];
  
  while ((type = mframe_next_arg (type, &info)))
    {
      printf ("type: %s\n", type);
      if (*info.type == _C_SHT)
        {
          short val;

          mframe_get_arg (argFrame, &info, &val);
          [fa addInt: val];
        }
      else if (*info.type == _C_USHT)
        {
          unsigned short val;

          mframe_get_arg (argFrame, &info, &val);
          [fa addUnsigned: val];
        }
      else if (*info.type == _C_UINT)
        {
          unsigned val;

          mframe_get_arg (argFrame, &info, &val);
          [fa addUnsigned: val];
        }
      else if (*info.type == _C_LNG)
        {
          long val;

          mframe_get_arg (argFrame, &info, &val);
          [fa addLong: val];
        }
      else if (*info.type == _C_ULNG)
        {
          unsigned val;
          
          mframe_get_arg (argFrame, &info, &val);
          [fa addUnsignedLong: val];
        }
      else if (*info.type == _C_FLT)
        {
          float val;
          
          mframe_get_arg (argFrame, &info, &val);
          [fa addFloat: val];
        }
      else if (*info.type == _C_DBL)
        {
          double val;
          
          mframe_get_arg (argFrame, &info, &val);
          [fa addDouble: val];
        }
      
    }
  return 0;
}
@end
