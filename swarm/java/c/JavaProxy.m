#include "JavaProxy.h"
#include <objc/objc-api.h>
#if 0
#include "mframe.h"
#endif

@implementation JavaProxy
- (retval_t)forward: (SEL)aSel : (arglist_t)argFrame
{
#if 0
  unsigned i;
  NSArgumentInfo info;
  const char *type = sel_get_type (aSel);
  
  if (!type)
    {
      aSel = sel_get_any_typed_uid (sel_get_name (aSel));
      type = sel_get_type (aSel);
    }
  if (type)
    {
      while ((type = mframe_next_arg (type, &info)))
        {
          if (*info.type == _C_INT)
            {
              double val;
              mframe_get_arg (argFrame, &info, &val);
            }
        }
    }
#endif
}
@end
