#import <defobj.h>
#import "tactivity.h"
#import "tactivity.xm"

void
_tactivity_implement ()
{
  [id_ActionGroup_test_c setTypeImplemented: ActionGroup_test];
  [id_ConcurrentGroup_test_c setTypeImplemented: ConcurrentGroup_test];
}

void
_tactivity_initialize ()
{
}
