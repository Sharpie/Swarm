// Swarm library. Copyright © 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "COMProxy.h"

@implementation COMProxy
PHASE(Creating)
PHASE(Setting)
PHASE(Using)
- (BOOL)isCOMProxy
{
  return YES;
}
@end
