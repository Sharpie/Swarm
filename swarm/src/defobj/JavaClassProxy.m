// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/JavaClassProxy.h>

@implementation JavaClassProxy
PHASE(Creating)
PHASE(Setting)
PHASE(Using)

- (BOOL)isJavaProxy
{
  return YES;
}

- (int)compare: obj2
{
  return [super compare: obj2];
}

@end
