// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CompositeItem.h>

@implementation CompositeItem

PHASE(Creating)

PHASE(Using)

- moveX: (long)deltaX Y: (long)deltaY
{
  [self subclassResponsibility: @selector (moveX:Y:)];

  return self;
}

- initiateMoveX: (long)deltaX Y: (long)deltaY
{
  if (moveSel && target)
    {
      if ([target perform: moveSel with: (id) deltaX with: (id) deltaY])
        [self moveX: deltaX Y: deltaY];
    }
  
  if (postMoveSel && target)
    [target perform: postMoveSel];

  return self;
}

@end
