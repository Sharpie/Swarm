// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CompositeItem.h>

@implementation CompositeItem

PHASE(Creating)

PHASE(Using)

- moveX: (long)delta_x Y: (long)delta_y
{
  [self subclassResponsibility: @selector (moveX:Y:)];

  return self;
}

- initiateMoveX: (long)delta_x Y: (long)delta_y
{
  if (moveSel && target)
    {
      if ([target perform: moveSel with: (id)delta_x with: (id)delta_y])
        [self moveX: delta_x Y: delta_y];
    }
  
  if (postMoveSel && target)
    [target perform: postMoveSel];

  return self;
}

@end
