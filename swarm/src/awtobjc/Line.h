// Swarm library. Copyright (C) 1996, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <awtobjc/CanvasItem.h>

@interface Line: CanvasItem
{
  int tx,ty,lx,ly;
}

- setTX: (int)tx TY: (int)ty LX: (int)lx LY: (int)ly;
- createItem;
@end
