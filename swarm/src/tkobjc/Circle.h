// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CanvasItem.h>
#import <gui.h>

@interface Circle: CanvasItem <Circle>
{
  int x, y, r;
}

- setX: (int)x Y: (int)y;
- setRadius: (unsigned)r;
- createItem;
@end
