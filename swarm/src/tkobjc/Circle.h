// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CanvasItem.h>
#import <gui.h>

@interface Circle: CanvasItem <_Circle>
{
  int x, y, r;
}

- setX: (int)x Y: (int)y;
- setRadius: (int)r;
- createItem;
- reportClick;
- (int)reportMoveX: (int)d_x Y: (int)d_y;
@end
