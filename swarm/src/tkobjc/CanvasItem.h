// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>

@interface CanvasItem: SwarmObject 
{
  id  canvas;
  const char *item;
  id target;
  SEL clickSel, moveSel, postMoveSel;
}

- setCanvas: canvas;
- setTargetId: target;
- setClickSel: (SEL)sel;
- setMoveSel: (SEL)sel;
- setPostMoveSel: (SEL)sel;
- createItem;
- createBindings;
- createEnd;
- clicked;
- initiateMoveX: (long)delta_x Y: (long)delta_y;
@end


