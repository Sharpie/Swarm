// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/SwarmObject.h>

@interface CanvasItem: SwarmObject {
  id  canvas ;
  char *item ;
  id target  ;
  SEL clickSel, moveSel, postMoveSel ;
}

-setCanvas: canvas ;
-setTargetId: target ;
-setClickSel: (SEL) sel ;
-setMoveSel: (SEL) sel ;
-setPostMoveSel: (SEL) sel ;
-createItem ;
-createBindings ;
-createEnd ;
-clicked ;
-initiateMoveX: (int) delta_x Y: (int) delta_y ;
@end


