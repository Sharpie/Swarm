// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>
#import <gui.h>

@interface CanvasAbstractItem: CreateDrop <CanvasItem>
{
  id canvas;
  id target;
  SEL clickSel, moveSel, postMoveSel;
}

- (void)createItem;
- (void)createBindings;
- createEnd;
- setCanvas: (id <Canvas>)canvas;
- (void)setTargetId: target;
- (void)setClickSel: (SEL)sel;
- (void)setMoveSel: (SEL)sel;
- (void)setPostMoveSel: (SEL)sel;
- (void)clicked;
- (void)initiateMoveX: (long)delta_x Y: (long)delta_y;
- (id <Canvas>)getCanvas;
@end


