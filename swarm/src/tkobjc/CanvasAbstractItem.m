// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CanvasAbstractItem.h>

@implementation CanvasAbstractItem

PHASE(Creating)

- setCanvas: (id <Canvas>)theCanvas
{
  canvas = theCanvas;
  return self;
}

- (void)createItem
{
  [self subclassResponsibility: @selector (createItem)];
}

- (void)createBindings
{
  [self subclassResponsibility: @selector (createBindings)];
}

- createEnd 
{
  [self createItem];
  [self createBindings];
  return [super createEnd];
}

PHASE(Using)

- (void)setTargetId: the_target
{
  target = the_target;
}    

- (void)setClickSel: (SEL)the_sel
{
  clickSel = the_sel;
}

- (void)setMoveSel: (SEL)the_sel
{
  moveSel = the_sel;
}

- (void)setPostMoveSel: (SEL)the_sel
{
  postMoveSel = the_sel;
}

- (void)clicked
{
  if (clickSel && target)
    [target perform: clickSel];
}

- (void)initiateMoveX: (long)delta_x Y: (long)delta_y
{
  [self subclassResponsibility: @selector(initiateMoveX:Y:)];
}

- (id <Canvas>)getCanvas
{
  return canvas;
}

@end

