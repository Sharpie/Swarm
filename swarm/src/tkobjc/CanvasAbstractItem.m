// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CanvasAbstractItem.h>

@implementation CanvasAbstractItem

PHASE(Creating)

- setCanvas: the_canvas
{
  canvas = the_canvas;
  return self;
}

- createItem
{
  [self subclassResponsibility: @selector (createItem)];

  return self;
}

- createBindings
{
  [self subclassResponsibility: @selector (createBindings)];

  return self;
}

- createEnd 
{
  [self createItem];
  [self createBindings];
  return self;
}

PHASE(Using)

- setTargetId: the_target
{
  target = the_target;
  return self;
}    

- setClickSel: (SEL)the_sel
{
  clickSel = the_sel;
  return self;
}

- setMoveSel: (SEL)the_sel
{
  moveSel = the_sel;
  return self;
}

- setPostMoveSel: (SEL)the_sel
{
  postMoveSel = the_sel;
  return self;
}

- clicked
{
  if (clickSel && target)
    [target perform: clickSel];
  return self ;
}

- initiateMoveX: (long)delta_x Y: (long)delta_y
{
  [self subclassResponsibility: @selector(initiateMoveX:Y:)];

  return self;
}

- getCanvas
{
  return canvas;
}

@end

