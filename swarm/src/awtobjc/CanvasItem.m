// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#import <awtobjc/global.h>
#import <awtobjc/Widget.h>
#import <awtobjc/CanvasItem.h>

@implementation CanvasItem

- setCanvas: the_canvas
{
  canvas = the_canvas;
  return self;
}

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

- createItem 
{
  [self subclassResponsibility: @selector(createItem)];
    return self;
}

- createBindings
{
  return self;
}

- createEnd
{
  [self createItem];
  [self createBindings];
  return self;
}

- clicked
{
  if(clickSel && target)
    [target perform: clickSel];
  return self;
}

- initiateMoveX: (long)delta_x Y: (long)delta_y
{

  if (moveSel && target)
    {
      abort ();
    }
  
  if(postMoveSel && target)
    [target perform: postMoveSel];
  
  return self;
}

@end

