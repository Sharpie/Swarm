// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <tkobjc/Widget.h>
#import <tkobjc/CanvasItem.h>

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

- setClickSel: (SEL) the_sel
{
  clickSel = the_sel;
  return self;
}

- setMoveSel: (SEL) the_sel
{
  moveSel = the_sel;
  return self;
}

- setPostMoveSel: (SEL) the_sel
{
  postMoveSel = the_sel;
  return self;
}

- createItem
{
  [self subclassResponsibility: @selector (createItem)];
  return self;
}

- createBindings
{
  const char *temp = tclObjc_objectToName (self);

  [globalTkInterp eval: "%s bind %s <Button-3> {%s clicked}", 
    [canvas getWidgetName], item, temp];
  [globalTkInterp eval: "%s bind %s <Button-1> {set curX %s; set curY %s}",
                  [canvas getWidgetName], item, "%x" , "%y"];

  [globalTkInterp eval: "%s bind %s <B1-Motion> {"
                  "%s initiateMoveX: [expr %s -$curX] Y: [expr %s -$curY];"
                  "set curX %s; set curY %s}",
                  [canvas getWidgetName],
                  item,
                  temp,
                  "%x", "%y", "%x", "%y"];
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
  if (clickSel && target)
    [target perform: clickSel];
  return self ;
}

- initiateMoveX: (long)delta_x Y: (long)delta_y
{
  if (moveSel && target)
    {
      if ([target perform: moveSel with: (id)delta_x with: (id)delta_y])
        [globalTkInterp eval: "%s move %s %ld %ld; set curX %s; set curY %s",
                        [canvas getWidgetName],
                        item,
                        delta_x, delta_y,
                        "%x", "%y"];
    }
  
  if (postMoveSel && target)
    [target perform: postMoveSel];
  
  return self;
}

@end

