// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CanvasItem.h>
#import <tkobjc/Widget.h>
#import <tkobjc/global.h>

@implementation CanvasItem

- createItem
{
  [self subclassResponsibility: @selector (createItem)];
  return self;
}

- createBindings
{
  const char *temp = [self getObjectName];
  const char *canvasName = [canvas getWidgetName];

  [globalTkInterp eval: "%s bind %s <Button-3> {%s clicked}", 
                  canvasName, item, temp];
  [globalTkInterp eval: "%s bind %s <Button-1> {set curX %s; set curY %s}",
                  canvasName, item, "%x" , "%y"];

  [globalTkInterp eval: "%s bind %s <B1-Motion> {"
                  "%s initiateMoveX: [expr %s -$curX] Y: [expr %s -$curY];"
                  "set curX %s; set curY %s}",
                  canvasName,
                  item,
                  temp,
                  "%x", "%y", "%x", "%y"];

  return self;
}

- initiateMoveX: (long)delta_x Y: (long)delta_y
{
  if (moveSel && target)
    if ([target perform: moveSel with: (id)delta_x with: (id)delta_y])
      [globalTkInterp eval: "%s move %s %ld %ld; set curX %s; set curY %s",
                      [canvas getWidgetName],
                      item,
                      delta_x, delta_y,
                      "%x", "%y"];
  
  if (postMoveSel && target)
    [target perform: postMoveSel];
  
  return self;
}

@end

