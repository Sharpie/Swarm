// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CanvasItem.h>
#import <tkobjc/Widget.h>
#import <tkobjc/global.h>

@implementation CanvasItem

PHASE(Creating)

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

PHASE(Using)

- initiateMoveX: (long)deltaX Y: (long)deltaY
{
  if (moveSel && target)
    if ([target perform: moveSel with: (id)deltaX with: (id)deltaY])
      [globalTkInterp eval: "%s move %s %ld %ld; set curX %s; set curY %s",
                      [canvas getWidgetName],
                      item,
                      deltaX, deltaY,
                      "%x", "%y"];
  
  if (postMoveSel && target)
    [target perform: postMoveSel];
  
  return self;
}

- (void)drop
{
  [globalTkInterp eval: "%s delete %s", [canvas getWidgetName], item];  
}

@end

