// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/Canvas.h>
#import <tkobjc/global.h>

@implementation Canvas

PHASE(Creating)

- createEnd
{
  [super createEnd];
  [globalTkInterp eval: "canvas %s", widgetName];
  return self;
}

PHASE(Using)

- addWidget: widget X: (int)x Y: (int)y centerFlag: (BOOL)centerFlag
{
  [globalTkInterp eval: "%s create window %d %d -anchor %s -window %s",
                  [self getWidgetName],
                  x, y,
                  (centerFlag ? "w" : "nw"),
                  [widget getWidgetName]];
  return self;
}

- removeWidget: widget
{
  [globalTkInterp eval: "%s delete %s", 
                  [self getWidgetName],
                  [widget getWidgetName]];
  return self;
}

@end
