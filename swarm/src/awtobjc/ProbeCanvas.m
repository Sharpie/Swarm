// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/ProbeCanvas.h>

@implementation ProbeCanvas

- setHorizontalScrollbarFlag: (BOOL)theHorizontalScrollbarFlag
{
  horizontalScrollbarFlag = theHorizontalScrollbarFlag;
  return self;
}

- createEnd
{
  const char *canvasName, *frameName;

  [self setClassIdFromSwarmName: "Frame"];
  [super createEnd];

  canvasName = [self getWidgetName];
  frameName = [parent getWidgetName];

  if (horizontalScrollbarFlag)
    printf ("ProbeCanvas: with horizontal scroll bar\n");
  else
    printf ("ProbeCanvas: without horizontal scroll bar\n");
  return self;
}

@end
