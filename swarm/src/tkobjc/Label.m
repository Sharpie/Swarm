// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <TkInterp.h>
#import <tkobjc/Label.h>

@implementation Label

- createEnd
{
  [super createEnd];

  // create the label
  [globalTkInterp eval: "label %s", widgetName];
  
  return self;
}

- setText: (const char *)text
{
  [globalTkInterp eval: "%s configure -text \"%s\"", widgetName, text];
  return self;
}

- anchorEast
{
  [globalTkInterp eval: "%s configure -anchor e",
                  [self getWidgetName]];
  return self;
}

- anchorWest
{
  [globalTkInterp eval: "%s configure -anchor w",
                  [self getWidgetName]];
  return self;
}

- colorBlue
{
  [globalTkInterp eval: "%s configure -foreground blue",
                  [self getWidgetName]];
  return self;
}

@end

