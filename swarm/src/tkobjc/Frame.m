// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// TopLevels and Frames are about the same, so we put these in one class.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <Tk.h>
#import <tkobjc/Frame.h>

@implementation Frame

// make a new top level frame. Can't use Widget default createEnd, because
// this is where the toplevel is actually built.
- createEnd
{
  if (parent == nil)
    {
      [self makeNameFromParentName: "."];
      [globalTkInterp eval: "toplevel %s; wm minsize %s 1 1",
                      widgetName, widgetName];
      [self registerAndLoad];
    }
  else
    {
      [super createEnd];
      [globalTkInterp eval: "frame %s", widgetName];
    }
  return self;
}

- (void)drop
{
  if (parent == nil)
    [globalTkInterp eval: "destroy %s", [self getWidgetName]]; 
}

@end

