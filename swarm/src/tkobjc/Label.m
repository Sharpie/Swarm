// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <Tk.h>
#import <tkobjc/Label.h>

@implementation Label

-createEnd {
  [super createEnd];

  // create the label
  [globalTkInterp eval: "label %s", widgetName];
  
  return self;
}

-setText: (char *) t {
  [globalTkInterp eval: "%s configure -text \"%s\"", widgetName, t];
  return self;
}

@end

