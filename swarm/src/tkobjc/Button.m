// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <tclObjc.h>
#import <Tk.h>
#import <tkobjc/Button.h>

@implementation Button

-createEnd {
  [super createEnd];

  // create the button
  [globalTkInterp eval: "button %s", widgetName];
  
  return self;
}

-setText: (char *) t {
  [globalTkInterp eval: "%s configure -text \"%s\"", widgetName, t];
  return self;
}

-setCommand: (char *) c {
  [globalTkInterp eval: "%s configure -command \"%s\"", widgetName, c];
  return self;
}

@end

