// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.


#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <tclObjc.h>
#import <Tk.h>
#import <tkobjc/Entry.h>

@implementation Entry

-createEnd {
  [super createEnd];

  // create the Entry
  [globalTkInterp eval: "entry %s; %s configure -width 10 -relief sunken;", widgetName, widgetName];
  [globalTkInterp eval: "%s configure -textvariable %s;", widgetName, variableName];
  return self;
}


-setValue: (char *) t {
  [globalTkInterp eval: "%s delete 0 end; %s insert 0 \"%s\"",
		  widgetName, widgetName, t];
  return self;
}

// ignore the height: doesn't work for entries.
-setWidth: (unsigned) w Height: (unsigned) h {
  [globalTkInterp eval: "%s configure -width %u", widgetName, w];
  return self;
}

@end

