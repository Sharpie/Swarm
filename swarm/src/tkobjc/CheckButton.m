// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <Tk.h>
#import <tkobjc/CheckButton.h>

inline int
stringIsFalse(const char * s) {
  return s[0] == '0' && s[1] == '\0';
}

@implementation CheckButton

-createEnd {
  [super createEnd];

  // create the checkbutton
  [globalTkInterp eval: "checkbutton %s;", widgetName];
  [globalTkInterp eval: "%s configure -variable %s;", widgetName,
		  variableName];
  return self;
}

-setBoolValue: (BOOL) v {
  if (v)
    [globalTkInterp eval: "%s select;", widgetName];
  else
    [globalTkInterp eval: "%s deselect;", widgetName];
  return self;
}

-setValue: (char *) v {
  return [self setBoolValue: stringIsFalse(v)];
}


// just ignore this entirely - does it mean anything?
-setWidth: (int) w Height: (int) h {
  return self;
}

-(BOOL) getBoolValue {
  const char * v;
  v = [self getValue];
  if (stringIsFalse(v))
    return 0;
  else
    return 1;
}

// could do setvalue with Tcl select/deselect
@end
