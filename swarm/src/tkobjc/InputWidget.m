// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#import <tkobjc/global.h>
#import <Tk.h>
#import <tkobjc/InputWidget.h>

@implementation InputWidget

// you shouldn't instantiate this yourself.
-createEnd {
  [super createEnd];

  variableName = malloc(strlen([self getObjcName]) + 5);
  sprintf(variableName, "%s-var", [self getObjcName]);

  return self;
}

// link the supplied variable to the variable the input widget is using
-linkVariable: (void *) p Type: (int) type{
  Tcl_UnlinkVar([globalTkInterp interp], variableName); // unlink anything there
  Tcl_LinkVar([globalTkInterp interp], variableName, p, type);
  return self;
}

-(const char *) getValue {
  [globalTkInterp eval: "%s get", widgetName];
  return [globalTkInterp result];
}

-setValue: (char *) v {
  [SubclassMustImplement raiseEvent];
  return nil;
}
@end
