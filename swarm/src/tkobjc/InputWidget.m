// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "internal.h"
#import <tkobjc/global.h>
#import <tkobjc/InputWidget.h>

#include <stdlib.h>

@implementation InputWidget

// you shouldn't instantiate this yourself.
- createEnd
{
  char *buf;
  [super createEnd];
  
  buf = malloc (strlen ([self getObjcName]) + 5);
  sprintf (buf, "%s-var", [self getObjcName]);
  variableName = buf;
  
  return self;
}

// link the supplied variable to the variable the input widget is using
- linkVariable: (void *)p Type: (int)type
{
  // unlink anything there
  tkobjc_unlinkVar (variableName);
  tkobjc_linkVar (variableName, p, type);
  return self;
}

- linkVariableInt: (void *)p
{
  return [self linkVariable: p Type: TCL_LINK_INT];
}

- linkVariableDouble: (void *)p
{
  return [self linkVariable: p Type: TCL_LINK_DOUBLE];
}

- linkVariableBoolean: (void *)p
{
  return [self linkVariable: p Type: TCL_LINK_BOOLEAN];
}

- (const char *)getValue
{
  [globalTkInterp eval: "%s get", widgetName];
  return [globalTkInterp result];
}

- setValue: (const char *)v
{
  [SubclassMustImplement raiseEvent];
  return nil;
}

@end
