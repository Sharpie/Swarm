// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "internal.h"
#import <tkobjc/global.h>
#import <tkobjc/InputWidget.h>
#include <misc.h>

@implementation InputWidget

PHASE(Creating)

// you shouldn't instantiate this yourself.
- createEnd
{
  const char *objectName = [self getObjectName];
  char *buf;
  [super createEnd];
  
  buf = xmalloc (strlen (objectName) + 5);
  sprintf (buf, "%s-var", objectName);
  variableName = buf;
  
  return self;
}

static void
relink (const char *variableName, void *p, int type)
{
  // unlink anything there
  tkobjc_unlinkVar (variableName);
  tkobjc_linkVar (variableName, p, type);
}

PHASE(Using)

- linkVariableInt: (int *)p
{
  relink (variableName, p, TCL_LINK_INT);

  return self;
}

- linkVariableDouble: (double *)p
{
  relink (variableName, p, TCL_LINK_DOUBLE);

  return self;
}

- linkVariableBoolean: (BOOL *)p
{
  relink (variableName, p, TCL_LINK_BOOLEAN);

  return self;
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
