// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/InputWidget.h>
#import "internal.h"
#import <tkobjc/global.h> // globalTkInterp
#include <misc.h> // strlen

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

- linkVariableBoolean: (unsigned *)p
{
  relink (variableName, p, TCL_LINK_BOOLEAN);

  return self;
}

- (const char *)getValue
{
  raiseEvent (SubclassMustImplement, "getValue");
  return NULL;
}

- setValue: (const char *)v
{
  raiseEvent (SubclassMustImplement, "setValue:");

  return nil;
}

@end
