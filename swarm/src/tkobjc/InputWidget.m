// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <tkobjc/InputWidget.h>
#include "internal.h"
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

- (void)linkVariableInt: (int *)p
{
  relink (variableName, p, TCL_LINK_INT);
}

- (void)linkVariableDouble: (double *)p
{
  relink (variableName, p, TCL_LINK_DOUBLE);
}

- (void)linkVariableBoolean: (unsigned *)p
{
  relink (variableName, p, TCL_LINK_BOOLEAN);
}

- (const char *)getValue
{
  raiseEvent (SubclassMustImplement, "getValue");
  return NULL;
}

- (void)setValue: (const char *)v
{
  raiseEvent (SubclassMustImplement, "setValue:");
}

@end
