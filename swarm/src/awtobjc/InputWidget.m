// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/InputWidget.h>

#include <stdlib.h>

@implementation InputWidget

// you shouldn't instantiate this yourself.
- createEnd
{
  char *buf;
  [super createEnd];
  
  buf = malloc (strlen ([self getObjectName]) + 5);
  sprintf (buf, "%s-var", [self getObjectName]);
  variableName = buf;
  
  return self;
}

// link the supplied variable to the variable the input widget is using
- linkVariable: (void *)p Type: (int)type
{
  printf ("linkVariable");
  return self;
}

- linkVariableInt: (void *)p
{
  printf ("linkVariableInt\n");
  return self;
}

- (const char *)getValue
{
  printf ("InputWidget getValue\n");
  return "InputWidget-getValue-not-done";
}

- setValue: (const char *)v
{
  [SubclassMustImplement raiseEvent];
  return nil;
}

@end
