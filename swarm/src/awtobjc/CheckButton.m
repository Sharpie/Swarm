// Swarm library. Copyright (C) 1996, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <awtobjc/CheckButton.h>

inline int
stringIsFalse(const char * s) {
  return s[0] == '0' && s[1] == '\0';
}

@implementation CheckButton

- createEnd
{
  [super createEnd];

  abort ();
  return self;
}

- setBoolValue: (BOOL)v
{
  abort ();
  return self;
}

- setValue: (const char *)v
{
  return [self setBoolValue: stringIsFalse (v)];
}

// just ignore this entirely - does it mean anything?
- setWidth: (unsigned)w Height: (unsigned)h
{
  return self;
}

- (BOOL)getBoolValue
{
  const char * v;
  v = [self getValue];
  if (stringIsFalse (v))
    return 0;
  else
    return 1;
}

@end




