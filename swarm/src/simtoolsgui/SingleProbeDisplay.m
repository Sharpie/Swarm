// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/SingleProbeDisplay.h>

@implementation SingleProbeDisplay

PHASE(Creating)

- setProbedObject: (id)anObject
{
  probedObject = anObject;

  return self;
}

- createEnd
{
  [super createEnd];
  [topLevel setWindowTitle: [self getId: NULL]];

  return self;
}

PHASE(Using)

- (void)setRemoveRef: (BOOL)theRemoveRef
{
  removeRef = theRemoveRef;
}

- (void)setObjectRef: (ref_t)theObjectRef
{
  objectRef = theObjectRef;
}

- getProbedObject
{
  return probedObject;
}

- (const char *)package: (const char *)windowName
{
  return [probedObject getObjectName];
}

- (const char *)getId: (const char *)windowName
{
  return [probedObject getIdName];
}

@end
