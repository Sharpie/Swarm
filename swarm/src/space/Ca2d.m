// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/Ca2d.h>

@implementation Ca2d

PHASE(Creating)

- setNumStates: (int)n
{
  numStates = n;
  return self;
}

- initializeLattice
{
  [SubclassMustImplement raiseEvent];
  return nil;
}

- createEnd
{
  // allocate buffers.
  if (numStates == 0)
    [InvalidCombination raiseEvent: "CA not initialized correctly.\n"];
  
  [super createEnd];
  
  // initialize ourselves.
  [self initializeLattice];
  return self;
}

PHASE(Using)

- stepRule
{
  [SubclassMustImplement raiseEvent: "Ca2d: no default step.\n"];
  return nil;
}

@end
