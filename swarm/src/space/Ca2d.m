// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/Ca2d.h>
#import <defobj.h>

@implementation Ca2d

PHASE(Creating)

- setNumStates: (unsigned)n
{
  numStates = n;
  return self;
}

- initializeLattice
{
  raiseEvent (SubclassMustImplement,
              "initializeLattice must be implemented by Ca2d subclass");
  return nil;
}

- createEnd
{
  // allocate buffers.
  if (numStates == 0)
    raiseEvent (InvalidCombination, "CA not initialized correctly.\n");

  [super createEnd];
  
  // initialize ourselves.
  [self initializeLattice];
  return self;
}
PHASE(Setting)
PHASE(Using)

- stepRule
{
  raiseEvent (SubclassMustImplement, "Ca2d: no default step.\n");
  return nil;
}

@end
