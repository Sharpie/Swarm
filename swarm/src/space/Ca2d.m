// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/Ca2d.h>

//S: Defines abstract protocol for cellular automata.

//D: Inherits from DblBuffer2d, defines abstract protocol
//D: for cellular automata. 

@implementation Ca2d

//M: Record the number of states the CA understands.
- setNumStates: (int)n
{
  numStates = n;
  return self;
}

//M: Check that numStates has been set.
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

//M: Use this to set up your CA to a default initial state.
//M: Unimplemented in Ca2d; subclass this to set up initial state of lattice.
- initializeLattice
{
  [SubclassMustImplement raiseEvent];
  return nil;
}

//M: One iteration of the CA rule.
//M: Unimplemented in Ca2d; subclass this to implement your CA rule.
- stepRule
{
  [SubclassMustImplement raiseEvent: "Ca2d: no default step.\n"];
  return nil;
}

@end
