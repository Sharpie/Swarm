// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// generic 2d cellular automata.

#import <space.h> // Ca2d
#import <space/DblBuffer2d.h>

@interface Ca2d: DblBuffer2d <Ca2d>
{
  unsigned numStates;
}

// initializing the object
- setNumStates: (unsigned)n;
- initializeLattice;				  // must subclass
- createEnd;

// running the CA rule
- stepRule;					  // must subclass

@end
