// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// generic 2d cellular automata.

#import <space/DblBuffer2d.h>

@interface Ca2d: DblBuffer2d {
  int numStates;
}

// initializing the object
-setNumStates: (int) n;
-initializeLattice;				  // must subclass
-createEnd;

// running the CA rule
-stepRule;					  // must subclass

@end
