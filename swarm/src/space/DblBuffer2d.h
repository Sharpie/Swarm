// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// 2d buffered space.

#import <space/Discrete2d.h>

@interface DblBuffer2d: Discrete2d {
  id *wBuf1, *wBuf2;				  // store data here

@public						  // only for the inlines
  id *newLattice;				  // pointer to lattice
}

// getting the entire lattice (use carefully!)
-(id *) getNewLattice;			  // for writing

// use after you've done writing on the lattice: synchronize old and new.
-updateLattice;

@end
