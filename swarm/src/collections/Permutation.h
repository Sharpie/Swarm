// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Permutation.h
Description:  permutation object - array of integers 
Library:      simtools
*/

#include <collections/Array.h>
#import <collections/ListShuffler.h>


@interface Permutation_c: Array_c
{
  int maxElement;                
  int minElement; 
  id uniformRandom;
  id shuffler;
}
+ createBegin: (id) aZone;
- (void) setMaxElement: (int) max;
- (void) setMinElement: (int) min;
- (void) setUniformRandom: (id) rnd;
- createEnd;
- generatePermutation;
- (void) describe: outputCharStream;

@end


