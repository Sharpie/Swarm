// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// ListShuffler -> randomize the order of the elements in a list

// Contributed by Sven Thommesen <sthomme@humsci.auburn.edu> 1998-08-21
// Reworked from Ted Belding's Shuffler.
// (Algorithm from Knuth vol 2)

#import <objectbase/SwarmObject.h>

@interface ListShuffler : SwarmObject 
{
  id uniformRandom;		// uniform unsigned distribution object
}

+ createBegin: aZone;
- setUniformRandom: rnd;
- createEnd;

+ create: aZone setUniformRandom: dist;

- shufflePartialList: list Num: (int)num;
- shuffleWholeList: list;
@end

