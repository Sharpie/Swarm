// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// ListShuffler -> randomize the order of the elements in a list

// Contributed by Sven Thommesen <sthomme@humsci.auburn.edu> 1998-08-21
// Reworked from Ted Belding's Shuffler.
// (Algorithm from Knuth vol 2)

#import <defobj/Create.h>

@interface ListShuffler_c : CreateDrop_s
{
  id uniformRandom;		// uniform unsigned distribution object
}

+ createBegin: aZone;
+ create: aZone setUniformRandom: dist;

- (void) setUniformRandom: rnd;
- createEnd;


- shufflePartialList: list Num: (int)num;
- shuffleWholeList: list;
@end

