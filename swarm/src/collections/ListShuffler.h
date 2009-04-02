// Swarm library. Copyright © 1996, 2000 Swarm Development Group. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// ListShuffler -> randomize the order of the elements in a list

// Contributed by Sven Thommesen <sthomme@humsci.auburn.edu> 1998-08-21
// Reworked from Ted Belding's Shuffler.
// (Algorithm from Knuth vol 2)

#import <Swarm/Create.h>
#import <Swarm/collections.h>

@interface ListShuffler_c: CreateDrop_s <ListShuffler>
{
  id uniformRandom; // uniform unsigned distribution object
}

+ create: aZone setUniformRandom: dist;
- setUniformRandom: rnd;
- createEnd;

- shufflePartialList: list Num: (unsigned)num;
- shuffleWholeList: list;
@end

