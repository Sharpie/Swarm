// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Permutation.h
Description:  permutation object - array of integers 
Library:      collections
*/

#import <collections/Array.h>

@interface PermutationItem_c: CreateDrop_s
{
@public
  id item;
  int position; // negative is to indicate removed
  int lastDirection; // -1 = prev, 0 = none, 1 = next
}
- setItem: item;
- setPosition: (unsigned)position;
- setLastDirection: (int)lastDirection;
- getItem;
- (unsigned)getPosition;
- (void)describe: outputCharStream;
@end

@interface Permutation_c: Array_c <Permutation>
{
@public
   id <Collection> collection;
   id <ListShuffler> shuffler;
   id rnd;
@protected
   Permutation_c *lastPermutation; 
   BOOL touchedFlag;
   BOOL untouchedFlag;
}
- setCollection: (id <Collection>)collection;
- setUniformRandom: rnd;
- setLastPermutation: (id <Permutation>)permutation;
- createEnd;
- (int)getLastDirectionFor: item;
- (BOOL)getTouchedFlag;
- (BOOL)getUntouchedFlag;
- (id <Collection>)getCollection;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)describe: outputCharStream;
@end


