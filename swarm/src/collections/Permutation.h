// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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
}
- setItem: item;
- setPosition: (int)position;
- getItem;
- (int)getPosition;
@end

@interface Permutation_c: Array_c
{
  @public
   id collection;
   id uniformRandom;
   id shuffler;
}
+ createBegin: aZone;
- setCollection: collection;
- setUniformRandom: rnd;
- createEnd;
- getCollection;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (void)describe: outputCharStream;
@end


