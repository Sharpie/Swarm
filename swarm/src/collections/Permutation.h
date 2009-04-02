// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

/*
Name:         Permutation.h
Description:  permutation object - array of integers 
Library:      collections
*/

#import <Swarm/Array.h>

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
- (BOOL)getTouchedFlag;
- (BOOL)getUntouchedFlag;
- (id <Collection>)getCollection;
- (void *)createTree;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end
