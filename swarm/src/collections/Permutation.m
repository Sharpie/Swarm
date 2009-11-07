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
Name:         Permutation.m
Description:  permutation object - array of integers 
Library:      collections
*/

#import <collections/Permutation.h>
#import <collections.h> // PermutationItem
#import <defobj/defalloc.h>

#include <misc/avl.h>

@implementation PermutationItem_c
PHASE(Creating)
- setItem: theItem
{
  item = theItem;
  return self;
}

- setPosition: (unsigned)thePosition
{
  position = thePosition;
  return self;
}

PHASE(Setting)
- setLastDirection: (int)theLastDirection
{
  lastDirection = theLastDirection;
  return self;
}
PHASE(Using)

- getItem
{
  return item;
}

- (unsigned)getPosition
{
  return position;
}

- (void)describe: stream
{
  [super describe: stream];
  [item describe: stream];
  [stream catC: "lastDirection: "];
  [stream catInt: lastDirection];
  [stream catC: " position: "];
  [stream catUnsigned: position];
  [stream catC: "\n"];
}

@end

@implementation Permutation_c
PHASE(Creating)

- setCollection: (id <Collection>)aCollection
{
  collection = aCollection;
  return self;
}

- setLastPermutation: (id <Permutation>)aPermutation
{
  lastPermutation =  (Permutation_c *)aPermutation;
  return self;
}

- setUniformRandom: theRnd
{
  rnd = theRnd;
  return self;
}

- createEnd
{
  id elem, index;
  unsigned i;

  count = [collection getCount];

  [super createEnd];

  if (collection == nil)
    raiseEvent (InvalidArgument, "Source collection required for Permutation");

  shuffler = [ListShuffler createBegin: getCZone (getZone (self))];
  if (rnd)
    [shuffler setUniformRandom: rnd];
  shuffler = [shuffler createEnd];

  if (count > 0)
    {
      void *tree = NULL;

      index = [collection begin: scratchZone];  
      touchedFlag = NO;
      untouchedFlag = NO;

      if (lastPermutation)
        tree = [lastPermutation createTree];
      
      for (elem = [index next], i = 0;
           [index getLoc] == Member;
           elem = [index next], i++)
        {
          int direction = 0;
          
          PermutationItem_c *pi =
            [[[(PermutationItem_c *)[PermutationItem createBegin: getCZone (getZone (self))]
                setPosition: i]
               setItem: elem]
              createEnd];
          
          if (lastPermutation)
            {
              PermutationItem_c *old = avl_find (tree, pi);
              
              if (old && old->position >= 0)
                direction = old->lastDirection;
            }
          if (direction == 1)
            touchedFlag = YES;
          else if (direction == 0)
            untouchedFlag = YES;
          pi->lastDirection = direction;
          [self atOffset: i put: pi];
        }
      if (lastPermutation)
        avl_destroy (tree, NULL);

      [index drop];
      [shuffler shuffleWholeList: self];
    }
  return self;
}

PHASE(Setting)
PHASE(Using)

static int
comparePermutationItems (const void *a, const void *b, void *param)
{
  return ((PTRUINT) ((PermutationItem_c *) a)->item -
          (PTRUINT) ((PermutationItem_c *) b)->item);
}

- (void *)createTree
{
  unsigned i;
  void *tree;

  tree = avl_create (comparePermutationItems, NULL);

  for (i = 0; i < count; i++)
    {
      PermutationItem_c *pi = block[i];

      if (pi->lastDirection)
        avl_probe (tree, pi);
    }
  return tree;
}

- (BOOL)getTouchedFlag
{
  return touchedFlag;
}

- (BOOL)getUntouchedFlag
{
  return untouchedFlag;
}

- (id <Collection>)getCollection
{
  return collection;
}

-(void)mapAllocations: (mapalloc_t) mapalloc
{
  id elem;
  id index = [self begin: scratchZone];
  unsigned i;

  for (elem = [index next], i = 0; i < count; elem = [index next], i++)
    mapObject (mapalloc, elem);

  [index drop];
  mapObject (mapalloc, shuffler);
  [super mapAllocations: mapalloc];  
}

@end

