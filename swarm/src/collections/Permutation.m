// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Permutation.m
Description:  permutation object - array of integers 
Library:      collections
*/

#import <collections/Permutation.h>
#import <collections.h> // PermutationItem
#import <defobj/defalloc.h>

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

- (void)describe: outputCharStream
{
  [super describe: outputCharStream];
  [item describe: outputCharStream];
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
  lastPermutation = aPermutation;
  shuffler = ((Permutation_c *) aPermutation)->shuffler;
  return self;
}

- setUniformRandom: rnd
{
  shuffler = [[[ListShuffler createBegin: getCZone (getZone (self))]
                setUniformRandom: rnd]
               createEnd];
  return self;
}

- createEnd
{
  id elem, index;
  unsigned i;
  unsigned permutationCount = lastPermutation ? [lastPermutation getCount] : 0;

  count = [collection getCount];

  [super createEnd];

  if (collection == nil)
    raiseEvent (InvalidArgument, "Source collection required for Permutation");

  index = [collection begin: scratchZone];  
  for (elem = [index next], i = 0; i < count; elem = [index next], i++)
    {
      PermutationItem_c *pi = nil;

      if (i < permutationCount)
        {
          pi = [lastPermutation atOffset: i];
          if (pi->position < 0)
            pi = nil;
          else
            pi->position = i;
        }
      if (pi == nil)
        pi = [[[[PermutationItem createBegin: getCZone (getZone (self))]
                 setPosition: i]
                setItem: elem]
               createEnd];
      [self atOffset: i put: pi];
    }
  [index drop];
  [shuffler shuffleWholeList: self];
  return self;
}

PHASE(Setting)
PHASE(Using)

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

- (void)describe: outputCharStream
{
  char buffer[20];
  id index;
  id elem;

  [outputCharStream catC: "Permutation:\n"];
  index = [self begin: scratchZone];
  [index setLoc:Start];
  elem = [index next];
  while (elem) 
    {
       [elem describe: outputCharStream];
       elem = [index next];
       [outputCharStream catC: buffer];

    }
  sprintf (buffer,"\n");
  [index drop];
}

@end

