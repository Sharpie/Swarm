// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Permutation.m
Description:  permutation object - array of integers 
Library:      collections
*/

#import <collections/Permutation.h>
#import <defobj/defalloc.h>

@implementation Permutation_c

PHASE(Creating)

+ createBegin: aZone
{
  Permutation_c *obj = [super createBegin: aZone];

  obj->shuffler = [ListShuffler createBegin: aZone];
  return obj;
}

- setCollection: aCollection
{
  collection = aCollection;
  return self;
}

- setUniformRandom: rnd
{
  [shuffler setUniformRandom: rnd];
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

  shuffler = [shuffler createEnd];
  index = [collection begin: scratchZone];
  elem = [index next];   
  for (i = 0; i < count; i++)
    {
      [self atOffset: i put: elem];
      elem = [index next];
    }
  [index drop];
  [self generatePermutation];
  return self;
}

PHASE(Using)

- generatePermutation
{
  [shuffler shuffleWholeList: self];
  return self;
}

-(void)mapAllocations: (mapalloc_t) mapalloc
{
  mapObject (mapalloc, shuffler);
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

