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
  Permutation_c  *newPermutation;

  newPermutation = [aZone allocIVars: self];
  newPermutation->minElement = 1;
  newPermutation->maxElement = 0;
  newPermutation->shuffler = [ListShuffler createBegin: aZone];
  
  return newPermutation;
}


- setMaxElement: (unsigned)max
{
  maxElement = max;
  return self;
}

- setMinElement: (unsigned)min
{
  if (min < 1)
    raiseEvent (InvalidArgument, "> Minimal element of a permutation"
               " must be greater than zero");
  minElement = min;
  return self;
}

- setUniformRandom: rnd
{
  [shuffler setUniformRandom: rnd];
  return self;
}

- createEnd
{
  unsigned i;

  if (maxElement <= minElement)
    raiseEvent (InvalidArgument,
                " > maximumElement of permutation is less or/n "
                " > equal to the minimumElement /n");
  
  [self setCount: maxElement - minElement + 1];
  [super createEnd];

  shuffler = [shuffler createEnd];
     
  for (i = minElement; i <= maxElement; i++)
    [self atOffset: i - minElement put: (id) i];
  
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
  mapObject(mapalloc, shuffler);
}

- (void)describe: outputCharStream
{
  char buffer[20];
  id index;
  unsigned elem;

  [outputCharStream catC: "Permutation:\n"];
  index = [self begin: scratchZone];
  [index setLoc:Start];
  elem = (unsigned) [index next];
  while (elem) 
    {
       sprintf (buffer, " %u ", elem);
       elem = (unsigned) [index next];
       [outputCharStream catC: buffer];

    }
  sprintf (buffer,"\n");
  [index drop];
}

@end

