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


- (void)setMaxElement: (int)max
{
  maxElement = max;
}

- (void)setMinElement: (int)min
{
  if (min < 1)
    raiseEvent (InvalidArgument, "> Minimal element of a permutation"
               " must be greater than zero");
  minElement = min;

}

- (void)setUniformRandom: rnd
{
  [shuffler setUniformRandom: rnd];
}

- createEnd
{
  int i;
  //id obj;
  if (maxElement <= minElement)
    raiseEvent (InvalidArgument,
                " > maximumElement of permutation is less or/n "
                " > equal to the minimumElement /n");
  
  [super setCount: maxElement - minElement + 1];
  [super createEnd];

  shuffler = [shuffler createEnd];
     
  for (i = minElement; i <= (int) maxElement; i++)
    [super atOffset: i - minElement put: (id) i];
  
  return self;
}

PHASE(Using)

- generatePermutation
{
  [shuffler shuffleWholeList: self];
  return self;
}

- (void)describe: outputCharStream
{
  char buffer[20];
  id index;
  int elem;

  [outputCharStream catC: "Permutation:\n"];
  index = [self begin: scratchZone];
  [index setLoc:Start];
  elem = (int) [index next];
  while (elem) 
    {
       sprintf (buffer, " %d ", elem);
       elem = (int) [index next];
       [outputCharStream catC: buffer];

    }
  sprintf (buffer,"\n");
  [index drop];
}

@end

