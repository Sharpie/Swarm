// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
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

- setPosition: (int)thePosition
{
  position = thePosition;
  return self;
}
PHASE(Using)
- getItem
{
  return item;
}

- (int)getPosition
{
  return position;
}

- (void) describe: outputCharStream
{
  [super describe: outputCharStream];
  [item describe: outputCharStream];
}

@end

@implementation Permutation_c

PHASE(Creating)

+ createBegin: aZone
{
  Permutation_c *obj = [super createBegin: aZone];

  obj->shuffler = [ListShuffler createBegin: getCZone(aZone)];
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
  for (elem = [index next], i = 0; i < count; elem = [index next], i++)
    [self atOffset: i put: 
	    [[[[PermutationItem createBegin: getCZone ( getZone (self))]
		setPosition: i]
	       setItem: elem]
	      createEnd]];
  [index drop];
  [shuffler shuffleWholeList: self];
  return self;
}

PHASE(Using)

- getCollection
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

