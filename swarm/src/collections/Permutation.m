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
      index = [collection begin: scratchZone];  
      touchedFlag = NO;
      untouchedFlag = NO;
      for (elem = [index next], i = 0;
           [index getLoc] == Member;
           elem = [index next], i++)
        {
          int direction = (lastPermutation
                           ? [lastPermutation getLastDirectionFor: elem]
                           : 0);
          if (direction == 1)
            touchedFlag = YES;
          else if (direction == 0)
            untouchedFlag = YES;
          [self atOffset: i
                put:
                  [[[[[PermutationItem createBegin: getCZone (getZone (self))]
                       setPosition: i]
                      setItem: elem]
                     setLastDirection: direction]
                    createEnd]];
        }
      [index drop];
      [shuffler shuffleWholeList: self];
    }
  return self;
}

PHASE(Setting)
PHASE(Using)
- (int)getLastDirectionFor: item
{
  int direction = 0;
  id index = [self begin: scratchZone];
  PermutationItem_c *pi;

  for (pi = [index next]; [index getLoc] == Member; pi = [index next])
    if (pi->position >= 0 && pi->item == item)
      {
        direction = pi->lastDirection;
        break;
      }
  [index drop];
  return direction;
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

- (void)describe: stream
{
  id index;
  id elem;

  [stream catC: "Permutation "];
  [stream catPointer: self];
  [stream catC: ":\n"];
  index = [self begin: scratchZone];
  [index setLoc:Start];
  for (elem = [index next]; [index getLoc] == Member; elem = [index next])
    {
      if (elem)
        [elem describe: stream];
      else
        [stream catC: "<null>\n"];
    }
  [index drop];
}

@end

