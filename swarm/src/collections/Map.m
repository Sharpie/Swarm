// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Map.m
Description:  sorted map implemented as linear list 
Library:      collections
*/

#import <collections/Map.h>
#import <defobj/defalloc.h>

//
// compareIDs --
//   function to compare two id values based on the unsigned magnitudes of
//   their id values
//               
//
int
compareIDs (id val1, id val2)
{
  if ((unsigned) val1 < (unsigned) val2)
    return -1;
  return ((unsigned) val1 > (unsigned) val2);
}

//
// compareIntegers --
//   function to compare two signed integer values stored within id values
//
int
compareIntegers (id val1, id val2)
{
  if ((int) val1 < (int) val2)
    return -1;
  return ((int) val1 > (int) val2);
}

//
// compare -- internal macro for selection of compare technique
//
#define compare(a, b) \
(compareFunc ? compareFunc(a,b) : [a compare: b])
#define indexCompare(a, b) \
(((Map_c *) collection)->compareFunc ? \
 ((Map_c *) collection)->compareFunc(a,b) : [a compare: b])


@implementation Map_c

PHASE(Creating)

+ createBegin: aZone
{
  Map_c *newMap;

  newMap = [aZone allocIVars: self];
  return newMap;
}

- (void) setCompareFunction: (int(*)(id,id))compareFunction
{
  compareFunc = compareFunction;
}

- createEnd
{
  if (createByMessageToCopy (self, createEnd))
    return self;
  self->list = [List create: getCZone (getZone (self))];
  setMappedAlloc (self);
  setNextPhase (self);
  return self;
}

PHASE(Using)

//
// copy: -- standard method to copy internal state of object
//
- copy: aZone
{
  Map_c *newMap;
  id index;
  mapentry_t entry, newEntry;

  newMap = [aZone allocIVars: getClass (self)];
  setMappedAlloc (newMap);
  newMap->list = [List create: getCZone (getZone (self))];
  index = [list begin: scratchZone];
  while ((entry = (mapentry_t) [index next]))
    {
      newEntry = [getZone (self) allocBlock: sizeof *entry];
      memcpy (newEntry, entry, sizeof *entry);
      [newMap->list addLast: (id) newEntry];
    }
  [index drop];
  return newMap;
}


- at: aKey
{
  id index, member;
  mapentry_t  anEntry;

  index = [list begin: scratchZone];
  for (member = nil; (anEntry = (mapentry_t) [index next]); )
    {
      if (compare (anEntry->key, aKey) == 0)
        {
          member = anEntry->member;
          break;
        }
    }
  [index drop];
  return member;
}

- (BOOL)at: aKey insert: anObject
{
  id index;
  mapentry_t newEntry, anEntry;
  int result;

  newEntry = [getZone (self) allocBlock: sizeof *newEntry];
  newEntry->key = aKey;
  newEntry->member = anObject;

  index = [list begin: scratchZone];
  while ((anEntry = (mapentry_t)[index next]))
    if ((result = compare (anEntry->key, aKey)) == 0)
      {
        [index drop];
        return NO;
      }
    else if (result > 0)
      break;
  [index addBefore: (id)newEntry];
  [index drop];
  count++;
  return YES;
}

- at: aKey replace: anObject
{
  id index, oldMem;
  mapentry_t anEntry;

  index = [list begin: scratchZone];
  while ((anEntry = (mapentry_t) [index next]))
    {
      if (compare (anEntry->key, aKey) == 0)
        {
          oldMem = anEntry->member;
          anEntry->member = anObject;
          [index drop];
          return oldMem;
        }
    }
  [index drop];
  return nil;
}

- (BOOL)at: aKey memberSlot: (id **)memPtr
{
  id index;
  mapentry_t anEntry, newEntry;
  int result;

  index = [list begin: scratchZone];
  while ((anEntry = (mapentry_t)[index next]))
    {
      if ((result = compare (anEntry->key, aKey)) == 0)
        {
          [index drop];
          *memPtr = &anEntry->member;
          return NO;
        }
      if (result > 0)
        break;
    }
  newEntry = [getZone (self) allocBlock: sizeof *newEntry];
  [index addBefore: (id) newEntry];
  [index drop];
  count++;
  newEntry->key = aKey;
  if (*memPtr)
    newEntry->member = **memPtr;
  *memPtr = &newEntry->member;
  return YES;
}

- (BOOL)at: aKey keySlot: (id **)keyPtr memberSlot: (id **)memPtr
{
  id index;
  mapentry_t anEntry, newEntry;
  int result;

  index = [list begin: scratchZone];
  while ((anEntry = (mapentry_t)[index next]))
    {
      if ((result = compare (anEntry->key, aKey)) == 0)
        {
          [index drop];
          *keyPtr = &anEntry->key;
          *memPtr = &anEntry->member;
          return NO;
      }
      if (result > 0)
        break;
    }
  newEntry = [getZone (self) allocBlock: sizeof *newEntry];
  [index addBefore: (id) newEntry];
  [index drop];
  count++;
  newEntry->key = aKey;
  *keyPtr = &newEntry->key;
  if (*memPtr)
    newEntry->member = **memPtr;
  *memPtr = &newEntry->member;
  return YES;
}

- removeKey: aKey
{
  id index, oldMem;
  mapentry_t anEntry;
  int result;

  index = [list begin: scratchZone];
  oldMem = nil;
  while ((anEntry = (mapentry_t) [index next]))
    {
      if ((result = compare (anEntry->key, aKey)) == 0)
        {
          [index remove];
          oldMem = anEntry->member;
          [getZone (self) freeBlock: anEntry blockSize: sizeof *anEntry];
        count--;
        break;
        }
      if (result > 0)
        break;
    }
  [index drop];
  return oldMem;
}

- begin: aZone
{
  MapIndex_c *newIndex;
  
  newIndex = [aZone allocIVars: [MapIndex_c self]];
  setMappedAlloc (newIndex);
  newIndex->collection = self;
  newIndex->listIndex = [list begin: getCZone (aZone)];
  return newIndex;
}

- _createIndex_: aZone forIndexSubclass: anIndexSubclass
{
  MapIndex_c *newIndex;

  newIndex = [aZone allocIVars: anIndexSubclass];
  setMappedAlloc (newIndex);
  newIndex->collection = self;
  newIndex->listIndex  = [list begin: getCZone (aZone)];
  return newIndex;
}

- createIndex: aZone fromMember: anObject
{
  return nil;
}

- createIndex: aZone fromKey: aKey
{
  return nil;
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  id index;
  mapentry_t anEntry;

  if (includeBlocks (mapalloc))
    {
      mapalloc->size = sizeof *anEntry;
      index = [list begin: scratchZone];
      while ((anEntry = (mapentry_t) [index next]))
        mapAlloc (mapalloc, anEntry);
      [index drop];
    }
  mapObject (mapalloc, list);
}

- lispin: expr
{
  return self;
}

- lispout: stream
{
  return self;
}

@end

@implementation MapIndex_c

- next
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) [listIndex next];
  if (anEntry != NULL)
    return anEntry->member;
  return NULL;
}

- next: (id *)key
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) [listIndex next];
  if (anEntry != NULL)
    {
      if (key)
        *key = anEntry->key;
      return anEntry->member;
    }
  return nil;
}

- prev
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) [listIndex prev];
  if (anEntry != NULL)
    return anEntry->member;
  return NULL;
}

- prev: (id *)key
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) [listIndex prev];
  if (anEntry != NULL)
    {
      if (key) 
        *key = anEntry->key;
      return anEntry->member;
    }
  return NULL;
}

- get
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) [listIndex get];
  if (!anEntry)
    return nil;
  return anEntry->member;
}

- get: (id *)key
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) [listIndex get];
  if (!anEntry)
    return nil;
  if (key) 
    *key = anEntry->key;
  return anEntry->member;
}

- getKey
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) [listIndex get];
  if (!anEntry)
    return nil;
  return anEntry->key;
}

- replace: anObject
{
  mapentry_t anEntry;
  id oldMem;

  anEntry = (mapentry_t) [listIndex get];
  if (!anEntry)
    return nil;
  oldMem  = anEntry->member;
  anEntry->member = anObject;
  return oldMem;
}

- remove
{
  mapentry_t anEntry;
  id oldMem;

  anEntry = (mapentry_t)[listIndex remove];
  if (!anEntry)
    return nil;
  oldMem  = anEntry->member;
  [getZone (collection) freeBlock: anEntry blockSize: sizeof *anEntry];
  collection->count--;
  return oldMem;
}

- setKey: aKey
{
  mapentry_t anEntry;

  [listIndex setLoc: Start];
  while ((anEntry = (mapentry_t) [listIndex next]))
    {
      if (indexCompare (anEntry->key, aKey) == 0)
        return anEntry->member;
    }
  [listIndex setLoc: Start];
  return nil;
}

- getLoc
{
  return [listIndex getLoc];
}

- (void) setLoc: (id <Symbol>)locSymbol
{
  [listIndex setLoc: locSymbol];
}

- (int) getOffset
{
  return [listIndex getOffset];
}

- setOffset: (int)offset
{
  return [listIndex setOffset: offset];
}

- (void) mapAllocations: (mapalloc_t)mapalloc
{
  mapObject (mapalloc, listIndex);
}

@end
