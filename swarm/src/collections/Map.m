// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Map.m
Description:  sorted map implemented as linear list 
Library:      collections
*/

#import <collections/Map.h>

#define compare( a, b ) \
(compareFunc ? compareFunc(a,b) : [a compare: b])
#define indexCompare( a, b ) \
(((Map_c *)collection)->compareFunc ? \
 ((Map_c *)collection)->compareFunc(a,b) : [a compare: b])


@implementation Map_c

PHASE(Creating)

+ createBegin: aZone
{
  Map_c *newMap;

  newMap = [aZone allocIVars: self];
  newMap->zone = aZone;
  return newMap;
}

- (void) setCompareFunction: (int(*)(id,id))compareFunction
{
  compareFunc = compareFunction;
}

- createEnd
{
  if ( ! createByMessageToCopy( self, createEnd ) ) {
    self->list = [List create: zone];
    setNextPhase( self );
  }
  return self;
}

PHASE(Using)

- at: aKey
{
  id       index, member;
  entry_t  anEntry;

  index = [list begin: scratchZone];
  for ( member = nil; (anEntry = (entry_t)[index next]); ) {
    if ( compare( anEntry->key, aKey ) == 0 ) {
      member = anEntry->member;
      break;
    }
  }
  [index drop];
  return member;
}

- (BOOL) at: aKey insert: anObject
{
  id       index;
  entry_t  newEntry, anEntry;
  int      result;

  newEntry = [zone allocBlock: sizeof *newEntry];
  newEntry->key    = aKey;
  newEntry->member = anObject;

  index = [list begin: scratchZone];
  result = 1;
  while ( (anEntry = (entry_t)[index next]) ) {
    if ( (result = compare( anEntry->key, aKey )) > 0 ) break;
  }
  [index addBefore: (id)newEntry];
  [index drop];
  count++;
  return ( result != 0 );
}

- at: aKey replace: anObject
{
  id       index, oldMem;
  entry_t  anEntry;

  index = [list begin: scratchZone];
  while ( (anEntry = (entry_t)[index next]) ) {
    if ( compare( anEntry->key, aKey ) == 0 ) {
      oldMem = anEntry->member;
      anEntry->member = anObject;
      [index drop];
      return oldMem;
    }
  }
  [index drop];
  return nil;
}

- (BOOL) at: aKey memberSlot: (id **)memPtr
{
  id       index;
  entry_t  anEntry, newEntry;
  int      result;

  index = [list begin: scratchZone];
  while ( (anEntry = (entry_t)[index next]) ) {
    if ( (result = compare( anEntry->key, aKey )) == 0 ) {
      [index drop];
      *memPtr = &anEntry->member;
      return 0;
    }
    if ( result > 0 ) break;
  }
  newEntry = [zone allocBlock: sizeof *newEntry];
  [index addBefore: (id)newEntry];
  [index drop];
  count++;
  newEntry->key = aKey;
  if ( *memPtr ) newEntry->member = **memPtr;
  *memPtr = &newEntry->member;
  return 1;
}

- (BOOL) at: aKey keySlot: (id **)keyPtr memberSlot: (id **)memPtr
{
  id       index;
  entry_t  anEntry, newEntry;
  int      result;

  index = [list begin: scratchZone];
  while ( (anEntry = (entry_t)[index next]) ) {
    if ( (result = compare( anEntry->key, aKey )) == 0 ) {
      [index drop];
      *keyPtr = &anEntry->key;
      *memPtr = &anEntry->member;
      return 0;
    }
    if ( result > 0 ) break;
  }
  newEntry = [zone allocBlock: sizeof *newEntry];
  [index addBefore: (id)newEntry];
  [index drop];
  count++;
  newEntry->key = aKey;
  *keyPtr = &newEntry->key;
  if ( *memPtr ) newEntry->member = **memPtr;
  *memPtr = &newEntry->member;
  return 1;
}

- removeKey: aKey
{
  id       index, oldMem;
  entry_t  anEntry;
  int      result;

  index = [list begin: scratchZone];
  oldMem = nil;
  while ( (anEntry = (entry_t)[index next]) ) {
    if ( (result = compare( anEntry->key, aKey )) == 0 ) {
      [index remove];
      oldMem = anEntry->member;
      count--;
      break;
    }
    if ( result > 0 ) break;
  }
  [index drop];
  return oldMem;
}

- begin: aZone
{
  MapIndex_c  *newIndex;

  newIndex = [aZone allocIVars: [MapIndex_c self]];
  newIndex->zone       = aZone;
  newIndex->collection = self;
  newIndex->listIndex  = [list begin: aZone];
  return newIndex;
}

- _createIndex_: aZone forIndexSubclass: anIndexSubclass
{
  MapIndex_c  *newIndex;

  newIndex = [aZone allocIVars: anIndexSubclass];
  newIndex->zone       = aZone;
  newIndex->collection = self;
  newIndex->listIndex  = [list begin: aZone];
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

@end

@implementation MapIndex_c

- next
{
  entry_t  anEntry;

  anEntry = (entry_t)[listIndex next];
  if ( anEntry != NULL ) return anEntry->member;
  return NULL;
}

- next: (id *)key
{
  entry_t  anEntry;

  anEntry = (entry_t)[listIndex next];
  if ( anEntry != NULL ) {
    if ( key ) *key = anEntry->key;
    return anEntry->member;
  }
  return nil;
}

- prev
{
  entry_t  anEntry;

  anEntry = (entry_t)[listIndex prev];
  if ( anEntry != NULL ) return anEntry->member;
  return NULL;
}

- prev: (id *)key
{
  entry_t  anEntry;

  anEntry = (entry_t)[listIndex prev];
  if ( anEntry != NULL ) {
    if ( key ) *key = anEntry->key;
    return anEntry->member;
  }
  return NULL;
}

- get
{
  entry_t  anEntry;

  anEntry = (entry_t)[listIndex get];
  if ( ! anEntry ) return nil;
  return anEntry->member;
}

- get: (id *)key
{
  entry_t  anEntry;

  anEntry = (entry_t)[listIndex get];
  if ( ! anEntry ) return nil;
  if ( key ) *key = anEntry->key;
  return anEntry->member;
}

- getKey
{
  entry_t  anEntry;

  anEntry = (entry_t)[listIndex get];
  if ( ! anEntry ) return nil;
  return anEntry->key;
}

- replace: anObject
{
  entry_t  anEntry;
  id       oldMem;

  anEntry = (entry_t)[listIndex get];
  if ( ! anEntry ) return nil;
  oldMem  = anEntry->member;
  anEntry->member = anObject;
  return oldMem;
}

- remove
{
  entry_t  anEntry;
  id       oldMem;

  anEntry = (entry_t)[listIndex remove];
  if ( ! anEntry ) return nil;
  oldMem  = anEntry->member;
  [collection->zone freeBlock: anEntry blockSize: sizeof *anEntry];
  collection->count--;
  return oldMem;
}

- setKey: aKey
{
  entry_t  anEntry;

  [listIndex setLoc: Start];
  while ( (anEntry = (entry_t)[listIndex next]) ) {
    if ( indexCompare( anEntry->key, aKey ) == 0 ) return anEntry->member;
  }
  [listIndex setLoc: Start];
  return nil;
}

- getLoc
{
  return [listIndex getLoc];
}

- (void) setLoc: locSymbol
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

@end
