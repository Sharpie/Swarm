// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Map.h
Description:  sorted map implemented as linear list 
Library:      collections
*/

#import <collections/Collection.h>

//
// struct mapentry, mapentry_t -- internal node containing key-member pair
//
typedef struct mapentry {
  id  key;
  id  member;
} *mapentry_t;

@interface Map_c: Collection_any <Map, Serialization>
{
@public
  id  list;               // list of key-member entries in key sequence
  compare_t compareFunc;  // function to compare keys, if any
}
/*** methods in Map_c (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- setCompareFunction: (compare_t)compareFunc;
- (compare_t)getCompareFunction;
- createEnd;
- copy: aZone;
- at: aKey;
- (BOOL)at: aKey insert: anObject;
- at: aKey replace: anObject;
- (BOOL)at: aKey memberSlot: (id **)memPtr;
- (BOOL)at: aKey keySlot: (id **)keyPtr memberSlot: (id **)memPtr;
- removeKey: aKey;
- begin: aZone;
- _createIndex_: aZone forIndexSubclass: anIndexSubclass;
- _createPermutedIndex_: aZone forIndexSubclass: anIndexSubclass;
- createIndex: aZone fromMember: anObject;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- lispIn: expr;
- lispOut: stream;
@end

@interface MapIndex_c: Index_any <MapIndex>
{
@public
  id listIndex;     // index into list of entries
}
/*** methods in MapIndex_c (inserted from .m file by m2h) ***/
- next;
- next: (id *)key;
- prev;
- prev: (id *)key;
- get;
- get: (id *)key;
- getKey;
- replace: anObject;
- remove;
- setKey: aKey;
- getLoc;
- (void)setLoc: (id <Symbol>)locSymbol;
- (int)getOffset;
- setOffset: (int)offset;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end
