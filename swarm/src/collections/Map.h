// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
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
- setCompareIntegers;
- setCompareUnsignedIntegers;
- setCompareCStrings;
- setCompareIDs;
- (compare_t)getCompareFunction;
- createEnd;
- copy: aZone;
- at: aKey;
- (BOOL)at: aKey insert: anObject;
- at: aKey replace: anObject;
- (BOOL)at: aKey memberSlot: (id **)memPtr;
- (BOOL)at: aKey keySlot: (id **)keyPtr memberSlot: (id **)memPtr;
- (BOOL)containsKey: aKey;
- removeKey: aKey;
- (void)forEachKey: (SEL)aSelector;
- (void)forEachKey: (SEL)aSelector : arg1;
- (void)forEachKey: (SEL)aSelector : arg1 : arg2;
- (void)forEachKey: (SEL)aSelector : arg1 : arg2 : arg3;
- begin: aZone;
- _createIndex_: aZone forIndexSubclass: anIndexSubclass;
- _createPermutedIndex_: aZone forIndexSubclass: anIndexSubclass;
- createIndex: aZone fromMember: anObject;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (BOOL)allSameClass;
- lispInCreate: expr;
- lispIn: expr;
- lispOutShallow: stream;
- lispOutDeep: stream;
- hdf5InCreate: hdf5Obj;
- hdf5In: hdf5Obj;
- hdf5OutShallow: hdf5Obj;
- hdf5OutDeep: hdf5Obj;
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
- (id <Symbol>)getLoc;
- (void)setLoc: (id <Symbol>)locSymbol;
- (int)getOffset;
- setOffset: (unsigned)offset;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end

extern int compareIDs (id val1, id val2);
extern int compareIntegers (id val1, id val2);
extern int compareUnsignedIntegers (id val1, id val2);
extern int compareCStrings (id val1, id val2);
