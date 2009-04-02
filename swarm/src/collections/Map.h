// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

/*
Name:         Map.h
Description:  sorted map implemented as linear list 
Library:      collections
*/

#import <Swarm/Collection.h>
#import <Swarm/defobj.h> // Serialization
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
- (id <MapIndex>)begin: (id <Zone>)aZone;
- (id <MapIndex>)mapBegin: (id <Zone>)aZone;
- _createIndex_: aZone forIndexSubclass: anIndexSubclass;
- _createPermutedIndex_: aZone forIndexSubclass: anIndexSubclass;
- createIndex: aZone fromMember: anObject;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- (BOOL)allSameClass;
- lispInCreate: expr;
- lispIn: expr;
- (void)lispOutShallow: stream;
- (void)lispOutDeep: stream;
- hdf5InCreate: hdf5Obj;
- hdf5In: hdf5Obj;
- (void)hdf5OutShallow: (id <HDF5>)hdf5Obj;
- (void)hdf5OutDeep: (id <HDF5>)hdf5Obj;
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
- (unsigned long)getKeyValue;
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
