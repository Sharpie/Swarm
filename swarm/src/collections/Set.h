// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Set.h
Description:  set of elements with id as key, implemented as linear list 
Library:      collections
*/

#import <collections/Collection.h>

@interface Set_c : Collection_any
{
@public
  Collection_any  *list;         // list of member entries
}
/*** methods in Set_c (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- createEnd;
- copy: aZone;
- (int) count;
- (id *) add: anObject;
- replace: anObject;
- (BOOL) contains: aKey;
- at: aKey;
- (BOOL) at: aKey memberSlot: (id **)memptr;
- remove: aKey;
- begin: aZone;
- createIndexIn: aZone fromMember: anObject;
- (void) mapAllocations: (mapalloc_t)mapalloc;
@end

@interface SetIndex_c : Index_any // <KeyedCollectionIndex>
{
@public
  id     listIndex;     // index into list of entries
}
/*** methods in SetIndex_c (inserted from .m file by m2h) ***/
- next;
- prev;
- get;
- replace: anObject;
- remove;
- getLoc;
- (void) setLoc: locSymbol;
- (int) getOffset;
- setOffset: (int)offset;
- (void) mapAllocations: (mapalloc_t)mapalloc;
@end
