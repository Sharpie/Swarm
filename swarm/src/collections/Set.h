// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Set.h
Description:  set of elements with id as key, implemented as linear list 
Library:      collections
*/

#import <collections/Collection.h>

@interface Set_c : Collection_any // <Set>
{
@public
  Collection_any  *list;         // list of key-member entries in key sequence
}
/*** methods implemented in .m file ***/
+ createBegin: aZone;
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
@end

@interface SetIndex_c : Index_any // <KeyedCollectionIndex>
{
@public
  id     listIndex;     // index into list of entries
}
/*** methods implemented in .m file ***/
- next;
- prev;
- get;
- replace: anObject;
- remove;
- getLoc;
- (void) setLoc: locSymbol;
- (int) getOffset;
- setOffset: (int)offset;
@end
