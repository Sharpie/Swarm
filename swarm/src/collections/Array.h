// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Array.h
Description:  implementation for Array type
Library:      collections
*/

#import <collections/Collection.h>

@interface Array_c : Collection_any
{
@public
  id   *block;    // block of id values
}
/*** methods implemented in .m file ***/
+ createBegin: aZone;
+ create: aZone setCount: (int)memberCount;;
+ create: aZone setMemberBlock: (id *)members setCount: (int)memberCount;
+ create: aZone setMemberAlloc: (id *)members setCount: (int)memberCount;
- (void) setInitialValue: initialValue;
- createEnd;
- (void) setMemberBlock: (id *)members setCount: (int)memberCount;
- (void) setMemberAlloc: (id *)members setCount: (int)memberCount;
- (void) setCount: (int)newCount;
- (void) setDefaultMember: memberValue;
- (void *) getMemberBlock;
- (void *) getMemberAlloc;
- getDefaultMember;
- (int) getCount;
- (int) count;
- atOffset: (int)offset;
- atOffset: (int)offset put: anObject;
- getFirst;
- getLast;
- begin: aZone;
- copy: aZone;
- (void) drop;
@end

@interface ArrayIndex_c : Index_any
{
@public
  id   *memPtr;         // pointer to current member, or Start or End
}
/*** methods implemented in .m file ***/
- next;
- prev;
- get;
- put: anObject;
- remove;
- getLoc;
- (void) setLoc: locSymbol;
- (int) getOffset;
- (void) setOffset: (int)offset;
- (int) compare: anIndex;
@end

