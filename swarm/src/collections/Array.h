// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Array.h
Description:  implementation for Array type
Library:      collections
*/

#import <collections/Collection.h>

@interface Array_c: Collection_any <Array>
{
@public
  id *block; // block of id values
}
/*** methods in Array_c (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- (void)setInitialValue: initialValue;
- createEnd;
+ create: aZone setCount: (int)memberCount;
+ create: aZone setMemberBlock: (id *)members setCount: (int)memberCount;
- (void)setMemberBlock: (id *)members setCount: (int)memberCount;
- (void)setDefaultMember: memberValue;
- (void)setCount: (int)memberCount;
- (void *)getMemberBlock;
- getDefaultMember;
- (int)getCount;
- (int)count;
- atOffset: (int)offset;
- atOffset: (int)offset put: anObject;
- getFirst;
- getLast;
- begin: aZone;
- copy: aZone;
- (void)describe: outputCharStream;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end

@interface ArrayIndex_c: Index_any <Index>
{
@public
  id *memPtr; // pointer to current member, or Start or End
}
/*** methods in ArrayIndex_c (inserted from .m file by m2h) ***/
- next;
- prev;
- get;
- put: anObject;
- remove;
- getLoc;
- (void)setLoc: (id <Symbol>)locSymbol;
- (int)getOffset;
- setOffset: (int)offset;
- (int)compare: anIndex;
@end

