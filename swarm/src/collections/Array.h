// Swarm library. Copyright � 1996-2000 Swarm Development Group.
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
+ create: aZone setCount: (unsigned)memberCount;
+ create: aZone setMemberBlock: (id *)members setCount: (unsigned)memberCount;
- (void)setMemberBlock: (id *)members setCount: (unsigned)memberCount;
- (void)setDefaultMember: memberValue;
- setCount: (unsigned)memberCount;
- (void *)getMemberBlock;
- getDefaultMember;
- (unsigned)getCount;
- (unsigned)count;
- atOffset: (unsigned)offset;
- atOffset: (unsigned)offset put: anObject;
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
- (id <Symbol>)getLoc;
- (void)setLoc: (id <Symbol>)locSymbol;
- (int)getOffset;
- setOffset: (unsigned)offset;
- (int)compare: anIndex;
@end

