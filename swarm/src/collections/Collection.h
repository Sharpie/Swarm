// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Collection.h
Description:  generic supertype for collections   
Library:      collections
*/

#import <defobj/Create.h>
#import <collections.h>
#import <defobj.h> // Serialization

@interface Collection_any: CreateDrop_s <Collection>
{
@public
  unsigned count;  // number of members in collection
  unsigned bits;   // bit allocations
#define  Bit_ReadOnly              (1 << 0)  // Not yet implemented
#define  Bit_ReplaceOnly           (1 << 1)
#define  Bit_MemberAlloc           (1 << 2)  // Array
#define  Bit_DefaultMember         (1 << 3)  // Array
#define  Bit_IndexFromMemberLoc    (1 << 4)  // List...
#define  Bit_CountSet              (1 << 6)  // Array, during create only
#define  Bit_InitialValueSet       (1 << 7)  // Collection, during create only
#define  IndexFromMemberLoc_Shift  20
#define  IndexFromMemberLoc_Mask   (0xfff << IndexFromMemberLoc_Shift)
#define  IndexFromMemberLoc_Min    -2044
}
/*** methods in Collection_any (inserted from .m file by m2h) ***/
- (void)setReplaceOnly: (BOOL)replaceOnly;
- (void)setInitialValue: initialValue;
- (void)setIndexFromMemberLoc: (int)byteOffset;
- (BOOL)getReplaceOnly;
- (int)getIndexFromMemberLoc;
- beginPermuted: aZone;
- (unsigned)getCount;
- (unsigned)count;
- atOffset: (int)offset;
- atOffset: (int)offset put: anObject;
- getFirst;
- first;
- getLast;
- last;
- (BOOL)contains: aMember;
- remove: aMember;
- (void)removeAll;
- (void)deleteAll;
- (void)forEach: (SEL)aSelector;
- (void)forEach: (SEL)aSelector : arg1;
- (void)forEach: (SEL)aSelector : arg1 : arg2;
- (void)forEach: (SEL)aSelector : arg1 : arg2 : arg3;
- (BOOL)allSameClass;
- (void)describe: outputCharStream;
- (void)describeForEach: outputCharStream;
- (void)describeForEachID: outputCharStream;
- _lispOutAttr_: stream;
- (BOOL)_lispInAttr_: index;
- copy: aZone;
- begin: aZone;
@end

@interface Index_any: Object_s <Index>
{
@public
  Collection_any *collection;  // base collection on which index created
}
/*** methods in Index_any (inserted from .m file by m2h) ***/
- getCollection;
- findNext: anObject;
- findPrev: anObject;

// Stubs for protocol compliance
- setOffset: (int)offset;
- (int)getOffset;
- (void)setLoc: locSymbol;
- put: anObject;
- remove;
- prev;
- next;
@end

@interface PermutedIndex_c: Index_any <PermutedIndex>
{
@public
  id index;
}
+ createBegin: aZone;
- setCollection: collection;
- setUniformRandom: rnd;
- createEnd;
- reshuffle;
- next;
- prev;
- findNext: anObject;
- findPrev: anObject;
- get;
- put: anObject;
- remove;
- getLoc;
- (void)setLoc: locSymbol;
- (int)getOffset;
- setOffset: (int)offset;
@end
