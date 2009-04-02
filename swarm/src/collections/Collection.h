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
Name:         Collection.h
Description:  generic supertype for collections   
Library:      collections
*/

#import <Swarm/Create.h>
#import <Swarm/collections.h>

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
- (id <PermutedIndex>)beginPermuted: (id <Zone>)aZone;
- (unsigned)getCount;
- atOffset: (unsigned)offset;
- atOffset: (unsigned)offset put: anObject;
- getFirst;
- getLast;
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
- (id <Index>)begin: (id <Zone>)aZone;
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
- setOffset: (unsigned)offset;
- (int)getOffset;
- (void)setLoc: (id <Symbol>)locSymbol;
- put: anObject;
- remove;
- prev;
- next;
@end

@interface PermutedIndex_c: Index_any <PermutedIndex>
{
@public
  id index;
  BOOL nextFlag;
}
+ createBegin: aZone;
- setCollection: (id <Collection>)aCollection;
- setUniformRandom: rnd;
- createEnd;
- reshuffle;
- next;
- prev;
- get;
- put: anObject;
- remove;
- (id <Symbol>)getLoc;
- (void)setLoc: (id <Symbol>)locSymbol;
- (int)getOffset;
- setOffset: (unsigned)offset;
@end
