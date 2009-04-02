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
Name:         Set.h
Description:  set of elements with id as key, implemented as linear list 
Library:      collections
*/

#import <Swarm/Collection.h>

@interface Set_c: Collection_any <Set>
{
@public
  Collection_any *list; // list of member entries
}
/*** methods in Set_c (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- createEnd;
- copy: aZone;
- (unsigned)getCount;
- (BOOL)add: anObject;
- replace: anObject;
- (BOOL)contains: aKey;
- (BOOL)containsKey: aKey;
- at: aKey;
- (BOOL)at: aKey memberSlot: (id **)memptr;
- remove: aKey;
- removeKey: aKey;
- (void)forEachKey: (SEL)aSelector;
- (void)forEachKey: (SEL)aSelector : arg1;
- (void)forEachKey: (SEL)aSelector : arg1 : arg2;
- (void)forEachKey: (SEL)aSelector : arg1 : arg2 : arg3;
- (id <Index>)begin: (id <Zone>)aZone;
- createIndex: aZone fromMember: anObject;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end

@interface SetIndex_c: Index_any <KeyedCollectionIndex>
{
@public
  id listIndex; // index into list of entries
}
/*** methods in SetIndex_c (inserted from .m file by m2h) ***/
- next;
- prev;
- get;
- replace: anObject;
- remove;
- (id <Symbol>)getLoc;
- (void)setLoc: (id <Symbol>)locSymbol;
- (int)getOffset;
- setOffset: (unsigned)offset;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end
