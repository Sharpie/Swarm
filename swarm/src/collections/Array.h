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
Name:         Array.h
Description:  implementation for Array type
Library:      collections
*/

#import <Swarm/Collection.h>
#import <Swarm/defobj.h> // Serialization

@interface Array_c: Collection_any <Array, Serialization>
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
- (id <Index>)begin: (id <Zone>)aZone;
- copy: aZone;
- (void)describe: outputCharStream;
- (void)mapAllocations: (mapalloc_t)mapalloc;
- lispInCreate: expr;
- lispIn: expr;
- hdf5InCreate: hdf5Obj;
- hdf5In: hdf5Obj;
- (void)lispOutShallow: (id <OutputStream>)stream;
- (void)lispOutDeep: (id <OutputStream>)stream;
- (void)hdf5OutShallow: (id <HDF5>)hdf5Obj;
- (void)hdf5OutDeep: (id <HDF5>)hdf5Obj;

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

