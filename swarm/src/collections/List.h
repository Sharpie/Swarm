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
Name:         List.h
Description:  implementations for List type
Library:      collections
*/

#import <Swarm/Collection.h>
#import <Swarm/defobj.h> // Serialization

typedef struct link *link_t;

@protocol Dummy  // (mangles file symbol name to avoid libobjects conflict)
- (unsigned)getCountPerBlock;
@end

@interface List_any: Collection_any <Dummy, Serialization>
{
@public
  link_t firstLink; // entry into circular chain of doubly linked nodes
}
#define  Bit_DequeOnly        (1 << 2)
#define  CountPerBlock_Shift  20
#define  CountPerBlock_Mask   (0xfff << CountPerBlock_Shift)

/*** methods in List_any (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- (void)setInitialValue: initialValue;
- (void)setDequeOnly: (BOOL)dequeOnly;
- createEnd;
- (void)setCountPerBlock: (int)countPerBlock;
- (BOOL)getDequeOnly;
- (unsigned)getCountPerBlock;
- lispInCreate: expr;
- lispIn: expr;
- hdf5InCreate: hdf5Obj;
- hdf5In: hdf5Obj;
- (void)lispOutShallow: (id <OutputStream>)stream;
- (void)lispOutDeep: (id <OutputStream>)stream;
- (void)hdf5OutShallow: (id <HDF5>)hdf5Obj;
- (void)hdf5OutDeep: (id <HDF5>)hdf5Obj;
@end

@interface ListIndex_any: Index_any
{
@public
  link_t link;   // link at current position, or endpoint symbol,
                 // or previous of last removed link
  int position;  // if positive, (offset + 1) of current member
                 // if negative, -(offset + 1) of last removed member
                 // if zero, link contains Start, End, or Unset
}
/*** methods in ListIndex_any (inserted from .m file by m2h) ***/
@end

