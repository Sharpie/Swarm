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
Name:         List_GEN.h
Description:  generic template for various forms of doubly linked list   
Library:      collections
*/

#if defined LINKED
#define TARGET List_linked
#define TINDEX ListIndex_linked
#define MLINKS 0
#define CONTIG 0

#elif defined MLINKS
#define TARGET List_mlinks
#define TINDEX ListIndex_mlinks
#define LINKED 0
#define CONTIG 0

#elif defined CONTIG
#define TARGET List_contig
#define TINDEX ListIndex_contig
#define LINKED 0
#define MLINKS 0
#endif

#import <Swarm/List.h>

struct link {
  link_t nextLink; // next link in list
  link_t prevLink; // previous link in list
#if ! MLINKS
  id refObject;    // object referenced by link
#endif
};

@interface TARGET: List_any <List>
/*** methods in TARGET (inserted from .m file by m2h) ***/
- copy: aZone;
- (void)addFirst: anObject;
- (void)addLast: anObject;
- removeFirst;
- removeLast;
- (id <ListIndex>)begin: (id <Zone>)aZone;
- (id <ListIndex>)listBegin: (id <Zone>)aZone;
- _createIndex_: aZone forIndexSubclass: anIndexSubclass;
- _createPermutedIndex_: aZone forIndexSubclass: anIndexSubclass;
- (void)describe: outputCharStream;
- createIndex: aZone fromMember: anObject;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end

@interface TINDEX: ListIndex_any <ListIndex>
#if CONTIG
{
@public
  int baseOffset;   // base offset of block contained as current link
}
#endif
/*** methods in TINDEX (inserted from .m file by m2h) ***/
- next;
- prev;
- get;
- put: anObject;
- replace: anObject;
- remove;
- (id <Symbol>)getLoc;
- (void)setLoc: (id <Symbol>)locSymbol;
- (int)getOffset;
- setOffset: (unsigned)offset;
- (void)addAfter: anObject;
- (void)addBefore: anObject;
@end


#if CONTIG

typedef struct block *block_t

struct block {
  int blockCount;    // number of member slots in current block
  int fillSlot;      // if first block, offset of first full slot;
                     // if last block (and not also first), first empty slot
  id slots[1];       // slots available for member storage
};

#endif

