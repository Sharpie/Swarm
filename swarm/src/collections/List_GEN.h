// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

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

#import <collections/List.h>

struct link {
  link_t     nextLink;     // next link in list
  link_t     prevLink;     // previous link in list
#if ! MLINKS
  id         refObject;    // object referenced by link
#endif
};

@interface TARGET : List_any
/*** methods in TARGET (inserted from .m file by m2h) ***/
- copy: aZone;
- (void) addFirst: anObject;
- (void) addLast: anObject;
- removeFirst;
- removeLast;
- begin: aZone;
- _createIndex_: aZone forIndexSubclass: anIndexSubclass;
- (void) describe: outputCharStream;
- createIndex: aZone fromMember: anObject;
- (void) mapAllocations: (mapalloc_t)mapalloc;
@end

@interface TINDEX : ListIndex_any // <Index>
#if CONTIG
{
@public
  int        baseOffset;   // base offset of block contained as current link
}
#endif
/*** methods in TINDEX (inserted from .m file by m2h) ***/
- next;
- prev;
- get;
- put: anObject;
- replace: anObject;
- remove;
- getLoc;
- (void) setLoc: locSymbol;
- (int) getOffset;
- setOffset: (int)offset;
- (void) addAfter: anObject;
- (void) addBefore: anObject;
@end


#if CONTIG

typedef struct block *block_t

struct block {
  int   blockCount;    // number of member slots in current block
  int   fillSlot;      // if first block, offset of first full slot;
                       // if last block (and not also first), first empty slot
  id    slots[1];      // slots available for member storage
};

#endif
