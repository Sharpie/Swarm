// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         List_GEN.m
Description:  generic template for various forms of doubly linked list   
Library:      collections
*/

#import <collections/List_GEN.h>
#import <collections/Set.h>
#import <defobj/defalloc.h>

#include <limits.h>
#define UNKNOWN_POS  (INT_MAX/2)

#include <memory.h>


#if MLINKS

static inline link_t getLinkFromMember( id anObject, long bits )
{
  int offset;

  offset =
    getField( bits, IndexFromMemberLoc_Shift, IndexFromMemberLoc_Mask ) +
    IndexFromMemberLoc_Min;

  return (link_t)( (void *)anObject + offset );
}

static inline id getMemberFromLink( link_t link, long bits )
{
  return (id)( (void *)link -
    ( getField( bits, IndexFromMemberLoc_Shift, IndexFromMemberLoc_Mask ) +
      IndexFromMemberLoc_Min ) );
}

#endif


@implementation TARGET

PHASE(UsingOnly)

//
// copy: -- standard method to copy internal state of object
//
- copy: aZone
{
  TARGET  *newList;
  id      index, member;

  newList = [aZone allocIVars: getClass( self )];
  setMappedAlloc( newList );
  index = [self begin: scratchZone];
  while ( (member = [index next]) ) [newList addLast: member];
  [index drop];
  return newList;
}

- (void) addFirst: anObject
{
  link_t  newLink;

#if LINKED
  newLink = [getZone( self ) allocBlock: sizeof *firstLink];
#elif MLINKS
  newLink = getLinkFromMember( anObject, bits );
#endif
  if ( firstLink ) {
    newLink->prevLink = firstLink->prevLink;
    newLink->nextLink = firstLink;
    firstLink->prevLink->nextLink = newLink;
    firstLink->prevLink = newLink;
    firstLink = newLink;
  } else {
    firstLink = newLink;
    newLink->prevLink = newLink->nextLink = newLink;
  }
#if LINKED
  newLink->refObject = anObject;
#endif
  count++;
}

- (void) addLast: anObject
{
  link_t  newLink;

#if LINKED
  newLink = [getZone( self ) allocBlock: sizeof *firstLink];
#elif MLINKS
  newLink = getLinkFromMember( anObject, bits );
#endif
  if ( firstLink ) {
    newLink->prevLink = firstLink->prevLink;
    newLink->nextLink = firstLink;
    firstLink->prevLink->nextLink = newLink;
    firstLink->prevLink = newLink;
  } else {
    firstLink = newLink;
    newLink->prevLink = newLink->nextLink = newLink;
  }
#if LINKED
  newLink->refObject = anObject;
#endif
  count++;
}

- removeFirst
{
  link_t  link;
  id      member;

  if ( firstLink ) {
    link = firstLink;
    if ( firstLink->nextLink != firstLink ) {
      firstLink->prevLink->nextLink = firstLink->nextLink;
      firstLink->nextLink->prevLink = firstLink->prevLink;
      firstLink = firstLink->nextLink;
    } else {
      firstLink = NULL;
    }
#if LINKED
    member = link->refObject;
    [getZone( self ) freeBlock: link blockSize: sizeof *link];
#elif MLINKS
    member = getMemberFromLink( link, bits );
#endif
    count--;
    return member;
  } else {
    raiseEvent( NoMembers, nil ); exit(0);
  }
}

- removeLast
{
  link_t link;
  id     member;

  if ( firstLink ) {
    link = firstLink->prevLink;
    if ( link->nextLink != link ) {
      link->prevLink->nextLink = firstLink;
      firstLink->prevLink = link->prevLink;
    } else {
      firstLink = NULL;
    }
#if LINKED
    member = link->refObject;
    [getZone( self ) freeBlock: link blockSize: sizeof *link];
#elif MLINKS
    member = getMemberFromLink( link, bits );
#endif
    count--;
    return member;
  } else {
    raiseEvent( NoMembers, nil ); exit(0);
  }
}

- begin: aZone
{
  TINDEX *newIndex;

  newIndex = [aZone allocIVars: [TINDEX self]];
  newIndex->collection = self;
  newIndex->link       = (link_t)Start;
  newIndex->position   = 0;
  return newIndex;
}

- _createIndex_: aZone forIndexSubclass: anIndexSubclass
{
  TINDEX *newIndex;

  newIndex = [aZone allocIVars: anIndexSubclass];
  newIndex->collection = self;
  newIndex->link       = (link_t)Start;
  newIndex->position   = 0;
  return newIndex;
}

//
// describe: -- standard method to generate debug description of object
//
- (void) describe: outputCharStream
{
#if MLINKS
  char  buffer[100];
#endif

  [super describe: outputCharStream];
#if MLINKS
  sprintf( buffer, "> internal links at offset: %d\n",
    getField( bits, IndexFromMemberLoc_Shift, IndexFromMemberLoc_Mask ) +
              IndexFromMemberLoc_Min );
  [outputCharStream catC: buffer];
#endif
}

- createIndex: aZone fromMember: anObject
{
#if MLINKS
  TINDEX  *newIndex;

  newIndex = [aZone allocIVars: [TINDEX self]];
  newIndex->collection = self;
  newIndex->link       = getLinkFromMember( anObject, bits );
  newIndex->position   = UNKNOWN_POS;
  return newIndex;
#else
  raiseEvent( SourceMessage,
  "> createIndex:fromMember: requires IndexFromMemberLoc value\n" ); exit(0);
#endif
}

- (void) mapAllocations: (mapalloc_t)mapalloc
{
#if LINKED
  link_t  link, nextLink;

  if ( ! includeBlocks( mapalloc ) ) return;

  mapalloc->size = sizeof *link;
  if ( firstLink ) {
    link = firstLink;
    do {
      nextLink = link->nextLink;
      mapAlloc( mapalloc, link );
      link = nextLink;
    } while ( link != firstLink );
  }
#endif
}

@end


@implementation TINDEX

- next
{
  if ( position > 0 ) {
    if ( link->nextLink != ((TARGET *)collection)->firstLink ) {
                                                // at interior link
#if MLINKS
      if ( position != UNKNOWN_POS )  // conditionalizes next statement
#endif
      position++;
      link = link->nextLink;
#if LINKED
      return link->refObject;
#elif MLINKS
      return getMemberFromLink( link, collection->bits );
#endif
    } else {
      position = 0;
      link = (link_t)End;
      return NULL;
    }
  } else if ( position == 0 ) {  // at Start or End
    if ( (id)link == Start ) {
      if ( ((TARGET *)collection)->firstLink ) {
        position = 1;
	link = ((TARGET *)collection)->firstLink;
#if LINKED
        return link->refObject;
#elif MLINKS
        return getMemberFromLink( link, collection->bits );
#endif
      } else {  // no members
        link = (link_t)End;
        return NULL;
      }
    } else {
      raiseEvent( AlreadyAtEnd, nil ); exit(0);
    }
  } else {  // member just removed
    if ( (id)link == Start ) {
      position = 0;
      return [self next];
    } else if ( (id)link == End ) {
      position = 0;
      return NULL;
    } else {
      position = (- position);
      link = link->nextLink;
#if LINKED
      return link->refObject;
#elif MLINKS
      return getMemberFromLink( link, collection->bits );
#endif
    }
  }
}

- prev
{
  if ( position > 0 ) {
    if ( link != ((TARGET *)collection)->firstLink ) {  // at interior link
#if MLINKS
      if ( position != UNKNOWN_POS )  // conditionalizes next statement
#endif
      position--;
      link = link->prevLink;
#if LINKED
      return link->refObject;
#elif MLINKS
      return getMemberFromLink( link, collection->bits );
#endif
    } else {
      position = 0;
      link = (link_t)Start;
      return NULL;
    }
  } else if ( position == 0 ) {  // at Start or End
    if ( (id)link == End ) {
      if ( ((TARGET *)collection)->firstLink ) {
        position = ((TARGET *)collection)->count;
	link = ((TARGET *)collection)->firstLink->prevLink;
#if LINKED
        return link->refObject;
#elif MLINKS
        return getMemberFromLink( link, collection->bits );
#endif
      } else {  // no members
        link = (link_t)Start;
        return NULL;
      }
    } else {
      raiseEvent( AlreadyAtStart, nil ); exit(0);
    }
  } else {  // member just removed
    if ( (id)link == Start ) {
      position = 0;
      return NULL;
    } else if ( (id)link == End ) {
      position = 0;
      return [self prev];
    } else {
#if MLINKS
      if ( position == UNKNOWN_POS )
        position = (- position);
      else  // conditionalizes next statement
#endif
      position = (- position) - 1;
#if LINKED
      return link->refObject;
#elif MLINKS
      return getMemberFromLink( link, collection->bits );
#endif
    }
  }
}

- get
{
  if ( position > 0 )
#if LINKED
    return link->refObject;
#elif MLINKS
    return getMemberFromLink( link, collection->bits );
#endif
  return NULL;
}

- put: anObject
{
  id      oldMem;
#if MLINKS
  link_t  oldLink;
#endif

  if ( position <= 0 ) raiseEvent( InvalidIndexLoc, nil );
#if LINKED
  oldMem = link->refObject;
  link->refObject = anObject;
#elif MLINKS
  oldLink = link;
  oldMem  = getMemberFromLink( link, collection->bits );
  link    = getLinkFromMember( anObject, collection->bits );
  if ( collection->count == 1 ) {
    ((TARGET *)collection)->firstLink = link;
    link->nextLink = link->prevLink = link;
  } else {
    link->nextLink = oldLink->nextLink;
    link->prevLink = oldLink->prevLink;
  }
#endif
  return oldMem;
}

- replace: anObject
{
  return [self put: anObject];
}

- remove
{
  link_t     oldLink;
  id         oldMem;

  if ( position <= 0 ) raiseEvent( InvalidIndexLoc, nil );

  oldLink = link;
#if LINKED
  oldMem  = link->refObject;
#elif MLINKS
  oldMem  = getMemberFromLink( link, collection->bits );
#endif
  if ( ((TARGET *)collection)->count > 1 ) {  // members to remain in collection

    if ( link == ((TARGET *)collection)->firstLink ) {  // removing first member
      ((TARGET *)collection)->firstLink = link->nextLink; // update first member
      link     = (link_t)Start;
      position = -1;
/*
    } else if ( link == ((TARGET *)collection)->firstLink->prevLink ) {
                                                       // removing last member
      link     = (link_t)End;
      position = -1;
*/
    } else {  // removing from interior
      position = (- position);
      link     = link->prevLink;
    }
    oldLink->nextLink->prevLink = oldLink->prevLink;  // remove link
    oldLink->prevLink->nextLink = oldLink->nextLink;

  } else {  // removing only member
    ((TARGET *)collection)->firstLink = NULL;
    link = (link_t)Start;
    position = -1;
  }
  collection->count--;
#if LINKED
  [getZone( collection ) freeBlock: oldLink blockSize: sizeof *link];
#endif
  return oldMem;
}

- getLoc
{
  if ( position > 0 ) return Member;
  if ( position < 0 ) return Removed;
  return (id)link;
}

- (void) setLoc: locSymbol
{
  if ( locSymbol == Start ) {
    position = 0;
    link     = (link_t)Start;
  } else if ( locSymbol == End ) {
    position = 0;
    link     = (link_t)End;
  } else {
    raiseEvent( InvalidLocSymbol, nil );
  }
}

- (int) getOffset
{
#if MLINKS
  if ( position == UNKNOWN_POS ) return -1;
#endif
  if ( position > 0 ) return (position - 1);
  return -1;
}

- setOffset: (int)offset
{
  if ( ( offset < 0 ) || ( offset >= collection->count ) ) {
    raiseEvent( OffsetOutOfRange, nil );
  }
  link = (link_t)Start;
  position = 0;
  for ( ; offset >= 0; offset-- ) [self next];
  return [self get];
}

- (void) addAfter: anObject
{
  link_t  newLink;

  if ( position < 0 || ( position == 0 && (id)link != Start ) ) {
    raiseEvent( InvalidIndexLoc, nil );
  }
#if LINKED
  newLink = [getZone( collection ) allocBlock: sizeof *link];
  newLink->refObject = anObject;
#elif MLINKS
  newLink = getLinkFromMember( anObject, collection->bits );
#endif
  if ( position ) {
    newLink->nextLink = link->nextLink;
    newLink->prevLink = link;
    link->nextLink->prevLink = newLink;
    link->nextLink = newLink;
  } else {
    if ( ((TARGET *)collection)->firstLink ) {
      newLink->prevLink = ((TARGET *)collection)->firstLink->prevLink;
      newLink->nextLink = ((TARGET *)collection)->firstLink;
      ((TARGET *)collection)->firstLink->prevLink->nextLink = newLink;
      ((TARGET *)collection)->firstLink->prevLink = newLink;
      ((TARGET *)collection)->firstLink = newLink;
    } else {
      ((TARGET *)collection)->firstLink = newLink;
      newLink->prevLink = newLink->nextLink = newLink;
    }
  }
  collection->count++;
}

- (void) addBefore: anObject
{
  link_t  newLink;

  if ( position < 0 || ( position == 0 && (id)link != End ) ) {
    raiseEvent( InvalidIndexLoc, nil );
  }
#if LINKED
  newLink = [getZone( collection ) allocBlock: sizeof *link];
  newLink->refObject = anObject;
#elif MLINKS
  newLink = getLinkFromMember( anObject, collection->bits );
#endif
  if ( position ) {
    if ( position == 1 ) ((TARGET *)collection)->firstLink = newLink;
    newLink->nextLink = link;
    newLink->prevLink = link->prevLink;
    link->prevLink->nextLink = newLink;
    link->prevLink = newLink;
    position++;
  } else {
    if ( ((TARGET *)collection)->firstLink ) {
      newLink->prevLink = ((TARGET *)collection)->firstLink->prevLink;
      newLink->nextLink = ((TARGET *)collection)->firstLink;
      ((TARGET *)collection)->firstLink->prevLink->nextLink = newLink;
      ((TARGET *)collection)->firstLink->prevLink = newLink;
    } else {
      ((TARGET *)collection)->firstLink = newLink;
      newLink->prevLink = newLink->nextLink = newLink;
    }
  }
  collection->count++;
}

@end
