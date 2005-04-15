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
Name:         List_GEN.m
Description:  generic template for various forms of doubly linked list   
Library:      collections
*/

#import <collections/List_GEN.h>
#import <collections/Set.h>
#import <defobj/defalloc.h>

#import <defobj/macros.h>

#include <misc.h> // INT_MAX
#define UNKNOWN_POS  (INT_MAX/2)

#define COUNT(l) ((TARGET *) l)->count
#define FIRST(l) ((TARGET *) l)->firstLink
#define FIRSTPREV(l) FIRST (l)->prevLink

#define TINDEXCLASS__(class) id_##class
#define TINDEXCLASS_(val) TINDEXCLASS__(val)
#define TINDEXCLASS TINDEXCLASS_(TINDEX)

#if MLINKS

static inline link_t
getLinkFromMember (id anObject, long bits)
{
  int offset;

  offset =
    getField (bits, IndexFromMemberLoc_Shift, IndexFromMemberLoc_Mask) +
    IndexFromMemberLoc_Min;

  return (link_t) ((void *) anObject + offset);
}

static inline id
getMemberFromLink (link_t link, long bits)
{
  return 
    (id) ((void *) link -
          (getField (bits, IndexFromMemberLoc_Shift, IndexFromMemberLoc_Mask) +
           IndexFromMemberLoc_Min));
}

#endif


@implementation TARGET
PHASE(Creating)
PHASE(Setting)
PHASE(Using)
PHASE(UsingOnly)
//
// copy: -- standard method to copy internal state of object
//
- copy: aZone
{
  TARGET *newList;
  id index, member;

  newList = [aZone allocIVars: getClass (self)];
  setMappedAlloc (newList);
  index = [self begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    [newList addLast: member];
  DROP (index);
  return newList;
}

- (void)addFirst: anObject
{
  link_t newLink;

#if LINKED
  newLink = ALLOCBLOCK (getZone (self), sizeof *firstLink);
#elif MLINKS
  newLink = getLinkFromMember (anObject, bits);
#endif
  if (firstLink)
    {
#if !LINKED
      if (newLink->nextLink)
#if 1
        {
          id ind = [self begin: getCZone (getZone (self))];
          BOOL found = ([ind findNext: anObject] != nil);

          DROP (ind);
          if (found)
            return;
        }
#else
      abort ();
#endif
#endif
      newLink->prevLink = firstLink->prevLink;
      newLink->nextLink = firstLink;
      firstLink->prevLink->nextLink = newLink;
      firstLink->prevLink = newLink;
      firstLink = newLink;
    }
  else
    {
      firstLink = newLink;
      newLink->prevLink = newLink->nextLink = newLink;
    }
#if LINKED
  newLink->refObject = anObject;
#endif
  count++;
}

- (void)addLast: anObject
{
  link_t newLink;

#if LINKED
  newLink = ALLOCBLOCK (getZone (self), sizeof *firstLink);
#elif MLINKS
  newLink = getLinkFromMember (anObject, bits);
#endif
  if (firstLink)
    {
#if !LINKED
      if (newLink->nextLink) 
#if 1
        {
          id ind = [self begin: getCZone (getZone (self))];
          BOOL found = ([ind findNext: anObject] != nil);

          DROP (ind);
          if (found)
            return;
        }
#else
      abort ();
#endif
#endif
      newLink->prevLink = firstLink->prevLink;
      newLink->nextLink = firstLink;
      firstLink->prevLink->nextLink = newLink;
      firstLink->prevLink = newLink;
    }
  else
    {
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
  link_t link;
  id member;

  if (firstLink)
    {
      link = firstLink;
      if (firstLink->nextLink != firstLink)
        {
          firstLink->prevLink->nextLink = firstLink->nextLink;
          firstLink->nextLink->prevLink = firstLink->prevLink;
          firstLink = firstLink->nextLink;
        }
      else
        firstLink = NULL;
#if LINKED
      member = link->refObject;
      FREEBLOCK_SIZE (getZone (self), link, sizeof *link);
#elif MLINKS
      member = getMemberFromLink (link, bits);
#endif
      count--;
      return member;
    } 
  else
    {
      raiseEvent (NoMembers, nil);
      exit (0);
    }
}

- removeLast
{
  link_t link;
  id member;

  if (firstLink)
    {
      link = firstLink->prevLink;
      if (link->nextLink != link)
        {
          link->prevLink->nextLink = firstLink;
          firstLink->prevLink = link->prevLink;
        }
      else
        firstLink = NULL;
#if LINKED
      member = link->refObject;
      FREEBLOCK_SIZE (getZone (self), link, sizeof *link);
#elif MLINKS
      member = getMemberFromLink (link, bits);
#endif
      count--;
      return member;
    }
  else
    {
      raiseEvent (NoMembers, nil);
      exit (0);
    }
}

- (id <ListIndex>)begin: (id <Zone>)aZone
{
  TINDEX *newIndex;

  newIndex = [aZone allocIVars: TINDEXCLASS];
  newIndex->collection = self;
  newIndex->link = (link_t) Start;
  newIndex->position = 0;
  return newIndex;
}

- (id <ListIndex>)listBegin: (id <Zone>)aZone
{
  return [self begin: aZone];
}

- _createIndex_: aZone forIndexSubclass: anIndexSubclass
{
  TINDEX *newIndex;

  newIndex = [aZone allocIVars: anIndexSubclass];
  newIndex->collection = self;
  newIndex->link = (link_t) Start;
  newIndex->position = 0;
  return newIndex;
}

- _createPermutedIndex_: aZone forIndexSubclass: anIndexSubclass
{
  return
    [[[PermutedIndex_c createBegin: aZone] setCollection: self] createEnd];
}

//
// describe: -- standard method to generate debug description of object
//
- (void)describe: outputCharStream
{
#if MLINKS
  char  buffer[100];
#endif

  [super describe: outputCharStream];
#if MLINKS
  sprintf (buffer, "> internal links at offset: %d\n",
           (int) getField (bits, IndexFromMemberLoc_Shift, IndexFromMemberLoc_Mask) +
           IndexFromMemberLoc_Min);
  [outputCharStream catC: buffer];
#endif
}

- createIndex: aZone fromMember: anObject
{
#if MLINKS
  TINDEX  *newIndex;
 
  newIndex = [aZone allocIVars: TINDEXCLASS];
  newIndex->collection = self;
  newIndex->link = getLinkFromMember (anObject, bits);
  newIndex->position = UNKNOWN_POS;
  return newIndex;
#else
  raiseEvent (SourceMessage,
              "> createIndex:fromMember: requires IndexFromMemberLoc value\n");
  exit (0);
#endif
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
#if LINKED
  link_t link, nextLink;

  if (!includeBlocks (mapalloc))
    return;

  mapalloc->size = sizeof *link;
  if (firstLink)
    {
      link = firstLink;
      do {
        nextLink = link->nextLink;
        mapAlloc (mapalloc, link);
        link = nextLink;
      } while (link != firstLink);
    }
#endif
}

@end


@implementation TINDEX
PHASE(Creating)
PHASE(Using)
- next
{
  if (position > 0)
    {
      if (link->nextLink != FIRST (collection))
        {
          // at interior link
#if MLINKS
          if (position != UNKNOWN_POS)  // conditionalizes next statement
#endif
            position++;
          link = link->nextLink;
#if LINKED
          return link->refObject;
#elif MLINKS
          return getMemberFromLink (link, collection->bits);
#endif
        }
      else
        {
          position = 0;
          link = (link_t) End;
          return NULL;
        }
    }
  else if (position == 0)
    { 
      // at Start or End
      if (INDEXSTARTP (link))
        {
          if (FIRST (collection))
            {
              position = 1;
              link = FIRST (collection);
#if LINKED
              return link->refObject;
#elif MLINKS
              return getMemberFromLink (link, collection->bits);
#endif
            }
            else
              {
                // no members
                link = (link_t) End;
                return NULL;
              }
        }
      else
        {
          raiseEvent (AlreadyAtEnd, nil);
          exit (0);
        }
    }
  else
    {
      // member just removed
      if (INDEXSTARTP (link))
        {
          position = 0;
          return [self next];
        }
      else if (INDEXENDP (link))
        {
          position = 0;
          return NULL;
        }
      else
        {
          position = (- position);
          link = link->nextLink;
          
          if (link == FIRST (collection))
            return NULL;
          
#if LINKED
          return link->refObject;
#elif MLINKS
          return getMemberFromLink (link, collection->bits);
#endif
        }
    }
}

- prev
{
  if (position > 0)
    {
      if (link != FIRST (collection))
        {
          // at interior link
#if MLINKS
          if (position != UNKNOWN_POS)  // conditionalizes next statement
#endif
            position--;
          link = link->prevLink;
#if LINKED
          return link->refObject;
#elif MLINKS
          return getMemberFromLink (link, collection->bits);
#endif
        }
      else
        {
          position = 0;
          link = (link_t) Start;
          return NULL;
        }
    }
  else if (position == 0)
    {
      // at Start or End
      if (INDEXENDP (link))
        {
          if (FIRST (collection))
            {
              position = COUNT (collection);
              link = FIRSTPREV (collection);
#if LINKED
              return link->refObject;
#elif MLINKS
              return getMemberFromLink (link, collection->bits);
#endif
            }
          else
            {
              // no members
              link = (link_t) Start;
              return NULL;
            }
        }
      else
        {
          raiseEvent (AlreadyAtStart, nil);
          exit (0);
        }
    }
  else
    { 
      // member just removed
      if (INDEXSTARTP (link))
        {
          position = 0;
          return NULL;
        }
      else if (INDEXENDP (link))
        {
          position = 0;
          return [self prev];
        }
      else
        {
#if MLINKS
          if (position == UNKNOWN_POS)
            position = (- position);
          else  // conditionalizes next statement
#endif
            position = (- position) - 1;
#if LINKED
          return link->refObject;
#elif MLINKS
          return getMemberFromLink (link, collection->bits);
#endif
        }
    }
}

- get
{
  if (position > 0)
#if LINKED
    return link->refObject;
#elif MLINKS
  return getMemberFromLink (link, collection->bits);
#endif
  return NULL;
}

- put: anObject
{
  id oldMem;
#if MLINKS
  link_t oldLink;
#endif

  if (position <= 0)
    raiseEvent (InvalidIndexLoc, nil);
#if LINKED
  oldMem = link->refObject;
  link->refObject = anObject;
#elif MLINKS
  oldLink = link;
  oldMem = getMemberFromLink (link, collection->bits);
  link = getLinkFromMember (anObject, collection->bits);
  if (collection->count == 1)
    {
      FIRST (collection) = link;
      link->nextLink = link->prevLink = link;
    }
  else
    {
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
  link_t oldLink;
  id oldMem;

  if (position <= 0)
    raiseEvent (InvalidIndexLoc, nil);
   
  oldLink = link;
#if LINKED
  oldMem  = link->refObject;
#elif MLINKS
  oldMem  = getMemberFromLink (link, collection->bits);
#endif
  if (COUNT (collection) > 1)
    {
      // members to remain in collection
      if (link == FIRST (collection)) // removing first member
        {
          // update first member
          FIRST (collection) = link->nextLink; 
          link = (link_t)Start;
          position = -1;
#if 0
        }
      else if (link == FIRSTPREV (collection))
        {
          // removing last member
          link = (link_t)End;
          position = -1;
#endif
        } 
      else // removing from interior
        {
          position = (- position);
          link = link->prevLink;
        }
      oldLink->nextLink->prevLink = oldLink->prevLink;  // remove link
      oldLink->prevLink->nextLink = oldLink->nextLink;
    }
  else // removing only member
    {
      FIRST (collection) = NULL;
      link = (link_t) Start;
      position = -1;
    }
  collection->count--;
#if LINKED
  FREEBLOCK_SIZE (getZone (collection), oldLink, sizeof *link);
#endif
  return oldMem;
}

- (id <Symbol>)getLoc
{
  if (position > 0)
    return Member;
  if (position < 0)
    return Removed;
  return (id)link;
}

- (void)setLoc: (id <Symbol>)locSymbol
{
  if (INDEXSTARTP (locSymbol))
    {
      position = 0;
      link = (link_t) Start;
    }
  else if (INDEXENDP (locSymbol))
    {
      position = 0;
      link = (link_t) End;
    }
  else
    raiseEvent (InvalidLocSymbol, nil);
}

- (int)getOffset
{
#if MLINKS
  if (position == UNKNOWN_POS)
    return -1;
#endif
  if (position > 0)
    return (position - 1);
  return -1;
}

- setOffset: (unsigned)offset
{
  if (offset >= collection->count)
    raiseEvent (OffsetOutOfRange, nil);
  link = (link_t) Start;
  position = 0;
  [self next];
  for (; offset > 0; offset--)
    [self next];
  return [self get];
}

- (void)addAfter: anObject
{
  link_t newLink;

  if (position < 0 || (position == 0 && !INDEXSTARTP (link)))
    raiseEvent (InvalidIndexLoc, nil);
#if LINKED
  newLink = ALLOCBLOCK (getZone (collection), sizeof *link);
  newLink->refObject = anObject;
#elif MLINKS
  newLink = getLinkFromMember (anObject, collection->bits);
#endif
  if (position)
    {
      newLink->nextLink = link->nextLink;
      newLink->prevLink = link;
      link->nextLink->prevLink = newLink;
      link->nextLink = newLink;
    } 
  else
    {
      if (FIRST (collection))
        {
          newLink->prevLink = FIRSTPREV (collection);
          newLink->nextLink = FIRST (collection);
          FIRSTPREV (collection)->nextLink = newLink;
          FIRSTPREV (collection) = newLink;
          FIRST (collection) = newLink;
        }
      else
        {
          FIRST (collection) = newLink;
          newLink->prevLink = newLink->nextLink = newLink;
        }
    }
  collection->count++;
}

- (void)addBefore: anObject
{
  link_t newLink;

  if (position < 0 || (position == 0 && !INDEXENDP (link)))
    raiseEvent (InvalidIndexLoc, nil);
#if LINKED
  newLink = ALLOCBLOCK (getZone (collection), sizeof *link);
  newLink->refObject = anObject;
#elif MLINKS
  newLink = getLinkFromMember (anObject, collection->bits);
#endif
  if (position)
    {
      if (position == 1)
        FIRST (collection) = newLink;
      newLink->nextLink = link;
      newLink->prevLink = link->prevLink;
      link->prevLink->nextLink = newLink;
      link->prevLink = newLink;
      position++;
    }
  else
    {
      if (FIRST (collection))
        {
          newLink->prevLink = FIRSTPREV (collection);
          newLink->nextLink = FIRST (collection);
          FIRSTPREV (collection)->nextLink = newLink;
          FIRSTPREV (collection) = newLink;
        }
      else
        {
          FIRST (collection) = newLink;
          newLink->prevLink = newLink->nextLink = newLink;
        }
    }
  collection->count++;
}

@end
