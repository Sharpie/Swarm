// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         List_mlinks.m
Description:  implementation of doubly linked list with link from member
Library:      collections
*/

#import <collections/List_mlinks.h>
#import "List_GEN.m"
#define _CLASSDEFS_ \
 @implementation List_mlinks \
 @implementation ListIndex_mlinks
#undef _CLASSDEFS_

id <ListIndex>
beginMlinksList (id list, id <Zone> aZone)
{
  ListIndex_mlinks *newIndex;

  newIndex = COMPONENT_ALLOCIVARS (getCZone (aZone), id_ListIndex_mlinks);
  newIndex->collection = list;
  newIndex->link = (link_t) Start;
  newIndex->position = 0;
  return newIndex;
}

id
getFirstMlinksList (id list)
{
  return getMemberFromLink (((struct List_mlinks *) list)->firstLink,
                            ((struct List_mlinks *) list)->bits);
}
