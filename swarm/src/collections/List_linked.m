// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         List_linked.m
Description:  default implementation of doubly linked list   
Library:      collections
*/

#import <collections/List_linked.h>
#import "List_GEN.m"
#define _CLASSDEFS_ \
 @implementation List_linked \
 @implementation ListIndex_linked
#undef _CLASSDEFS_ 

id <ListIndex>
beginLinkedList (struct List_linked *list)
{
  ListIndex_linked *newIndex;

  newIndex = COMPONENT_ALLOCIVARS (getZone (list), id_ListIndex_linked);
  newIndex->collection = list;
  newIndex->link = (link_t) Start;
  newIndex->position = 0;
  return newIndex;
}


