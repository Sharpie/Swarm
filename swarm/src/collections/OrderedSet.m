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
Name:         OrderedSet.m
Description:  set of elements in an externally maintained linear sequence
Library:      collections
*/

#import <collections/OrderedSet.h>
#import <defobj/defalloc.h>

#import <defobj/macros.h>
#import <collections/macros.h>

@implementation OrderedSet_c

PHASE(Creating)

+ createBegin: aZone
{
  OrderedSet_c *newOrderedSet;

  newOrderedSet = [aZone allocIVars: self];
  return newOrderedSet;
}

- createEnd
{
  createByCopy ();
  setNextPhase (self);
  return self;
}

PHASE(Setting)
PHASE(Using)

- (BOOL)contains: anObject
{
  id index, member;
  BOOL ret = NO;

  index = [self begin: getCZone (getZone (self))];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    if (member == anObject)
      {
        ret = YES;
        break;
      }
  DROP (index);
  return ret;
}

- (BOOL)add: anObject
{
  MLIST_ADDLAST (self, anObject);
  return NO;
}

- remove: aMember
{
  id index;

  index = MLIST_CREATEINDEX_FROMMEMBER (self, getCZone (getZone (self)), aMember);
  MLIST_INDEX_REMOVE (index);
  DROP (index);
  return aMember;
}

@end


@implementation OrderedSetIndex_c: ListIndex_mlinks
PHASE(Creating)
PHASE(Using)
@end
