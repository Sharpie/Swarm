// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

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
  MLINK_ADD (self, anObject);
  return NO;
}

- remove: aMember
{
  id index;

  index = MLINK_CREATEINDEX_FROMMEMBER (self, getCZone (getZone (self)), aMember);
  MLINK_INDEX_REMOVE (index);
  DROP (index);
  return aMember;
}

@end


@implementation OrderedSetIndex_c: ListIndex_mlinks
PHASE(Creating)
PHASE(Using)
@end
