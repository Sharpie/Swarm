// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         OrderedSet.m
Description:  set of elements in an externally maintained linear sequence
Library:      collections
*/

#import <collections/OrderedSet.h>


@implementation OrderedSet_c

PHASE(Creating)

+ createBegin: aZone
{
  OrderedSet_c  *newOrderedSet;

  newOrderedSet = [aZone allocIVars: self];
  return newOrderedSet;
}

- createEnd
{
  createByCopy( );
  setNextPhase( self );
  return self;
}

PHASE(Using)

- (BOOL) contains: anObject
{
  id  index, member;

  index = [self begin: scratchZone];
  while ( (member = [index next]) ) if ( member == anObject ) return 1;
  return 0;
}

- (id *) add: anObject
{
  [self addLast: anObject];
  return (id *)nil;
}

- remove: aMember
{
  id  index;

  index = [self createIndex: scratchZone fromMember: aMember];
  [index remove];
  [index drop];
  return aMember;
}

@end


@implementation OrderedSetIndex_c : ListIndex_mlinks
@end
