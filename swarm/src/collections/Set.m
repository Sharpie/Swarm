// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Set.m
Description:  set of elements with id as key, implemented as linear list
Library:      collections
*/

#import <collections/Set.h>
#import <collections/List.h>
#import <defobj/defalloc.h>


@implementation Set_c

PHASE(Creating)

+ createBegin: aZone
{
  Set_c *newSet;

  newSet = [aZone allocIVars: self];
  return newSet;
}

- createEnd
{
  if (createByMessageToCopy (self, createEnd))
    return self;
  self->list = [List create: getCZone (getZone (self))];
  setMappedAlloc (self);
  setNextPhase (self);
  return self;
}

PHASE(Using)

- copy: aZone
{
  Set_c *newSet;

  newSet = [aZone copyIVars: self];
  newSet->list = [List create: aZone];
  return newSet;
}

- (int)count
{
  return [list count];
}

- (unsigned)getCount
{
  return [list count];
}

- (BOOL)add: anObject
{
  [(id) list addLast: anObject];
  return NO;
}

- replace: anObject
{
  raiseEvent (NotImplemented, nil);
  return nil;
}

- (BOOL)contains: aKey
{
  return [self at: aKey] != nil;
}

- (BOOL)containsKey: aKey
{
  return [self contains: aKey];
}

- at: aKey
{
  id  index, member;

  index = [(id) list begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    if (member == aKey)
      break;
  [index drop];
  return member;
}

- (BOOL)at: aKey memberSlot: (id **)memptr
{
  raiseEvent (NotImplemented, nil);

  return NO;
}

- remove: aKey
{
  id  index, member;

  index = [(id) list begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    if (member == aKey)
      {
        member = [index remove];
        break;
      }
  [index drop];
  return member;
}

- removeKey: aKey
{
  return [self remove: aKey];
}

- (void)forEachKey: (SEL)aSelector
{
  [self forEach: (SEL)aSelector];
}

- (void)forEachKey: (SEL)aSelector : arg1
{
  [self forEach: (SEL)aSelector: arg1];
}

- (void)forEachKey: (SEL)aSelector : arg1 : arg2
{ 
  [self forEach: (SEL)aSelector: arg1 : arg2];
}

- (void)forEachKey: (SEL)aSelector : arg1 : arg2 : arg3
{ 
  [self forEach: (SEL)aSelector: arg1 : arg2];
}

- begin: aZone
{
  SetIndex_c *newIndex;

  newIndex = [aZone allocIVars: [SetIndex_c self]];
  setMappedAlloc (newIndex);
  newIndex->collection = self;
  newIndex->listIndex  = [(id) list begin: getCZone (aZone)];
  
  return newIndex;
}

- createIndex: aZone fromMember: anObject
{
  raiseEvent (NotImplemented, nil);

  return nil;
}

- (void) mapAllocations: (mapalloc_t)mapalloc
{
  mapObject (mapalloc, list);
}

@end


@implementation SetIndex_c

- next
{
  return [listIndex next];
}

- prev
{
  return [listIndex next];
}

- get
{
  return [listIndex get];
}

- replace: anObject
{
  return [listIndex replace: anObject];
}

- remove
{
  return [listIndex remove];
}

- (id <Symbol>)getLoc
{
  return [listIndex getLoc];
}

- (void)setLoc: (id <Symbol>)locSymbol
{
  [listIndex setLoc: locSymbol];
}

- (int)getOffset
{
  return [listIndex getOffset];
}

- setOffset: (unsigned)offset
{
  return [listIndex setOffset: offset];
}

- (void) mapAllocations: (mapalloc_t)mapalloc
{
  mapObject (mapalloc, listIndex);
}

@end
