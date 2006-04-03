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
Name:         Collection.m
Description:  generic supertype for collections   
Library:      collections
*/

#import <collections/Collection.h>
#import <collections/Set.h>
#import <collections/Permutation.h> // PermutationItem_c
#import <collections.h> // INDEX{START,END}P, Permutation
#import <defobj/defalloc.h>
#import <defobj.h> // hdf5In

#include <swarmconfig.h> // HAVE_HDF5
#include <misc.h> // abort

@implementation Collection_any

PHASE(Creating)

- (void)setReplaceOnly: (BOOL)replaceOnly
{
  setBit (bits, Bit_ReplaceOnly, replaceOnly);
}

- (void)setInitialValue: initialValue
{
  raiseEvent (SubclassMustImplement, "setInitialValue: not implemented");
}

- (void)setIndexFromMemberLoc: (int)byteOffset  // belongs elsewhere...
{
  if (byteOffset > -2044 && byteOffset <= 2048)
    {
      setField (bits, IndexFromMemberLoc_Shift,
                byteOffset - IndexFromMemberLoc_Min);
      bits |= Bit_IndexFromMemberLoc;
    }
  else
    raiseEvent (InvalidArgument,
                "> IndexFromMemberLoc must be within range of -2044 to +2048\n"
                "> value specified: %d\n", byteOffset);
}


- (BOOL)_lispInAttr_: index
{
  id key = [index get];
  const char *name = [key getKeywordName];
  
  if (strcmp (name, "replace-only") == 0)
    [self setReplaceOnly: lispInBoolean (index)];
  else
    return NO;
  return YES;
}

PHASE(Using)

- (id <Index>)begin: (id <Zone>)aZone
{
  raiseEvent (SubclassMustImplement, "begin: not implemented");
  return nil;
}

- copy: aZone
{
  raiseEvent (SubclassMustImplement, "copy: not implemented");
  return nil;
}

- (BOOL)getReplaceOnly
{
  return bits & Bit_ReplaceOnly;
}

- (int)getIndexFromMemberLoc
{
  return
    getField (bits, IndexFromMemberLoc_Shift, IndexFromMemberLoc_Mask) - 2044;
}

- (id <PermutedIndex>)beginPermuted: (id <Zone>)aZone
{
  return [[[PermutedIndex_c createBegin: aZone]
	    setCollection: self] createEnd];
}

- (unsigned)getCount
{
  return count;
}

static id
indexAtOffset (Collection_any *self, unsigned offset)
{
  id index;

  if (offset >= self->count)
    raiseEvent (OffsetOutOfRange, nil);
  for (offset++, index = [(id) self begin: scratchZone]; offset > 0; offset--)
    [index next];
  return index;
}

- atOffset: (unsigned)offset
{
  id index, member;

  index = indexAtOffset (self, offset);
  member = [index get];
  [index drop];
  return member;
}

- atOffset: (unsigned)offset put: anObject
{
  id index, member;

  index = indexAtOffset (self, offset);
  member = [index replace: anObject];
  [index drop];
  return member;
}

- getFirst
{
  return [self atOffset: 0];
}

- getLast
{
  return [self atOffset: count - 1];
}

- (BOOL)contains: aMember
{
  id index, member;

  index = [(id) self begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    if (member == aMember)
      {
        [index drop];
        return YES;
      }
  [index drop];
  return NO;
}

- remove: aMember
{
  id index, member;

  index = [(id) self begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    if (member == aMember)
      {
        [index remove];
        break;
      }
  [index drop];
  return member; 
}

- (void)removeAll
{
  id index;

  index = [(id) self begin: scratchZone];
  for ([index next]; [index getLoc] == Member; [index next])
    [index remove];
  [index drop];
}

- (void)deleteAll
{
  id index, member;

  index = [(id) self begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    {
      [index remove];
      if (member)
        [member drop];
    }
  [index drop];
}

- (void)forEach: (SEL)aSelector
{
  id index, member;

  index = [(id) self begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    if (member)
      [member perform: aSelector];
  [index drop];
}

- (void)forEach: (SEL)aSelector : arg1
{
  id index, member;

  index = [(id) self begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    if (member)
      [member perform: aSelector with: arg1];
  [index drop];
}

- (void)forEach: (SEL)aSelector : arg1 : arg2
{
  id index, member;

  index = [(id) self begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    if (member)
      [member perform: aSelector with: arg1 with: arg2];
  [index drop];
}

- (void)forEach: (SEL)aSelector : arg1 : arg2 : arg3
{
  id index, member;

  index = [(id) self begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    if (member)
      [member perform: aSelector with: arg1 with: arg2 with: arg3];
  [index drop];
}
 
- (BOOL)allSameClass
{
  id index, member;
  Class firstClass;
  BOOL ret = YES;

  index = [(id) self begin: scratchZone];
  member = [index next];

  firstClass = member ? [member class] : Nil;
  for (member = [index next];
       [index getLoc] == Member;
       member = [index next])
    if (!member)
      {
        if (firstClass)
          {
            ret = NO;
            break;
          }
      }
    else if ([member class] != firstClass)
      {
        ret = NO;
        break;
      }
  [index drop];
  return ret;
}  

//
// describe: -- standard method to generate debug description object
//
- (void)describe: outputCharStream
{
  char  buffer[100];

  [super describe: outputCharStream];
  sprintf ( buffer, "> number of members: %d\n", count );
  [outputCharStream catC: buffer];
}

//
// describeForEach: --
//   generate debug description for each member of a collection
//
- (void)describeForEach: outputCharStream
{
  id index, member;

  index = [(id) self begin: scratchZone];
  for (member = [index next];
       [index getLoc] == Member;
       member = [index next])    
    [member describe: outputCharStream];
  [index drop];
}

//
// describeForEachID: --
//   generate debug id description for each member of a collection
//
- (void)describeForEachID: outputCharStream
{
  id index, member;

  index = [(id) self begin: scratchZone];
  for (member = [index next];
       [index getLoc] == Member;
       member = [index next])    
    [member describeID: outputCharStream];
  [index drop];
}

- _lispOutAttr_: outputCharStream
{
  if (bits & Bit_ReplaceOnly)
    [outputCharStream catC: " #:replace-only #t"];
  
  return self;
}

@end

// Index_any: index for any Collection

@implementation Index_any
PHASE(Creating)

PHASE(Using)
- getCollection
{
  return collection;
}

- findNext: anObject
{
  id member;

  while (!INDEXENDP ([(id) self getLoc]))
    {
      member = [(id) self next];
      if (member == anObject)
        return member;
    }
  return nil;
}

- findPrev: anObject
{
  id member;

  while (!INDEXSTARTP ([(id) self getLoc]))
    {
      member = [(id) self prev];
      if (member == anObject)
        return member;
    }
  return NULL;
}

- setOffset: (unsigned)offset
{
  raiseEvent (SubclassMustImplement, "setOffset: not implemented");
  return nil;
} 

- (int)getOffset
{
  raiseEvent (SubclassMustImplement, "getOffset not implemented");
  return 0;
}

- prev
{
  raiseEvent (SubclassMustImplement, "prev not implemented");
  return nil;
}

- next
{
  raiseEvent (SubclassMustImplement, "next not implemented");
  return nil;
}

- remove
{
  raiseEvent (SubclassMustImplement, "remove not implemented");
  return nil;
}

- get
{
  raiseEvent (SubclassMustImplement, "get not implemented");
  return nil;
}

- put: anObj
{
  raiseEvent (SubclassMustImplement, "put: not implemented");
  return nil;
}

- (void)setLoc: (id <Symbol>)locsym
{
  raiseEvent (SubclassMustImplement, "setLoc: not implemented");
}

- (id <Symbol>)getLoc
{
  raiseEvent (SubclassMustImplement, "getLoc not implemented");
  return nil;
}

@end


@implementation PermutedIndex_c
PHASE(Creating)
+ createBegin: aZone
{
  PermutedIndex_c *obj = [aZone allocIVars: self];
  obj->collection = [Permutation createBegin: getCZone (aZone)];
  obj->nextFlag = NO;
  return obj;
}

- setCollection: (id <Collection>)aCollection
{
  [(id <Permutation>) collection setCollection: aCollection];
  return self;
}

- setUniformRandom: rnd
{
  [(id <Permutation>) collection setUniformRandom: rnd];
  return self;
}

- createEnd
{
  collection = [collection createEnd];
  index = [collection begin: getCZone (getZone (self))];
  setMappedAlloc (self);  
  return self;
}

PHASE(Using)

- reshuffle 
{
  [((Permutation_c *) collection)->shuffler shuffleWholeList: collection];
  [index drop];
  index = [collection begin: getCZone (getZone (self))];
  return self;
}

- (id <Symbol>)_getLoc_
{
  if (((Permutation_c *) collection)->count == 0)
    return End;
  else if (![(Permutation_c *) collection getTouchedFlag])
    return Start;
  else if (![(Permutation_c *) collection getUntouchedFlag])
    return nextFlag ? Member : End; 
  else
    return Member;
}

- (void)_updatePermutation_
{
  Permutation_c *perm = (Permutation_c *) collection;
  Collection_any *original = (Collection_any *) perm->collection;
  collection = [[[[[Permutation createBegin: getCZone (getZone (self))]
                    setCollection: original]
                   setLastPermutation: perm]
                  setUniformRandom: perm->rnd]
                 createEnd];
  {
    id <Symbol> loc = [self _getLoc_];
    id current = loc == Member ? [self get] : nil;

    [self reshuffle];
    if (loc == Member)
      {
        while ([index getLoc] != End)
          if (((PermutationItem_c *) [index next])->item == current)
            break;
      }
    else
      [index setLoc: loc];
  }
  [perm drop];
}

- (void)_clearDirection_
{
  [collection forEach: M(setLastDirection:) : (id) 0];
}

- next
{
  [self _updatePermutation_];
  nextFlag = NO;
  while ([self _getLoc_] != End)
    {
      PermutationItem_c *pi;

      pi = [index next];

      if (pi)
	{
	  if (pi->position >= 0 && pi->lastDirection != 1)
            {
              pi->lastDirection = 1;
              nextFlag = YES;
              return pi->item;
            }
	}
      else
        [index setLoc: Start];
    }
  return nil;
}

- prev
{
  [self _updatePermutation_];
  while ([self getLoc] != Start)
    {
      PermutationItem_c *pi = [index prev];

      if (pi)
	{
	  if (pi->position >= 0)
            {
              pi->lastDirection = -1;
              return pi->item;
            }
	}
      else
        [index setLoc: End];
    }
  return nil;
}

- get
{
  PermutationItem_c *pi;

  pi = [index get];
  return pi ? pi->item : nil;
}

- put: anObject;
{
  PermutationItem_c *pi;

  pi = [index get];
  if (pi != nil)
    {
      if (pi->position >= 0)
	{
	  pi->item = anObject;
	  return [[((Permutation_c *) collection) getCollection]
		   atOffset: pi->position
		   put: anObject];
	}
    }
  abort ();
}

- remove
{
  PermutationItem_c *pi;

  pi = [index get];
  if (pi != nil)
    {
      id ret;
      id removeIndex = 
	indexAtOffset ((Collection_any *)(((Permutation_c *) collection)->collection),
                       pi->position);
      pi->item = nil;
      pi->position = -1;

      ret = [removeIndex remove];
      [removeIndex drop];
      return ret;
    }
  else
    abort ();
}

- (id <Symbol>)getLoc
{
  [self _updatePermutation_];
  return [self _getLoc_];
}

- (void)setLoc: (id <Symbol>)locSymbol
{
  [self _updatePermutation_];
  [self _clearDirection_];
  return [index setLoc: locSymbol];
}

- (int)getOffset
{
  [self _updatePermutation_];
  return [index getOffset];
}

- setOffset: (unsigned)offset;
{
  [self _updatePermutation_];
  [self _clearDirection_];
  return [index setOffset: offset];
}

- (void) mapAllocations: (mapalloc_t)mapalloc
{
  mapObject(mapalloc, collection);
  mapObject(mapalloc, index);
}

@end;
