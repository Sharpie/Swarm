// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Collection.m
Description:  generic supertype for collections   
Library:      collections
*/

#import <collections/Collection.h>
#import <collections/Set.h>
#import <collections/Permutation.h>
#import <collections.h> // INDEX{START,END}P.
#import <defobj/defalloc.h>

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
                byteOffset - IndexFromMemberLoc_Min );
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

- begin: aZone
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

- (id)beginPermuted: (id) aZone
{
  PermutedIndex_c *newIndex;
  newIndex = [PermutedIndex_c createBegin: aZone forCollection: self];
  newIndex = [newIndex createEnd];
  return newIndex;
}

- (int)getCount
{
  return count;
}

- (int)count
{
  return count;
}

static id
indexAtOffset (Collection_any *self, int offset)
{
  id   index;

  if (offset < 0 || offset >= self->count)
    raiseEvent (OffsetOutOfRange, nil);
  for (index = [(id) self begin: scratchZone]; offset >= 0; offset--)
    [index next];
  return index;
}

- atOffset: (int)offset
{
  id index, member;

  index = indexAtOffset (self, offset);
  member = [index get];
  [index drop];
  return member;
}

- atOffset: (int)offset put: anObject
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

- first
{
  return [self atOffset: 0];
}

- getLast
{
  return [self atOffset: count - 1];
}

- last
{
  return [self atOffset: count - 1];
}

- (BOOL)contains: aMember
{
  id  index, member;

  index = [(id) self begin: scratchZone];
  while ((member = [index next]) && member != aMember);
  [index drop];
  return member != nil; 
}

- remove: aMember
{
  id  index, member;

  index = [(id) self begin: scratchZone];
  while ((member = [index next]) && member != aMember);
  if (member)
    [index remove];
  [index drop];
  return member; 
}

- (void)removeAll
{
  id index;

  index = [(id) self begin: scratchZone];
  while ([index next])
    [index remove];
  [index drop];
}

- (void)deleteAll
{
  id index, member;

  index = [(id) self begin: scratchZone];
  while ((member = [index next]))
    {
      [index remove];
      [member drop];
    }
  [index drop];
}

- (void)forEach: (SEL)aSelector
{
  id index, member;

  index = [(id) self begin: scratchZone];
  while ((member = [index next]))
    [member perform: aSelector];
  [index drop];
}

- (void)forEach: (SEL)aSelector : arg1
{
  id index, member;

  index = [(id) self begin: scratchZone];
  while ((member = [index next]))
    [member perform: aSelector with: arg1];
  [index drop];
}

- (void)forEach: (SEL)aSelector : arg1 : arg2
{
  id index, member;

  index = [(id) self begin: scratchZone];
  while ((member = [index next]))
    [member perform: aSelector with: arg1 with: arg2];
  [index drop];
}

- (void)forEach: (SEL)aSelector : arg1 : arg2 : arg3
{
  id index, member;

  index = [(id) self begin: scratchZone];
  while ((member = [index next]))
    [member perform: aSelector with: arg1 with: arg2 with: arg3];
  [index drop];
}

//
// describe: -- standard method to generate debug description object
//
- (void)describe: outputCharStream
{
  char  buffer[100];

  [super describe: outputCharStream];
  sprintf( buffer, "> number of members: %d\n", count );
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
  while ((member = [index next]))
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
  while ((member = [index next]))
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
  return NULL;
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

- setOffset: (int)offset
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

- (void)setLoc: locsym
{
  raiseEvent (SubclassMustImplement, "setLoc: not implemented");
}

- getLoc
{
  raiseEvent (SubclassMustImplement, "getLoc not implemented");
  return nil;
}

@end


@implementation PermutedIndex_c
PHASE(Creating)
+ createBegin: (id) aZone forCollection: aCollection
{
  PermutedIndex_c *newIndex;
  newIndex = [aZone allocIVars: id_PermutedIndex_c];
  newIndex->collection = [Permutation createBegin: [aZone getComponentZone] 
				      forCollection: aCollection];
  return newIndex;
}

- setUniformRandom: rnd
{
  [(Permutation_c *)collection setUniformRandom: rnd];
  return self;
}

- createEnd
{
  collection = [collection createEnd];
  index = [collection begin: [getZone(self) getComponentZone]];
  setMappedAlloc(self);  
  return self;
}

PHASE(Using)

- generatePermutation
{
  [(Permutation_c *)collection generatePermutation];
  return self;
}

- next
{
  return [index next];
}

- prev
{
  return [index prev];
}

- findNext: anObject;
{
  return [index findNext: anObject];
}

- findPrev: anObject;
{
  return [index findPrev: anObject];
}

- get
{
  return [index get];
}

- put: anObject;
{
  raiseEvent(SourceMessage,
	     "> Elements can not be added via the PermutedIndex");
  return nil;

}

- remove;
{
  raiseEvent(SourceMessage,
	     "> Elements can not be removed via the PermutedIndex");
  return nil;
}

- getLoc
{
  return [index getLoc];
}

- (void)setLoc: locSymbol
{
  return [index setLoc: locSymbol];
}

- (int)getOffset
{
  return [index getOffset];
}

- setOffset: (int)offset;
{
  return [index setOffset: offset];
}

- (void) mapAllocations: (mapalloc_t) mapalloc
{
  mapObject(mapalloc, collection);
  mapObject(mapalloc, index);
}

@end;
