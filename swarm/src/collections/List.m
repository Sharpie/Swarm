// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         List.m
Description:  implementations for List type
Library:      collections
*/

#import <collections/List.h>
#import <defobj/defalloc.h>
#import <defobj.h> // SaveError

#include <collections/predicates.h> // keywordp

@implementation List_any

PHASE(Creating)

+ createBegin: aZone
{
  List_any *newList;

  newList = [aZone allocIVars: self];
  return newList;
}

- (void)setInitialValue: initialValue
{
  firstLink = (link_t) initialValue;
  setBit (bits, Bit_InitialValueSet, 1);
}

- (void)setDequeOnly: (BOOL)dequeOnly
{
  setBit (bits, Bit_DequeOnly, dequeOnly);
}

- createEnd
{
  id index, member;

  if ((bits & Bit_InitialValueSet) && (bits & Bit_IndexFromMemberLoc))
    raiseEvent (InvalidCombination,
                "> cannot specify an initial value with IndexFromMemberLoc option\n");
  
  if (bits & Bit_InitialValueSet)
    {
      if (createByMessageToCopy (self, createEnd))
        return self;
      setClass (self, id_List_linked);
      setMappedAlloc (self);
      index = [(id) firstLink begin: scratchZone];
      firstLink = NULL;
      while ((member = [index next]))
        [(id) self addLast: member];
      [index drop];
    }
  else
    {
      createByCopy ();
      if (bits & Bit_IndexFromMemberLoc)
        setClass (self, id_List_mlinks);
      else
        setClass (self, id_List_linked);
      setMappedAlloc (self);
    }
  return self;
}

- lispInCreate: expr
{
  id index, member;

  index = [(id) expr begin: scratchZone];
  while ((member = [index next]))
    {
      if (keywordp (member))
        {
          const char *name = [member getKeywordName];

          if (strcmp (name, "index-from-member-loc") == 0)
            [self setIndexFromMemberLoc: lispInInteger (index)];
          else if (strcmp (name, "initial-value-set") == 0)
            [self setInitialValue: lispIn ([self getZone], [index next])];
          else if (![self _lispInAttr_: index])
            raiseEvent (InvalidArgument, "unknown keyword `%s'", name);
        }
    }
  [index drop];
  return self;
}

PHASE(Setting)

- (void)setCountPerBlock: (int)countPerBlock
{
  raiseEvent (NotImplemented, nil);
}

PHASE(Using)

- (BOOL)getDequeOnly
{
  return bits & Bit_DequeOnly;
}

- (int)getCountPerBlock
{
  raiseEvent (NotImplemented, nil);
  
  abort ();
}

- lispIn: expr
{
  id index, member;

  index = [(id) expr begin: scratchZone];
  while ((member = [index next]))
    if (keywordp (member))
      [index next];
    else
      [(id) self addLast: lispIn ([self getZone], member)];
  [index drop];
  return self;
}

- lispOut: outputCharStream deep: (BOOL)deepFlag
{
  id index, member;

  [outputCharStream catC: "(" MAKE_INSTANCE_FUNCTION_NAME " 'List"];

  index = [(id) self begin: scratchZone];
  while ((member = [index next]))
    [member lispOut: outputCharStream deep: (BOOL)deepFlag];
  [index drop];

  [self _lispOutAttr_: outputCharStream];

  if (bits & Bit_IndexFromMemberLoc)
    {
      char buf[6];

      [outputCharStream catC: " #:index-from-member-loc "];
      sprintf (buf, "%d", [self getIndexFromMemberLoc]);
      [outputCharStream catC: buf];
    }

#if 0
  if (bits & Bit_InitialValueSet)
    {
      // Would need to preserve original setting...
      [outputCharStream catC: "#:initial-value-set "];
    }
#endif
  
  [outputCharStream catC: ")"];
  
  return self;
}

#ifdef HAVE_HDF5
- hdf5Out: hdf5Obj deep: (BOOL)deepFlag
{
  if (deepFlag)
    [super hdf5Out: hdf5Obj deep: YES];
  else
    {
      if (![self allSameClass])
        raiseEvent (SaveError,
                    "shallow HDF5 serialization on Collections must be of same type");
      else
        {
          id aZone = [self getZone];
          id hdf5CompoundType = [[[HDF5CompoundType createBegin: aZone]
                                   setSourceClass: [[self getFirst] class]]
                                  createEnd];
          
          id hdf5ObjDataset =
            [[[[[HDF5 createBegin: aZone]
                 setName: "list"]
                setParent: hdf5Obj]
               setType: hdf5CompoundType count: [self getCount]]
              createEnd];


          {
            id <Index> li = [self begin: scratchZone];
            id member;

            while ((member = [li next]))
              {
                unsigned rn = [li getOffset];

                [hdf5ObjDataset numberRecord: rn];
                [hdf5ObjDataset selectRecord: rn];
                [member hdf5Out: hdf5ObjDataset deep: NO];
              }
            [li drop];
          }
          [hdf5ObjDataset drop];
          [hdf5CompoundType drop];
        }
    }
  return self;
}
#endif

@end


// ListIndex_any: index for List_c

@implementation ListIndex_any
@end
