// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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
#include <misc.h> // abort

#import <defobj/java.h> // SD_JAVA_{INSTANTIATE,FINDJAVACLASS} (in HDF5in)

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

static void
setListClass (id obj, id aClass)
{
  Class_s *nextPhase = id_List_any;

  if (getBit (getClass (obj)->info, _CLS_DEFINEDCLASS))
    nextPhase = ((BehaviorPhase_s *) getClass (obj))->nextPhase;
  
  if (nextPhase == id_List_any)
    nextPhase = aClass;
  
  setClass (obj, nextPhase);
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
      setListClass (self, id_List_linked);
      setMappedAlloc (self);
      index = [(id) firstLink begin: scratchZone];
      firstLink = NULL;
      for (member = [index next];
           [index getLoc] == Member;
           member = [index next])
        [(id) self addLast: member];
      [index drop];
    }
  else
    {
      createByCopy ();
      if (bits & Bit_IndexFromMemberLoc)
        setListClass (self, id_List_mlinks);
      else
        setListClass (self, id_List_linked);
      setMappedAlloc (self);
    }
  return self;
}

- lispInCreate: expr
{
  id index, member;

  index = [(id) expr begin: scratchZone];
  for (member = [index next];
       [index getLoc] == Member;
       member = [index next])
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

- hdf5InCreate: hdf5Obj
{
  return self;
}

PHASE(Setting)

- (void)setCountPerBlock: (int)countPerBlock
{
  raiseEvent (NotImplemented, nil);
}

- lispIn: expr
{
  id index, member;

  index = [(id) expr begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    if (keywordp (member))
      [index next];
    else
      [(id) self addLast: lispIn ([self getZone], member)];
  [index drop];
  return self;
}

#include "List_HDF5in.m"

PHASE(Using)

- (BOOL)getDequeOnly
{
  return bits & Bit_DequeOnly;
}

- (unsigned)getCountPerBlock
{
  raiseEvent (NotImplemented, nil);
  
  abort ();
}

- (void)_lispOut_: outputCharStream deep: (BOOL)deepFlag
{
  id index, member;

  [outputCharStream catStartMakeInstance: [self getTypeName]];
  [outputCharStream catSeparator];
  index = [(id) self begin: scratchZone];
  if (deepFlag)
    {
      for (member = [index next];
           [index getLoc] == Member; 
           member = [index next])
        if (member)
          [member lispOutDeep: outputCharStream];
    }
  else
    {
      for (member = [index next];
           [index getLoc] == Member; 
           member = [index next])
        if (member)
          [member lispOutShallow: outputCharStream];
    }
  [index drop];

  [self _lispOutAttr_: outputCharStream];

  if (bits & Bit_IndexFromMemberLoc)
    {
      [outputCharStream catSeparator];
      [outputCharStream catKeyword: "index-from-member-loc"];
      [outputCharStream catSeparator];
      [outputCharStream catInt: [self getIndexFromMemberLoc]];
    }

#if 0
  if (bits & Bit_InitialValueSet)
    {
      // Would need to preserve original setting...
      [outputCharStream catC: "#:initial-value-set "];
    }
#endif
  
  [outputCharStream catEndExpr];
}

- (void)lispOutDeep: stream
{
  [self _lispOut_: stream deep: YES];
}

- (void)lispOutShallow: stream
{
  [self _lispOut_: stream deep: NO];
}

#include "List_HDF5out.m"

@end

// ListIndex_any: index for List_c

@implementation ListIndex_any
@end
