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
Name:         List.m
Description:  implementations for List type
Library:      collections
*/

#import <collections/List.h>
#import <defobj/defalloc.h>
#import <defobj.h> // SaveError

#include <collections/predicates.h> // keywordp
#include <misc.h> // abort

#include <swarmconfig.h>

#ifdef HAVE_JDK
#import <defobj/java.h> // SD_JAVA_{INSTANTIATE,FINDJAVACLASS} (in HDF5in)
#endif

#import <defobj/HDF5Object.h>

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

#if SWARM_OBJC_DONE
  if (getBit (getClass (obj)->info, _CLS_DEFINEDCLASS))
    nextPhase = ((BehaviorPhase_s *) getClass (obj))->nextPhase;
#else
  if (swarm_class_getDefinedClassBit(swarm_object_getClass(obj))) {
    classData_t classData = _obj_getClassData(swarm_object_getClass(obj));
    nextPhase = (Class_s *)classData->initialPhase->nextPhase->definingClass;
  }
#endif
  
  if (nextPhase == id_List_any)
    nextPhase = aClass;

  if (!strncmp(swarm_class_getName(swarm_object_getClass(obj)), "List_any", 8))
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

- (void)lispOutDeep: (id <OutputStream>)stream
{
  [self _lispOut_: stream deep: YES];
}

- (void)lispOutShallow: (id <OutputStream>)stream
{
  [self _lispOut_: stream deep: NO];
}

#include "List_HDF5out.m"

@end

// ListIndex_any: index for List_c

@implementation ListIndex_any
@end
