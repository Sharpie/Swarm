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
Name:         Map.m
Description:  sorted map implemented as linear list 
Library:      collections
*/

#import <collections/Map.h>
#import <defobj/defalloc.h>

#import <collections/List_linked.h>
#import <collections/collections_classes.h>

#include <defobj/swarm-objc-api.h> // object_get_class
#include <collections/predicates.h> // keywordp, stringp

#import <defobj.h> // hdf5in, HDF5

#import <misc.h> // memcpy
#include <swarmconfig.h> // HAVE_HDF5

#import <defobj/macros.h>
#import <collections/macros.h>

#define COMPARE_FUNCTION "compare-function"
#define COMPARE_INT "compare-integers"
#define COMPARE_UNSIGNED "compare-unsigned-integers"
#define COMPARE_CSTRING "compare-c-strings"
#define COMPARE_ID "compare-ids"

#define COMPAREFUNCEQ(f) (compareFunc == &(f))

#define GROUP_KEYS "keys"
#define GROUP_VALUES "values"

compare_t compareFuncs[] = { compareIntegers,
                             compareUnsignedIntegers,
                             compareCStrings,
                             compareIDs };

//
// compareIDs --
//   function to compare two id values based on the unsigned magnitudes of
//   their id values
//               
//
int
compareIDs (id val1, id val2)
{
  if ((PTRUINT) val1 < (PTRUINT) val2)
    return -1;
  return ((PTRUINT) val1 > (PTRUINT) val2);
}

//
// compareIntegers --
//   function to compare two signed integer values stored within id values
//
int
compareIntegers (id val1, id val2)
{
  if ((PTRINT) val1 < (PTRINT) val2)
    return -1;
  return ((PTRINT) val1 > (PTRINT) val2);
}

//
// compareUnsignedIntegers --
//   function to compare two unsigned integer values stored within id values
// (Functionally identical to compareIDs, but the identity distinction is
//  useful for deciding the appropriate use of Map keys.)
//
int
compareUnsignedIntegers (id val1, id val2)
{
  if ((PTRUINT) val1 < (PTRUINT) val2)
    return -1;
  return ((PTRUINT) val1 > (PTRUINT) val2);
}

//
// compareCStrings --
//   function to compare two asciz C strings.
// (Needed to identify Map keys as C strings for serialization.)
//
int
compareCStrings (id val1, id val2)
{
  return strcmp ((const char *)val1, (const char *)val2);
}

//
// compare -- internal macro for selection of compare technique
//
#define compare(a, b) \
(compareFunc ? compareFunc(a,b) : [a compare: b])
#define indexCompare(a, b) \
(((Map_c *) collection)->compareFunc ? \
 ((Map_c *) collection)->compareFunc(a,b) : [a compare: b])


@implementation Map_c

PHASE(Creating)

+ createBegin: aZone
{
  Map_c *newMap;

  newMap = [aZone allocIVars: self];
  return newMap;
}

- setCompareFunction: (compare_t)compareFunction
{
  compareFunc = compareFunction;
  return self;
}

- setCompareCStrings
{
  compareFunc = compareCStrings;
  return self;
}

- setCompareIntegers
{
  compareFunc = compareIntegers;
  return self;
}

- setCompareUnsignedIntegers
{
  compareFunc = compareUnsignedIntegers;
  return self;
}

- setCompareIDs
{
  compareFunc = compareIDs;
  return self;
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

static void
setCompareFunctionByName (id self, const char *funcName)
{
  if (strcmp (funcName, COMPARE_INT) == 0)
    [self setCompareIntegers];
  else if (strcmp (funcName, COMPARE_UNSIGNED) == 0)
    [self setCompareUnsignedIntegers];
  else if (strcmp (funcName, COMPARE_CSTRING) == 0)
    [self setCompareCStrings];
  else if (strcmp (funcName, COMPARE_ID) == 0)
    [self setCompareIDs];
  else
    raiseEvent (InvalidArgument, "Unknown compare function: %s",
                funcName);
}

- lispInCreate: expr
{
  id index, member;

  index = [(id) expr begin: scratchZone];
  for (member = LIST_INDEX_NEXT (index);
       LIST_INDEX_GETLOC (index) == Member;
       member = LIST_INDEX_NEXT (index))
    {
      if (keywordp (member))
        {
          const char *name = [member getKeywordName];
          
          if (strcmp (name, COMPARE_FUNCTION) == 0)
            setCompareFunctionByName (self,
                                      [lispInKeyword (index) getKeywordName]);
              
          else if (![self _lispInAttr_: index])
            raiseEvent (InvalidArgument, "unknown keyword `%s'", name);
        }
    }
  [index drop];
  return self;
}

- hdf5InCreate: hdf5Obj
{
  const char *funcName  = [hdf5Obj getAttribute: COMPARE_FUNCTION];

  if (funcName)
    setCompareFunctionByName (self, funcName);

  return self;
}

PHASE(Setting)
- lispIn: expr
{
  id index, member;
  id aZone = getZone (self);

  index = [(id) expr begin: scratchZone];
  for (member = LIST_INDEX_NEXT (index);
       LIST_INDEX_GETLOC (index) == Member;
       member = LIST_INDEX_NEXT (index))
    {
      if (keywordp (member))
        [index next];
      else if (pairp (member))
        {
          id pair = member;
          id keyExpr = [pair getCar];
          id valueExpr = [pair getCdr];
          id key, value;
          
          if (valuep (keyExpr))
            {
              if ([keyExpr getValueType] != fcall_type_slonglong)
                raiseEvent (InvalidArgument, "ArchiverValue not integer");
              key = (id) (PTRINT) [keyExpr getInteger];
            }
          else if (stringp (keyExpr))
            {
              if (COMPAREFUNCEQ (compareCStrings))
                key = (id) STRDUP ([keyExpr getC]);
              else
                key = [(id <Copy>)keyExpr copy: aZone];
            }
          else
            key = lispIn (aZone, keyExpr);
          value = lispIn (aZone, valueExpr);
          [(id) self at: key insert: value];
        }
      else
        raiseEvent (InvalidArgument,
                    "Expecting quoted dotted pair or cons expression");
    }
  [index drop];
  return self;
}

- hdf5In: hdf5Obj
{
  id aZone = getZone (self);

  if ([hdf5Obj getDatasetFlag])
    {
      Class class = [hdf5Obj getClass];
      unsigned i, c_count = [hdf5Obj getCount];
      const char **rowNames = [hdf5Obj readRowNames];
      const char *fmt = NULL;
      
      if (COMPAREFUNCEQ (compareIntegers))
        fmt = PTRINTFMT;
      else if (COMPAREFUNCEQ (compareUnsignedIntegers))
        fmt = PTRUINTFMT;
      else
        fmt = NULL;

      for (i = 0; i < c_count; i++)
        {
          id obj = [class create: aZone];
          id key;
          
          [hdf5Obj selectRecord: i];
          [hdf5Obj shallowLoadObject: obj];
          if (fmt)
            sscanf (rowNames[i], fmt, (int *) &key);
          else
            {
              if (COMPAREFUNCEQ (compareCStrings))
                key = (id) rowNames[i];
              else
                key = [String create: aZone setC: rowNames[i]];
            }
          [(id) self at: key insert: obj];
        }
      [[hdf5Obj getZone] free: rowNames]; // but not the contents
    }
  else
    {
      if ((COMPAREFUNCEQ (compareIDs) || compareFunc == NULL)
          && [hdf5Obj checkName: GROUP_KEYS])
        {
          id keyGroup = [[(id <HDF5>)[[HDF5 createBegin: aZone]
                         setParent: hdf5Obj]
                        setName: GROUP_KEYS]
                       createEnd];
          id valueGroup = [[(id <HDF5>)[[HDF5 createBegin: aZone]
                              setParent: hdf5Obj]
                             setName: GROUP_VALUES]
                            createEnd];
          {
            int process_object (id keyComponent)
              {
                id valueComponent = [[(id <HDF5>)[[HDF5 createBegin: aZone]
                                        setParent: valueGroup]
                                       setName: [keyComponent getHDF5Name]]
                                      createEnd];
                id key = hdf5In (aZone, keyComponent);
                id value = hdf5In (aZone, valueComponent);

                [self at: key insert: value];
                [valueComponent drop];
                return 0;
              }
            [keyGroup iterate: process_object];
            [keyGroup drop];
            [valueGroup drop];
          }
        }
      else if (COMPAREFUNCEQ (compareIntegers)
               || COMPAREFUNCEQ (compareUnsignedIntegers))
        {
          const char *fmt;
          
          int process_object (id keyComponent)
            {
              const char *keyStr = [keyComponent getHDF5Name];
              PTRINT key;
              id value = hdf5In (aZone, keyComponent);
              
              sscanf (keyStr, fmt, &key);
              [self at: (id) key insert: value];
              return 0;
            }

          fmt = COMPAREFUNCEQ (compareIntegers) ? PTRINTFMT : PTRUINTFMT;
          [hdf5Obj iterate: process_object];
        }
      else if COMPAREFUNCEQ (compareCStrings)
        {
          int process_object (id keyComponent)
            {
              const char *key = STRDUP ([keyComponent getHDF5Name]);
              id value =  hdf5In (aZone, keyComponent);

              [self at: (id) key insert: value];
              return 0;
            }
          [hdf5Obj iterate: process_object];
        }
      else // assume strings
        {
          int process_object (id keyComponent)
            {
              const char *key = STRDUP ([keyComponent getHDF5Name]);
              id value =  hdf5In (aZone, keyComponent);

              [self at: [String create: aZone setC: key] insert: value];
              return 0;
            }
          [hdf5Obj iterate: process_object];
        }
    }
  return self;
}

PHASE(Using)

//
// copy: -- standard method to copy internal state of object
//
- copy: aZone
{
  Map_c *newMap;
  id index;
  mapentry_t entry, newEntry;

  newMap = [aZone allocIVars: getClass (self)];
  setMappedAlloc (newMap);
  newMap->list = [List create: getCZone (getZone (self))];
  index = LIST_BEGIN (list);
  for (entry = (mapentry_t) LIST_INDEX_NEXT (index);
       LIST_INDEX_GETLOC (index) == Member;
       entry = (mapentry_t) LIST_INDEX_NEXT (index))
    {
      newEntry = ALLOCBLOCK (getZone (self), sizeof *entry);
      memcpy (newEntry, entry, sizeof *entry);
      [newMap->list addLast: (id) newEntry];
    }
  DROP (index);
  return newMap;
}


- at: aKey
{
  id index, member;
  mapentry_t  anEntry;

  index = LIST_BEGIN (list);
  for (member = nil; (anEntry = (mapentry_t) LIST_INDEX_NEXT (index)); )
    {
      if (compare (anEntry->key, aKey) == 0)
        {
          member = anEntry->member;
          break;
        }
    }
  DROP (index);
  return member;
}

- (BOOL)at: aKey insert: anObject
{
  id index;
  mapentry_t anEntry;
  int result;

  index = LIST_BEGIN (list);
  for (anEntry = (mapentry_t) LIST_INDEX_NEXT (index);
       LIST_INDEX_GETLOC (index) == Member;
       anEntry = (mapentry_t) LIST_INDEX_NEXT (index))
    if ((result = compare (anEntry->key, aKey)) == 0)
      {
        DROP (index);
        return NO;
      }
    else if (result > 0)
      break;
  
  {
    mapentry_t newEntry;
    
    newEntry = ALLOCBLOCK (getZone (self), sizeof *newEntry);
    newEntry->key = aKey;
    newEntry->member = anObject;
    
    LIST_INDEX_ADDBEFORE (index, (id) newEntry);
  }
  DROP (index);
  count++;
  return YES;
}

- at: aKey replace: anObject
{
  id index, oldMem;
  mapentry_t anEntry;

  index = LIST_BEGIN (list);
  for (anEntry = (mapentry_t) LIST_INDEX_NEXT (index);
       LIST_INDEX_GETLOC (index) == Member;
       anEntry = (mapentry_t) LIST_INDEX_NEXT (index))
    {
      if (compare (anEntry->key, aKey) == 0)
        {
          oldMem = anEntry->member;
          anEntry->member = anObject;
          DROP (index);
          return oldMem;
        }
    }
  DROP (index);
  return nil;
}

- (BOOL)at: aKey memberSlot: (id **)memPtr
{
  id index;
  mapentry_t anEntry, newEntry;
  int result;

  index = LIST_BEGIN (list);
  for (anEntry = (mapentry_t) LIST_INDEX_NEXT (index);
       LIST_INDEX_GETLOC (index) == Member;
       anEntry = (mapentry_t) LIST_INDEX_NEXT (index))
    {
      if ((result = compare (anEntry->key, aKey)) == 0)
        {
          DROP (index);
          *memPtr = &anEntry->member;
          return NO;
        }
      if (result > 0)
        break;
    }
  newEntry = ALLOCBLOCK (getZone (self), sizeof *newEntry);
  LIST_INDEX_ADDBEFORE (index, (id) newEntry);
  DROP (index);
  count++;
  newEntry->key = aKey;
  if (*memPtr)
    newEntry->member = **memPtr;
  *memPtr = &newEntry->member;
  return YES;
}

- (BOOL)at: aKey keySlot: (id **)keyPtr memberSlot: (id **)memPtr
{
  id index;
  mapentry_t anEntry, newEntry;
  int result;

  index = LIST_BEGIN (list);
  for (anEntry = (mapentry_t) LIST_INDEX_NEXT (index);
       LIST_INDEX_GETLOC (index) == Member;
       anEntry = (mapentry_t) LIST_INDEX_NEXT (index))
    {
      if ((result = compare (anEntry->key, aKey)) == 0)
        {
          DROP (index);
          *keyPtr = &anEntry->key;
          *memPtr = &anEntry->member;
          return NO;
      }
      if (result > 0)
        break;
    }
  newEntry = ALLOCBLOCK (getZone (self), sizeof *newEntry);
  LIST_INDEX_ADDBEFORE (index, (id) newEntry);
  DROP (index);
  count++;
  newEntry->key = aKey;
  *keyPtr = &newEntry->key;
  if (*memPtr)
    newEntry->member = **memPtr;
  *memPtr = &newEntry->member;
  return YES;
}

- (BOOL)containsKey: aKey
{
  id index;
  mapentry_t anEntry;
  
  index = LIST_BEGIN (list);
  for (anEntry = (mapentry_t) LIST_INDEX_NEXT (index);
       LIST_INDEX_GETLOC (index) == Member;
       anEntry = (mapentry_t) LIST_INDEX_NEXT (index))
    {
      if (compare (anEntry->key, aKey) == 0) 
        {
          DROP (index);
          return YES;
        }
    }
  DROP (index);
  return NO; 
}
  
- removeKey: aKey
{
  id index, oldMem;
  mapentry_t anEntry;
  int result;

  index = LIST_BEGIN (list);
  oldMem = nil;
  for (anEntry = (mapentry_t) LIST_INDEX_NEXT (index);
       LIST_INDEX_GETLOC (index) == Member;
       anEntry = (mapentry_t) LIST_INDEX_NEXT (index))
    {
      if ((result = compare (anEntry->key, aKey)) == 0)
        {
          LIST_INDEX_REMOVE (index);
          oldMem = anEntry->member;
          FREEBLOCK_SIZE (getZone (self), anEntry, sizeof (*anEntry));
          count--;
          break;
        }
      if (result > 0)
        break;
    }
  DROP (index);
  return oldMem;
}

- (void)forEachKey: (SEL)aSelector
{
  id index, key;

  index = [(id) self begin: scratchZone];
  for (MAP_INDEX_NEXTKEY (index, &key);
       MAP_INDEX_GETLOC (index) == Member;
       MAP_INDEX_NEXTKEY (index, &key))
    [key perform: aSelector];
  [index drop];
}

- (void)forEachKey: (SEL)aSelector : arg1
{
  id index, key;

  index = [(id) self begin: scratchZone];
  for (MAP_INDEX_NEXTKEY (index, &key);
       MAP_INDEX_GETLOC (index) == Member;
       MAP_INDEX_NEXTKEY (index, &key))
    [key perform: aSelector with: arg1];
  [index drop];
}

- (void)forEachKey: (SEL)aSelector : arg1 : arg2
{ 
  id index, key;
  
  index = [(id) self begin: scratchZone];
  for (MAP_INDEX_NEXTKEY (index, &key);
       MAP_INDEX_GETLOC (index) == Member;
       MAP_INDEX_NEXTKEY (index, &key))
    [key perform: aSelector with: arg1 with: arg2];
  [index drop];
}

- (void)forEachKey: (SEL)aSelector : arg1 : arg2 : arg3
{ 
  id index, key;
  
  index = [(id) self begin: scratchZone];
  for (MAP_INDEX_NEXTKEY (index, &key);
       MAP_INDEX_GETLOC (index) == Member;
       MAP_INDEX_NEXTKEY (index, &key))
    [key perform: aSelector with: arg1 with: arg2 with: arg3];
  [index drop];
}

- (id <MapIndex>)begin: (id <Zone>)aZone
{
  MapIndex_c *newIndex;
  
  newIndex = [aZone allocIVars: id_MapIndex_c];
  setMappedAlloc (newIndex);
  newIndex->collection = self;
  newIndex->listIndex = LIST_BEGIN (list);
  return newIndex;
}

- (id <MapIndex>)mapBegin: (id <Zone>)aZone
{
  return [self begin: aZone];
}

- _createIndex_: aZone forIndexSubclass: anIndexSubclass
{
  MapIndex_c *newIndex;

  newIndex = COMPONENT_ALLOCIVARS (aZone, anIndexSubclass);
  setMappedAlloc (newIndex);
  newIndex->collection = self;
  newIndex->listIndex  = LIST_BEGIN (list);
  return newIndex;
}

- _createPermutedIndex_: aZone forIndexSubclass: anIndexSubclass
{
  MapIndex_c *newIndex;

  newIndex = COMPONENT_ALLOCIVARS (aZone, anIndexSubclass);
  setMappedAlloc (newIndex);
  newIndex->collection = self;
  newIndex->listIndex  = [list beginPermuted: getCZone (aZone)];

  return newIndex;
}


- createIndex: aZone fromMember: anObject
{
  MapIndex_c *newIndex;
  id anEntry, listIndex;
  
  newIndex = COMPONENT_ALLOCIVARS (aZone, id_MapIndex_c);
  setMappedAlloc (newIndex);
  newIndex->collection = self;
  listIndex = LIST_BEGIN (list);
  LIST_INDEX_SETLOC (listIndex, Start);
  
  for (anEntry = LIST_INDEX_NEXT (listIndex);
       LIST_INDEX_GETLOC (listIndex) == Member;
       anEntry = LIST_INDEX_NEXT (listIndex))
    {
      if (((mapentry_t) anEntry)->member == anObject)
	{
	  newIndex->listIndex = listIndex;
          return newIndex;
	}
    }

  DROP (listIndex);
  DROP (newIndex);
  return nil;
}

- (compare_t)getCompareFunction
{
  return compareFunc;
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  id index;
  mapentry_t anEntry;

  if (includeBlocks (mapalloc))
    {
      mapalloc->size = sizeof *anEntry;
      index = LIST_BEGIN (list);
      for (anEntry = (mapentry_t) LIST_INDEX_NEXT (index);
           LIST_INDEX_GETLOC (index) == Member;
           anEntry = (mapentry_t) LIST_INDEX_NEXT (index))
        mapAlloc (mapalloc, anEntry);
      DROP (index);
    }
  mapObject (mapalloc, list);
}

- (BOOL)allSameKeyClass
{
  id <MapIndex> mi;
  id key;
  Class firstClass;
  BOOL ret = YES;
  
  mi = [self begin: scratchZone];
  if (MAP_INDEX_NEXTKEY (mi, &key))
    {
      firstClass = [key class];
      while (MAP_INDEX_GETLOC (mi) == Member)
        {
          if ([key class] != firstClass)
            {
              ret = NO;
              break;
            }
          MAP_INDEX_NEXTKEY (mi, &key);
        }
    }
  [mi drop];
  return ret;
}

- (BOOL)allStringKeys
{
  if (![self allSameKeyClass])
    return NO;
  else
    {
      id <MapIndex> mi;
      id key;
      BOOL ret = YES;

      mi = [self begin: scratchZone];
      if (MAP_INDEX_NEXTKEY (mi, &key) == nil)
        ret = NO;
      else
        ret = stringp (key);
      [mi drop];

      return ret;
    }
}
  

- (BOOL)allSameClass
{
  BOOL sameMembers = [super allSameClass];

  if (sameMembers)
    {
      if (compareFunc)
        return YES;
      else
        return [self allSameKeyClass];
    }
  return NO;
}

- (void)_lispOut_: outputCharStream deep: (BOOL)deepFlag
{
  id index, member, key;

  [outputCharStream catStartMakeInstance: [self getTypeName]];
  index = [(id) self begin: scratchZone];
  for (member = MAP_INDEX_NEXTKEY (index, &key);
       MAP_INDEX_GETLOC (index) == Member;
       member = MAP_INDEX_NEXTKEY (index, &key))
    {
      [outputCharStream catSeparator];
      [outputCharStream catStartCons];
      [outputCharStream catSeparator];
      if (COMPAREFUNCEQ (compareIDs) || compareFunc == NULL)
        {
          if (deepFlag)
            [key lispOutDeep: outputCharStream];
          else
            [key lispOutShallow: outputCharStream];
        }
      else if (COMPAREFUNCEQ (compareUnsignedIntegers))
        [outputCharStream catUnsigned: (unsigned) (PTRUINT) key];
      else if (COMPAREFUNCEQ (compareIntegers))
        [outputCharStream catInt: (int) (PTRINT) key];
      else if (COMPAREFUNCEQ (compareCStrings))
        [outputCharStream catString: (const char *) key];
      else
        abort ();
      [outputCharStream catSeparator];
      if (deepFlag)
        [member lispOutDeep: outputCharStream];
      else
        [member lispOutShallow: outputCharStream];
      [outputCharStream catEndCons];
    }
  [index drop];
  
  [self _lispOutAttr_: outputCharStream];

  if (compareFunc)
    {
      [outputCharStream catSeparator];
      [outputCharStream catKeyword: COMPARE_FUNCTION];
      
      [outputCharStream catSeparator];
      if (COMPAREFUNCEQ (compareIntegers))
        [outputCharStream catKeyword: COMPARE_INT];
      else if (COMPAREFUNCEQ (compareUnsignedIntegers))
        [outputCharStream catKeyword: COMPARE_UNSIGNED];
      else if (COMPAREFUNCEQ (compareCStrings))
        [outputCharStream catKeyword: COMPARE_CSTRING];
      else if (COMPAREFUNCEQ (compareIDs))
        [outputCharStream catKeyword: COMPARE_ID];
      else
        raiseEvent (InvalidArgument, "Unknown compare function");
    }
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

static void
hdf5_store_compare_function_attribute (id hdf5Obj, compare_t compareFunc)
{
  if (COMPAREFUNCEQ (compareIDs))
    [hdf5Obj storeAttribute: COMPARE_FUNCTION value: COMPARE_ID];
  else if (COMPAREFUNCEQ (compareIntegers))
    [hdf5Obj storeAttribute: COMPARE_FUNCTION value: COMPARE_INT];
  else if (COMPAREFUNCEQ (compareUnsignedIntegers))
    [hdf5Obj storeAttribute: COMPARE_FUNCTION value: COMPARE_UNSIGNED];
  else if (COMPAREFUNCEQ (compareCStrings))
    [hdf5Obj storeAttribute: COMPARE_FUNCTION value: COMPARE_CSTRING];
}

- (void)hdf5OutDeep: (id <HDF5>)hdf5Obj 
{
  id aZone = getZone (self);
  id key, value;
  BOOL keyStringFlag = NO;
  
  [hdf5Obj storeTypeName: [self getTypeName]];
  
  if ((compareFunc == NULL || COMPAREFUNCEQ (compareIDs))
      && !(keyStringFlag = [self allStringKeys]))
    {
      id keyGroup = [[(id <HDF5>)[[[HDF5 createBegin: aZone]
                         setWriteFlag: YES]
                        setParent: hdf5Obj]
                       setName: GROUP_KEYS]
                      createEnd];
      id valueGroup = [[(id <HDF5>)[[[HDF5 createBegin: aZone]
                           setWriteFlag: YES]
                          setParent: hdf5Obj]
                         setName: GROUP_VALUES]
                        createEnd];
      
      id <MapIndex> mi = [self begin: scratchZone];
      
      while ((value = MAP_INDEX_NEXTKEY (mi, &key)))
        {
          id valueInstanceGroup, keyInstanceGroup;
          char buf[DSIZE (unsigned) + 1];
          unsigned offset = [mi getOffset];
          
          sprintf (buf, "%u", offset);
          keyInstanceGroup = [[(id <HDF5>)[[[HDF5 createBegin: aZone]
                                  setWriteFlag: YES]
                                 setParent: keyGroup]
                                setName: buf]
                               createEnd];
          [key hdf5OutDeep: keyInstanceGroup];
          [keyInstanceGroup drop];
          
          valueInstanceGroup = [[(id <HDF5>)[[[HDF5 createBegin: aZone]
                                    setWriteFlag: YES]
                                   setParent: valueGroup]
                                  setName: buf]
                                 createEnd];
          [value hdf5OutDeep: valueInstanceGroup];
          [valueInstanceGroup drop];
        }
      if (compareFunc)
        [hdf5Obj storeAttribute: COMPARE_FUNCTION value: COMPARE_ID];
      [keyGroup drop];
      [valueGroup drop];
      [mi drop];
    }
  else
    {
      void store_map_deep (const char * (*getKeyStr) (id key))
        {
          id key, value;
          id <MapIndex> mi = [self begin: scratchZone];
          
          while ((value = MAP_INDEX_NEXTKEY (mi, &key)))
            {
              id valueInstanceGroup = [[(id <HDF5>)[[[HDF5 createBegin: aZone]
                                           setWriteFlag: YES]
                                          setParent: hdf5Obj]
                                         setName: getKeyStr (key)]
                                        createEnd];
              
              [value hdf5OutDeep: valueInstanceGroup];
              [valueInstanceGroup drop];
            }
          [mi drop];
        }
      
      hdf5_store_compare_function_attribute (hdf5Obj, compareFunc);
      if (keyStringFlag)
        {
          const char *getKeyStr (id key)
            {
              return [key getC];
            }
          store_map_deep (getKeyStr);
        }
      else if (COMPAREFUNCEQ (compareCStrings))
        {
          const char *getKeyStr (id key)
            {
              return (const char *) key;
            }
          store_map_deep (getKeyStr);
        }
      else if (COMPAREFUNCEQ (compareUnsignedIntegers))
        {
          char buf[DSIZE (unsigned) + 1];
          
          const char *getKeyStr (id key)
            {
              sprintf (buf, PTRUINTFMT, (PTRUINT) key);
              return buf;
            }
          store_map_deep (getKeyStr);
        }
      else if (COMPAREFUNCEQ (compareIntegers))
        {
          char buf[DSIZE (int) + 1];
          
          const char *getKeyStr (id key)
            {
              sprintf (buf, PTRINTFMT, (PTRINT) key);
              return buf;
            }
          store_map_deep (getKeyStr);
        }
      else 
        abort ();
    }
}
  
- (void)hdf5OutShallow: (id <HDF5>)hdf5Obj
{
  if (![self allSameClass])
    raiseEvent (SaveError,
                "shallow HDF5 serialization on Map must be same type");
  else
    {
      id aZone = getZone (self);
      Class memberProto = [self getFirst];
      id compoundType = [[(id <HDF5CompoundType>)[HDF5CompoundType createBegin: aZone]
                           setPrototype: memberProto]
                          createEnd];
      id dataset =
        [[(id <HDF5>)[[[[(id <HDF5>)[HDF5 createBegin: aZone]
               setName: [hdf5Obj getHDF5Name]]
              setWriteFlag: YES]
             setParent: hdf5Obj]
            setCompoundType: compoundType]
           setCount: [self getCount]]
          createEnd];
      id member, key;
      id <MapIndex> mi = [self begin: aZone];
      BOOL keyIsString = NO;
      
      [dataset storeTypeName: [self getTypeName]];
      [dataset storeComponentTypeName: [memberProto getTypeName]];
      hdf5_store_compare_function_attribute (dataset, compareFunc);
      
      if (MAP_INDEX_NEXTKEY (mi, &key) == nil)
        key = nil;
      
      if (COMPAREFUNCEQ (compareIDs) || compareFunc == NULL)
        keyIsString = stringp (key);
      
      [mi setLoc: Start];
      while ((member = MAP_INDEX_NEXTKEY (mi, &key)))
        {
          unsigned rn = [mi getOffset];
          
          if (keyIsString)
            [dataset nameRecord: rn name: [key getC]];
          else if (COMPAREFUNCEQ (compareCStrings))
            [dataset nameRecord: rn name: (const char *) key];
          else if (COMPAREFUNCEQ (compareUnsignedIntegers))
            [dataset numberRecord: (PTRUINT) key];
          else if (COMPAREFUNCEQ (compareIntegers))
            {
              char buf[DSIZE (int) + 1];
              
              sprintf (buf, PTRINTFMT, (PTRINT) key);
              [dataset nameRecord: rn name: buf];
            }
          else
            raiseEvent (SaveError, "cannot shallow-serialize Map %s",
                        [hdf5Obj getHDF5Name]);
          
          [dataset selectRecord: rn];
          [member hdf5OutShallow: dataset];
        }
      [dataset writeLevels];
      [dataset writeRowNames];
      [dataset drop];
      [mi drop];
      [compoundType drop];
    }
}

@end

@implementation MapIndex_c
PHASE(Using)

- next
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) LIST_INDEX_NEXT (listIndex);
  if (anEntry != NULL)
    return anEntry->member;
  return NULL;
}

- next: (id *)key
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) LIST_INDEX_NEXT (listIndex);
  if (anEntry != NULL)
    {
      if (key)
        *key = anEntry->key;
      return anEntry->member;
    }
  return nil;
}

- prev
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) LIST_INDEX_PREV (listIndex);
  if (anEntry != NULL)
    return anEntry->member;
  return NULL;
}

- prev: (id *)key
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) LIST_INDEX_PREV (listIndex);
  if (anEntry != NULL)
    {
      if (key) 
        *key = anEntry->key;
      return anEntry->member;
    }
  return NULL;
}

- get
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) LIST_INDEX_GET (listIndex);
  if (!anEntry)
    return nil;
  return anEntry->member;
}

- get: (id *)key
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) LIST_INDEX_GET (listIndex);
  if (!anEntry)
    return nil;
  if (key) 
    *key = anEntry->key;
  return anEntry->member;
}

- getKey
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) LIST_INDEX_GET (listIndex);
  if (!anEntry)
    return nil;
  return anEntry->key;
}

- (unsigned long)getKeyValue
{
  return (unsigned long) (PTRUINT) [self getKey];
}

- replace: anObject
{
  mapentry_t anEntry;
  id oldMem;

  anEntry = (mapentry_t) LIST_INDEX_GET (listIndex);
  if (!anEntry)
    return nil;
  oldMem  = anEntry->member;
  anEntry->member = anObject;
  return oldMem;
}

- remove
{
  mapentry_t anEntry;
  id oldMem;

  anEntry = (mapentry_t) LIST_INDEX_REMOVE (listIndex);
  if (!anEntry)
    return nil;
  oldMem  = anEntry->member;
  FREEBLOCK_SIZE (getZone (collection), anEntry, sizeof *anEntry);
  collection->count--;
  return oldMem;
}

- setKey: aKey
{
  mapentry_t anEntry;

  LIST_INDEX_SETLOC (listIndex, Start);
  for (anEntry = (mapentry_t) LIST_INDEX_NEXT (listIndex);
       LIST_INDEX_GETLOC (listIndex) == Member;
       anEntry = (mapentry_t) LIST_INDEX_NEXT (listIndex))
    {
      if (indexCompare (anEntry->key, aKey) == 0)
        return anEntry->member;
    }
  LIST_INDEX_SETLOC (listIndex, Start);
  return nil;
}

- (id <Symbol>)getLoc
{
  return LIST_INDEX_GETLOC (listIndex);
}

- (void)setLoc: (id <Symbol>)locSymbol
{
  LIST_INDEX_SETLOC (listIndex, locSymbol);
}

- (int)getOffset
{
  return [listIndex getOffset];
}

- setOffset: (unsigned)offset
{
  return [listIndex setOffset: offset];
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  mapObject (mapalloc, listIndex);
}

@end
