// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Map.m
Description:  sorted map implemented as linear list 
Library:      collections
*/

#import <collections/Map.h>
#import <defobj/defalloc.h>

#include <objc/objc-api.h> // object_get_class
#include <collections/predicates.h> // keywordp, stringp

#import <defobj.h> // hdf5in, HDF5
#include <misc.h> // XFREE

#include <swarmconfig.h> // HAVE_HDF5

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
  while ((member = [index next]))
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
  id aZone = [self getZone];  

  index = [(id) expr begin: scratchZone];
  while ((member = [index next]) != nil)
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
              if ([keyExpr getValueType] != _C_INT)
                raiseEvent (InvalidArgument, "ArchiverValue not integer");
              key = (id) (PTRINT) [keyExpr getInteger];
            }
          else if (stringp (keyExpr))
            {
              if (COMPAREFUNCEQ (compareCStrings))
                key = (id) strdup ([keyExpr getC]);
              else
                key = [keyExpr copy: aZone];
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
  id aZone = [self getZone];

  if ([hdf5Obj getDatasetFlag])
    {
      id aZone = [self getZone];
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
      XFREE (rowNames); // but not the contents
    }
  else
    {
      if ((COMPAREFUNCEQ (compareIDs) || compareFunc == NULL)
          && [hdf5Obj checkName: GROUP_KEYS])
        {
          id keyGroup = [[[[[HDF5 createBegin: aZone]
                          setCreateFlag: NO]
                         setParent: hdf5Obj]
                        setName: GROUP_KEYS]
                       createEnd];
          id valueGroup = [[[[[HDF5 createBegin: aZone]
                               setCreateFlag: NO]
                              setParent: hdf5Obj]
                             setName: GROUP_VALUES]
                            createEnd];
          {
            int process_object (id keyComponent)
              {
                id valueComponent = [[[[[HDF5 createBegin: aZone]
                                         setCreateFlag: NO]
                                        setParent: valueGroup]
                                       setName: [keyComponent getName]]
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
              const char *keyStr = [keyComponent getName];
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
              const char *key = strdup ([keyComponent getName]);
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
              const char *key = strdup ([keyComponent getName]);
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
  index = [list begin: scratchZone];
  while ((entry = (mapentry_t) [index next]))
    {
      newEntry = [getZone (self) allocBlock: sizeof *entry];
      memcpy (newEntry, entry, sizeof *entry);
      [newMap->list addLast: (id) newEntry];
    }
  [index drop];
  return newMap;
}


- at: aKey
{
  id index, member;
  mapentry_t  anEntry;

  index = [list begin: scratchZone];
  for (member = nil; (anEntry = (mapentry_t) [index next]); )
    {
      if (compare (anEntry->key, aKey) == 0)
        {
          member = anEntry->member;
          break;
        }
    }
  [index drop];
  return member;
}

- (BOOL)at: aKey insert: anObject
{
  id index;
  mapentry_t newEntry, anEntry;
  int result;

  newEntry = [getZone (self) allocBlock: sizeof *newEntry];
  newEntry->key = aKey;
  newEntry->member = anObject;

  index = [list begin: scratchZone];
  while ((anEntry = (mapentry_t)[index next]))
    if ((result = compare (anEntry->key, aKey)) == 0)
      {
        [index drop];
        return NO;
      }
    else if (result > 0)
      break;
  [index addBefore: (id)newEntry];
  [index drop];
  count++;
  return YES;
}

- at: aKey replace: anObject
{
  id index, oldMem;
  mapentry_t anEntry;

  index = [list begin: scratchZone];
  while ((anEntry = (mapentry_t) [index next]))
    {
      if (compare (anEntry->key, aKey) == 0)
        {
          oldMem = anEntry->member;
          anEntry->member = anObject;
          [index drop];
          return oldMem;
        }
    }
  [index drop];
  return nil;
}

- (BOOL)at: aKey memberSlot: (id **)memPtr
{
  id index;
  mapentry_t anEntry, newEntry;
  int result;

  index = [list begin: scratchZone];
  while ((anEntry = (mapentry_t)[index next]))
    {
      if ((result = compare (anEntry->key, aKey)) == 0)
        {
          [index drop];
          *memPtr = &anEntry->member;
          return NO;
        }
      if (result > 0)
        break;
    }
  newEntry = [getZone (self) allocBlock: sizeof *newEntry];
  [index addBefore: (id) newEntry];
  [index drop];
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

  index = [list begin: scratchZone];
  while ((anEntry = (mapentry_t)[index next]))
    {
      if ((result = compare (anEntry->key, aKey)) == 0)
        {
          [index drop];
          *keyPtr = &anEntry->key;
          *memPtr = &anEntry->member;
          return NO;
      }
      if (result > 0)
        break;
    }
  newEntry = [getZone (self) allocBlock: sizeof *newEntry];
  [index addBefore: (id) newEntry];
  [index drop];
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
  
  index = [list begin: scratchZone];
  while ((anEntry = (mapentry_t)[index next]))
    {
      if (compare(anEntry->key, aKey) == 0) 
        {
          [index drop];
          return YES;
        }
    }
  [index drop];
  return NO; 
}
  
- removeKey: aKey
{
  id index, oldMem;
  mapentry_t anEntry;
  int result;

  index = [list begin: scratchZone];
  oldMem = nil;
  while ((anEntry = (mapentry_t) [index next]))
    {
      if ((result = compare (anEntry->key, aKey)) == 0)
        {
          [index remove];
          oldMem = anEntry->member;
          [getZone (self) freeBlock: anEntry blockSize: sizeof *anEntry];
        count--;
        break;
        }
      if (result > 0)
        break;
    }
  [index drop];
  return oldMem;
}

- begin: aZone
{
  MapIndex_c *newIndex;
  
  newIndex = [aZone allocIVars: [MapIndex_c self]];
  setMappedAlloc (newIndex);
  newIndex->collection = self;
  newIndex->listIndex = [list begin: getCZone (aZone)];
  return newIndex;
}

- _createIndex_: aZone forIndexSubclass: anIndexSubclass
{
  MapIndex_c *newIndex;

  newIndex = [aZone allocIVars: anIndexSubclass];
  setMappedAlloc (newIndex);
  newIndex->collection = self;
  newIndex->listIndex  = [list begin: getCZone (aZone)];
  return newIndex;
}

- _createPermutedIndex_: aZone forIndexSubclass: anIndexSubclass
{
  MapIndex_c *newIndex;

  newIndex = [aZone allocIVars: anIndexSubclass];
  setMappedAlloc (newIndex);
  newIndex->collection = self;
  newIndex->listIndex  = [list beginPermuted: getCZone (aZone)];

  return newIndex;
}

- createIndex: aZone fromMember: anObject
{
  MapIndex_c *newIndex;
  id anEntry, listIndex;
  
  newIndex = [aZone allocIVars: [MapIndex_c self]];
  setMappedAlloc (newIndex);
  newIndex->collection = self;
  listIndex = [list begin: getCZone (aZone)];
  [listIndex setLoc: Start];
  anEntry = [listIndex next];
  while (anEntry)
    {
      if (((mapentry_t) anEntry)->member == anObject)
	{
	  newIndex->listIndex = listIndex;
          return newIndex;
	}
      anEntry = [listIndex next];
    }

  [listIndex drop];
  [newIndex drop];
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
      index = [list begin: scratchZone];
      while ((anEntry = (mapentry_t) [index next]))
        mapAlloc (mapalloc, anEntry);
      [index drop];
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
  if ([mi next: &key])
    {
      firstClass = [key class];
      while ([mi getLoc] == (id) Member)
        {
          if ([key class] != firstClass)
            {
              ret = NO;
              break;
            }
          [mi next: &key];
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
      if ([mi next: &key] == nil)
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

- _lispOut_: outputCharStream deep: (BOOL)deepFlag
{
  id index, member, key;

  [outputCharStream catC: "(" MAKE_INSTANCE_FUNCTION_NAME " '"];
  [outputCharStream catC: [self getTypeName]];

  index = [(id) self begin: scratchZone];
  while ((member = [index next: &key]))
    {
      [outputCharStream catC: " (cons "];
      if (COMPAREFUNCEQ (compareIDs) || compareFunc == NULL)
        {
          if (deepFlag)
            [key lispOutDeep: outputCharStream];
          else
            [key lispOutShallow: outputCharStream];
        }
      else if (COMPAREFUNCEQ (compareUnsignedIntegers))
        {
          char buf[DSIZE (unsigned)];
          
          sprintf (buf, PTRINTFMT, (PTRINT) key);
          [outputCharStream catC: buf];
        }
      else if (COMPAREFUNCEQ (compareIntegers))
        {
          char buf[DSIZE (unsigned)];
          
          sprintf (buf, PTRINTFMT, (PTRINT) key);
          [outputCharStream catC: buf];
        }
      else if (COMPAREFUNCEQ (compareCStrings))
        {
          [outputCharStream catC: "\""];
          [outputCharStream catC: (const char *) key];
          [outputCharStream catC: "\""];
        }
      else
        abort ();
      [outputCharStream catC: " "];
      if (deepFlag)
        [member lispOutDeep: outputCharStream];
      else
        [member lispOutShallow: outputCharStream];
      [outputCharStream catC: ")"];
    }
  [index drop];
  
  [self _lispOutAttr_: outputCharStream];

  if (compareFunc)
    {
      [outputCharStream catC: " #:"];
      [outputCharStream catC: COMPARE_FUNCTION];
      
      [outputCharStream catC: " #:"];
      if (COMPAREFUNCEQ (compareIntegers))
        [outputCharStream catC: COMPARE_INT];
      else if (COMPAREFUNCEQ (compareUnsignedIntegers))
        [outputCharStream catC: COMPARE_UNSIGNED];
      else if (COMPAREFUNCEQ (compareCStrings))
        [outputCharStream catC: COMPARE_CSTRING];
      else if (COMPAREFUNCEQ (compareIDs))
        [outputCharStream catC: COMPARE_ID];
      else
        raiseEvent (InvalidArgument, "Unknown compare function");
    }
  [outputCharStream catC: ")"];
  return self;
}

- lispOutDeep: stream
{
  return [self _lispOut_: stream deep: YES];
}

- lispOutShallow: stream
{
  return [self _lispOut_: stream deep: NO];
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

- hdf5OutDeep: hdf5Obj 
{
  id aZone = [hdf5Obj getZone];
  id key, value;
  BOOL keyStringFlag = NO;
  
  [hdf5Obj storeTypeName: [self getTypeName]];
  
  if ((compareFunc == NULL || COMPAREFUNCEQ (compareIDs))
      && !(keyStringFlag = [self allStringKeys]))
    {
      id keyGroup = [[[[[HDF5 createBegin: aZone]
                         setCreateFlag: YES]
                        setParent: hdf5Obj]
                       setName: GROUP_KEYS]
                      createEnd];
      id valueGroup = [[[[[HDF5 createBegin: aZone]
                           setCreateFlag: YES]
                          setParent: hdf5Obj]
                         setName: GROUP_VALUES]
                        createEnd];
      
      id <MapIndex> mi = [self begin: scratchZone];
      
      while ((value = [mi next: &key]))
        {
          id valueInstanceGroup, keyInstanceGroup;
          char buf[DSIZE (unsigned) + 1];
          unsigned offset = [mi getOffset];
          
          sprintf (buf, "%u", offset);
          keyInstanceGroup = [[[[[HDF5 createBegin: aZone]
                                  setCreateFlag: YES]
                                 setParent: keyGroup]
                                setName: buf]
                               createEnd];
          [key hdf5OutDeep: keyInstanceGroup];
          [keyInstanceGroup drop];
          
          valueInstanceGroup = [[[[[HDF5 createBegin: aZone]
                                    setCreateFlag: YES]
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
          
          while ((value = [mi next: &key]))
            {
              id valueInstanceGroup = [[[[[HDF5 createBegin: aZone]
                                           setCreateFlag: YES]
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
  return self;
}
  
- hdf5OutShallow: hdf5Obj
{
  if (![self allSameClass])
    raiseEvent (SaveError,
                "shallow HDF5 serialization on Map must be same type");
  else
    {
      id aZone = [self getZone];
      Class memberProto = [self getFirst];
      id compoundType = [[[HDF5CompoundType createBegin: aZone]
                           setClass: [memberProto class]]
                          createEnd];
      id dataset =
        [[[[[[[HDF5 createBegin: aZone]
               setName: [hdf5Obj getName]]
              setCreateFlag: YES]
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
      
      if ([mi next: &key] == nil)
        key = nil;
      
      if (COMPAREFUNCEQ (compareIDs) || compareFunc == NULL)
        keyIsString = stringp (key);
      
      [mi setLoc: Start];
      while ((member = [mi next: &key]))
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
                        [hdf5Obj getName]);
          
          [dataset selectRecord: rn];
          [member hdf5OutShallow: dataset];
        }
      [dataset writeLevels];
      [dataset writeRowNames];
      [dataset drop];
      [mi drop];
      [compoundType drop];
    }
  return self;
}

@end

@implementation MapIndex_c
PHASE(Using)
- next
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) [listIndex next];
  if (anEntry != NULL)
    return anEntry->member;
  return NULL;
}

- next: (id *)key
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) [listIndex next];
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

  anEntry = (mapentry_t) [listIndex prev];
  if (anEntry != NULL)
    return anEntry->member;
  return NULL;
}

- prev: (id *)key
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) [listIndex prev];
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

  anEntry = (mapentry_t) [listIndex get];
  if (!anEntry)
    return nil;
  return anEntry->member;
}

- get: (id *)key
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) [listIndex get];
  if (!anEntry)
    return nil;
  if (key) 
    *key = anEntry->key;
  return anEntry->member;
}

- getKey
{
  mapentry_t anEntry;

  anEntry = (mapentry_t) [listIndex get];
  if (!anEntry)
    return nil;
  return anEntry->key;
}

- replace: anObject
{
  mapentry_t anEntry;
  id oldMem;

  anEntry = (mapentry_t) [listIndex get];
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

  anEntry = (mapentry_t)[listIndex remove];
  if (!anEntry)
    return nil;
  oldMem  = anEntry->member;
  [getZone (collection) freeBlock: anEntry blockSize: sizeof *anEntry];
  collection->count--;
  return oldMem;
}

- setKey: aKey
{
  mapentry_t anEntry;

  [listIndex setLoc: Start];
  while ((anEntry = (mapentry_t) [listIndex next]))
    {
      if (indexCompare (anEntry->key, aKey) == 0)
        return anEntry->member;
    }
  [listIndex setLoc: Start];
  return nil;
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

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  mapObject (mapalloc, listIndex);
}

@end
