// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Zone.m
Description:  superclass support for all zone implementations
Library:      defobj
*/

#import <defobj/Zone.h>
#import <defobj/defalloc.h>

#import <collections/List_linked.h>
#import <swarmconfig.h>
#ifdef HAVE_JDK
#import <defobj/directory.h>
#endif

#include <misc.h> // memset, xmalloc, XFREE, MAX_ALIGNMENT
#include "internal.h"

//
// temporary hack to guarantee double word alignment of allocations
//
static inline void *
dalloc (size_t blockSize)
{
  static BOOL notAligned = NO;
  void *block;

  block = xmalloc (blockSize);

// if flag is set at compile time (-DPTR_MALLOC_DALIGN), then runtime check
// for double-word alignment check can be suppressed from this code.

#ifdef PTR_MALLOC_DALIGN
  if (PTR_MALLOC_DALIGN)
    return block;
#endif

  if (((unsigned long) block & ~0x7) == (unsigned long) block)
    return block;
  
  if (!notAligned)
    {
      notAligned = YES;
      fprintf (stderr,
               "Double word alignment of malloc allocations not guaranteed\n"
               "on local machine architecture.\n"
               "Please report to swarm@santafe.edu.\n"
               "Standard fixup taken, execution continuing...\n" );
    }
  
  XFREE (block);
  block = xmalloc (blockSize + 7);
  return ((void *) (((unsigned long) block + 7) & ~0x7));
}

//
// _obj_fillalloc, _obj_fillfree -- 
//   fill patterns for allocated and freed blocks, for debugging
//
unsigned char _obj_fillalloc = 0xaa, _obj_fillfree = 0xff;


@implementation Zone_c

PHASE(Creating)

+ createBegin: aZone
{
  Zone_c *newZone;

  // the very first zone is created externally by explicit initialization

  newZone = [aZone allocIVars: self];
  return newZone;
}

- (void)setPageSize: (size_t)pageSize
{
  raiseEvent( NotImplemented,
    "> PageSize option not yet implemented.\n"
    "> Page size must be a power of two, at least 256, and no greater than\n"
    "> the page size of the zone owner.  Page size requested was: %d.\n",
    pageSize );
}

- createEnd
{
  int indexOffset = 2 * sizeof (id);

  if (createByMessageToCopy (self, createEnd))
    return self;

  setMappedAlloc (self);
  setNextPhase (self);

  // create internal support objects for zone

  componentZone = [self allocIVarsComponent: id_ComponentZone_c];
  ((ComponentZone_c *) componentZone)->baseZone = self;
  ((ComponentZone_c *) componentZone)->componentZone = componentZone;

  population = [List_linked createBegin: componentZone];
  [population setIndexFromMemberLoc: -indexOffset];
  population = [(id)population createEnd];
  populationTotal = 0;

  // zero internal allocation statistics to remove zone overhead

  if (_obj_debug)
    {
      objectCount = 0;
      objectTotal = 0;
    }
  return self;
}

PHASE(Using)

- (size_t)getPageSize
{
  return 0;
}

//
// allocIVars: -- allocate and initialize instance variables for object
//
- allocIVars: (Class)aClass
{
  Object_s *newObject;
  size_t size = aClass->instance_size;

  //!! need to guarantee that class inherits from Object_s, to define slot
  //!! for zbits

  // allocate object of required size, including links in object header

  newObject = (Object_s *) dalloc (size + 2 * sizeof (id));
  
  // add object to the population list, skipping over links in object header

  newObject = (Object_s *) ((id *) newObject + 2);
  if (population)
    {
      [population addLast: newObject];
      populationTotal += size;
    }

  // initialize and return the newly allocated object

  memset (newObject, 0, size);
  setClass (newObject, aClass);   
  newObject->zbits = (unsigned long) self;   
  return (id) newObject;
}

//
// copyIVars: -- allocate object with contents copied from existing object
//
- copyIVars: anObject
{
  Object_s  *newObject;
  int       instanceSize;

  // allocate object of required size, including links in object header

  instanceSize = getClass (anObject)->instance_size;
  newObject = (Object_s *) dalloc (instanceSize + 2 * sizeof (id));

  // add object to the population list, skipping over links in object header

  newObject = (Object_s *) ((id *) newObject + 2);
  [population addLast: newObject];
  populationTotal += instanceSize;

  // initialize and return the newly allocated object

  memcpy (newObject, anObject, instanceSize);
  newObject->zbits = (unsigned long)self;
  if (getMappedAlloc ((Object_s *) anObject))
    setMappedAlloc (newObject);  
  return newObject;
}

//
// freeIVars: -- free object allocated by allocIVars: or copyIVars:
//
- (void)freeIVars: anObject
{
  id index;
  size_t size;

#ifdef HAVE_JDK
  if (swarmDirectory)
    swarm_directory_objc_remove (jniEnv, anObject);
#endif

  size = getClass (anObject)->instance_size;
  index = [population createIndex: getCZone (scratchZone)
                      fromMember: anObject];
  [index remove];
  [index drop];
  populationTotal -= size;


  if (_obj_debug)
    {
      if (getBit (((Object_s *) anObject)->zbits, BitComponentAlloc))
        raiseEvent (InvalidOperation,
                    "> object being freed by freeIVars: (%0#8x: %s)\n"
                    "> was allocated for restricted internal use by\n"
                    "> allocIVarsComponent: or copyIVarsComponent:,\n"
                    "> and may only be freed by freeIVarsComponent:\n",
                    anObject, getClass (anObject)->name);
      
      memset ((id *) anObject - 2, _obj_fillfree, size + (2 * sizeof (id)));
    }
  XFREE ((id *) anObject - 2);
}

//
// allocIVarsComponent: -- allocate an internal component object
//
- allocIVarsComponent: (Class)aClass
{
  Object_s *newObject;

  // allocate object of required size, including links in object header
  
  newObject = (Object_s *) dalloc (aClass->instance_size);

  if (_obj_debug)
    {
      objectCount++;
      objectTotal += aClass->instance_size;
    }
  
  // initialize and return the new object, without adding to population list

  memset (newObject, 0, aClass->instance_size);
  setClass (newObject, aClass);   
  newObject->zbits = (unsigned long) self;
  setBit (newObject->zbits, BitComponentAlloc, 1);
  return newObject;
}

//
// copyIVarsComponent: -- allocate component object with copied contents
//
- copyIVarsComponent: anObject
{
  Object_s  *newObject;

  // allocate object of required size, including links in object header

  newObject = (Object_s *) dalloc (getClass (anObject)->instance_size);

  if (_obj_debug)
    {
      objectCount++;
      objectTotal += getClass (anObject)->instance_size;
    }
  
  // initialize and return the new object, without adding to population list
  
  memcpy (newObject, anObject, getClass (anObject)->instance_size);
  newObject->zbits = (unsigned long) self;
  if (getMappedAlloc ((Object_s *) anObject))
    setMappedAlloc (newObject);  
  setBit (newObject->zbits, BitComponentAlloc, 1); 
  return newObject;
}

//
// freeIVarsComponent: --
//   free object allocated by allocIVarsComponent: or copyIVarsComponent:
//
- (void)freeIVarsComponent: anObject
{ 
#ifdef HAVE_JDK
  if (swarmDirectory)
    swarm_directory_objc_remove (jniEnv, anObject);
#endif

  if (_obj_debug)
    {
      if (!getBit (((Object_s *) anObject)->zbits, BitComponentAlloc))
        raiseEvent( InvalidOperation,
                    "> object being freed by freeIVarsComponent: (%0#8x: %s)\n"
                    "> was not allocated by allocIVarsComponent:\n"
                    "> or copyIVarsComponent:\n",
                    anObject, getClass (anObject)->name);
      
      objectCount--;
      objectTotal -= getClass (anObject)->instance_size;

      memset ((id *) anObject, _obj_fillfree,
              getClass (anObject)->instance_size);
    }
  XFREE (anObject);
}

//
// getComponentZone --
//   obtain version of zone qualified for allocation of object components
//
- getComponentZone
{
  return componentZone;
}

//
// alloc: -- alloc block of requested size, without initialization of contents
//
- (void *)alloc: (size_t)size
{
  void *ptr, *aptr, *newBlock;
  size_t headerSize = sizeof (ptrdiff_t) + sizeof (size_t);
  size_t offset;
  size_t extraAlloc;
  
  if (_obj_debug && size == 0)
    raiseEvent (InvalidAllocSize, nil);
  newBlock = xmalloc (size + headerSize);
  ptr = newBlock + headerSize;
  aptr = alignptrto (ptr, MAX_ALIGNMENT);
  extraAlloc = (size_t) (ptrdiff_t) (aptr - ptr);
  if (extraAlloc)
    {
      newBlock = xrealloc (newBlock, size + headerSize + extraAlloc);
      ptr = newBlock + headerSize;
      aptr = alignptrto (ptr, MAX_ALIGNMENT);
      extraAlloc = (size_t) (ptrdiff_t) (aptr - ptr);
      if (extraAlloc != 0)
        abort ();
    }
  offset = (ptrdiff_t) (aptr - newBlock);

  *(ptrdiff_t *) (aptr - sizeof (ptrdiff_t)) = offset;
  *(size_t *) (aptr - headerSize) = size;
  if (_obj_debug)
    {
      allocCount++;
      allocTotal += size;
      memset (aptr, _obj_fillalloc, size);
    }
  return aptr;
}

//
// free: -- free block allocated by alloc:
//
- (void)free: (void *)aBlock
{
  ptrdiff_t offset = *(ptrdiff_t *) (aBlock - sizeof (ptrdiff_t));

  if (_obj_debug)
    {
      size_t size = *(size_t *) (aBlock - sizeof (ptrdiff_t) - sizeof (size_t));
      allocTotal -= size;
      allocCount--;
    }
  XFREE (aBlock - offset);
}

//
// allocBlock: -- allocate block, with block size required on free
//
- (void *)allocBlock: (size_t)size
{
  void  *newBlock;
  
  if (_obj_debug && size == 0)
    raiseEvent (InvalidAllocSize, nil);
  newBlock = dalloc (size);
  if (_obj_debug)
    {
      blockCount++;
      blockTotal += size;
      memset (newBlock, _obj_fillalloc, size);
    }
  return newBlock;
}

//
// freeBlock: -- free block allocated by allocBlock: or copyBlock:
//
- (void)freeBlock: (void *)aBlock blockSize: (size_t)size
{
  if (_obj_debug)
    {
      blockCount--;
      blockTotal -= size;
      memset (aBlock, _obj_fillfree, size);
    }
  XFREE (aBlock);
}

//
// getPopulation --
//   return collection of objects explicitly allocated within the zone
//
- getPopulation
{
  return population;
}

//
// describe: -- generate object description string
//
- (void)describe: outputCharStream
{
  char buffer[200];

  [super describe: outputCharStream];
  sprintf (buffer, "> number of objects in population: %u\n"
           "> total size of objects in population: %lu\n",
           [population getCount],
           (unsigned long) populationTotal);
  [outputCharStream catC: buffer];
  
  if (_obj_debug)
    {
      sprintf (buffer,
               "> number of internal objects: %3lu  total size: %lu\n"
               "> number of internal blocks:  %3lu  total size: %lu\n"
               "> number of alloc blocks:  %5lu  total size: %lu\n",
               (unsigned long) objectCount, (unsigned long) objectTotal,
               (unsigned long) blockCount, (unsigned long) blockTotal,
               (unsigned long) allocCount, (unsigned long) allocTotal);
      [outputCharStream catC: buffer];
    }
}

//
// describeForEach: --
//   generate debug description for each member of the zone population
//
- (void)describeForEach: outputCharStream
{
  id index, member;

  index = [population begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    [member describe: outputCharStream];
  [index drop];
}

//
// describeForEachID: --
//   generate debug id description for each member of the zone population
//
- (void)describeForEachID: outputCharStream
{
  id index, member;

  index = [population begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    [member describeID: outputCharStream];
  [index drop];
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
  id  index, member;

  // map all objects within the zone population, including internal block
  // allocations

  mapalloc->zone = self;
  mapalloc->descriptor = t_PopulationObject;

  index = [population begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    {
      [index prev];
      mapAlloc (mapalloc, member);
    }
   [index drop];

  // map components of the zone itself
  mapObject (mapalloc, componentZone);
  mapObject (mapalloc, population);
}

@end

//
// ComponentZone_c --
//   qualified view of a zone to create objects as internal components that
//   are not contained in the zone population
//   

@implementation ComponentZone_c

- allocIVars: (Class)aClass
{
  return [baseZone allocIVarsComponent: aClass];
}

- copyIVars: anObject
{
  return [baseZone copyIVarsComponent: anObject];
}

- getComponentZone
{
  return componentZone;
}

@end
