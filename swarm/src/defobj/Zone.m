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
Name:         Zone.m
Description:  superclass support for all zone implementations
Library:      defobj
*/

#import <defobj/Zone.h>
#import <defobj/defalloc.h>

#import <collections/List_linked.h>
#import <swarmconfig.h>
#import <defobj/directory.h>

#import <defobj/macros.h>
#import <collections/macros.h>

#include <misc.h> // memset, xmalloc, XFREE, MAX_ALIGNMENT
#include "internal.h"


extern void *GC_malloc_uncollectable (size_t);
extern void *GC_realloc (void *buf, size_t size);
extern void GC_free (void *buf);

//
// hack to guarantee double word alignment of allocations
//
static inline void *
dalloc (size_t blockSize, BOOL GCFixedRootFlag)
{
  static BOOL notAligned = NO;
  void *block;

  block = (GCFixedRootFlag 
           ? GC_malloc_uncollectable (blockSize)
           : xmalloc (blockSize));

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
 
  if (GCFixedRootFlag) 
    GC_free (block);
  else
    XFREE (block);

  {
    size_t newSize = blockSize + 7;

    block = (GCFixedRootFlag
             ? GC_malloc_uncollectable (newSize)
             : xmalloc (newSize));
    return ((void *) (((unsigned long) newSize) & ~0x7));
  }
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
  newZone->GCFixedRootFlag = NO;
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

- (void)setGCFixedRootFlag: (BOOL)theGCFixedRootFlag
{
  GCFixedRootFlag = theGCFixedRootFlag;
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

  population = [List_any createBegin: componentZone];
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
#if SWARM_OBJC_DONE
  size_t size = aClass->instance_size;
#else
  size_t size = swarm_class_getInstanceSize(aClass);
#endif

  //!! need to guarantee that class inherits from Object_s, to define slot
  //!! for zbits

  // allocate object of required size, including links in object header

  newObject = (Object_s *) dalloc (size + 2 * sizeof (id), GCFixedRootFlag);

  // clears mlinks of population entry
  memset (newObject, 0, size + 2 * sizeof (id));
  
  // add object to the population list, skipping over links in object header

  newObject = (Object_s *) ((id *) newObject + 2);

  if (population)
    {
      MLIST_ADDLAST (population, newObject);
      populationTotal += size;
    }

  // initialize and return the newly allocated object

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

#if SWARM_OBJC_DONE
  instanceSize = getClass (anObject)->instance_size;
#else
  instanceSize = swarm_class_getInstanceSize(swarm_object_getClass(anObject));
#endif
  newObject = (Object_s *) dalloc (instanceSize + 2 * sizeof (id), GCFixedRootFlag);

  // clears mlinks of population entry
  memset (newObject, 0, instanceSize + 2 * sizeof (id));

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

  swarm_directory_objc_remove (anObject);

#if SWARM_OBJC_DONE
  size = getClass (anObject)->instance_size;
#else
  size = swarm_class_getInstanceSize(swarm_object_getClass(anObject));
#endif
  index = MLIST_CREATEINDEX_FROMMEMBER (population, getCZone (scratchZone), anObject);
  MLIST_INDEX_REMOVE (index);
  DROP (index);
  populationTotal -= size;


  if (_obj_debug)
    {
      if (getBit (((Object_s *) anObject)->zbits, BitComponentAlloc))
        raiseEvent (InvalidOperation,
                    "> object being freed by freeIVars: (%0#8x: %s)\n"
                    "> was allocated for restricted internal use by\n"
                    "> allocIVarsComponent: or copyIVarsComponent:,\n"
                    "> and may only be freed by freeIVarsComponent:\n",
#if SWARM_OBJC_DONE
                    anObject, getClass (anObject)->name);
#else
                    anObject, swarm_class_getName(swarm_object_getClass(anObject)));
#endif
      
      memset ((id *) anObject - 2, _obj_fillfree, size + (2 * sizeof (id)));
    }
  if (GCFixedRootFlag)
    GC_free ((id *) anObject - 2);
  else
    XFREE ((id *) anObject - 2);
}

//
// allocIVarsComponent: -- allocate an internal component object
//
- allocIVarsComponent: (Class)aClass
{
  Object_s *newObject;

  // allocate object of required size, including links in object header

#if SWARM_OBJC_DONE
  newObject = (Object_s *) dalloc (aClass->instance_size, GCFixedRootFlag);
#else  
  newObject = (Object_s *) dalloc (swarm_class_getInstanceSize(aClass), GCFixedRootFlag);
#endif

  if (_obj_debug)
    {
      objectCount++;
#if SWARM_OBJC_DONE
      objectTotal += aClass->instance_size;
#else
      objectTotal += swarm_class_getInstanceSize(aClass);
#endif
    }
  
  // initialize and return the new object, without adding to population list

#if SWARM_OBJC_DONE
  memset (newObject, 0, aClass->instance_size);
#else
  memset (newObject, 0, swarm_class_getInstanceSize(aClass));
#endif
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

#if SWARM_OBJC_DONE
  newObject = (Object_s *) dalloc (getClass (anObject)->instance_size, GCFixedRootFlag);
#else
  newObject = (Object_s *) dalloc (swarm_class_getInstanceSize(swarm_object_getClass(anObject)),
				   GCFixedRootFlag);
#endif

  if (_obj_debug)
    {
      objectCount++;
#if SWARM_OBJC_DONE
      objectTotal += getClass (anObject)->instance_size;
#else
      objectTotal += swarm_class_getInstanceSize(swarm_object_getClass(anObject));
#endif
    }
  
  // initialize and return the new object, without adding to population list
  
#if SWARM_OBJC_DONE
  memcpy (newObject, anObject, getClass (anObject)->instance_size);
#else
  memcpy (newObject, anObject, swarm_class_getInstanceSize(swarm_object_getClass(anObject)));
#endif
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
  swarm_directory_objc_remove (anObject);

  if (_obj_debug)
    {
      if (!getBit (((Object_s *) anObject)->zbits, BitComponentAlloc))
        raiseEvent( InvalidOperation,
                    "> object being freed by freeIVarsComponent: (%0#8x: %s)\n"
                    "> was not allocated by allocIVarsComponent:\n"
                    "> or copyIVarsComponent:\n",
                    anObject, swarm_class_getName(swarm_object_getClass(anObject)));
      
      objectCount--;
#if SWARM_OBJC_DONE
      objectTotal -= getClass (anObject)->instance_size;

      memset ((id *) anObject, _obj_fillfree,
              getClass (anObject)->instance_size);
#else
      objectTotal -= swarm_class_getInstanceSize(swarm_object_getClass(anObject));

      memset ((id *) anObject, _obj_fillfree,
              swarm_class_getInstanceSize(swarm_object_getClass(anObject)));
#endif
    }
  if (GCFixedRootFlag)
    GC_free (anObject);
  else
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
  size_t adjSize = size + headerSize;
  
  if (GCFixedRootFlag)
    abort ();
  if (_obj_debug && size == 0)
    raiseEvent (InvalidAllocSize, nil);
  newBlock = xmalloc (adjSize);
  ptr = newBlock + headerSize;
  aptr = alignptrto (ptr, MAX_ALIGNMENT);
  extraAlloc = (size_t) (ptrdiff_t) (aptr - ptr);
  if (extraAlloc)
    {
      size_t newSize = size + headerSize + extraAlloc;

      newBlock = xrealloc (newBlock, newSize);
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
  if (aBlock)
    {
      ptrdiff_t offset = *(ptrdiff_t *) (aBlock - sizeof (ptrdiff_t));

      if (GCFixedRootFlag)
        abort ();

      if (_obj_debug)
        {
          size_t size = *(size_t *) (aBlock - sizeof (ptrdiff_t) - sizeof (size_t));
          allocTotal -= size;
          allocCount--;
        }
      XFREE (aBlock - offset);
    }
  else
    raiseEvent(WarningMessage, "Trying to free nil");
}

//
// allocBlock: -- allocate block, with block size required on free
//
- (void *)allocBlock: (size_t)size
{
  void  *newBlock;
  
  if (_obj_debug && size == 0)
    raiseEvent (InvalidAllocSize, nil);
  newBlock = dalloc (size, GCFixedRootFlag);
  if (_obj_debug)
    {
      blockCount++;
      blockTotal += size;
      if (!GCFixedRootFlag)
	memset (newBlock, _obj_fillalloc, size);
    }
  return newBlock;
}

//
// freeBlock: -- free block allocated by allocBlock:
//
- (void)freeBlock: (void *)aBlock blockSize: (size_t)size
{
  if (_obj_debug)
    {
      blockCount--;
      blockTotal -= size;
      memset (aBlock, _obj_fillfree, size);
    }
  if (GCFixedRootFlag)
    {
      extern void GC_free (void *);

      GC_free (aBlock);
    }
  else
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
  DROP (index);
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
  DROP (index);
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
  DROP (index);

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
  return ALLOCIVARSCOMPONENT (baseZone, aClass);
}

- copyIVars: anObject
{
  return [baseZone copyIVarsComponent: anObject];
}

- getComponentZone
{
  // SWARM_OBJC_TODO - why not just return ourself?
  return self;

  //abort ();
  //return componentZone;
}

@end
