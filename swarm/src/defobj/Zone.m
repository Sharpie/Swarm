// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
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
#import <collections.h>

#include <stdlib.h>
#include <memory.h>

//
// temporary hack to guarantee double word alignment of allocations
//
static inline void *dalloc( size_t blockSize )
{
  static BOOL  notAligned;
  void         *block;

  block = malloc( blockSize );
  if ( ! block ) raiseEvent( OutOfMemory, nil );
  if ( ( (unsigned long)block & ~0x7 ) == (unsigned long)block ) return block;
  if ( ! notAligned ) {
    notAligned = 1;
    fprintf( stderr,
      "Double word alignment of malloc allocations not guaranteed on local\n"
      "machine architecture.  Please report to swarm@santafe.edu.\n"
      "Standard fixup taken, execution continuing...\n" );
  }
  free( block );
  block = malloc( blockSize + 7 );
  return ( (void *)( (unsigned long)block & ~0x7 ) );
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
  Zone_c  *newZone;

  // the very first zone is created externally by explicit initialization

  newZone = [aZone allocIVars: self];
  return newZone;
}

- (void) setPageSize: (int)pageSize
{
  raiseEvent( NotImplemented,
    "> PageSize option not yet implemented.\n"
    "> Page size must be a power of two, at least 256, and no greater than\n"
    "> the page size of the zone owner.  Page size requested was: %d.\n",
    pageSize );
}

- createEnd
{
  if ( createByMessageToCopy( self, createEnd ) ) return self;

  setMappedAlloc( self );
  setNextPhase( self );

  // create internal support objects for zone

  componentZone = [self allocIVarsComponent: id_ComponentZone_c];
  ((ComponentZone_c *)componentZone)->baseZone = self;
  ((ComponentZone_c *)componentZone)->componentZone = componentZone;

  population = [List createBegin: componentZone];
  [population setIndexFromMemberLoc: - (2 * sizeof(id))];
  population = [(id)population createEnd];

  // zero internal allocation statistics to remove zone overhead

  if ( _obj_debug ) {
    objectCount = 0;
    objectTotal = 0;
  }
  return self;
}

PHASE(Using)

- (int) getPageSize
{
  return 0;
}

//
// allocIVars: -- allocate and initialize instance variables for object
//
- allocIVars: aClass
{
  Object_s  *newObject;

  //!! need to guarantee that class inherits from Object_s, to define slot
  //!! for zbits

  // allocate object of required size, including links in object header

  newObject = (Object_s *)dalloc(
                ((Class)aClass)->instance_size + 2 * sizeof (id) );

  // add object to the population list, skipping over links in object header

  newObject = (Object_s *)((id *)newObject + 2);
  [population addLast: newObject];

  // initialize and return the newly allocated object

  memset( newObject, 0, ((Class)aClass)->instance_size );
  setClass( newObject, aClass );   
  newObject->zbits = (unsigned long)self;   
  return (id)newObject;
}

//
// copyIVars: -- allocate object with contents copied from existing object
//
- copyIVars: anObject
{
  Object_s  *newObject;
  int       instanceSize;

  // allocate object of required size, including links in object header

  instanceSize = getClass(anObject)->instance_size;
  newObject = (Object_s *)dalloc( instanceSize + 2 * sizeof(id) );

  // add object to the population list, skipping over links in object header

  newObject = (Object_s *)((id *)newObject + 2);
  [population addLast: newObject];

  // initialize and return the newly allocated object

  memcpy( newObject, anObject, instanceSize );
  newObject->zbits = (unsigned long)self;
  if ( getMappedAlloc( (Object_s *)anObject ) ) setMappedAlloc( newObject );  
  return newObject;
}

//
// freeIVars: -- free object allocated by allocIVars: or copyIVars:
//
- (void) freeIVars: anObject
{
  id   index;

  index = [population createIndex: scratchZone fromMember: anObject];
  [index remove];
  [index drop];

  if ( _obj_debug ) {
     if ( getBit( ((Object_s *)anObject)->zbits, BitComponentAlloc ) )
       raiseEvent( InvalidOperation,
 "> object being freed by freeIVars: (%0#8x: %s)\n"
 "> was allocated for restricted internal use by allocIVarsComponent: or\n"
 "> copyIVarsComponent:, and may only be freed by freeIVarsComponent:\n",
       anObject, getClass( anObject )->name );

     memset( (id *)anObject - 2, _obj_fillfree,
             getClass(anObject)->instance_size + ( 2 * sizeof (id) ) );
  }
  free( (id *)anObject - 2 );
}

//
// allocIVarsComponent: -- allocate an internal component object
//
- allocIVarsComponent: aClass
{
  Object_s  *newObject;

  // allocate object of required size, including links in object header

  newObject = (Object_s *)dalloc( ((Class)aClass)->instance_size );

  if ( _obj_debug ) {
    objectCount++;
    objectTotal += ((Class)aClass)->instance_size;
  }

  // initialize and return the new object, without adding to population list

  memset( newObject, 0, ((Class)aClass)->instance_size );
  setClass( newObject, aClass );   
  newObject->zbits = (unsigned long)self;
  setBit( newObject->zbits, BitComponentAlloc, 1 ); 
  return newObject;
}

//
// copyIVarsComponent: -- allocate component object with copied contents
//
- copyIVarsComponent: anObject
{
  Object_s  *newObject;

  // allocate object of required size, including links in object header

  newObject = (Object_s *)dalloc( getClass(anObject)->instance_size );

  if ( _obj_debug ) {
    objectCount++;
    objectTotal += getClass(anObject)->instance_size;
  }

  // initialize and return the new object, without adding to population list

  memcpy( newObject, anObject, getClass(anObject)->instance_size );
  newObject->zbits = (unsigned long)self;
  if ( getMappedAlloc( (Object_s *)anObject ) ) setMappedAlloc( newObject );  
  setBit( newObject->zbits, BitComponentAlloc, 1 ); 
  return newObject;
}

//
// freeIVarsComponent: --
//   free object allocated by allocIVarsComponent: or copyIVarsComponent:
//
- (void) freeIVarsComponent: anObject
{
  if ( _obj_debug ) {
    if ( ! getBit( ((Object_s *)anObject)->zbits, BitComponentAlloc ) )
      raiseEvent( InvalidOperation,
        "> object being freed by freeIVarsComponent: (%0#8x: %s)\n"
        "> was not allocated by allocIVarsComponent: or copyIVarsComponent:\n",
        anObject, getClass( anObject )->name );

    objectCount--;
    objectTotal -= getClass(anObject)->instance_size;

    memset( (id *)anObject, _obj_fillfree, getClass(anObject)->instance_size );
  }
  free( anObject );
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
- (void *) alloc: (size_t)size
{
  void  *newBlock;

  if ( _obj_debug && size == 0 ) raiseEvent( InvalidAllocSize, nil );
  newBlock = dalloc( size );
  if ( _obj_debug ) {
    allocCount++;
    memset( newBlock, _obj_fillalloc, size );
  }
  return newBlock;
}

//
// free: -- free block allocated by alloc:
//
- (void) free: (void *) aBlock
{
  if ( _obj_debug ) allocCount--;
  free(aBlock);
}

//
// allocBlock: -- allocate block, with block size required on free
//
- (void *) allocBlock: (size_t)size
{
  void  *newBlock;

  if ( _obj_debug && size == 0 ) raiseEvent( InvalidAllocSize, nil );
  newBlock = dalloc( size );
  if ( _obj_debug ) {
    blockCount++;
    blockTotal += size;
    memset( newBlock, _obj_fillalloc, size );
  }
  return newBlock;
}

//
// freeBlock: -- free block allocated by allocBlock: or copyBlock:
//
- (void) freeBlock: (void *)aBlock blockSize: (size_t)size
{
  if ( _obj_debug ) {
    blockCount--;
    blockTotal -= size;
    memset( aBlock, _obj_fillfree, size );
  }
  free(aBlock);
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
- (void) describe: outputCharStream
{
  char  buffer[200];

  [super describe: outputCharStream];
  sprintf( buffer, "> number of objects in population: %d\n",
           [population getCount] );
  [outputCharStream catC: buffer];

  if ( _obj_debug ) {
    sprintf( buffer,
      "> number of internal objects: %3d  total size: %d\n"
      "> number of internal blocks:  %3d  total size: %d\n"
      "> number of alloc: blocks:  %5d  (total size not available)\n",
             objectCount, objectTotal, blockCount, blockTotal, allocCount );
    [outputCharStream catC: buffer];
  }
}

//
// xfprint -- execute xprint on each member of the zone population
//
- (void) xfprint
{
  id  index, member;

  index = [population begin: scratchZone];
  while ( (member = [index next]) ) [member xprint];
  [index drop];
}

//
// xfprintid -- execute xprintid on each member of the zone population
//
- (void) xfprintid
{
  id  index, member;

  index = [population begin: scratchZone];
  while ( (member = [index next]) ) [member xprintid];
  [index drop];
}

//
// mapAllocations: -- standard method to identify internal allocations
//
- (void) mapAllocations: (mapalloc_t)mapalloc
{
  id  index, member;

  // map all objects within the zone population, including internal block
  // allocations

  mapalloc->zone       = self;
  mapalloc->descriptor = t_PopulationObject;

  index = [population begin: scratchZone];
  while ( (member = [index next]) ) {
    [index prev];
    mapAlloc( mapalloc, member );
  }

  // map components of the zone itself

  mapObject( mapalloc, componentZone );
  mapObject( mapalloc, population );
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
  return [baseZone allocIVarsComponent: (Class)aClass];
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
