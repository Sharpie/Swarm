// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Zone.m
Description:  initial malloc pass-through implementation of Zone
Library:      defobj
*/

#import <defobj/Zone.h>

#include <stdlib.h>
#include <memory.h>

unsigned char _obj_fillalloc = 0xaa, _obj_fillfree = 0xff;

static void *phonyFreeList;  // phony free list for incomplete AllocSize


@implementation Zone_c

PHASE(Creating)

+ createBegin: aZone
{
  Zone_c  *newZone;

  if ( aZone ) {
    newZone = [aZone allocIVars: self];
  } else {
    newZone =
      (Zone_c *)calloc( 1, ((Class)[Zone_c self])->instance_size );
    setClass( newZone, self );
  }
  newZone->ownerZone = aZone;
  return newZone;
}

- (void) setObjectCollection: (BOOL)objectCollection
{
  objects = [List createBegin: globalZone];
  [(id)objects setIndexFromMemberLoc: - (2 * sizeof(id))];
  objects = [(id)objects createEnd];
}

- (void) setPageSize: (int)pageSize
{
  raiseEvent( InvalidArgument,
    "Page size must be a power of two, at least 256, and no greater than\n"
    "the page size of the zone owner.  Page size requested was: %d.\n",
    pageSize );
}

- createEnd
{
  createByCopy( );
  setNextPhase( self );
  return self;
}

PHASE(Using)

- (void)drop
{
  //!! override as necessary, later...
}

- (BOOL) getObjectCollection
{
  return objects != nil;
}

- (int) getPageSize
{
  return 0;
}

- getSubzones
{
  return nil;
}

- (void) mergeWithOwner {}

- allocIVars: (Class)aClass
{
  id  newObject;

  if ( objects ) {
    newObject = (id)malloc( aClass->instance_size + (2 * sizeof (id)) );
    if ( ! newObject ) raiseEvent( OutOfMemory, nil );
    newObject = (id)((id *)newObject + 2);
    [objects addLast: newObject];
  } else {
    newObject = (id)malloc( aClass->instance_size );
    if ( ! newObject ) raiseEvent( OutOfMemory, nil );
  }
  memset( newObject, 0, aClass->instance_size );
  *(id *)newObject = aClass;   
  return newObject;
}

- copyIVars: anObject
{
  id   newObject;
  int  instanceSize;

  instanceSize = getClass(anObject)->instance_size;
  if ( objects ) {
    newObject = (id)malloc( instanceSize + 2*sizeof(id) );
    if ( ! newObject ) raiseEvent( OutOfMemory, nil );
    newObject = (id)((id *)newObject + 2);
    [objects addLast: newObject];
  } else {
    newObject = (id)malloc( instanceSize );
    if ( ! newObject ) raiseEvent( OutOfMemory, nil );
  }
  memcpy( newObject, anObject, instanceSize );
  return newObject;
}

- (void) freeIVars: anObject
{
  id   index;

  if ( objects ) {
    index = [objects createIndex: scratchZone fromMember: anObject];
    [index remove];
    [index drop];
    if ( _obj_debug ) memset( (id *)anObject - 2, _obj_fillfree,
                        getClass(anObject)->instance_size + (2 * sizeof(id)) );
    free( (id *)anObject - 2 );
  } else {
    if ( _obj_debug ) memset( (id *)anObject, _obj_fillfree,
                              getClass(anObject)->instance_size );
    free( anObject );
  }
}

// object allocation functions with deprecated names

- allocObject: (Class)aClass
{
  return [self allocIVars: aClass];
}
- (void) freeObject: anObject
{
  [self freeIVars: anObject];
}

//
// getObjects -- return collection of objects allocated within zone
//
- getObjects
{
  return objects;
}

//
// alloc: -- alloc block of requested size, without guaranteed initialization
//
- (void *) alloc: (size_t)size
{
  void  *newBlock;

  if ( _obj_debug && size == 0 ) raiseEvent( InvalidAllocSize, nil );
  newBlock = malloc( size );
  if ( ! newBlock ) raiseEvent( OutOfMemory, nil );
  if ( _obj_debug ) memset( newBlock, _obj_fillalloc, size );
  return newBlock;
}

// interface like free(): free bytes

- (void) free: (void *) aBlock
{
  free(aBlock);
}

//
// allocBlock: -- allocate block, with size also required on free
//
- (void *) allocBlock: (size_t)size
{
  void  *newBlock;

  if ( _obj_debug && size == 0 ) raiseEvent( InvalidAllocSize, nil );
  newBlock = malloc( size );
  if ( ! newBlock ) raiseEvent( OutOfMemory, nil );
  if ( _obj_debug ) memset( newBlock, _obj_fillalloc, size );
  return newBlock;
}

// copyBlock:blockSize: -- allocate new block and initialize to old

- (void *) copyBlock: (void *)aBlock blockSize: (size_t)size
{
  void  *newAlloc;

  if ( _obj_debug && size == 0 ) raiseEvent( InvalidAllocSize, nil );
  newAlloc = malloc( size );
  memcpy( newAlloc, aBlock, size );
  return newAlloc;
}

// interface like free(): free bytes, only require size of block

- (void) freeBlock: (void *) aBlock blockSize: (size_t)size
{
  if ( _obj_debug ) memset( aBlock, _obj_fillfree, size );
  free(aBlock);
}

- createAllocSize: (size_t)allocSize
{
  AllocSize *newAllocSize;

  if ( _obj_debug && allocSize == 0 ) raiseEvent( InvalidAllocSize, nil );
  newAllocSize = [self allocIVars: id_AllocSize];
  newAllocSize->zone      = (id)self;
  newAllocSize->allocSize = allocSize;
  newAllocSize->freeList  = &phonyFreeList;
  return newAllocSize;
}

- (BOOL) isSubzoneAlloc: (void *)pointer
{
  return 0;
}

- getAllocSubzone: (void *)pointer
{
  return nil;
}

@end


@implementation AllocSize

- getZone
{
  return zone;
}

- (size_t) getAllocSize
{
  return allocSize;
}

@end

void *allocSize( id anAllocSize )
{
  void *newAlloc = ((AllocSize *)anAllocSize)->freeList;
  if ( newAlloc ) {
    ((AllocSize *)anAllocSize)->freeList = *(void **)newAlloc;
    return newAlloc;
  }
  return [((AllocSize *)anAllocSize)->zone
            allocBlock: ((AllocSize *)anAllocSize)->allocSize];
}

void freeSize( id anAllocSize, void *aBlock )
{
  *(void **)aBlock = ((AllocSize *)anAllocSize)->freeList;
  ((AllocSize *)anAllocSize)->freeList = aBlock;
}
