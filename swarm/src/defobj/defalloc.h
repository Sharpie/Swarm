// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         defalloc.h
Description:  additional interfaces for block-level memory allocation
Library:      defobj
*/

//
// getZone() --
//   macro to obtain zone in which object allocated
//
#define getZone( anObject ) \
({ unsigned long _zbits_ = (anObject)->zbits; \
  ( _zbits_ & BitSuballocList ? \
   (id)((Object_s *)( _zbits_ & ~0x7 ))->zbits : \
   (id)( _zbits_ & ~0x7 ) ); })

//
// getCZone() --
//   macro to obtain version of zone qualified for allocation of object
//   components
//
#define getCZone( aZone ) \
( _obj_debug ? [(aZone) getComponentZone] : ((id *)(aZone))[2] )

//
// setMappedAlloc(), unsetMappedAlloc(), getMappedAlloc() --
//   macros to set/get bit that indicates that mapping of internal allocations
//   is required within an object
//
#define setMappedAlloc( anObject )  ((anObject)->zbits |= BitMappedAlloc)
#define unsetMappedAlloc(anObject)  ((anObject)->zbits &= ~BitMappedAlloc)
#define getMappedAlloc( anObject )  ((anObject)->zbits & BitMappedAlloc)

//
// bits within the zbits instance variable of any zone-allocated object
//
#define BitMappedAlloc     0x4  // set by suballoc list or explicit macro 
#define BitSuballocList    0x2  // set whenever object contains suballoc list
#define BitComponentAlloc  0x1  // set if object is not in the zone population

//
// struct mapalloc, mapalloc_t --
//   structure that maps an internal allocation within an object
/*
Definition of these types appears in the DefObject.h superclass, since they
are required to declare any mapAllocations: method.  All use of these types,
however, is by macros that require an explicit #import of <defobj/defalloc.h>.
Following is a commented-out copy of the definitions from DefObject.h:

typedef struct mapalloc *mapalloc_t;
struct mapalloc {
  void  (*mappingFunction)( mapalloc_t mapalloc, BOOL objectAllocation );
  void  *alloc;         // allocated object or block
  id    descriptor;     // descriptor for contents of allocated block, if any
  id    zone;           // zone of allocated block, as used by descriptor
  int   size;           // size of allocated block, as used by descriptor
};
*/

//
// MapAllocations --
//   protocol defining message to be implemented for any object for which
//   the MappedAlloc bit has been set
//
@protocol MapAllocations
- (void)     mapAllocations: (mapalloc_t)mapalloc;
@end

//
// mapObject() --
//   macro to identify an internal object within the allocations mapped
//   by a containing object
//
// This macro is for use inside a mapAllocations: method, which must be
// implemented on any method for which the MappedAlloc bit has been set.
// Each object identified by the macro must have been allocated as an
// internal component of the object being mapped.
//
#define mapObject( mapalloc, anObject ) \
( mapalloc->alloc = (anObject), mapalloc->mappingFunction( mapalloc, 1 ) )

//
// includeBlocks() --
//   macro to indicate whether internal blocks to be mapped within a
//   mapAllocations: request to map internal allocations
//
#define includeBlocks( mapalloc ) ( mapalloc->zone != nil )

//
// mapAlloc() --
//   macro to identify an internal allocation of a containing object
//
// This macro checks the current setting of the descriptor variable in the
// mapalloc structure to determine how to map the size and contents of the
// storage block identified.  The default setting of the descriptor is
// t_ByteArray, which checks the size field of the mapalloc structure to
// determine the block size.
//
// Raw storage blocks should be identified as part of an object allocation
// mapping only if the includeBlocks() macro returns true on the mapalloc
// structure of a current mapAllocations: request.
//
#define mapAlloc( mapalloc, aBlock ) \
( mapalloc->alloc = aBlock, mapalloc->mappingFunction( mapalloc, 0 ) )

//
// getSuballocList() --
//   macro to obtain the list of suballocations within an object, if any
//
#define getSuballocList( anObject ) \
({ unsigned long _zbits_ = (anObject)->zbits; \
 ( _zbits_ & BitSuballocList ? (id)( _zbits_ & ~0x7 ) : nil ); })

//
// struct suballocEntry, suballocEntry_t --
//   pointer to structure contained as member in the list of suballocations
//   for an object
//
typedef struct suballocEntry {
  notify_t  notifyFunction;  // function to notify on change of alloc, if any
  void      *argument;       // argument supplied in the addRef:withArg: call
                             //   that registered the allocation
  id        links[2];        // links that include entry into suballoc list
} *suballocEntry_t;

//
// struct suballocHeader, suballocHeader_t --
//   pointer to header of structure containing an object suballocation
//   (required as argument of addRef:withArg: when notifyFunction is nil)
//
typedef struct suballocHeader {
  size_t  suballocSize;  // total number of bytes in suballoc including header
  int     suballocKey;   // key for sorting suballocations within suballocList
    // remaining bytes of suballocation immediately follow the header ...
} *suballocHeader_t;

//
// _obj_fillalloc, _obj_fillfree -- 
//   fill patterns for allocated and freed blocks, for debugging
//
extern unsigned char  _obj_fillalloc, _obj_fillfree;
