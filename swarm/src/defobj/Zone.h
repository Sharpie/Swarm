// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Zone.h
Description:  superclass support for all zone implementations  
Library:      defobj
*/

#import <defobj/Create.h>
#import <defobj/defalloc.h>

@interface Zone_c : CreateDrop_s
{
  id componentZone;          // view of zone qualified for component allocation
  id population;             // external objects created within the zone
  size_t populationTotal;    // total size of population objects
  unsigned objectCount;      // count of internal objects allocated in the zone
  size_t objectTotal;        // total size of internal objects
  unsigned blockCount;       // count of internal blocks allocated in the zone
  size_t blockTotal;         // total size of internal blocks
  unsigned allocCount;       // count of blocks allocated by alloc:
  size_t allocTotal;         // total size of alloc'ed blocks
  id internalAllocations;    // collection of all internal allocations
}
/*** methods in Zone_c (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- (void)setPageSize: (size_t)pageSize;
- createEnd;
- (unsigned)getPageSize;
- allocIVars: aClass;
- copyIVars: anObject;
- (void)freeIVars: anObject;
- allocIVarsComponent: aClass;
- copyIVarsComponent: anObject;
- (void)freeIVarsComponent: anObject;
- getComponentZone;
- (void *)alloc: (size_t)size;
- (void)free: (void *) aBlock;
- (void *)allocBlock: (size_t)size;
- (void)freeBlock: (void *)aBlock blockSize: (size_t)size;
- getPopulation;
- (void)describe: outputCharStream;
- (void)describeForEach: outputCharStream;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end

@interface ComponentZone_c : CreateDrop_s
{
@public
  id baseZone;       // zone from which component objects to be allocated
  id componentZone;  // reference to self for support of getComponentZone
}
/*** methods in ComponentZone_c (inserted from .m file by m2h) ***/
- allocIVars: (Class)aClass;
- copyIVars: anObject;
- getComponentZone;
@end
