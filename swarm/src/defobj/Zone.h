// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Zone.h
Description:  initial malloc pass-through implementation of Zone   
Library:      defobj
*/

#import <defobj/Create.h>
#import <collections.h>

@interface Zone_c : CreateDrop_s
{
@public
  id <Zone>  ownerZone;  // zone which supplies pages for local zone
  id         objects;    // collection of allocated objects
}
/*** methods implemented in .m file ***/
+ createBegin: aZone;
- (void) setObjectCollection: (BOOL)objectCollection;
- (void) setPageSize: (int)pageSize;
- createEnd;
- (void)drop;
- (BOOL) getObjectCollection;
- (int) getPageSize;
- getSubzones;
- (void) mergeWithOwner;
- allocIVars: (Class)aClass;
- copyIVars: anObject;
- (void) freeIVars: anObject;
- allocObject: (Class)aClass;
- getObjects;
- (void) freeObject: anObject;
- (void *) alloc: (size_t)size;
- (void) free: (void *) aBlock;
- (void *) allocBlock: (size_t)size;
- (void *) copyBlock: (void *)aBlock blockSize: (size_t)size;
- (void) freeBlock: (void *) aBlock blockSize: (size_t)size;
- createAllocSize: (size_t)allocSize;
- (BOOL) isSubzoneAlloc: (void *)pointer;
- getAllocSubzone: (void *)pointer;
@end
