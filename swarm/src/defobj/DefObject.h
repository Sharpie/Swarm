// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         DefObject.h
Description:  top-level superclass to provide standard services
Library:      defobj
*/

#define DEFINE_CLASSES
#import <defobj.h>

#ifdef INHERIT_OBJECT
@interface Object_s : Object
#else
@interface Object_s
{
  Class  isa;  // implementing class for object
}
#endif
/*** methods implemented in .m file ***/
+ (char *) getName;
+ getClass;
- getClass;
+ (BOOL) respondsTo: (SEL)aSel;
- (BOOL) respondsTo: (SEL)aSel;
+ getSuperclass;
+ (BOOL) isSubclass: aClass;
+ (void) setTypeImplemented: aType;
+ getTypeImplemented;
+ getMethods;
+ getIVarDefs;
+ getAllMethods;
+ getAllIVarDefs;
+ (IMP) getMethodFor: (SEL)aSel;
+ self;
- perform: (SEL)aSel;
- perform: (SEL)aSel with: anObject1;
- perform: (SEL)aSel with: anObject1 with: anObject2;
- perform: (SEL)aSel with: anObject1 with: anObject2 with: anObject3;
- createIDString: aZone;
- describe: anOutputStream;
@end

//
// getClass -- macro to get class of instance
//
#define getClass(anObject) (*(Class *)anObject)

//
// setClass -- macro to set behavior of instance to compatible class
//
#define setClass(anObject, behaviorClass) \
  (*(Class *)anObject = (Class)behaviorClass)

//
// callMethodInClass -- macro for lookup in an alternate superclass
//
#define callMethodInClass(aClass, aMessage, args...) \
((*(get_imp([aClass self], @selector(aMessage)))) \
  (self, @selector(aMessage), ## args))

//
// respondsTo -- return true if object responds to message  
//
extern BOOL respondsTo( id anObject, SEL aSel );

//
// methodFor -- lookup method for message on object
//
extern IMP methodFor( id anObject, SEL aSel );

//
// bit field macros for use in subclasses
//

#define getBit( word, bit ) \
  ((word) & bit)
#define setBit( word, bit, value ) \
  ( (value) ? ((word) |= bit) : ((word) &= ~bit) )

#define setField( word, shift, value ) \
(word) |= ((value) << shift)

#define getField( word, shift, mask ) \
((unsigned)((word) & mask) >> shift)

//
// AllocSize -- class and inline funcs for fast allocation of same-size blocks
//
@interface AllocSize : Object_s
{
@public
  id <Zone>  zone;
  int        allocSize;
  void       **freeList;
}
/*** methods implemented in .m file ***/
- getZone;
- (size_t) getAllocSize;
@end

extern inline void *allocSize( id anAllocSize )
{
  void *newAlloc = ((AllocSize *)anAllocSize)->freeList;
  if ( newAlloc ) {
    ((AllocSize *)anAllocSize)->freeList = *(void **)newAlloc;
    return newAlloc;
  }
  return [((AllocSize *)anAllocSize)->zone
            allocBlock: ((AllocSize *)anAllocSize)->allocSize];
}

extern inline void freeSize( id anAllocSize, void *aBlock )
{
  *(void **)aBlock = ((AllocSize *)anAllocSize)->freeList;
  ((AllocSize *)anAllocSize)->freeList = aBlock;
}
