// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
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
{
@public
  unsigned  zbits;  // word that contains zone in which object allocated, plus
                    // additional bits about the memory allocations for the
                    // object
}
#else
@interface Object_s
{
@public
  Class     isa;    // class that implements the behavior of an object
  unsigned  zbits;  // word that contains zone in which object allocated, plus
                    // additional bits about the object
}
#endif
/*** methods in Object_s (inserted from .m file) ***/
+ (char *) getName;
+ (BOOL) respondsTo: (SEL)aSel;
- (BOOL) respondsTo: (SEL)aSel;
+ getClass;
- getClass;
- getZone;
- (void) dropAllocations: (BOOL)componentAlloc;
- (void) drop;
- (ref_t) addRef: (notify_t)notifyFunction withArgument: (void *)arg;;
- (void) removeRef: (ref_t)refVal;
+ getSuperclass;
+ (BOOL) isSubclass: aClass;
+ (void) setTypeImplemented: aType;
+ getTypeImplemented;
+ getOwner;
+ (IMP) getMethodFor: (SEL)aSel;
+ getDefiningClass;
+ getNextPhase;
+ self;
- getType;
- (int) compare: anObject;
- perform: (SEL)aSel;
- perform: (SEL)aSel with: anObject1;
- perform: (SEL)aSel with: anObject1 with: anObject2;
- perform: (SEL)aSel with: anObject1 with: anObject2 with: anObject3;
- (void) setDisplayName: (char *)aName;
- (char *) getDisplayName;
- (void) describe: outputCharStream;
- (void) xprint;
- (void) xprintid;
@end

//
// macros for accessing bits at defined locations inside instance variables
//

#define getBit( word, bit ) \
  ( (word) & bit )
#define setBit( word, bit, value ) \
  ( (value) ? ((word) |= bit) : ((word) &= ~bit) )

#define setField( word, shift, value ) \
(word) |= ((value) << shift)

#define getField( word, shift, mask ) \
((unsigned)((word) & mask) >> shift)

//
// callMethodInClass() -- macro for method lookup in an alternate superclass
//
// This macro is similar to the macro CALL_METHOD_IN_CLASS in
// GNU libobjects-0.1.19/src/behavior.h, by Andrew McCallum.
//
#define callMethodInClass( aClass, aMessage, args... ) \
({ SEL _sel_ = (aMessage); \
get_imp( (aClass), _sel_ )( self, _sel_ , ## args ); })
extern IMP get_imp( Class class, SEL sel );  // function used by macro

//
// respondsTo() -- function to test if object responds to message  
//
extern BOOL respondsTo( id anObject, SEL aSel );

//
// getMethodFor() --
//   function to look up the method that implements a message for an object
//
extern IMP getMethodFor( id anObject, SEL aSel );

//
// getClass() -- macro to get class of instance
//
#define getClass( anObject ) \
( *(Class *)(anObject) )

//
// setClass() -- macro to set behavior of instance to compatible class
//
#define setClass( anObject, aClass ) \
( *(Class *)(anObject) = (Class)(aClass) )

//
// struct mapalloc, mapalloc_t --
//   structure that maps an internal allocation within an object
//
// The mapalloc_t type is required in the declaration of any mapAllocations:
// method, so the definition is included here for reference in the method
// declarations of any subclass.  The implementation of a mapAllocations:
// method, however, as well as all other other support for internal
// allocations, still requires an explicit #import of <defobj/defalloc.h>.
//
typedef struct mapalloc *mapalloc_t;
struct mapalloc {
  void  (*mappingFunction)( mapalloc_t mapalloc, BOOL objectAllocation );
  void  *alloc;         // allocated object or block
  id    descriptor;     // descriptor for contents of allocated block, if any
  id    zone;           // zone of allocated block, as used by descriptor
  int   size;           // size of allocated block, as used by descriptor
};
