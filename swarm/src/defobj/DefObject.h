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
#import <defobj/defalloc.h>

#ifdef INHERIT_OBJECT
@interface Object_s : Object
{
@public
  unsigned  zbits;  // word that contains zone in which object allocated, plus
                    // additional bits about the object
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
- (ref_t) addRef: (notify_t)notifyFunction withArg: (void *)arg;;
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
- copy;
- (void) setDisplayName: (char *)aName;
- (char *) getDisplayName;
- (void) describe: outputCharStream;
- (void) xprint;
@end

//
// getClass() -- macro to get class of instance
//
#define getClass( anObject ) ( *(Class *)anObject )

//
// setClass() -- macro to set behavior of instance to compatible class
//
#define setClass( anObject, aClass ) \
  ( *(Class *)anObject = (Class)aClass )

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
// This macro is identical in function to the macro CALL_METHOD_IN_CLASS
// in GNU libobjects-0.1.19/src/behavior.h, by Andrew McCallum.
//
#define callMethodInClass( aClass, aMessage, args... ) \
get_imp( aClass, @selector(aMessage) )( self, @selector(aMessage), ## args )
extern IMP get_imp( Class class, SEL sel );

//
// respondsTo() -- function to test if object responds to message  
//
extern BOOL respondsTo( id anObject, SEL aSel );

//
// getMethodFor() --
//   function to look up the method that implements a message for an object
//
extern IMP getMethodFor( id anObject, SEL aSel );
