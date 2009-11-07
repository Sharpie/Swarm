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
Name:         DefObject.h
Description:  top-level superclass to provide standard services
Library:      defobj
*/

#define DEFINE_CLASSES
#import <Swarm/defobj.h>
#import <objc/Object.h>

#import <Swarm/swarmconfig.h> // PTRUINT

@class ObjectEntry;

#ifdef INHERIT_OBJECT
#if SWARM_OSX
@interface Object_s: NSObject <DefinedClass, Serialization, GetName>
#else
@interface Object_s: Object <DefinedClass, Serialization, GetName>
#endif
{
@public
  // Word that contains zone in which object allocated, plus
  // additional bits about the memory allocations for the object.
  PTRUINT zbits;
  ObjectEntry *foreignEntry;
}
#else
@interface Object_s <DefinedClass, Serialization, GetName>
{
@public
   // Class that implements the behavior of an object
   Class isa;           
   // Word that contains zone in which object allocated, plus
   // additional bits about the object.
   PTRUINT zbits; 
   ObjectEntry *entry;
}
#endif
/*** methods in Object_s (inserted from .m file by m2h) ***/
- (const char *)getName;
+ (BOOL)respondsTo: (SEL)aSel;
- (BOOL)respondsTo: (SEL)aSel;
+ (BOOL)conformsTo: (Protocol *)aProtocol;
+ (Class)getClass;
- (Class)getClass;
- getZone;
- (void)dropAllocations: (BOOL)componentAlloc;
- (void)drop;
- (ref_t)addRef: (notify_t)notifyFunction withArgument: (void *)arg;;
- (void)removeRef: (ref_t)refVal;
+ getSuperclass;
+ (BOOL)isSubclass: aClass;
+ (void)setTypeImplemented: aType;
+ getTypeImplemented;
+ getOwner;
+ (IMP)getMethodFor: (SEL)aSel;
+ getDefiningClass;
+ getNextPhase;
+ self;
- getType;
- (int)compare: anObject;
- perform: (SEL)aSel;
- perform: (SEL)aSel with: anObject1;
- perform: (SEL)aSel with: anObject1 with: anObject2;
- perform: (SEL)aSel with: anObject1 with: anObject2 with: anObject3;
- (void)setDisplayName: (const char *)aName;
- (const char *)getDisplayName;
- (const char *)getObjectName;
- (const char *)getTypeName;
- (void)describe: outputCharStream;
- (void)describeID: outputCharStream;
- (void)xprint;
- (void)xprintid;
- (void)xfprint;
- (void)xfprintid;
- lispInCreate: expr;
- lispIn: expr;
- hdf5InCreate: (id <HDF5>)hdf5Obj;
- hdf5In: (id <HDF5>)hdf5Obj;

- (void)lispSaveStream: stream Boolean: (const char *)aName Value: (int)val;
- (void)lispSaveStream: stream Char: (const char*)aName Value: (char)val;
- (void)lispSaveStream: stream Short: (const char*)aName Value: (short)val;
- (void)lispSaveStream: stream UnsignedShort: (const char*)aName Value: (unsigned short)val;
- (void)lispSaveStream: stream Integer: (const char*)aName Value: (int)val;
- (void)lispSaveStream: stream Unsigned: (const char*) aName Value: (unsigned int)val;
- (void)lispSaveStream: stream Long: (const char*)aName Value: (long int)val;
- (void)lispSaveStream: stream UnsignedLong: (const char*)aName Value: (unsigned long int)val;
- (void)lispSaveStream: stream LongLong: (const char*)aName Value: (long long int)val;
- (void)lispSaveStream: stream UnsignedLongLong: (const char*) aName Value: (unsigned long long int)val;
- (void)lispSaveStream: stream Float: (const char*) aName Value: (double)val;
- (void)lispSaveStream: stream Double: (const char*) aName Value: (double)val;

- (void)lispOutVars: stream deep: (BOOL)deepFlag;
- (void)lispStoreBooleanArray: (BOOL *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;
- (void)lispStoreCharArray: (char *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;
- (void)lispStoreShortArray: (short int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;
- (void)lispStoreIntegerArray: (int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;
- (void)lispStoreUnsignedArray: (unsigned int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;
- (void)lispStoreLongArray: (long int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;
- (void)lispStoreUnsignedLongArray: (unsigned long int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;
- (void)lispStoreLongLongArray: (long long int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;
- (void)lispStoreUnsignedLongLongArray: (unsigned long long int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;
- (void)lispStoreFloatArray: (float *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;
- (void)lispStoreDoubleArray: (double *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;
- (void)lispOutDeep: stream;
- (void)lispOutShallow: stream;
- (void)hdf5OutDeep: hdf5obj;
- (void)hdf5OutShallow: hdf5obj;
- (void)updateArchiver: archiver;
@end

#ifdef __cplusplus
extern "C" {
#endif
extern id lispIn (id aZone, id expr);
extern BOOL lispInBoolean (id index);
extern int lispInInteger (id index);
extern const char *lispInString (id index);
extern id lispInKeyword (id index);

#ifdef __cplusplus
}
#endif

//
// macros for accessing bits at defined locations inside instance variables
//

#define getBit(word, bit) ((word) & bit)
#define setBit(word, bit, value)((value) ? ((word) |= (PTRUINT) bit) : \
                                           ((word) &= ~((PTRUINT) bit)))

// ((value) ? ((word) |= bit) : 
//				  ((word) &= ~ bit))

#define setField(word, shift, value) (word) |= ((value) << shift)

#define getField(word, shift, mask) ((PTRUINT) ((word) & mask) >> shift)

//
// callMethodInClass() -- macro for method lookup in an alternate superclass
//
// This macro is similar to the macro CALL_METHOD_IN_CLASS in
// GNU libobjects-0.1.19/src/behavior.h, by Andrew McCallum.
//
#define callMethodInClass(aClass, aMessage, args...) \
  ({ SEL _sel_ = (aMessage); \
     swarm_class_getMethodImplementation ((aClass), _sel_) (self, _sel_ , ## args); })

#if SWARM_OBJC_DONE
extern IMP get_imp (Class class_, SEL sel);  // function used by macro
#endif

//
// respondsTo() -- function to test if object responds to message  
//
extern BOOL respondsTo (id anObject, SEL aSel);

//
// getMethodFor() --
//   function to look up the method that implements a message for an object
//
extern IMP getMethodFor (Class aClass, SEL aSel);

//
// getClass() -- macro to get class of instance
//
#if SWARM_OBJC_DONE
#define getClass(anObject) (*(Class *)(anObject))
#else
#define getClass(anObject) swarm_object_getClass(anObject)
#endif

//
// setClass() -- macro to set behavior of instance to compatible class
//
#if SWARM_OBJC_DONE
#define setClass(anObject, aClass) (*(Class *)(anObject) = (Class)(aClass))
#else
#define setClass(anObject, aClass) swarm_object_setClass(anObject, aClass)
#endif

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
  void (*mappingFunction) (mapalloc_t mapalloc, BOOL objectAllocation);
  void *alloc;            // allocated object or block
  id <Symbol> descriptor; // descriptor for contents of allocated block, if any
  id zone;                // zone of allocated block, as used by descriptor
  size_t size;            // size of allocated block, as used by descriptor
};

