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
Name:         defobj.h
Description:  standard objects for GNU Objective C extensions
Library:      defobj
*/

#ifndef __swarm_defobj_h
#define __swarm_defobj_h

#include <Swarm/swarmconfig.h>

#if defined(__cplusplus) && !defined(__OBJC__)
extern "C" {
#endif
typedef enum {fcall_type_void = 0,
              fcall_type_boolean,
              fcall_type_uchar, fcall_type_schar,
              fcall_type_ushort, fcall_type_sshort,
              fcall_type_uint, fcall_type_sint,
              fcall_type_ulong, fcall_type_slong,
              fcall_type_ulonglong, fcall_type_slonglong, 
              fcall_type_float, fcall_type_double, fcall_type_long_double,
              fcall_type_object, fcall_type_class,
              fcall_type_string, fcall_type_selector, 
              fcall_type_jobject, fcall_type_jstring, fcall_type_jselector,
              fcall_type_iid
              } fcall_type_t;

#ifdef __APPLE__
#import <Cocoa/Cocoa.h>
#endif
#import <Swarm/swarm-objc-api.h>
#include <stdio.h>

typedef union {
  id object;
  SEL selector;
  Class _class;
  const char *string;
  BOOL boolean;
  char schar;
  unsigned char uchar;
  short sshort;
  unsigned short ushort;
  int sint;
  unsigned int uint;
  long slong;
  unsigned long ulong;
  long long slonglong;
  unsigned long long ulonglong;
  float _float;
  double _double;
  long double _long_double;
  void *iid;
} types_t;

typedef struct {
  fcall_type_t type;
  types_t val;
} val_t;

#define FCALL_TYPE_COUNT 23

#if defined(__cplusplus) && !defined(__OBJC__)
}
#else
#import <Swarm/deftype.h>
#include <Swarm/externvar.h>

//S: Standard objects for GNU Objective C extensions

//D: The defobj library supports the style of object-oriented programming
//D: that is used throughout Swarm.  It defines a specific style for using
//D: the Objective C language that includes its own standard conventions for
//D: creating objects and for storage allocation, error handling, and debugging
//D: support.

@protocol GetName
//S: Get name which identifies object in its context of use.

//D: Get name which identifies object in its context of use.

//M: The getName message returns a null-terminated character string that
//M: identifies an object in some context of use.  This message is commonly
//M: used for objects that are created once in some fixed context where
//M: they are also assigned a unique name.  Constant objects defined as
//M: part of a program or library are examples.  This message is intended
//M: only for returning a name associated with an object throughout its
//M: lifetime.  It does not return any data that ever changes, however it 
//M: does return a different memory handle from the scratchZone (which can be
//M: freed using the free: method).
USING
- (const char *)getName;
@end

@protocol DefinedObject <GetName>
//S: Object with defined type and implementation.

//D: DefinedObject is the top-level supertype for all objects that follow
//D: the object programming conventions of the defobj library.  The
//D: messages defined by this type are the only messages which should be
//D: assumed to be automatically available on objects that follow these
//D: conventions.  In particular, use of messages defined by the Object
//D: superclass of the GNU Objective C runtime should not generally be
//D: assumed because future implementations of some objects might not give
//D: continued access to them.

//D: The DefinedObject type defines a minimum of standard messages, and
//D: leaves to other types the definition of message that might or might
//D: not apply in any general way to particular objects.
USING
//M: The respondsTo: message returns true if the object implements the
//M: message identified by the selector argument.  To implement a message
//M: means only that some method will receive control if the message is
//M: sent to the object.  (The method could still raise an error.)  The
//M: respondsTo: message is implemented by direct lookup in a method
//M: dispatch table, so is just as fast as a normal message send.  It
//M: provides a quick way to test whether the type of an object includes a
//M: particular message.
- (BOOL)respondsTo: (SEL)aSel;

+ (BOOL)conformsTo: (Protocol *)protocol;
- (BOOL)conformsTo: (Protocol *)protocol;

//M: getClass returns the class that implements the current behavior of an
//M: object.
- (Class)getClass;

//M: getTypeName returns the name of the originating type of this object.
- (const char *)getTypeName;

//M: Adds an external reference to an object that is notified when a
//M: an object is dropped.
- (ref_t)addRef: (notify_t)notifyFunction withArgument: (void *)arg;

//M: Remove an external reference to an object.
- (void)removeRef: (ref_t)refVal;

//M: A local implementation of an Object method.
- (int)compare: anObject;

//M: A local implementation of an Object method.
- perform: (SEL)aSel;

//M: A local implementation of an Object method.
- perform: (SEL)aSel with: anObject1;

//M: A local implementation of an Object method.
- perform: (SEL)aSel with: anObject1 with: anObj2;

//M: Perform a selector with three object arguments.
- perform: (SEL)aSel with: anObject1 with: anObj2 with: anObj3;

//M: Assigns a character string as a name that identifies an
//M: object for display or debug purposes.
- (void)setDisplayName: (const char *)displayName;

//M: Return a string that identifies an object for external
//M: display purposes, either from a previously assigned
//M: string or an identification string default     
- (const char *)getDisplayName;

//M: The describe: message prints a brief description of the object for
//M: debug purposes to the object passed as its argument.  The object
//M: passed as the outputCharStream argument must accept a catC: message as
//M: defined in String and OutputStream in the collections library.
//M: Particular object types may generate object description strings with
//M: additional information beyond the built-in default, which is just to
//M: print the hex value of the object id pointer along with the name of
//M: its class, and the display name of the object, if any.
- (void)describe: outputCharStream;

//M: Prints a one-line describe string, consisting of the built-in default to
//M: outputCharStream.
- (void)describeID: outputCharStream;

//M: Like describe:, but output goes to standard output.
- (void)xprint;

//M: Like describeID:, but output goes to standard output.
- (void)xprintid;

//M: print description for each member of a collection on debug output stream
- (void)xfprint;

//M: print id for each member of a collection on debug output stream
- (void)xfprintid;

//M: The getZone message returns the zone in which the object was created.
- getZone;
@end

@protocol Customize
//S: Create-phase customization.

//D: Some types accept create-time messages not only when creating a new
//D: instance, but to customize a new version of the type itself.  Objects
//D: created from a customized type will have all options preset that
//D: create-time messages sent to the customized type object have already
//D: set.  If many objects all need the same create-time options, it is
//D: often simpler (and can also be faster) to create a customized version
//D: of a type first, and then create further instances from that type.

//D: Customizing a type object does not modify the original type object,
//D: but instead creates a new type object that has the customizations
//D: built-in.  A create: message on the new type object creates a new
//D: instance as if the same sequence of create-time messages had been
//D: sent to the original type object using createBegin: and createEnd.
//D: A type is customized by bracketing the sequence of create-time
//D: messages not with the createBegin: and createEnd messages used to
//D: create a new instance, but with customizeBegin: and customizeEnd
//D: messages instead.

//D: Whether a customized version of a type can be created depends on the
//D: implementation of the type itself.  If a type does not support
//D: customization, a customizeBegin: message on the type raises an error.
//D: All types defined by an @protocol declaration may be relied on to
//D: support at least one cycle of customization to create a new type
//D: object.  Whether an already customized type object (returned by
//D: customizeEnd) supports a further cycle of customization (by another
//D: sequence of customizeBegin:/customizeEnd) depends on the
//D: implementation of the original starting type.  A type should not be
//D: relied on to support more than one cycle of customization unless it is
//D: specifically documented to do so.

CREATING
//E: newArrayType = [Array customizeBegin: aZone];
//E: [newArrayType setCount: 100];
//E: newArrayType = [newArrayType customizeEnd];
//E: array1 = [newArrayType create: aZone];
//E: array2 = [newArrayType create: aZone];
//E: // [array1 getCount] and [array2 getCount] are both 100

//M: Returns an interim value for receiving create-time
//M: messages much like createBegin:.

//M: The zone passed to customizeBegin: is the same zone from which storage
//M: for the new, finalized type object will be taken.  This zone need not
//M: be the same as any instance later created from that type, since a new
//M: zone argument is still passed in any subsequent create message on that
//M: type.
+ customizeBegin: aZone;

//M: Returns the new, customized version of the original type.
- customizeEnd;

//M: The customizeCopy: message creates a new copy of the interim object
//M: returned by customizeBegin: which may be used for further
//M: customizations that do not affect the customization already in
//M: progress.  It may be used to branch off a path of a customization in
//M: progress to create an alternate final customization. 

//M: customizeCopy may be used only on an interim object returned by
//M: customizeBegin: and not yet finalized by customizeEnd.  The new version
//M: of the interim object being customized may be allocated in the same
//M: or different zone as the original version, using the zone argument
//M: required by customizeCopy:

//E: newArrayType1 = [Array customizeBegin: aZone];
//E: [newArrayType1 setCount: 100];
//E: newArrayType2 = [newArrayType2 customizeCopy: aZone];
//E: [newArrayType2 setDefaultMember: UnsetMember];
//E:
//E: newArrayType1 = [newArrayType1 customizeEnd];
//E:  newArrayType2 = [newArrayType2 customizeEnd];
//E: array1 = [newArrayType1 create: aZone];  // no DefaultMember option
//E: array2 = [newArrayType create: aZone];   // DefaultMember option set
- customizeCopy: aZone;
@end

@protocol Create <DefinedObject, Customize>
//S: Create an instance of a type with optional customization.

//D: The Create supertype defines standard messages that provide a
//D: general-purpose protocol for creating new objects.  These messages may
//D: be used either to create a new instance of a type in one message, or
//D: to bracket a series of messages that customize available options for
//D: an object to be created.  The separation of create-time specifications
//D: from later behavior of an object gives substantial flexibility to
//D: adapt a generic type to particular needs.

//D: These create messages may be implemented either by a type that hides its
//D: implementing classes, or directly by a class that adopts these messages as
//D: a uniform interface for creating objects.  If implemented directly by a
//D: class, then the class object serves as the type object in all message
//D: descriptions that follow.  Otherwise, a type object might be implemented
//D: in a variety of ways that guarantee only that published messages on a
//D: type are accepted.

//D: In addition to the create messages defined by Create, an object type
//D: may support any other messages of any other names or calling
//D: conventions.  These messages define only a standard method for
//D: creating new objects that other types are free to inherit and
//D: implement in conformance with a uniform convention.  Further
//D: conventions are established elsewhere create combination messages
//D: for standard ways in which create messages
//D: that combine several steps can be combined.

//D: Any interim object returned by either createBegin: or customizeBegin:
//D: supports the getZone and drop messages that a finalized instance may
//D: also support.  These messages are defined by the Drop type, which is
//D: normally inherited by a type to declare these messages on a finalized
//D: instance.  This type is not inherited by the Create type because the
//D: messages would then apply to the finalized instance, not to the
//D: interim object.  Even though not declared, the messages are available
//D: on the interim objects nonetheless.  The drop message on an interim
//D: object may be used if it turns out that a finalized version is no
//D: longer required after creation or customization has already begun.

//D: The createBegin: and createEnd messages bracket a series of messages
//D: that specify options for an object being created.  The intermediate
//D: messages can set values defined as parameters of the type, or provide
//D: other forms of specification using available messages.  A particular
//D: object type defines the specific messages that are valid for sending
//D: during this interim creation phase.

//E: newArray = [Array createBegin: aZone];
//E: [newArray setInitialValue: aList];
//E: [newArray setDefaultMember: UnsetMember];
//E: [newArray setCount: [aList getCount] * 2 );
//E: newArray = [newArray createEnd];   // ! note reassignment of newArray
CREATING
//M: The create: message creates a new instance of a type with default
//M: options.  The zone argument specifies the source of storage for the
//M: new object.  The receiving object of this message is a previously
//M: defined type object.  The message is declared as a class message (with
//M: a + declaration tag) to indicate that the message is accepted only by
//M: the type object itself rather than an already created instance of the
//M: type (which a - declaration tag otherwise defines).

//M: The create: message returns the new object just created.  This object is
//M: an instance of some class selected to implement the type.  The class
//M: which a type selects to implement an object may be obtained by the
//M: getClass message, but is not otherwise visible to the calling program.
//M: A caller never refers to any class name when creating objects using
//M: these messages, only to type names, which are automatically published
//M: as global constants from any @protocol declaration. 
+ create: aZone;

//M: createBegin: returns an interim object intended only for receiving
//M: create-time messages.  If a type was defined by a @protocol
//M: declaration, these messages are those appearing in either the CREATING
//M: or SETTING sections.  Otherwise, the messages valid as create-time
//M: messages are defined by the type without any specific syntactic
//M: marker.
+ createBegin: aZone;

//M: The createEnd message completes the process of specifying available
//M: options for an object being created.  Typically it validates that
//M: requested options are valid and consistent with one another, and
//M: raises an error if they are not.  The standard, predefined error
//M: InvalidCombination may be raised by createEnd to indicate an
//M: invalid combination of requests, or other, more specific forms
//M: of error handling may be used.

//M: If all requests received since the initial createBegin: are valid,
//M: both individually and in combination with each other, then createEnd
//M: determines a finalized form of object that satisfies all requests
//M: received and then returns this object.  Any additional storage
//M: required for the finalized object is taken from the same zone
//M: originally passed to createBegin.  The object may have whatever
//M: implementation is selected to best satisfy a particular request.
//M: Different requests may result in entirely different implementations
//M: being returned.  The only guarantee is that a returned object supports
//M: the messages defined for further use of the finalized object.  If a
//M: type was defined by a @protocol declaration, these messages are those
//M: appearing in either the SETTING or USING sections.

//M: On return from createEnd, the id of the interim object returned by
//M: createBegin: is no longer guaranteed to be valid for further use, and
//M: should no longer be referenced.  A variable which holds this such an
//M: id can be reassigned the new id returned by createEnd, so that the
//M: same variable holds successive versions of the object being created.
- createEnd;
@end

@protocol Drop
//S: Deallocate an object allocated within a zone.

//D: The Drop supertype defines the drop message, which is a standard
//D: message for indicating that an object no longer exists and will never
//D: again be referenced.  Any future attempt to reference a dropped object
//D: is an error.  This error may or not produce predictable effects
//D: depending on the level of debug checking and other factors.

//M: Immediate effects of the drop message depends on the subtype of Zone
//M: used to provide storage for the object.  For some zone types, the drop
//M: message immediately deallocates storage for the object and makes the
//M: freed storage available for other use.  Subsequent use could include
//M: the allocation of a new object at precisely the same location,
//M: resulting in a new object id identical to a previously dropped one.

//M: The Drop type may be inherited by any type that provides drop support
//M: for its instances.  In addition to freeing the storage and
//M: invalidating the object, a drop message may release other resources
//M: acquired or held within the object.  Not every object which can be
//M: created can also be dropped, and some objects can be dropped which are
//M: not directly creatable.  Some objects may be created as a side effect
//M: of other operations and still be droppable, and some objects may be
//M: created with links to other objects and not droppable on their own.  A
//M: type independently inherits Create or Drop types, or both, to indicate
//M: its support of these standard interfaces to define the endpoints of an
//M: object lifecycle.
USING
- (void)drop;
@end

@protocol Zone <Create, Drop, CREATABLE>
//S: Modular unit of storage allocation.

//D: A zone is a source of storage for objects or other allocated data.
//D: Whenever a new object is created, a zone must be identified from which
//D: the storage for its instance variables, or other internal data, is
//D: obtained.  A program may establish multiple zones to ensure that
//D: objects with similar lifetime or storage needs are allocated together,
//D: and in general to optimize allocation and reuse of storage.

//D: Zones also maintain a collection of all objects allocated within the
//D: zone.  This collection, referred to as the "population" of a zone, is
//D: a set of all objects which have been created but not yet dropped
//D: within the zone.  Collections maintained automatically by zones can
//D: eliminate a need for other, separately maintained collections in
//D: applications that need to keep track of entire populations of objects.
//D: Collections of allocated objects can provide support for object query,
//D: external object storage, and automatic storage reclamation.

//D: A zone may be used to obtain storage not only for objects, but also
//D: for raw storage blocks like those provided by the C malloc function.
//D: All objects and storage blocks allocated in a zone remain local to
//D: that zone.  This means that allocation of storage in other zones does
//D: not affect the efficiency of storage allocation within a particular
//D: zone.  For most zone types, individual allocations may still be freed
//D: within a zone, and total storage of a zone may grow and shrink
//D: according to aggregate needs.  In addition to freeing individual
//D: allocations, an entire zone may also dropped.  Dropping a zone
//D: automatically frees all object allocations made within it, including final
//D: drop processing on any allocated objects that need it.  Release of an
//D: entire zone can be much faster than individual release of each object
//D: within it.

//D: The Zone type is a fully implemented type that provides default
//D: storage management support for objects and other allocated storage.
//D: It is also a supertype for other zones that implement alternative
//D: policies for use in specialized situations.

//D: A zone is created using standard create messages just like other
//D: objects.  This means that a zone must identify another zone from which
//D: it obtains its storage.  Storage is typically obtained from this other
//D: zone in large units called pages, which are then managed by the local
//D: zone to support internal allocations.  The getZone message of the
//D: DefinedObject type returns the zone which provides these
//D: base pages.

//D: Since a new zone always requires that an existing zone be identified,
//D: no new zones could be created unless there were some zones that
//D: already existed.  Two such zones are predefined as part of the
//D: defobj library: globalZone and scratchZone.

CREATING
//M: PageSize specifies the size of pages within which a zone manages its
//M: internal allocation.  Its default is typically a natural page size
//M: (perhaps 4K) for the local machine architecture.  The default should
//M: be overridden only when tuning storage allocation for very specific
//M: situations.  Allocations within a zone are not limited to the page
//M: size, since any requests that exceed the page size are simply passed
//M: up to the owner zone within which the zone was allocated.
- (void)setPageSize: (size_t)pageSize;

USING
- (size_t)getPageSize;

//M: allocIVars: allocates the instance variable structure for a new
//M: object.  The initial word of this structure is set to class id passed
//M: as its argument.  The class also determines the size of the structure
//M: allocated.  All remaining contents of this structure are initialized
//M: to binary zeroes.
- allocIVars: (Class)aClass;

//M: copyIVars: creates copies an existing instance variable structure into
//M: a new allocation made within the local zone.  The existing instance
//M: variable structure may be in any zone, but must contain a class
//M: pointer in its first word that correctly describes the size of the
//M: structure.
- copyIVars: anObject;

//M: freeIVars: releases storage that was previously allocated to hold the
//M: instance variable structure of an object.  The first word of the
//M: object must be a class pointer that correctly describes the size of
//M: the structure.  Storage allocated by allocIVars: or copyIVars: may be
//M: freed only by freeIVars:, and freeIVars: may be used only to free
//M: storage allocated by one of these messages.
- (void)freeIVars: anObject;

//M: These messages allocate, copy, and free

//M: This message allocates the storage that holds the
//M: instance variables for an object.  It allocates the object as an
//M: internal component of the zone that is not included in the zone
//M: population.  It is used by classes that allocate additional objects
//M: as part of the implementation of another object, and that control the
//M: mapping of this storage separately from the zone level objects.
- allocIVarsComponent: (Class)aClass;

//M: Like allocateIVarsComponent, except it copies the storage that holds
//M: the instances variables for an object.
- copyIVarsComponent: anObject;

//M: Frees the instance variable storage for an object.
- (void)freeIVarsComponent: anObject;

//M: Returns a specially qualified version of the zone that automatically
//M: allocates all its objects with the internal component qualification,
//M: even if allocated with allocIVars: or copyIVars:.  This qualified zone
//M: may be passed as an argument to a create: or createBegin: message so
//M: that it will create the new object as an internal component object.
- getComponentZone;

//M: alloc: allocates a new storage block much like the malloc function of
//M: the C library.  The storage is aligned according to the most restrictive
//M: requirements for any data type on the local machine architecture.  The
//M: storage is not initialized to any known contents.
- (void *)alloc: (size_t)size;

//M: free: releases a block of storage previously allocated using alloc:.
//M: The size of the block is not required as an argument because alloc:
//M: has saved this size as necessary as part of the initial allocation.
//M: free: may be used only to free a block allocated by alloc:, and a
//M: block allocated by alloc: may be freed only by free:.
- (void)free: (void *)aBlock;

//M: allocBlock: allocates a new storage block similar to alloc:, except
//M: that the size of the block allocated must be passed as an argument
//M: when freeing the block. 
- (void *)allocBlock: (size_t)size;

//M: freeBlock:blockSize: must be used to free any block previously
//M: allocated by allocBlock:.
- (void)freeBlock: (void *)aBlock blockSize: (size_t)size;

//M: getPopulation returns a collection all objects allocated in a zone
//M: using either allocIVars: or copyIVars: and not yet freed using
//M: freeIVars:.  getObjects returns nil if the ObjectCollection option is
//M: false.  The collection returned has the type OrderedSet as defined in
//M: the collections library, with the ReadOnly option set true and the
//M: IndexSafety option set to SafeAlways.  The members of this collection
//M: may change as objects are allocated and freed, but may not added or
//M: removed directly within the collection.
- getPopulation;

//M: Generate debug description for each member of the zone population.
- (void)describeForEach: outputCharStream;

//M: Generate debug id description for each member of the zone population.
- (void)describeForEachID: outputCharStream;
@end

@protocol DefinedClass <DefinedObject>
//S: Class which implements an interface of a type.

//D: Class which implements an interface of a type.
USING
+ getSuperclass;
+ (BOOL)isSubclass: aClass;

+ (void)setTypeImplemented: aType;
+ getTypeImplemented;

+ (IMP)getMethodFor: (SEL)aSel;
@end

@protocol CreatedClass <Create, DefinedClass>
//S: Class with variables and/or methods defined at runtime.

//D: Class with variables and/or methods defined at runtime.

CREATING
- setName: (const char *)name;
- setClass: (Class)aClass;
- setSuperclass: aClass;
- setDefiningClass: aClass;
- at: (SEL)aSel addMethod: (IMP)aMethod;
- lispInCreate: expr;
- hdf5InCreate: hdf5Obj;
- (void)lispOutShallow: stream;
- (void)hdf5OutShallow: hdf5Obj;
- (void)updateArchiver: archiver;
USING
- getDefiningClass;
@end

@protocol BehaviorPhase <CreatedClass>
//S: Created class which implements a phase of object behavior.

//D: Created class which implements a phase of object behavior.

CREATING
- (void)setNextPhase: aClass;

USING
- getNextPhase;
@end

@protocol Copy
//S: Copy all state defined as part of object.

//D: An object type that supplies the copy operation defines what it
//D: includes as the contents of an object copied.  There is no global rule
//D: for what is considered "inside" a copied object vs. merely referenced
//D: by it.  (There is no fixed notion of "shallow" vs. "deep" copy found
//D: in some object libraries.)  After copying, the new object may still
//D: contain some references to other elements also referenced by the
//D: starting object, but in general the new object minimizes any
//D: dependencies shared with the starting object.  Any object type
//D: supplying the copy message should also supply documentation on its
//D: rules for copied objects.

//M: The copy message creates a new object that has the same contents and
//M: resulting behavior as a starting object, except that an independent
//M: copy of the contents of the starting object is created so that further
//M: changes to one object do not affect the other.  The zone argument
//M: specifies the source of storage for the new object.  The message
//M: returns the id of the new object created.
USING
- copy: (id <Zone>)aZone;
@end

//
// Miscellaneous operations for mixing into other types.
//

@protocol GetOwner
//S: Get object on which existence of object depends.

//D: Ownership hierarchies arrange themselves in a strict, single-rooted
//D: tree.  The top-level node of an ownership hierarchy typically returns
//D: nil as its owner.  If an object is regarded merely as one part of
//D: another object defined as its owner, then copying or dropping the
//D: owner object should copy or drop the member object as well.  Owner and
//D: member are neutral terms for a generic relationship sometimes called
//D: parent vs.  child, but it is up to a particular object type to define
//D: specifically what it means by a getOwner relationship.

//M: The getOwner message returns another object which is considered as the
//M: owner of an initial object.  What is considered as an owner depends on
//M: its specific object type, but might be a larger object of which the
//M: local object is a part, or an object that has exclusive control over
//M: the local object.  The principal constraint established by an
//M: ownership structure is that a given object can have only a single
//M: other object as its unambiguous owner.
USING
- getOwner;
@end

@protocol SetInitialValue
//S: Create using initial value from an existing object.

//D: The SetInitialValue type defines a variety of messages relating to an
//D: initial or unmodifiable value established as part of an object.  This
//D: message is typically provided when creation of a new object might be
//D: more easily accomplished by copying the value of an existing object
//D: rather than establishing a new value from scratch.  As with the copy
//D: message, precisely what is considered the value of an existing object
//D: to copy is defined only by the particular object type that supplies
//D: these messages.

//D: If an object has a value which can be established at create time, it
//D: is often useful (and can also enable significant optimization) to
//D: declare that no further modification will occur to this value during
//D: further use of the object.  A restriction against modifying a value is
//D: referred to as a "read-only" restriction.  This type supplies messages
//D: to declare a read-only restriction along with any initial value.  For
//D: some object types, a read-only restriction can also be added or
//D: removed after an object has already been created.

CREATING
//M: The setInitialValue: message requires another object as its argument,
//M: from which the value of a newly created object is to be taken.  Unlike
//M: a copy message, the object used as the source of the new value need
//M: not have the identical type as the new object to be created.  A
//M: particular object type defines the types of initial value objects
//M: which it can accept, along with any special conversion or
//M: interpretation it might apply to such a value.
- (void)setInitialValue: initialValue;
@end

@protocol Symbol <Create, GetName, CREATABLE>
//S: Object defined as a distinct global id constant.

//D: A Symbol is an object created with a fixed name.  It has no behavior
//D: except to get the name with which it was created.  A Symbol is
//D: typically used to define unique id values which are assigned to global
//D: constant names.  These names, capitalized according to the recommended
//D: convention for global object constants, are used by some libraries as
//D: flags or enumerated value codes in arguments or return values of
//D: messages.

//D: Ordinarily, a symbol is created with its character string name
//D: matching the global id constant to which it is assigned.  These global
//D: program constants can then provide a minimal level of self
//D: documentation as objects.  Subtypes of Symbol can extend the base of a
//D: named, global id constant to establish further components of a global,
//D: constant definition.

//D: A symbol is fully creatable using standard Create messages.  A
//D: character string name must be supplied for any new symbol; there is no
//D: default.  Symbol inherits the getName message, which returns the
//D: symbol name.

CREATING
//M: create:setName: is a combination message defined as a caller
//M: convenience.  See combination messages for a summary of conventions
//M: on combination messages.
+ create: (id <Zone>)aZone setName: (const char *)name;

//#: macro used to create and initialize a symbol
#define defsymbol(name) name = [Symbol create: globalZone setName: #name]
@end

@protocol EventType <Symbol>
//S: A report of some condition detected during program execution.

//D: A report of some condition detected during program execution.

USING
//M: Raise an event noting the event symbol type.
- (void)raiseEvent;

//M: Raise an event noting the event symbol type using a format string
//M: and arguments.
- (void)raiseEvent: (const void *)eventData, ...;

//#: macro to raise Warning or Error with source location strings
#define raiseEvent( eventType, formatString, args... ) \
[eventType raiseEvent: \
"\r", __FUNCTION__, __FILE__, __LINE__, formatString , ## args]
@end


@protocol Warning <EventType, CREATABLE>
//S: A condition of possible concern to a program developer.

//D: A condition of possible concern to a program developer.

USING
//M: Associate a message string with this warning.
- (void)setMessageString: (const char *)messageString;

//M: Return the message associated with this warning.
- (const char *)getMessageString;

externvar id <Warning>
  WarningMessage,         //G: message in the source defines warning
  ResourceAvailability,   //G: resource from runtime environment not available
  LibraryUsage,           //G: invalid usage of library interface
  DefaultAssumed,         //G: non-silent use of default
  ObsoleteFeature,        //G: using feature which could be removed in future
  ObsoleteMessage,        //G: using message which could be removed in future
  SaveWarning;            //G: non-fatal problem saving a resource  

//#: macro used to create and initialize an Error symbol
#define defwarning(name, message) \
  [(name = [Warning create: globalZone setName: #name]) \
    setMessageString: message]
@end

@protocol Error <Warning, CREATABLE>
//S: A condition which prevents further execution.

//D: A condition which prevents further execution.

externvar id <Error> 
  SourceMessage,        //G: message in the source defines error
  NotImplemented,       //G: requested behavior not implemented by object
  SubclassMustImplement,//G: requested behavior must be implemented by subclass
  InvalidCombination,   //G: invalid combination of set messages for create
  InvalidOperation,     //G: invalid operation for current state of receiver
  InvalidArgument,      //G: argument value not valid
  CreateSubclassing,    //G: improper use of Create subclassing framework
  CreateUsage,          //G: incorrect sequence of Create protocol messages
  OutOfMemory,          //G: no more memory available for allocation
  InvalidAllocSize,     //G: no more memory available for allocation
  InternalError,        //G: unexpected condition encountered in program
  BlockedObjectAlloc,   //G: method from Object with invalid allocation
  BlockedObjectUsage,   //G: method inherited from Object superclass
  ProtocolViolation,    //G: object does not comply with expected protocol
  LoadError,            //G: unable to access a resource
  SaveError;            //G: unable to save a resource

//#: macro used to create and initialize a Warning symbol
#define deferror(name, message) \
  [(id)(name = [Error create: globalZone setName: #name])	\
    setMessageString: message]
@end

@protocol Arguments <Create, Drop, CREATABLE>
//S: A class that provides customizable command line argument parsing support

//D: A class that provides customizable command line argument parsing support

//E: Let's say you want to add a new argument, say `protocol' to your standard 
//E: list of commands.  In other words you want the following to happen at the
//E: command line when you type --help.
//E: ------------------------
//E: mgd@wijiji[/opt/src/mgd/src/mySwarmApp] $ ./mySwarmApp --help
//E: Usage: mySwarmApp [OPTION...]
//E: 
//E:   -s, --varyseed             Select random number seed from current time
//E:   -S, --seed=INTEGER         Specify seed for random numbers
//E:   -b, --batch                Run in batch mode
//E:   -m, --mode=MODE            Specify mode of use (for archiving)
//E:   -p, --protocol=PROTOCOL    Set protocol
//E:   -?, --help                 Give this help list
//E:       --usage                Give a short usage message
//E:   -V, --version              Print program version
//E: 
//E: Mandatory or optional arguments to long options are also mandatory or 
//E: optional for any corresponding short options.
//E: 
//E: Report bugs to bug-swarm@swarm.org. 
//E: -----------------------
//E: 
//E: To implement this you need to make your own subclass of Arguments
//E: like the following:
//E: 
//E: #import <defobj/Arguments.h>
//E: 
//E: @interface MySwarmAppArguments: Arguments_c
//E: {
//E:   const char *protocolArg;
//E: }
//E: - (const char *)getProtocolArg;
//E: @end
//E: 
//E: @implementation MySwarmAppArguments
//E: 
//E: + createBegin: (id <Zone>)aZone
//E: {
//E:   static struct argp_option options[] = {
//E:     {"protocol", 'p', "PROTOCOL", 0, "Set protocol", 3},
//E:     { 0 }
//E:   };
//E:   
//E:   MySwarmAppArguments *obj = [super createBegin: aZone];
//E: 
//E:   [obj addOptions: options];
//E:   return obj;
//E: }
//E: 
//E: - (int)parseKey: (int)key arg: (const char *)arg
//E: {
//E:   if (key == 'p')
//E:     {
//E:       protocolArg = arg;
//E:       return 0;
//E:     }
//E:   else
//E:     return [super parseKey: key arg: arg];
//E: }
//E: 
//E: - (const char *)getProtocolArg
//E: {
//E:   return protocolArg;
//E: }
//E: 
//E: @end
//E:
//E: To actually invoke this in the main.m program, you do the following:
//E:
//E: int 
//E: main (int argc, const char ** argv) 
//E: {
//E:   initSwarmArguments (argc, argv, [MySwarmAppArguments class]);
//E:   
//E:   // the usual - buildObjects:, - buildActions:, - activateIn: calls
//E:   
//E:   return 0;					  
//E: }

CREATING
- setArgc: (unsigned)count Argv: (const char **)theArgv;
- setAppName: (const char *)appName;
- setAppModeString: (const char *)appModeString;
- setOptionFunc: (int (*) (int, const char *))optionFunc;
- setBugAddress: (const char *)bugAddress;
- setVersion: (const char *)version;
- addOptions: (struct argp_option *)options;

//M: Takes an option specification that includes the following information:

//M: - The name of the option specification

//M: - The key of the option.  This an integer that, if printiable, is
//M:   the single-character use of the option.  For example, `-p' 
//M:   vs. `--protocol' are the different versions of the same thing.
//M:   One is intended to be mnemonic, the other convenient.

//M: - If non-NULL, an argument label that says that the option
//M:   requires an argument (in this case, the protocol name).

//M: - Flags that change the visibility and parsing of the option 

//M: - Documentation for the option

//M: - A sorting integer; relative placement of the option in the help
//M:   screen.
- (void)addOption: (const char *)name key: (int)key arg: (const char *)arg flags: (int)flags doc: (const char *)doc group: (int)group;

+ createArgc: (int)argc Argv: (const char **)argv appName: (const char *)appName version: (const char *)version bugAddress: (const char *)bugAddress options: (struct argp_option *)options optionFunc: (int (*) (int, const char *))optionFunc inhibitExecutableSearchFlag: (BOOL)inhibitExecutableSearchFlag;

//M: This method is called for each option that occurs.
- (int)parseKey: (int)key arg: (const char *)arg;
SETTING
- setInhibitArchiverLoadFlag: (BOOL)inhibitArchiverLoadFlag;
- setInhibitExecutableSearchFlag: (BOOL)theInhibitExecutableSearchFlag;
- setBatchModeFlag: (BOOL)batchModeFlag;
- setVarySeedFlag: (BOOL)varySeedFlag;
- setVerboseFlag: (BOOL)verboseFlag;
//M: Specify a default path to use for configuration files when
//M: installed location of Swarm cannot be determined. 
//M: Defaults to current directory.
- setDefaultAppConfigPath: (const char *)path;
//M: Specify a default path to use for data files when installed location
//M: of Swarm cannot be determined.  Defaults to current directory.
- setDefaultAppDataPath: (const char *)path;
- setFixedSeed: (unsigned)seed;


USING
- (BOOL)getBatchModeFlag;
- (BOOL)getVarySeedFlag;
- (BOOL)getFixedSeedFlag;
- (unsigned)getFixedSeed;
- (BOOL)getVerboseFlag;
- (const char *)getAppName;
- (const char *)getAppModeString;
- (int)getArgc;
- (int)getLastArgIndex;
- (const char **)getArgv;
- (const char *)getExecutablePath;
- (const char *)getSwarmHome;
- (const char *)getConfigPath;
- (const char *)getDataPath;
//M: A path where application-specific data files can be expected to be found.
- (const char *)getAppDataPath;
//M: A path where application-specific configuration files can be expected
//M: to be found.
- (const char *)getAppConfigPath;
- (BOOL)getShowCurrentTimeFlag;
- (BOOL)getInhibitArchiverLoadFlag;
//M: The user assigned seed value for random number generation
@end

@protocol Archiver <Create, Drop, RETURNABLE>
//S: High level abstract serialization interface.
//D: High level abstract serialization interface.
CREATING

//M: Make the Archiver ignore any file found in the specified path
- setInhibitLoadFlag: (BOOL)inhibitLoadFlag;

//M: Set the physical path for the Archiver to read/write
- setPath: (const char *)path;

//M: Make the Archiver expect application metadata, such as `mode'
//M: information 
- setSystemArchiverFlag: (BOOL)systemArchiverFlag;

//M: Specify that the Archiver instance use the default system path
- setDefaultPath;

//M: Specify that the Archiver to use the default application path
- setDefaultAppPath;
USING
- (void)registerClient: client;
- (void)unregisterClient: client;

//M: Register with the Archiver a deep serialization of the object
//M: (serialization only occurs when Archiver is saved)
- (void)putDeep: (const char *)key object: object;

//M: As per -putDeep, but only make a shallow version
- (void)putShallow: (const char *)key object: object;

//M: Create the object with `key' using the Archiver's own Zone
- getObject: (const char *)key;

//M: Create the object with `key' in the specified Zone
- getWithZone: (id <Zone>)aZone key: (const char *)key;

//M: Ensure that that all registered 
//M: the requested backend
- (void)sync;
@end

@protocol LispArchiver <Archiver, CREATABLE>
//S: Protocol for creating Lisp instances of the Archiver
//D: Protocol for creating Lisp instances of the Archiver
//D: Default system path is ~/.swarmArchiver.scm.
//D: Default application path is <swarmdatadir>/<appname>/<appname>.scm
//D: or the current directory.
CREATING

//M: Convenience method to create LispArchiver from a specified path
+ create: (id <Zone>)aZone setPath: (const char *)path;

SETTING
USING
@end

@protocol HDF5Archiver <Archiver, CREATABLE>
//S: Protocol for creating HDF5 instances of the Archiver
//D: Protocol for creating HDF5 instances of the Archiver
//D: Default system path is ~/swarmArchiver.hdf
//D: Default application path is <swarmdatadir>/<appname>/<appname>.hdf 
//D: or the current directory.
CREATING

//M: Convenience method to create an HDF5Archiver from a specified path
+ create: (id <Zone>)aZone setPath: (const char *)path;

SETTING
USING
@end

@protocol HDF5 <Create, Drop, CREATABLE>
//S: HDF5 interface
//D: HDF5 interface
CREATING
- setWriteFlag: (BOOL)writeFlag;
- setDatasetFlag: (BOOL)datasetFlag;
- setExtensibleVectorType: (fcall_type_t)type;
- setExtensibleDoubleVector;
- setParent: parent;
- setCompoundType: compoundType;
- setCount: (unsigned)count;
SETTING
//M: Create-time use is to name the file or group.
//M: Setting-time use is to rename component datasets that don't
//M: parent's name.
- setName: (const char *)name; 
- setBaseTypeObject: baseTypeObject;
USING
- (void)iterate: (int (*) (id <HDF5>hdf5Obj))iterateFunc;
- (void)iterate: (int (*) (id <HDF5>hdf5Obj))iterateFunc drop: (BOOL)dropFlag;
- (BOOL)getDatasetFlag;
- (BOOL)getWriteFlag;
- (size_t)getDatasetRank;
- (size_t)getDatasetDimension: (unsigned)dimNumber;
- (fcall_type_t)getDatasetType;
- (unsigned)getCount;
- (const char *)getHDF5Name;
- getCompoundType;
- (Class)getClass;
- (void)assignIvar: obj;

- (BOOL)checkName: (const char *)name;
- (BOOL)checkDatasetName: (const char *)datasetName;

- (void)loadDataset: (void *)ptr;
- (void)storeAsDataset: (const char *)name typeName: (const char *)typeName type: (fcall_type_t)type rank: (unsigned)rank dims: (unsigned *)dims ptr: (void *)ptr;

- (void)addDoubleToVector: (double)val;

- (void)storeTypeName: (const char *)typeName;
- (void)storeComponentTypeName: (const char *)typeName;
- (void)shallowLoadObject: obj;
- (void)shallowStoreObject: obj;
- (void)nameRecord: (unsigned)recordNumber name: (const char *)recordName;
- (void)numberRecord: (unsigned)recordNumber;
- (void)selectRecord: (unsigned)recordNumber;

- (const char **)readRowNames;
- (void)writeRowNames;
- (void)writeLevels;

- (void)storeAttribute: (const char *)attributeName value: (const char *)valueString;
- (void)iterateAttributes: (int (*) (const char *key, const char *value))iterateFunc;
- (const char *)getAttribute: (const char *)attributeName;
- (void)flush;
@end

@protocol HDF5CompoundType <Create, Drop, CREATABLE>
//S: HDF5 composite type interface
//D: HDF5 composite type interface
CREATING
- setPrototype: prototype;
USING
- getPrototype;
@end

typedef void *JOBJECT;
typedef void *COMOBJECT;

@protocol FArguments <Create, Drop, CREATABLE>
//S: A language independent interface to dynamic call argument construction.
//D: A language independent interface to dynamic call argument construction.
CREATING
- setLanguage: (id <Symbol>)languageType;
//M: The selector is used to set argument types.  Some languages won't
//M: have any, and so for those languages this need not be called.
- setSelector: (SEL)aSel;
- setJavaSignature: (const char *)javaSignature;
+ create: (id <Zone>)aZone setSelector: (SEL)aSel;
- addArgument: (types_t *)value ofType: (fcall_type_t)type;
- addArgument: (void *)value ofObjCType: (char)type;
- addChar: (char)value;
- addBoolean: (BOOL)value;
- addUnsignedChar: (unsigned char)value;
- addShort: (short)value;
- addUnsignedShort: (unsigned short)value;
- addInt: (int)value;
- addUnsigned: (unsigned)value;
- addLong: (long)value;
- addUnsignedLong: (unsigned long)value;
- addLongLong: (long long)value;
- addUnsignedLongLong: (unsigned long long)value;
- addFloat: (float)value;
- addDouble: (double)value;
- addLongDouble: (long double)value;
- addString: (const char *)value;
- (void)addObject: obj;
- addSelector: (SEL)aSel;
- addJavaObject: (JOBJECT)obj;
- setObjCReturnType: (char)type;
- setReturnType: (fcall_type_t)retType;
- setBooleanReturnType;
USING
- (val_t)getRetVal;
- (id <Symbol>)getLanguage;
- (void *)getResult;
@end

typedef enum callTypes { ccall, COMcall, JScall, javacall, javastaticcall,  objccall } call_t;

@class FCall_c;

@protocol FCall <Create, Drop, CREATABLE>
//S: A language independent interface to dynamic calls.
//D: A language independent interface to dynamic calls.
CREATING
+ create: (id <Zone>)aZone target: obj
                         selector: (SEL)sel
                        arguments: (id <FArguments>)fa;
+ create: (id <Zone>)aZone target: obj
                       methodName: (const char *)methodName
                        arguments: (id <FArguments>)fa;
- setArguments: args;
- setFunctionPointer: (func_t)fn;
- setMethodFromSelector: (SEL)method inObject: object;
- setMethodFromName: (const char *)methodName inObject: object;
- setJavaMethodFromName: (const char *)methodName inObject: (JOBJECT)jObj;
- setJavaMethodFromName: (const char *)methodName inClass: (const char *)className;
USING
- (call_t)getCallType;
- getArguments;
- (void)performCall;
- (void *)getResult;
- (retval_t)getRetVal: (retval_t)retVal buf: (types_t *)buf;
- (func_t)getFunctionPointer;
extern void defobj_init_java_call_tables (void *jniEnv);
extern void updateTarget (FCall_c *self, id target);
@end

@protocol Serialization
//S: Object serialization protocol.
//D: Object serialization protocol.

CREATING
//M: Process keyword parameters in expression in order to get 
//M: create-time parameters.
- lispInCreate: expr;

//M: Process HDF5 object to set create-time parameters.
- hdf5InCreate: (id <HDF5>)hdf5Obj;

#ifdef __cplusplus
extern "C" {
#endif

//F: Load an object from a lisp expression of the form
//F: (make-{class,instance} #:arg1 x #:arg y). 
extern id lispIn (id <Zone> aZone, id expr);

//F: Load an object from a HDF5 object.
extern id hdf5In (id <Zone> aZone, id <HDF5> hdf5Obj);

#ifdef __cplusplus
}
#endif

SETTING
//M: Process an archived Lisp representation of object state from a
//M: list of instance variable name / value pairs.
- lispIn: expr;

//M: Load instance variables from an HDF5 object.
- hdf5In: (id <HDF5>)hdf5Obj;

USING
//M: Output a shallow Lisp representation of object state to a stream.
- (void)lispOutShallow: stream;

//M: Output a deep Lisp representation of object state to a stream.
- (void)lispOutDeep: stream;

//M: Output just key/variable pairs, where variables are serialized 
//M: deep or shallow per deepFlag.
- (void)lispOutVars: stream deep: (BOOL)deepFlag;

//M: On the given stream, save a Boolean variable called "aName" 
//M: which has value "val".
//M: Explanation: 
//M: The Swarm lisp serialization approach assumes that objects have
//M: lispOutDeep: and lispOutShallow: methods which indicate which
//M: variables are supposed to be saved.  If an object is 
//M: subclassed from SwarmObject, there are default lispOutDeep: 
//M: and lispOutShallow: methods. Those methods employ on the method,
//M: lispOutVars:deep:, which is the "default" approach to try to save
//M: all variables, either deep or shallow.  Sometimes one needs
//M: to selectively list particular instance variables to be saved.

//M: This is necessary, for example, if one wants to save a Swarm
//M: itself, because the usage of lispOutVars: will result in 
//M: a variable "activity" being saved as nil, and so when the
//M: saved values are read back in, the "activity" variable will be
//M: erased and nil will appear in its place.  

//M: Here is an example of how a subclass called "BFagent" 
//M: might override lispOutDeep: to customize 
//M: the selection of variables to be saved.  Note that the
//M: same could be used to override lispOutShallow:. 

//M: The key thing
//M: to remember is that when one tries to do a deep save on
//M: a high level object, such as a Swarm, then the Swarm
//M: libraries will try to track from top to bottom, finding
//M: all collections and objects, and all objects and collections
//M: inside them, and so forth, and each will be told to execute
//M: its lispOutDeep: method.  So all objects you want to save need
//M: a lispOutDeep: method, or else the default will try to save all
//M: variables.  If you omit some objects or variables from your
//M: lispOutDeep: method, then they will not appear in the saved
//M: file, which is what you want if you want to be sure that
//M: pre-existing interited values of variables are not obliterated 
//M: by bogus saved values.

//E: - (void)lispOutDeep: stream
//E: {
//E:  [stream catStartMakeInstance: "BFagent"];
//E:  [self lispSaveStream: stream Double: "demand" Value: demand];
//E:  [self lispSaveStream: stream Double: "profit" Value: profit];
//E:  [self lispSaveStream: stream Double: "wealth" Value: wealth];
//E:  [self lispSaveStream: stream Double: "position" Value: position];
//E:  [self lispSaveStream: stream Double: "cash" Value: cash];
//E:  [self lispSaveStream: stream Double: "price" Value: price];
//E:  [self lispSaveStream: stream Double: "dividend" Value: dividend];
//E:  [self lispSaveStream: stream Integer: "myID" Value: myID];
//E:  [stream catEndMakeInstance];
//E:
//E: }
//E: An example of such a usage can be found in version 2.4 of the
//E: Artificial Stock Market (http://ArtStkMkt.sourceforge.net). 
- (void)lispSaveStream: stream Boolean: (const char *)aName Value: (int)val;

//M: On the given stream, save a character variable called "aName"
//M: which has value "val".
- (void)lispSaveStream: stream Char: (const char *)aName Value: (char)val;

//M: On the given stream, save a short integer variable called "aName"
//M: which has value "val".
- (void)lispSaveStream: stream Short: (const char *)aName Value: (short)val;

//M: On the given stream, save an unsigned short integer variable
//M: called "aName" which has value "val".
- (void)lispSaveStream: stream UnsignedShort: (const char *)aName Value: (unsigned short)val;

//M: On the given stream, save an integer variable called "aName"
//M: which has value "val".
- (void)lispSaveStream: stream Integer: (const char *)aName Value: (int)val;

//M: On the given stream, save an unsigned integer variable called
//M: "aName" which has value "val".
- (void)lispSaveStream: stream Unsigned: (const char *) aName Value: (unsigned)val;

//M: On the given stream, save a long variables called "aName"
//M: which has value "val".
- (void)lispSaveStream: stream Long: (const char *)aName Value: (long)val;

//M: On the given stream, save an unsigned long variable
//M: called "aName" which has value "val".
- (void)lispSaveStream: stream UnsignedLong: (const char *)aName Value: (unsigned long)val;

//M: On the given stream, save a long long variable called
//M: "aName" which has value "val".
- (void)lispSaveStream: stream LongLong: (const char *)aName Value: (long long)val;

//M: On the given stream, save an unsigned long long variable called
//M: "aName" which has value "val".
- (void)lispSaveStream: stream UnsignedLongLong: (const char *) aName Value: (unsigned long long)val;

//M: On the given stream, save a float valued variable called "aName"
//M: which has value "val".
- (void)lispSaveStream: stream Float: (const char *) aName Value: (double)val;

//M: On the given stream, save a double valued variable called "aName"
//M: which has value "val".
- (void)lispSaveStream: stream Double: (const char *) aName Value: (double)val;


//M: For customized archiving of dynamically allocated arrays within
//M: objects. To use this method, override the object's lispOutDeep:
//M: method as follows. The array is assumed allocated in one long
//M: piece of memory, but it can be treated in segments to make it
//M: two dimensional.  The number of rows is "rank" and the length of
//M: the i'th row is dims[i], thus allowing for a ragged array.
//M: 
//M: In this example, Attribute is a class, culture is a
//M: dynamically allocated array of numDims integers, so rank is 1 and
//M: a pointer to numDims is passed through for the length of that row.

//E: - (void)lispOutDeep: stream
//E: {
//E:  [stream catStartMakeInstance: "Attribute"];
//E:  [super lispOutVars: stream deep: NO]; //saves all ints, doubles, BOOLs, and static arrays. Saves the values of all other objects as nil
//E:  // Note one can use the previous "lispSaveStream:..." methods to
//E:  // customize the choice of variables to be saved.
//E:  // Now save an array called "culture", which has 1 row 
//E:  // and "numDims" columns, onto the stream.
//E:  [super lispStoreIntegerArray: culture Keyword: "culture" Rank: 1 Dims: &numDims Stream: stream];
//E:  [stream catEndMakeInstance];
//E: }
- (void)lispStoreIntegerArray: (int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;

//M: Lisp save array of Booleans, see lispStoreIntegerArray:Keyword:Rank:Dims.
- (void)lispStoreBooleanArray: (BOOL *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;

//M: Lisp save array of characters, see lispStoreIntegerArray:Keyword:Rank:Dims.
- (void)lispStoreCharArray: (char *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;

//M: Lisp save array of short integers, see lispStoreIntegerArray:Keyword:Rank:Dims.
- (void)lispStoreShortArray: (short int *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;

//M: Lisp save array of unsigned integers, see lispStoreIntegerArray:Keyword:Rank:Dims.
- (void)lispStoreUnsignedArray: (unsigned *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;

//M: Lisp save array of long, see lispStoreIntegerArray:Keyword:Rank:Dims.
- (void)lispStoreLongArray: (long *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;

//M: Lisp save array of unsigned long, see lispStoreIntegerArray:Keyword:Rank:Dims.
- (void)lispStoreUnsignedLongArray: (unsigned long *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;

//M: Lisp save array of long long, see lispStoreIntegerArray:Keyword:Rank:Dims.
- (void)lispStoreLongLongArray: (long long *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;

//M: Lisp save array of unsigned long long, see lispStoreIntegerArray:Keyword:Rank:Dims.
- (void)lispStoreUnsignedLongLongArray: (unsigned long long *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;

//M: Lisp save array of floats, see lispStoreIntegerArray:Keyword:Rank:Dims.
- (void)lispStoreFloatArray: (float *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;

//M: Lisp save array of doubles, see lispStoreIntegerArray:Keyword:Rank:Dims.
- (void)lispStoreDoubleArray: (double *)ptr Keyword: (const char *)keyword Rank: (unsigned)rank Dims: (unsigned *)dims Stream: stream;


//M: Output a shallow HDF5 representation of object state to a stream.
- (void)hdf5OutShallow: (id <HDF5>)hdf5obj;

//M: Output a deep HDF5 representation of object state to a stream.
- (void)hdf5OutDeep: (id <HDF5>)hdf5obj;

- (void)updateArchiver: archiver;

#ifdef __cplusplus
extern "C" {
#endif
//F: Expect and convert a boolean from next index item.
extern BOOL lispInBoolean (id index);

//F: Expect and convert an integer from next index item.
extern int lispInInteger (id index);

//F: Expect and convert a string from next index item.
extern const char *lispInString (id index);

//F: Expect and convert a keyword from next index item.
extern id lispInKeyword (id index);

#ifdef __cplusplus
}
#endif

@end

//G: The singleton Arguments object.
externvar id <Arguments> arguments;

//G: The singleton HDF5 system Archiver object.
externvar id <HDF5Archiver> hdf5Archiver;

//G: The singleton Lisp system Archiver object.
externvar id <LispArchiver> lispArchiver;

//G: The singleton HDF5 application Archiver object.
externvar id <HDF5Archiver> hdf5AppArchiver;

//G: The singleton Lisp application Archiver object.
externvar id <LispArchiver> lispAppArchiver;

//G: Predefined type descriptors for allocated blocks.
externvar id <Symbol> t_ByteArray, t_LeafObject, t_PopulationObject;

//G: Language tags (e.g. for use in FArguments)
externvar id <Symbol> LanguageCOM, LanguageJS, LanguageJava, LanguageObjc;

//#: Abbreviation for @selector().
#define M(messageName) @selector (messageName)

//F: Function to generate object id string in standard format
//F: (Up to 78 characters of the supplied buffer argument could be filled.)
extern void _obj_formatIDString (char *buffer, id anObject);

//F: Lookup a defobj type object by name.
extern id defobj_lookup_type (const char *name);

//
// type objects generated for module
//
#if SWARM_OPENSTEP
#import <Swarm/defobj_types.h>
#else
#import <defobj/defobj_types.h>
#endif

extern void initDefobj (id <Arguments> arguments);

//F: internal module initialization function
extern void _obj_initModule (void *module);

//#: module initialization macro
#define initModule(module) _obj_initModule(_##module##_)

//G: internal variable for globalZone macro
externvar id _obj_globalZone;   

//G: internal variable for scratchZone macro
externvar id _obj_scratchZone; 

//#: A zone for allocating global objects.
#define globalZone _obj_globalZone

//#: A zone for allocating temporary objects.
#define scratchZone _obj_scratchZone

#ifndef _obj_debug
//G: if true then perform all debug error checking
externvar BOOL _obj_debug;   
#endif

//G: output file for error messages
externvar FILE *_obj_xerror;  
//G: output file for debugging messages   
externvar FILE *_obj_xdebug;  

//F: Set the display name.
extern void xsetname (id anObject, const char *name); 

//F: Print description of object on debug output stream.
extern void xprint (id anObject);                

//F: Print only the id string for an object on debug output stream.
extern void xprintid (id anObject);              

//F: Print description for each member of a collection on debug output stream.
extern void xfprint (id anObject);

//F: Print id for each member of a collection on debug output stream.
extern void xfprintid (id anObject);  

//F: Debug function to perform message on an object.
extern void xexec (id anObject, const char *name);

//F: Debug function to perform message on each member of a collection.
extern void xfexec (id anObject, const char *name); 

//F: Get an object from textual pointer description.
extern id nameToObject (const char *name);

//#: Name to use for Lisp archiving class-creation function
#define MAKE_CLASS_FUNCTION_NAME "make-class"

//#: Name to use for Lisp archiving object-creation function
#define MAKE_INSTANCE_FUNCTION_NAME "make-instance"

//#: Name to use for Lisp archive custom-parse function
#define PARSE_FUNCTION_NAME "parse"

//#: Conservative approximation of the number of decimal digits for a
//#: object of a given type, not including terminator. 
//#: signchar + roundup (log (10)/log(2) = 3.3219).
#define DSIZE(type) (1 + sizeof (type) * 8 / 3 + 1)

extern char *zstrdup (id <Zone> aZone, const char *str);

#define ZSTRDUP(aZone, str) zstrdup (aZone, str)

#define SSTRDUP(str) ZSTRDUP(scratchZone, str)

#define GSTRDUP(str) ZSTRDUP(globalZone, str)

#define OSTRDUP(obj, str) ZSTRDUP([obj getZone], str)

#define STRDUP(str) OSTRDUP(self, str)

#define ZFREEBLOCK(aZone, block) [aZone free: (void *) (block)]

#define SFREEBLOCK(block) ZFREEBLOCK (scratchZone, block)

#define OFREEBLOCK(obj, block) ZFREEBLOCK ([obj getZone], block)

#define FREEBLOCK(block) OFREEBLOCK(self, block)
#endif
#endif
