// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         collections.h
Description:  standard collection types   
Library:      collections
*/

#import <defobj.h>

//S: Standard collection types

//D: The collections library follows the library interface conventions of
//D: the defobj library.  It also depends on standard supertypes and
//D: classes defined by this library.  Initialization of the collections
//D: library automatically initializes the defobj library as well.  Since
//D: defobj also requires the collections library, both must always be
//D: linked into an application together.

@deftype MemberType
//S: Accessors for member type.

//D: The default member type of any collection is the id type defined by
//D: Objective C.  All messages which add, remove, or access members
//D: declare their member value with the id type.  None of the collection
//D: types, however, requires that the member value actually point to a
//D: valid object.  The member values of any collection may contain an
//D: arbritrary bit pattern, provided these bits fit in the space available
//D: within an id pointer.

//D: In the few cases when default operations assume that a member points
//D: to a valid object, there are alternate ways to avoid these defaults
//D: and use an arbritrary bit pattern as a member value instead.  When
//D: using an arbritrary bit pattern, the member value must be cast to or
//D: from the id type when adding or retrieving it from the collection.  By
//D: global portability assumptions, the number of bits in an id value
//D: may be safely assumed to be at least as large as an int
//D: (signed or unsigned), but not necessarily as large as a long.

//D: There is also an option to override the default member type so that it
//D: can be larger than a id pointer.  Use of this option, however, changes
//D: the way member values must be passed to or from a collection.  More
//D: information is given with the MemberType option below.

// (.. This option is not currently implemented for any collection types.)

//M: The MemberType option may be used to declare the type of member which
//M: a collection contains.  Its value must be an objec having one of the
//M: ValueType types defined in defobj.  (..Currently no ValueType objects
//M: are implemented, so MemberType is not supported.)

//M: The member type defaults to the Symbol value Unknown.  Specifying a
//M: member type has no effect on the collection if the storage size of the
//M: data type is no larger than a pointer of id type.  The member type,
//M: however, may be helpful as documentation, and members can be checked
//M: for conformance to the type as part of debug options.

//M: If the data type has a size greater than id, the collection
//M: automatically allocates and frees space for the member value whenever
//M: member values are added or removed.  For such a collection, all member
//M: values must be passed to the collection not as the value itself, but
//M: as a pointer to the value (cast to id as necessary).  If a new member
//M: is being added, the value will be copied to a new member location
//M: contained within the collection.  Member values returned by the
//M: collection will be returned as pointers to this internal value, not as
//M: the value itself.  In the case of member values removed from the
//M: collection, a nil pointer is returned.
CREATING
- (void)setMemberType: aDataType;
USING
- getMemberType;
@end

@deftype IndexSafety
//S: Interface for defining the degree of Index safety desired.

// (.. This option is not currently implemented for any collection types.)

//D: If not specified, the default value depends on the specific collection
//D: subtype.  In typical cases, the default value is Unsafe.  A non-default
//D: value is required for indexes to have a guarantee of safety when
//D: independent modifications to the underlying collection are also
//D: performed.

//D: Without a guarantee of index safety, the effect of any operation which
//D: modifies the underlying collection (adding, removing, or replacing
//D: members) could be to disrupt the position assumed by any existing
//D: index.  The effect of such disruption is, in general, completely
//D: unpredictable: it could be an immediate crash, a wrong sequence of
//D: members, or other difficult-to-debug conditions.  To avoid such
//D: unpredictability, several means for assuring the safety of indexes
//D: under independent collection update are provided.

//D: The simplest way to guarantee index safety is to set the IndexSafety
//D: option to the value SafeAlways when a collection is created.  With the
//D: value of SafeAlways, any active index on the collection will be
//D: updated automatically to reflect any change to the underlying
//D: collection whenever such a change occurs.  This automatic update
//D: continues to occur throughout the lifetime of any index on the
//D: collection; it ceases to occur only after an index is dropped.  This
//D: is the easiest option to use, but consumes extra resources to
//D: accomplish (both time and space within the collection), so is most
//D: suitable when the extra resource usage deesn't matter or there's no
//D: better way.

//D: With an IndexSafety value of SafeAlways, there is also an additional
//D: option to declare an external index update handler.  The handler is
//D: specified using the setIndexUpdateHandler:: message.  If an
//D: IndexUpdateHandler is specified, the value for IndexSafety is also
//D: automatically set to SafeAlways.

//D: The first argument of setIndexUpdateHandler:: specifies a function
//D: which is called whenever a modification to the underlying collection
//D: occurs which could require an update to existing indexes.  The second
//D: argument establishes a pointer value which the handler function may
//D: use to identify the indexes which need to be updated, or any other
//D: information needed for the update.

//D: (.. Exact call conventions for the IndexUpdateHandler are still being
//D: established, but the activity library will soon be converted to use
//D: this technique, so the conventions will be resolved during that
//D: conversion.)

//D: Besides the extreme values of Unsafe and SafeAlways, another valid
//D: value for IndexSafety is UnsafeAtMember.  This value indicates that
//D: the only modification of the collection which could disrupt existing
//D: indexes is to remove the member contained at the current index
//D: position, or to change the member on either side of an index
//D: positioned between members.  For some collection subtypes, this level
//D: of safety is automatically assured, and for others it can be requested
//D: with less overall resource impact than SafeAlways.
CREATING
- (void)setIndexSafety: indexSafety;
- (void)setIndexHandler: (fixup_t)fixupHandler
           withArgument: (void *)arg;

USING
- getIndexSafety;
- (fixup_t)getIndexHandler: (void **)arg;
@end

@deftype Offsets 
//S: Methods for accessing collection members by position.

//D: An offset is an integer value that gives relative position of a member
//D: in the enumeration sequence of a collection.  Offsets start the count
//D: of the first member at zero, just like C array indexing.

//D: Offsets provide an alternate means to access the members of a
//D: collection, without creating a separate index object.  Some collection
//D: subtypes (such as Array) support fast, direct access by integer member
//D: offset, while others support member offsets only as a shorthand for
//D: sequential access through every preceding member.  Access by offsets
//D: is supported on all collections regardless of whether its speed on a
//D: particular collection type.

//D: atOffset: and atOffset:put: raise the error OffsetOutOfRange if the
//D: offset is less than zero or greater than or equal to the count of
//D: members in the collection.

USING
//M: Returns the member at a particular member offset.
- atOffset: (int)offset;

//M: The atOffset: put: message replaces the member at a particular offset
//M: with a new value, and returns the previous member value at this offset.
- atOffset: (int)offset put: anObject;

//M: Equivalent to [aCollection atOffset: 0].
- getFirst;

//M: Equivalent to [aCollection atOffset: [aCollection getCount] - 1].
- getLast;
@end

@deftype ForEach
//S: Messages for performing the same message on objects in a collection.

//D: The forEach messages supply a convenient shorthand for repeatedly
//D: performing the same message on all objects contained as members in a
//D: collection.  The message to be sent is identified by the argument
//D: aSelector.  This selector must define the same number of arguments as
//D: contained in any remaining argument slots of the forEach message.  The
//D: argument types of the message to be sent must be either the id type,
//D: or some other type that will fit in the same number of bits as the id
//D: type.  By global portability assumptions, the argument type could be
//D: as large as an int (signed or unsigned), but not necessarily as large
//D: as a long.  To use the message, any non-id value must be cast to the
//D: id type as part of the call expression.

//D: The forEach: messages are implemented by a simple loop through all
//D: members of a collection, using an internal, temporary index.  If any
//D: operation more complex than a simple message send is required, this
//D: operation should just be coded directly using a loop that traverses
//D: its own index.

USING
- (void)forEach: (SEL)aSelector;
- (void)forEach: (SEL)aSelector : arg1;
- (void)forEach: (SEL)aSelector : arg1 : arg2;
- (void)forEach: (SEL)aSelector : arg1 : arg2 : arg3;

- (void)describeForEach: outputCharStream;
- (void)describeForEachID: outputCharStream;

@end


@deftype Collection <Create, SetInitialValue, Copy, Drop, MemberType, Offsets, ForEach>
//S: A generic collection interface.

//D: A collection is a grouping of object references or other data values which
//D: are assigned explicitly as members of the collection.  Depending on the
//D: subtype, collection members may also be maintained in various associations
//D: with each other, such as an ordering of members or an association of
//D: members with identifying key values.  Major Collection subtypes include
//D: Array, List, Set and Map.  The Collection supertype establishes common
//D: conventions (and associated messages) supported by all types of
//D: collections.

//D: All collections support traversal over their members using a separate
//D: object called an Index.  All collections also follow common rules
//D: regarding the types of data values which may be added as members.  The
//D: next two subsections summarize these basic features of all collections.

//D: An index is a special type of object that references a current
//D: position in an enumeration sequence over a collection.  An enumeration
//D: sequence contains every member of a collection exactly once.  Every
//D: collection defines an associated type of object for its index.  The
//D: index type of a collection defines additional ways in which the
//D: members of a collection may be processed beyond any messages available
//D: directly on the collection.  Often the operations of an index provide
//D: the most basic and flexible means for manipulating individual members
//D: of a collection.

//D: An index object into a collection may be created at any time.  An
//D: index is the basic means to traverse through all members of a
//D: collection.  Multiple indexes on the same collection may all exist at
//D: the same time and may reference the same or different positions.
//D: Depending on the collection type, it may be possible to modify a
//D: collection through its indexes.

//D: Once an index is created, the sequence of members in its enumeration
//D: sequence is guaranteed to remain the same, provided that no new
//D: members are added to the underlying collection, or existing members
//D: removed.  If a member is located once at a particular position, it is
//D: guaranteed to remain at that position as long as the index itself
//D: remains.

//D: Many collection types define an explicit ordering over their members.
//D: For such collections, the sequence of members referred to by an index
//D: will always be consistent with this ordering.  An explicit, total
//D: ordering also guarantees that all indexes of the same collection have
//D: the same member sequence.

//D: If no such ordering is defined, however, some particular sequence of
//D: all the collection still becomes associated with each created index.
//D: All collection members are guaranteed to be contained somewhere in the
//D: enumeration sequence for any particular index, but two indexes on the
//D: same collection are not guaranteed to have the same sequence.

//D: The Index type corresponds to the iterator types defined as part of
//D: many other object-oriented libraries.  The name Index is shorter and
//D: emphasizes the more abstract and multi-function role of these basic
//D: support objects for any collection.  For more background on design of
//D: indexes and iterators, see the Interface Design Notes for the
//D: collections library.

CREATING

//M: This boolean-valued option restricts valid usage of a collection by
//M: excluding all operations which add or remove members.  For some
//M: collection subtypes, a replace-only restriction can obtain many of the
//M: same performance advantages as a read-only collection, but without
//M: disabling replace operations as well.  Just like the ReadOnly option,
//M: the ReplaceOnly option may be reset after a collection is created,
//M: provided it was not originally set to true.
- (void)setReplaceOnly: (BOOL)replaceOnly;

- (void)setIndexFromMemberLoc: (int)byteOffset;

USING
- (BOOL)getReplaceOnly;

//M: getCount returns the integer number of members currently contained in
//M: the collection.  All collections maintain their count internally so
//M: that no traversal of collection members is required simply to return
//M: this value.
- (int)getCount;

//M: The contains: message returns true if the collection contains any
//M: member value which matches the value passed as its argument.
//M: Depending on the collection subtype, this may require traversing
//M: sequentially through all members of the collection until a matching
//M: member is found.  For other subtypes, some form of direct indexing
//M: from the member value may be supported.  The message is supported
//M: regardless of its speed.
- (BOOL)contains: aMember;

//M: The remove: message removes the first member in the collection with a
//M: value matching the value passed as its argument.  If there is no such
//M: member, a nil value is returned.  As with the contains: message, the
//M: speed of this operation may vary from very low to linear in the number
//M: of members, depending on the collection subtype.
- remove: aMember;

//M: The removeAll message removes all existing members of a collection and
//M: sets its member count to zero.  The collection then remains valid for
//M: further members to be added.  This message has no effect on the
//M: objects which might be referenced by any removed member values.  If
//M: resources consumed by these objects also need to be released, such
//M: release operations (such as drop messages) can be performed prior to
//M: removing the member values.
- (void)removeAll;

//M: Like removeAll:, but drops the member(s) as well.
- (void)deleteAll;

//M: The begin: message is the standard method for creating a new index for
//M: traversing the elements of a collection.  All further information
//M: about indexes is documented under the Index type.
- begin: aZone;
@end

//G: values for index safety
extern id <Symbol> Unsafe, UnsafeAtMember, SafeAlways;


@deftype Index <DefinedObject, Copy, Drop>
//S: Reference into the enumeration sequence for a collection.

//D: An index is a reference into an enumeration sequence of a collection.
//D: Such an enumeration sequence contains all members of the collection in
//D: some order.  This order will always be consistent with ordering of
//D: members in the collection, assuming there is such an ordering.
//D: Otherwise, the sequence will still contain all members in some order
//D: that remains fixed provided that new members are not added or removed
//D: from the collection.

//D: An index is created by a begin: or createIndex: message against a
//D: collection.  Each major collection type has its own corresponding
//D: index type, which supports specialized types of processing against
//D: the valid contents of that kind of collection.  Once created, an
//D: index is a separate object from the collection itself, but it remains
//D: valid only so long as the collection itself still exists.  Multiple
//D: indexes may exist at the same time against the same collection, and
//D: each index maintains its own position within an enumeration sequence
//D: for the collection.

//D: Many indexes provde the ability modify the collection they refer
//D: to, in addition to simply traversing members.  An index often provides
//D: the complete means for maintaining the contents of a collection, more than
//D: could otherwise be performed on the collection itself.  The position or
//D: other status of the index is automatically updated to reflect any changes
//D: made through the index itself.

//D: If changes to a collection are made while other indexes exist, those
//D: other indexes could be affected in potentially catastrophic ways.
//D: Each collection type documents which kinds of changes can be made
//D: without affecting or invalidating existing indexes.  The IndexSafety
//D: option of Collection provides additional ways to protect indexes
//D: against possible effects of independent updates.

//D: Each index is a stand-alone object allocated within a zone passed as
//D: an argument in the message that created it.  This zone need not match
//D: the zone of a collection.  It is common for index lifetimes to be
//D: shorter than their collection.  For example, indexes can be created in
//D: a temporary scratch zone for use only within a local loop.

//D: Because messages to a collection are the only valid way to create an
//D: index, create messages and create-time options are not used with index
//D: types.  All valid processing on an index is determined by
//D: characteristics of the collection from which it is created.  Index
//D: types are typically named after the type of collection they are
//D: created from, and serve principally to define the specific messages
//D: valid for an index on that type of collection.

//D: Index objects support the universal messages of the top-level
//D: DefinedObject supertype, along with the standard drop: and getZone
//D: messages.  Even though they cannot be created except from a
//D: collection, new index objects can be created from an existing index
//D: using the standard copy: message.  Each copy refers to the same
//D: collection as the initial index, and starts at the same position in
//D: its enumeration sequence.  In all other respects, however, the new
//D: copy is an independent index that maintains its own position under any
//D: further processing.

USING
//M: getCollection returns the collection referred to by an index.  This
//M: collection never changes during the lifetime of the index.
- getCollection;

//M: The next message positions the index to the next valid member
//M: after its current position, or to a special position after the
//M: end of all valid members.  In addition to
//M: repositioning the index, both messages return the new member value to
//M: which they are positioned, or nil if there is no such member.
- next;

//M: The prev message works similarly,
//M: but positions to a valid member preceding the current position, or to
//M: a special position preceding all valid members. 
- prev;

//M: findNext: repeatedly performs next until the member value of the
//M: index matches the argument.  nil is returned if the index reaches
//M: the end of valid members without matching the argument.  
- findNext: anObject;

//M: findPrev: repeatedly performs prev until the member value of the
//M: index matches the argument.  nil is returned if the index reaches
//M: the end of valid members without matching the argument. 
- findPrev: anObject;

//M: get returns the member value at which the index is currently
//M: positioned, or nil if the index is not positioned at a member.

//M: The get message provides an alternate way to obtain the current member
//M: value in a loop that traverses a collection; its return value is the
//M: same as next or prev would return when first positioning to a new
//M: member.
- get;

//M: The put: message replaces the member value at the current index
//M: position with its argument.  An InvalidIndexLoc error is raised if
//M: the index is not positioned at a current member.
- put: anObject;

//M: The remove message removes the member at the current location of an
//M: index, and returns the member value removed.  The index position is
//M: set to a special position between the members which previously
//M: preceded and followed the removed member.  If there is no preceding or
//M: following member, the index is set to the special location before the
//M: start or after the end of all members.  After a current member is
//M: removed, there is no member at the current index location, but a
//M: subsequent next or prev message will continue with the same member
//M: that would have been accessed had the current member not been removed.
//M: An InvalidIndexLoc error is raised if the index is not positioned
//M: at a current member.
- remove;

//M: The getLoc message returns a symbol constant which indicates the type
//M: of location at which an index is currently positioned.  This index
//M: location symbol has one of the following values: Start, End, 
#if 0
//M: Between,
#endif
//M: and Member.

//M: The Start symbol indicates the special position preceding all members
//M: in the enumeration sequence for the collection.  This is the location
//M: at which an index is positioned when it is first created.  The End
//M: symbol indicates the special position following all members in the
//M: collection.  This is the location at which an index is positioned just
//M: after a next message has returned nil, as a result of moving beyond
//M: the last member.   The Member symbol indicates that the index
//M: is positioned at some current member in the enumeration sequence
//M: of a collection.
#if 0
//M: The Between symbol indicates the special position
//M: between two other members, at which an index is positioned after a
//M: current member, between two other members, is removed. 
#endif

//M: The getLoc message is needed to traverse a collection which could
//M: contain nil values for its members.  Without getLoc, there would be no
//M: way to distinguish a nil value returned by next as either a valid
//M: member value or the special value returned at the end of members.
//M: With getLoc, a loop that traverses a collection can test specifically
//M: for the end (or start) of members.

//E: Following is a simple loop which illustrates such usage:

//E: index = [aCollection begin: aZone];
//E:  do {
//E:    member = [index next];
//E:    // do something with member ...
//E:  } while ( [index getLoc] == Member );
- getLoc;

//M: The setLoc: message may be used to reset the current location of an
//M: index to either Start or End.  It may be used to reprocess a
//M: collection using an existing index after some other location has
//M: already been reached.  It may also be used to position an index at the
//M: end of all members prior to traversing members in reverse order using
//M: prev.

//M: Besides Start and End, setLoc: accepts the special argument values of
//M: BetweenAfter and BetweenBefore, which are also defined symbols.  These
//M: argument values are only valid if the index is positioned at a current
//M: member.  They reposition the index to the special location between the
//M: current member and its immediately following or preceding member.
- (void)setLoc: locSymbol;

//M: Provided there is no major computational cost, an index also maintains
//M: the integer offset of its current member within the enumeration
//M: sequence of the collection.  These integer offset values have the same
//M: definition as in the atOffset: messages of Collection.  The getOffset
//M: message returns this current offset value.  If the index is current
//M: positioned at the Start or End location, getOffset returns -1.  If the
//M: index is positioned at a Between location, getOffset returns the
//M: offset of the immediately preceding member.  If the offset is not
//M: currently available from the index, getOffset returns the special
//M: value UnknownOffset.  This value is defined by a macro as the
//M: maximally negative value of a 32-bit, 2's-complement integer.

//M: An offset is always available from an index if its current position
//M: has been reached by repeated operations from an initial Start or End
//M: position, and there has been no other modification to the underlying
//M: collection.  Some forms of direct member access operations supported
//M: by some index types, however, may result in an integer offset not
//M: being available.  These restrictions are noted with the individual
//M: index type.
- (int)getOffset;

//M: Using the setOffset: message, an index may be positioned directly to a
//M: member using the offset of the member within its enumeration sequence.
//M: The speed of this operation depends on the specific type of the
//M: collection, just as noted for the atOffset: message on Collection.  In
//M: the worst case, this operation is linear in the magnitude of the
//M: offset.
- setOffset: (int)offset;

//M: The compare: message compares the current location of one index with
//M: the current location of another index passed as its argument.  If the
//M: two indexes have the same location, compare: returns zero.  Otherwise,
//M: compare: returns +1 or -1 according to whether the argument index
//M: precedes or follows the receiver index in the enumeration sequence of
//M: the collection.  If either of the two indexes has an unknown offset,
//M: and the location of the other index is anything other than Start or
//M: End or an immediately adjacent member, compare: returns the
//M: UnknownOffset integer value.
- (int)compare: anIndex;

//G: values for index location
extern id <Symbol>  Start, End, Between, Removed, Member;

//
// return value for index offset if not defined
//
#define UndefinedOffset -0x80000000;

//G: error types for collections
extern id <Error> OffsetOutOfRange, NoMembers, 
  AlreadyAtEnd, AlreadyAtStart, InvalidIndexLoc, InvalidLocSymbol;

@end


@deftype DefaultMember
//S: Methods for setting and getting the default member in a collection.

//D: When this option is set, the initial value of all new members will be
//D: set to the member value given (otherwise the default is nil).  This
//D: option gives a convenient way to distinguish members which have never
//D: been set from any other valid member value, which could include nil.
//D: This option may be reset after array creation only if some setting for
//D: the option was given at create time.  (The initial, explicitly set
//D: value can still be the default nil, but a value must be set explicitly
//D: for the option to be resettable later).  The get message for this
//D: option always retrieves the current setting, but this value has no
//D: effect except when the count of an array is increased, so that new
//D: members need to be initialized.
CREATING
- (void)setDefaultMember: memberValue;

SETTING
- (void)setDefaultMember: memberValue;

USING
- getDefaultMember;
@end

@deftype MemberBlock
//S: A way to wrap an existing C array for access as an object collection.

//D: This option provides a means to wrap an existing C array for access as
//D: an object collection.  If this option is given, dynamic resizing is
//D: not supported.  The current C array being wrapped, however, can be
//D: replaced by giving a new setting for MemberBlock after an array
//D: already exists.  A new setting can be given only if some setting was
//D: initially given at create time.

//D: Even if a setting was not given for MemberBlock at create time, the
//D: get message for MemberBlock always returns a pointer to whatever
//D: internal memory array is currently being used by the array object.  If
//D: a MemberBlock setting was given, the pointer returned will be the same
//D: as the one previously given.  In either case, the pointer returned may
//D: be used to manipulate member values in any way desired using native C
//D: expressions.  After an array has been created, there is no way to
//D: determine whether the MemberBlock pointer was established externally
//D: or by internal allocation.  If the pointer was established by internal
//D: allocation, however, the external program must make no attempt to free
//D: or otherwise reallocate this memory.

//D: A count must always be supplied with an external member allocation,
//D: using the setCount: argument of the compound message.  If an external
//D: allocation is being used, the only way to reset the count is also to
//D: reset MemberBlock; any attempt to use the setCount: message by itself
//D: will raise an error.  Whenever an external member allocation is being
//D: used, the external program is entirely responsible for assuring that
//D: the MemberBlock value is a pointer to valid allocated memory
//D: containing at least the number of member slots given by setCount:.

//D: With an external member allocation, the array itself will not attempt
//D: to either allocate this memory or free it when the array is dropped.
//D: Dropping the array only removes its reference to the external
//D: allocation.

//D: The pointer to allocated memory is given as a pointer to id type, but
//D: if a data type given as MemberType has size greater than id, the
//D: pointer must actually point to an array of slots each of which has the
//D: size of the data type.

//D: Since an array neither allocates nor frees an external member
//D: allocation, the same region of allocated memory may be referenced by
//D: multiple arrays, including overlapping member ranges each defined by
//D: starting location and count.  This flexibility enables alternate
//D: subrange views of a single, contiguous initial allocation by means of
//D: separately created external collections.
CREATING
+ create: aZone setMemberBlock: (id *)members setCount:(int)count;

SETTING
- (void)setMemberBlock: (id *)members setCount: (int)count;
- (void)setMemberAlloc: (id *)members setCount: (int)count;

USING
- (void *)getMemberBlock;
- (void *)getMemberAlloc;
@end

@deftype Array <Collection, CREATABLE, DefaultMember, MemberBlock>
//S: Collection supporting access only by relative position.

//D: An array is a collection of members that are all created as members of
//D: the collection at the same time.  Existing member values may be
//D: replaced with new values, but the members themselves are fixed at
//D: constant offsets within the collection.  The fixed structure of an
//D: array permits very fast access to members by integer offset positions,
//D: since the location of each member may be directly calculated.

//D: The Array type is one of the simplest collection types in the
//D: collections library, and the closest to a data structure directly
//D: supported in C.  Unlike C arrays, the group of members belonging to
//D: the array is not necessarily fixed for the lifetime of the array, but
//D: may be dynamically resized to contain a different number of members.
//D: When an array is dynamically resized, existing member values are
//D: preserved as much as possible.

//D: The Array type adds few messages to the generic messages inherited
//D: from Collection.  This type is provided partly so that a
//D: fixed-structure array can be accessed with the same uniform set of
//D: basic messages as any other kind of object collection.  It also
//D: handles all required memory allocation within the collection.  As an
//D: option, however, the Array type can be used to wrap an existing C
//D: array for external access as an object collection.  It can also
//D: provide access to an internal C array for direct manipulation using C
//D: expressions.  These forms of low-level access support hybrid modes of
//D: use in which advantages of both low-level manipulation and external
//D: object access can be combined.

//D: The Array type is directly creatable, and supports all standard
//D: messages of Collection except removal of individual members.  The
//D: messages based on an integer offset, either on the collection
//D: (atOffset:, atOffset:put:), or an index (setOffset:) all execute in
//D: fast constant time.  Members of an array are fully ordered according
//D: to these integer offsets.  Sequential access to members through its
//D: members is also fully supported.  The Array type disables the remove
//D: message inherited from Collection; the message is defined, but any
//D: attempt to remove a member will raise an error that the operation is
//D: not supported.

//D: While the IndexSafety option of an Array defaults to Unsafe, indexes
//D: are inherently safe under all operations with the exception of
//D: resizing an array or resetting its member allocation.  (The only
//D: individual member operation which modifies contents of an array is
//D: replacing a member value, and this does not affect existing indexes.)
//D: The UnsafeAtMember value of IndexSafety is not supported since
//D: individual members cannot be added or removed.  A value of SafeAlways
//D: makes existing indexes safe even under resizing or reallocation.  If
//D: an existing index referred to a member location that is no longer
//D: contained in the array, the index location is reset to the End
//D: location.

//D: The default value of the ReplaceOnly option is true, and cannot be
//D: overridden.

//D: The type of index returned by begin: on an array is simply Index.
//D: There is no special index type for Array because there are no
//D: additional messages beyond those already defined by Index.

//D: All the Array create-time options can also be set after the array is
//D: already created, subject to restrictions noted below.

CREATING
+ create: aZone setCount: (int)count;

SETTING
//M: The Count option sets the number of members which belong to the
//M: collection.  Any non-negative value including zero is valid.  If the
//M: array already exists, the any existing members up to the new count
//M: will preserve their existing values.  If the new count is greater than
//M: the existing count, or a new array is being created, all members will
//M: be assigned an initial default value of either nil, or a value
//M: previously specified for DefaultMember.
- (void)setCount: (int)count;
@end


@deftype EndsOnly
//S: Restricts a list so that members can be added or removed only
//S: at either end.

//D: The EndsOnly option restricts a list so that members can be added or
//D: removed only at either end.  This restriction permits an alternate
//D: implementation that both reduces memory usage and enables fast, direct
//D: access to members by integer offset.  It obtains key performance
//D: advantages of an array while preserving the dynamic extendability of a
//D: list.
CREATING
- (void)setEndsOnly: (int)countPerBlock;

USING
- (int)getEndsOnly;
@end

@deftype List <Collection, CREATABLE, EndsOnly>
//S:  Collection of members in an externally assigned linear sequence.

//D: A list is a collection of members that are all maintained at some
//D: externally assigned position in a linear sequence of all members.  The
//D: sequence is established by the position at which members are added:
//D: members can be added at the start of list, at the end, or at any point
//D: in the middle.

//D: A list is also one of the most dynamic of basic collections, in that
//D: the cost of adding and removing members is very low.  Members can be
//D: removed from any position just as easily as they can be added.  A list
//D: automatically grows and shrinks to reflect the number of members at
//D: any one time, and there is no fixed capacity which limits the size to
//D: which a list may grow.

//D: The List type is supports all messages of Collection.  If created with
//D: default options, it provides no special speedup of accesses by integer
//D: offset.

//D: Unless the EndsOnly option is specified, the default value of
//D: IndexSafety is UnsafeAtMember.  This means that indexes are inherently
//D: safe under all operations except those which affect the member at an
//D: index or immediately adjacent to an index between members.  If the
//D: EndsOnly option is specified, the default value of IndexSafety is
//D: Unsafe.  In this case, indexes could be invalidated by any operation
//D: that adds or removes members.  The SafeAlways can be specified for any
//D: case to get a guarantee of full index safety.

CREATING
//M: The addFirst: message adds a new member to the front of the list.
- (void)addFirst: anObject;

//M: The addLast: message adds a new member to the end of the list.
- (void)addLast: anObject;

//M: Removes the first member from the list and returns it.
- removeFirst;

//M: Removes the last member from the list and returns it.
- removeLast;
@end

@deftype ListIndex <Index>
//S: Index with insertion capability at any point in list.

//D: The addAfter: and addBefore: messages add members at a particular
//D: point in the sequence of members maintained by a list.  The current
//D: location of an index determines the point at which a new member will
//D: be added.  The addAfter: message adds a member at the list position
//D: immediately following the current index location.  addBefore: adds a
//D: member to the immediately preceding location.  Neither message changes
//D: the current location of the index, except that an index can change
//D: from a Start or End location to a location of Between.

//D: Since an index may be positioned to any location in a list, these
//D: messages enable the construction of any desired sequence of members.
//D: Since the current index location remains unchanged, multiple members
//D: may all be inserted successively at some point in a list; previously
//D: added members are just pushed out one-by-one as new members are added.

//D: An index with a location of Start, End, or Between is just as valid a
//D: location for addAfter: or addBefore: as an index positioned at a
//D: member.  In these cases, there is no member at the current location of
//D: the index, so the new member is just inserted directly at the current
//D: index location, and the index is left positioned between the new
//D: member and the member that was previously adjacent in the opposite
//D: direction.  If the previous location was Start and the message
//D: addAfter:, or the location was End and the message addBefore:, the
//D: index location remains Start or End.

//D: If either the addAfter: or addBefore: message is requested with the
//D: EndsOnly option, and the index location is not Start or End, or
//D: if remove is requested on an index that is not positioned at either the
//D: first or last member, an invalid operation error is raised.

USING
//M: Add a member after the index.
- (void)addAfter: anObject;

//M: Add a member before the index.
- (void)addBefore: anObject;
@end

@deftype Stack <List>
//S: Form of list with add/remove both at start of list.

//D: A Stack is a subtype of List which restricts the addition and removal
//D: of members to the start of a list.  

USING
//M: Push an item on the stack.
- (void)push: anObject;

//M: Pop an item off the stack, and return it.
- pop;
@end

@deftype Queue <List>
//S: Form of list with add at start of list and remove at last.

//D: Queue is a subtype of List which restricts addition of members to the
//D: start of the list and removal to the end.

USING
//M: Queue-up an item.
- (void)enqueue: anObject;

//M: Pull an item off the queue, and return it.
- dequeue;
@end


//F: A routine for comparing objects.
//F: Only useful for equality (EQ) discrimination.
extern int compareIDs (id, id);

//F: A routine for comparing integers.
extern int compareIntegers (id, id);

@deftype DupOption
//S: An interface for defining how duplicates should be handled.
//D: An interface for defining how duplicates should be handled.

CREATING
- (void)setDupOption: dupOption;
- (void)setDupMembersType: aCollectionType;

USING
- getDupOption;
- getDupMembersType;

//G: values for DupOption
extern id <Symbol> DupIsError, DupRejected, KeepAllDups, KeepCountOnly;
@end

@deftype Sorted
//S: An option that determines if a keyed collection is kept in order.

//D: If this option is true, the immediate members of a keyed collection
//D: (included collections of duplicate members, if any) are totally
//D: ordered according to an ordering relation defined on key values.  The
//D: default order for sorting is determined by a standard compare: message
//D: on a key value to be added vs. existing keys of the collection.  This
//D: default method to establish order can be overridden using the
//D: CompareFunction option.

//D: The default for the Sorted function is true, unless a value is also given
//D: for the option BucketFunction.

CREATING
- (void)setSorted: (BOOL)sorted;

USING
- (BOOL)getSorted;
@end

@deftype CompareFunction
//S: Interface for defining the compare function to use when comparing
//S: to members in a collection.

//D: Use the function pointed to by the argument as the method for
//D: determining whether two members have the same key value, and also for
//D: ordering the keys of the collection if the Sorted option is true. 

//D: The function given will be called whenever one key value needs to be
//D: compared with another.  Multiple calls to the function typically occur
//D: whenever members are added or removed from the collection, until the
//D: correct member for insertion or removal is determined.

//D: The compare function is called repeatedly by the collection to compare
//D: two key values.  The function should return zero if the key values are
//D: equal, -1 for the first key argument less than the second, and +1 for
//D: the first greater than the second.  If a keyed collection is not
//D: sorted, either -1 or +1 may be returned for unequal keys, regardless
//D: of whether one might be taken as greater or less than the other.

//T: The compare function accepts two key values as its arguments.  Like
//T: member types, key types declared with the id pointer type, but may
//T: contain any type of value (such as an integer) up to the size of an
//T: id.  An explicit compare function can support any type of value as a
//T: key, regardless of whether it is an object that supports a standard
//T: compare: message.
typedef int (*compare_t) (id, id);

CREATING
- (void)setCompareFunction: (compare_t)aFunction;

USING
- (compare_t)getCompareFunction;
@end

@deftype BucketFunction
//S: The BucketFunction option specifies a function that is called whenever
//S: a member of an unsorted collection is added or removed.

//D: The BucketFunction option specifies a function that is called whenever
//D: a member of an unsorted collection is added or removed.  It may also
//D: be used for a sorted collection to provide faster insertion and
//D: removal of members.

//D: The responsibility of a "bucket function" is to assign the key value
//D: to some integer in a range from zero up to some maximum value.  These
//D: integers identify "buckets" to which the same key value will also be
//D: assigned whenever passed to the function.  If the collection is
//D: unsorted, these buckets represent hash buckets, and for speed should
//D: be relatively sparse (few keys likely in any one bucket, given the
//D: expected size of the collection).  If the collection is sorted, these
//D: buckets represent ordered buckets to which the same range of key
//D: values will always be assigned.  Such sorted buckets can speed up the
//D: search for a key location within a sorted collection.  Sorted buckets
//D: can also be sparse to minimize the need for further sorting within
//D: each bucket.  If many keys end up in the same sorted bucket, the
//D: sorting process normally used without buckets is still used, so there
//D: is less performance impact than with hash keys of an unsorted
//D: collection.

//D: A new bucket function may be established any time during the lifetime
//D: of a collection.  A new bucket function can ensure that keys remain
//D: suitably distributed to buckets as the total number of keys in a
//D: collection grows or shrinks.

//T: A bucket function accepts a single key value as its argument, and
//T: return an integer that identifies a bucket.  As with a compare
//T: function, the key value is declared as an id pointer type, but may
//T: actually contain whatever kind of value is used within the collection.
typedef int (*bucket_t) (id);

CREATING
- (void)setBucketFunction: (bucket_t)aFunction setMaxBuckets: (int)max;
USING
- (bucket_t)getBucketFunction;

//D: A value for MaxBuckets must be given in the same message that sets a
//D: bucket function.  Its value specifies the number of buckets to which a
//D: bucket function assigns its keys.  The maximum value returned by a
//D: bucket function should be one less than this value.
- (int)getMaxBuckets;
@end

@deftype PartiallyOrdered
//S: Specifies that messages for maintaining a partial order are
//S: enabled on the collection.

//D: A partially ordered collection supports the maintenance of individual
//D: ordering relations among its members.  Each such relation is an
//D: assertion that one member should always precede another in the
//D: enumeration sequence of the collection.  When an index traverses the
//D: collection, it will guarantee that all the predecessors of a member
//D: have already been visited before the member itself is reached.

//D: If true, PartiallyOrdered specifies that messages for maintaining a
//D: partial order are enabled on the collection.  An error will be raised
//D: if individual members do not have a unique identity within the
//D: innermost collection within which they are contained.  This means that
//D: if duplicate keys are permitted by the collection, a collection type
//D: must also be specified for GroupType which does not itself accept
//D: duplicates.  Every member in a partial order must have a unique
//D: identity so that the member itself is sufficient to identify it within
//D: the network of partial orders.
CREATING
- (void)setPartiallyOrdered: (BOOL)partiallyOrdered;

USING
- (BOOL)getPartiallyOrdered;
@end

@deftype PartialOrderContext
//S: Set the larger context of a partial order relationship.

//D: Ordering relations may be maintained not only between members
//D: belonging to the same collection, but between members and other
//D: collections, as long as all participants in an ordering are contained
//D: somewhere within a larger common collection.  An ordering relation
//D: with a nested collection specifies that all members in the collection
//D: are successors or predecessors of the other member.  Ordering
//D: relations can also be established between members of a larger,
//D: containing collection, and individual members of a nested collection.

//D: The keyed collection specified as the value for PartialOrderContext
//D: must specify another collection in which the local collection is
//D: maintained.  It is an error if the PartialOrderContext collection does
//D: not actually contain the local collection.
CREATING
- (void)setPartialOrderContext: aKeyedCollection;

USING
- getPartialOrderContext;
@end

@deftype PartialOrderRelations
//S: These messages maintain the ordering relations within a partially
//S: ordered collection.

//D: These messages maintain the ordering relations within a partially
//D: ordered collection.
USING
- (void)getPredecessors: aMember;
- (void)getSuccessors: aMember;
- (void)getCostarts: aMember;
- (void)getCoends: aMember;
@end

@deftype KeyedCollection <Collection, DupOption, Sorted, CompareFunction, BucketFunction, PartiallyOrdered, PartialOrderContext, PartialOrderRelations>
//S: Member identity definition shared by Set and Map types.

//D: A keyed collection is a collection in which each member can be
//D: compared with some other value that identifies the member.  This value
//D: is referred to as the member key.  The key value may be determined
//D: either by the member value itself, which defines a Set, or by external
//D: association with the member when the member is first added, which
//D: defines a Map.

//D: The keyed collection type establishes the common behavior shared by
//D: both Set and Map.  Standard options are provided to declare ordering
//D: of members in the collection, which can be based either on an inherent
//D: ordering of the key values (a sorted collection) or external
//D: maintenance of a total or partial order.  Additionally, options are
//D: provided to declare whether duplicate key values should be permitted,
//D: and if so, how.

//D: The options for handling of duplicate keys provide considerable
//D: flexibility.  Entire structures of nested collections can be created
//D: that subdivide the key space through progressive levels.  By accepting
//D: duplicate keys, and organizing members of the same identity into
//D: subgroups, keyed collections support the abstract data type known as a
//D: bag or multiset.  The index on a keyed collection includes the ability
//D: to traverse throughout the contents of any of these nested
//D: collections.

//D: The KeyedCollection type inherits all standard behavior of Collection.
//D: The KeyedCollection type is not itself creatable; it only serves as a
//D: common supertype for Set and Map collection types.

//D: The default value of IndexSafety for a KeyedCollection is Unsafe, but
//D: values of either UnsafeAtMember or SafeAlways can be requested to
//D: guarantee a higher level of safety.

CREATING
// - (void)setIndexFromMember: (int)byteOffset;
USING

// - (int)getIndexFromMember;

//M: Creates a new index on the collection and
//M: also sets its current position to the first member matching the key
//M: value given as its final argument.  It returns the index just created.
- createIndex: aZone at: aKey;

//M: Creates a new index on the collection and
//M: also sets its current position to the member passed as its final
//M: argument.  This message is valid only if an internal member slot was
//M: defined for the collection with the MemberSlot option.
- createIndex: aZone setMember: aMember;

- createIndex: aZone fromMember: anObject;

//M: The at: message returns the existing member of the collection which
//M: matches the key value passed as its argument, or nil if there is no
//M: key value in the collection which matches.  If duplicate entries for
//M: this key exist, the entire collection of duplicate members created for
//M: the key value is returned instead.
- at: aKey;

//M: The getCountAtKey message returns the number of member entries which are
//M: all associated with its key value argument.  This number is zero if
//M: the key value is not contained in the collection, and will never
//M: exceed one one unless a collection accepts duplicate entries for a key.
- (int)getCountAtKey: aKey;

//M: The containsKey: message returns true if the key value passed as its
//M: argument is contained in the collection, and false otherwise. 
- (BOOL)containsKey: aKey;

//M: The removeKey: message removes a member matching a key value from the
//M: collection, and returns the member just removed.  It returns nil if
//M: there is no key value in the collection which matches.  If more than
//M: one entry was present for the key value, it removes and returns the
//M: first member in the internal collection created for duplicate members.
- removeKey: aKey;
@end

@deftype KeyedCollectionIndex <Index>
//S: Index behavior shared by Set and Map types.

//D: An index to a keyed collection traverses all members of the
//D: collection, regardless of whether these members belong to collections
//D: of members entered under duplicate key values.  Internally, however,
//D: an index keeps track of any specific subcollection it is currently
//D: processing.

///M: The setMember: message repositions the index to the member passed as
///M: its argument.  This message is valid only if an internal member slot
///M: was defined for the collection with the MemberSlot option.
// - setMember: aMember;

// - (int)getCountAtKey;
@end


@deftype MemberSlot
//S: Allocation in member/key for fast setMember:/setKey:

//D: The MemberSlot option indicates that space has been reserved within
//D: each member that allows it to contain a link directly to its position
//D: in the enumeration for a collection.  If such space has been reserved,
//D: special messages can be used that rapidly position an index directly
//D: to the member.  Operations to remove members are also much faster.

//D: The value of MemberSlot specifies the offset in bytes from the start
//D: of each member where the space for its position link has been
//D: reserved.  

//D: The offset of the position link from the start of a member may be
//D: either positive or negative, in the range of -2048 to +2047.  The
//D: default value of MemberSlot is UnknownOffset (a large negative value),
//D: which specifies that no slot for an internal position link is
//D: available within each member.

//T: This type declares the required space for a position link in a
//T: collection that does not accept duplicate key values.

//T: If a member slot is defined, a memory pointer of some kind must always
//T: be used for the value of every member.  These members may be object id
//T: pointers, but other types of memory pointers are acceptable as well.

//T: The contents of the first two words of position link vary according to
//T: the position of a member in a collection. 
typedef struct memberData { void *memberData[2]; } member_t;

//T: The third word of a
//T: dupmember_t link contains the id of the collection in which a member
//T: is directly contained.  This collection is either an internally
//T: created collection containing members with duplicate keys, or the
//T: collection to which the member was added if there is no other member
//T: with the same key.
typedef struct { void *memberData[2]; id owner; } dupmember_t;
@end

@deftype Set <KeyedCollection, CREATABLE>
//S: Collection of members each having a defined identity.

//D: Set is a subtype of KeyedCollection in which the key value associated
//D: with each member is determined by the member value itself.  The key
//D: value may be identical to the member itself, or may be defined as a a
//D: function of the member using a create-time option.

//D: The Set type inherits most of its interface from the KeyedCollection
//D: supertype.  Set defines no create-time options beyond those already
//D: defined by KeyedCollection.  If a custom compare or bucket function is
//D: specified, the member value is passed as the key value arguments of
//D: these functions.  These functions determine what part of the member
//D: value is part of the key value, by determining which key values will
//D: compare equal to any member.

USING
//M: The add: message adds a new member to a set.  It returns true if
//M: member added matched the key of any member already contained in the
//M: Set.  Whether or not the member was actually added to the collection
//M: or not depends on the setting of DupOption.
- (id *)add: anObject;

///M: The add:setIndex: message adds a new member to a set just like add:,
///M: but also has a side effect of setting the position of the index passed
///M: as the setIndex: argument to the member which was just added.  If an
///M: index positioned to the new member is needed anyway, setting the index
///M: in the same operation avoids repeating a search for the position at
///M: which the member key belongs.
// - (BOOL)add: anObject setIndex: anIndex;

//M: The replace: message replaces a member stored at a given key with
//M: another member value that matches the same key.  The new value to
//M: replace the existing one is passed as the argument.  replace: returns
//M: the member value that was replaced, or nil if the collection contained
//M: no member with a matching key.
- replace: anObject;
@end

@deftype OrderedSet <KeyedCollection, List, CREATABLE>
//S: A set of members in an externally assigned linear sequence.

//D: An OrderedSet is a totally ordered collection of members in which every
//D: member also has a distinct identity as defined by comparison against
//D: a key value.

//D: (.. This type is currently implemented only using the low-level option
//D: of an internal member slot, and the messages for that option do not
//D: match the documentation in KeyedCollection.  If you need one of these
//D: objects, then either use a List or wait for some other
//D: implementation.)

//D: The sequence of members of an OrderedSet is established using the same
//D: messages that maintain member sequence in a List.  An OrderedSet
//D: supports customization and access by key as defined by Set and
//D: KeyedCollection.  The union of messages from all these sources defines
//D: the total interface of an OrderedSet.  Members with duplicate keys,
//D: however, are not valid for an OrderedSet.  Each member must have a
//D: unique position within the member sequence
@end

@deftype Map <KeyedCollection, CREATABLE>
//S: Collection of associations from key objects to member objects.

//D: Map is a subtype of KeyedCollection in which the key value associated
//D: with each member is independent of the member itself.  Whenever a new
//D: member is added to the collection, a key value to be associated with
//D: the member must be supplied also.  A Map defines a mapping from key
//D: values to member values.

//D: For the Map type, key values are independent of the member values with
//D: which they are associated.  Map defines two additional options to
//D: document information about its key values.  Map also defines its own
//D: messages to distinguish the key value from member value in any
//D: operation which involves both.

CREATING
// - (void)setIndexFromKey: (int)byteOffset;
- (void)setKeyType: aDataType;
- (void)setKeySize: (size_t)size;
USING
// - (int)getIndexFromKey;
// - (size_t)getKeyAllocSize;

//M: at:insert: inserts an entry into a Map containing the key and member
//M: values given as its arguments.  It returns true if the key was not
//M: previously contained in the collection.  If the key was already
//M: present, the action of the message depends on the value in effect for
//M: the DuplicatesOption.  With default options, the attempt to insert a
//M: duplicate key is simply rejected and false is returned.
- (BOOL)	at: aKey insert: anObject;

///M: at:insert:setIndex: is similar to at:insert:, except that the index
///M: passed as the final argument is repositioned to the member just
///M: inserted.  If further operations are required on the entry in a map
///M: that was just inserted, an index provides faster repeated access to
///M: this entry than a message involving further key comparison, such as
///M: at:, would require.  (..  This message is not currently implemented.)
// - (BOOL)at: aKey insert: anObject setIndex: anIndex;

//M: Replaces an existing member value associated with a key
//M: value by a new value given as its final argument.  The message returns
//M: the member value which was formerly associated with the key value.
//M: The message returns nil if more than one member is present with the
//M: same key.
- at: aKey replace: anObject;

// - insertGroup: aKey;

- removeKey: aKey;
// - removeKey: aKey getKey: (id *)oldKey;

///M: Replaces a key value with another key value that still
///M: compares equal to a key value already present.  It may be used to
///M: replace an allocation of one object, already contained in a Map, by a
///M: different object allocation.  replaceKey: returns nil if no existing
///M: key value matches its key value argument; otherwise it returns the key
///M: value which was replaced by its argument.
// - replaceKey: aKey;

///M: (.. This message is not currently implemented.  It will eventually give
///M: a fast way to create an index already positioned at an entry with a
///M: particular key value.)
// - createIndex: aZone setKey: aKey;

// - createIndex: aZone setMember: aMember;
@end

@deftype MapIndex <KeyedCollectionIndex>
//S: The index behavior for a Map.

//D: The index behavior for a Map.

USING
//M: The setKey: messages repositions the index to
//M: an entry having a key value that matches its argument.  If there is
//M: more than one entry matching this key value, the index is positioned
//M: to the first entry that matches.
- setKey: aKey;

//M: The getKey message returns the key value associated with the current
//M: location of the index.  It returns nil if the index is not currently
//M: positioned at a member. 
- getKey;

//M: Return the next item and it's key.
- next: (id *)key;

//M: Return the previous item and it's key.
- prev: (id *)key;

//M: Return the current item and it's key.
- get:  (id *)key;

@end


@deftype OutputStream <Create, Drop, CREATABLE>
//S: Stream of output bytes.

//D: The OutputStream type currently supports only the writing of a
//D: character string to a file.  It is a placeholder for more general
//D: stream types.  A stream is a collection that supports only sequential
//D: addition of members (an output stream) or sequential removal of
//D: members (an input stream).
CREATING
+ create: aZone setFileStream: (FILE *)fileStream;
- setFileStream: (FILE *)fileStream;

USING
- (FILE *)getFileStream;
- (void)catC: (const char *)cstring;
@end

@deftype InputStream <Create, Drop, CREATABLE>
//S: Stream of input data.

//D: This type reads Lisp-like expressions into lists.
CREATING
+ create: aZone setFileStream: (FILE *)file;
-               setFileStream: (FILE *)fileStream;

USING
- (FILE *)getFileStream;
- getExpr;
@end

//G: Tokens used by the archiving parser.
extern id <Symbol> ArchiverLiteral, ArchiverQuote, ArchiverEOL, ArchiverDot;

@deftype String <Create, Drop, Copy, CREATABLE>
//S: Character string object (later to support collection behavior).

//D: The String object type packages a null-terminated, C-format character
//D: string into an object.  All memory allocation needed to hold the
//D: string value is handled by the object.  This type currently defines
//D: only the most rudimentary operations for initializing and appending
//D: C-format character strings.  These are sufficient for its current
//D: limited roles in places that need a uniformity between character
//D: strings and other kinds of allocated objects.

CREATING
+ create: aZone setC: (const char *)cstring;
- setLiteralFlag: (BOOL)literalFlag;

SETTING
- (void)setC: (const char *)cstring;

USING
- (const char *)getC;
- (void)catC: (const char *)cstring;
- (int)getCount;
- (int)compare: aString;
- (BOOL)getLiteralFlag;
@end

@class LiteralString;


#import <collections/types.h>
