// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         collections.h
Description:  standard collection types   
Library:      collections
*/

#import <defobj.h>

//
// Collection -- generic collection interface
//
@deftype Collection <Create, SetInitialValue, Copy, Drop>
CREATING
- (void)        setMemberType: aDataType;
- (void)        setIndexSafety: indexSafety;
- (void)        setIndexUpdateHandler: (func_t)handler : (void *)arg;
- (void)        setReplaceOnly: (BOOL)replaceOnly;
USING
-               getMemberType;
-               getIndexSafety;
- (func_t)      getIndexUpdateHandler: (void *)arg;
- (BOOL)        getReplaceOnly;

- (int)		getCount;

-		atOffset: (int)offset;
-		atOffset: (int)offset put: anObject;

-		getFirst;
-		getLast;

- (BOOL)	contains: aMember;
-		remove: aMember;
- (void)	removeAll;

-		begin: aZone;

- (void)	forEach: (SEL)aSelector;
- (void)	forEach: (SEL)aSelector : arg1;
- (void)	forEach: (SEL)aSelector : arg1 : arg2;
- (void)	forEach: (SEL)aSelector : arg1 : arg2 : arg3;
@end

// values for index safety

id <Symbol>  Unsafe, UnsafeAtMember, SafeAlways;


//
// Index -- reference into the enumeration sequence for a collection
//
@deftype Index <DefinedObject, Copy, Drop>
-		getCollection;

-		next;
-		prev;
-		findNext: anObject;
-		findPrev: anObject;

-		get;
-		put: anObject;
-		remove;

-		getLoc;
- (void)	setLoc: locSymbol;
- (int)		getOffset;
-		setOffset: (int)offset;

- (int)		compare: anIndex;
@end

//
// values for index location
//
extern id <Symbol>  Start, End, Between, Removed, Member;

//
// return value for index offset if not defined
//
#define UndefinedOffset -0x80000000;

//
// error types for collections
//
extern id <Error>
  OffsetOutOfRange, NoMembers, 
  AlreadyAtEnd, AlreadyAtStart, InvalidIndexLoc, InvalidLocSymbol;


//
// Array -- collection supporting access only by relative position
//
@deftype Array <Collection, CREATABLE>
CREATING
- (void)	setDefaultMember: memberValue;
- (void)	setMemberBlock: (id *)members setCount: (int)count;

+		create: aZone setCount: (int)count;
+		create: aZone setMemberBlock:(id *)members setCount:(int)count;
SETTING
- (void)	setCount: (int)count;
- (void)        setDefaultMember: memberValue;

- (void)        setMemberBlock: (id *)members setCount: (int)count;
USING

- (void)	setDefaultMember: memberValue;
-		getDefaultMember;

- (void)	setMemberAlloc: (id *)members setCount: (int)count;
- (void *)	getMemberAlloc;
@end


//
// List - collection of members in an externally assigned linear sequence
//
@deftype List <Collection, CREATABLE>
CREATING
- (void)	setEndsOnly: (int)countPerBlock;
USING
- (int)		getEndsOnly;

- (void)	addFirst: anObject;
- (void)	addLast: anObject;
-		removeFirst;
-		removeLast;
@end

//
// ListIndex -- index with insertion capability at any point in list
//
@deftype ListIndex <Index>
- (void)	addAfter: anObject;
- (void)	addBefore: anObject;
@end

//
// Stack -- form of list with add/remove both at start of list
//
@deftype Stack <List>
- (void)	push: anObject;
-		pop;
@end

//
// Queue -- form of list with add at start of list and remove at last
//
@deftype Queue <List>
- (void)	enqueue: anObject;
-		dequeue;
@end


typedef int (*idXid_t)( id, id );

//
// KeyedCollection -- member identity definition shared by Set and Map types 
//
@deftype KeyedCollection
CREATING
- (void)	setDupOption: dupOption;
- (void)	setDupMembersType: aCollectionType;

- (void)	setSorted: (BOOL)sorted;
- (void)	setCompareFunction: (idXid_t)aFunction;
- (void)        setBucketFunction: (idXid_t)aFunction setMaxBuckets: (int)max;

- (void)	setPartiallyOrdered: (BOOL)partiallyOrdered;
- (void)	setPartialOrderContext: aKeyedCollection;
- (void)	setIndexFromMember: (int)byteOffset;
USING
-		getDupOption;
-		getDupMembersType;

- (BOOL)	getSorted;
- (int(*)())	getCompareFunction;
- (int(*)())	getBucketFunction;
- (int)		getMaxBuckets;

- (BOOL)	getPartiallyOrdered;
- (void)	setPartialOrderContext: aKeyedCollection;
-		getPartialOrderContext;

- (int)		getIndexFromMember;

- (void)	getPredecessors: aMember;
- (void)	getSuccessors: aMember;
- (void)	getCostarts: aMember;
- (void)	getCoends: aMember;

-		at: aKey;
- (int)		getCountAtKey: aKey;

- (BOOL)	containsKey: aKey;

-		createIndex: aZone setMember: aMember;
@end

//
// KeyedCollectionIndex -- index behavior shared by Set and Map types
//
@deftype KeyedCollectionIndex <Index>
-		setMember: aMember;
- (int)		getCountAtKey;
@end


// values for DupOption
id <Symbol>  DupIsError, DupRejected, KeepAllDups, KeepCountOnly;

//
// member_t -- allocation in member/key for fast setMember:/setKey:
//
typedef struct memberData { void *memberData[2]; } member_t;

//
// Set -- collection of members each having a defined identity
//
@deftype Set <KeyedCollection, CREATABLE>
- (id *)	add: anObject;
-		replace: anObject;
@end

//
// OrderedSet -- a set of members in an externally assigned linear sequence
//
@deftype OrderedSet <KeyedCollection, List, CREATABLE>
@end

//
// Map -- collection of associations from key objects to member objects
//
@deftype Map <KeyedCollection, CREATABLE>
CREATING
- (void)	setIndexFromKey: (int)byteOffset;
- (void)	setKeyType: aDataType;
- (void)	setKeySize: (size_t)size;
USING
- (int)		getIndexFromKey;
- (size_t)	getKeyAllocSize;

- (BOOL)	at: aKey insert: anObject;
-		at: aKey replace: anObject;

-		insertGroup: aKey;

-		removeKey: aKey;
-		removeKey: aKey getKey: (id *)oldKey;
-		replaceKey: aKey;

-		createIndex: aZone setMember: aMember;
@end

@deftype MapIndex <KeyedCollectionIndex>
-		setKey: aKey;
-		getKey;

-		next: (id *)key;
-		prev: (id *)key;
-		get:  (id *)key;
@end


//
// OutputStream -- stream of output bytes
//
@deftype OutputStream <Create, Drop, CREATABLE>
CREATING
+		create: aZone setFileStream: (FILE *)fileStream;
-		setFileStream: (FILE *)fileStream;
USING
- (FILE *)	getFileStream;
- (void)	appendC: (char *)cstring;
@end

//
// String -- character string object (later to support collection behavior)
//
@deftype String <Create, Drop, Copy, CREATABLE>
CREATING
+		create: aZone setC: (char *)cstring;
SETTING
- (void)	setC: (char *)cstring;
USING
- (char *)	getC;
- (void)	appendC: (char *)cstring;

- (int)		getCount;
- (int)		compare: aString;
@end


//
// declarations of obsolete message names
//
@protocol _collections_compatibility
- (int)		count;  // now getCount
-		first;  // now getFirst
-		last;   // now getLast
-		atOffset: (int)offset replace: anObject;
- (void)	setIndexFromMemberLoc: (int)offset;
-		createIndex: aZone fromMember: aMember;
- (int)		length;
@end

//
// Include automatically generated definitions for this module.
//
#import <collections/types.h>
