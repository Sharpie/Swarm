// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         OrderedSet.h
Description:  set of elements in an externally maintained linear sequence
Library:      collections
*/

#import <collections/List_mlinks.h>

@interface OrderedSet_c: List_mlinks <OrderedSet>
/*** methods in OrderedSet_c (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- createEnd;
- (BOOL)contains: anObject;
- (BOOL)add: anObject;
- remove: aMember;
@end

@interface OrderedSetIndex_c : ListIndex_mlinks <KeyedCollectionIndex>
/*** methods in OrderedSetIndex_c (inserted from .m file by m2h) ***/
@end

