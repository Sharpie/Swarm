// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         List.h
Description:  implementations for List type
Library:      collections
*/

#import <collections/Collection.h>

typedef struct link *link_t;

@protocol Dummy  // (mangles file symbol name to avoid libobjects conflict)
- (int) getCountPerBlock;
@end

@interface List_any : Collection_any <Dummy>
{
@public
  link_t     firstLink;    // entry into circular chain of doubly linked nodes
}
#define  Bit_DequeOnly        (1 << 2)
#define  CountPerBlock_Shift  20
#define  CountPerBlock_Mask   (0xfff << CountPerBlock_Shift)

/*** methods implemented in .m file ***/
+ createBegin: aZone;
- (void) setInitialValue: initialValue;
- (void) setDequeOnly: (BOOL)dequeOnly;
- (void) setCountPerBlock: (int)countPerBlock;
- createEnd;
- (BOOL) getDequeOnly;
- (void) setCountPerBlock: (int)countPerBlock;
- (int) getCountPerBlock;
@end

@interface ListIndex_any : Index_any
{
@public
  link_t     link;         // link at current position, or endpoint symbol,
                           // or previous of last removed link
  int        position;     // if positive, (offset + 1) of current member
                           // if negative, -(offset + 1) of last removed member
                           // if zero, link contains Start, End, or Unset
}
@end

