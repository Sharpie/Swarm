// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         List.m
Description:  implementations for List type
Library:      collections
*/

#import <collections/List.h>
#import <objc/objc-api.h>    // for sending messages to @class names


@implementation List_any

PHASE(Creating)

+ createBegin: aZone
{
  List_any  *newList;

  newList = [aZone allocIVars: self];
  return newList;
}

- (void) setInitialValue: initialValue
{
  firstLink = (link_t)initialValue;
  setBit( bits, Bit_InitialValueSet, 1 );
}

- (void) setDequeOnly: (BOOL)dequeOnly
{
  setBit( bits, Bit_DequeOnly, dequeOnly );
}

- createEnd
{
  id  index, member;

  if ( ( bits & Bit_InitialValueSet ) && ( bits & Bit_IndexFromMemberLoc ) )
    raiseEvent( InvalidCombination,
       "cannot specify an initial value with IndexFromMemberLoc option\n" );

  if ( bits & Bit_InitialValueSet ) {
    if ( createByMessageToCopy( self, createEnd ) ) return self;
    setClass( self, id_List_linked );
    setMappedAlloc( self );
    index = [(id)firstLink begin: scratchZone];
    firstLink = NULL;
    while ( (member = [index next]) ) [(id)self addLast: member];
    [index drop];

  } else {
    createByCopy( );
    if ( bits & Bit_IndexFromMemberLoc )
      setClass( self, id_List_mlinks );
    else
      setClass( self, id_List_linked );
      setMappedAlloc( self );
  }
  return self;
}

PHASE(Setting)

- (void) setCountPerBlock: (int)countPerBlock
{
  raiseEvent( NotImplemented, nil );
}

PHASE(Using)

- (BOOL) getDequeOnly
{
  return bits & Bit_DequeOnly;
}

- (int) getCountPerBlock
{
  raiseEvent( NotImplemented, nil );
  exit(1);
}

@end


// ListIndex_any: index for List_c

@implementation ListIndex_any
@end
