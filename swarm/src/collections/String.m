// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         String.m
Description:  character string object   
Library:      collections
*/

#import <collections/String.h>
#include <string.h>
#include <assert.h>


@implementation String_c

+ createBegin: aZone
{
  String_c *newString;

  newString = [aZone allocIVars: self];

  // create initial string of length zero

  newString->zone   = aZone;
  newString->count  = 0;
  newString->string = "";
  return newString;
}

- createEnd
{
  createByMessageToCopy( self, copy: );
  setNextPhase( self );
  return self;
}

+ create: aZone  // same as createBegin: -- optimization only
{
  String_c *newString;

  newString = [aZone allocIVars: getNextPhase( self )];

  newString->zone   = aZone;
  newString->count  = 0;
  newString->string = "";
  return newString;
}

+ create: aZone setC: (char *)cstring
{
  String_c *newString;

  assert( cstring );
  newString = [aZone allocIVars: getNextPhase( self )];

  newString->zone = aZone;
  newString->count = strlen( cstring );

  if ( newString->count > 0 ) {
    newString->string =
      [aZone copyBlock: cstring blockSize: newString->count + 1];
  } else {
    newString->string = "";
  }
  return newString;
}

PHASE(Setting)

- (void) setC: (char *)cstring
{
  int   countNew;
  char  *stringNew;

  if( ! cstring ) raiseEvent( InvalidArgument, "argument is nil\n" );
  countNew = strlen( cstring );
  if ( countNew > 0 ) {
    stringNew = [zone copyBlock: cstring blockSize: countNew + 1];
  } else {
    stringNew = "";
  }
  if ( count > 0 ) [zone freeBlock: string blockSize: count + 1];
  string = stringNew;
  count  = countNew;
}

PHASE(Using)

- copy: aZone
{
  String_c  *newString;

  newString = [aZone copyIVars: self];
  if ( count > 0 ) {
    newString->string = [aZone copyBlock: string blockSize: count + 1];
  }
  return newString;
}

- (char *) getC
{
  return string;
}

- (void) appendC: (char *)cstring
{
  int   appendCount;
  char  *stringNew;

  if( ! cstring ) raiseEvent( InvalidArgument, "argument is nil\n" );
  appendCount = strlen( cstring );
  if ( (count + appendCount) > 0 ) {
    stringNew = [zone allocBlock: count + appendCount + 1];
    memcpy( stringNew, string, count );
    memcpy( stringNew + count, cstring, appendCount + 1 );
    if ( count > 0 ) [zone freeBlock: string blockSize: count + 1];
    string = stringNew;
    count  = count + appendCount;
  }
}

- (int) getCount
{
  return count;
}

- (int) count
{
  return count;
}

- (int) length
{
  return count;
}

- (int) compare: aString
{
  return strcmp( string, ((String_c *)aString)->string );
}

- (void) drop
{
  if ( *string != '\0' ) [zone freeBlock: string blockSize: count + 1];
  [zone freeIVars: self];
}

@end
