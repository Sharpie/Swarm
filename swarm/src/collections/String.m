// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         String.m
Description:  character string object   
Library:      collections
*/

#import <collections/String.h>
#import <defobj/defalloc.h>
#include <string.h>
#include <assert.h>


@implementation String_c

PHASE(Creating)
     
+ createBegin: aZone
{
  String_c *newString;

  newString = [aZone allocIVars: self];

  // create initial string of length zero

  newString->count  = 0;
  newString->literalFlag = 0;
  newString->string = "";
  return newString;
}

- createEnd
{
  createByMessageTo( self, copy: );
  setMappedAlloc( self );
  setNextPhase( self );
  return self;
}

+ create: aZone  // same as createBegin: -- optimization only
{
  String_c *newString;

  newString = [aZone allocIVars: getNextPhase( self )];
  setMappedAlloc( newString );
  newString->count  = 0;
  newString->string = "";
  return newString;
}

- setLiteralFlag : (BOOL)theLiteralFlag
{
  literalFlag = theLiteralFlag;
  return self;
}

+ create: aZone setC: (char *)cstring
{
  String_c *newString;

  assert( cstring );
  newString = [aZone allocIVars: getNextPhase( self )];
  setMappedAlloc( newString );
  newString->count = strlen( cstring );

  if ( newString->count > 0 ) {
    newString->string = [aZone allocBlock: newString->count + 1];
    memcpy( newString->string, cstring,  newString->count + 1 );
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

  if( ! cstring ) raiseEvent( InvalidArgument, "> argument is nil\n" );
  countNew = strlen( cstring );
  if ( countNew > 0 ) {
    stringNew = [getZone( self ) allocBlock: countNew + 1];
    memcpy( stringNew, cstring, countNew + 1 );
  } else {
    stringNew = "";
  }
  if ( count > 0 ) [getZone( self ) freeBlock: string blockSize: count + 1];
  string = stringNew;
  count  = countNew;
}

PHASE(Using)

//
// copy: -- standard method to copy internal state of object
//
- copy: aZone
{
  String_c  *newString;

  newString = [aZone copyIVars: self];
  setMappedAlloc( newString );
  if ( count > 0 ) {
    newString->string = [aZone allocBlock: count + 1];
    memcpy( newString->string, string, count + 1 );
  }
  return newString;
}

- (char *) getC
{
  return string;
}

- (void) catC: (char *)cstring
{
  id    zone;
  int   appendCount;
  char  *stringNew;

  zone = getZone( self );
  if( ! cstring ) raiseEvent( InvalidArgument, "> argument is nil\n" );
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

- (void) appendC: (char *)cstring
{
  [self catC: cstring];
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

- (BOOL)getLiteralFlag
{
  return literalFlag;
}

//
// describe: -- standard method to generate debug description
//
- (void) describe: outputCharStream
{
  char  buffer[100];

  [super describe: outputCharStream];
  sprintf( buffer, "> number of characters: %d\n", count );
  [outputCharStream catC: buffer];
  if ( count <= 64 ) {
    sprintf( buffer, "> string value: %s\n", string );
    [outputCharStream catC: buffer];
  } else {
    sprintf( buffer, "> string value (first 50 characters): \"%.50s\"\n",
             string );
    [outputCharStream catC: buffer];
  }
}

- out : outputCharStream
{
  if (literalFlag)
    {
      [outputCharStream catC: "\""];
      [outputCharStream catC: [self getC]];
      [outputCharStream catC: "\""];
    }
  else
    [outputCharStream catC: [self getC]];
  return self;
}

//
// mapAllocations: -- standard method to map internal allocations
//
- (void) mapAllocations: (mapalloc_t)mapalloc
{
  if ( ! includeBlocks( mapalloc ) || *string == '\0' ) return;
  mapalloc->size = count + 1;
  mapAlloc( mapalloc, string );
}

@end
