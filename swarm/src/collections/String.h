// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         String.h
Description:  character string object
Library:      collections
*/

#import <defobj/Create.h>
#import <collections.h>

@interface String_c : CreateDrop_s
{
@public
  char  *string;
  int   count;
  BOOL literalFlag;
}
/*** methods in String_c (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- createEnd;
+ create: aZone;
+ create: aZone setC: (char *)cstring;
- setLiteralFlag : (BOOL)literalFlag;
- (void) setC: (char *)cstring;
- copy: aZone;
- (char *) getC;
- (void) catC: (char *)cstring;
- (void) appendC: (char *)cstring;
- (int) getCount;
- (int) count;
- (int) length;
- (int) compare: aString;
- (BOOL)getLiteralFlag;
- (void) describe: outputCharStream;
- (void) mapAllocations: (mapalloc_t)mapalloc;
@end
