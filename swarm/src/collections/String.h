// Swarm library. Copyright (C) 1996 Santa Fe Institute.
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
}
/*** methods implemented in .m file ***/
+ createBegin: aZone;
- createEnd;
+ create: aZone;
+ create: aZone setC: (char *)cstring;
- copy: aZone;
- (void) setC: (char *)cstring;
- (char *) getC;
- (void) appendC: (char *)cstring;
- (int) count;
- (int) length;
- (int) compare: aString;
- (void) drop;
@end
