// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:        OutputStream.h
Description: character string object
Library:     collections
*/

#import <defobj/Create.h>
#import <collections.h>

@interface OutputStream_c: CreateDrop_s <OutputStream>
{
@public
  FILE *fileStream;
}
/*** methods in OutputStream_c (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- (void)setFileStream: (FILE *)file;
- createEnd;
+ create: aZone setFileStream: (FILE *)file;
- (FILE *)getFileStream;
- (void)catC: (const char *)cstring;
- (void)catChar: (char)ch;
- (void)catFloat: (float)flt;
- (void)catDouble: (double)dbl;
- (void)catLongDouble: (long double)ldbl;
- (void)catInt: (int)i;
- (void)catUnsigned: (unsigned)un;
- (void)catShort: (short)sht;
- (void)catUnsignedShort: (unsigned short)usht;
- (void)catLong: (long)lng;
- (void)catUnsignedLong: (unsigned long)ulng;
- (void)catLongLong: (long long)lnglng;
- (void)catUnsignedLongLong: (unsigned long long)ulnglng;
- (void)catPointer: (void *)ptr;
@end
