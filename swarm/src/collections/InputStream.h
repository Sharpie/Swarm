// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:        InputStream.h
Description: character string object
Library:     collections
*/

#import <defobj/Create.h>
#import <collections.h>

@interface InputStream_c : CreateDrop_s
{
@public
  FILE  *fileStream;
}
/*** methods in OutputStream_c (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- (void) setFileStream: (FILE *)file;
- createEnd;
+ create: aZone setFileStream: (FILE *)file;
- (FILE *) getFileStream;
- getExpr;
@end
