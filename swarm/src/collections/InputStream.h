// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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

@interface InputStream_c: CreateDrop_s <InputStream>
{
@public
  FILE  *fileStream;
  id expr;
}
+ createBegin: aZone;
- setFileStream: (FILE *)file;
- setExpr: expr;
- createEnd;
+ create: aZone setFileStream: (FILE *)file;
+ create: aZone setExpr: expr;
- (FILE *)getFileStream;
- getExpr;
@end

