// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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
}
/*** methods in OutputStream_c (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- (void)setFileStream: (FILE *)file;
- createEnd;
+ create: aZone setFileStream: (FILE *)file;
- (FILE *)getFileStream;
- getExpr;
@end

@interface ArchiverKeyword_c: CreateDrop_s
{
  const char *keywordName;
}
- setKeywordName: (const char *)name;
- (const char *)getKeywordName;
@end

@interface ArchiverArray_c: CreateDrop_s
{
  unsigned rank;
  unsigned *dims;
  size_t elementSize;
  unsigned elementCount;
  void *data;
}
- setArray: array;
- (void *)getData;
- (unsigned *)getDims;
- (size_t)getElementSize;
- (unsigned)getElementCount;
- (void)drop;
@end

@interface ArchiverValue_c: CreateDrop_s
{
  char type;
  union {
    double d;
    float f;
    int i;
    unsigned char ch;
  } number;
}
- setDouble: (double)val;
- setFloat: (float)val;
- setInteger: (int)val;
- setChar: (unsigned char)val;
- setBoolean: (BOOL)val;
- (double)getDouble;
- (float)getFloat;
- (int)getInteger;
- (unsigned char)getChar;
- (BOOL)getBoolean;
- (char)getValueType;
@end

@interface ArchiverPair_c: CreateDrop_s
{
  id car;
  id cdr;
}
- setCar: car;
- setCdr: cdr;
- getCar;
- getCdr;
@end
