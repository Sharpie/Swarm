// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
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
- lispOutShallow: stream;
- lispOutDeep: stream;
@end

@interface ArchiverArray_c: CreateDrop_s
{
  unsigned rank;
  unsigned *dims;
  size_t elementSize;
  unsigned elementCount;
  char type;
  void *data;
}
- setArray: array;
- (void *)getData;
- (unsigned *)getDims;
- (size_t)getElementSize;
- (unsigned)getElementCount;
- (char)getArrayType;
- lispOutShallow: stream;
- lispOutDeep: stream;
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
    id obj;
  } value;
}
- setDouble: (double)val;
- setFloat: (float)val;
- setInteger: (int)val;
- setChar: (unsigned char)val;
- setBoolean: (BOOL)val;
- setNil;
- (double)getDouble;
- (float)getFloat;
- (int)getInteger;
- (unsigned char)getChar;
- (BOOL)getBoolean;
- getObject;
- (char)getValueType;
- lispOutShallow: stream;
- lispOutDeep: stream;
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
