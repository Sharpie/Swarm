// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
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
#import <collections/List_linked.h>

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

@interface ArchiverKeyword_c: CreateDrop_s <ArchiverKeyword>
{
  const char *keywordName;
}
- setKeywordName: (const char *)name;
- (const char *)getKeywordName;
- lispOutShallow: (id <OutputStream>)stream;
- lispOutDeep: (id <OutputStream>)stream;
@end

@interface ArchiverArray_c: CreateDrop_s <ArchiverArray>
{
  unsigned rank;
  unsigned *dims;
  size_t elementSize;
  unsigned elementCount;
  const char *type;
  void *data;
}
- setArray: array;
- (void *)getData;
- (unsigned *)getDims;
- (size_t)getElementSize;
- (unsigned)getElementCount;
- (char)getArrayType;
- convertToType: (char)destType dest: (void *)ptr;
- lispOutShallow: (id <OutputStream>)stream;
- lispOutDeep: (id <OutputStream>)stream;
- (void)drop;
@end

@interface ArchiverValue_c: CreateDrop_s <ArchiverValue>
{
  char type;
  union {
    long double ld;
    double d;
    float f;
    long long ll;
    unsigned char ch;
    id obj;
  } value;
}
- setFloat: (float)val;
- setDouble: (double)val;
- setLongDouble: (long double)val;
- setLongLong: (long long)val;
- setChar: (unsigned char)val;
- setBoolean: (BOOL)val;
- setNil;
- (long double)getLongDouble;
- (double)getDouble;
- (float)getFloat;
- (long long)getLongLong;
- (int)getInteger;
- (unsigned)getUnsigned;
- (unsigned char)getChar;
- (BOOL)getBoolean;
- getObject;
- (char)getValueType;
- lispOutShallow: (id <OutputStream>)stream;
- lispOutDeep: (id <OutputStream>)stream;
@end

@interface ArchiverPair_c: CreateDrop_s <ArchiverPair>
{
  id car;
  id cdr;
  BOOL consFormatFlag;
}
- setCar: car;
- setCdr: cdr;
- setConsFormatFlag: (BOOL)theConsFormatFlag;
- (BOOL)getConsFormatFlag;
- getCar;
- getCdr;
- lispOutShallow: (id <OutputStream>)stream;
- lispOutDeep: (id <OutputStream>)stream;
@end


@interface ArchiverList_c: List_linked <ArchiverList>
{
}
- lispOutShallow: (id <OutputStream>)stream;
- lispOutDeep: (id <OutputStream>)stream;
@end
