#import <defobj/Create.h>
#import <defobj.h>
#import <collections.h>
#import <collections/List_linked.h>

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
  fcall_type_t type;
  void *data;
}
- setArray: array;
- (void *)getData;
- (unsigned)getRank;
- (unsigned *)getDims;
- (size_t)getElementSize;
- (unsigned)getElementCount;
- (fcall_type_t)getArrayType;
- convertToType: (fcall_type_t)destType dest: (void *)ptr;
- lispOutShallow: (id <OutputStream>)stream;
- lispOutDeep: (id <OutputStream>)stream;
- (void)drop;
@end

@interface ArchiverValue_c: CreateDrop_s <ArchiverValue>
{
  fcall_type_t type;
  union {
    long double ld;
    double d;
    float f;
    long long ll;
    BOOL bool;
    unsigned char ch;
    id obj;
    Class class;
  } value;
}
- setFloat: (float)val;
- setDouble: (double)val;
- setLongDouble: (long double)val;
- setLongLong: (long long)val;
- setChar: (unsigned char)val;
- setBoolean: (BOOL)val;
- setNil;
- setClass: (Class)class;
- (long double)getLongDouble;
- (double)getDouble;
- (float)getFloat;
- (long long)getLongLong;
- (int)getInteger;
- (unsigned)getUnsigned;
- (unsigned char)getChar;
- (BOOL)getBoolean;
- getObject;
- (Class)getClass;
- (fcall_type_t)getValueType;
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

@interface ArchiverQuoted_c: CreateDrop_s <ArchiverQuoted>
{
  id value;
}
- setQuotedObject: value;
- getQuotedObject;
- lispOutDeep: stream;
@end
