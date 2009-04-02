#import <Swarm/Create.h>
#import <Swarm/defobj.h>
#import <Swarm/collections.h>
#import <Swarm/List_linked.h>

@interface ArchiverKeyword_c: CreateDrop_s <ArchiverKeyword>
{
  const char *keywordName;
}
- setKeywordName: (const char *)name;
- (const char *)getKeywordName;
- (void)lispOutShallow: (id <OutputStream>)stream;
- (void)lispOutDeep: (id <OutputStream>)stream;
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
- (void)lispOutShallow: (id <OutputStream>)stream;
- (void)lispOutDeep: (id <OutputStream>)stream;
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
    BOOL bool_;
    char ch;
    id obj;
    Class class;
  } value;
}
- setFloat: (float)val;
- setDouble: (double)val;
- setLongDouble: (long double)val;
- setLongLong: (long long)val;
- setChar: (char)val;
- setBoolean: (BOOL)val;
- setNil;
- setClass: (Class)class_;
- (long double)getLongDouble;
- (double)getDouble;
- (float)getFloat;
- (long long)getLongLong;
- (int)getInteger;
- (unsigned)getUnsigned;
- (char)getChar;
- (BOOL)getBoolean;
- getObject;
- (Class)getClass;
- (fcall_type_t)getValueType;
- (void)lispOutShallow: (id <OutputStream>)stream;
- (void)lispOutDeep: (id <OutputStream>)stream;
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
- (void)lispOutShallow: (id <OutputStream>)stream;
- (void)lispOutDeep: (id <OutputStream>)stream;
@end


@interface ArchiverList_c: List_linked <ArchiverList>
{
}
- (void)lispOutShallow: (id <OutputStream>)stream;
- (void)lispOutDeep: (id <OutputStream>)stream;
@end

@interface ArchiverQuoted_c: CreateDrop_s <ArchiverQuoted>
{
  id value;
}
- setQuotedObject: value;
- getQuotedObject;
- (void)lispOutDeep: (id <OutputStream>)stream;
@end
