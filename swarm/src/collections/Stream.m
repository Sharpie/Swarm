#import <collections/Stream.h>
#import <collections/List.h>
#import <collections/predicates.h>
#import <defobj/defalloc.h>
#import "../defobj/internal.h" // size_for_objc_type
#include <objc/objc-api.h>

@implementation ArchiverKeyword_c

PHASE(Creating)

- setKeywordName: (const char *)name
{
  keywordName = name;
  return self;
}

PHASE(Using)

- (const char *)getKeywordName
{
  return keywordName;
}

- lispOutShallow: (id <OutputStream>)stream
{
  return [self lispOutDeep: stream];
}

- lispOutDeep: (id <OutputStream>)stream
{
  [stream catC: "#:"];
  [stream catC: keywordName];
  return self;
}

@end
    
@implementation ArchiverArray_c

PHASE(Creating)

- setArray: array
{
  id <List> l;
  id proto;
  
  for (l = array, rank = 0; archiver_list_p (l); rank++)
    l = [l getFirst];

  proto = l;

  if (!valuep (proto))
    raiseEvent (InvalidArgument, "Array element not numeric");
  
  dims = [getZone (self) alloc: rank * sizeof (unsigned)];
  
  {
    unsigned dimnum;
    
    elementCount = 1;
    for (l = array, dimnum = 0; archiver_list_p (l); l = [l getFirst], dimnum++)
      {
        dims[dimnum] = [l getCount];
        elementCount *= dims[dimnum];
      }
  }

  {
    char *typebuf = [getZone (self) alloc: 2];
    
    typebuf[0] = [proto getValueType];
    typebuf[1] = '\0';
    type = typebuf;
  }
  elementSize = size_for_objc_type (type);
  {
    size_t size = elementCount * elementSize;

    data = [getZone (self) alloc: size];
    memset (data, 0, size);
  }
  
  {
    unsigned coord[rank];
    
    void expand (id val, unsigned dimnum)
      {
        if (archiver_list_p (val))
          {
            id <Index> li = [val begin: scratchZone];
            id item;
            unsigned pos = 0;
            
            while ((item = [li next]) != nil)
              {
                coord[dimnum] = pos;
                expand (item, dimnum + 1);
                pos++;
              }
            [li drop];
          }
        else
          {
            unsigned i;
            unsigned mult = 1;
            unsigned offset = 0;
            
            offset = coord[rank - 1];

            if (!valuep (val))
              raiseEvent (InvalidArgument, "Array element not a number");

            for (i = rank - 1; i > 0; i--)
              {
                mult *= dims[i];
                offset += coord[i - 1] * mult;
              }
            switch (*type)
              {
              case _C_ID:
                ((id *) data)[offset] = [val getObject];
                break;
	      case _C_CLASS:
		((Class *) data)[offset] = [val getClass];
		break;
              case _C_LNG_LNG:
                ((long long *) data)[offset] = [val getLongLong];
                break;
              case _C_FLT:
                ((float *) data)[offset] = [val getFloat];
                break;
              case _C_DBL:
                ((double *) data)[offset] = [val getDouble];
                break;
              case _C_LNG_DBL:
                ((long double *) data)[offset] = [val getLongDouble];
                break;
              case _C_UCHR:
                ((unsigned char *) data)[offset] = [val getChar];
                break;
              default:
                raiseEvent (InvalidArgument, "Unknown element type");
              }
          }
      }
    expand (array, 0);
  }
  return self;
}

PHASE(Using)

- (void *)getData
{
  return data;
}

- convertToType: (char)destType dest: (void *)ptr
{
  unsigned coord[rank];
  void permute (unsigned dim)
    {
      unsigned i;

      if (dim < rank)
        {
          for (i = 0; i < dims[dim]; i++)
            {
              coord[dim] = i;
              permute (dim + 1);
            }
        }
      else
        {
          unsigned offset = 0;
          unsigned mult = 1;
          long long val;
          
          offset = coord[rank - 1];
          for (i = rank - 1; i > 0; i--)
            {
              mult *= dims[i];
              offset += coord[i - 1] * mult;
            }
          val = ((long long *) data)[offset];
          switch (destType)
            {
            case _C_INT:
              ((int *) ptr)[offset] = (int) val;
              break;
            case _C_UINT:
              ((unsigned *) ptr)[offset] = (unsigned) val;
              break;
            case _C_LNG:
              ((long *) ptr)[offset] = (long) val;
              break;
            case _C_ULNG:
              ((unsigned long *) ptr)[offset] = (unsigned long) val;
              break;
            case _C_LNG_LNG:
              ((long long *) ptr)[offset] = (long long) val;
              break;
            case _C_ULNG_LNG:
              ((unsigned long long *) ptr)[offset] = (unsigned long long) val;
              break;
            case _C_SHT:
              ((short *) ptr)[offset] = (short) val;
              break;
            case _C_USHT:
              ((unsigned short *) ptr)[offset] = (unsigned short) val;
              break;
            default:
              abort ();
            }
        }
    }
  if (*type == _C_LNG_LNG)
    permute (0);
  else
    memcpy (ptr, data, size_for_objc_type (type) * elementCount);
  return self;
}

- (unsigned *)getDims
{
  return dims;
}

- (unsigned)getElementCount
{
  return elementCount;
}

- (size_t)getElementSize
{
  return elementSize;
}

- (char)getArrayType
{
  return *type;
}

- lispOutShallow: (id <OutputStream>)stream
{
  return [self lispOutDeep: stream];
}

- lispOutDeep: (id <OutputStream>)stream
{
  lisp_process_array (objc_type_for_array (type, rank, dims), 
                      data, data, stream, YES);
  return self;
}

- (void)drop
{
  [getZone (self) free: dims];
  [getZone (self) free: data];
  [getZone (self) free: (void *) type];
  [super drop];
}

@end

@implementation ArchiverValue_c
PHASE(Creating)
- setDouble: (double)val
{
  type = _C_DBL;
  value.d = val;
  return self;
}

- setLongDouble: (long double)val
{
  type = _C_LNG_DBL;
  value.ld = val;
  return self;
}

- setFloat: (float)val
{
  type = _C_FLT;
  value.f = val;
  return self;
}

- setLongLong: (long long)val
{
  type = _C_LNG_LNG;
  value.ll = val;
  return self;
}  

- setChar: (unsigned char)val
{
  type = _C_UCHR;
  value.ch = val;
  return self;
}  

- setBoolean: (BOOL)val
{
  type = _C_UCHR;
  value.ch = (unsigned char) val;
  return self;
}

- setNil
{
  type = _C_ID;
  value.obj = nil;
  return self;
}

- setClass: (Class)aClass
{
  type = _C_CLASS;
  value.class = aClass;
  return self;
}

PHASE(Using)

- (char)getValueType
{
  return type;
}

- (long double)getLongDouble
{
  return value.ld;
}

- (double)getDouble
{
  return value.d;
}

- (float)getFloat
{
  return value.f;
}

- (long long)getLongLong
{
  return value.ll;
}

- (int)getInteger
{
  if (type != _C_LNG_LNG)
    raiseEvent (InvalidArgument, "expecting an integer");
  return (int) value.ll;
}

- (unsigned)getUnsigned
{
  if (type != _C_LNG_LNG)
    raiseEvent (InvalidArgument, "expecting an integer");
  return (unsigned) value.ll;
}

- (unsigned char)getChar
{
  return value.ch;
}

- (BOOL)getBoolean
{
  return value.ch;
}

- getObject
{
  return value.obj;
}

- (Class)getClass
{
  return value.class;
}

- lispOutShallow: (id <OutputStream>)stream
{
  return [self lispOutDeep: stream];
}

- lispOutDeep: (id <OutputStream>)stream
{
  switch (type)
    {
    case _C_CHR:
    case _C_UCHR:
      [stream catChar: value.ch];
      break;
    case _C_DBL:
      [stream catDouble: value.d];
      break;
    case _C_LNG_DBL:
      [stream catLongDouble: value.ld];
      break;
    case _C_FLT:
      [stream catFloat: value.f];
      break;
    case _C_LNG_LNG:
      [stream catLongLong: value.ll];
      break;
    default:
      [stream catC: "serialization for this type not implemented yet"];
      break;
    }
  return self;
}

@end

@implementation ArchiverPair_c
PHASE(Creating)

- setCar: val;
{
  car = val;
  return self;
}

- setCdr: val
{
  cdr = val;
  return self;
}

- setConsFormatFlag: (BOOL)theConsFormatFlag
{
  consFormatFlag = theConsFormatFlag;
  return self;
}

PHASE(Using)
- getCar
{
  return car;
}

- getCdr
{
  return cdr;
}

- (BOOL)getConsFormatFlag
{
  return consFormatFlag;
}

- lispOutShallow: (id <OutputStream>)stream
{
  return [self lispOutDeep: stream];
}

- lispOutDeep: (id <OutputStream>)stream
{
  [stream catC: (consFormatFlag ? "(cons " : "(")];
  [car lispOutDeep: stream];
  [stream catC: (consFormatFlag ? " ": " . ")];
  [cdr lispOutDeep: stream];
  [stream catC: ")"];
  return self;
}
@end

@implementation ArchiverList_c
PHASE(Creating)
PHASE(Using)
- lispOutShallow: (id <OutputStream>)stream
{
  [self lispOutDeep: stream];
  return self;
}

- lispOutDeep: (id <OutputStream>)stream
{
  id index = [self begin: getZone (self)];
  id member;

  [stream catC: "("];

  // get the first member of the list
  member = [index next];

  if (list_literal_p (member))
    [stream catC: [member getC]];
  else if (cons_literal_p (member))
    [stream catC: [member getC]];
  else if (stringp (member))
    {
      const char *funcName = [member getC];

      if (strcmp (funcName, MAKE_INSTANCE_FUNCTION_NAME) == 0
          || strcmp (funcName, MAKE_CLASS_FUNCTION_NAME) == 0) 
         {
           [stream catC: [member getC]];
           member = [index next];
           [stream catC: " "];
           [member lispOutDeep: stream];
         }
      else if (strcmp (funcName, PARSE_FUNCTION_NAME) == 0)
        [stream catC: [member getC]];        
      else
        raiseEvent (InvalidArgument, "function not one of ",
                    MAKE_INSTANCE_FUNCTION_NAME
                    " or "
                    MAKE_CLASS_FUNCTION_NAME
                    " or "
                    PARSE_FUNCTION_NAME);
    }
  else
    [member lispOutDeep: stream];
  
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    {
      [stream catC: " "];
      if (member == (id) ArchiverEOL)
        break;
      else if (keywordp (member)
               || valuep (member)
               || arrayp (member)
               || pairp (member)
               || quotedp (member)
               || stringp (member)
               || archiver_list_p (member))
        [member lispOutDeep: stream];
      else
        raiseEvent (InvalidArgument, "expression type not supported");
    }
  [stream catC: ")"];
  [index drop];
  return self;
}
@end

@implementation ArchiverQuoted_c
PHASE(Creating)
- setQuotedObject: aValue
{
  value = aValue;
  return self;
}

PHASE(Using)
- getQuotedObject
{
  return value;
}

- lispOutDeep: (id <OutputStream>)stream
{
  [stream catC: "'"];
  if (stringp (value))
    [stream catC: [value getC]];
  else
    [value lispOutDeep: stream];
  return self;
}
@end
