#import <collections/Stream.h>
#import <collections/List.h>
#import <collections/predicates.h>
#import <defobj/defalloc.h>
#import "../defobj/internal.h" // fcall_type_size
#include <defobj/swarm-objc-api.h>

@implementation ArchiverKeyword_c

PHASE(Creating)

- setKeywordName: (const char *)name
{
  keywordName = STRDUP (name);
  return self;
}

PHASE(Using)

- (const char *)getKeywordName
{
  return keywordName;
}

- (void)lispOutShallow: (id <OutputStream>)stream
{
  [self lispOutDeep: stream];
}

- (void)lispOutDeep: (id <OutputStream>)stream
{
  [stream catC: "#:"];
  [stream catC: keywordName];
}

- (void)drop
{
  FREEBLOCK (keywordName);
  [super drop];
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

  type = [proto getValueType];
  elementSize = fcall_type_size (type);
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
            switch (type)
              {
              case fcall_type_object:
                ((id *) data)[offset] = [val getObject];
                break;
	      case fcall_type_class:
		((Class *) data)[offset] = [val getClass];
		break;
              case fcall_type_boolean:
                ((BOOL *) data)[offset] = [val getBoolean];
                break;
              case fcall_type_slonglong:
                ((long long *) data)[offset] = [val getLongLong];
                break;
              case fcall_type_float:
                ((float *) data)[offset] = [val getFloat];
                break;
              case fcall_type_double:
                ((double *) data)[offset] = [val getDouble];
                break;
              case fcall_type_long_double:
                ((long double *) data)[offset] = [val getLongDouble];
                break;
              case fcall_type_schar:
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

- (unsigned)getRank
{
  return rank;
}

- (void *)getData
{
  return data;
}

- convertToType: (fcall_type_t)destType dest: (void *)ptr
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
            case fcall_type_sint:
              ((int *) ptr)[offset] = (int) val;
              break;
            case fcall_type_uint:
              ((unsigned *) ptr)[offset] = (unsigned) val;
              break;
            case fcall_type_slong:
              ((long *) ptr)[offset] = (long) val;
              break;
            case fcall_type_ulong:
              ((unsigned long *) ptr)[offset] = (unsigned long) val;
              break;
            case fcall_type_slonglong:
              ((long long *) ptr)[offset] = (long long) val;
              break;
            case fcall_type_ulonglong:
              ((unsigned long long *) ptr)[offset] = (unsigned long long) val;
              break;
            case fcall_type_sshort:
              ((short *) ptr)[offset] = (short) val;
              break;
            case fcall_type_ushort:
              ((unsigned short *) ptr)[offset] = (unsigned short) val;
              break;
            default:
              abort ();
            }
        }
    }
  if (type == fcall_type_slonglong)
    permute (0);
  else
    memcpy (ptr, data, fcall_type_size (type) * elementCount);
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

- (fcall_type_t)getArrayType
{
  return type;
}

- (void)lispOutShallow: (id <OutputStream>)stream
{
  [self lispOutDeep: stream];
}

- (void)lispOutDeep: (id <OutputStream>)stream
{
  lisp_process_array (rank, dims, type, data, data, stream, YES);
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
  type = fcall_type_double;
  value.d = val;
  return self;
}

- setLongDouble: (long double)val
{
  type = fcall_type_long_double;
  value.ld = val;
  return self;
}

- setBoolean: (BOOL)val
{
  type = fcall_type_boolean;
  value.bool_ = val;
  return self;
}

- setFloat: (float)val
{
  type = fcall_type_float;
  value.f = val;
  return self;
}

- setLongLong: (long long)val
{
  type = fcall_type_slonglong;
  value.ll = val;
  return self;
}  

- setChar: (char)val
{
  type = fcall_type_schar;
  value.ch = val;
  return self;
}  

- setNil
{
  type = fcall_type_object;
  value.obj = nil;
  return self;
}

- setClass: (Class)aClass
{
  type = fcall_type_class;
  value.class = aClass;
  return self;
}

PHASE(Using)

- (fcall_type_t)getValueType
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
  if (type != fcall_type_slonglong)
    raiseEvent (InvalidArgument, "expecting an integer");
  return (int) value.ll;
}

- (unsigned)getUnsigned
{
  if (type != fcall_type_slonglong)
    raiseEvent (InvalidArgument, "expecting an integer");
  return (unsigned) value.ll;
}

- (char)getChar
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

- (void)lispOutShallow: (id <OutputStream>)stream
{
  [self lispOutDeep: stream];
}

- (void)lispOutDeep: (id <OutputStream>)stream
{
  switch (type)
    {
    case fcall_type_boolean:
      [stream catBoolean: value.bool_];
      break;
    case fcall_type_schar: case fcall_type_uchar:
      [stream catChar: value.ch];
      break;
    case fcall_type_double:
      [stream catDouble: value.d];
      break;
    case fcall_type_long_double:
      [stream catLongDouble: value.ld];
      break;
    case fcall_type_float:
      [stream catFloat: value.f];
      break;
    case fcall_type_slonglong:
      [stream catLongLong: value.ll];
      break;
    case fcall_type_object:
      if (value.obj != nil)
        abort ();
      else
        [stream catNil];
      break;
    default:
      [stream catC: "serialization for this type not implemented yet"];
      break;
    }
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

- (void)lispOutShallow: (id <OutputStream>)stream
{
  [self lispOutDeep: stream];
}

- (void)lispOutDeep: (id <OutputStream>)stream
{
  [stream catC: (consFormatFlag ? "(cons " : "(")];
  [car lispOutDeep: stream];
  [stream catC: (consFormatFlag ? " ": " . ")];
  [cdr lispOutDeep: stream];
  [stream catC: ")"];
}
@end

@implementation ArchiverList_c
PHASE(Creating)
PHASE(Setting)
PHASE(Using)
- (void)lispOutShallow: (id <OutputStream>)stream
{
  [self lispOutDeep: stream];
}

- (void)lispOutDeep: (id <OutputStream>)stream
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
      if (ARCHIVEREOLP (member))
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

- (void)lispOutDeep: (id <OutputStream>)stream
{
  [stream catC: "'"];
  if (stringp (value))
    [stream catC: [value getC]];
  else
    [value lispOutDeep: stream];
}
@end
