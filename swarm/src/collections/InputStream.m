// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         InputStream.m
Description:  character string object   
Library:      collections
*/

#import <collections.h>
#import <collections/StringObject.h> // setLiteralFlag:
#import <collections/OutputStream.h>
#import <collections/InputStream.h>
#include <objc/objc-api.h> // type definitions
#include <misc.h> // errno, fputs, isspace, isdigit
#include <collections/predicates.h>
#include <defobj/internal.h>  // lisp_process_array

@implementation InputStream_c

PHASE(Creating)

+ createBegin: aZone
{
  InputStream_c *newStream = [aZone allocIVars: self];
  return newStream;
}

- (void)setFileStream: (FILE *)file
{
  fileStream = file;
}

- createEnd
{
  createByCopy ();
  setNextPhase (self);
  return self;
}

+ create: aZone setFileStream: (FILE *)file
{
  InputStream_c *newStream;

  newStream = [aZone allocIVars: getNextPhase (self)];
  newStream->fileStream = file;
  return newStream;
}

PHASE(Using)

- (FILE *)getFileStream
{
  return fileStream;
}

static id
readString (id inStream, BOOL literalFlag)
{
  int c;
  id string;
  FILE *fp = [inStream getFileStream];
  char buf[2];

  string = [String createBegin: [inStream getZone]];
  [string setC: ""];
  [string setLiteralFlag: literalFlag];
  string = [string createEnd];
  
  buf[1] = '\0';
  while ((c = fgetc (fp)) != EOF
         && !(literalFlag 
              ? c == '"'
              : (isspace (c) || c == '(' || c == ')')))
    {
      buf[0] = c;
      [string catC: buf];
    }
  if (c == EOF)
    {
      [string drop];
      return nil;
    }
  else if (!literalFlag)
    ungetc (c, fp);
  return string;
}

- (void)_unexpectedEOF_
{
  raiseEvent (InvalidArgument, "unexpected EOF");
}

- (void)_badType_ : obj
{
  raiseEvent (InvalidArgument, "wrong type");
}
  
- getExpr
{
  int c;
  id aZone = [self getZone];
  
  while (((c = fgetc (fileStream)) != EOF) && isspace (c));
  if (c == EOF)
    return nil;
  else if (c == '\'')
    return [self getExpr];
#if 0
  else if (c == 'n' || c == 'N')
    {
      id <String> remain = readString (self, 0);

      if (strcmp ([remain getC], "il") != 0)
        {
          id <String> newString = [String create: aZone setC: "n"];

          [newString catC: [remain getC]];
          return newString;
        }
      else
        return [[[ArchiverValue createBegin: aZone]
                  setNil] createEnd];
    }
#endif
  else if (c == ':')
    {
      id newObj = readString (self, 0);
      
      if (newObj == nil)
        [self _unexpectedEOF_];
      
      return [[[ArchiverKeyword createBegin: aZone]
                setKeywordName: [newObj getC]]
               createEnd];
    }
  else if (c == '#')
    {
      int c2 = fgetc (fileStream);

      if (c2 == ':')
        {
          id newObj = readString (self, 0);
          
          if (newObj == nil)
            [self _unexpectedEOF_];
          
          return [[[ArchiverKeyword createBegin: aZone]
                    setKeywordName: [newObj getC]]
                   createEnd];
        }
      else if (c2 >= '0' && c2 <= '9')
        {
          unsigned rank;

          ungetc (c2, fileStream);
          {
            int ret = fscanf (fileStream, "%u", &rank);
            
            if (ret != 1)
              raiseEvent (InvalidArgument,
                          "Unable to scan array dimensions [ret = %d]", ret);
          }
          
          {
            id newObj = [self getExpr];
            
            if (newObj == nil)
              [self _unexpectedEOF_];
            
            return [[[ArchiverArray createBegin: aZone]
                      setArray: newObj]
                     createEnd];
          }
        }
      else if (c2 == '\\')
        {
          unsigned val;
          unsigned char c3;

          c3 = fgetc (fileStream);

          if (c3 >= '0' && c3 <= '3')
            {
              int ret;

              ungetc (c3, fileStream);
              ret = fscanf (fileStream, "%o", &val);
              
              if (ret != 1)
                raiseEvent (InvalidArgument,
                            "Unable to scan octal character value");
              c3 = (unsigned char)val;
            }
          return [[[ArchiverValue createBegin: aZone]
                    setChar: c3] createEnd];
        }
      else if (c2 == 't' || c2 == 'f')
        return [[[ArchiverValue createBegin: aZone]
                  setBoolean: (c2 == 't')] createEnd];
      else
        raiseEvent (InvalidArgument, "Unknown `#' form");
    }
  else if (c == '(')
    {
      id list = [List create: aZone];
        
        while (YES)
        {
          id newObj = [self getExpr];

          if (newObj == nil)
            [self _unexpectedEOF_];
          if (newObj == (id) ArchiverEOL)
            break;
          [list addLast: newObj];
        }
      
      if (ARCHIVERDOTP ([list atOffset: 1]) && [list getCount] == 3)
        {
          id pair = [ArchiverPair createBegin: aZone];

          [pair setCar: [list getFirst]];
          [pair setCdr: [list getLast]];

          pair = [pair createEnd];
          [list drop];
          return pair;
        }
      else if (cons_literal_p ([list getFirst]))
        {
          id pair;

          if ([list getCount] != 3)
            raiseEvent (InvalidArgument, "cons accepts only two arguments");
          
          pair = [ArchiverPair createBegin: aZone];
          
          [pair setCar: [list atOffset: 1]];
          [pair setCdr: [list atOffset: 2]];
          pair = [pair createEnd];
          [list drop];
          return pair;
        }
      return list;
    }
  else if (c == ')')
    return ArchiverEOL;
  else if (c == '.')
    return ArchiverDot;
  else if (c == '"')
    {
      id string = readString (self, 1);

      if (string)
        return string;
      else
        [self _badType_ : string];
    }
  else if (c == ';')  // Lisp comment
    {
      while (YES)  // suck up the rest of line
        {
          c = fgetc (fileStream);
          if (c == '\n')            // end of comment (marked by end of line)
            return [self getExpr];  // re-commence search 
          else if (c == EOF)
            return nil;
        } 
    }
  else
    {
      id string;
      BOOL isNumeric = YES;
      char type = _C_INT;

      ungetc (c, fileStream);
      string = readString (self, 0);

      {
        const char *str = [string getC];
        size_t len = strlen (str);
        size_t pos;
        
        for (pos = 0; pos < len; pos++)
          {
            char ch = str[pos];
            
            if (ch == '.')
              type = _C_DBL;
            else if (!isdigit ((int)ch) && !(pos == 0 && ch == '-'))
              {
                if (pos == len - 2)
                  {
                    if (ch == 'F')
                      type = _C_FLT;
                    else if (ch == 'D')
                      type = _C_DBL;
                    else
                      {
                        isNumeric = NO;
                        break;
                      }
                  }
                else
                  {
                    isNumeric = NO;
                    break;
                  }
              }
          }
      
        if (isNumeric)
          {
            id number = [ArchiverValue createBegin: aZone];
            
            if (type == _C_DBL || type == _C_FLT)
              {
                double val;
                
                errno = 0;
                val = strtod (str, NULL);
                if (errno != 0)
                  raiseEvent (InvalidArgument, "Could not convert to double");
                if (type == _C_FLT)
                  [number setFloat: (float)val];
                else
                  [number setDouble: val];
              }
            else if (type == _C_INT)
              {
                long val;

                errno = 0;
                val = strtol (str, NULL, 10);
                if (errno != 0)
                  raiseEvent (InvalidArgument, "Could not convert to long");
                [number setInteger: (int)val];
              }
            else
              abort ();
            return [number createEnd];
          }
      }
      if (string)
        return string;
      else
        [self _badType_ : string];
    }
  raiseEvent (LoadError, "Unexpected character `%c'\n");
  return nil;
}

@end

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
  
  for (l = array, rank = 0; listp (l); rank++)
    l = [l getFirst];

  proto = l;

  if (!valuep (proto))
    raiseEvent (InvalidArgument, "Array element not numeric");
  
  dims = xcalloc (rank, sizeof (unsigned));
  
  {
    unsigned dimnum;
    
    elementCount = 1;
    for (l = array, dimnum = 0; listp (l); l = [l getFirst], dimnum++)
      {
        dims[dimnum] = [l getCount];
        elementCount *= dims[dimnum];
      }
  }
  
  switch ([proto getValueType])
    {
    case _C_ID:
      elementSize = sizeof (id);
      break;
    case _C_INT:
      elementSize = sizeof (int);
      break;
    case _C_DBL:
      elementSize = sizeof (double);
      break;
    case _C_FLT:
      elementSize = sizeof (float);
      break;
    case _C_UCHR:
      elementSize = sizeof (unsigned char);
      break;
    default:
      raiseEvent (InvalidArgument, "Unknown number type");
    }

  type = [proto getValueType];
  
  data = xcalloc (elementCount, elementSize);
  
  {
    unsigned coord[rank];
    
    void expand (id val, unsigned dimnum)
      {
        if (listp (val))
          {
            id <Index> li = [val begin: [val getZone]];
            id item;
            unsigned pos = 0;
            
            while ((item = [li next]) != nil)
              {
                coord[dimnum] = pos;
                expand (item, dimnum + 1);
                pos++;
              }
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
            switch ([val getValueType])
              {
              case _C_ID:
                ((id *) data)[offset] = [val getObject];
                break;
              case _C_INT:
                ((int *) data)[offset] = [val getInteger];
                break;
              case _C_FLT:
                ((float *) data)[offset] = [val getFloat];
                break;
              case _C_DBL:
                ((double *) data)[offset] = [val getDouble];
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
  return type;
}

- lispOutShallow: (id <OutputStream>)stream
{
  return [self lispOutDeep: stream];
}

- lispOutDeep: (id <OutputStream>)stream
{
  char buf[1 + 1];
  
  sprintf(buf, "%c", type);
  lisp_process_array (objc_type_for_array (buf, rank, dims), 
                      data, data, stream, YES);
  return self;
}

- (void)drop
{
  xfree (dims);
  xfree (data);
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

- setFloat: (float)val
{
  type = _C_FLT;
  value.f = val;
  return self;
}

- setInteger: (int)val
{
  type = _C_INT;
  value.i = val;
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
  value.ch = (unsigned char)val;
  return self;
}

- setNil
{
  type = _C_ID;
  value.obj = nil;
  return self;
}

PHASE(Using)

- (char)getValueType
{
  return type;
}

- (double)getDouble
{
  return value.d;
}

- (float)getFloat
{
  return value.f;
}

- (int)getInteger
{
  return value.i;
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

- lispOutShallow: (id <OutputStream>)stream
{
  return [self lispOutDeep: stream];
}

- lispOutDeep: (id <OutputStream>)stream
{
  switch (type)
    {
    case _C_ID:
      {
        [stream catC: "id is of type = "];
        // [stream catC: [value.obj getType]];
        [stream catC: ""];
        break;
      }
    case _C_DBL:
      [stream catDouble: value.d];
      break;
    case _C_FLT:
      [stream catFloat: value.f];
      break;
    case _C_INT:
      [stream catInt: value.i];
      break;
    case _C_CHR:
    case _C_UCHR:
      [stream catChar: value.ch];
      break;
    default:
      [stream catC: "not implemented yet"];
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

PHASE(Using)
- getCar
{
  return car;
}

- getCdr
{
  return cdr;
}

static void
lisp_output_one_of_pair (id obj, id stream)
{
  if (listp (obj))
    [stream catExpr: obj];
  else if (stringp (obj) || valuep (obj) || arrayp (obj))
    [obj lispOutDeep: stream];
  else 
    raiseEvent(InvalidArgument, "one of ArchiverPair_c can't be serialized");
}

- lispOutShallow: (id <OutputStream>)stream
{
  return [self lispOutDeep: stream];
}

- lispOutDeep: (id <OutputStream>)stream
{
  [stream catC: "(cons "];
  lisp_output_one_of_pair (car, stream);
  [stream catC: " "];
  lisp_output_one_of_pair (cdr, stream);
  [stream catC: ")"];
  return self;
}

@end
