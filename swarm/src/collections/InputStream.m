// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         InputStream.m
Description:  character string object   
Library:      collections
*/

#import <collections.h>
#import <collections/InputStream.h>
#include <objc/objc-api.h> // type definitions
#include <misc.h> // errno, fputs, isspace, isdigit

@implementation InputStream_c

PHASE(Creating)

+ createBegin: aZone
{
  InputStream_c *newStream = [aZone allocIVars: self];
  return newStream;
}

- (void) setFileStream: (FILE *)file
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

  string = [String createBegin : [inStream getZone]];
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
  
  while (((c = fgetc (fileStream)) != EOF) && isspace (c));
  if (c == EOF)
    return nil;
  else if (c == '\'')
    {
      id list = [List create : [self getZone]];
      id obj;

      [list addFirst: ArchiverLiteral];
      obj = [self getExpr];
      if (obj == nil)
        [self _unexpectedEOF_];
      [list addLast: obj];
      return list;
    }
  else if (c == '#')
    {
      int c2 = fgetc (fileStream);

      if (c2 == ':')
        {
          id newObj = [self getExpr];
          
          if (newObj == nil)
            [self _unexpectedEOF_];
          
          return [[[ArchiverKeyword createBegin: [self getZone]]
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
            
            return [[[ArchiverArray createBegin: [self getZone]]
                      setArray: newObj]
                     createEnd];
          }
        }
      else
        raiseEvent (InvalidArgument, "Unknown `#' form");
    }
  else if (c == '(')
    {
      id list = [List create: [self getZone]];
      
      while (1)
        {
          id newObj = [self getExpr];

          if (newObj == nil)
            [self _unexpectedEOF_];
          if (newObj == (id)ArchiverEOL)
            break;
          [list addLast: newObj];
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
  else
    {
      id string;
      BOOL isNumeric = YES;
      char type = _C_INT;
      BOOL isDouble = NO;

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
            else if (!isdigit (ch))
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
            id number = [ArchiverNumber createBegin: [self getZone]];
            
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

  if (!numberp (proto))
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
  
  switch ([proto getNumberType])
    {
    case _C_INT:
      elementSize = sizeof (int);
      break;
    case _C_DBL:
      elementSize = sizeof (double);
      break;
    case _C_FLT:
      elementSize = sizeof (float);
      break;
    default:
      raiseEvent (InvalidArgument, "Unknown number type");
    }
  
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

            if (!numberp (val))
              raiseEvent (InvalidArgument, "Array element not a number");

            for (i = rank - 1; i > 0; i--)
              {
                mult *= dims[i];
                offset += coord[i - 1] * mult;
              }
            switch ([val getNumberType])
              {
              case _C_INT:
                ((int *) data)[offset] = [val getInteger];
                break;
              case _C_FLT:
                ((float *) data)[offset] = [val getFloat];
                break;
              case _C_DBL:
                ((double *) data)[offset] = [val getDouble];
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

- (void)drop
{
  xfree (dims);
  xfree (data);
}

@end

@implementation ArchiverNumber_c

PHASE(Creating)

- setDouble: (double)val
{
  type = _C_DBL;
  number.d = val;
  return self;
}

- setFloat: (float)val
{
  type = _C_FLT;
  number.f = val;
  return self;
}

- setInteger: (int)val
{
  type = _C_INT;
  number.i = val;
  return self;
}

PHASE(Using)

- (char)getNumberType
{
  return type;
}

- (double)getDouble
{
  return number.d;
}

- (float)getFloat
{
  return number.f;
}

- (int)getInteger
{
  return number.i;
}

@end
