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
#include <stdio.h> // fputs
#include <ctype.h> // isspace

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
      id list = [List create: [self getZone]];

      if (c2 == ':')
        {
          id newObj = [self getExpr];
          
          if (newObj == nil)
            [self _unexpectedEOF_];
          
          [list addLast: ArchiverSymbol];
          [list addLast: newObj];
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
            
            [list addLast: ArchiverArray];
            [list addLast: (id)rank];
            [list addLast: newObj];
            xfprint (newObj);
          }
        }
      else
        raiseEvent (InvalidArgument, "Unknown `#' form");
      return list;
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

      ungetc (c, fileStream);
      string = readString (self, 0);
      if (string)
        return string;
      else
        [self _badType_ : string];
    }
  abort ();
}

@end
