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
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <ctype.h>

@implementation InputStream_c

PHASE(Creating)

+ createBegin: aZone
{
  InputStream_c  *newStream = [aZone allocIVars: self];
  return newStream;
}

- (void) setFileStream: (FILE *)file
{
  fileStream = file;
}

- createEnd
{
  createByCopy( );
  setNextPhase( self );
  return self;
}

+ create: aZone setFileStream: (FILE *)file
{
  InputStream_c  *newStream;

  newStream = [aZone allocIVars: getNextPhase( self )];
  newStream->fileStream = file;
  return newStream;
}

PHASE(Using)

- (FILE *) getFileStream
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

- _unexpectedEOF_
{
  abort ();
}

- _badType_ : obj
{
  abort ();
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
  else if (c == '(')
    {
      id list = [List create : [self getZone]];

      while (1)
        {
          id newObj = [self getExpr];

          if (newObj == nil)
            [self _unexpectedEOF_];
          if (newObj == ArchiverEOL)
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
