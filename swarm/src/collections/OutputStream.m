// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         OutputStream.m
Description:  character string object   
Library:      collections
*/

#import <collections/OutputStream.h>
#import <collections/InputStream.h>
#import <collections/predicates.h>
#include <misc.h> // FILE, fputs

@implementation OutputStream_c

PHASE(Creating)

+ createBegin: aZone
{
  OutputStream_c  *newStream;

  newStream = [aZone allocIVars: self];
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
  OutputStream_c  *newStream;

  newStream = [aZone allocIVars: getNextPhase (self)];
  newStream->fileStream = file;
  return newStream;
}

PHASE(Using)

- (FILE *)getFileStream
{
  return fileStream;
}

- (void)catC: (const char *)cstring
{
  fputs (cstring, fileStream);
}

- (void)catChar: (char)ch
{
  char buf[DSIZE(char) + 1];
  [self catC: "#\\"];
  {
    if (isprint ((unsigned char)ch))
      {
        buf[0] = (unsigned char)ch;
        buf[1] = '\0';
      }
    else
      sprintf (buf, "%03o", (unsigned) ch);
  }
  [self catC: buf];
}

- (void)catDouble: (double)dbl
{
  char buf[DSIZE(double) + 1];
  sprintf (buf, "%fD0", dbl);
  [self catC: buf];
}

- (void)catFloat: (float)flt
{
  char buf[DSIZE(float) + 1];
  sprintf (buf, "%fF0", flt);
  [self catC: buf];
}

- (void)catInt: (int)i
{
  char buf[DSIZE(int) + 1];
  sprintf (buf, "%d", i);
  [self catC: buf];
}

- (void)catUnsigned: (unsigned)un
{
  char buf[DSIZE(unsigned) + 1];
  sprintf (buf, "%u", un);
  [self catC: buf];
}

- (void)catExpr: expr
{
  id index = [expr begin: [self getZone]];
  id member;

  if (!listp (expr))
    raiseEvent(InvalidArgument, "expression must be a list!\n");

  [self catC: "("];

  // get the first member of the list
  member = [index next];
  if (stringp (member))
      [self catC: [member getC]];
  else
    raiseEvent(InvalidArgument, "first argument must be a string!\n");
  
  // advance to the next element in list
  member = [index next];
  if (stringp (member))
    {
      [self catC: " '"];
      [self catC: [member getC]];
    }
  else
    raiseEvent(InvalidArgument, "second argument must be a string!\n");
  
  while ((member = [index next]))
    {
      [self catC: " "];
      if (member == (id) ArchiverEOL)
        break;
      else if (stringp (member))
        {
          [self catC: "\""];
          [self catC: [member getC]];
          [self catC: "\""];
        }
      else if (listp (member))
        [self catExpr: member];
      else if (keywordp (member) || valuep(member) || 
               arrayp(member) || pairp(member))
        [member lispOutDeep: self];
      else
        raiseEvent(InvalidArgument, "expression type not supported");
    }
  [self catC: ")"];
  [index drop];
}

@end
