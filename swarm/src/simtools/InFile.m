// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <simtools/InFile.h>
#import <collections.h>

#include <misc.h> // fopen, fgetc, fscanf, fclose, ungetc

#import <defobj.h> // ObsoleteFeature, raiseEvent

@implementation InFile
PHASE(Creating)

+ create: aZone setName: (const char *)theName
{
  FILE *aFile;

  aFile = fopen (theName,"r");
  if (aFile == NULL)
    return nil;
  
  return [[self create: aZone] _setFile_: aFile];
}

+ create: aZone withName: (const char *)theName
{
  raiseEvent (ObsoleteMessage, "please use +create:setName: instead\n");
  return [self create: aZone setName: theName];
}

- _setFile_: (FILE *) aFile
{
  theFile = aFile;
  return self;
}

PHASE(Using)

- (int)getWord: (char *)aWord
{
  int c, pos = 0;

  while ((c = fgetc (theFile)) != EOF
         && (c == ' ' || c == '\t' || c == '\n' || c == '\r'));
  while (c != EOF && !(c == ' ' || c == '\t' || c == '\n' || c == '\r'))
    {
      aWord[pos++] = c;
      c = fgetc (theFile);
    }
  aWord[pos] = '\0';
  if (c == EOF)
    return pos > 0;
  else
    {
      ungetc (c, theFile);
      return 1;
    }
}

- (int)getLine: (char *)aLine 
{
  int c, pos = 0;
  
  while ((c = fgetc (theFile)) != EOF && (c == ' ' || c == '\t'));

  while (c != EOF && c != '\n')
    {
      aLine[pos++] = c;
      c = fgetc (theFile);
    }
  aLine[pos] = '\0';
  if (c == EOF)
    return 0;
  else
    {
      ungetc ('\n', theFile);
      return 1;
    }
}

- (int)getInt: (int *) anInt
{
  return fscanf (theFile, "%d", anInt) != EOF;
}

- (int)getUnsigned: (unsigned *)anUnsigned
{
  return fscanf (theFile, "%u", anUnsigned) != EOF;
}

- (int)getLong: (long *)aLong
{
  return fscanf (theFile, "%ld", aLong) != EOF;
}

- (int)getUnsignedLong: (unsigned long *)anUnsLong
{
  return fscanf (theFile, "%lu", anUnsLong) != EOF;
}

- (int)getDouble: (double *)aDouble
{
  return fscanf (theFile, "%lg", aDouble) != EOF;
}

- (int)getFloat: (float *)aFloat
{
  return fscanf (theFile, "%g", aFloat) != EOF;
}

- (int)getChar: (char *)aChar
{
  int c = fgetc (theFile);

  *aChar = c;
  return c != EOF;
}

- (int)unGetChar: (char)aChar
{
  return ungetc (aChar, theFile) != EOF;
}

- (int)skipLine
{
  int c;
  
  while (1)
    {
      c = fgetc (theFile);
      
      if (c == EOF)
        return 0;
      if (c == '\n')
        return 1;
    }
}

- (void)drop
{
  fclose (theFile);
  [super drop];
}

@end
