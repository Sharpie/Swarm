// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/OutFile.h>
#include <stdio.h>

id <Warning> CannotOpenOutFile;

@implementation OutFile

+ create: aZone withName: (const char *)theName
{
  FILE *aFile = fopen (theName, "w");

  if (aFile == NULL)
    {
      [CannotOpenOutFile
        raiseEvent: "Unable to open %s as an OutFile object!\n",
        theName];
      return nil;
    }
  
  return [[self create: aZone] _setFile_: aFile];
}

- _setFile_: (FILE *)aFile
{
  theFile = aFile;
  return self;
}

- (FILE *)_getFile_
{
  return theFile;
}

- putString: (const char *)aString
{
  fprintf (theFile, "%s", aString);
  return self;
}

- putInt: (int)anInt
{
  fprintf (theFile, "%d", anInt);
  return self;
}

- putUnsigned: (unsigned)anUnsigned
{
  fprintf (theFile, "%u", anUnsigned);
  return self;
}

- putLong: (long)aLong
{
  fprintf (theFile, "%ld", aLong);
  return self;
}

- putUnsignedLong: (unsigned long)anUnsLong
{
  fprintf(theFile, "%lu", anUnsLong);
  return self;
}

- putDouble: (double)aDouble
{
  fprintf(theFile, "%+24.16e", aDouble);
  return self;
}

- putFloat: (float)aFloat
{
  fprintf(theFile, "%+15.7e", aFloat);
  return self;
}

- putChar: (char)aChar
{
  fprintf (theFile, "%c", aChar);
  return self;  
}

- putNewLine
{
  fprintf (theFile, "\n");
  return self;
}

- putTab
{
  fprintf (theFile, "\t");
  return self;
}

- (void)drop
{
  fclose (theFile);
  [super drop];
}

@end
