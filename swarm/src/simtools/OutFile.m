// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.


#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <stdio.h>
#import <simtools/OutFile.h>

//S: A class to perform file output.
//D: This class is intended to simplify output file-I/O in Swarm. It 
//D: essentially deals with the detailed file opening and closing routines 
//D: thus alleviating the need for C file I/O procedure calls. 
@implementation OutFile

//M: The create:withName: method opens a file named theName and creates an 
//M: Outfile object.
+ create: aZone withName: (const char *)theName
{
  FILE *aFile;
  id anObj;
  
  aFile = fopen (theName, "w");
  if (aFile == NULL)
    {
      fprintf (stderr,
               "Unable to open %s as an OutFile object!\n",
               theName);
      return nil;
    }
  
  anObj = [OutFile create: aZone];
  [anObj _setFile_: aFile];
  
  return anObj;
}

-_setFile_: (FILE *)aFile
{
  theFile = aFile;
  return self;
}

//M: The putString: method takes an instance of type string and writes it
//M: into the open file.
- putString: (const char *)aString
{
  fprintf (theFile, "%s", aString);
  return self;
}

//M: The putInt: method takes an instance of type int and writes it into
//M: the open file.
- putInt: (int)anInt
{
  fprintf (theFile, "%d", anInt);
  return self;
}

//M: The putUnsigned: method takes an instance of type unsigned and writes it 
//M: into the open file.
- putUnsigned: (unsigned)anUnsigned
{
  fprintf (theFile, "%u", anUnsigned);
  return self;
}

//M: The putLong: method takes an instance of type long and writes it 
//M: into the open file.
- putLong: (long)aLong
{
  fprintf (theFile, "%ld", aLong);
  return self;
}

//M: The putUnsignedLong: method takes an instance of type unsigned long and 
//M: writes it into the open file.
- putUnsignedLong: (unsigned long)anUnsLong
{
  fprintf(theFile, "%lu", anUnsLong);
  return self;
}

//M: The putDouble: method takes an instance of type double and writes it 
//M: into the open file.
- putDouble: (double)aDouble
{
  fprintf(theFile, "%+24.16e", aDouble);
  return self;
}

//M: The putFloat: method takes an instance of type float and writes it 
//M: into the open file.
- putFloat: (float)aFloat
{
  fprintf(theFile, "%+15.7e", aFloat);
  return self;
}

//M: The putChar: method takes an instance of type char and writes it 
//M: into the open file.
- putChar: (char)aChar
{
  fprintf (theFile, "%c", aChar);
  return self;  
}

//M: The putNewline method writes a newline into the open file.
- putNewLine
{
  fprintf (theFile, "\n");
  return self;
}

//M: The putTab method writes a tab into the open file.
- putTab
{
  fprintf (theFile, "\t");
  return self;
}

//M: The drop method closes the open file.  This method must be called to
//M: close the file.
- (void)drop
{
  fclose (theFile);
  [super drop];
}

@end
