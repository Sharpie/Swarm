// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <stdio.h>
#import <simtools/InFile.h>
#import <collections/String.h>

//S: A class to perform file input.
//D: This class is intended to simplify the input file I/O in Swarm. It deals
//D: with the detailed file opening and closing routines thus alleviating the
//D: need for C file I/O procedure calls.
@implementation InFile

//M: The create:withName: method is the create method for InFiles, where 
//M: theName is the name of the file to open.
+ create: aZone withName: (const char *)theName
{
  FILE *aFile;
  id anObj;

  aFile = fopen (theName,"r");
  if (aFile == NULL)
    {
      fprintf (stderr,
               "Unable to open %s as an InFile object!\n",
               theName);
      return nil;
    }
  
  anObj = [InFile create: aZone];
  [anObj _setFile_: aFile];
  
  return anObj;
}

- _setFile_: (FILE *) aFile
{
  theFile = aFile;
  return self;
}

//M: The getWord: method returns a string that does not contain spaces, tabs,
//M: and newlines.
- (int)getWord: (char *)aWord
{
  return fscanf (theFile,"%s",aWord) != EOF;
}

//M: The getLine: method loads the argument string with the characters up to,
//M: but not including a newline character.
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

//M: The getInt: method takes a pointer of type Int and loads it with an 
//M: instance of that type from the open file.  In case of failure, the method
//M: returns 0.
- (int)getInt: (int *) anInt
{
  return fscanf (theFile, "%d", anInt) != EOF;
}

//M: The getUnsigned: method takes a pointer of type unsigned and loads it  
//M: with an instance of that type from the open file. In case of failure, the 
//M: method returns 0.
- (int)getUnsigned: (unsigned *)anUnsigned
{
  return fscanf (theFile, "%u", anUnsigned) != EOF;
}

//M: The getLong: method takes a pointer of type long and loads it with an 
//M: instance of that type from the open file. In case of failure, the method
//M: returns 0.
- (int)getLong: (long *)aLong
{
  return fscanf (theFile, "%ld", aLong) != EOF;
}

//M: The getUnsignedLong: method takes a pointer of type unsigned long and 
//M: loads it with an instance of that type from the open file. In case of
//M: failure, the method returns 0.
- (int)getUnsignedLong: (unsigned long *)anUnsLong
{
  return fscanf (theFile, "%lu", anUnsLong) != EOF;
}

//M: The getDouble: method takes a pointer of type double and loads it with an 
//M: instance of that type from the open file. In case of failure, the method
//M: returns 0.
-(int) getDouble: (double *)aDouble
{
  return fscanf (theFile, "%lg", aDouble) != EOF;
}

//M: The getFloat: method takes a pointer of type float and loads it with an 
//M: instance of that type from the open file. In case of failure, the method
//M: returns 0.
- (int)getFloat: (float *)aFloat
{
  return fscanf (theFile, "%g", aFloat) != EOF;
}

//M: The getChar: method takes a pointer of type char and loads it with an 
//M: instance of that type from the open file. In case of failure, the method
//M: returns 0.
- (int)getChar: (char *)aChar
{
  int c = fgetc (theFile);

  *aChar = c;
  return c != EOF;
}

//M: The unGetChar: method is used by ObjectLoader when backtracking is 
//M: required.
- (int)unGetChar: (char)aChar
{
  return ungetc (aChar, theFile) != EOF;
}

//M: The skipLine method is used by ObjectLoader when skipping lines is 
//M: required.
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

//M: The drop method must be called when the user wants to close the file.
//M: Only by dropping the InFile object will the file truly be closed.
- (void)drop
{
  fclose (theFile);
  [super drop];
}

@end
