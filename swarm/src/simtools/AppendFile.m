// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.


#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <stdio.h>
#import <simtools/AppendFile.h>

@implementation AppendFile

+create: aZone withName: (const char *) theName
{
  FILE *aFile;
  id anObj;
  
  aFile = fopen (theName,"a");	// opens in "a" - append mode				
  if (aFile == NULL)
    {
      fprintf (stderr,
               "Unable to open %s as an AppendFile object!\n",theName);	
      return nil;
    }
  
  anObj = [AppendFile create: aZone];
  [anObj _setFile_: aFile];
  
  return anObj;
}
