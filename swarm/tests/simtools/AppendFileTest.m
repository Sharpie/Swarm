// Swarm library. Copyright (C) 1999, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase.h>
#import <simtools.h>
#import <simtools/AppendFile.h>
#import <simtools/OutFile.h>

#include <misc.h>

int
main(int argc, const char ** argv) 
{
  id outFile;
  id appFile;
  id inFile;
  char aLine[128];

  initSwarmBatch(argc, argv);

  // first, create a file to be "appended" to
  outFile = [OutFile create: globalZone setName: "File.test"];

  [outFile putString: "first thing - should not be overwritten!!\n"];

  [outFile drop]; // close the file

  // open the same file in "append" mode
  appFile = [AppendFile create: globalZone setName: "File.test"];

  [appFile putString: "second thing - should be after first thing!!\n"];

  [appFile drop]; // close the file


  // retrieve the file, using InFile object and test to see the lines
  // were written to the file correctly

  inFile = [InFile create: globalZone setName: "File.test"];

  [inFile getLine: aLine];
  printf("InFile:  %s\n", aLine);

  if (strcmp(aLine, "first thing - should not be overwritten!!") != 0)
    {
      fprintf(stderr, "first line in File.test is wrong");
      return 1;
    }
  
  [inFile skipLine];
  [inFile getLine: aLine];  

  printf("InFile:  %s\n", aLine);
  if (strcmp(aLine, "second thing - should be after first thing!!") != 0)
    {
      fprintf(stderr, "first line in File.test is wrong");
      return 1;
    }
  
  [inFile drop];

  return 0;
}


