// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdio.h>
#import <stdlib.h>
#import <swarmobject.h>
#import <simtools.h>
#import <simtools/AppendFile.h>
#import <simtools/OutFile.h>

int
main(int argc, char ** argv) {
  id outFile;
  id appFile;

  initSwarm(argc, argv);

  // first, create a file to be "appended" to
  outFile = [OutFile create: globalZone withName: "File.test"];
  //  outFile =[OutFile createEnd];

  [outFile putString: "first thing - should not be overwritten!!\n"];

  [outFile drop]; // close the file

  // open the same file in "append" mode
  appFile = [AppendFile create: globalZone withName: "File.test"];
  // appFile =[AppendFile createEnd];

  [appFile putString: "second thing - should be after first thing!!\n"];

  [appFile drop]; // close the file

  return 0;
}


