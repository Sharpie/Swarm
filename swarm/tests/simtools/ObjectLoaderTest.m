// Swarm library. Copyright (C) 1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools.h>
#import "TestObject.h"

#include <misc.h>

int
main(int argc, const char ** argv) 
{
  TestObject * theObj;
  char fileName[128];

  initSwarmBatch(argc, argv);

  theObj = [TestObject createBegin: globalZone];
  theObj =[theObj createEnd];

  sprintf(fileName, "%s/test.data", getenv("srcdir"));
  
  [ObjectLoader load: theObj fromFileNamed: fileName];

  [theObj printObject];

  if ([theObj getVar1] != 2)
    {
      fprintf(stderr, 
              "ObjectLoader incorrectly stored var1 instance variable\n");
      return 1;
    }

  if ([theObj getVar2] != 3)
    {
      fprintf(stderr, 
              "ObjectLoader incorrectly stored var2 instance variable\n");
      return 1;
    }

  if ([theObj getChar1] != 'a')
    {
      fprintf(stderr, 
              "ObjectLoader incorrectly stored char1 instance variable\n");
      return 1;
    }

  return 0;
}


