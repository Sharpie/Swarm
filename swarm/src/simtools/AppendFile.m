// Swarm library. Copyright © 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/AppendFile.h>

#include <misc.h> // fopen

#import <defobj.h> // ObsoleteFeature, raiseEvent

@implementation AppendFile
PHASE(Creating)

+ create: aZone setName: (const char *)theName
{
  FILE *aFile = fopen (theName, "a");	// opens in "a" - append mode

  if (aFile == NULL)
    return nil;
  
  return [[self create: aZone] _setFile_: aFile];
}

+ create: aZone withName: (const char *)theName
{
  raiseEvent (ObsoleteMessage, "please use +create:setName: instead\n");
  return [self create: aZone setName: theName];
}

PHASE(Using)

@end
