// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools.h> // OutFile
#import <simtools/OutFile.h>

// This object opens the designated file in `append' mode
// rather than `create' mode, thus not wiping out
// an existing file.

@interface AppendFile: OutFile <OutFile>
{
}

+ create: aZone setName: (const char *)theName;
+ create: aZone withName: (const char *)theName;

@end
