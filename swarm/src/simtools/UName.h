// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Unique Name Generator -> used to create names (using a base string "critter"
//                          it generates "critter1", "critter2", etc. etc.

#import <objectbase/SwarmObject.h>

@interface UName : SwarmObject
{
  int counter;
  id baseString;
}

+ create: aZone setBaseName: (const char *)aString;
+ create: aZone setBaseNameObject: aStringObject;

- setBaseName: (const char *)aString;
- setBaseNameObject: aStringObject;

- (const char *)getNewName;
- getNewNameObject;

- resetCounter;

@end
