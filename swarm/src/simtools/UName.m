// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/UName.h>
#import <collections.h>
#import <defobj.h> // STRDUP
#include <misc.h> // sprintf

@implementation UName

PHASE(Creating)

+ create: aZone setBaseName: (const char *)aString
{
  id obj = [self createBegin: aZone];

  [obj setBaseName: aString];
  return [obj createEnd];
}

+ create: aZone setBaseNameObject: aStringObject
{
  id obj = [self createBegin: aZone];

  [obj setBaseNameObject: aStringObject];
  return [obj createEnd];
}

- setBaseName: (const char *)aString
{
  if (baseString)
    [baseString drop];
  
  baseString = [String create: [self getZone] setC: aString];

  [self resetCounter];

  return self;
}

- setBaseNameObject: aStringObject 
{
  if (baseString)
    [baseString drop];
  
  baseString = [aStringObject copy: [self getZone]];
  
  [self resetCounter];

  return self;
}

- createEnd
{
  if (!baseString)
    raiseEvent (InvalidArgument,
                "No Base Name was given when creating a UName object...\n");
  
  [super createEnd];
  [self resetCounter];

  return self;
}

PHASE(Using)

- resetCounter
{
  counter = 0;

  return self;
}

- (const char *)getNewName
{
  id aCopy;
  char suffix[11];
  char *result;

  aCopy = [String create: [self getZone] setC: [baseString getC]];

  sprintf (suffix,"%d",counter);
  
  counter++;
  
  [aCopy catC: suffix];

  result = STRDUP ([aCopy getC]);

  [aCopy drop];

  return result;
}

- getNewNameObject
{
  id aCopy;
  char suffix[11];

  aCopy = [String create: [self getZone] setC: [baseString getC]];

  sprintf (suffix, "%d", counter);

  counter++;
  
  [aCopy catC: suffix];

  return aCopy;
}

@end
