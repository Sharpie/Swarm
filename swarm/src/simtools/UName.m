// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "UName.h"
#import <collections.h>
#include <misc.h>

id <Error> NoBaseNameForUName;

@implementation UName

+ create: aZone setBaseName: (const char *)aString
{
  id obj = [self createBegin: aZone];

  [obj setBaseName: aString];
  [obj createEnd];

  return obj;
}

+ create: aZone setBaseNameObject: aStringObject
{
  id obj;
  
  obj = [self create: aZone];
  [obj setBaseNameObject: aStringObject];
  [obj createEnd];
  
  return obj;
}

- resetCounter
{
  counter = 0;

  return self;
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
    [NoBaseNameForUName
      raiseEvent:
        "No Base Name was given when creating a UName object...\n"];
  
  [super createEnd];
  [self resetCounter];

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

  result = strdup ([aCopy getC]);

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
