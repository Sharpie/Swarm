// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Uname.m
Description:  Unique Name Generator
Library:      simtools
*/

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <stdlib.h>
#import <string.h>
#import <collections/String.h>
#import "UName.h"

@implementation UName

+ create: aZone setBaseName: (const char *)aString
{
  id obj;
  
  obj = [super createBegin: aZone];
  [obj setBaseName: aString];
  [obj createEnd];
  return obj;
}

+ create: aZone setBaseNameObject: aStringObject
{
  id obj;
  
  obj = [super create: aZone];
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
    {
      fprintf (stderr,
               "No Base Name was given when creating a UName object...\n");
      exit(-1);
    }
  
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
  
  [aCopy appendC: suffix];

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
  
  [aCopy appendC: suffix];

  return aCopy;
}

@end
