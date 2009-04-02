// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

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
  
  baseString = [(id <Copy>)aStringObject copy: [self getZone]];
  
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
