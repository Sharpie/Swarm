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

#import <simtools/ObjectLoader.h>
#import <simtools/InFile.h>
#import <defobj.h>
#import <objectbase.h> // probeLibrary, arguments

#include <misc.h> // stpcpy

static void
loadAborted (id anObject, const char *filename, unsigned line, const char *arg)
{
  raiseEvent (LoadError,
              "Could not initialize class loader for %s [%s,%d] (%s)\n",
              [anObject name], filename, line, arg);
}


#define ABORT(obj,msg) loadAborted (obj, __FILE__, __LINE__, msg)

@implementation ObjectLoader
PHASE(Creating)

+ createBegin: aZone
{
  ObjectLoader *anObj = [super createBegin: aZone];

  anObj->probeMapCache = nil;

  return anObj;
}

+ load: anObject from: aFileObject
{
  id anObj = [self create: [aFileObject getZone]];

  [anObj setFileObject: aFileObject];
  [anObj loadObject: anObject];
  [anObj drop];

  return self;
}

+ load: anObject fromFileNamed: (const char *)aFileName
{
  id anObj, aFileObject;

  aFileObject = [InFile create: [anObject getZone] setName: aFileName];

  if (!aFileObject)
    ABORT (anObject, aFileName);
      
  anObj = [self create: [aFileObject getZone]];
  [anObj setFileObject: aFileObject];
  [anObj loadObject: anObject];
  [anObj drop];

  [aFileObject drop];

  return self;
}

+ load: anObject fromAppConfigFileNamed: (const char *)aFileName
{
  const char *configPath = [arguments getAppConfigPath];
  char buf[strlen (configPath) + strlen (aFileName) + 1], *p;
  
  p = stpcpy (buf, configPath);
  stpcpy (p, aFileName);

  return [self load: anObject fromFileNamed: buf];
}

+ load: anObject fromAppDataFileNamed: (const char *)aFileName
{
  const char *dataPath = [arguments getAppDataPath];
  char buf[strlen (dataPath) + strlen (aFileName) + 1], *p;
  
  p = stpcpy (buf, dataPath);
  stpcpy (p, aFileName);

  return [self load: anObject fromFileNamed: buf];
}

PHASE(Setting)

- setFileObject: aFileObject
{
  theFileObject = aFileObject;

  return self;
}

PHASE(Using)

- loadObject: anObject
{
  id <ProbeMap> aProbeMap;
  id aProbe;
  char aString[200], aChar;

  if (probeMapCache)
    aProbeMap = probeMapCache;
  else
    aProbeMap = [probeLibrary getCompleteVarMapFor: [anObject class]];
  
  // find the @begin...

  while (1)
    {
      while(1)
        {
          if (!([theFileObject getChar: &aChar]))
            ABORT (anObject, "getChar");
          
          if (aChar == '#')
            {
              if (![theFileObject skipLine])
                ABORT (anObject, "skipLine");
            }
          else
            {
              if (![theFileObject unGetChar: aChar])
                ABORT (anObject, "ungetChar");
              break;
            }
        }
      
      if (![theFileObject getWord: aString])
        ABORT (anObject, "getWord");
      
      if ((aString[0] == '@') && 
          (aString[1] == 'b') &&
          (aString[2] == 'e') &&
          (aString[3] == 'g') &&
          (aString[4] == 'i') &&
          (aString[5] == 'n'))
        {
          if (![theFileObject skipLine])
            ABORT (anObject, "skipLine2");
          break;
        }
    }
  
  while (1)
    {
      while (1)
        { 
          if (!([theFileObject getChar: &aChar]))
            ABORT (anObject, "getChar2");
          
          if (aChar == '#')
            {
              if (![theFileObject skipLine])
                ABORT (anObject, "skipLine3");
            }
          else
            {
              if (![theFileObject unGetChar: aChar])
                ABORT (anObject, "unGetChar2");
              break;
            }
        }
      
      if (![theFileObject getWord: aString])
        ABORT (anObject, "getWord2");
      
      // Check for single quotes that surround the alphanumeric representation
      // of unsigned char variables.
      if (aString[0] == '\'')
        {
          [theFileObject skipLine];
          if (![theFileObject getWord: aString])
            ABORT (anObject, "getWord3");
        }
      
      if ((aString[0] == '@') && 
          (aString[1] == 'e') &&
          (aString[2] == 'n') &&
          (aString[3] == 'd')) 
        {
          [theFileObject skipLine];
          // Note: if this fails we don't care...
          // (users may or may not have a newline
          // character at the end of their last 
          // object specification (last line in file).   
          break;
        }
      
      aProbe = [aProbeMap getProbeForVariable: aString]; 
      if (!aProbe)
        ABORT (anObject, aString);
      
      if (![theFileObject getLine: aString])
        ABORT (anObject, aString);
      
      if (![aProbe setData: anObject ToString: aString])
        ABORT (anObject, aString);
      
      if (![theFileObject skipLine])
        ABORT (anObject, aString);
    }
  
  return self;  
}

- setTemplateProbeMap: (id <ProbeMap>)probeMap
{
  probeMapCache = probeMap;
  return self;
}

- updateCache: exampleTarget 
{
  if (!exampleTarget)
    probeMapCache = nil;
  else 
    probeMapCache = [probeLibrary getCompleteVarMapFor: 
                                    [exampleTarget class]];
  
  return self;
}

@end
