// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/ObjectLoader.h>
#import <simtools/InFile.h>
#import <objectbase.h> // probeLibrary, arguments

#include <stdio.h>
#include <misc.h> // stpcpy

@implementation ObjectLoader

+ createBegin: aZone
{
  ObjectLoader *anObj;

  anObj = [super createBegin: aZone];
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

  aFileObject = [InFile create: [anObject getZone] withName: aFileName];

  if(!aFileObject)
    [self _crash_: anObject];
      
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

+ (void)_crash_: anObject
{
  [CouldNotInitializeObjectLoader
    raiseEvent: "Could not initialize class loader for %s (factory)\n",
    [anObject name]];
}

- (void)_crash_: anObject
{
  [CouldNotInitializeObjectLoader
    raiseEvent: "Could not initialize class loader for %s (instance)\n",
    [anObject name]];
}

- setFileObject: aFileObject
{
  theFileObject = aFileObject;

  return self;
}

- loadObject: anObject
{
  id aProbeMap, aProbe;
  char aString[200], aChar;

  if(probeMapCache)
    aProbeMap = probeMapCache;
  else
    aProbeMap = [probeLibrary getCompleteVarMapFor: [anObject class]];
  
  // find the @begin...

  while (1)
    {
      while(1)
        {
          if (!([theFileObject getChar: &aChar]))
            [self _crash_: anObject];
          
          if (aChar == '#')
            {
              if (![theFileObject skipLine])
                [self _crash_: anObject];
            }
          else
            {
              if (![theFileObject unGetChar: aChar])
                [self _crash_: anObject];
              break;
            }
        }
      
      if (![theFileObject getWord: aString])
        [self _crash_: anObject];
      
      if ((aString[0] == '@') && 
          (aString[1] == 'b') &&
          (aString[2] == 'e') &&
          (aString[3] == 'g') &&
          (aString[4] == 'i') &&
          (aString[5] == 'n'))
        {
          if (![theFileObject skipLine])
            [self _crash_: anObject];   
          break;
        }
    }
  
  while(1)
    {
      while(1)
        { 
          if (!([theFileObject getChar: &aChar]))
            [self _crash_: anObject];
          
          if (aChar == '#')
            {
              if (![theFileObject skipLine])
                [self _crash_: anObject];
            }
          else
            {
              if (![theFileObject unGetChar: aChar])
                [self _crash_: anObject];
              break;
            }
        }
      
      if (![theFileObject getWord: aString])
        [self _crash_: anObject];
      
      // Check for single quotes that surround the alphanumeric representation
      // of unsigned char variables.
      if (aString[0] == '\'')
        {
          [theFileObject skipLine];
          if (![theFileObject getWord: aString])
            [self _crash_: anObject];
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
        [self _crash_: anObject];
      
      if (![theFileObject getLine: aString])
        [self _crash_: anObject];
      
      if (![aProbe setData: anObject ToString: aString])
        [self _crash_: anObject];
      
      if (![theFileObject skipLine])
        [self _crash_: anObject];
    }
  
  return self;  
}

- setTemplateProbeMap: probeMap
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
