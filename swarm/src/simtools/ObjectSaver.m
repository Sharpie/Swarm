// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/ObjectSaver.h>
#import <simtools/OutFile.h>
#import <defobj.h> // SaveError

@implementation ObjectSaver
PHASE(Creating)

+ createBegin: aZone
{
  ObjectSaver *anObj = [super createBegin: aZone];
  anObj->templateProbeMap = nil;

  return anObj;
}

+ save: anObject to: aFileObject
{
  id anObj;

  anObj = [self create: [aFileObject getZone]];
  [anObj setFileObject: aFileObject];
  [anObj saveObject: anObject];
  [anObj drop];
  return self;
}

+ save: anObject to: aFileObject withTemplate: (id <ProbeMap>)aProbeMap
{
  id anObj;

  anObj = [self create: [aFileObject getZone]];
  [anObj setFileObject: aFileObject];
  [anObj setTemplateProbeMap: aProbeMap];
  [anObj saveObject: anObject];
  [anObj drop];

  return self;
}

+ save: anObject toFileNamed: (const char *)aFileName
{
  id anObj, aFileObject;

  aFileObject = [OutFile create: [anObject getZone] setName: aFileName];

  if (!aFileObject)
    [self _crash_: aFileObject];
      
  anObj = [self create: [aFileObject getZone]];
  [anObj setFileObject: aFileObject];
  [anObj saveObject: anObject];
  [anObj drop];
  [aFileObject drop];

  return self;
}

+ save: anObject toFileNamed: (const char *)aFileName
                withTemplate: (id <ProbeMap>)aProbeMap
{
  id anObj;
  id aFileObject;

  aFileObject = [OutFile create: [anObject getZone] setName: aFileName];

  if(!aFileObject)
    [self _crash_: aFileObject];
      
  anObj = [self create: [aFileObject getZone]];
  [anObj setFileObject: aFileObject];
  [anObj setTemplateProbeMap: aProbeMap];
  [anObj saveObject: anObject];
  [anObj drop];
  [aFileObject drop];

  return self;
}

PHASE(Setting)

- setFileObject: aFileObject
{
  theFileObject = aFileObject;

  return self;
}

PHASE(Using)

+ (void)_crash_: anObject
{
  raiseEvent (SaveError, 
              "Could not save %s properly (factory)\n",
                [anObject name]);
}

- (void)_crash_: anObject
{
  raiseEvent (SaveError,
              "Could not save %s properly (instance)\n",
              [anObject name]);
}

- saveObject: anObject
{
  id <ProbeMap> aProbeMap;
  id aProbe, anIndex;
  char aBuffer[2000];

  if (templateProbeMap)
    aProbeMap = templateProbeMap;
  else
    aProbeMap = [probeLibrary getCompleteProbeMapFor: [anObject class]];

  //put the @begin...

  [theFileObject putString: "#Machine Generated Object-IVAR-Dump\n"];
  [theFileObject putString: "@begin\n"];

  anIndex = [aProbeMap begin: [self getZone]];
  while ((aProbe = [anIndex next]))
    if ([aProbe isKindOf: [VarProbe class]])
      if ([aProbe getInteractiveFlag])
        {
          [theFileObject putString: [aProbe getProbedVariable]];
          [theFileObject putTab];
          [theFileObject putString: 
                           [aProbe probeAsString: anObject
                                   Buffer: aBuffer withFullPrecision: 1]];
          [theFileObject putNewLine];
        }
  [anIndex drop];
  
  //put the @end...
  [theFileObject putString: "@end\n"];

  return self;  
}

- setTemplateProbeMap: (id <ProbeMap>)aProbeMap
{
  templateProbeMap = aProbeMap;
  return self;
}

@end
