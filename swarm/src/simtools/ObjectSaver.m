// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/ObjectSaver.h>
#import <simtools/OutFile.h>

#include <stdio.h>

@implementation ObjectSaver

+ createBegin: aZone
{
  ObjectSaver *anObj;

  anObj = [super createBegin: aZone];
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

+ save: anObject to: aFileObject withTemplate: aProbeMap
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

  aFileObject = [OutFile create: [anObject getZone] withName: aFileName];

  if (!aFileObject)
    [self _crash_: aFileObject];
      
  anObj = [self create: [aFileObject getZone]];
  [anObj setFileObject: aFileObject];
  [anObj saveObject: anObject];
  [anObj drop];
  [aFileObject drop];

  return self;
}

+ save: anObject toFileNamed: (const char *)aFileName withTemplate: aProbeMap
{
  id anObj;
  id aFileObject;

  aFileObject = [OutFile create: [anObject getZone] withName: aFileName];

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

+ (void)_crash_: anObject
{
  if ([anObject respondsTo: M(getInstanceName)])
    fprintf (stderr,"Could not save %s properly...\n",
             [anObject getInstanceName]);
  else
    fprintf (stderr,"Could not save %s properly...\n",
             [anObject name]);
  exit(-1);
}

- (void)_crash_: anObject
{
  if ([anObject respondsTo: M(getInstanceName)])
    fprintf (stderr,"Could not save %s properly...\n",
             [anObject getInstanceName]);
  else
    fprintf (stderr,"Could not save %s properly...\n",
             [anObject name]);
  exit (-1);
}

- setFileObject: aFileObject
{
  theFileObject = aFileObject;

  return self;
}

- saveObject: anObject
{
  id aProbeMap, aProbe, anIndex;
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
      if ([aProbe isInteractive])
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

- setTemplateProbeMap: aProbeMap
{
  templateProbeMap = aProbeMap;
  return self;
}

@end
