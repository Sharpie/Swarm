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
