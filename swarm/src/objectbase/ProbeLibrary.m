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

#import <objectbase/ProbeLibrary.h>
#import <objectbase.h>
#import <collections.h>
#import <defobj/defalloc.h> // getZone
#import <defobj/directory.h> // SD_GETCLASS

@implementation ProbeLibrary
PHASE(Creating)
+ createBegin: aZone
{
  ProbeLibrary *tempObj;

  tempObj = [super createBegin: aZone];
  tempObj->objectToNotify = nil;  // paranoia
  return tempObj;
}

- createEnd
{
  sigFigsDisplay = SIGFIGS_DISPLAYED;
  sigFigsSaved = SIGFIGS_SAVED;
  classMap = [[Map createBegin: getZone (self)] createEnd];
  return self;
}

PHASE(Using)

- setDisplayPrecision: (unsigned)nSigDisplay
{
  sigFigsDisplay = nSigDisplay;
  return self;
}

- (unsigned)getDisplayPrecision
{
  return sigFigsDisplay;
}

- setSavedPrecision: (unsigned)nSigSaved
{
  sigFigsSaved = nSigSaved;
  return self;
}

- (unsigned)getSavedPrecision
{
  return sigFigsSaved;
}

// used internally
- setObjectToNotify: anObject
{
  if (anObject != nil
      && ([anObject 
            respondsTo: 
              M(eventOccurredOn:via:withProbeType:on:ofType:withData:)] == NO))
    raiseEvent (NotImplemented,
                "Object %0#p of class %s does not implement "
                "standard probe hook message.\n", 
                anObject,
                [[anObject class] name]);
  
  objectToNotify = anObject;
  return self;
}

// used internally
- getObjectToNotify
{
  return objectToNotify;
}

- (id <ProbeMap>)_probeMapForObject_: anObject
{
  id ret = [classMap at: anObject];
  
  if (!ret)
    ret = [classMap at: SD_GETCLASS (anObject)];
  
  return ret;
}

- (BOOL)isProbeMapDefinedForObject: anObject
{
  return [self _probeMapForObject_: anObject] != nil;
}

- (BOOL)isProbeMapDefinedFor: (Class)aClass
{
  return ([classMap at: aClass] != nil);
}

- (id <ProbeMap>)getProbeMapForObject: anObject
{
  id ret = [self _probeMapForObject_: anObject];

  if (!ret)
    {
      Class cls = SD_GETCLASS (anObject);

      if (cls)
        ret = [self getProbeMapFor: cls];
      else
        {
          id <ProbeMap> temp = [ProbeMap createBegin: getZone (self)];
          [temp setProbedObject: anObject];
          if (objectToNotify != nil)
            [temp setObjectToNotify: objectToNotify];
          temp = [temp createEnd];
          [classMap at: anObject insert: temp];
          
          ret = temp;
        }
    }
  return ret;
}

- (id <ProbeMap>)getProbeMapFor: (Class)aClass
{
  id ret;
  
  if ((ret = [classMap at: aClass]) == nil)
    {
      id <ProbeMap> temp;
      temp = [ProbeMap createBegin: getZone (self)];
      [temp setProbedClass: aClass];
      if (objectToNotify != nil)
        [temp setObjectToNotify: objectToNotify];
      temp = [temp createEnd];
      [classMap at: aClass insert: temp];
    }
  else
    return ret;
  return [classMap at: aClass];
}

- (id <ProbeMap>)getCompleteProbeMapForObject: anObject
{
  id <ProbeMap> temp;

  temp = [CompleteProbeMap createBegin: getZone (self)];
  [temp setProbedObject: anObject];
  if (objectToNotify != nil)
    [temp setObjectToNotify: objectToNotify];
  return [temp createEnd];
}

- (id <ProbeMap>)getCompleteProbeMapFor: (Class)aClass
{
  id <ProbeMap> temp;

  temp = [CompleteProbeMap createBegin: getZone (self)];
  [temp setProbedClass: aClass];
  if (objectToNotify != nil)
    [temp setObjectToNotify: objectToNotify];
  return [temp createEnd];
}

- (id <ProbeMap>)getCompleteVarMapForObject: anObject
{
  id <ProbeMap> temp;

  temp = [CompleteVarMap createBegin: getZone (self)];
  [temp setProbedObject: anObject];
  if (objectToNotify != nil)
    [temp setObjectToNotify: objectToNotify];
  return [temp createEnd];
}

- (id <ProbeMap>)getCompleteVarMapFor: (Class)aClass
{
  id <ProbeMap> temp;

  temp = [CompleteVarMap createBegin: getZone (self)];
  [temp setProbedClass: aClass];
  if (objectToNotify != nil)
    [temp setObjectToNotify: objectToNotify];
  return [temp createEnd];
}

- (id <VarProbe>)getProbeForVariable: (const char *)aVariable
                             inObject: anObject
{
  return [[self getProbeMapForObject: anObject] 
           getProbeForVariable: (const char *)aVariable];
}

- (id <VarProbe>)getProbeForVariable: (const char *)aVariable
                             inClass: (Class)aClass
{
  return [[self getProbeMapFor: aClass] 
           getProbeForVariable: (const char *)aVariable];
}

- (id <MessageProbe>)getProbeForMessage: (const char *)aMessage
                                inObject: anObject
{
  return [[self getProbeMapForObject: anObject]
           getProbeForMessage: (const char *)aMessage];
}

- (id <MessageProbe>)getProbeForMessage: (const char *)aMessage
                                inClass: (Class)aClass
{
  return [[self getProbeMapFor: aClass]
           getProbeForMessage: (const char *)aMessage];
}

- setProbeMap: (id <ProbeMap>)aMap ForObject: anObject
{
  if ([anObject respondsTo: M(isCOMProxy)]
      || [anObject respondsTo: M(isJavaProxy)])
    {
      if ([classMap at: anObject])
        [classMap at: anObject replace: aMap];
      else
        [classMap at: anObject insert: aMap];
    }
  else
    [self setProbeMap: aMap For: getClass (anObject)];
  return self;
}

- setProbeMap: (id <ProbeMap>)aMap For: (Class)aClass
{
  if (objectToNotify != nil)
    [aMap setObjectToNotify: objectToNotify];

  if ([classMap at: aClass])
    [classMap at: aClass replace: aMap];
  else
    [classMap at: aClass insert: aMap];

  return self;
}

@end
