// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/ProbeMap.h>
#import <objectbase/EmptyProbeMap.h>
#import <objectbase/CompleteProbeMap.h>
#import <objectbase/CompleteVarMap.h>
#import <objectbase/ProbeLibrary.h>
#import <collections.h>

@implementation ProbeLibrary

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
  classMap = [[Map createBegin: [self getZone]] createEnd];
  return self;
}

- setDisplayPrecision: (int)nSigDisplay
{
  sigFigsDisplay = nSigDisplay;
  return self;
}

- (int)getDisplayPrecision
{
  return sigFigsDisplay;
}

- setSavedPrecision: (int)nSigSaved
{
  sigFigsSaved = nSigSaved;
  return self;
}

- (int)getSavedPrecision
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

- (BOOL)isProbeMapDefinedFor: (Class) aClass
{
  return ([classMap at: aClass] != nil) ;
}

- getProbeMapFor: (Class) aClass
{
  id ret_val;
  
  if ((ret_val = [classMap at: aClass]) == nil)
    {
      id <ProbeMap> temp;
      temp = [ProbeMap createBegin: [self getZone]];
      [temp setProbedClass: aClass];
      if (objectToNotify != nil)
        [temp setObjectToNotify: objectToNotify];
      temp = [temp createEnd];
      [ classMap at: aClass insert: temp];
    }
  else
    return ret_val;
  return [classMap at: aClass];
}

// Since ProbeLibrary is the source of all probes, I am adding methods for
// making complete probemaps as well, even though they are not cached...

- getCompleteProbeMapFor: (Class) aClass
{
  id <ProbeMap> temp;

  temp = [CompleteProbeMap createBegin: [self getZone]];
  [temp setProbedClass: aClass];
  if (objectToNotify != nil) [temp setObjectToNotify: objectToNotify];
  return [temp createEnd];
}

- getCompleteVarMapFor: (Class) aClass
{
  id <ProbeMap> temp;

  temp = [CompleteVarMap createBegin: [self getZone]];
  [temp setProbedClass: aClass];
  if (objectToNotify != nil)
    [temp setObjectToNotify: objectToNotify];
  return [temp createEnd];
}

- getProbeForVariable: (const char *)aVariable inClass: (Class) aClass
{
  return [[self getProbeMapFor: aClass] 
           getProbeForVariable: (const char *)aVariable];
}

- getProbeForMessage: (const char *)aMessage inClass: (Class) aClass
{
  return [[self getProbeMapFor: aClass]
           getProbeForMessage: (const char *)aMessage];
}

- setProbeMap: aMap For: (Class) aClass
{
  if (objectToNotify != nil)
    [aMap setObjectToNotify: objectToNotify];

  if([classMap at: aClass])
    [classMap at: aClass replace: aMap];
  else
    [classMap at: aClass insert: aMap];

  return self;
}

@end
