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

//S: A (singleton) Class, whose instance is used as a container for a global
//S: mapping between classnames and their 'default' ProbeMaps. These defaults
//S: can be changed by the user, thus allowing him/her to customize the default
//S: contents of the ProbeDisplays generated when probing objects.
//D: The normal Swarm simulation will probably only ever contain one instance 
//D: of this class, namely the probeLibrary object. This object is used
//D: for Library Generation of Probes and ProbeMaps: its role is to cache one 
//D: unique "official" ProbeMap for every Class ever probed during a
//D: run of Swarm. These ProbeMaps are generated as they are requested. 
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

//M: The setDisplayPrecision: method sets the number of significant digits for 
//M: floating point and double floating point numbers displayed on GUI widgets.
//M: This method is currently only implemented for VarProbes. It has not been 
//M: implemented for MessageProbes yet. 
//M: The setDisplayPrecision method allows all probes checked out from the 
//M: global ProbeLibrary instance to access this displayed precision. However, 
//M: individual probes can vary from this global default, by using the 
//M: setFloatFormat method on a exisiting probe. 
- setDisplayPrecision: (int)nSigDisplay
{
  sigFigsDisplay = nSigDisplay;
  return self;
}

//M: The getDisplayPrecision method gets the current display precision set in 
//M: the ProbeLibrary instance.
- (int)getDisplayPrecision
{
  return sigFigsDisplay;
}

//M: The setSavedPrecision: method sets the number of significant digits saved 
//M: for floating-point and double floating-point numbers through ObjectSaver. 
//M: This function sets the global default precision for all floating point 
//M: numbers, including double floating point numbers. This floating point 
//M: precision affects all numbers saved via the ObjectSaver class. There is 
//M: currently no way to override this global default for an individual probe. 
- setSavedPrecision: (int)nSigSaved
{
  sigFigsSaved = nSigSaved;
  return self;
}

//M: The getSavedPrecision method gets the current saved precision set in the 
//M: ProbeLibrary instance.
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

//M: The isProbeMapDefinedFor: method returns True if there is a non-nil value 
//M: in the ProbeLibrary for that class and False otherwise.
- (BOOL)isProbeMapDefinedFor: (Class) aClass
{
  return ([classMap at: aClass] != nil) ;
}

//M: The getProbeMapFor: method returns a ProbeMap for the aClass class. If a 
//M: specific ProbeMap has been designed and installed in the ProbeLibrary for 
//M: that class, then that specific ProbeMap is returned. If a custom ProbeMap 
//M: was not designed and installed, then a CompleteProbeMap is created and 
//M: returned.
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

//Since ProbeLibrary is the source of all probes, I am adding methods for
//making complete probemaps as well, even though they are not cached...

//M: The getCompleteProbeMapFor: method returns a ProbeMap containing Probes 
//M: for all the instance variables and messages of the given Class (including 
//M: inherited variables and messages). The current implementation of 
//M: ProbeLibrary does not cache CompleteProbeMaps. 
- getCompleteProbeMapFor: (Class) aClass
{
  id <ProbeMap> temp;

  temp = [CompleteProbeMap createBegin: [self getZone]];
  [temp setProbedClass: aClass];
  if (objectToNotify != nil) [temp setObjectToNotify: objectToNotify];
  return [temp createEnd];
}

//M: The getCompleteVarMapFor: method returns a ProbeMap containing Probes for 
//M: all the instance variables of the given Class (including inherited 
//M: variables) but does not include any MessageProbes. 
- getCompleteVarMapFor: (Class) aClass
{
  id <ProbeMap> temp;

  temp = [CompleteVarMap createBegin: [self getZone]];
  [temp setProbedClass: aClass];
  if (objectToNotify != nil)
    [temp setObjectToNotify: objectToNotify];
  return [temp createEnd];
}

//M: The getProbeForVariable:inClass: method returns a probe that has been 
//M: "checked out" from the appropriate Probes in the probe library. 
//M: Note: The returned probe will be cached so to avoid affecting the results 
//M:       of future requests for the same probes, clone the probe prior to 
//M:       making modifications to the probe.
- getProbeForVariable: (const char *)aVariable inClass: (Class) aClass
{
  return [[self getProbeMapFor: aClass] 
           getProbeForVariable: (const char *)aVariable];
}

//M: The getProbeForMessage:inClass: method returns a probe that has been 
//M: "checked out" from the appropriate Probes in the probe library. 
//M: Note: The returned probe will be cached so to avoid affecting the results 
//M:       of future requests for the same probes, clone the probe prior to 
//M:       making modifications to the probe.
- getProbeForMessage: (const char *)aMessage inClass: (Class) aClass
{
  return [[self getProbeMapFor: aClass]
           getProbeForMessage: (const char *)aMessage];
}

//M: The setProbeMap:For: method sets the standard probe map as the probe map.
//M: The returned Probe will be cached as though it was produced by the
//M: library itself.
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
