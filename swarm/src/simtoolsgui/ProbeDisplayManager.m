// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// The probe display manager allows you to easily create probe displays.
// Also, it keeps a list of active probes, so it's easy to update them
// all with one "update" to the manager itself.

// Note that we don't maintain the list of probe displays ourselves -
// instead, the create/drop methods in the probe display objects
// themselves call us. (That's because only the probe display object
// itself knows when it's being destroyed, so it has to call us anyway.)

#import <simtoolsgui/ProbeDisplayManager.h>
#import <simtoolsgui/ProbeDisplay.h>
#import <collections.h>

#import <objectbase/DefaultProbeMap.h>
#import <simtoolsgui/CommonProbeDisplay.h>

@implementation ProbeDisplayManager

- createEnd
{
  probeList = [List create: [self getZone]];
  dropImmediatelyFlag = YES;

  return self;
}

- addProbeDisplay: pd
{
  [probeList addLast: pd];

  return self;
}

- dropProbeDisplaysFor: anObject
{
  id index, aProbeDisplay ;
  id reaperQ ;
  
  // We need a reaperQ because there may be more than one ProbeDisplay
  // on a given object... Also, the object will [removeProbeDisplay: self]
  // when asked to -drop.
  
  reaperQ = [List create: [self getZone]] ;

  index = [probeList begin: [self getZone]] ;
  while ( (aProbeDisplay = [index next]) )
    if([aProbeDisplay getProbedObject] == anObject)
      [reaperQ addLast: aProbeDisplay] ;
  [index drop] ;     

  index = [reaperQ begin: [self getZone]] ;
  while ( (aProbeDisplay = [index next]) ){
    [index remove] ;
    [aProbeDisplay drop] ;  
  }
  [index drop] ;     
  [reaperQ drop] ;

  return self ;
}

// just for removing the probe display from the probelist
- removeProbeDisplay: pd
{
  [probeList remove: pd];
  return self;
}

- update
{
  id index;
  id member;

  [probeList forEach: @selector(update)];

  // remove marked probeDisplay

  index = [probeList begin: scratchZone];
  
  while ((member = [index next]))
    if ([member getMarkedForDropFlag])
      {
        [member drop];
        [self removeProbeDisplay: member];
        break;
      }
  [index drop];
  
  return self;
}

- (const char *)_computeProbeDisplayKeyFor_: (const char *)variableName
{
  id key = [String create: [self getZone] setC: "ProbeDisplay-"];
  
  [key catC: variableName];
  return [key getC];
}

- (id <ProbeDisplay>)_createDefaultProbeDisplayFor_     : anObject 
                             setWindowGeometryRecordName: (const char *)windowGeometryRecordName
{
  id tempPD, tempPM;
  
  tempPM = [DefaultProbeMap createBegin: [anObject getZone]];
  [tempPM setProbedClass: [anObject class]];
  [tempPM setObjectToNotify: [probeLibrary getObjectToNotify]];
  tempPM = [tempPM createEnd];

  [probeLibrary setProbeMap: tempPM For: [anObject class]];

  tempPD = [ProbeDisplay createBegin: [self getZone]];
  [tempPD setProbedObject: anObject];
  [tempPD setProbeMap: tempPM];
  [tempPD setWindowGeometryRecordName: windowGeometryRecordName];
  tempPD = [tempPD createEnd];
  
  return tempPD;
  //  return [[[ProbeDisplay createBegin: [self getZone]]
  //	    setProbedObject: anObject]
  //	   createEnd];
}

- (id <ProbeDisplay>)_createProbeDisplayFor_      : anObject
                       setWindowGeometryRecordName: (const char *)windowGeometryRecordName
{
  //  if ([anObject respondsTo: @selector(getProbeMap)]) {
  if (([anObject respondsTo: @selector(getProbeMap)]) &&
      ([probeLibrary isProbeMapDefinedFor: [anObject class]]))
    return [[[[[ProbeDisplay createBegin: [self getZone]]
                setProbedObject: anObject]
               setWindowGeometryRecordName: windowGeometryRecordName]
              setProbeMap: [anObject getProbeMap]]
             createEnd];
  else
    return [self _createDefaultProbeDisplayFor_: anObject
                 setWindowGeometryRecordName: windowGeometryRecordName];
}
  
- (id <ProbeDisplay>)createArchivedProbeDisplayFor: anObject
                                      variableName: (const char *)variableName
{
  return [self _createProbeDisplayFor_: anObject
               setWindowGeometryRecordName:
                 [self _computeProbeDisplayKeyFor_: variableName]];
}

- (id <ProbeDisplay>)createProbeDisplayFor: anObject
{
  return [self _createProbeDisplayFor_: anObject
               setWindowGeometryRecordName: NULL];
}

- (id <ProbeDisplay>)createDefaultProbeDisplayFor: anObject 
{
  return [self _createDefaultProbeDisplayFor_: anObject
               setWindowGeometryRecordName: NULL];
}

- (id <ProbeDisplay>)createArchivedDefaultProbeDisplayFor: anObject 
                                             variableName: (const char *)variableName
{
  return [self _createDefaultProbeDisplayFor_: anObject
               setWindowGeometryRecordName: 
                 [self _computeProbeDisplayKeyFor_: variableName]];
}

- (id <CompleteProbeDisplay>)createArchivedCompleteProbeDisplayFor: anObject
                                                      variableName: (const char *)variableName
{
  return [[[[ProbeDisplay createBegin: [self getZone]]
             setProbedObject: anObject]
            setWindowGeometryRecordName: 
              [self _computeProbeDisplayKeyFor_: variableName]]
           createEnd];
}

- (id <CompleteProbeDisplay>)createCompleteProbeDisplayFor: anObject
{
  return [[[ProbeDisplay createBegin: [self getZone]]
            setProbedObject: anObject]
           createEnd];
}

- (void)setDropImmediatelyFlag: (BOOL)theDropImmediatelyFlag
{
  dropImmediatelyFlag = theDropImmediatelyFlag;
}

- (BOOL)getDropImmediatelyFlag
{
  return dropImmediatelyFlag;
}

@end
