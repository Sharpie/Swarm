// Swarm library. Copyright (C) 1996 Santa Fe Institute.
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

#import <simtools/ProbeDisplayManager.h>
#import <simtools/ProbeDisplay.h>
#import <collections.h>

@implementation ProbeDisplayManager

-createEnd {
  probeList = [List create: zone];
  return self;
}

-addProbeDisplay: pd {
  [probeList addLast: pd];
  return self;
}

-dropProbeDisplaysFor: anObject {
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

-removeProbeDisplay: pd {
  [probeList remove: pd];
  return self;
}

-update {
  [probeList forEach: @selector(update)];
  return self;
}

-createProbeDisplayFor: (id) anObject {
  if ([anObject respondsTo: @selector(getProbeMap)])
    return [[[[ProbeDisplay createBegin: [self getZone]]
	       setProbedObject: anObject]
	      setProbeMap: [anObject getProbeMap]]
	     createEnd];
  else
    return [self createCompleteProbeDisplayFor: (id) anObject];
}

-createCompleteProbeDisplayFor: (id) anObject {
  return [[[ProbeDisplay createBegin: [self getZone]]
	    setProbedObject: anObject]
	   createEnd];
}
 
@end
