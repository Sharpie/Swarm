// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/SwarmObject.h>
#import <swarmobject/probing.h>

//This means that 'somewhere' there must be a definition of probeLibrary...
//I don't actually import a .h with its definition because it will almost
//certainly contain a reference to a subclass of SwarmObject thereby
//generating a multiple inclusion error (since "SwarmObject.h" is imported
//here and <swarmobject.h> will be imported whereever probeLibrary is actually
//defined...

@implementation SwarmObject

-(const char *) getInstanceName {
	return [self name] ;
}

-getProbeMap {
	return [probeLibrary getProbeMapFor: [self class]] ;
}

-getCompleteProbeMap {
	return [probeLibrary getCompleteProbeMapFor: [self class]] ;
}

-getProbeForVariable: (char *) aVariable {
  return
    [probeLibrary getProbeForVariable: aVariable inClass: [self class]] ;
}

-getProbeForMessage: (char *) aMessage {
  return
    [probeLibrary getProbeForMessage: aMessage inClass: [self class]] ;
}

@end
