// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/ProbeMap.h>
#import <swarmobject/EmptyProbeMap.h>
#import <swarmobject/CompleteProbeMap.h>
#import <swarmobject/ProbeLibrary.h>
#import <collections.h>

@implementation ProbeLibrary

-createEnd {
	classMap = [[Map createBegin: [self getZone]] createEnd] ;
	return self ;
}

-getProbeMapFor: (Class) aClass {

  id ret_val ;

  if( (ret_val = [classMap at: aClass]) == nil){
    [ classMap at: aClass insert: [[[ProbeMap createBegin: [self getZone]]
      setProbedClass: aClass] createEnd] ] ;
  } else {
    return ret_val ;
  }

  return [classMap at: aClass] ;
}

//Since ProbeLibrary is the source of all probes, I am adding methods for
//making complete probemaps as well, even though they are not cached...

-getCompleteProbeMapFor: (Class) aClass {

  return
		[[[CompleteProbeMap createBegin: [self getZone]]
			 setProbedClass: aClass]
			 createEnd] ;
}

-getProbeForVariable: (char *) aVariable inClass: (Class) aClass {
	return
		[[self getProbeMapFor: aClass] getProbeForVariable: (char *) aVariable] ;
}

-getProbeForMessage: (char *) aMessage inClass: (Class) aClass {
  return [[self getProbeMapFor: aClass] getProbeForMessage: (char *) aMessage] ;
}

-setProbeMap: aMap For: (Class) aClass {

  if([classMap at: aClass])
    [classMap at: aClass replace: aMap] ;
  else
    [classMap at: aClass insert: aMap] ;

  return self  ;
}

@end
