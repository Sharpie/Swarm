// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#include <stdio.h>

#import <swarmobject/Probe.h>

// SAFEPROBES enables lots of error checking here.
#define SAFEPROBES 1

@implementation Probe

-setProbedClass: (Class) aClass {
  if (SAFEPROBES) {
    if (probedClass != 0) {
      fprintf(stderr, "It is an error to reset the class\n");
      return nil;
    }
  }
  probedClass = aClass;
  return self;
}

-(Class) getProbedClass {
  return probedClass;
}

-createEnd {

  //return full description by default...
  stringReturnType = DefaultString ;

  //by default we do not want to bother checking the class is valid
  //each time the probe is used...
  safety = 0 ;

  return self;
}

-(char *) getProbedType {
  return probedType;
}

-setStringReturnType: returnType{
  stringReturnType = returnType ;
  return self ;
}

-setSafety {
  safety = 1 ;
  return self ;
}

-unsetSafety {
  safety = 0 ;
  return self ;
}

-clone: aZone {
  [self subclassResponsibility: @selector(clone)] ;
  return self ;
}

-(void *) probeRaw: (id) anObject {
  [self subclassResponsibility: @selector(probeRaw)] ;
  return self ;
}

-(void *) probeAsPointer: (id) anObject {
  [self subclassResponsibility: @selector(probeAsPointer)] ;
  return self ;
}

-(int) probeAsInt: (id) anObject {
  [self subclassResponsibility: @selector(probeAsInt)] ;
  return 0 ;
}

-(double) probeAsDouble: (id) anObject {
  [self subclassResponsibility: @selector(probeAsDouble)] ;
  return 0.0 ;
}

-(char *) probeAsString: (id) anObject Buffer: (char *) buf {
  [self subclassResponsibility: @selector(probeAsString)] ;
  return NULL ;
}

@end
