// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#include <stdio.h>
#import <swarmobject/EmptyProbeMap.h>

// SAFEPROBES enables lots of error checking here.
#define SAFEPROBES 1

@implementation EmptyProbeMap

-createEnd {
  if (SAFEPROBES) {
    if (probedClass == 0) {
      fprintf(stderr, "ProbeMap object was not properly initialized\n");
      return nil;
    }
  }

  probes = [Map create: [self getZone]] ;
  if (probes == nil)
    return nil;

  numEntries = 0 ;
 
  return self ;
}







