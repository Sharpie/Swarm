// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#include <stdio.h>
#import <swarmobject/EmptyProbeMap.h>

// SAFEPROBES enables lots of error checking here.
#define SAFEPROBES 1

//Used in order to ensure that probemaps do not reorder their contents 
//alphabetically...

static int p_compare(id a, id b){
  if(!([a compare: b]))
    return 0 ;
  else
    return -1 ;
}

@implementation EmptyProbeMap

-createEnd {
  if (SAFEPROBES) {
    if (probedClass == 0) {
      fprintf(stderr, "ProbeMap object was not properly initialized\n");
      return nil;
    }
  }

  probes = [Map createBegin: [self getZone]] ;
  [probes setCompareFunction: &p_compare] ;
  probes = [probes createEnd] ;

  if (probes == nil)
    return nil;

  numEntries = 0 ;
 
  return self ;
}

