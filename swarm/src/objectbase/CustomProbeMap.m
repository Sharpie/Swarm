// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdarg.h>
#import <stdio.h>
#import <objectbase/CustomProbeMap.h>

#import "local.h"

//S: A subclass of ProbeMap, whose initial state is empty unlike the default
//S: probeMap initial state which contains all the VarProbes of the requested
//S: target class.
//D: This subclass of the ProbeMap is used to create probe maps which are 
//D: initialised in an emtpy state or with the VarProbes and MessageProbes 
//D: intended. In other words, the probed class is set, as is the case with 
//D: the normal ProbeMap class but upon createEnd no VarProbes or 
//D: MessageProbes will be present within it. This feature is useful when 
//D: creating a probe map from scratch (e.g. to be used in conjunction with 
//D: the setProbeMap:For: message of the ProbeLibrary). 
@implementation CustomProbeMap

+create: aZone forClass: (Class) aClass withIdentifiers: (char *) vars, ... {
  id newCPM;
  va_list argumentPointer;
  char * identifier;

  newCPM = [CustomProbeMap createBegin: aZone];
  [newCPM setProbedClass: aClass];
  newCPM = [newCPM createEnd];

  // adding all the variables and methods to be probed
  // this uses a : delimited list of strings of the form:
  //  "var1", "var2", ..., ":", "method1", "method2",..., NULL

  va_start(argumentPointer, vars);

  // start with the variables
  identifier = vars;
  do {
    [newCPM 
      addProbe: 
	[probeLibrary 
	  getProbeForVariable: identifier
	  inClass: aClass]];
    identifier = va_arg(argumentPointer, char *);
  }  while (identifier[0] != ':' && 
	    identifier != NULL);

  // now do the methods
  while ((identifier = va_arg(argumentPointer, char *)) != NULL) {
    [newCPM 
      addProbe: 
	[[probeLibrary 
	   getProbeForMessage: identifier
	   inClass: aClass]
	  setHideResult: 0]];
  }
  va_end(argumentPointer);

  return newCPM;
}

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

@end
