// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/CustomProbeMap.h>
#import <defobj.h> // Warning
#import "local.h"

#include <misc.h> // va_list, va_start, va_end

@implementation CustomProbeMap

PHASE(Creating)

+ create: aZone forClass: (Class)aClass withIdentifiers: (const char *)vars, ...
{
  id newCPM;
  va_list argumentPointer;
  const char *identifier;
  
  newCPM = [CustomProbeMap createBegin: aZone];
  [newCPM setProbedClass: aClass];
  newCPM = [newCPM createEnd];
  
  // adding all the variables and methods to be probed
  // this uses a : delimited list of strings of the form:
  //  "var1", "var2", ..., ":", "method1", "method2",..., NULL
  
  va_start (argumentPointer, vars);
  
  // start with the variables
  identifier = vars;
  do
    {
      [newCPM 
        addProbe: 
          [probeLibrary 
            getProbeForVariable: identifier
            inClass: aClass]];
      identifier = va_arg (argumentPointer, const char *);
    } while (identifier[0] != ':' && identifier != NULL);
  
  // now do the methods
  while ((identifier = va_arg (argumentPointer, const char *)) != NULL)
    [newCPM 
      addProbe: 
        [[probeLibrary 
           getProbeForMessage: identifier
           inClass: aClass]
          setHideResult: 0]];
  va_end (argumentPointer);
  
  return newCPM;
}

- createEnd
{
  if (SAFEPROBES)
    if (probedClass == 0)
      {
        [Warning raiseEvent: "ProbeMap object was not properly initialized\n"];
        return nil;
      }
  
  probes = [Map createBegin: [self getZone]];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];
  
  if (probes == nil)
    return nil;

  numEntries = 0;
  
  return self;
}

@end
