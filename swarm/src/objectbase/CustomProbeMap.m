// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/CustomProbeMap.h>
#import <defobj.h> // Warning
#import "local.h"

#include <misc.h> // va_list, va_start, va_end

@implementation CustomProbeMap

static id
addProbesWithIdentifers(id theProbeLibrary,
                        id customProbeMap,
                        Class aClass,
                        const char *vars,
                        va_list argumentPointer)
{
  const char *identifier;
  
  // adding all the variables and methods to be probed this uses a :
  // delimited list of strings of the form: "var1", "var2", ..., ":",
  // "method1", "method2",..., NULL
  
  // start with the variables
  identifier = vars;
  do
    {
      [customProbeMap 
        addProbe: 
          [theProbeLibrary 
            getProbeForVariable: identifier
            inClass: aClass]];
      identifier = va_arg (argumentPointer, const char *);
    } while (identifier && identifier[0] != ':');
  
  // now do the methods
  while ((identifier = va_arg (argumentPointer, const char *)) != NULL)
    [customProbeMap 
      addProbe: 
        [[theProbeLibrary 
           getProbeForMessage: identifier
           inClass: aClass]
          setHideResult: 0]];
  
  return customProbeMap;
}

PHASE(Creating)

+ create: aZone forClass: (Class)aClass withIdentifiers: (const char *)vars, ...
{
  va_list args;
  id newCPM;

  va_start(args, vars);

  newCPM = [CustomProbeMap createBegin: aZone];
  [newCPM setProbedClass: aClass];
  newCPM = [newCPM createEnd];

  newCPM = addProbesWithIdentifers(probeLibrary, newCPM, aClass, vars, args);
  va_end(args);

  return newCPM;  
}


- createEnd
{
  if (SAFEPROBES)
    if (probedClass == 0)
      {
        raiseEvent (WarningMessage,
                    "ProbeMap object was not properly initialized\n");
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

PHASE(Setting)

- addProbesForClass: (Class) aClass 
   withIdentifiers:  (const char *)vars, ...
{
  va_list args;

  va_start(args, vars);
  addProbesWithIdentifers(probeLibrary, self, aClass, vars, args);
  va_end(args);

  return self;
}

PHASE(Using)

@end

