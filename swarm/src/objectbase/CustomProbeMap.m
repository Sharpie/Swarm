// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <objectbase/CustomProbeMap.h>
#import <defobj.h> // Warning
#import <defobj/defalloc.h> // getZone
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
  while (identifier && identifier[0] != ':')
    {
      [customProbeMap 
        addProbe: 
          [theProbeLibrary 
            getProbeForVariable: identifier
            inClass: aClass]];
      identifier = va_arg (argumentPointer, const char *);
    }
  
  // now do the methods
  while ((identifier = va_arg (argumentPointer, const char *)) != NULL)
    {
      id probe = [theProbeLibrary 
             getProbeForMessage: identifier
             inClass: aClass];

      if (!probe)
        raiseEvent(WarningMessage, "Unable to create probe for %s", identifier);
      else
        [customProbeMap 
            addProbe: [probe setHideResult: 0]];
    }
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
  
  probes = [Map createBegin: getZone (self)];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];
  
  if (probes == nil)
    return nil;

  count = 0;
  
  return self;
}

PHASE(Setting)

- addProbesForClass: (Class)aClass 
   withIdentifiers:  (const char *)vars, ...
{
  va_list args;

  va_start (args, vars);
  addProbesWithIdentifers (probeLibrary, self, aClass, vars, args);
  va_end (args);

  return self;
}

PHASE(Using)

@end

