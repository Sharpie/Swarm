// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/EmptyProbeMap.h>

@implementation EmptyProbeMap
 
PHASE(Creating)

+ create: aZone forClass: (Class)aClass
{
  EmptyProbeMap *newEPM;

  newEPM = [super createBegin: aZone];
  [newEPM setProbedClass: aClass];
  newEPM = [newEPM createEnd];

  return newEPM;  
}

PHASE(Setting)

PHASE(Using)

@end


