// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <objectbase/probing.h>

@implementation SwarmObject

- getProbeMap
{
  return [probeLibrary getProbeMapFor: [self class]];
}

- getCompleteProbeMap
{
  return [probeLibrary getCompleteProbeMapFor: [self class]];
}

- getProbeForVariable: (const char *)aVariable 
{
  return [probeLibrary getProbeForVariable: aVariable inClass: [self class]];
}

- getProbeForMessage: (const char *)aMessage
{
  return [probeLibrary getProbeForMessage: aMessage inClass: [self class]];
}

- eventOccurredOn: (id) anObject
              via: (id) aProbe
    withProbeType: (const char *) aProbeType
               on: (const char *) probedElement
           ofType: (char)dataType
         withData: (void *)data
{
  [self subclassResponsibility: 
	  M(eventOccurredOn:via:withProbeType:on:ofType:withData:)];
  return self;
}
     
@end
