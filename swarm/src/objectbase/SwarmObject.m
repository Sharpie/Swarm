// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <objectbase/probing.h>
#import <defobj/directory.h>

@implementation SwarmObject
PHASE(Creating)
PHASE(Using)
- getProbeMap
{
  return [probeLibrary getProbeMapFor: SD_GETCLASS (self)];
}

- getCompleteProbeMap
{
  return [probeLibrary getCompleteProbeMapFor: SD_GETCLASS (self)];
}

- getProbeForVariable: (const char *)aVariable 
{
  return [probeLibrary getProbeForVariable: aVariable
                       inClass: SD_GETCLASS (self)];
}

- getProbeForMessage: (const char *)aMessage
{
  return [probeLibrary getProbeForMessage: aMessage
                       inClass: SD_GETCLASS (self)];
}

- (void)eventOccurredOn: anObject
                    via: aProbe
          withProbeType: (const char *)aProbeType
                     on: (const char *)probedElement
                 ofType: (char)dataType
               withData: (void *)data
{
  [self subclassResponsibility: 
	  M(eventOccurredOn:via:withProbeType:on:ofType:withData:)];
}
     
@end
