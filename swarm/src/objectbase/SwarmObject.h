// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// base class for all Swarm objects: gives creation and debugging
// protocol, basic implementation.

#import <defobj/Create.h>

@interface SwarmObject: CreateDrop
{
}

- getProbeMap;
- getCompleteProbeMap;
- getProbeForVariable: (const char *)aVariable;
- getProbeForMessage: (const char *)aMessage;
- eventOccurredOn: anObject
              via: aProbe
    withProbeType: (const char *)aProbeType
               on: (const char *)probedElement
           ofType: (char)dataType
         withData: (void *)data;
@end

