// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// place holder for a map of probes. This will be replaced with the
// generic Map class. Given a class, build an array of probe objects that
// work on that class (one per variable).

#import <collections/Map.h>
#import <objectbase/Probe.h>
#import <objectbase/VarProbe.h>
#import <objectbase/MessageProbe.h>

@interface ProbeMap : SwarmObject
{
  Class probedClass;
  int numEntries;
  id probes;
  id objectToNotify;  //could be an object or a list
}

+ createBegin: aZone;

// This sets every member of *this* ProbeMap up so that it will
// send this message to the designated object every time it's activated.
// It can be overridden by unsetting a particular ProbeMap or by 
// unsetting a particular Probe.  Or a given ProbeMap or Probe can be
// individually given extra methods to perform.
//
// the receiver that gets set here must accept a message of the form:
//    -eventOccurredOn: probedObject 
//                 via: aProbe
//       withProbeType: (const char *)aProbeType
//                  on: (char *)probedElement
//              ofType: (const char *)dataType
//            withData: (void *)data;
//
//    VarProbe  => aProbeType = "VarProbe"
//                 probedElement = variable name string
//                 data = a pointer to either a new value or string
//                        it's the user's responsibility to be able
//                        to handle either strings or values
// MessageProbe => aProbeType = "MessageProbe"
//                 probedElement = selector name string
//                 data = a pointer to a string of argument labels and
//                        arguments separated by spaces
- setObjectToNotify: anObject;  //can be called multiple times
- getObjectToNotify;

- setProbedClass: (Class)class;
- _copyCreateEnd_;
- createEnd;
- clone: aZone;
- (int)getNumEntries;
- (Class)getProbedClass;
- addProbeMap: (ProbeMap *)aProbeMap;
- dropProbeMap: (ProbeMap *)aProbeMap;
- addProbe: (Probe *)aProbe;
- _fastAddProbe_: (Probe *)aProbe;
- dropProbeForVariable: (const char *)aVariable;
- dropProbeForMessage: (const char *)aMessage;
- (VarProbe *)getProbeForVariable: (const char *)aVariable;
- (MessageProbe *)getProbeForMessage: (const char *)aMessage;
- begin: aZone;
@end

