// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>

@interface ProbeLibrary : CreateDrop {
        id objectToNotify;
	id myZone ;
	id classMap ;
}

-createEnd ;

// This sets every member of every ProbeMap up so that it will
// send this message to the designated object every time it's activated.
// It can be overridden by unsetting a particular ProbeMap or by 
// unsetting a particular Probe.  Or a given ProbeMap or Probe can be
// individually given extra methods to perform.
//
// the receiver that gets set here must accept a message of the form:
//    -eventOccurredOn: (id) probedObject 
//                 via: (id) aProbe
//       withProbeType: (const char *) aProbeType
//                  on: (char *) probedElement
//              ofType: (const char) dataType
//            withData: (void *) data;
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
-setObjectToNotify: (id) anObject;
-getObjectToNotify;

-(BOOL) isProbeMapDefinedFor: (Class) aClass;
-getProbeMapFor: (Class) aClass ;
-getCompleteProbeMapFor: (Class) aClass ;
-getCompleteVarMapFor: (Class) aClass ;
-getProbeForVariable: (char *) aVariable inClass: (Class) aClass ;
-getProbeForMessage: (char *) aVariable inClass: (Class) aClass ;
-setProbeMap: aMap For: (Class) aClass ;
@end
