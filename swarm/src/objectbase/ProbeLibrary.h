// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>
#import <objectbase.h>

#define SIGFIGS_DISPLAYED 6
#define SIGFIGS_SAVED 6

@interface ProbeLibrary : CreateDrop <ProbeLibrary>
{
  id objectToNotify;
  id myZone;
  id classMap;
  int sigFigsDisplay;
  int sigFigsSaved;
}

- createEnd;

- setDisplayPrecision: (int)nSigDisplay;
- (int)getDisplayPrecision;
- setSavedPrecision: (int)nSigSaved;
- (int)getSavedPrecision;

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
//                  on: (const char *) probedElement
//              ofType: (char) dataType
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
- setObjectToNotify: anObject;
- getObjectToNotify;

- (BOOL)isProbeMapDefinedFor: (Class)aClass;
- (id <ProbeMap>)getProbeMapFor: (Class)aClass;
- (id <ProbeMap>)getCompleteProbeMapFor: (Class)aClass;
- (id <ProbeMap>)getCompleteVarMapFor: (Class)aClass;
- (id <VarProbe>)getProbeForVariable: (const char *)aVariable inClass: (Class) aClass;
- (id <MessageProbe>)getProbeForMessage: (const char *)aVariable inClass: (Class) aClass;
- setProbeMap: (id <ProbeMap>)aMap For: (Class)aClass;

@end
