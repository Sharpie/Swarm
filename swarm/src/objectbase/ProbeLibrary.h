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

#import <Swarm/Create.h>
#import <Swarm/objectbase.h>

#define SIGFIGS_DISPLAYED 6
#define SIGFIGS_SAVED 6

@interface ProbeLibrary : CreateDrop <ProbeLibrary>
{
  id objectToNotify;
  id myZone;
  id classMap;
  unsigned sigFigsDisplay;
  unsigned sigFigsSaved;
}

- createEnd;

- setDisplayPrecision: (unsigned)nSigDisplay;
- (unsigned)getDisplayPrecision;
- setSavedPrecision: (unsigned)nSigSaved;
- (unsigned)getSavedPrecision;

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
- (BOOL)isProbeMapDefinedForObject: anObject;

- (id <ProbeMap>)getProbeMapForObject: anObject;
- (id <ProbeMap>)getProbeMapFor: (Class)aClass;

- (id <ProbeMap>)getCompleteProbeMapForObject: anObject;
- (id <ProbeMap>)getCompleteProbeMapFor: (Class)aClass;

- (id <ProbeMap>)getCompleteVarMapForObject: anObject;
- (id <ProbeMap>)getCompleteVarMapFor: (Class)aClass;

- (id <VarProbe>)getProbeForVariable: (const char *)aVariable inObject: anObject;
- (id <VarProbe>)getProbeForVariable: (const char *)aVariable inClass: (Class)aClass;

- (id <MessageProbe>)getProbeForMessage: (const char *)aVariable inObject: anObject;
- (id <MessageProbe>)getProbeForMessage: (const char *)aVariable inClass: (Class)aClass;

- setProbeMap: (id <ProbeMap>)aMap For: (Class)aClass;
- setProbeMap: (id <ProbeMap>)aMap ForObject: anObject;

@end
