// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <objectbase/probing.h>

//This means that 'somewhere' there must be a definition of probeLibrary...
//I don't actually import a .h with its definition because it will almost
//certainly contain a reference to a subclass of SwarmObject thereby
//generating a multiple inclusion error (since "SwarmObject.h" is imported
//here and <objectbase.h> will be imported whereever probeLibrary is actually
//defined...

//S: A superclass of most objects in a Swarm simulation that provides support
//S: for probing.
//D: A SwarmObject is an object that is intended to be a member of a Swarm. 
//D: It's behavior will be perpetuated by messages sent from the schedule 
//D: of events defined in the context of a Swarm object. 
//D: The SwarmObject is where the models of all the agents of a simulation 
//D: will reside. Hence, most of the burden on defining the messages that
//D: can be sent to any agent lies with the user. SwarmObject inherits its 
//D: basic functionality from the Create and Drop object types defined in the
//D: defobj library. 
@implementation SwarmObject

//M: The getInstanceName method returns a meaningful word or phrase to indicate
//M: the object being probed (and in the case of a probe display, place it in a
//M: widget on the screen).
- (const char *)getInstanceName
{
  return [self name];
}

- (const char *)getIdName
{
  return [self getInstanceName];
}

//M: The getProbeMap method returns a pointer to the ProbeMap for the object
//M: if there has been one creaded for that object's class.  If it hasn't been 
//M: created, then it creates a default ProbeMap.
- getProbeMap
{
  return [probeLibrary getProbeMapFor: [self class]];
}

//M: The getCompleteProbeMap method returns a newly created CompleteProbeMap
//M: for an object. 
- getCompleteProbeMap
{
  return [probeLibrary getCompleteProbeMapFor: [self class]];
}

//M: The getProbeForVariable: method returns the VarProbe indexed in the
//M: ProbeMap by the string aVariable.
- getProbeForVariable: (const char *)aVariable 
{
  return [probeLibrary getProbeForVariable: aVariable inClass: [self class]];
}

//M: The getProbeForMessage: method returns the MessageProbe indexed in the 
//M: ProbeMap by the string aMessage.
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
