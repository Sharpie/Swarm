// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// place holder for a map of probes. This will be replaced with the
// generic Map class. Given a class, build an array of probe objects that
// work on that class (one per variable).

#import <objectbase/SwarmObject.h>
#import <objectbase.h>

#ifdef BUILDING_SWARM
#ifdef HAVE_JDK
#import "../defobj/java.h"
#endif
#import "../defobj/COM.h"
#endif

@interface ProbeMap: SwarmObject <ProbeMap>
{
  Class probedClass;
  id probedObject;
  unsigned count;
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
- setProbedObject: object;
- _copyCreateEnd_;
- createEnd;
- clone: aZone;
- (unsigned)getCount;
- (Class)getProbedClass;
- addProbeMap: (id <ProbeMap>)aProbeMap;
- dropProbeMap: (id <ProbeMap>)aProbeMap;
- addProbe: (id <Probe>)aProbe;
- _fastAddProbe_: (id <Probe>)aProbe;
- (void)dropProbeForVariable: (const char *)aVariable;
- (void)dropProbeForMessage: (const char *)aMessage;
- (id <VarProbe>)getProbeForVariable: (const char *)aVariable;
- (id <MessageProbe>)getProbeForMessage: (const char *)aMessage;
- begin: aZone;

#ifdef BUILDING_SWARM
#ifdef HAVE_JDK
- (void)addJavaFields: (jclass)javaClass;
- (void)addJavaMethods: (jclass)javaClass;
#endif
- (void)_addVarProbeForClass_: (Class)aClass variableName: (const char *)aName;
- (void)_addVarProbeForObject_: anObject variableName: (const char *)aName;
- (void)_addVarProbe_: (COMclass)cClass getter: (COMmethod)getter setter: (COMmethod)setter;
- (void)_addMessageProbe_: (Class)aClass selector: (SEL)aSel;
- (void)addCOMFields: (COMclass)cClass;
- (void)addCOMMethods: (COMclass)cClass;
- (void)addJSFields: (COMobject)cObj;
- (void)addJSMethods: (COMobject)cObj;
#endif

- (void)addObjcFields: (Class)oClass;
- (void)addObjcMethods: (Class)oClass;

- (void)describeForEach: stream;
@end

