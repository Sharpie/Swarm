// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// place holder for a map of probes. This will be replaced with the
// generic Map class. Given a class, build an array of probe objects that
// work on that class (one per variable).

#import <collections/Map.h>
#import <swarmobject/Probe.h>
#import <swarmobject/VarProbe.h>
#import <swarmobject/MessageProbe.h>

@interface ProbeMap : SwarmObject {
  Class probedClass;
  int numEntries;
  id probes;
}

-setProbedClass: (Class) class;
-_copyCreateEnd_;
-createEnd;
-clone: aZone;
-(int) getNumEntries ;
-(Class) getProbedClass;
-addProbeMap: (ProbeMap *) aProbeMap ;
-dropProbeMap: (ProbeMap *) aProbeMap ;
-addProbe: (Probe *) aProbe ;
-_fastAddProbe_: (Probe *) aProbe ;
-dropProbeForVariable: (char *) aVariable;
-dropProbeForMessage: (char *) aMessage;
-(VarProbe *) getProbeForVariable: (char *) aVariable;
-(MessageProbe *) getProbeForMessage: (char *) aMessage;
-begin: aZone ;
@end

