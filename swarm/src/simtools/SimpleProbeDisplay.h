// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject.h>
#import <tkobjc.h>
#import <simtools/VarProbeWidget.h>
#import <simtools/MessageProbeWidget.h>

@interface SimpleProbeDisplay : SwarmObject {
  id probedObject;  
  ProbeMap * probeMap;
  Frame *topFrame, *leftFrame, *rightFrame,
        *middleFrame, *bottomFrame, *topLevel;
  Label *myTitle ;
  int numberOfProbes;
  id *widgets;
}

-setProbedObject: anObject;
-setProbeMap: (ProbeMap *) probeMap;
-createEnd;

-getProbedObject;
-getProbeMap;
-update;

-(char *) package ;
-(const char *) getId ;

@end



