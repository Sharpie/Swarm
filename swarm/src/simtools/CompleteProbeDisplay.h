// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/SwarmObject.h>
#import <swarmobject/MessageProbe.h>
#import <swarmobject/ProbeMap.h>

#import <tkobjc.h>
#import <simtools/ClassDisplayWidget.h>

@interface CompleteProbeDisplay : SwarmObject {
  id probedObject;
  id the_canvas ;
  Frame *topFrame ;
  id widgets, topLevel;
  ref_t objectRef;
  BOOL removeRef;
}

-setProbedObject: anObject;
-createEnd;

-getProbedObject;
-update;

-(void) setRemoveRef: (BOOL) torf;
-(void) setObjectRef: (ref_t) or;
-(void) drop;

@end
