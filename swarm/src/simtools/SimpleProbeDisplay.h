// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/SwarmObject.h>
#import <tkobjc.h>
#import <swarmobject/ProbeMap.h>
#import <simtools/VarProbeWidget.h>
#import <simtools/MessageProbeWidget.h>

@interface SimpleProbeDisplay : SwarmObject
{
  id probedObject;  
  ProbeMap * probeMap;
  Frame *topFrame, *leftFrame, *rightFrame,
    *middleFrame, *bottomFrame;
  Label *myTitle ;
  int numberOfProbes;
  id *widgets;
  ref_t objectRef;
  BOOL removeRef;
  BOOL markedForDropFlag;
  
  Frame *topLevel;
  const char *windowGeometryRecordName;
}

- setWindowGeometryRecordName: (const char *)theName;
- setProbedObject: anObject;
- setProbeMap: (ProbeMap *) probeMap;
- createEnd;

- getProbedObject;
- getProbeMap;
- update;

- (const char *)package;
- (const char *)getId;

- (void)setRemoveRef: (BOOL) torf;
- (void)setObjectRef: (ref_t) or;
- (void)drop;
- (BOOL)getMarkedForDropFlag;

@end
