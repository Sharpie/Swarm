// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject.h>
#import <tkobjc.h>
#import <simtools/VarProbeWidget.h>
#import <simtools/MessageProbeWidget.h>

@interface ClassDisplayWidget : Frame {
  id probedObject;
  Class theClass ;
  Label *myTitle ;
  ProbeMap * probeMap;
  Frame *leftFrame, *rightFrame, *middleFrame, *bottomFrame;
  id hideB ;
  id superB ;
  id topRow ;
  int numberOfProbes;
  int maxLabelWidth ;
  id *widgets;
  id mySuperClass ;
  id mySubClass ;
  id owner ;
}

+createBegin: aZone ;
-setProbedObject: anObject;
-setClassToDisplay: (Class) aClass ;
-setMaxLabelWidth: (int) width ;
-setOwner: anOwner ;
-setMySuperClass: aWidget ;
-setMySubClass: aWidget ;
-createEnd;

-getProbedObject;
-getProbeMap;
-armSuperButton;
-update;

@end
