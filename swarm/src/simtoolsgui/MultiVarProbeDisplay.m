// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/MultiVarProbeDisplay.h>
#import <simtoolsgui/MultiVarProbeWidget.h>
#import <simtoolsgui.h>

@implementation MultiVarProbeDisplay

PHASE(Creating)

+ createBegin: aZone
{
  MultiVarProbeDisplay *obj = [super createBegin: aZone];

  obj->fieldLabelingFlag = YES;

  return obj;
}

- setObjectList: (id <List>)aList
{
  objectList = aList;
  
  return self;
}

- setProbeMap: (id <ProbeMap>)aProbeMap
{
  probeMap = aProbeMap;
  
  return self;
}

- setObjectNameSelector: (SEL)aSel
{
  objectNameSelector = aSel;
  
  return self;
}

- setFieldLabelingFlag: (BOOL)flag
{
  fieldLabelingFlag = flag;

  return self;
}

- createEnd
{
  [super createEnd];

  widget = [MultiVarProbeWidget createBegin: [self getZone]];
  [widget setParent: topFrame];
  [widget setFieldLabelingFlag: fieldLabelingFlag];
  [widget setProbeMap: probeMap];
  [widget setObjectList: objectList];
  [widget setObjectNameSelector: objectNameSelector];
  widget = [widget createEnd];

  [widget pack];
  [widget update];

  [self install];
  return self;
}

PHASE(Using)

- (void)drop
{
  [widget drop];
  [super drop];
}
@end

