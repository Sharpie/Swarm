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

  [topLevel setWindowTitle: [self getId: NULL]];

  top_top_Frame =  [Frame createParent: topFrame];
  
  raisedFrame =  [Frame createBegin: [self getZone]];
  [raisedFrame setParent: top_top_Frame];
  [raisedFrame setReliefFlag: YES];
  raisedFrame = [raisedFrame createEnd];

  title = [CompleteProbeDisplayLabel createBegin: [self getZone]];
  [title setParent: raisedFrame];
  [title setTargetWidget: self];
  [title setProbedObject: nil];
  title = [title createEnd];
  [title setText: [[objectList getFirst] name]];

  hideB = [SimpleProbeDisplayHideButton createBegin: [self getZone]];
  [hideB setParent: top_top_Frame];
  [hideB setProbeDisplay: self];
  hideB = [hideB createEnd];

  [raisedFrame packBeforeAndFillLeft: hideB expand: NO];  

  middleFrame = [Frame createParent: topFrame];

  widget = [MultiVarProbeWidget createBegin: [self getZone]];
  [widget setParent: middleFrame];
  [widget setFieldLabelingFlag: fieldLabelingFlag];
  [widget setProbeMap: probeMap];
  [widget setObjectList: objectList];
  [widget setObjectNameSelector: objectNameSelector];
  widget = [widget createEnd];

  [widget pack];
  [widget update];
  
  [top_top_Frame packFill];
  [middleFrame pack];

  [self install];
  return self;
}

PHASE(Using)

- update
{
  if (!markedForDropFlag)
    [widget update];
  
  return self;
}

- (void)drop
{
  [hideB drop];
  [title drop];
  [widget drop];

  [top_top_Frame drop];
  [middleFrame drop];
  [raisedFrame drop];
  [super drop];
}

- (const char *)package: (const char *)windowName
{
  return [[objectList getFirst] getObjectName];
}

- (const char *)getId: (const char *)windowName
{
  return [[objectList getFirst] getIdName];
}

@end

