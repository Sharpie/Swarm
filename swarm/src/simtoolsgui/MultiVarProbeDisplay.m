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

- (void)update
{
  if (!markedForDropFlag)
    [widget update];
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
  return [[objectList getFirst] getDisplayName];
}

@end

