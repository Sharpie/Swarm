// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/GUISwarm.h>
#import <simtools/ControlPanel.h>
#import <simtools.h>

@implementation GUISwarm

- setWindowGeometryRecordName: (const char *)theWindowGeometryRecordName
{
  baseWindowGeometryRecordName = theWindowGeometryRecordName;
  return self;
}

- (const char *)windowGeometryRecordName
{
  return baseWindowGeometryRecordName;
}

- setWindowGeometryRecordNameForComponent: (const char *)componentName
                                   widget: theWidget
{
  return [theWidget
           setWindowGeometryRecordName:
             buildWindowGeometryRecordName (baseWindowGeometryRecordName,
                                            componentName)];
}

- buildObjects
{
  [super buildObjects];
  controlPanel = [ControlPanel create: [self getZone]];
  // create the actionCache, we will initialize it in activateIn
  actionCache = [ActionCache createBegin: [self getZone]];
  SET_WINDOW_GEOMETRY_RECORD_NAME (actionCache);
  [actionCache setControlPanel: controlPanel];
  actionCache = [actionCache createEnd];
  return self;
}

// use of this method should be deprecated.  Control of an independent
// Swarm should be handled by that Swarm's control panel, regardless
// of whether it's a guiswarm or batchswarm or whatever.
- go
{
  return [controlPanel startInActivity: [self getActivity]];
}

- activateIn: swarmContext
{
  [super activateIn: swarmContext];
  [actionCache setScheduleContext: self];
  return [self getActivity];
}

@end
