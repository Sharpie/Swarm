// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/GUISwarm.h>
#import <simtoolsgui.h>
#import <simtoolsgui/global.h> // buildWindowGeometryRecordName

@implementation GUISwarm

PHASE(Creating)

- setWindowGeometryRecordName: (const char *)theWindowGeometryRecordName
{
  baseWindowGeometryRecordName = theWindowGeometryRecordName;
  return self;
}

- setWindowGeometryRecordNameForComponent: (const char *)componentName
                                   widget: theWidget
{
  return [theWidget
           setWindowGeometryRecordName:
             buildWindowGeometryRecordName (baseWindowGeometryRecordName,
                                            componentName)];
}

PHASE(Using)

- buildObjects
{
  [super buildObjects];
  controlPanel = [ControlPanel create: [self getZone]];
  // create the actionCache, we will initialize it in activateIn
  actionCache = [ActionCache createBegin: [self getZone]];
  SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME (actionCache);
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

- (void)drop
{
  [actionCache drop];
  [controlPanel drop];
  [super drop];
}
@end
