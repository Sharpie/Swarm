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

#import <simtoolsgui/GUISwarm.h>
#import <simtoolsgui.h>

@implementation GUISwarm

PHASE(Creating)

- setWindowGeometryRecordName: (const char *)theWindowGeometryRecordName
{
  baseWindowGeometryRecordName = (theWindowGeometryRecordName
				  ? STRDUP (theWindowGeometryRecordName)
				  : NULL);
  return self;
}

- setSaveSizeFlag: (BOOL)theSaveSizeFlag
{
  saveSizeFlag = theSaveSizeFlag;
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
  [actionCache setSaveSizeFlag: saveSizeFlag];
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

- (id <Activity>)activateIn: swarmContext
{
  [super activateIn: swarmContext];
  [actionCache setScheduleContext: self];
  return [self getActivity];
}

- (id <ActionCache>)getActionCache
{
    return actionCache;
}

- (id <ControlPanel>)getControlPanel
{
    return controlPanel;
}

- (void)drop
{
  if (baseWindowGeometryRecordName)
    FREEBLOCK (baseWindowGeometryRecordName);
  [actionCache drop];
  [controlPanel drop];
  [super drop];
}
@end
