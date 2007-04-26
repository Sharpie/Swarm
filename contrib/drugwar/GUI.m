// DrugWar model. Copyright © 2000 Swarm Development Group
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.
#import "GUI.h"
#import "Society.h"
#import "Agent.h"
#import <simtools.h>
#import <simtoolsgui.h>

@implementation GUI
+ createBegin: aZone
{
  GUI *newMe;
  newMe = [super createBegin: aZone];
  newMe->guiZone = [Zone create: aZone];

  newMe->displayFrequency = 1;
  newMe->canvasHeight = 600;
  newMe->canvasWidth = 700;
  newMe->nodeSize = 60;

  return newMe;
}

- createEnd
{
  id me;
  me = [super createEnd];
  [ObjectLoader load: me fromAppDataFileNamed: "GUI-Data.001"];
  return me;
}

- build 
{
  // get the guiSwarm overhead objects
  [super buildObjects];
  society = [Society create: guiZone];

  [ObjectLoader load: society fromAppDataFileNamed: "Society-Data.001"];
  [society setNodeSize: nodeSize];  // ignore what's in the society data file
  // build the probes needed at startup
  [self buildProbes];

  // tell controlPanel to wait for a click
  [actionCache waitForControlEvent];
  if ([controlPanel getState] == ControlStateQuit)
    return self;

  // instantiate the canvas
  canvas = [Canvas createBegin: guiZone];
  SET_WINDOW_GEOMETRY_RECORD_NAME (canvas);
  canvas = [canvas createEnd];

  [canvas setHeight: canvasHeight];
  [canvas setWidth: canvasWidth];
  [canvas setWindowTitle: "Boolean Social Network"];
  [canvas packFillLeft: YES];
  GUI_UPDATE_IDLE_TASKS();

  [society setCanvas: canvas];
  // build the model
  [society build];

  [self buildActivity];

  return self;
}

- buildProbes
{
  // instantiate the probes for the gui and the model
  [Agent designProbeMap: guiZone];
  CREATE_ARCHIVED_PROBE_DISPLAY (society);
  CREATE_ARCHIVED_PROBE_DISPLAY (self);

  return self;
}

- buildActivity
{
  [super buildActions];
  [society buildActivity];
  
  displayActions = [ActionGroup create: guiZone];
  [displayActions createActionTo: probeDisplayManager message: M(update)];
  [displayActions createActionTo: actionCache         message: M(doTkEvents)];

  displaySchedule = [Schedule createBegin: guiZone];
  [displaySchedule setRepeatInterval: displayFrequency];
  displaySchedule = [displaySchedule createEnd];
  [displaySchedule at: 0 createAction: displayActions];

  return self;
}

- activateIn: swarmContext
{
  [super activateIn: swarmContext];
  [society activateIn: self];
  [displaySchedule activateIn: self];
  return [self getActivity];
}

@end
