// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui.h> // GUISwarm
#import <objectbase/Swarm.h> // Swarm

// a GUISwarm is a Swarm with some support for graphical interface. In
// particular, it creates a control panel for you and also provides a
// "go" method that handles the user hitting buttons like "stop" and
// "step". When using this, you still need to be sure to schedule
// calls to the controlPanels' doTkEvents method.

@interface GUISwarm: Swarm <GUISwarm>
{
  id <ControlPanel> controlPanel;
  id <ActionCache> actionCache;
  const char *baseWindowGeometryRecordName;
}

- setWindowGeometryRecordName: (const char *)windowGeometryRecordName;
- setWindowGeometryRecordNameForComponent: (const char *)componentName
                                   widget: aWidget;
- (id <ActionCache>)getActionCache;
- (id <ControlPanel>)getControlPanel;
- buildObjects;
- go;				   // returns Completed or ControlStateQuit
- (void)drop;
@end
