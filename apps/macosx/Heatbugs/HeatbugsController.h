/* All Rights reserved */

#import <Cocoa/Cocoa.h>

@interface HeatbugsController : NSObject
{
  id infoPanel;
  id inspectorPanel;
  NSMutableArray *simulations;
}

- (IBAction) dropSimulation: (id)sender;
- (IBAction) stepSimulation: (id)sender;
- (IBAction) stopSimulation: (id)sender;
- (IBAction) newSimulation: (id)sender;
- (IBAction) startSimulation: (id)sender;

- (IBAction) showInfoPanel: (id)sender;
- (IBAction) showInspectorPanel: (id)sender;

@end
