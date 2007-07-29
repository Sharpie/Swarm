/* All Rights reserved */

#include <AppKit/AppKit.h>

@interface HeatbugsSimulation : NSObject
{
  id rasterWindow;
  id rasterView;
  NSColorList *colorList;
  id swarmThreadController;
}

- heatbugSpaceView;
- (NSColorList *)colorList;

- (void) dropSimulation: (id)sender;
- (void) stepSimulation: (id)sender;
- (void) stopSimulation: (id)sender;
- (void) newSimulation: (id)sender;
- (void) startSimulation: (id)sender;

@end
