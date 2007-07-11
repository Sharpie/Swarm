// Heatbugs application. Copyright © 2004 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.


#import "HeatbugsSimulation.h"
#import "HeatbugBatchSwarm.h"
#import <SwarmOSX/Raster.h>
#import <SwarmOSX/GNUstepSwarm.h>

@implementation HeatbugsSimulation

- (void)awakeFromNib
{
    fprintf(stderr, "HeatbugsSimulation awoke from nib\n");

    // We should have an inspector with the color panel for setting these.
    if (!colorList)
    {
	colorList = [[NSColorList alloc] initWithName: @"Heatbugs"];
	// some defaults
	[colorList setColor: [NSColor greenColor] forKey: @"bugColor"];
	[colorList setColor: [NSColor redColor] forKey: @"heatColor"];
    }

  // Create rasters and attach to windows
  rasterView = [[Raster alloc] initWithFrame:
				 [[rasterWindow contentView] bounds]
			       pointSize:NSMakeSize(5, 5)];
  [(NSWindow *)rasterWindow setContentView: rasterView];
}

- heatbugSpaceView
{
    return rasterView;
}

- (NSColorList *)colorList
{
    return colorList;
}

- (void) startSimulation: (id)sender
{
  [swarmThreadController startSwarmSimulation: sender];
}

- (void) dropSimulation: (id)sender
{
  [swarmThreadController dropSwarmSimulation: sender];
}


- (void) stepSimulation: (id)sender
{
  [swarmThreadController stepSwarmSimulation: sender];
}


- (void) stopSimulation: (id)sender
{
  [swarmThreadController stopSwarmSimulation: sender];
}

- (void) newSimulation: (id)sender
{
#if 1
  swarmThreadController = [GNUstepSwarmController new];
  [swarmThreadController spawnSimulation: [HeatbugBatchSwarm class]
   withDelegate: self];

  [rasterView addDisplay: [[swarmThreadController getSwarm]
			    getHeatDisplay]];
  [rasterView addDisplay: [[swarmThreadController getSwarm]
			    getHeatbugDisplay]];
#endif

  [rasterWindow makeKeyAndOrderFront: self];
}

@end
