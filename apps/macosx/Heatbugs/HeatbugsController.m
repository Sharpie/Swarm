// Heatbugs application. Copyright © 2004 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.


#import "HeatbugsController.h"
#import "HeatbugsSimulation.h"
#import "HeatbugBatchSwarm.h"
#import <SwarmOSX/Raster.h>

@implementation HeatbugsController

- (void)awakeFromNib
{
  fprintf(stderr, "HeatbugsController awoke from nib\n");

  simulations = [NSMutableArray new];
}

- (void) startSimulation: (id)sender
{
  HeatbugsSimulation *aSim = [simulations objectAtIndex: 0];
  [aSim startSimulation: sender];
}

- (void) dropSimulation: (id)sender
{
  HeatbugsSimulation *aSim = [simulations objectAtIndex: 0];
  [aSim dropSimulation: sender];
}


- (void) stepSimulation: (id)sender
{
  HeatbugsSimulation *aSim = [simulations objectAtIndex: 0];
  [aSim stepSimulation: sender];
}


- (void) stopSimulation: (id)sender
{
  HeatbugsSimulation *aSim = [simulations objectAtIndex: 0];
  [aSim stopSimulation: sender];
}

- (void) newSimulation: (id)sender
{
  HeatbugsSimulation *aSim = [HeatbugsSimulation new];

  [NSBundle loadNibNamed: @"Simulation" owner: aSim];
  [simulations addObject: aSim];

  [aSim newSimulation: sender];
}

- (void) showInfoPanel: (id)sender
{
    [infoPanel makeKeyAndOrderFront: self];
}

- (void) showInspectorPanel: (id)sender
{
  [inspectorPanel makeKeyAndOrderFront: self];
}

@end
