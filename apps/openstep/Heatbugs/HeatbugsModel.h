//
//  HeatbugsModel.h
//  Heatbugs
//
//  Created by Scott Christley on 4/17/09.
//  Copyright __MyCompanyName__ 2009. All rights reserved.
//

#import "Heatbug.h"
#import "HeatSpace.h"

#import <Swarm/Swarm.h>


@interface HeatbugsModel : Swarm {
	int numBugs;					  // simulation parameters
	double evaporationRate;
	double diffuseConstant;
	int worldXSize, worldYSize;
	int minIdealTemp, maxIdealTemp;
	int minOutputHeat, maxOutputHeat;
	double randomMoveProbability;

	BOOL randomizeHeatbugUpdateOrder;

	id <ActionGroup> modelActions;	          // scheduling data structures
  id <Schedule> modelSchedule;

  long cycle;

	id actionForEach;                               // for frobbing randomization
	
	id <List> heatbugList;			  // list of all the heatbugs
	id <Grid2d> world;				  // objects representing
	HeatSpace *heat;				  // the world
}

- getHeatbugList;				  // access methods into the
- (id <Grid2d>)getWorld;			  // model swarm. These methods
- (HeatSpace *)getHeat;                           // allow the model swarm to
// be observed.
- (BOOL)toggleRandomizedOrder;                    // method to toggle the
// randomization feature
- addHeatbug: (Heatbug *)bug;			  // special method for demo


+ createBegin: aZone;				  // extra methods you
- createEnd;					  // provide for Swarms
- buildObjects;
- buildActions;
- activateIn: swarmContext;

- (void)step;

@end
