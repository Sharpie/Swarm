//
//  HeatbugsGUI.h
//  Heatbugs
//
//  Created by Scott Christley on 4/17/09.
//  Copyright __MyCompanyName__ 2009. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Swarm/OpenStepSwarm.h>
#import <Swarm/openstep.h>
#import "HeatbugsModel.h"

@interface HeatbugsGUI : OpenStepGUISwarm {
  HeatbugsModel *mainModel;
  
  int displayFrequency;
  id displayActions;
  id displaySchedule;

	id <Value2dDisplay> heatDisplay;              // display the heat
	id <Object2dDisplay> heatbugDisplay;          // display the heatbugs
}

+ createBegin: aZone;
- buildObjects;
- buildActions;
- activateIn: swarmContext;
- go;

// provide various
- (id)heatDisplay;
- (id)heatbugDisplay;

@end
