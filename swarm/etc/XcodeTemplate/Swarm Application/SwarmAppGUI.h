//
//  ÇPROJECTNAMEÈGUI.h
//  ÇPROJECTNAMEÈ Swarm Application
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ. All rights reserved.
//

#import "ÇPROJECTNAMEÈModel.h"
#import <Swarm/OpenStepSwarm.h>
#import <Swarm/Value2dDisplay.h>
#import <Swarm/Object2dDisplay.h>

@interface ÇPROJECTNAMEÈGUI : OpenStepGUISwarm {
  ÇPROJECTNAMEÈModel *mainModel;
	
  int displayFrequency;
  id displayActions;
  id displaySchedule;

  // displays
  // id <Object2dDisplay> someDisplay;
}

+ createBegin: aZone;
- buildObjects;
- buildActions;
- activateIn: swarmContext;

- mainModel;

// - someDisplay;

- (void)updateGraphicalDisplays;

@end
