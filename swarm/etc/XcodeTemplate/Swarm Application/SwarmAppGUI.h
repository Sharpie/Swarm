//
//  ÇPROJECTNAMEÈGUI.h
//  ÇPROJECTNAMEÈ Swarm Application
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Swarm/OpenStepSwarm.h>
#import "ÇPROJECTNAMEÈModel.h"

@interface ÇPROJECTNAMEÈGUI : OpenStepGUISwarm {
  ÇPROJECTNAMEÈModel *mainModel;
  
  NSMutableDictionary *simParameters;

  int displayFrequency;
  id displayActions;
  id displaySchedule;
}

+ createBegin: aZone;
- buildObjects;
- buildActions;
- activateIn: swarmContext;
- go;

@end
