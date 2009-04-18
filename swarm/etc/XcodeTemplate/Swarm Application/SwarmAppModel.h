//
//  ÇPROJECTNAMEÈModel.h
//  ÇPROJECTNAMEÈ
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ. All rights reserved.
//

#import <Swarm/Swarm.h>


@interface ÇPROJECTNAMEÈModel : Swarm {
  id <ActionGroup> modelActions;	          // scheduling data structures
  id <Schedule> modelSchedule;

  long cycle;
}

+ createBegin: aZone;				  // extra methods you
- createEnd;					  // provide for Swarms
- buildObjects;
- buildActions;
- activateIn: swarmContext;

- (void)step;

@end
