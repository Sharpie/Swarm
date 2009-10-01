//
//  ÇPROJECTNAMEÈBatch.h
//  ÇPROJECTNAMEÈ Swarm Application
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ. All rights reserved.
//

#import <Swarm/OpenStepSwarm.h>
#import "ÇPROJECTNAMEÈModel.h"

@interface ÇPROJECTNAMEÈBatch : OpenStepSwarmModel {
  ÇPROJECTNAMEÈModel *mainModel;

  int experimentDuration;
  int outputRate;

  id stopSchedule;
  
  id batchActions;
  id batchSchedule;
}

- createEnd;
- buildObjects;
- buildActions;
- activateIn: swarmContext;
- go;

- (void) outputBatchData;

@end
