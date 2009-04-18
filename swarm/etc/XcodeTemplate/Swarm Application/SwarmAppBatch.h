//
//  ÇPROJECTNAMEÈBatch.h
//  ÇPROJECTNAMEÈ Swarm Application
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Swarm/Swarm.h>
#import "ÇPROJECTNAMEÈModel.h"

@interface ÇPROJECTNAMEÈBatch : Swarm {
  ÇPROJECTNAMEÈModel *mainModel;

  int experimentDuration;
  int outputRate;

  id stopSchedule;
  
  id batchActions;
  id batchSchedule;
}

+ createBegin: aZone;
- buildObjects;
- buildActions;
- activateIn: swarmContext;
- go;

- (void) outputBatchData;

@end
