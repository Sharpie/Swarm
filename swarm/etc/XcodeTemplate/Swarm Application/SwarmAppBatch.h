//
//  �PROJECTNAME�Batch.h
//  �PROJECTNAME� Swarm Application
//
//  Created by �FULLUSERNAME� on �DATE�.
//  Copyright �ORGANIZATIONNAME� �YEAR�. All rights reserved.
//

#import <Swarm/OpenStepSwarm.h>
#import "�PROJECTNAME�Model.h"

@interface �PROJECTNAME�Batch : OpenStepSwarmModel {
  �PROJECTNAME�Model *mainModel;

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
