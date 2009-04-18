//
//  �PROJECTNAME�Batch.h
//  �PROJECTNAME� Swarm Application
//
//  Created by �FULLUSERNAME� on �DATE�.
//  Copyright �ORGANIZATIONNAME� �YEAR�. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Swarm/Swarm.h>
#import "�PROJECTNAME�Model.h"

@interface �PROJECTNAME�Batch : Swarm {
  �PROJECTNAME�Model *mainModel;

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
