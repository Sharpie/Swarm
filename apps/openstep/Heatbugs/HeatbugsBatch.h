//
//  HeatbugsBatch.h
//  Heatbugs
//
//  Created by Scott Christley on 4/17/09.
//  Copyright __MyCompanyName__ 2009. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Swarm/Swarm.h>
#import "HeatbugsModel.h"

@interface HeatbugsBatch : Swarm {
  HeatbugsModel *mainModel;

  NSMutableDictionary *simParameters;

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
