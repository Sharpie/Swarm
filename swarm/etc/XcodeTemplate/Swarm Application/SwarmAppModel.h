//
//  �PROJECTNAME�Model.h
//  �PROJECTNAME�
//
//  Created by �FULLUSERNAME� on �DATE�.
//  Copyright �ORGANIZATIONNAME� �YEAR�. All rights reserved.
//

#import <Swarm/OpenStepSwarm.h>


@interface �PROJECTNAME�Model : OpenStepSwarmModel {
  id <ActionGroup> modelActions;	          // scheduling data structures
  id <Schedule> modelSchedule;

  id RNG;
  id randomDouble;
  id randomNormal;

  long cycle;
}

// default creation where we are provided parameters
+ create: (id)anObserver withParameters: (NSDictionary *)params;

- buildObjects;
- buildActions;
- activateIn: swarmContext;

- (long)numberOfCycles;
- (id)randomDouble;
- (id)randomNormal;

- (void)step;

@end
