//
//  �PROJECTNAME�Model.h
//  �PROJECTNAME�
//
//  Created by �FULLUSERNAME� on �DATE�.
//  Copyright �ORGANIZATIONNAME� �YEAR�. All rights reserved.
//

#import <Swarm/Swarm.h>


@interface �PROJECTNAME�Model : Swarm {
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
